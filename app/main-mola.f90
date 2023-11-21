program main
   use, intrinsic :: iso_fortran_env
   use :: mpi_f08
   use :: netcdf
   use :: img2nc
   use :: mola_megdr 

   implicit none

   integer :: i, j, k
   integer :: uni, ios, ierr
   
   character(len=MAX_PATH_LEN) :: filename, data_dir
   character(len=MAX_NAME_LEN) :: outnc
   character(len=MAX_RANGE_LEN) :: range

   character(len=MAX_NAME_LEN), parameter :: DEFAULT_OUT = './mola.nc'
   character(len=MAX_NAME_LEN), parameter :: DEFAULT_DAT = './mola-megdr'

   integer(int32), allocatable :: distri_1d(:), distri_2d(:,:)
   logical, allocatable :: distri_logical(:, :)

   character(len=MAX_PATH_LEN), allocatable :: file_list(:,:)
   character(len=MAX_PATH_LEN), allocatable :: file_list_local(:)
   
   type(boundary) :: edge, outline
   integer(int32) :: coarse

   integer(int32) :: numlon, numlat
   integer(int32) :: nx, ny
   integer(int32) :: local_nx, local_ny
   integer(int32) :: global_nx, global_ny

   real(real64) :: step_lon, step_lat
   real(real64), allocatable :: lon(:), lat(:)
   real(real64) :: offset_x, offset_y

   character(:), allocatable :: cache

   integer(int32) :: n_jobs, n_jobs_total

   type(tile), allocatable :: tiles(:)
   type(LabelFile), allocatable :: lbls(:)

   type(buff) :: buff_single

   type(nc_t) :: nc

   logical :: isExist

   logical, allocatable :: is_on_west_edge(:,:), is_on_east_edge(:,:)
   logical, allocatable :: is_on_south_edge(:,:), is_on_north_edge(:,:)
   integer(int32) :: idx_west_global, idx_east_global
   integer(int32) :: idx_west_local, idx_east_local
   integer(int32) :: idx_south_global, idx_north_global
   integer(int32) :: idx_south_local, idx_north_local
!=====================================================================!

   call mpi_initialize(ierr)
   
!== Initialize
   data_dir = ''
   filename = ''
   outnc = ''


   call preprocess(data_dir, outnc, range, coarse, edge, resolution_default=MOLA_MEG128_PPD_MAX, ppd_max=MOLA_MEG128_PPD_MAX)
   outline = outline_mola(edge)
   call default(data_dir, outnc)

!== Allocation

   call allocate_lists(outline, file_list, distri_1d, distri_2d, distri_logical)

   call create_name_list(data_dir, file_list, outline)

   call set_distribution(distri_1d, distri_2d, distri_logical, outline)


   numlon = get_siz_lon_meg128(outline%get_west(), outline%get_east())
   numlat = get_siz_lat_meg128(outline%get_south(), outline%get_north())

   trimming_prepare: block
      allocate(is_on_west_edge(numlon, numlat))
      allocate(is_on_east_edge(numlon, numlat))
      allocate(is_on_south_edge(numlon, numlat))
      allocate(is_on_north_edge(numlon, numlat ))

      call set_is_on_west_edge(is_on_west_edge)
      call set_is_on_east_edge(is_on_east_edge)
      call set_is_on_south_edge(is_on_south_edge)
      call set_is_on_north_edge(is_on_north_edge)

   end block trimming_prepare


   do j = 1, size(file_list, dim=2)
      do i = 1, size(file_list, dim=1)
         file_list(i, j) = trim(file_list(i,j))
         ! inquire(file=file_list(i,j), exist=isExist)
      end do
   end do

   block
      n_jobs_total = numlon*numlat
      n_jobs = int(n_jobs_total/petot)
      if (thisis <= mod(n_jobs_total, petot)) n_jobs = n_jobs + 1

      allocate(file_list_local(n_jobs))
   end block 


   k = 1
   do j = 1, numlat
      do i = 1, numlon
         if (distri_logical(i,j)) then
            file_list_local(k) = adjustl(file_list(i,j))
            k = k + 1
         end if 
      end do
   end do


   allocate(tiles(1:n_jobs))

   allocate(lbls(1:n_jobs))

   read_lbls: block
      integer(int32) :: uni = 100
      do k = 1, n_jobs
         
         filename = trim(adjustl(file_list_local(k)))//'.lbl'

         call lbls(k)%init()
         inquire(file=filename, exist=isExist)
         if (isExist) then
            open(uni, file=filename, form='unformatted', access='stream', status='old')
            call lbls(k)%load_file(uni, ios=ios)
            call lbls(k)%read_buff()
            close(uni)
         else
            write(stderr, '(A)') "ERROR: File "//trim(filename)//' does not exist.'
         end if
      end do
   end block read_lbls

   lbls_val_check: block
      do concurrent (k = 1:n_jobs)

         cache = trim(lbls(k)%get_value(LINE_SAMPLES))
         read(cache, *) nx

         cache = trim(lbls(k)%get_value(LINES))
         read(cache, *) ny

         if (k == 1) then
            local_nx = nx
            local_ny = ny
         else
            if (local_nx /= nx) write(stderr, '(a)') 'ERROR: Non-uniform LINE_SAMPLES value found.'
            if (local_ny /= ny) write(stderr, '(a)') 'ERROR: Non-uniform LINES value found.'
         end if
      end do
   end block lbls_val_check

   set_step_nums: block
      local_nx = nx/coarse
      local_ny = ny/coarse

      global_nx = nx*numlon/coarse
      global_ny = ny*numlat/coarse

      step_lon = (dble(outline%get_east()) - dble(outline%get_west()))/dble(global_nx)
      step_lat = (dble(outline%get_north()) - dble(outline%get_south()))/dble(global_ny)

   end block set_step_nums

   
   sync_io: block
      integer(int32) :: uni = 100
      do k = 1, n_jobs

         allocate(buff_single%data(nx, ny), source=MINUS_9999_AFTER_SWAP16)
         allocate(tiles(k)%shrinked_data(local_nx, local_ny))

         filename = trim(adjustl(file_list_local(k)))//'.img'

         call tiles(k)%set_path(filename)

         call buff_single%read_data(uni, file=tiles(k)%get_path())

         call buff_single%big_to_little()

         if (coarse > 1) call buff_single%shrink(coarse)

         do concurrent (j = 1:local_ny)
            tiles(k)%shrinked_data(:, j) = buff_single%data(:, local_ny-j+1)
         end do

         deallocate(buff_single%data)

         print '(a, a, i5, a, i5, a)', trim(tiles(k)%get_path()), ': loaded. (', k, '/', n_jobs, ')'

      end do
   end block sync_io



   lonlat_prepare: block 
      integer :: i, j
      real(real64) :: south, west

      if (coarse > 1) then
         offset_x = +step_lon/2d0
         offset_y = -step_lat/2d0
      end if

      
      allocate(lon(global_nx))
      allocate(lat(global_ny))

      west = dble(outline%get_west())
      south = dble(outline%get_south())


      do concurrent(i = 1:global_nx)
         lon(i) = dble(i-1)*step_lon + west + offset_x
      end do

      do concurrent(j = 1:global_ny)
         lat(j) = dble(j)*step_lat + south + offset_y 
      end do

      if (edge%get_south() == 0) lat(1) = step_lat + offset_y

      
   end block lonlat_prepare

   
   trimming: block

      do i = 1, global_nx
         if (edge%get_west() <= lon(i)) then
            idx_west_global = i
            exit 
         end if
      end do 

      do i = global_nx, 1, -1
         if (lon(i) <= edge%get_east()) then
            idx_east_global = i
            exit
         end if
      end do

      do j = 1, global_ny 
         if (edge%get_south() <= lat(j) ) then
            idx_south_global = j
            exit
         end if
      end do

      do j = global_ny, 1, -1
         if ( lat(j) <= edge%get_north()) then 
            idx_north_global = j
            exit
         end if
      end do 


      idx_west_local = idx_west_global
      idx_east_local = mod(idx_east_global, local_nx)
      if (idx_east_local == 0) idx_east_local =  local_nx

      idx_south_local = idx_south_global
      idx_north_local = mod(idx_north_global, local_ny)
      if (idx_north_local == 0) idx_north_local = local_ny

      k = 1 
      do j = 1, numlat
         do i = 1, numlon
            if (distri_logical(i, j)) then
              
               if (is_on_west_edge(i,j) .and. is_on_east_edge(i,j)) then
                  call trimming_west_east(tiles(k)%shrinked_data, idx_west_local, idx_east_local)
               
               else if (is_on_west_edge(i,j)) then
                  call trimming_west(tiles(k)%shrinked_data, idx_west_local)

               else if (is_on_east_edge(i,j)) then
                  call trimming_east(tiles(k)%shrinked_data, idx_east_local)    
               end if


               if (is_on_south_edge(i,j) .and. is_on_north_edge(i,j)) then
                  call trimming_south_north(tiles(k)%shrinked_data, idx_south_local, idx_north_local)
               
               else if (is_on_south_edge(i,j)) then
                  call trimming_south_north(tiles(k)%shrinked_data, idx_south_local, size(tiles(k)%shrinked_data, dim=2))

               else if (is_on_north_edge(i,j)) then
                  ! call trimming_north(tiles(k)%shrinked_data, idx_north_local)
                  call trimming_south_north(tiles(k)%shrinked_data, 1, idx_north_local)
               end if

               k = k + 1
            end if
         end do 
      end do

      call resize_longitude(lon, idx_west_global, idx_east_global, global_nx)
      call resize_latitude(lat, idx_south_global, idx_north_global, global_ny)

   end block trimming


   call nc%init(outnc, [size(lon), size(lat)])
   call mpi_barrier(mpi_comm_world, ierr)

   call nc%put_lonlat(lon, lat, count=[size(lon), size(lat)])


   parallel_io: block
      integer ::  n
      integer :: start_nc(2), count_nc(2) 

      k = 1
      do j = 1, numlat
         do i = 1, numlon
            if (distri_logical(i,j)) then

               if (is_on_west_edge(i,j) .and. is_on_east_edge(i,j)) then
               
                  start_nc(1) = 1
                  count_nc(1) = idx_east_local-idx_west_local+1

               else if (is_on_west_edge(i,j)) then

                  start_nc(1) = 1
                  count_nc(1) = local_nx-idx_west_global+1

               else if (is_on_east_edge(i,j)) then
                  start_nc(1) = 1 + (local_nx - idx_west_global + 1) + (i-2)*local_nx
                  count_nc(1) = idx_east_local

               else 
                  start_nc(1) = 1 + (local_nx - idx_west_global + 1) + (i-2)*local_nx
                  count_nc(1) = local_nx

               end if


               if (is_on_south_edge(i,j) .and. is_on_north_edge(i,j)) then
                  start_nc(2) = 1
                  count_nc(2) = idx_north_local - idx_south_local + 1

                  
               else if (is_on_south_edge(i,j)) then
                  ! print *, thisis, 'south edge'
                  start_nc(2) = 1
                  count_nc(2) = local_ny - idx_south_global +1


               else if (is_on_north_edge(i,j)) then
                  ! print *, thisis, 'north edge'
                  start_nc(2) = 1 + (local_ny - idx_south_global +1) + (j-2)*local_ny
                  count_nc(2) = idx_north_local


               else
                  ! print *, thisis, 'middle'
                  start_nc(2) = 1 + (local_ny-idx_south_global +1) + (j-2)*local_ny
                  count_nc(2) = local_ny
               end if
   
               ! print *, thisis, k, 'start_nc: ', start_nc(1)
               ! print *, thisis, k, 'count_nc: ', count_nc(1)

               call nc%put_elev(tiles(k)%shrinked_data, start=start_nc, count=count_nc)
               k = k + 1

            end if
         end do
      end do

      ! put_varはブロッキングするため、余りのプロセスでは空の書き込み命令を呼びだす。
      n = numlon*numlat
      if ( petot-mod((N/petot+1)*petot, N) < thisis) then
         call nc%put_elev_empty()
      end if

      print *, "image: ", thisis, " complete: ", trim(adjustl(outnc))
      call mpi_barrier(mpi_comm_world, ierr)

   end block parallel_io


   call nc%close()

   call mpi_finalize()

contains

   subroutine default(data_dir, outnc)
      implicit none
      character(*), intent(inout) :: data_dir, outnc

      if (trim(outnc) == '')    outnc = DEFAULT_OUT
      if (trim(data_dir) == '') data_dir = DEFAULT_DAT

   end subroutine default


end program main