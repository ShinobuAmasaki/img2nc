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
   character(len=15) :: range

   integer(int32), allocatable :: distri_1d(:), distri_2d(:,:)
   logical, allocatable :: distri_logical(:, :)

   character(len=MAX_PATH_LEN), allocatable :: file_list(:,:)
   character(len=MAX_PATH_LEN), allocatable :: file_list_local(:)
   
   type(boundary) :: edge, outline
   integer(int32) :: coarse !, resolution

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
   integer(int32) :: idx_w_global, idx_e_global
   integer(int32) :: idx_west_local, idx_east_local
!=====================================================================!

   call mpi_initialize(ierr)
   
   call default(data_dir, outnc, coarse, edge)

   call preprocess(data_dir, outnc, range, coarse, edge)


   outline = outline_mola(edge)


   call allocate_lists(outline, file_list, distri_1d, distri_2d, distri_logical)

   call create_name_list(data_dir, file_list, outline)

   call set_distribution(distri_1d, distri_2d, distri_logical, outline)


   numlon = get_siz_lon_meg128(outline%get_west(), outline%get_east())
   numlat = get_siz_lat_meg128(outline%get_south(), outline%get_north())

   triming_prepare: block
      allocate(is_on_west_edge(numlon, numlat))
      allocate(is_on_east_edge(numlon, numlat))

      call set_is_on_west_edge(is_on_west_edge)
      call set_is_on_east_edge(is_on_east_edge)

   end block triming_prepare


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

      if (isIm1) then
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
      end if
   end block lonlat_prepare

  
   print *, size(tiles(1)%shrinked_data, dim=1), size(tiles(1)%shrinked_data, dim=2)   
   triming: block

      

      do i = 1, global_nx
         if (edge%get_west() <= lon(i)) then
            idx_w_global = i
            exit 
         end if
      end do 

      do i = global_nx, 1, -1
         if (lon(i) <= edge%get_east()) then
            idx_e_global = i
            exit
         end if
      end do


      idx_west_local = idx_w_global
      idx_east_local = mod(idx_e_global, local_nx)
      if (idx_east_local == 0) idx_east_local =  local_nx

      k = 1 
      do j = 1, numlat
         do i = 1, numlon
            if (distri_logical(i, j)) then
               if (is_on_west_edge(i,j) .and. is_on_east_edge(i,j)) then
                  call triming_west_east(tiles(k)%shrinked_data, idx_west_local, idx_east_local)
               
               else if (is_on_west_edge(i,j)) then
                  call triming_west(tiles(k)%shrinked_data, idx_west_local)

               else if (is_on_east_edge(i,j)) then
                  call triming_east(tiles(k)%shrinked_data, idx_east_local)
               
               end if

               k = k + 1
            end if
         end do 
      end do

      print *, size(tiles(1)%shrinked_data, dim=1), size(tiles(1)%shrinked_data, dim=2)

   end block triming


   call nc%init(outnc, [idx_e_global-idx_w_global+1, global_ny])
   call mpi_barrier(mpi_comm_world, ierr)

   call nc%put_lonlat(lon(idx_w_global:idx_e_global), lat, count=[idx_e_global-idx_w_global+1, global_ny])


   parallel_io: block
      integer ::  n 
      integer :: start_nc(2), count_nc(2)
      k = 1
      do j = 1, numlat
         do i = 1, numlon
            if (distri_logical(i,j)) then

               if (is_on_west_edge(i,j) .and. is_on_east_edge(i,j)) then
               
                  start_nc(:) = [(i-1)*local_nx+1, (numlat-j)*local_ny+1]
                  count_nc(:) = [idx_east_local-idx_west_local+1, local_ny]
                  
               else if (is_on_west_edge(i,j)) then

                  start_nc(:) = [(i-1)*local_nx+1, (numlat-j)*local_ny+1]
                  count_nc(:) = [local_nx-idx_w_global+1, local_ny]


               else if (is_on_east_edge(i,j)) then
                  start_nc(:) = [(i-1)*local_nx-idx_w_global+2, (numlat-j)*local_ny+1]
                  count_nc(:) = [idx_east_local, local_ny]
               else 
                  start_nc(:) = [(i-1)*local_nx-idx_w_global+2, (numlat-j)*local_ny+1]
                  count_nc(:) = [local_nx, local_ny]
               end if
   

               print *, k, 'start_nc: ', start_nc
               print *, k, 'count_nc: ', count_nc
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

   subroutine default(data_dir, outnc, coarse, edge)
      implicit none
      character(*), intent(inout) :: data_dir, outnc
      type(boundary), intent(out) :: edge
      integer(int32) ::  coarse

      coarse = 16
      outnc = './mola.nc'
      data_dir = './mola-megdr'

      call edge%set_west(-180)
      call edge%set_east(180)
      call edge%set_south(-90)
      call edge%set_north(90)

   end subroutine default
      

   subroutine set_is_on_west_edge(is_on_west_edge)
      implicit none
      logical :: is_on_west_edge(:,:)

      is_on_west_edge(:, :) = .false.

      is_on_west_edge(1, :) = .true.

   end subroutine set_is_on_west_edge


   subroutine set_is_on_east_edge(is_on_east_edge)
      implicit none
      logical :: is_on_east_edge(:,:)

      is_on_east_edge(:, :) = .false.

      is_on_east_edge(size(is_on_east_edge, dim=1), :) = .true.

   end subroutine set_is_on_east_edge

   
   subroutine triming_west_east(array, idx_w, idx_e)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:,:)
      integer(int32), intent(in) :: idx_w, idx_e

      integer(int16), allocatable :: buff(:, :)
      integer(int32) :: ny

      ny = size(array, dim=2)

      allocate(buff(idx_e-idx_w+1, ny))

      buff(:,:) = array(idx_w:idx_e, :)

      deallocate(array)
      allocate(array(idx_e-idx_w+1, ny))
      array(:,:) = buff(:, :)
   end subroutine triming_west_east


   subroutine triming_west (array, idx_w)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:, :)
      integer(int32), intent(in) :: idx_w

      integer(int16), allocatable :: buff(:, :)
      integer(int32) :: nx, ny

      nx = size(array, dim=1)
      ny = size(array, dim=2)

      allocate(buff(nx-idx_w+1, ny))

      buff(:, :) = array(idx_w:nx, :)

      deallocate(array)
      allocate(array(nx-idx_w+1, ny))

      array(:,:) = buff(:, :)

   end subroutine triming_west


   subroutine triming_east (array, idx_e)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:,:)
      integer(int32), intent(in) :: idx_e

      integer(int16), allocatable :: buff(:,:)
      integer(int32) :: ny

      ny = size(array, dim=2)

      allocate(buff(idx_e, ny))

      buff(:,:) = array(1:idx_e, :)

      deallocate(array)
      allocate(array(idx_e, ny))

      array(:,:) = buff(:,:)
   end subroutine triming_east 

   



end program main