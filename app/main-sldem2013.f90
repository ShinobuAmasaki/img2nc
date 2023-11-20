program main
   use, intrinsic :: iso_fortran_env
   use mpi_f08
   use netcdf
   use img2nc
   use sldem2013
   implicit none
   
   integer(int32) :: ierr, itag
   integer(int32) :: i, j, k, p, q, r
   type(mpi_status) :: istatus

   integer(int32), allocatable :: distri_1d(:), distri_2d(:,:)
   logical, allocatable :: distri_logical(:, :)

   character(len=MAX_PATH_LEN), allocatable :: file_list(:,:)
   character(len=MAX_PATH_LEN), allocatable :: file_list_local(:)
   integer(int32) :: n_jobs, n_jobs_total

   character(len=MAX_PATH_LEN) :: filename, data_dir
   character(len=MAX_NAME_LEN) :: outnc
   character(len=15) :: range
   type(boundary) :: edge
   integer(int32) :: coarse

   integer(int32) :: numlon, numlat
   real(real64) :: step_lon, step_lat
   real(real64), allocatable :: lon(:), lat(:)

   ! Input/Output
   integer :: uni, ios

   ! Magic parameters
   integer(int32) :: nx
   integer(int32) :: ny

   integer(int32) :: global_nx
   integer(int32) :: global_ny
   integer(int32) :: local_nx
   integer(int32) :: local_ny

   character(:), allocatable :: cache

   real(real64) :: offset_x, offset_y

   type(buff), target :: buff_single, buff_double(2)

   type(tile), allocatable :: tiles(:)
   type(LabelFile), allocatable :: lbls(:)
   logical, allocatable :: finish_flag(:)

   ! for netcdf
   type(nc_t) :: nc


!---------------------------------------------------------------------!
!---------------------------------------------------------------------!
   call mpi_initialize(ierr)

!---------------------------------------------------------------------!
   ! Validate command line arguments. 
   call default(data_dir, outnc, coarse, edge)

   ! Read parameter from command line arguments. 
   call preprocess(data_dir, outnc, range, coarse, edge)
   

   ! 入力処理用の名簿を作成する。
   call allocate_lists(edge, file_list, distri_1d, distri_2d, distri_logical)
   call create_name_list(data_dir, edge, file_list)

   ! データ分散を司るリストを作成する。
   call set_distribution(distri_1d, distri_2d, distri_logical)

   numlon = size(distri_2d, dim=1)
   numlat = size(distri_2d, dim=2)


   block
      ! n_jobs = count(distri_logical)
      n_jobs_total = numlat*numlon
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


   allocate(tiles(n_jobs))
   allocate(lbls(n_jobs))


   read_lbls: block 
      integer(int32) :: uni = 100
      do k = 1, n_jobs
         
         filename = trim(adjustl(file_list_local(k)))//'.lbl'

         call lbls(k)%init()

         open(uni, file=filename, form='unformatted', access='stream', status='old' )
         call lbls(k)%load_file(uni, ios=ios)
         call lbls(k)%read_buff()
         close(uni)

      end do
   end block read_lbls 

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

   ! 粗視化の
   set_step_nums: block
      local_nx = nx/coarse
      local_ny = ny/coarse

      global_nx = nx*numlon/coarse
      global_ny = ny*numlat/coarse

      step_lon = (dble(edge%get_east()) - dble(edge%get_west()))/dble(global_nx)
      step_lat = (dble(edge%get_north()) - dble(edge%get_south()))/dble(global_ny)
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

         do concurrent(q = 1:local_ny)
            tiles(k)%shrinked_data(:,q) = buff_single%data(:, local_ny-q+1)
         end do

         deallocate(buff_single%data)

         print '(a, a, i5, a, i5, a)', trim(tiles(k)%get_path()), ': loaded. (', k, '/', n_jobs, ')'

      end do
   end block sync_io

   call nc%init(outnc, [global_nx, global_ny])
   call mpi_barrier(mpi_comm_world, ierr)


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

         west = dble(edge%get_west())
         south = dble(edge%get_south())

         do concurrent(i = 1:global_nx)
            lon(i) = dble(i-1)*step_lon + west + offset_x
         end do

         do concurrent(j = 1:global_ny)
            lat(j) = dble(j)*step_lat + south + offset_y
         end do
      end if
   end block lonlat_prepare

  
   call nc%put_lonlat(lon, lat, count=[global_nx, global_ny])


   parallel_io: block
      integer ::  n 
      integer :: start_nc(2), count_nc(2)
      k = 1
      do j = 1, numlat
         do i = 1, numlon

            if (distri_logical(i,j)) then


               start_nc(:) = [(i-1)*local_nx+1, (j-1)*local_ny+1]
               count_nc(:) = [local_nx, local_ny]

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

      print *, "image: ", thisis, " complete."
      call mpi_barrier(mpi_comm_world, ierr)
   
   end block parallel_io


   ! NetCDF Finalize
   call nc%close()

!---------------------------------------------------------------------!
!---------------------------------------------------------------------!
   call mpi_finalize()
   
contains

   subroutine default(data_dir, outnc, coarse, edge)
      implicit none
      character(*), intent(inout) :: data_dir, outnc
      type(boundary), intent(out) :: edge
      integer(int32) :: coarse

      coarse = 16
      outnc = './out.nc'
      data_dir = './sldem2013'

      call edge%set_east(4)
      call edge%set_west(-4)
      call edge%set_south(-4)
      call edge%set_north(4)

   end subroutine default     

end program main