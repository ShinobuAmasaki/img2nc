program main
   use, intrinsic :: iso_fortran_env
   use mpi_f08
   use netcdf
   use img2nc
   use global_m
   use buff_t
   use Tile_m
   implicit none
   
   integer(int32) :: ierr, itag
   integer(int32) :: i, j, p, q, r
   type(mpi_status) :: istatus

   integer(int32), allocatable :: distri_1d(:), distri_2d(:,:)
   logical, allocatable :: distri_logical(:, :)
   character(len=MAX_PATH_LEN), allocatable :: file_list(:,:)

   character(len=MAX_PATH_LEN) :: filename, data_dir
   character(len=MAX_NAME_LEN) :: outnc
   character(len=15) :: range
   type(boundary) :: edge
   integer(int32) :: coarse

   integer(int32) :: numlon, numlat
   real(real64) :: step_lon, step_lat
   real(real64), allocatable :: lon(:), lat(:)

   integer, parameter :: MAX_BUFF_SLOT = 64

   ! Input/Output
   integer :: uni, ios, progress, howmany

   ! Magic parameters
   integer(int32), parameter :: nx = 4096
   integer(int32), parameter :: ny = 4096

   integer(int32) :: global_nx
   integer(int32) :: global_ny
   integer(int32) :: local_nx
   integer(int32) :: local_ny

   real(real64) :: offset_x, offset_y

   type(buff), target :: buff_single

   type(tile), allocatable :: tiles(:, :)

   ! for netcdf
   integer(int32) :: ncid
   integer(int32) :: dim_id_lon, dim_id_lat
   integer(int32) :: var_id_lon, var_id_lat, var_id_elev

!---------------------------------------------------------------------!
!---------------------------------------------------------------------!
   call mpi_initialize(ierr)

!---------------------------------------------------------------------!
!- コマンドライン引数のバリデーション
   call default(data_dir, outnc, coarse, edge)
   call preprocess(data_dir, outnc, range, coarse, edge)

!- 入力処理用の名簿を作成する。
   call allocate_lists(edge, file_list, distri_1d, distri_2d, distri_logical)
   call create_filelist(data_dir, edge, file_list)

!- データ分散を司るリストを作成する。
   call set_distribution(distri_1d, distri_2d, distri_logical)

!- データ読み込み
   call allocate_tiles(tiles, distri_2d)

   numlon = size(distri_2d, dim=1)
   numlat = size(distri_2d, dim=2)

   uni = 100 + thisis

   block
      global_nx = nx*numlon/coarse
      global_ny = ny*numlat/coarse
      local_nx = nx/coarse
      local_ny = ny/coarse
   end block

   block
      step_lon = (dble(edge%get_east()) - dble(edge%get_west()))/dble(global_nx)
      step_lat = (dble(edge%get_north()) - dble(edge%get_south()))/dble(global_ny)
   end block

   block
      offset_x = +step_lon/2d0 
      offset_y = -step_lat/2d0
      ! offset_x = +step_lon*(0.5d0 - 1d0/(2d0*coarse)) 
      ! offset_y = -step_lat*(0.5d0 - 1d0/(2d0*coarse))
   end block

   block
      howmany = count(distri_logical)
      progress = 1 
   end block

   ! データの読み込み
   do j = 1, numlat
      do i = 1, numlon
         if (distri_logical(i,j)) then

            allocate(buff_single%data(nx, ny))

            allocate(tiles(i,j)%shrinked_data(local_nx, local_ny))
            buff_single%data(:,:) = 0_int16

            filename = trim(adjustl(file_list(i,j)))//'.img'

            call tiles(i,j)%set_path(filename)

            tiles(i,j)%p_data => buff_single

            open(uni, file=tiles(i,j)%get_path(), access='stream', iostat=ios, status='old', form='unformatted')
            read(uni, iostat=ios) buff_single%data(:,:)
            close(uni)

            

            call big_to_little(buff_single%data)

            if (coarse > 1) call buff_single%shrink(coarse)

            do q = 1, local_ny
               tiles(i,j)%shrinked_data(:, q) = buff_single%data(:, local_ny-q+1)
            end do

            deallocate(buff_single%data)

            nullify(tiles(i,j)%p_data)

            print '(a, a, i5, a, i5, a)', trim(tiles(i, j)%get_path()), ': loaded. (', progress, '/', howmany, ')'
            progress = progress + 1

         end if
      end do
   end do

   
   ! Define the outputting netcdf file
   block
      call check( nf90_create_par(outnc, NF90_HDF5, mpi_comm_world%mpi_val, mpi_info_null%mpi_val, ncid))
      
      call check( nf90_def_dim(ncid, 'longitude', global_nx, dim_id_lon))
      call check( nf90_def_dim(ncid, 'latitude', global_ny, dim_id_lat))
      
      call check( nf90_def_var(ncid, 'longitude', NF90_DOUBLE, dim_id_lon, var_id_lon))
      call check( nf90_def_var(ncid, 'latitude', NF90_DOUBLE, dim_id_lat, var_id_lat))
      call check( nf90_def_var(ncid, 'elevation', NF90_INT, [dim_id_lon, dim_id_lat], var_id_elev, deflate_level=1))

      call check( nf90_put_att(ncid, var_id_lon, 'units', 'deg.'))
      call check( nf90_put_att(ncid, var_id_lat, 'units', 'deg.'))
      call check( nf90_put_att(ncid, var_id_elev, 'units', 'meters'))

      call check( nf90_enddef(ncid))
   end block
   call mpi_barrier(mpi_comm_world, ierr)


   block
      integer :: i, j
      real(real64) :: south, west
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
   end block
   call mpi_barrier(mpi_comm_world, ierr)
   
   block
      if (isIm1) then
         call check( nf90_put_var(ncid, var_id_lon, lon, start=[1], count=[global_nx]))
         call check( nf90_put_var(ncid, var_id_lat, lat, start=[1], count=[global_ny]))
      end if
   end block

   block
      integer :: i, j, n 
      integer :: start_nc(2), count_nc(2)
      do j = 1, numlat
         do i = 1, numlon

            if (distri_logical(i,j)) then


               start_nc(:) = [(i-1)*local_nx+1, (j-1)*local_ny+1]
               count_nc(:) = [local_nx, local_ny]

               call check(nf90_put_var(ncid, var_id_elev, tiles(i,j)%shrinked_data, start=start_nc, count=count_nc))

            end if
         end do
      end do

      ! put_varはブロッキングするため、余りのプロセスでは空の書き込み命令を呼びだす。
      n = numlon*numlat
      if ( petot-mod((N/petot+1)*petot, N) < thisis) then
         call check(nf90_put_var(ncid, var_id_elev, [0d0], start=[1,1], count=[0, 0]))
      end if

      print *, "image: ", thisis, " complete."
      call mpi_barrier(mpi_comm_world, ierr)
   end block

   call check (nf90_close(ncid))

!---------------------------------------------------------------------!
!---------------------------------------------------------------------!
   call mpi_finalize(ierr)
   
contains

   subroutine allocate_tiles(tiles, distri_2d)
      implicit none
      type(Tile), allocatable, intent(inout) :: tiles(:,:)
      integer(int32), intent(in) :: distri_2d(:,:)

      integer(int32) :: numlon, numlat 

      numlon = size(distri_2d, dim=1)
      numlat = size(distri_2d, dim=2)

      allocate(tiles(numlon, numlat))
   end subroutine allocate_tiles


   subroutine default(data_dir, outnc, coarse, edge)
      implicit none
      character(*), intent(inout) :: data_dir, outnc
      type(boundary), intent(out) :: edge
      integer(int32) :: coarse

      coarse = 16
      outnc = './out.nc'
      data_dir = './dat'

      call edge%set_east(16)
      call edge%set_west(0)
      call edge%set_south(0)
      call edge%set_north(16)

   end subroutine default

   subroutine check(status)
      implicit none
      integer, intent(in) :: status
      integer :: ierr

      if (status /= nf90_noerr) then
         ! ステータスコードから対応するエラーメッセージを取得し、コンソールに表示する。
         print '(a,i3,a)', 'PE ', thisis, ': '//trim(nf90_strerror(status))
         ! call mpi_finalize(ierr)
         stop "Stopped"
      end if

   end subroutine check

   pure function swap16(value)
      implicit none
      integer(int16), intent(in) :: value
      integer(int16) :: swap16

      swap16 = ishft(value,8)

      swap16 = ior(swap16, ishft(value, -8))

   end function swap16


   subroutine big_to_little(array)
      implicit none
      integer(int16), intent(inout) :: array(:,:)
      integer(int32) :: siz_i, siz_j, i, j

      siz_i = size(array, dim=1)
      siz_j = size(array, dim=2)

      do concurrent (j = 1:siz_j)
         do concurrent (i = 1:siz_i)
            array(i,j) = swap16(array(i,j))
         end do
      end do 
   end subroutine big_to_little
      

end program main