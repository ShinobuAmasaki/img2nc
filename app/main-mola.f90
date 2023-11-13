program main
   use, intrinsic :: iso_fortran_env
   use netcdf
   use img2nc
   implicit none
   

   character(MAX_PATH_LEN) :: filestem, data_dir, iomsg
   character(:), allocatable :: filename, outnc
   integer :: ios
   type(LabelFile) :: lbl
   logical :: flag

   integer(int32), parameter :: nx = 11520, ny = 5632
   integer(int16) :: array(nx, ny)
   real(real64), allocatable :: lon(:), lat(:)

   type(buff) :: buff_single

   integer(int32) :: ncid
   integer(int32) :: dim_id_lon, dim_id_lat
   integer(int32) :: var_id_lon, var_id_lat, var_id_elev

   type(boundary) :: edge

   real(real64) :: step_lon, step_lat

   data_dir = 'mola-megdr/meg128'
   filestem = 'megr44n180hb'
   outnc = 'mola.nc'

   call edge%set_east(270)
   call edge%set_west(180)
   call edge%set_south(0)
   call edge%set_north(44)

   read_lbls: block
      integer(int32) :: uni = 100

      filename = trim(adjustl(data_dir))//'/'//trim(adjustl(filestem))//'.lbl'
      ! filename = 'sldem2013/lon000/DTM_MAP_01_N25E000N24E001SC.lbl'

      print *, filename
      inquire(file=filename, exist=flag)
      print *, flag

      call lbl%init()

      open(uni, file=filename, form='unformatted', access='stream', status='old', iostat=ios, iomsg=iomsg)
      if (ios /= 0) then
         print *, "I/O error"
         print *, iomsg
      end if
      
      call lbl%load_file(uni)      
      call lbl%read_buff()
      close(uni)

   end block read_lbls


   read_imgs: block
      integer :: uni = 100
      integer :: q

      filename = trim(adjustl(data_dir))//'/'//trim(adjustl(filestem))//'.img'

      print *, filename
      inquire(file=filename, exist=flag)
      print *, flag

      allocate(buff_single%data(nx, ny))

      call buff_single%read_data(uni, file=filename)

      call buff_single%big_to_little() 


      do concurrent(q = 1:ny)
         array(:, q) = buff_single%data(:, ny-q+1)
      end do

      deallocate(buff_single%data)

   end block read_imgs
      

   init_nc: block
      call check( nf90_create(outnc, NF90_HDF5, ncid))
      
      call check( nf90_def_dim(ncid, 'longitude', nx, dim_id_lon))
      call check( nf90_def_dim(ncid, 'latitude', ny, dim_id_lat))
      
      call check( nf90_def_var(ncid, 'longitude', NF90_DOUBLE, dim_id_lon, var_id_lon))
      call check( nf90_def_var(ncid, 'latitude', NF90_DOUBLE, dim_id_lat, var_id_lat))
      call check( nf90_def_var(ncid, 'elevation', NF90_INT, [dim_id_lon, dim_id_lat], var_id_elev, deflate_level=1))

      call check( nf90_put_att(ncid, var_id_lon, 'units', 'deg.'))
      call check( nf90_put_att(ncid, var_id_lat, 'units', 'deg.'))
      call check( nf90_put_att(ncid, var_id_elev, 'units', 'meters'))

      call check( nf90_enddef(ncid))
   end block init_nc

   set_step_nums: block
      step_lon = (dble(edge%get_east()) - dble(edge%get_west()))/dble(nx)
      step_lat = (dble(edge%get_north()) - dble(edge%get_south()))/dble(ny)
   end block set_step_nums

   print *, step_lon, step_lat

   lonlat_prepare: block 
      integer :: i, j
      real(real64) :: south, west


      allocate(lon(nx))
      allocate(lat(ny))

      west = dble(edge%get_west())
      south = dble(edge%get_south())

      print *, west, south
      do concurrent(i = 1:nx)
         lon(i) = dble(i-1)*step_lon + west
      end do

      do concurrent(j = 1:ny)
         lat(j) = dble(j)*step_lat + south
      end do

   end block lonlat_prepare


   lonlat_putvar: block
     
      call check( nf90_put_var(ncid, var_id_lon, lon, start=[1], count=[nx]))
      call check( nf90_put_var(ncid, var_id_lat, lat, start=[1], count=[ny]))

   end block lonlat_putvar

   call check(nf90_put_var(ncid, var_id_elev, array, start=[1,1], count=[nx, ny]))
   call check (nf90_close(ncid))

contains

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

end program main