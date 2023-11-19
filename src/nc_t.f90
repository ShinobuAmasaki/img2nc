module nc_c
   use, intrinsic :: iso_fortran_env
   use :: netcdf
   use :: mpi_f08
   use :: global_m
   implicit none
   private

   public :: check

   type :: dimid_t 
      integer(int32) :: lon, lat ! longitude & latitude
   end type
   
   type :: varid_t
      integer(int32) :: lon, lat 
      integer(int32) :: elev
   end type 
 
   type, public :: nc_t
      integer(int32) :: ncid
      type(dimid_t) :: dim_id
      type(varid_t) :: var_id
   contains
      procedure, public  :: init => nc_initialize
      procedure, public  :: put_lonlat => nc_put_var_lonlat
      procedure, public  :: put_elev => wrap__nf90_put_var_elevation
      procedure, public  :: put_elev_empty => nc_put_elev_empty
      procedure, public  :: close => wrap__nf90_close
      procedure, private :: create_par => wrap__nf90_create_par
      procedure, private :: define_dim => wrap__nf90_def_dim
      procedure, private :: define_var => wrap__nf90_def_var
      procedure, private :: put_attribute => wrap__nf90_put_att
      procedure, private :: end_define => wrap__nf90_enddef
      procedure, private :: wrap__nf90_put_var_lon
      procedure, private :: wrap__nf90_put_var_lat
   end type


   character(9), parameter :: LONGITUDE = 'longitude'
   character(8), parameter :: LATITUDE = 'latitude'
   character(9), parameter :: ELEVATION = 'elevation'
   character(5), parameter :: UNITS = 'units'
   character(6), parameter :: UNIT_METER = 'meters'
   character(4), parameter :: UNIT_DEGREE = 'deg.'

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

!--------------------------------------------------------------------------------------------!

   subroutine nc_initialize(self, path, nx_ny)
      implicit none
      class(nc_t) :: self
      character(*), intent(in) :: path
      integer(int32) :: nx_ny(2)

      call self%create_par(path, NF90_HDF5, MPI_COMM_WORLD, MPI_INFO_NULL)
      call self%define_dim(nx_ny)
      call self%define_var()
      call self%put_attribute()
      call self%end_define()

   end subroutine


   subroutine wrap__nf90_create_par(self, path, cmode, comm, info)
      implicit none
      class(nc_t) :: self
      character(*), intent(in)   :: path
      integer, intent(in)        :: cmode        !NF90_HDF5 is expected.  
      type(MPI_comm), intent(in) :: comm
      type(MPI_info), intent(in) :: info

      call check(nf90_create_par(path, cmode, comm%mpi_val, info%mpi_val, self%ncid))

   end subroutine wrap__nf90_create_par


   subroutine wrap__nf90_def_dim(self, nx_ny)
      implicit none
      class(nc_t) :: self
      integer(int32) :: nx_ny(2)

      call check( nf90_def_dim(self%ncid, LONGITUDE, nx_ny(1), self%dim_id%lon))
      call check( nf90_def_dim(self%ncid, LATITUDE, nx_ny(2), self%dim_id%lat))

   end subroutine wrap__nf90_def_dim


   subroutine wrap__nf90_def_var(self)
      implicit none
      class(nc_t) :: self

      call check( nf90_def_var(self%ncid, LONGITUDE, NF90_DOUBLE, self%dim_id%lon, self%var_id%lon))
      call check( nf90_def_var(self%ncid, LATITUDE, NF90_DOUBLE, self%dim_id%lat, self%var_id%lat))
      call check( nf90_def_var(self%ncid, ELEVATION, NF90_INT, [self%dim_id%lon, self%dim_id%lat],  &
                     self%var_id%elev, deflate_level=1))

   end subroutine wrap__nf90_def_var

   
   subroutine wrap__nf90_put_att(self)
      implicit none
      class(nc_t) :: self

      call check( nf90_put_att(self%ncid, self%var_id%lon, UNITS, UNIT_DEGREE))
      call check( nf90_put_att(self%ncid, self%var_id%lat, UNITS, UNIT_DEGREE))
      call check( nf90_put_att(self%ncid, self%var_id%elev, UNITS, UNIT_METER))

   end subroutine wrap__nf90_put_att


   subroutine wrap__nf90_enddef(self)
      implicit none
      class(nc_t) :: self

      call check( nf90_enddef(self%ncid))

   end subroutine wrap__nf90_enddef

!-------------------------------------------------------------------------------------------!
   
   subroutine nc_put_var_lonlat(self, lon, lat, count)
      implicit none
      class(nc_t) :: self
      real(real64), intent(in) :: lon(:), lat(:)
      integer(int32) :: count(2)

      if (isIm1) then
         call self%wrap__nf90_put_var_lon(lon, count(1))
         call self%wrap__nf90_put_var_lat(lat, count(2))
      end if
      
   end subroutine


   subroutine wrap__nf90_put_var_lon(self, array, count)
      implicit none
      class(nc_t) :: self
      real(real64), intent(in) :: array(:)
      integer(int32), dimension(:) :: count(1)

      call check(nf90_put_var(self%ncid, self%var_id%lon, array, start=[1], count=count))

   end subroutine wrap__nf90_put_var_lon


   subroutine wrap__nf90_put_var_lat(self, array, count)
      implicit none
      class(nc_t) :: self
      real(real64), intent(in) :: array(:)
      integer(int32), dimension(:) :: count(1)

      call check(nf90_put_var(self%ncid, self%var_id%lat, array, start=[1], count=count))

   end subroutine wrap__nf90_put_var_lat

!-------------------------------------------------------------------------------------------!

   subroutine wrap__nf90_put_var_elevation(self, array, start, count)
      implicit none
      class(nc_t) :: self
      integer(int16), intent(in) :: array(:,:)
      integer(int32), intent(in) :: start(2)
      integer(int32), intent(in) :: count(2)


      call check(nf90_put_var(self%ncid, self%var_id%elev, array, start=start, count=count))
   end subroutine wrap__nf90_put_var_elevation


   subroutine nc_put_elev_empty (self)
      implicit none
      class(nc_t) :: self

      call check(nf90_put_var(self%ncid, self%var_id%elev, [0], start=[1,1], count=[0,0]))

   end subroutine
!-------------------------------------------------------------------------------------------!

   subroutine wrap__nf90_close(self)
      implicit none
      class(nc_t) :: self

      call check( nf90_close(self%ncid))

   end subroutine wrap__nf90_close

end module nc_c