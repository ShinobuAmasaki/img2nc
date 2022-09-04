module mod_array_division
   use, intrinsic :: iso_fortran_env
   use mpi_f08
   use mod_read_img
   implicit none

   type :: global_area
      integer(int32) :: west, east, south, north
      integer(int32) :: n_div, n_mod
      integer(int32) :: nlon_img, nlat_img ! number of imgs.
      integer(int32) :: nlon ! for lon
      integer(int32) :: nlat ! for lat
      integer(int32), allocatable :: local_nlon(:), local_nlat(:)
   contains
      procedure :: preload_global_area_setting
      procedure :: gather_local_nlon_nlat
      procedure :: set_nlon_nlat => global_set_nlon_nlat
   end type global_area

   type :: local_area
      integer(int32) :: nlon, nlat
      integer(int32) :: nlon_img, nlat_img
      integer(int32) :: lon_begin, lon_end
      integer(int32) :: lon_e_begin, lon_e_end
      integer(int32) :: lat_begin, lat_end
      integer(int32) :: lat_e_begin, lat_e_end
   contains
      procedure :: preload_local_area_setting
      procedure :: divide_array_index
      procedure :: set_nlon_nlat => local_set_nlon_nlat
   end type local_area

contains

   subroutine divide_array_index(self,global, this_number, priority)
      class(local_area), intent(inout) :: self
      type(global_area), intent(in) :: global
      integer(int32), intent(in) :: this_number
      character(len=*), intent(in) :: priority
      integer(int32) :: n_mod, n_div, n_begin, n_end

      n_mod = global%n_mod
      n_div = global%n_div

      if (n_mod /= 0) then
         !割り切れない場合
         if (this_number <= n_mod) then
            !余りを分配する前方のイメージについて
            n_begin = (n_div + 1)*(this_number - 1) + 1
            n_end   = n_begin + n_div 
         
         else
            !余りを分配しない後方のイメージについて
            n_begin = (n_div + 1)*n_mod + n_div*(this_number - n_mod - 1) + 1
            n_end   = n_begin + n_div - 1
         end if

      else
         !割り切れる場合 
         n_begin = n_div*(this_number - 1) + 1
         n_end   = n_begin + n_div - 1
      end if 

      if (priority == 'lon') then
         !経度方向を優先
         self%lat_begin = n_begin
         self%lat_end = n_end
         self%lon_begin = 1
         self%lon_end = global%nlon_img
      
      else if (priority == 'lat') then
         !緯度方向を優先
         self%lon_begin = n_begin
         self%lon_end   = n_end
         self%lat_begin = 1
         self%lat_end   = global%nlat_img
      end if

   end subroutine divide_array_index


   subroutine preload_local_area_setting(self, global, this_number)
      class(local_area), intent(inout) :: self
      type(global_area), intent(in) :: global
      integer(int32), intent(in) :: this_number
      integer(int32) :: n_mod, n_div, k

      self%nlat_img = global%nlat_img

      n_div = global%n_div
      n_mod = global%n_mod

      if (this_number <= n_mod) then

         self%nlon_img = n_div + 1
      else

         self%nlon_img = n_div
      end if

   end subroutine preload_local_area_setting


   subroutine local_set_nlon_nlat(self, single)
      class(local_area), intent(inout) :: self
      type(Tile), intent(in) :: single

      self%nlon = size(single%data, dim=1)
      self%nlat = size(single%data, dim=2)

   end subroutine local_set_nlon_nlat


   subroutine preload_global_area_setting(self, name_list, petot)
      class(global_area), intent(inout) :: self
      character(len=*), intent(in) :: name_list(:,:)
      integer(int32), intent(in) :: petot
      
      self%nlon_img = size(name_list, dim=1)
      self%nlat_img = size(name_list, dim=2)

      self%n_div = self%nlon_img / petot
      self%n_mod = mod(self%nlon_img, petot)

   end subroutine preload_global_area_setting
      

   subroutine gather_local_nlon_nlat(self, local, comm, ierr)
      class(global_area), intent(inout) :: self
      type(local_area), intent(in) :: local
      type(mpi_comm), intent(in) :: comm
      integer(int32), intent(out) :: ierr

      self%local_nlon(1) = local%nlon
      self%local_nlat(1) = local%nlat

      call mpi_allgather(self%local_nlon(1), 1, mpi_integer4, self%local_nlon(1), 1, mpi_integer4, comm, ierr)
      call mpi_allgather(self%local_nlat(1), 1, mpi_integer4, self%local_nlat(1), 1, mpi_integer4, comm, ierr)

   end subroutine gather_local_nlon_nlat

   subroutine global_set_nlon_nlat(self, priority)
      class(global_area), intent(inout) :: self
      character(len=*), intent(in) :: priority
      integer(int32) :: nlon, nlat

      if (priority == 'lat') then
         nlon = sum(self%local_nlon(:), dim=1)
         nlat = self%local_nlat(1)

      else if (priority == 'lon') then
         nlon = self%local_nlon(1)
         nlat = sum(self%local_nlat(:), dim=1)
      
      end if
      
      self%nlon = nlon
      self%nlat = nlat

   end subroutine global_set_nlon_nlat

end module mod_array_division