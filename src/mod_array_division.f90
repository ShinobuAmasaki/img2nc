module mod_array_division
   use, intrinsic :: iso_fortran_env
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
   end type global_area

   type :: local_area
      integer(int32) :: nlon, nlat
      integer(int32) :: nlon_img, nlat_img
      integer(int32) :: i_begin, i_end
      integer(int32) :: i_e_begin, i_e_end
      ! integer(int32) :: j_begin, j_end
      ! integer(int32) :: j_e_begin, j_e_end
   contains
      procedure :: preload_local_area_setting
      procedure :: divide_array_index
   end type local_area

contains

   subroutine divide_array_index(self,global, this_number)
      class(local_area), intent(inout) :: self
      type(global_area), intent(in) :: global
      integer(int32), intent(in) :: this_number
      integer(int32) :: n_mod, n_div, i_begin, i_end

      n_mod = global%n_mod
      n_div = global%n_div

      if (n_mod /= 0) then
         !割り切れない場合
         if (this_number <= n_mod) then
            !余りを分配する前方のイメージについて
            i_begin = (n_div + 1)*(this_number - 1) + 1
            i_end   = i_begin + n_div 
         
         else
            !余りを分配しない後方のイメージについて
            i_begin = (n_div + 1)*n_mod + n_div*(this_number - n_mod - 1) + 1
            i_end   = i_begin + n_div - 1
         end if

      else
         !割り切れる場合 
         i_begin = n_div*(this_number - 1) + 1
         i_end   = i_begin + n_div - 1
      end if 

      self%i_begin = i_begin
      self%i_end   = i_end 

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


   subroutine preload_global_area_setting(self, name_list, petot)
      class(global_area), intent(inout) :: self
      character(len=*), intent(in) :: name_list(:,:)
      integer(int32), intent(in) :: petot
      
      self%nlon_img = size(name_list, dim=1)
      self%nlat_img = size(name_list, dim=2)

      self%n_div = self%nlon_img / petot
      self%n_mod = mod(self%nlon_img, petot)

   end subroutine preload_global_area_setting
      

end module mod_array_division