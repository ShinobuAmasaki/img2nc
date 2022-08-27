module mod_array_division
   use, intrinsic :: iso_fortran_env
   implicit none

   type global_area
      integer(int32) :: west, east, south, north
      integer(int32) :: n_div, n_mod
      integer(int32) :: nx_img, ny_img ! number of imgs.
      integer(int32) :: nx, ny ! number of total elements. use these to allocate coarray(:,:)[:]
   end type global_area

   type local_area
      integer(int32) :: nx, ny
      integer(int32) :: nx_img, ny_img
      integer(int32) :: i_begin, i_end
      integer(int32) :: i_e_begin, i_e_end
   end type local_area

end module mod_array_division