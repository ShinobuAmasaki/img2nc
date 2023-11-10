module buff_t
   use, intrinsic :: iso_fortran_env
   implicit none

   type buff
      integer(int16), allocatable :: data(:,:)
   end type 

end module buff_t