module argument_t
   implicit none
   
   type, public :: argument
      character(:), allocatable :: v
   end type

end module argument_t