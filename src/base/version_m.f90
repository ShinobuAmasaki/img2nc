module version_m
   implicit none
   private
   
   character(5), parameter :: version_str = '3.0.0'

   public :: version

contains

   function version()
      implicit none
      character(:), allocatable :: version

      version = trim(adjustl(version_str))
   end function version

end module version_m