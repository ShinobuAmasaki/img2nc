module global_m
   use, intrinsic :: iso_fortran_env
   use :: mpi_f08
   implicit none
   
   integer(int32) :: petot, rank, thisis
   logical :: isIm1

contains

   subroutine mpi_initialize(ierr)
      implicit none
      integer(int32) :: ierr
      
      call mpi_init(ierr)
      call mpi_comm_size(mpi_comm_world, petot, ierr)
      call mpi_comm_rank(mpi_comm_world, rank, ierr)
      thisis = rank + 1 ! 1始まりに変更する。

      isIm1 = (thisis == 1)
   end

   subroutine gently_stop()
      use base_m
      implicit none
      integer :: ierr

      if(isIm1) call print_usage()
      call mpi_finalize(ierr)
      stop

   end subroutine gently_stop

   subroutine gently_stop_quiet()
      implicit none
      integer :: ierr
      
      call mpi_initialize(ierr)

   end subroutine gently_stop_quiet
     

end module global_m