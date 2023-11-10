module buff_t
   use, intrinsic :: iso_fortran_env
   implicit none

   type buff
      integer(int16), allocatable :: data(:,:)
   contains
      procedure :: shrink
   end type

contains

   subroutine shrink (self, coarse)
      implicit none
      class(buff) :: self
      integer(int32), intent(in) :: coarse
      integer(int32), allocatable :: cache(:,:)
      integer(int32) :: siz_i, siz_j
      integer(int32) :: i, j, ii, jj
      real(real64) :: denomi 


      siz_i = size(self%data, dim=1)
      siz_j = size(self%data, dim=2)


      allocate(cache(siz_i, siz_j))

      cache(:,:) = self%data(:,:)

      deallocate(self%data)
      
      allocate(self%data(siz_i/coarse, siz_j/coarse))


      denomi = 1d0/dble(coarse**2)
      do j = 1, siz_j, coarse
         do i = 1, siz_i, coarse
            ii = ceiling(i/dble(coarse))
            jj = ceiling(j/dble(coarse))

            self%data(ii,jj) = nint( sum(cache(i:i+coarse-1, j:j+coarse-1)) * denomi , kind=int16)

         end do
      end do

      deallocate(cache) 

   end subroutine shrink


end module buff_t