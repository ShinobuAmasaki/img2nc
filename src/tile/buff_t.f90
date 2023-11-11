module buff_t
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   implicit none
   private

   public :: buff

   type buff
      integer(int16), allocatable :: data(:,:)
   contains
      procedure :: shrink
      procedure :: big_to_little
      procedure :: read_data => read_data_from_file
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
      do concurrent ( j = 1:siz_j:coarse)
         do concurrent (i = 1:siz_i:coarse)

            ii = ceiling(i/dble(coarse))
            jj = ceiling(j/dble(coarse))

            self%data(ii,jj) = nint( sum(cache(i:i+coarse-1, j:j+coarse-1)) * denomi , kind=int16)

         end do
      end do

      deallocate(cache) 

   end subroutine shrink


   subroutine big_to_little(self)
      use :: base_m 
      implicit none
      class(buff) :: self
      integer(int32) :: siz_i, siz_j, i, j

      siz_i = size(self%data, dim=1)
      siz_j = size(self%data, dim=2)

      do concurrent (j = 1:siz_j)
         do concurrent (i = 1:siz_i)
            self%data(i,j) = swap16(self%data(i,j))
         end do
      end do 
   end subroutine big_to_little


   subroutine read_data_from_file(self, uni, file)
      use :: base_m
      implicit none
      class(buff) :: self
      character(MAX_PATH_LEN), intent(in) :: file
      integer(int32), intent(in) :: uni
      
      integer(int32) :: ios
      character(256) :: iomsg
      logical :: isExist

      inquire(file=file, exist=isExist)

      if (isExist) then
         open(uni, file=file, status='old', access='stream', form='unformatted', iostat=ios, iomsg=iomsg)
         
         if (ios /= 0) then
            write(stderr, *) trim(iomsg)
            return
         end if
         
         read(uni, iostat=ios, iomsg=iomsg) self%data(:,:)

         if (ios /= 0) then
            write(stderr, *) trim(iomsg)
         end if

      end if 
   end subroutine read_data_from_file


end module buff_t