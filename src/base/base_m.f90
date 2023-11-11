module base_m
   use :: iso_fortran_env, stderr=>error_unit, stdout=>output_unit
   implicit none
      
   character(1), parameter, private :: CR = achar(13), LF = achar(10)
   character(2), parameter, public  :: CRLF=CR//LF

   integer, parameter :: MAX_PATH_LEN = 1024
   integer, parameter :: MAX_NAME_LEN = 256

contains

   subroutine head_single_line (buff, line)
      implicit none
      character(*), intent(inout) :: buff
      character(*), intent(out)   :: line

      integer :: index

      line = ''

      index = scan(buff, CRLF)

      if (index == 0) return

      line = trim(buff(1:index-1))

      ! Truncate the line
      buff = buff(index+2:)

   end subroutine head_single_line
      

   !> buffにラベルデータの全体を読み込む。
   subroutine load_file_buffered (unit, buff, ios)
      implicit none
      integer(int32), intent(in) :: unit
      character(*), intent(inout) :: buff
      integer(int32), intent(out), optional :: ios

      character(10) :: access, form
      character(:), allocatable :: iomsg
      logical :: isOpened, isUnformatted, isStream
      integer(int32) :: ios_

      iomsg = ''
      ios_ = 0

      isOpened = .false.
      isUnformatted = .false.
      isStream = .false.
      access = ''
      form = ''

      inquire (unit, opened=isOpened, unformatted=form, access=access, iostat=ios_, iomsg=iomsg)

      if (present(ios)) ios = ios_ 

      if (ios_ /= 0 ) then
         write(stderr, '(a)') iomsg
         return
      end if

      if (trim(form) == 'YES') then
         isUnformatted = .true.
      else
         isUnformatted = .false.
      end if

      if (trim(access) == 'STREAM') then
         isStream = .true.
      else
         isStream = .false.
      end if

      if (.not. isOpened) then
         write(stderr, '(a)') "Error: File is not opened."
         return
      
      else if (.not. isUnformatted) then
         write(stderr, '(a)') "Error: File opened as 'FORMATTED'."
         return
      
      else if (.not. isStream) then
         write(stderr, '(a)') "Error: File is opened without the `access='stream'` specifier."
         return
      
      end if
      
      read(unit, iostat=ios_, iomsg=iomsg) buff
      if (present(ios)) ios = ios_ 

      if (ios_ > 0 ) then
         write(stderr, '(a)') iomsg
         return
      end if

   end subroutine load_file_buffered


   subroutine print_usage()
      use version_m
      implicit none
      
      write(stdout, *) 'img2nc v'//version()//', convert data of SLDEM2013 into NetCDF.'
   end subroutine

end module base_m