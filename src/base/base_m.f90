module base_m
   use :: iso_fortran_env, stderr=>error_unit, stdout=>output_unit
   implicit none
      
   character(1), parameter, private :: CR = achar(13), LF = achar(10)
   character(2), parameter, public  :: CRLF=CR//LF

   integer, parameter :: MAX_PATH_LEN = 1024
   integer, parameter :: MAX_NAME_LEN = 256
   integer, parameter :: MAX_RANGE_LEN = 17

   integer(int16), parameter :: MINUS_9999_AFTER_SWAP16 = -3624_int16

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
      write(stdout, *) ''
      write(stdout, *) 'Usage: mpiexec -n NPROCS img2nc -d DIR -o FILE -r RANGE'
      write(stdout, *) ''
      write(stdout, *) 'OPTIONS::'
      write(stdout, *) '   mpiexec -n NPROCS'
      write(stdout, *) '         NPROCS is a the number of parallelization.' 
      write(stdout, *) ''
      write(stdout, *) '   -h, --help'
      write(stdout, *) '         display this help and exit.'
      write(stdout, *) ''
      write(stdout, *) '   -d DIR, --dir DIR'
      write(stdout, *) '         specify the root of the directory containing SLDEM2013 files.'
      write(stdout, *) ''
      write(stdout, *) '   -o FILE, --output FILE'
      write(stdout, *) '         specify the name of the netcdf file to output.'
      write(stdout, *) ''
      write(stdout, *) '   -r RANGE, --range RANGE'
      write(stdout, *) '         specify the process range <lon_west>/<lon_east>/<lat_south>/<lat_north>'
      write(stdout, *) ''
      write(stdout, *) '         -r <lon_west>/<lon_east>/<lat_south>/<lat_north>'
      write(stdout, *) '               lon_west:    0 -- 359, or -180 -- 179'
      write(stdout, *) '               lon_east:    1 -- 360, or -179 -- 180'
      write(stdout, *) '               lat_south: -90 -- 89'
      write(stdout, *) '               lat_north: -89 -- 90'
      write(stdout, *) ''
      write(stdout, *) '   -c DENOMINATOR, --coarse DENOMINATOR'
      write(stdout, *) '         specify the denominator for coarse vision at the loading *.img file'
      write(stdout, *) '         following integer is acceptable: '
      write(stdout, *) '               1    -> 1/1  size'
      write(stdout, *) '               2    -> 1/2  size'
      write(stdout, *) '               4    -> 1/4  size'
      write(stdout, *) '               8    -> 1/8  size'
      write(stdout, *) '               ...  '
      write(stdout, *) '               2**n -> 1/2**n size'

   end subroutine

   pure function swap16(value)
      implicit none
      integer(int16), intent(in) :: value
      integer(int16) :: swap16

      swap16 = ishft(value,8)

      swap16 = ior(swap16, ishft(value, -8))

   end function swap16

end module base_m