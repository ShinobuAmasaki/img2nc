module preprocess_m
   use, intrinsic :: iso_fortran_env, stdout=>output_unit, stderr=>error_unit
   use :: mpi_f08
   use :: in_operator_m
   use :: command_line_arguments_m
   use :: argument_t
   use :: boundary_t
   use :: global_m
   use :: base_m


   integer(int32) :: argc
   type(argument), allocatable :: arg(:)
   character(:), allocatable :: arg_whole

   character(len=6) :: h_flag_l = '--help'      ! help
   character(len=8) :: o_flag_l = '--output'    ! output
   character(len=5) :: d_flag_l = '--dir'       ! data dir
   character(len=7) :: r_flag_l = '--range'     ! range
   character(len=8) :: c_flag_l = '--coarse'    ! coarse
   character(len=2) :: h_flag_s = '-h'
   character(len=2) :: o_flag_s = '-o'
   character(len=2) :: d_flag_s = '-d'
   character(len=2) :: r_flag_s = '-r'
   character(len=2) :: c_flag_s = '-c'
   integer(int32) :: coarse_default = 16

contains

   subroutine preprocess (data_dir, outnc, range, coarse, edge )
      implicit none
      character(*),   intent(out) :: data_dir, outnc
      character(MAX_RANGE_LEN),  intent(out) :: range
      integer(int32), intent(out) :: coarse
      type(boundary), intent(out)   :: edge
      
      call get_value_from_args(data_dir, outnc, range, coarse)
      call validate_range(range, edge)

      print *, outnc

      if (.not. is_valid_coarse(coarse)) then
         write(stderr, *) "ERROR: invalid coarse number specified."
         call gently_stop()
      end if

   end subroutine


   subroutine get_value_from_args (data_dir, outnc, range, coarse)
      implicit none
      character(*),   intent(out) :: data_dir, outnc
      character(MAX_RANGE_LEN),  intent(out) :: range
      integer(int32), intent(out) :: coarse
      integer :: ios
      
      character(4) :: coarse_char
      character(:), allocatable :: outnc_buff
      character(:), allocatable :: range_buff

      integer :: index ! the indices of the value in arguments

      range_buff = ''
      coarse_char = ''
      outnc_buff  = ''

      call get_arguments(argc, arg, arg_whole)

      if (argc == 0) then
         write(stderr, *) "ERROR: too few arguments."
         call gently_stop()
      end if
   

      if ((wrap(h_flag_l) .in. arg_whole) .or. (wrap(h_flag_s) .in. arg_whole)) then
         ! write(stderr, *) "PRINT HELP"
         call gently_stop()
      end if

      if ((wrap(o_flag_l) .in. arg_whole) .or. (wrap(o_flag_s) .in. arg_whole)) then

         ! -oオプションフラグが指定されている場合、値OUTNCはフラグの次の引数から読み取る。
         index = get_flag_index(o_flag_l, o_flag_s, arg) + 1

         outnc_buff = trim(adjustl( arg(index)%v ))
         print *, outnc_buff
      end if

      if ((wrap(d_flag_l) .in. arg_whole) .or. (wrap(d_flag_s) .in. arg_whole)) then
         
         index = get_flag_index(d_flag_l, d_flag_s, arg) + 1
         data_dir = trim(adjustl( arg(index)%v ))

      end if


      if ((wrap(r_flag_l) .in. arg_whole) .or. (wrap(r_flag_s) .in. arg_whole)) then

         ! -rのオプションフラグが指定されている場合、値RANGEはフラグの次の引数から読み取る。
         index = get_flag_index(r_flag_l, r_flag_s, arg) + 1
         range_buff = trim(adjustl( arg(index)%v ))
      else
         ! 指定されていない場合、値RANGEは最後の引数にあると仮定する。
         block
            integer:: p ! the index of the last space 
            p = scan(arg_whole, ' ', back=.true.)
            range_buff = trim(adjustl(arg_whole(p:len(arg_whole))))
            ! print *, range
         end block
      end if


      if ((wrap(c_flag_l) .in. arg_whole) .or. (wrap(c_flag_s) .in. arg_whole)) then
         ! -cオプションフラグが指定されている場合、値COARSEはフラグの次の引数から読み取る。
         index = get_flag_index(c_flag_l, c_flag_s, arg) + 1
         coarse_char = trim(adjustl( arg(index)%v ))

         read(coarse_char, *, iostat=ios) coarse
         if (ios /= 0) then
            write(stderr, *) "Invalid COARSE value."
            write(stderr, *) "===> Use default value: 16"
            coarse = coarse_default
         end if

      end if

      outnc = trim(adjustl(outnc_buff))
      range = trim(adjustl(range_buff))

   end subroutine get_value_from_args


   subroutine validate_range (range, edge)
      implicit none
      character(*), intent(in) :: range
      type(boundary), intent(inout) :: edge
      integer(int32) :: p, q, r, n
      integer :: ios
      integer :: west, east, south, north
      character(256) :: msg

      if (trim(adjustl(range)) == '') return

      edge = boundary() 

      n = len(trim(adjustl( range )))

      print *, range

      ! West
      p = index(range, '/')
      read (range(1:p-1), *, iostat=ios) west 
      call assert_iostat(ios)
      call edge%set_west(west)

      ! East
      q = index(range(p+1:n), '/') + p
      read(range(p+1:q-1), *, iostat=ios) east
      call assert_iostat(ios)
      call edge%set_east(east)

      r = index(range(q+1:n), '/') + q
      read(range(q+1:r-1), *, iostat=ios) south
      call assert_iostat(ios)
      call edge%set_south(south)

      read(range(r+1:n), *, iostat=ios, iomsg=msg) north
      if(ios /= 0)  write(stderr, *) msg
      call assert_iostat(ios)
      call edge%set_north(north)

      call edge%check_valid_range()


      if (.not. edge%get_is_valid()) then
         write(stderr, *) "ERROR: invalid range specified."
         call gently_stop() 
      end if

   contains

      subroutine assert_iostat(ios)
         implicit none
         integer, intent(in) :: ios
         
         if (ios /= 0) then
            call edge%set_is_valid(.false.)
            write(stderr, *) "Invalid range specified."
            call gently_stop()
            return
         end if

      end subroutine assert_iostat

   end subroutine validate_range


   function is_valid_coarse(coarse) result(res)
      implicit none
      integer,intent(in) :: coarse
      integer :: i, tester 
      logical :: res

      res = .false.

      do i = 0, 12
         tester = 1 * 2**i
         res = coarse == tester .or. res
      end do
     
   end function is_valid_coarse

!---------------------------------------------------------------------!


   function get_flag_index(long_flag, short_flag, arg_array) result(res)
      implicit none
      character(*), intent(in) :: long_flag, short_flag
      type(argument), intent(in), allocatable :: arg_array(:)
      integer(int32) :: res, siz, i
      
      res = 0
      siz = size(arg_array, dim=1)
      
      do i = 1, siz
         if (arg_array(i)%v == long_flag .or. arg_array(i)%v == short_flag) then
            res = i
            return
         end if
      end do

   end function get_flag_index


   function wrap(str) result(res)
      implicit none
      character(*), intent(in) :: str
      character(:), allocatable :: res

      res = ' '//trim(adjustl(str))//' '

   end function wrap


end module preprocess_m