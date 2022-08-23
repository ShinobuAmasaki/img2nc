module mod_preprocess
   use, intrinsic ::iso_fortran_env
   use mod_boundary
   implicit none

   interface preprocess
      procedure :: preprocess_out_4_sides, preprocess_type_boundary
   end interface preprocess

   interface valid_range
      procedure :: valid_range_4_sides, valid_range_type_boundary
   end interface valid_range

   character(len=6) :: h_flag_l = '--help'      ! help
   character(len=8) :: o_flag_l = '--output'    ! output
   character(len=5) :: d_flag_l = '--dir'       ! data dir
   character(len=7) :: r_flag_l = '--range'     ! range
   character(len=2) :: h_flag_s = '-h'
   character(len=2) :: o_flag_s = '-o'
   character(len=2) :: d_flag_s = '-d'
   character(len=2) :: r_flag_s = '-r'
   character(len=12) :: version = 'v1.0.2'

contains

   subroutine preprocess_type_boundary(dir, output, range, sides)
      character(len=*), intent(out) :: dir, output, range
      type(boundary), intent(out) :: sides
      
      integer(int32) :: count, i
      character(len=256) :: arg
      logical :: o_read, d_read, r_read, o_exist, d_exist, r_exist, is_valid_range

      !Initialize
      o_read = .false.
      d_read = .false.
      r_read = .false.
      o_exist = .false.
      d_exist = .false.
      r_exist = .false.
      is_valid_range = .false.

      arg = ''
      dir = ''
      output = ''
      range = ''

      !引数の数を取得する
      count = command_argument_count()

      if (count == 0) then
         print *, 'ERROR: too few arguments'
         call print_usage()
      end if

      !引数読み込み
      i = 1
      do while (i <= count)
         ! 値引数の読み込み
         if (o_read) then
            call get_command_argument(i, output)
            call confusing_with_option_flag(output)
            o_read = .false.
            o_exist = .true.
            i = i + 1
            cycle

         else if (d_read) then
            call get_command_argument(i, dir)
            call confusing_with_option_flag(dir)
            d_read = .false.
            d_exist = .true.
            i = i + 1
            cycle
         
         else if (r_read) then
            call get_command_argument(i, range)
            call confusing_with_option_flag(range)
            r_read = .false.
            r_exist = .true.
            i = i + 1
            cycle
         
         end if
         !----------------------------------------!

         ! 1つの引数を取得
         call get_command_argument(i, arg)

         !----------------------------------------!
         ! ヘルプフラグ
         if ( is_help_flag(arg) ) then
            !ヘルプメッセージを表示する。
            print *, 'given -h'
            call print_usage()
            
         ! 出力ファイル名フラグ
         else if ( is_out_flag(arg) ) then
            o_read = .true.
            i = i + 1
            cycle
         
         ! データルートディレクトリフラグ
         else if ( is_dir_flag(arg) ) then
            d_read = .true.
            i = i + 1
            cycle

         ! 範囲フラグ
         else if ( is_range_flag(arg) ) then
            r_read = .true.
            i = i + 1
            cycle

         end if
      
         i = i + 1
      end do

            !---------------------------------------------!
      ! ここまでにout, dir, rangeに値が格納されていない場合
      if ( .not. (o_exist .and. d_exist .and. r_exist) ) then
         print *, 'ERROR: not enough arguments'
         call print_usage()
      end if

      !---------------------------------------------!
      !dir変数が有効なディレクトリでない場合
      if ( .not. is_valid_dir_path(dir) ) then
         print *, 'ERROR: given an invalid directry path'
         stop
      end if

      !範囲指定の検査
      ! call valid_range(range, is_valid_range, west, east, south, north)
      call valid_range(range, sides)
      if ( .not. sides%get_is_valid() ) then
         print *, 'ERROR: given an invalid range'
         call print_usage()
      end if

      if ( .not. is_valid_out_name(output) ) then
         print *, 'ERROR: given an unopenable file name'
         stop
      end if

   end subroutine preprocess_type_boundary

   
   subroutine preprocess_out_4_sides(dir, output, range, west, east, south, north)
      character(len=*), intent(out) :: dir, output
      character(len=*), intent(out) :: range
      integer(int32), intent(out) :: west, east, south, north
     
      integer(int32) :: count, i
      character(len=256) :: arg
      logical :: o_read, d_read, r_read, o_exist, d_exist, r_exist, is_valid_range

      ! 初期化
      o_read = .false.
      d_read = .false.
      r_read = .false.
      o_exist = .false.
      d_exist = .false.
      r_exist = .false.
      is_valid_range = .false.

      arg = ''
      dir = ''
      output = ''
      range = ''

      !引数の数を取得する。
      count = command_argument_count()

      !引数無しの場合はヘルプメッセージを表示して終了する。
      if (count == 0) then
         print *, 'ERROR: too few arguments'
         call print_usage()

      end if

      ! 引数の読み込み
      i = 1
      do while (i <= count)
         ! print *, i

         ! 値引数の読み込み
         if (o_read) then
            call get_command_argument(i, output)
            call confusing_with_option_flag(output)
            o_read = .false.
            o_exist = .true.
            i = i + 1
            cycle

         else if (d_read) then
            call get_command_argument(i, dir)
            call confusing_with_option_flag(dir)
            d_read = .false.
            d_exist = .true.
            i = i + 1
            cycle
         
         else if (r_read) then
            call get_command_argument(i, range)
            call confusing_with_option_flag(range)
            r_read = .false.
            r_exist = .true.
            i = i + 1
            cycle
         
         end if
         !----------------------------------------!

         ! 1つの引数を取得
         call get_command_argument(i, arg)

         !----------------------------------------!
         ! ヘルプフラグ
         if ( is_help_flag(arg) ) then
            !ヘルプメッセージを表示する。
            print *, 'given -h'
            call print_usage()
            
         ! 出力ファイル名フラグ
         else if ( is_out_flag(arg) ) then
            o_read = .true.
            i = i + 1
            cycle
         
         ! データルートディレクトリフラグ
         else if ( is_dir_flag(arg) ) then
            d_read = .true.
            i = i + 1
            cycle

         ! 範囲フラグ
         else if ( is_range_flag(arg) ) then
            r_read = .true.
            i = i + 1
            cycle

         end if
      
         i = i + 1
      end do
   
      !---------------------------------------------!
      ! ここまでにout, dir, rangeに値が格納されていない場合
      if ( .not. (o_exist .and. d_exist .and. r_exist) ) then
         print *, 'ERROR: not enough arguments'
         call print_usage()
      end if

      !---------------------------------------------!
      !dir変数が有効なディレクトリでない場合
      if ( .not. is_valid_dir_path(dir) ) then
         print *, 'ERROR: given an invalid directry path'
         stop
      end if

      !範囲指定の検査
      call valid_range(range, is_valid_range, west, east, south, north)
      if ( .not. is_valid_range ) then
         print *, 'ERROR: given an invalid range'
         call print_usage()
      end if

      if ( .not. is_valid_out_name(output) ) then
         print *, 'ERROR: given an unopenable file name'
         stop
      end if
   
   end subroutine preprocess_out_4_sides

   !ヘルプメッセージを出力する。
   subroutine print_usage()

      print *, 'img2nc '// trim(version) //', convert data of sldem2013 into netcdf'
      print *, ''
      print *, 'Usage: img2nc [option]... -r RANGE'
      print *, ''
      print *, 'OPTIONS::'
      print *, '   -h, --help    display this help and exit'
      print *, ''
      print *, '   -d DIR, --dir DIR'
      print *, '                 specify root of the directory containing netcdf files'
      print *, ''
      print *, '   -o FILE, --output FILE'
      print *, '                 specify name of the netcdf file to output'
      print *, ''
      print *, '   -r RANGE, --range RANGE'
      print *, '                 specify the plot range <lon_west>/<lon_east>/<lat_south>/<lat_north>'
      print *, ''
      print *, '                 -r <lon_west>/<lon_east>/<lat_south>/<lat_north>'
      print *, '                    lon_west:    0 - 359'
      print *, '                    lon_east:    1 - 360'
      print *, '                    lat_south: -90 -  89'
      print *, '                    lat_north: -89 -  90'

      stop
   end subroutine print_usage


   logical function is_help_flag(str) result(res)
      character(len=*) :: str

      if ( trim(str) == h_flag_s .or. trim(str) == h_flag_l ) then
         res = .true.
      else
         res = .false.
      end if
   
   end function is_help_flag


   logical function is_out_flag(str) result(res)
      character(len=*) :: str

      if ( trim(str) == o_flag_s .or. trim(str) == o_flag_l ) then
         res = .true.
      else
         res = .false.
      end if
   
   end function is_out_flag


   logical function is_dir_flag(str) result(res)
      character(len=*) :: str

      if ( trim(str) == d_flag_s .or. trim(str) == d_flag_l ) then
         res = .true.
      else
         res = .false.
      end if
   
   end function is_dir_flag


   logical function is_range_flag(str) result(res)
      character(len=*) :: str

      if ( trim(str) == r_flag_s .or. trim(str) == r_flag_l ) then
         res = .true.
      else
         res = .false.
      end if
   
   end function is_range_flag


   logical function is_valid_dir_path(str) result(res)
      character(len=*) :: str
      integer(int32) :: val
      integer(int32) :: access ! for ifort

      val = access(trim(str), 'r')

      if (val == 0) then
         res = .true.
      else
         res = .false.
      end if
   end function is_valid_dir_path

   subroutine confusing_with_option_flag(str)
      character(len=*), intent(in) :: str

      !文字列strの最初の文字が'-'の場合、プログラムを終了する。オプションフラッグが2個続いた場合などを想定する
      if (str(1:1) == '-') then
         print *, 'ERROR: given an invalid argument'
         stop
      end if
   
   end subroutine confusing_with_option_flag


   subroutine valid_range_type_boundary(str, sides)
      character(len=*), intent(in) :: str
      type(boundary), intent(out) :: sides
      integer(int32) :: p, q, r, n
      integer(int32) :: tmp

      ! initialize
      sides = boundary()
      tmp = 0

      n = len(trim(str)) !文字数

      ! 1st value
      p = index(str, '/')
      read(str(1:p), *, err=150) tmp
      call sides%set_west(tmp)

      ! 2nd value
      q = index(str(p+1:n), '/') + p
      read(str(p+1:q), *, err=150) tmp
      call sides%set_east(tmp)

      ! 3rd value
      r = index(str(q+1:n), '/') + q
      read(str(q+1:r), *, err=150) tmp
      call sides%set_south(tmp)

      ! 4th value
      read(str(r+1:n), *, err=150) tmp
      call sides%set_north(tmp)

      ! validation check
      call sides%check_valid_range()

      return

      ! Error process
150   continue
      call sides%set_is_valid(.false.)

   end subroutine valid_range_type_boundary


   subroutine valid_range_4_sides(str, is_valid, west, east, south, north)
   ! logical function is_valid_range(str) result(res)
      character(len=*), intent(in) :: str
      integer :: p, q, r, n
      integer(int32), intent(out) :: west, east, south, north
      logical, intent(out) :: is_valid

      is_valid = .false.

      n = len(trim(str))
      ! print *, n

      ! slash index
      p = index(str, '/')
      ! print *, p
      read(str(1:p), *, err=200) west
      !2
      q = index(str(p+1:n), '/') + p
      ! print *, q
      read(str(p+1:q), *, err=200) east
      !3
      r = index(str(q+1:n), '/') + q
      ! print *, r
      read(str(q+1:r), *, err=200) south
      !4
      read(str(r+1:n), *, err=200) north

      !rangeの範囲が有効かチェックする。
      ! westは0-359
      if ( west < 0 .or. 359 < west ) then
         return

      ! eastは1-360
      else if ( east < 1 .or. 360 < east ) then
         return

      ! southは-90から89
      else if ( south < -90 .or. 89 < south ) then
         return
      
      ! northは-89から90
      else if ( north < -89 .or. 90 < north ) then
         return
      
      end if

      !西の方が大きい
      if ( east <= west ) then
         return
      
      else if ( north <= south ) then
         return
      
      end if

      is_valid = .true.    !フラグの更新

200   return

   end subroutine valid_range_4_sides


   logical function is_valid_out_name(str) result(res)
      character(len=*), intent(in) :: str

      !ファイルが開けるか開けないかで有効・無効を判定する。
      open(100, file=trim(str), status='replace', err=300)
      close(100)
      res = .true.
      return

300   res = .false.  ! 開けない場合はfalseを返す。

   end function is_valid_out_name


end module mod_preprocess
