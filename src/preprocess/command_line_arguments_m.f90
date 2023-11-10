module command_line_arguments_m
   use, intrinsic :: iso_fortran_env
   use :: argument_t 
   implicit none
   private

   public :: get_arguments


contains

   subroutine get_arguments(argc, arg, arg_whole)
      implicit none
      integer(int32), intent(out) :: argc
      type(argument), allocatable, intent(out) :: arg(:)
      character(:), allocatable, intent(out) :: arg_whole

      ! 引数全体のデータを読み込むブロック
      block
         integer :: whole_length
      
         ! 引数全体の長さを取得し、その長さでarg_wholeを割り付ける。
         call get_command(length=whole_length)
         allocate(character(whole_length) :: arg_whole)

         ! 変数arg_wholeに引数のデータを書き込む。
         call get_command(command=arg_whole)
      
      end block

      ! 引数の個数を取得して、変数argcに代入する。
      argc = command_argument_count()

      ! 配列argを　
      allocate(arg(0:argc))

      block
         integer(int32) :: n, length_nth_arg

         do n = 0, argc

            ! 第n引数の長さを取得する。
            call get_command_argument(number=n, length=length_nth_arg)

            ! 第n引数の長さと同じ長さの文字列を割り付ける
            allocate (character(length_nth_arg) :: arg(n)%v)

            ! 第n引数の値を文字列で取得する。
            call get_command_argument(number=n, value=arg(n)%v)

         end do

      end block

   !! cf. 
   end subroutine get_arguments



end module command_line_arguments_m
