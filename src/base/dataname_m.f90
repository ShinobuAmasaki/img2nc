module dataname_m
   use, intrinsic :: iso_fortran_env
   implicit none
   
contains

   subroutine create_data_name(west, north, code)
      integer(int32), intent(in) :: west, north
      integer(int32) :: east, south
      character(len=*), intent(out) :: code
      character(len=11) :: prefix
      character(len=2) :: postfix
      character(len=4) :: e_west, e_east
      character(len=3) :: e_north, e_south

      !e.g. DTM_MAP_01_N44E323N43E324SC.img
      if (west == 359) then
         !東端が360度となる場合は、変数eastに整数0を代入する。
         east = 0
      else
         east = west + 1
      end if

      south = north - 1 

      prefix = 'DTM_MAP_01_'  ! 11
      postfix = 'SC'          ! 2

      ! 西端と東端の記述
      write(e_west, '(a, i3.3)') 'E', west ! 4
      write(e_east, '(a, i3.3)') 'E', east ! 4

      !北端の記述
      if ( north >= 0 ) then
         write(e_north, '(a, i2.2)') 'N', abs(north) ! 3

      else if ( north < 0 ) then
         write(e_north, '(a, i2.2)') 'S', abs(north) ! 3
      
      end if

      !南端の記述
      if ( south >= 0 ) then
         write(e_south, '(a, i2.2)') 'N', abs(south) ! 3

      else if ( south < 0 ) then
         write(e_south, '(a, i2.2)') 'S', abs(south) ! 3

      end if

      code = prefix // e_north // e_west // e_south // e_east // postfix

   end subroutine create_data_name

end module dataname_m