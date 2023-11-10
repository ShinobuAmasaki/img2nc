module name_list_m
   use, intrinsic :: iso_fortran_env
   implicit none

   integer(int32), parameter :: MAX_NAME_LENGTH = 256
   
contains

   subroutine create_data_name(west, north, name)
      integer(int32), intent(in) :: west, north
      character(*), intent(out) :: name

      integer(int32) :: east, south
      character(len=11) :: prefix
      character(len=2) :: postfix
      character(len=4) :: west_edge, east_edge
      character(len=3) :: north_edge, south_edge

      !e.g. DTM_MAP_01_N44E323N43E324SC.img
      if (west == 359) then
         !東端が360度となる場合は、変数eastに整数0を代入する。
         east = 0
      else
         east = west + 1
      end if

      south = north - 1

      prefix = 'DTM_MAP_01_'
      postfix = 'SC'

      ! 西端と東端の文字列を生成する。
      write(west_edge, '(a, i3.3)') 'E', west
      write(east_edge, '(a, i3.3)') 'E', east

      ! 北端の文字列を生成する。
      if (north >= 0) then
         write(north_edge, '(a, i2.2)') 'N', abs(north)
      
      else if (north < 0 ) then
         write(north_edge, '(a, i2.2)') 'S', abs(north)
      end if

      ! 南端の文字列を生成する。
      if (south >= 0) then
         write(south_edge, '(a, i2.2)') 'N', abs(south)
      
      else if (south < 0) then
         write(south_edge, '(a, i2.2)') 'S', abs(south)

      end if 

      name = prefix // north_edge // west_edge // south_edge // east_edge // postfix

   end subroutine create_data_name


   subroutine create_name_list(data_root, list, edge)
      use :: boundary_t
      implicit none
      character(*), intent(in) :: data_root
      character(len=MAX_NAME_LENGTH), intent(out) :: list(:, :)
      type(boundary), intent(in) :: edge

      character(len=27) :: code
      integer(int32) :: siz_lon, siz_lat
      integer(int32) :: i, j, ii, jj
      integer(int32) :: west_edge, north_edge
      character(len=6) :: lon_dir

      siz_lon = edge%get_east() - edge%get_west()
      siz_lat = edge%get_north() - edge%get_south()

      ! 北から南へ走査する。
      do j = 1, siz_lat
         
         north_edge = edge%get_north() - j + 1

         ! 西から東へ走査する。
         do i = 1, siz_lon
            west_edge = edge%get_west() + i - 1

            call create_data_name(west_edge, north_edge, code)

            write(lon_dir, '(a, i3.3)') 'lon', west_edge

            list(i,j) = trim(data_root)// '/' //trim(lon_dir)// '/' // code 

         end do
      end do

   end subroutine create_name_list

end module name_list_m