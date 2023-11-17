module mola_megdr
   use, intrinsic :: iso_fortran_env
   use :: base_m
   implicit none

   interface outline_mola
      procedure :: outline__mola_megdr
   end interface

   interface create_data_name_mola
      module procedure :: create_data_name_mola
   end interface

   interface create_name_list_mola
      module procedure :: create_name_list_mola
   end interface 

contains

   function outline__mola_megdr(edge) result(res)
      use, intrinsic :: iso_fortran_env
      use :: boundary_t
      implicit none
      type(boundary), intent(in) :: edge
      type(boundary) :: res


      
   end function outline__mola_megdr
   

   subroutine create_data_name_mola(west_arg, north_arg, code, kind_arg)
      implicit none
      integer(int32), intent(in) :: west_arg, north_arg
      integer(int32) :: west, east, south, north
      character(len=*), intent(out) :: code
      character(len=1), intent(in), optional :: kind_arg
      character(len=1) :: kind
      character(len=3) :: prefix
      character(len=2) :: postfix

      character(len=3) :: e_west, e_north 

      prefix = 'meg'

      if (.not. present(kind_arg)) then
         kind = 'r'
      else
         kind = kind_arg
      end if

      west = west_arg
      north = north_arg

      e_west = ''
      e_north = ''

      if (west < 0) then
         west = west + 360
      end if

      if (0 <= west .and. west < 90) then
         e_west = '000'
      else if (90 <= west .and. west < 180) then
         e_west = '090'
      else if (180 <= west .and. west < 270) then
         e_west = '180'
      else if (270 <= west .and. west < 360) then
         e_west = '270'
      end if

      if ( north <= -44 ) then
         e_north = '44s'
      else if (-44 < north .and. north <= 0) then
         e_north = '00n'
      else if (0 < north  .and. north <= 44) then
         e_north = '44n'
      else if (44 < north) then
         e_north = '88n'
      end if

      postfix = 'hb'

      code = prefix//kind//e_north//e_west//postfix

   end subroutine create_data_name_mola


   subroutine create_name_list_mola(data_root, list, edge, kind_arg)
      use :: boundary_t
      implicit none
      character(*), intent(in) :: data_root
      character(len=MAX_NAME_LEN), intent(out) :: list(:,:)
      type(boundary), intent(in) :: edge
      character(len=1), optional :: kind_arg
      type(boundary) :: outline

      character(len=22) :: code
      integer(int32) :: siz_lon, siz_lat
      integer(int32) :: i, j, north, west

      integer(int32) :: e_west, e_east, e_south, e_north

      e_west = edge%get_west()
      e_east = edge%get_east()
      e_south = edge%get_south()
      e_north = edge%get_north()

      siz_lon = get_siz_lon_meg128(e_west, e_east)
      siz_lat = get_siz_lat_meg128(e_south, e_north)

      print *, siz_lon, siz_lat

      do j = 1, siz_lat

         north = e_north - (j-1)*44


         do i = 1, siz_lon

            west = e_west + (i-1)*90

            if (present(kind_arg)) then
               call create_data_name_mola(west, north, code, kind_arg)
            else
               call create_data_name_mola(west, north, code)
            end if

            list(i, j) = trim(data_root)//'/'//'meg128'//'/'//code
         
         end do
      end do
   
   end subroutine create_name_list_mola

   !
   function get_siz_lon_meg128(west, east) result(res)
      implicit none
      integer(int32), intent(in) :: west, east
      integer(int32) :: res

      integer(int32) :: most_west, most_east

      if (west < -90) then
         most_west = -2
      else if (west < 0) then
         most_west = -1
      else if (west < 89) then
         most_west = 0
      else if (west < 180) then
         most_west = 1
      else if (west < 270) then
         most_west = 2
      else if (west < 360) then
         most_west = 3
      end if

      if (east <= -90) then
         most_east = -1
      else if (east <= 0) then
         most_east = 0
      else if (east <= 90) then
         most_east = 1
      else if (east <= 180) then
         most_east = 2
      else if (east <= 270) then
         most_east = 3
      else if (east <= 360) then
         most_east = 4
      end if

      res = most_east - most_west


   end function get_siz_lon_meg128


   function get_siz_lat_meg128(south, north) result(res)
      implicit none
      integer(int32), intent(in) :: south, north
      integer(int32) :: res

      select case (north)
      case (45:)
         if (south >= 45) then
            res = 1
         else if (south > 0) then
            res = 2
         else if (south >= -44) then
            res = 3
         else
            res = 4
         end if
      case (1:44)
         if (south >= 1) then
            res = 1
         else if (south >= -44) then
            res = 2
         else
            res = 3
         end if
      case (-43:0)
         if (south >= -44) then
            res = 1
         else
            res = 2
         end if
      case (:-44)
         res = 1
      end select

   end function get_siz_lat_meg128

end module mola_megdr