module mola_megdr
   use, intrinsic :: iso_fortran_env
   use :: base_m
   implicit none
   private

   public :: outline_mola
   public :: create_data_name
   public :: create_name_list
   public :: allocate_lists
   public :: set_distribution
   public :: get_siz_lon_meg128
   public :: get_siz_lat_meg128

   ! Procedures for trimming
   public :: resize_longitude
   public :: resize_latitude
   public :: set_is_on_west_edge
   public :: set_is_on_east_edge
   public :: set_is_on_south_edge
   public :: set_is_on_north_edge
   public :: trimming_west_east
   public :: trimming_west
   public :: trimming_east
   public :: trimming_south_north
   public :: trimming_south
   public :: trimming_north

   integer(int32), parameter, public :: MOLA_MEG128_PPD_MAX=128

   interface outline_mola
      procedure :: outline__mola_meg128
   end interface

   interface create_data_name
      module procedure :: create_data_name_mola
   end interface

   interface create_name_list
      module procedure :: create_name_list_mola
   end interface

   interface allocate_lists
      module procedure :: allocate_lists_mola
   end interface

   interface set_distribution
      module procedure :: set_distribution_mola
   end interface 

contains

   function outline__mola_meg128(edge) result(res)
      use, intrinsic :: iso_fortran_env
      use :: boundary_t
      implicit none
      type(boundary), intent(in) :: edge
      type(boundary) :: res

      integer :: west, east, south, north

      west = edge%get_west()
      east = edge%get_east()
      south = edge%get_south()
      north = edge%get_north()

      select case(west)
      case (:-91)
         call res%set_west(-180)
      case (-90:-1)
         call res%set_west(-90)
      case (0:89)
         call res%set_west(0)
      case (90:179)
         call res%set_west(90)
      case (180:269)
         call res%set_west(180)
      case (270:)
         call res%set_west(270)
      end select

      select case(east)
      case (:-90)
         call res%set_east(-90)
      case (-89:0)
         call res%set_east(0)
      case (1:90)
         call res%set_east(90)
      case (91:180)
         call res%set_east(180)
      case (181:270)
         call res%set_east(270)
      case (271:)
         call res%set_east(360)
      end select

      select case(south)
      case (:-45)
         call res%set_south(-88)
      case (-44:-1)
         call res%set_south(-44)
      case (0:43)
         call res%set_south(0)
      case (44:)
         call res%set_south(44)
      end select 

      select case (north)
      case (:-44)
         call res%set_north(-44)
      case (-43:0)
         call res%set_north(0)
      case (1:44)
         call res%set_north(44)
      case (45:)
         call res%set_north(88)
      end select
      
   end function outline__mola_meg128
   

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


   subroutine allocate_lists_mola(outline, list, numlist_1d, numlist_2d, logical_array)
      use global_m
      use boundary_t
      implicit none

      character(len=MAX_PATH_LEN), intent(out), allocatable :: list(:,:)
      integer(int32), intent(out), allocatable :: numlist_2d(:,:), numlist_1d(:)
      logical, intent(out), allocatable :: logical_array(:,:)
      type(boundary), intent(in) :: outline 
      
      integer(int32) :: lon_size, lat_size

      lon_size = get_siz_lon_meg128(outline%get_west(), outline%get_east())
      lat_size = get_siz_lat_meg128(outline%get_south(), outline%get_north())

      allocate(list(lon_size, lat_size))
      allocate(numlist_1d(lon_size*lat_size))
      allocate(numlist_2d(lon_size, lat_size))
      allocate(logical_array(lon_size, lat_size))

   end subroutine allocate_lists_mola


   subroutine create_name_list_mola(data_root, list, edge, kind_arg)
      use :: boundary_t
      implicit none
      character(*), intent(in) :: data_root
      character(len=MAX_PATH_LEN), intent(out) :: list(:,:)
      type(boundary), intent(in) :: edge
      character(len=1), optional :: kind_arg
      type(boundary) :: outline

      character(len=22) :: code
      integer(int32) :: siz_lon, siz_lat
      integer(int32) :: i, j, north, west, jj

      integer(int32) :: e_west, e_east, e_south, e_north

      e_west = edge%get_west()
      e_east = edge%get_east()
      e_south = edge%get_south()
      e_north = edge%get_north()

      siz_lon = get_siz_lon_meg128(e_west, e_east)
      siz_lat = get_siz_lat_meg128(e_south, e_north)


      do j = 1, siz_lat

         north = e_north - (j-1)*44

         jj = siz_lat - j + 1

         do i = 1, siz_lon

            west = e_west + (i-1)*90


            if (present(kind_arg)) then
               call create_data_name_mola(west, north, code, kind_arg)
            else
               call create_data_name_mola(west, north, code)
            end if

            list(i, jj) = trim(data_root)//'/'//'meg128'//'/'//code
         
         end do
      end do
   
   end subroutine create_name_list_mola

   subroutine set_distribution_mola(distri_1d, distri_2d, distri_logical, outline)
      use global_m
      use boundary_t
      implicit none
      integer(int32), intent(inout) :: distri_1d(:), distri_2d(:, :)
      logical, intent(inout) :: distri_logical(:, :)
      type(boundary), intent(in) :: outline

      integer :: petot_max    ! the maximum value of processor emement total 
      integer :: num_i, num_j, i, j, k

      num_i = get_siz_lon_meg128(outline%get_west(), outline%get_east())
      num_j = get_siz_lat_meg128(outline%get_south(), outline%get_north())

      petot_max = num_i*num_j
      if (petot > petot_max) then 
         if (isIm1) then
            write(stderr, *) "ERROR: Too many processor elements. Max is: ", petot_max
         end if
         call gently_stop()
      end if

      do i = 1, num_i*num_j
         if (petot /=1) then
            distri_1d(i) = mod(i-1, petot) + 1
         else
            distri_1d(i) = 1
         end if
      end do

      distri_2d = reshape(distri_1d, [num_i, num_j])

      do j = 1, num_j
         do i = 1, num_i
            distri_logical(i, j) = distri_2d(i, j) == thisis
         end do
      end do
   end subroutine set_distribution_mola
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

      integer :: most_north, most_south 

      select case (north)
      case (45:)
         most_north = 4
      case (1:44)
         most_north = 3
      case (-43:0)
         most_north = 2
      case (:-44)
         most_north = 1
      end select

      select case (south)
      case (44:)
         most_south = 3
      case (0:43)
         most_south = 2
      case (-44:-1)
         most_south = 1
      case (:-45)
         most_south = 0
      end select
      
      res = most_north - most_south
      
   end function get_siz_lat_meg128


   subroutine resize_longitude(lon, idx_west, idx_east, global_nx)
      implicit none
      real(real64), allocatable, intent(inout) :: lon(:)
      integer(int32), intent(in) :: idx_west, idx_east
      integer(int32), intent(in) :: global_nx
      real(real64), allocatable :: buf(:)

      if (idx_west == 0 .and. idx_east == global_nx) return

      allocate(buf(idx_east-idx_west+1))
      buf(:) = lon(idx_west:idx_east)

      deallocate(lon)
      allocate(lon(idx_east-idx_west+1))

      lon(:) = buf(:)

   end subroutine

   subroutine resize_latitude(lat, idx_south, idx_north, global_ny)
      implicit none
      real(real64), allocatable, intent(inout) :: lat(:)
      integer(int32), intent(in) :: idx_south, idx_north
      integer(int32), intent(in) :: global_ny
      real(real64), allocatable :: buf(:)


      if (idx_south == 0 .and. idx_north == global_ny) return

      allocate(buf(idx_north-idx_south+1))
      buf(:) = lat(idx_south:idx_north)

      deallocate(lat)
      allocate(lat(idx_south:idx_north))

      lat(:) = buf(:)
   
   end subroutine resize_latitude


   subroutine set_is_on_west_edge(is_on_west_edge)
      implicit none
      logical, intent(out) :: is_on_west_edge(:,:)

      is_on_west_edge(:, :) = .false.

      is_on_west_edge(1, :) = .true.

   end subroutine set_is_on_west_edge


   subroutine set_is_on_east_edge(is_on_east_edge)
      implicit none
      logical, intent(out) :: is_on_east_edge(:,:)

      is_on_east_edge(:, :) = .false.

      is_on_east_edge(size(is_on_east_edge, dim=1), :) = .true.

   end subroutine set_is_on_east_edge


   subroutine set_is_on_south_edge(is_on_south_edge)
      implicit none
      
      logical, intent(out) :: is_on_south_edge(:,:)

      is_on_south_edge(:, :) = .false.

       ! 配列の上が南の順番になっているので、このマスクは最初の行が南端
      is_on_south_edge(:, 1) = .true.

   end subroutine set_is_on_south_edge


   subroutine set_is_on_north_edge(is_on_north_edge)
      implicit none
      
      logical, intent(out) :: is_on_north_edge(:, :)

      is_on_north_edge(:, :) = .false.

      ! 配列の上が南の順番になっているので、このマスクは最後の行が北端
      is_on_north_edge(:, size(is_on_north_edge, dim=2)) = .true.

   end subroutine set_is_on_north_edge

    
   subroutine trimming_west_east(array, idx_w, idx_e)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:,:)
      integer(int32), intent(in) :: idx_w, idx_e

      integer(int16), allocatable :: buff(:, :)
      integer(int32) :: ny

      ny = size(array, dim=2)

      allocate(buff(idx_e-idx_w+1, ny))

      buff(:,:) = array(idx_w:idx_e, :)

      deallocate(array)
      allocate(array(idx_e-idx_w+1, ny))
      array(:,:) = buff(:, :)
   end subroutine trimming_west_east


   subroutine trimming_west (array, idx_w)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:, :)
      integer(int32), intent(in) :: idx_w

      integer(int16), allocatable :: buff(:, :)
      integer(int32) :: nx, ny

      nx = size(array, dim=1)
      ny = size(array, dim=2)

      allocate(buff(nx-idx_w+1, ny))

      buff(:, :) = array(idx_w:nx, :)

      deallocate(array)
      allocate(array(nx-idx_w+1, ny))

      array(:,:) = buff(:, :)

   end subroutine trimming_west


   subroutine trimming_east (array, idx_e)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:,:)
      integer(int32), intent(in) :: idx_e

      integer(int16), allocatable :: buff(:,:)
      integer(int32) :: ny

      ny = size(array, dim=2)

      allocate(buff(idx_e, ny))

      buff(:,:) = array(1:idx_e, :)

      deallocate(array)
      allocate(array(idx_e, ny))

      array(:,:) = buff(:,:)
   end subroutine trimming_east 

   
   subroutine trimming_south_north(array, idx_s, idx_n)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:, :)
      integer(int32), intent(in) :: idx_s, idx_n

      integer(int16), allocatable :: buff(:, :)
      integer(int32) :: nx

      nx = size(array, dim=1)

      allocate(buff(nx, idx_n-idx_s+1))

      buff(:,:) = array(:, idx_s:idx_n)

      deallocate(array)
      allocate(array(nx, idx_n-idx_s+1))

      array(:, :) = buff(:, :)

   end subroutine trimming_south_north 


   subroutine trimming_south(array, idx_s)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:,:)
      integer(int32), intent(in) :: idx_s

      integer(int16), allocatable :: buff(:,:)
      integer(int32) :: nx, ny

      nx = size(array, dim=1)
      ny = size(array, dim=2)

      allocate(buff(nx, ny-idx_s+1))

      buff(:, :) = array(:, 1:ny-idx_s+1)

      deallocate(array)
      allocate(array(nx, ny-idx_s+1))

      array(:,:) = buff(:,:)

   end subroutine trimming_south


   subroutine trimming_north(array, idx_n)
      implicit none
      integer(int16), allocatable, intent(inout) :: array(:,:)
      integer(int32), intent(in) :: idx_n

      integer(int16), allocatable :: buff(:,:)
      integer(int32) :: nx, ny

      nx = size(array, dim=1)

      allocate(buff(nx, idx_n))

      buff(:, :) = array(:, 1:idx_n)

      deallocate(array)
      allocate(array(nx, idx_n))

      array(:,:) = buff(:,:)

   end subroutine trimming_north


end module mola_megdr