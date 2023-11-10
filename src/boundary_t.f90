module boundary_t
   use, intrinsic :: iso_fortran_env
   implicit none
   private
   public :: boundary

   ! type definition
   type :: boundary
      private
      integer(int32) :: west_lon, east_lon, south_lat, north_lat
      logical :: is_valid
   contains
      procedure, public, pass :: set_four
      procedure, public, pass :: set_west
      procedure, public, pass :: get_west
      procedure, public, pass :: set_east
      procedure, public, pass :: get_east
      procedure, public, pass :: set_south
      procedure, public, pass :: get_south
      procedure, public, pass :: set_north
      procedure, public, pass :: get_north
      procedure, public, pass :: set_is_valid
      procedure, public, pass :: get_is_valid
      procedure, public, pass :: check_valid_range
      procedure, public, pass :: check => test_and_update_valid_flag
      procedure, public, pass :: is_west_edge_between_0_and_359
      procedure, public, pass :: is_east_edge_between_1_and_360
      procedure, public, pass :: is_south_edge_between_m90_and_p89
      procedure, public, pass :: is_north_edge_between_m89_and_p90
      procedure, public, pass :: is_north_edge_NORTH_OF_south_edge
      procedure, public, pass :: is_east_edge_EAST_OF_west_edge
   end type boundary

   ! interface of constructor
   interface boundary
      module procedure init_boundary
   end interface boundary


contains

   
!-- definition of constructor
   function init_boundary() result(init)
      type(boundary) :: init

      call init%set_west(0)
      call init%set_east(0)
      call init%set_south(0)
      call init%set_north(0)

      call init%set_is_valid(.true.)
      
   end function init_boundary


!-- setter and getter

   subroutine set_four(self, west, east, south, north)
      class(boundary), intent(inout) :: self
      integer(int32), intent(in) :: west, east, south, north
      
      call self%set_west(west)
      call self%set_east(east)
      call self%set_south(south)
      call self%set_north(north)
   end subroutine set_four

   subroutine set_west(self, lon)
      class(boundary), intent(inout) :: self
      integer(int32), intent(in) :: lon
      
      self%west_lon = lon

   end subroutine set_west


   function get_west(self) result(lon)
      class(boundary), intent(in) :: self
      integer(int32) :: lon

      lon = self%west_lon
   
   end function get_west


   subroutine set_east(self, lon)
      class(boundary), intent(inout) :: self
      integer(int32), intent(in) :: lon
      
      self%east_lon = lon

   end subroutine set_east
   

   function get_east(self) result(lon)
      class(boundary), intent(in) :: self
      integer(int32) :: lon

      lon = self%east_lon
   
   end function get_east


   subroutine set_south(self, lat)
      class(boundary), intent(inout) :: self
      integer(int32), intent(in) :: lat

      self%south_lat = lat
      
   end subroutine set_south


   function get_south(self) result(lat)
      class(boundary), intent(in) :: self
      integer(int32) :: lat

      lat = self%south_lat

   end function get_south


   subroutine set_north(self, lat)
      class(boundary), intent(inout) :: self
      integer(int32), intent(in) :: lat
      
      self%north_lat = lat

   end subroutine set_north


   function get_north(self) result(lat)
      class(boundary), intent(in) :: self
      integer(int32) :: lat

      lat = self%north_lat

   end function get_north 

   
   subroutine set_is_valid(self, flag)
      class(boundary), intent(inout) :: self
      logical, intent(in) :: flag

      self%is_valid = flag

   end subroutine set_is_valid


   function get_is_valid(self) result(flag)
      class(boundary), intent(in) :: self
      logical :: flag

      flag = self%is_valid
   
   end function get_is_valid


!-- validation check
   function is_west_edge_between_0_and_359(self) result(flag)
      class(boundary), intent(in) :: self
      logical :: flag
      integer(int32) :: west

      west = self%west_lon

      !valid range
      if (0 <= west .and. west <= 359) then
         flag = .true.

      !invalid range
      else
         flag = .false.
      end if

   end function is_west_edge_between_0_and_359


   function is_east_edge_between_1_and_360(self) result(flag)
      class(boundary), intent(in) :: self
      logical :: flag
      integer(int32) :: east

      east = self%east_lon

      if (1 <= east .and. east <= 360) then
         flag = .true.

      else
         flag = .false.
      end if

   end function is_east_edge_between_1_and_360


   function is_south_edge_between_m90_and_p89(self) result(flag)
      class(boundary), intent(in) :: self
      logical :: flag
      integer(int32) :: south

      south = self%south_lat

      if (-90 <= south .and. south <= 89) then
         flag = .true.

      else
         flag = .false.
      end if

   end function is_south_edge_between_m90_and_p89


   function is_north_edge_between_m89_and_p90(self) result(flag)
      class(boundary), intent(in) :: self
      logical :: flag
      integer(int32) :: north

      north = self%north_lat

      if (-89 <= north .and. north <= 90) then
         flag = .true.

      else
         flag = .false.
      end if

   end function is_north_edge_between_m89_and_p90


   function is_east_edge_EAST_OF_west_edge(self) result(flag)
      class(boundary), intent(in) :: self
      logical :: flag
      integer(int32) :: west, east

      west = self%west_lon
      east = self%east_lon

      if (west < east) then
         flag = .true.

      else
         flag = .false.
      end if

   end function is_east_edge_EAST_OF_west_edge


   function is_north_edge_NORTH_OF_south_edge(self) result(flag)
      class(boundary), intent(in) :: self
      logical :: flag
      integer(int32) :: south, north

      south = self%south_lat
      north = self%north_lat

      if (south < north) then
         flag = .true.
      
      else
         flag = .false.
      end if

   end function is_north_edge_NORTH_OF_south_edge


!-- call self%this_function(checker_function)
   subroutine test_and_update_valid_flag(self, f)
      class(boundary), intent(inout) :: self
      logical :: flag

      interface
         logical function f(this)
            import boundary
            class(boundary), intent(in) :: this
         end function
      end interface

      flag = f(self)

      self%is_valid = flag .and. self%is_valid

   end subroutine test_and_update_valid_flag


!-- call validation checks
   subroutine check_valid_range(self)
      class(boundary), intent(inout) :: self

      call self%check(is_west_edge_between_0_and_359)
      call self%check(is_east_edge_between_1_and_360)
      call self%check(is_south_edge_between_m90_and_p89)
      call self%check(is_north_edge_between_m89_and_p90)

      call self%check(is_north_edge_NORTH_OF_south_edge)
      call self%check(is_east_edge_EAST_OF_west_edge)

   end subroutine check_valid_range


end module boundary_t