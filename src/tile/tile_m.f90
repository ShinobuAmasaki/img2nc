module Tile_m
   use, intrinsic :: iso_fortran_env
   use :: Label_m
   use :: base_m
   use :: buff_t
   implicit none
   private

   type, public :: Tile
      private
      character(MAX_PATH_LEN) :: path_to_img
      integer(int32), public :: west_lon, east_lon, south_lat, north_lat
      type(buff),public, pointer :: p_data => null()
      integer(int16), public, allocatable :: shrinked_data(:, :) 
      type(LabelFile) :: lbl
   contains
      procedure :: set_path
      procedure :: get_path

   end type Tile

   
contains

   subroutine set_path(self, name)
      implicit none
      class(tile) :: self
      character(MAX_PATH_LEN), intent(in) :: name

      self%path_to_img = trim(adjustl(name))

   end subroutine

   function get_path(self) result(res)
      implicit none
      class(tile) :: self
      character(:), allocatable :: res

      res = self%path_to_img
   end function get_path


   ! lblの読み込み、
   subroutine preprocess(self)
      
      implicit none
      class(Tile) :: self

   end subroutine

   subroutine shrink_process(self, coarse)
      implicit none
      class(Tile) :: self
      integer(int32) :: coarse
   end subroutine

   subroutine postprocess()
   end subroutine

end module Tile_m