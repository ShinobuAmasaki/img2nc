module sldem2013
   use, intrinsic :: iso_fortran_env
   use :: base_m
   implicit none

   interface create_data_name_sldem2013
      module procedure :: create_data_name_sldem2013
   end interface create_data_name_sldem2013

   interface allocate_lists
      module procedure :: allocate_lists_sldem2013
   end interface 

   interface create_name_list
      module procedure :: create_filelist_sldem2013
   end interface

   interface set_distribution
      module procedure :: set_distribution_sldem2013
   end interface

contains

   subroutine create_data_name_sldem2013(west_arg, north_arg, code)
      implicit none
      integer(int32), intent(in) :: west_arg, north_arg
 
      integer(int32) :: west, north
      integer(int32) :: east, south
      character(len=*), intent(out) :: code
      character(len=11) :: prefix
      character(len=2) :: postfix
      character(len=4) :: e_west, e_east
      character(len=3) :: e_north, e_south

      west = west_arg
      north = north_arg

      if (west < 0) then
         west = west + 360
      end if

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

   end subroutine create_data_name_sldem2013


   subroutine create_name_list_sldem2013(data_root, list, edge)
      use :: boundary_t
      implicit none
      character(*), intent(in) :: data_root
      character(len=MAX_NAME_LEN), intent(out) :: list(:, :)
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

            call create_data_name_sldem2013(west_edge, north_edge, code)

            write(lon_dir, '(a, i3.3)') 'lon', west_edge

            list(i,j) = trim(data_root)// '/' //trim(lon_dir)// '/' // code 

         end do
      end do

   end subroutine create_name_list_sldem2013


   subroutine allocate_lists_sldem2013(edge, list, numlist_1d, numlist_2d, logical_array)
      use global_m
      use boundary_t
      implicit none
      
      character(len=MAX_PATH_LEN), intent(out), allocatable :: list(:,:)
      integer(int32), intent(out), allocatable :: numlist_2d(:,:), numlist_1d(:)
      logical, intent(out), allocatable :: logical_array(:,:)
      type(boundary), intent(in) :: edge
      integer(int32) :: lon_size, lat_size

      lon_size = edge%get_east() - edge%get_west()
      lat_size = edge%get_north() - edge%get_south()

      allocate(list(lon_size, lat_size))
      allocate(numlist_2d(lon_size, lat_size))
      allocate(numlist_1d(lon_size*lat_size))
      allocate(logical_array(lon_size, lat_size))

   end subroutine allocate_lists_sldem2013


   subroutine create_filelist_sldem2013(data_root, edge, list)
      use boundary_t
      implicit none
      
      character(len=*), intent(in) :: data_root
      type(boundary), intent(inout) :: edge
      character(len=MAX_PATH_LEN), intent(out) :: list(:,:)
         ! assume name_list is already allocated
      character(len=27) :: code
      integer(int32) :: siz_lon, siz_lat, i, j, ii, jj, e_west, e_north
      character(len=6) :: lon_dir


      siz_lon = edge%get_east() - edge%get_west()
      siz_lat = edge%get_north() - edge%get_south()

      do j = 1, siz_lat
         !南から北へ
         ! e_north = edge%get_north() - j + 1
         e_north = edge%get_south() + j

         do i = 1, siz_lon
            !西から東へ
            
            e_west = edge%get_west() + i - 1

            if (edge%get_west() < 0) then
               e_west = e_west + 360
            end if

            if (e_west > 359 ) then
               e_west = e_west - 360
            end if
            

            call create_data_name_sldem2013(e_west, e_north, code)

            write(lon_dir, '(a,i3.3)') 'lon', e_west

            list(i,j) = trim(data_root) // '/' // trim(lon_dir) // '/' // code

         end do
      end do
   end subroutine create_filelist_sldem2013


   subroutine set_distribution_sldem2013 (distri_1d, distri_2d, distri_logical)
      use global_m
      implicit none
      integer(int32), intent(inout) :: distri_1d(:), distri_2d(:, :)
      logical, intent(inout) :: distri_logical(:, :)
      integer :: peMax, i, j
      integer :: num_i, num_j

      num_i = size(distri_2d, dim=1)
      num_j = size(distri_2d, dim=2)

      peMax = num_i * num_j 

      if (petot > peMax) then
         if (isIm1) then
            write(stderr, *) "ERROR: Too many processor elements."
            print *, petot, peMax
         end if
         call gently_stop()
      end if

      do i = 1, num_i*num_j
         if (petot /= 1) then
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

   end subroutine set_distribution_sldem2013


end module sldem2013
