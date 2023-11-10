module division_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: base_m
   use :: boundary_t
   use :: dataname_m

contains

   subroutine allocate_lists(edge, list, numlist_1d, numlist_2d, logical_array)
      use global_m
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

   end subroutine allocate_lists


   subroutine create_filelist(data_root, edge, list)
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

            call create_data_name(e_west, e_north, code)

            write(lon_dir, '(a,i3.3)') 'lon', e_west

            list(i,j) = trim(data_root) // '/' // trim(lon_dir) // '/' // code

         end do
      end do
   end subroutine create_filelist


   subroutine set_distribution (distri_1d, distri_2d, distri_logical)
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

   end subroutine set_distribution

end module division_m