module mod_tile
   use, intrinsic :: iso_fortran_env
   use mod_boundary

   
   type Tile
      private
      integer(int32), public :: west_lon, east_lon, south_lat, north_lat
      integer(int16), public, dimension(:,:), pointer :: p_data => null()
      integer(int16), public, allocatable :: data(:,:)
   contains
      procedure :: halve => tile_halve_shrink
      procedure :: quater => tile_quarter_shrink
      procedure :: one_eighth => tile_eighth_shrink
      procedure :: one_sixteenth => tile_sixteenth_shrink
      procedure :: read_boundary => set_lonlat_to_tile_from_boundary
   end type Tile


contains

! ----------------------------------------------------- !
   subroutine tile_halve_shrink(self)
      class(Tile) :: self
      integer(int32), allocatable :: work(:,:)
      integer(int32) :: i, j, ii, jj, nx, ny, nx_h, ny_h

      !タイルのデータの大きさを変数に代入する。
      nx = size(self%data, dim=1)
      ny = size(self%data, dim=2)
      nx_h = nx / 2 ! 4096 -> 2048
      ny_h = ny / 2

      !work配列を割り付けてに入力配列をコピーする。
      allocate(work(nx,ny))
      work(1:nx,1:ny) = self%data(1:nx,1:ny) 

      !タイルの割り付けを解放し、新たに半分のサイズで割り付ける。
      deallocate(self%data)
      allocate(self%data(nx_h,ny_h))

      ! work配列からタイルへコピーする。
      do j = 1, ny, 2
         do i = 1, nx, 2
            ii = ceiling(i/2.)
            jj = ceiling(j/2.)

            !4点のデータを平均してタイルに代入する。
            ! self%data(ii,jj) = nint( (work(i,j)+work(i+1,j)+work(i,j+1)+work(i+1,j+1)) / 4.0 )
            self%data(ii,jj) = nint( sum(work(i:i+1, j:j+1)) / 4.0 )
         end do
      end do
      
      !work配列の割り付けを解放する。
      deallocate(work)

      return
   end subroutine tile_halve_shrink

! ----------------------------------------------------- !
   subroutine tile_quarter_shrink(self)
      class(Tile) :: self
      integer(int32), allocatable :: work(:,:)
      integer(int32) :: i, j, ii, jj, nx, ny, nx_q, ny_q
   
      !タイルのデータの大きさを変数に代入する。
      nx = size(self%data, dim=1)
      ny = size(self%data, dim=2)
      nx_q = nx / 4 ! 4096 -> 1024
      ny_q = ny / 4 


      !work配列を割り付け、入力をコピーする。
      allocate(work(nx,ny))
      work(1:nx,1:ny) = self%data(1:nx,1:ny)

      !タイルを8分の1サイズに再割り付け
      deallocate(self%data)
      allocate(self%data(nx_q, ny_q))

      !work配列からタイルにコピーする。
      do j = 1, ny, 4
         do i = 1, nx, 4
            ii = ceiling(i/4.)
            jj = ceiling(j/4.)

            !64点のデータを平均してタイルに代入する。
            self%data(ii,jj) = nint( sum(work(i:i+3, j:j+3)) / 16.0 )
         end do
      end do

      deallocate(work)

   end subroutine tile_quarter_shrink

! ----------------------------------------------------- !
   subroutine tile_eighth_shrink(self)
      class(Tile) :: self
      integer(int32), allocatable :: work(:,:)
      integer(int32) :: i, j, ii, jj, nx, ny, nx_e, ny_e
   
      !タイルのデータの大きさを変数に代入する。
      nx = size(self%data, dim=1)
      ny = size(self%data, dim=2)
      nx_e = nx / 8 ! 4096 -> 512
      ny_e = ny / 8 


      !work配列を割り付け、入力をコピーする。
      allocate(work(nx,ny))
      work(1:nx,1:ny) = self%data(1:nx,1:ny)

      !タイルを8分の1サイズに再割り付け
      deallocate(self%data)
      allocate(self%data(nx_e, ny_e))

      !work配列からタイルにコピーする。
      do j = 1, ny, 8
         do i = 1, nx, 8
            ii = ceiling(i/8.)
            jj = ceiling(j/8.)

            !64点のデータを平均してタイルに代入する。
            self%data(ii,jj) = nint( sum(work(i:i+7, j:j+7)) / 64.0 )
         end do
      end do

      deallocate(work)

   end subroutine tile_eighth_shrink

! ----------------------------------------------------- !
   subroutine tile_sixteenth_shrink(self)
      class(Tile) :: self
      integer(int32), allocatable :: work(:,:)
      integer(int32) :: i, j, ii, jj, nx, ny, nx_s, ny_s
   
      !タイルのデータの大きさを変数に代入する。
      nx = size(self%data, dim=1)
      ny = size(self%data, dim=2)
      nx_s = nx / 16 ! 4096 -> 256
      ny_s = ny / 16 


      !work配列を割り付け、入力をコピーする。
      allocate(work(nx,ny))
      work(1:nx,1:ny) = self%data(1:nx,1:ny)

      !タイルを8分の1サイズに再割り付け
      deallocate(self%data)
      allocate(self%data(nx_s, ny_s))

      !work配列からタイルにコピーする。
      do j = 1, ny, 16
         do i = 1, nx, 16
            ii = ceiling(i/16.)
            jj = ceiling(j/16.)

            !256点のデータを平均してタイルに代入する。
            self%data(ii,jj) = nint( sum(work(i:i+15, j:j+15)) / 256.0 )
         end do
      end do

      deallocate(work)

   end subroutine tile_sixteenth_shrink

! ----------------------------------------------------- !

   subroutine set_lonlat_to_tile_from_boundary(self, bound)
      class(Tile), intent(inout) :: self
      type(boundary), intent(in) :: bound

      self%west_lon  = bound%get_west()
      self%east_lon  = bound%get_east()
      self%south_lat = bound%get_south()
      self%north_lat = bound%get_north()
   end subroutine set_lonlat_to_tile_from_boundary

! ----------------------------------------------------- !
   subroutine array_16_shrink(array)
      type(Tile), intent(inout) :: array(:,:)
      integer(int32) :: i, j, m, n

      m = size(array, dim=1)
      n = size(array, dim=2)

      ! print *, m, n

      print *, 'Progress: 1/16 shrink processing.'
      do j = 1, n
         do i = 1, m    
            call tile_sixteenth_shrink(array(i,j))
         end do
      end do
   
   end subroutine array_16_shrink

 ! ----------------------------------------------------- !
   function total_size_of_tile_array(array, dim) result(total)
      type(Tile), intent(in) :: array(:,:)
      integer(int32), intent(in) :: dim
      integer(int32) :: k, n, total

      total = 0
      n = size(array, dim=dim)

      do k = 1, n
      
         if (dim == 1) then
            total = total + size(array(k,1)%data, dim)

         else if (dim == 2) then
            total = total + size(array(1,k)%data, dim)

         end if
      
      end do
   end function total_size_of_tile_array


end module mod_tile