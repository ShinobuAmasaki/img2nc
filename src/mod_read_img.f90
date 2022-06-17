module mod_read_img
   use, intrinsic :: iso_fortran_env
   use mod_read_lbl
   implicit none

   ! integer(int32) :: samples=4096

   type Tile
      private
      integer(int32), public :: west_lon, east_lon, south_lat, north_lat
      integer(int32), public, allocatable :: data(:,:)
   contains
      procedure :: halve => tile_halve_shrink
   end type Tile

   type Image
      private
      character(len=256)   :: filename
      integer(int32)       :: nlon, nlat
      real(real64), public :: west_lon, east_lon, south_lat, north_lat
      integer(int16), public, allocatable :: dem(:,:) 
      type(label), public :: label
   contains
      procedure :: set_name => image_set_name
      procedure :: read_lbl => image_read_lbl
      procedure :: load_image => image_load_img
      procedure :: size_dem => image_size_dem
      procedure :: img2tile => image_convert_to_tile
   end type Image

contains

   subroutine image_set_name(self, code)
      class(image) :: self
      character(len=256) :: code

      self%filename = trim(code) // '.img'
      call self%label%set_name(code)
      return
   end subroutine image_set_name

   subroutine image_read_lbl(self)
      class(image) :: self

      call self%label%read_lbl()
   end subroutine image_read_lbl

   subroutine image_load_img(self)
      class(image) :: self
      integer :: unit
      integer :: i, j, jj
      integer(int16) :: val
      character(len=100) :: errmsg

      ! 経度方向のデータ数を構造体変数nlonに代入する。
      self%nlon = self%label%get_nlon()
      ! 緯度方向のデータ数を構造体変数nlatに代入する。
      self%nlat = self%label%get_nlat()

      ! 東西南北の端の経緯度を、変数labelから取得して、構造体変数に代入する。
      self%east_lon = self%label%get_east()
      self%west_lon = self%label%get_west()
      self%south_lat = self%label%get_south()
      self%north_lat = self%label%get_north()

      ! 構造体の配列変数demを経度・緯度のデータ数の範囲で割り付ける。
      allocate(self%dem(self%nlon, self%nlat))
      ! 配列を初期化する。
      self%dem(:,:) = 0

      ! 既存のIMGファイルをバイナリストリームで開く。
      open(newunit=unit, file=self%filename, form='unformatted', access='stream', status='old')

      ! データの読み込む。
      do j = 1, self%nlat
         do i = 1, self%nlon

            ! 緯度方向には南から書き込むために、ループ変数から対応するインデックスを計算して代入する。
            jj = self%nlat + 1 - j

            ! 入力ストリームから2バイト読み込んで、変数valに代入する。。
            read(unit, end=100, err=110, iomsg=errmsg) val
            
            ! 読み込んだ値をバイトスワップして、配列に書き込む。
            self%dem(i,jj) = swap16(val)

         end do
      end do

100   continue
      close(unit)
      return

110   continue
      print *, 'IO Error: ', trim(errmsg)
      stop      

   end subroutine image_load_img

   function image_size_dem(self, dim) result(size_dem)
      class(image) :: self
      integer(int32) :: dim
      integer(int32) :: size_dem

      size_dem = size(self%dem, dim)
   end function image_size_dem

   function image_convert_to_tile(self, shrink) result(single)
      class(image) :: self
      type(Tile) :: single
      integer :: i, j 
      integer(int32) :: nlon, nlat
      integer(int32), optional :: shrink
      ! real(real64) :: west, east, south, north, step_lon, step_lat

      !経緯度の標本数を変数に代入する。
      nlon = self%nlon
      nlat = self%nlat
      
      !領域端の経緯度を変数に代入する。
      single%west_lon = nint(self%west_lon)
      single%east_lon = nint(self%east_lon)
      single%south_lat = nint(self%south_lat)
      single%north_lat = nint(self%north_lat)

      !データ配列の定義
      allocate(single%data(nlon, nlat))
      do j = 1, nlat
         do i = 1, nlon
            single%data(i,j) = self%dem(i,j)
         end do
      end do 

      !引数shrinkの指定がある場合
      if ( present(shrink) ) then
         if (shrink == 1) then
            return 

         else if (shrink == 2) then
            call tile_halve_shrink(single)
         
         else if (shrink == 4) then
            call tile_quarter_shrink(single)
         
         else if (shrink == 8) then
            call tile_eighth_shrink(single)
         
         else if (shrink == 16) then
            call tile_sixteenth_shrink(single)
         
         ! else if (shrink == 32) then
            ! call 
         end if
      end if
         
   end function image_convert_to_tile

! ----------------------------------------------------- !
   subroutine tile_halve_shrink(self)
      class(Tile) :: self
      ! real(real64), allocatable ::  work_lon(:), work_lat(:)
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
   subroutine array_16_shrink(array)
      type(Tile), intent(inout) :: array(:,:)
      integer(int32) :: i, j, m, n

      m = size(array, dim=1)
      n = size(array, dim=2)

      ! print *, m, n

      print *, 'Progress: 1/16 shrink processing.'
      do i = 1, m
         do j = 1, n
            call tile_sixteenth_shrink(array(i,j))
         end do
      end do
   
   end subroutine array_16_shrink

 ! ----------------------------------------------------- !
   function size_of_tile_array(array, dim) result(total)
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
   end function size_of_tile_array

! ----------------------------------------------------- !

   ! ビットシフトにより16bit整数型のエンディアンを変換する関数
   integer(int16) function swap16(value)
      integer(int16), intent(in) :: value

      ! 引数valueの値を左に8桁シフト
      swap16 = ishft(value,8)
      ! valueを右に8桁シフトした値と、左にシフトした値の論理和をとる。
      swap16 = ior(swap16,ishft(value,-8))
   end function swap16

end module mod_read_img