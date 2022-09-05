module mod_read_img
   use, intrinsic :: iso_fortran_env
   use mod_read_lbl
   use mod_tile
   implicit none


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
      procedure :: clear => image_clear
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
      integer(int16), allocatable :: samples(:)
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
      allocate(self%dem(self%nlat, self%nlon), source=int2(0))
      ! 配列を初期化する。
      ! self%dem(:,:) = 0

      ! 一時メモリを確保
      allocate(samples(self%nlon), source=int2(0))

      ! 既存のIMGファイルをバイナリストリームで開く。
      open(newunit=unit, file=self%filename, form='unformatted', access='stream', status='old')

      ! lines方向(下から上)に読み込み
      do j = 1, self%nlat
         ! 緯度方向には南から書き込むために、ループ変数から対応するインデックスを計算して代入する。
         ! jj = self%nlat + 1 - j
         ! do i = self%nlon, 1, -1
         !    ! 入力ストリームから2バイト読み込んで、変数valに代入する。。
         !    read(unit, end=100, err=110, iomsg=errmsg) val           
         !    ! 読み込んだ値をバイトスワップして、配列に書き込む。
         !    self%dem(i,j) = swap16(val)
         ! end do

         ! 1行(samples; 画像の右から左)読み込み
         do i = 1, self%nlon
            read(unit, err=110, iomsg=errmsg) val
            samples(i) = swap16(val)
         end do

         ! データ書き込み
         self%dem(:,j) = samples(:)

         ! clear
         samples(:) = int2(0)

      end do

100   continue
      ! Regular
      if(allocated(samples)) then
         deallocate(samples)
      end if

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
      allocate(single%data(nlat, nlon))
      ! do j = 1, nlat
      !    do i = 1, nlon
      single%data(1:nlat,1:nlon) = self%dem(1:nlat,1:nlon)
      !    end do
      ! end do 

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

   
   subroutine image_clear(self)
      class(Image), intent(inout) :: self

      self%filename = ''
      self%nlon = 0
      self%nlat = 0
      self%west_lon = 0
      self%east_lon = 0
      self%south_lat = 0
      self%north_lat = 0
      if (allocated(self%dem)) then
         deallocate(self%dem)
      end if
      call self%label%clear()

   end subroutine image_clear


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