module read_img
   use iso_fortran_env
   implicit none

   type Image      
      private
      character(len=256)   :: filename
      integer(int32)       :: nlon, nlat
      real(real64), public :: west_lon, east_lon, south_lat, north_lat

      integer(int16), public, allocatable :: dem(:,:)
      
      type(label) :: label

   contains
      ! Procedure
      procedure :: set_name => image_set_name
      procedure :: read_lbl => image_read_lbl
      procedure :: load_image => image_load_img
      procedure :: size_dem => image_size_dem

   end type Image

contains

   subroutine image_set_name(self, code)
      class(image) :: self
      character(len=256) :: code

      self%filename = trim(code) // '.img'
      
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
      self%nlon = self%label%line_samples
      ! 緯度方向のデータ数を構造体変数nlatに代入する。
      self%nlat = self%label%lines

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

   ! ビットシフトにより16bit整数型のエンディアンを変換する関数
   integer(int16) function swap16(value)
      integer(int16), intent(in) :: value

      ! 引数valueの値を左に8桁シフト
      swap16 = ishft(value,8)
      ! valueを右に8桁シフトした値と、左にシフトした値の論理和をとる。
      swap16 = ior(swap16,ishft(value,-8))
   end function swap16

end module read_img