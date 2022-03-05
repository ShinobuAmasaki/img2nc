module read_img
   use iso_fortran_env
   implicit none

   ! integer :: i

   type Label
      private
      character(len=256) :: infile_lbl

      ! OBJECT = IMAGE_MAP_PROJECTION
      real(real64) :: north_lat, south_lat
      real(real64) :: west_lon, east_lon 

      ! OBJECT = IMAGE
      integer(int32) :: bands
      character(len=256) :: band_storage_type, band_name
      integer(int32) :: lines, line_samples
      character(len=256) :: sample_type
      integer(int32) :: sample_bits
      character(len=256) :: image_value_type, sample_bit_mask
      real(real64) :: offset, scaling_factor
      character(len=256) :: stretched_flag
      integer(int32) :: valid_minimum, valid_maximum, dummy, minimum, maximum
      real(real64) :: average, stdev
      integer(int32) :: mode_pixel
      character(len=256) :: unit
   contains
      ! Procedure
      procedure :: get_name => label_get_name
      procedure :: set_name => label_set_name
      procedure :: read_lbl => label_read_lbl
      procedure :: get_east => label_get_east_lon
      procedure :: get_west => label_get_west_lon
      procedure :: get_south => label_get_south_lat
      procedure :: get_north => label_get_north_lat
      
   end type Label

   type Image      
      private
      character(len=256) :: infile_img
      integer(int32) :: nlon, nlat
      integer(int16), allocatable :: dem(:,:)
      real(real64), public :: west_lon, east_lon, south_lat, north_lat
      type(label), public :: label
   contains
      ! Procedure
      procedure :: set_name => image_set_name
      ! procedure :: get_name => image_get_name
      procedure :: read_lbl => image_read_lbl
      procedure :: load_image => image_load_img
      procedure :: size_dem => image_size_dem
      procedure :: load_data => image_load_data

   end type Image

contains

   subroutine label_get_name(self, name)
      class(label) :: self
      character(*), intent(out) :: name 

      name = self%infile_lbl
   end subroutine label_get_name

   subroutine label_set_name(self, code)
      class(label) :: self
      character(len=256), intent(in) :: code

      self%infile_lbl = trim(code) // '.lbl'
   end subroutine label_set_name

   subroutine label_read_lbl(self)
      class(label) :: self
      integer :: unit, index
      character(len=256) :: str, symbol, object

      open(newunit=unit, file=self%infile_lbl, form='formatted', access='stream',status='old')

      ! 行を読み込んで'MAP_SCALE ='の行までループして移動する。
      do while (.true.)
         read(unit, *) str, symbol, object
         if(trim(str) .eq. 'MAP_SCALE') exit
      end do

      ! ラベルファイルのデータ値を構造体変数に読み込む。
      read(unit,*) str, symbol, self%north_lat
      read(unit,*) str, symbol, self%south_lat
      read(unit,*) str, symbol, self%east_lon
      read(unit,*) str, symbol, self%west_lon

      ! 行を読み込んで'OBJRCT = IMAGE'の行までループして移動する。
      do while (.true.)
         read(unit, *) str, symbol , object
         if (trim(object) .eq. 'IMAGE') exit
      enddo

      ! ラベルファイルのデータ値を構造体変数に読み込む。
      read(unit,*) str, symbol, self%bands
      read(unit,*) str, symbol, self%band_storage_type
      read(unit,*) str, symbol, self%band_name
      read(unit,*) str, symbol, self%lines
      read(unit,*) str, symbol, self%line_samples
      read(unit,*) str, symbol, self%sample_type
      read(unit,*) str, symbol, self%sample_bits
      read(unit,*) str, symbol, self%image_value_type
      read(unit,*) str, symbol, self%sample_bit_mask
      read(unit,*) str, symbol, self%offset
      read(unit,*) str, symbol, self%scaling_factor
      read(unit,*) str, symbol, self%stretched_flag
      read(unit,*) str, symbol, self%valid_minimum
      read(unit,*) str, symbol, self%valid_maximum
      read(unit,*) str, symbol, self%dummy
      read(unit,*) str, symbol, self%minimum
      read(unit,*) str, symbol, self%maximum
      read(unit,*) str, symbol, self%average
      read(unit,*) str, symbol, self%stdev
      read(unit,*) str, symbol, self%mode_pixel
      read(unit,*) str, symbol, self%unit

      close(unit)
      return

   end subroutine label_read_lbl

   function label_get_east_lon(self) result(lon)
      class(label) :: self
      real(real64) :: lon

      lon = self%east_lon
      return
   end function label_get_east_lon

   function label_get_west_lon(self) result(lon)
      class(label) :: self
      real(real64) :: lon

      lon = self%west_lon
      return
   end function label_get_west_lon

   function label_get_south_lat(self) result(lat)
      class(label) :: self
      real(real64) :: lat

      lat = self%south_lat
      return
   end function label_get_south_lat

   function label_get_north_lat(self) result(lat)
      class(label) :: self
      real(real64) :: lat

      lat = self%north_lat
      return
   end function label_get_north_lat

!------------------------------------------------!
   subroutine image_set_name(self, code)
      class(image) :: self
      character(len=256) :: code

      call self%label%set_name(trim(code))

      self%infile_img = trim(code) // '.img'
      return
   end subroutine image_set_name

   subroutine image_read_lbl(self)
      class(image) :: self

      call self%label%read_lbl()
   end subroutine image_read_lbl

   subroutine image_load_img(self)
      class(image) :: self
      character(len=256) :: filename
      integer :: index, unit
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
      open(newunit=unit, file=self%infile_img, form='unformatted', access='stream', status='old')

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

   subroutine image_load_data(self, data)
      class(image) :: self
      real(real64), intent(out) :: data(:,:)

      data(:,:) = dble(self%dem(:,:))
   end subroutine image_load_data

   ! ビットシフトにより16bit整数型のエンディアンを変換する関数
   integer(int16) function swap16(value)
      integer(int16), intent(in) :: value

      ! 引数valueの値を左に8桁シフト
      swap16 = ishft(value,8)
      ! valueを右に8桁シフトした値と、左にシフトした値の論理和をとる。
      swap16 = ior(swap16,ishft(value,-8))
   end function swap16

end module read_img