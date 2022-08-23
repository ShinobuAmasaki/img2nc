module mod_read_lbl
   use,intrinsic :: iso_fortran_env
   implicit none

   type Label
      private
      character(len=256) :: filename

      ! OBJECT = IMAGE_MAP_PROJECTION
      real(real64) :: north_lat, south_lat
      real(real64) :: west_lon, east_lon 

      ! OBJECT = IMAGE
      integer(int32)       :: bands
      character(len=256)   :: band_storage_type, band_name
      
      integer(int32)       :: lines, line_samples
      character(len=256)   :: sample_type
      integer(int32)       :: sample_bits
      character(len=256)   :: image_value_type, sample_bit_mask
      real(real64)         :: offset, scaling_factor
      character(len=256)   :: stretched_flag
      integer(int32)       :: valid_minimum, valid_maximum, dummy, minimum, maximum
      real(real64)         :: average, stdev
      integer(int32)       :: mode_pixel
      character(len=256)   :: unit
   contains
      ! Procedure
      procedure :: get_name => label_get_name
      procedure :: set_name => label_set_name
      procedure :: read_lbl => label_read_lbl
      procedure :: get_east => label_get_east_lon
      procedure :: get_west => label_get_west_lon
      procedure :: get_south => label_get_south_lat
      procedure :: get_north => label_get_north_lat
      procedure :: get_nlon => label_get_line_samples
      procedure :: get_nlat => label_get_lines
      procedure :: clear => label_clear
      
   end type Label

contains

   subroutine label_get_name(self, name)
      class(label) :: self
      character(*), intent(out) :: name 

      name = self%filename
   end subroutine label_get_name

   !引数にコードを受け取って
   subroutine label_set_name(self, code)
      class(label) :: self
      character(len=256), intent(in) :: code

      self%filename = trim(code) // '.lbl'
   end subroutine label_set_name

   !lblファイルから一部のパラメーターを読み込む手続き
   subroutine label_read_lbl(self)
      class(label) :: self
      integer :: unit
      character(len=256) :: str, symbol, object

      open(newunit=unit, file=self%filename, form='formatted', access='stream',status='old')

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

   function label_get_line_samples(self) result(line_samples)
      class(label) :: self
      integer(int32) :: line_samples

      line_samples = self%line_samples
      return
   end function label_get_line_samples

   function label_get_lines(self) result(lines)
      class(label) :: self
      integer(int32) :: lines

      lines = self%lines
      return
   end function label_get_lines

   subroutine label_clear(self)
      class(label) :: self
      
      self%filename = ''
      self%north_lat = 0d0
      self%south_lat = 0d0
      self%west_lon = 0d0
      self%east_lon = 0d0
      
      self%bands = 0
      self%band_storage_type = ''
      self%band_name = ''
      
      self%lines = 0
      self%line_samples = 0
      self%sample_type  = ''
      self%sample_bits  = 0
      self%image_value_type = ''
      self%sample_bit_mask  = ''
      self%offset = 0d0
      self%scaling_factor = 0d0
      self%stretched_flag = ''
      self%valid_minimum = 0
      self%valid_maximum = 0
      self%dummy = 0
      self%average = 0d0
      self%stdev = 0d0
      self%mode_pixel = 0
      self%unit = ''

   end subroutine label_clear

end module mod_read_lbl