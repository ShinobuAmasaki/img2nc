module Label_m
   use, intrinsic :: iso_fortran_env
   use :: base_m
   implicit none
   private

   public :: LabelFile

   integer(int32), parameter, public :: LBL_KEY_LENGTH = 256
   integer(int32), parameter, public :: LBL_VALUE_LENGTH = 2048
   integer(int32), parameter, public :: LBL_BUFF_SIZE = 4096
   character(4), parameter, public :: LBL_NULL_STRING = 'NULL'
   character(3), parameter, public :: LBL_END_STATEMENT = 'END'

   integer(int32), parameter :: LBL_MAIN_SIZE = 34
   integer(int32), parameter :: LBL_PROJECTION_SIZE = 28
   integer(int32), parameter :: LBL_PROCPARA_SIZE = 5
   integer(int32), parameter :: LBL_IMAGE_SIZE = 21
   integer(int32), parameter :: LBL_QUALITYINFO_SIZE = 7
   

   ! LableData ∈ LabelObject ⊆ LabelFile

   ! LabelDataは、データ要素(key, value)である。
   ! LabelObjectは、LableDataの配列である。
   ! LabelFileは、複数個のLabelObjectからなり、lblファイル全体のデータを収容する。

   type LabelData
      character(LBL_KEY_LENGTH) :: key
      character(LBL_VALUE_LENGTH) :: value
   end type LabelData

   type LabelObject
      type(LabelData), allocatable :: data(:)
   contains
      procedure :: get_value => get_value_LabelObject
      procedure :: set_value => set_value_LableObject
      procedure :: read_line => read_line_LableOject
   end type LabelObject

   type LabelFile
      type(LabelObject) :: main
      type(LabelObject) :: projection
      type(LabelObject) :: process_para
      type(LabelObject) :: image
      type(LabelObject) :: quality_info
      character(LBL_BUFF_SIZE) :: buff = ''
   contains
      procedure :: get_value => get_value_LabelFile
      procedure :: read_file => read_file_buffered_LabelFile
      procedure :: init => init_LabelFile
      procedure :: readbuff => read_main_values_from_buff
   end type LabelFile

 
contains

   subroutine init_LabelFile(self)
      implicit none
      class(LabelFile), intent(inout) :: self

      call init_main_LabelFile(self)
      call init_projection_LabelFile(self)
      call init_process_para_LabelFile(self)
      call init_image_LabelFile(self)
      call init_quality_info_LabelFile(self)
      
   end subroutine
   

   subroutine init_main_LabelFile(self)
      use keys_m
      implicit none
      class(LabelFile), intent(inout) :: self

      if (allocated(self%main%data)) then
         deallocate(self%main%data)
      end if

      allocate(self%main%data(LBL_MAIN_SIZE))

      self%main%data(1)%key = PDS_VERSION_ID
      self%main%data(2)%key = RECORD_TYPE
      self%main%data(3)%key = FILE_NAME
      self%main%data(4)%key = HAT_IMAGE
      self%main%data(5)%key = MISSION_NAME
      self%main%data(6)%key = DATA_SET_ID
      self%main%data(7)%key = DATA_SET_NAME
      self%main%data(8)%key = L2DB_ORIGINAL_ID
      self%main%data(9)%key = PRODUCT_ID
      self%main%data(10)%key = INSTRUMENT_TYPE
      self%main%data(11)%key = INSTRUMENT_ID
      self%main%data(12)%key = INSTRUMENT_NAME
      self%main%data(13)%key = INSTRUMENT_HOST_NAME
      self%main%data(14)%key = TARGET_TYPE
      self%main%data(15)%key = TARGET_NAME
      self%main%data(16)%key = START_TIME
      self%main%data(17)%key = STOP_TIME
      self%main%data(18)%key = SOFTWARE_NAME
      self%main%data(19)%key = SOFTWARE_VERSION
      self%main%data(20)%key = PROCESS_VERSION_ID
      self%main%data(21)%key = PRODUCT_CREATION_TIME
      self%main%data(22)%key = PRODUCER_ID  
      self%main%data(23)%key = PRODUCT_SET_ID
      self%main%data(24)%key = PRODUCT_VERSION_ID
      self%main%data(25)%key = UPPER_LEFT_LATITUDE
      self%main%data(26)%key = UPPER_LEFT_LONGITUDE
      self%main%data(27)%key = UPPER_RIGHT_LATITUDE
      self%main%data(28)%key = UPPER_RIGHT_LONGITUDE
      self%main%data(29)%key = LOWER_LEFT_LATITUDE
      self%main%data(30)%key = LOWER_LEFT_LONGITUDE
      self%main%data(31)%key = LOWER_RIGHT_LATITUDE
      self%main%data(32)%key = LOWER_RIGHT_LONGITUDE
      self%main%data(33)%key = IMAGE_CENTER_LATITUDE
      self%main%data(34)%key = IMAGE_CENTER_LONGITUDE

   end subroutine init_main_LabelFile

   
   subroutine init_projection_LabelFile(self)
      use :: keys_m
      implicit none
      class(LabelFile) :: self

      if (allocated(self%projection%data)) then
         deallocate(self%projection%data)
      end if

      allocate(self%projection%data(LBL_PROJECTION_SIZE))

      self%projection%data(1)%key = HAT_DATA_SET_MAP_PROJECTION
      self%projection%data(2)%key = MAP_PROJECTION_TYPE
      self%projection%data(3)%key = COORDINATE_SYSTEM_TYPE
      self%projection%data(4)%key = COORDINATE_SYSTEM_NAME
      self%projection%data(5)%key = A_AXIS_RADIUS
      self%projection%data(6)%key = B_AXIS_RADIUS
      self%projection%data(7)%key = C_AXIS_RADIUS
      self%projection%data(8)%key = FIRST_STANDARD_PARALLEL
      self%projection%data(9)%key = SECOND_STANDARD_PARALLEL
      self%projection%data(10)%key = POSITIVE_LONGITUDE_DIRECTION
      self%projection%data(11)%key = CENTER_LATITUDE
      self%projection%data(12)%key = CENTER_LONGITUDE
      self%projection%data(13)%key = REFERENCE_LATITUDE
      self%projection%data(14)%key = REFERENCE_LONGITUDE
      self%projection%data(15)%key = LINE_FIRST_PIXEL
      self%projection%data(16)%key = LINE_LAST_PIXEL
      self%projection%data(17)%key = SAMPLE_FIRST_PIXEL
      self%projection%data(18)%key = SAMPLE_LAST_PIXEL
      self%projection%data(19)%key = MAP_PROJECTION_ROTATION
      self%projection%data(20)%key = MAP_RESOLUTION
      self%projection%data(21)%key = MAP_SCALE
      self%projection%data(22)%key = MAXIMUM_LATITUDE
      self%projection%data(23)%key = MINIMUM_LATITUDE
      self%projection%data(24)%key = EASTERNMOST_LONGITUDE
      self%projection%data(25)%key = WESTERNMOST_LONGITUDE
      self%projection%data(26)%key = LINE_PROJECTION_OFFSET
      self%projection%data(27)%key = SAMPLE_PROJECTION_OFFSET
      self%projection%data(28)%key = RESAMPLING_METHOD

   end subroutine init_projection_LabelFile
   

   subroutine init_process_para_LabelFile(self)
      use :: keys_m
      implicit none
      class(LabelFile) :: self
      
      if (allocated(self%process_para%data)) then
         deallocate(self%process_para%data)
      end if

      allocate(self%process_para%data(LBL_PROCPARA_SIZE))

      self%process_para%data(1)%key = PARAMETER_SET_NAME
      self%process_para%data(2)%key = HORIZONTAL_TRANSFORM_METHOD
      self%process_para%data(3)%key = VERTICAL_TRANSFORM_METHOD
      self%process_para%data(4)%key = MOSAIC_PRIORITY
      self%process_para%data(5)%key = SMOOTHING_WIDTH
   
   end subroutine init_process_para_LabelFile


   subroutine init_image_LabelFile(self)
      use :: keys_m
      implicit none
      class(LabelFile) :: self

      if (allocated(self%image%data)) then
         deallocate(self%image%data)
      end if

      allocate(self%image%data(LBL_IMAGE_SIZE))

      self%image%data(1)%key = BANDS
      self%image%data(2)%key = BAND_STORAGE_TYPE
      self%image%data(3)%key = BAND_NAME
      self%image%data(4)%key = LINES
      self%image%data(5)%key = LINE_SAMPLES
      self%image%data(6)%key = SAMPLE_TYPE
      self%image%data(7)%key = SAMPLE_BITS
      self%image%data(8)%key = IMAGE_VALUE_TYPE
      self%image%data(9)%key = SAMPLE_BIT_MASK
      self%image%data(10)%key = OFFSET
      self%image%data(11)%key = SCALING_FACTOR
      self%image%data(12)%key = STRETCHED_FLAG
      self%image%data(13)%key = VALID_MINIMUM
      self%image%data(14)%key = VALID_MAXIMUM
      self%image%data(15)%key = DUMMY
      self%image%data(16)%key = MINIMUM
      self%image%data(17)%key = MAXIMUM
      self%image%data(18)%key = AVERAGE
      self%image%data(19)%key = STDEV
      self%image%data(20)%key = MODE_PIXEL
      self%image%data(21)%key = UNIT

   end subroutine init_image_LabelFile


   subroutine init_quality_info_LabelFile(self)
      use :: keys_m
      implicit none
      class(LabelFile) :: self

      if (allocated(self%quality_info%data)) then
         deallocate(self%quality_info%data)
      end if

      allocate(self%quality_info%data(LBL_QUALITYINFO_SIZE))

      self%quality_info%data(1)%key = QA_PERCENT_GOOD_PIXEL
      self%quality_info%data(2)%key = QA_PERCENT_DUMMY_PIXEL
      self%quality_info%data(3)%key = QA_PERCENT_BAD_PIXEL
      self%quality_info%data(4)%key = QA_PERCENT_INTERPOLATED_PIXEL
      self%quality_info%data(5)%key = QA_PERCENT_SHADOW_PIXEL
      self%quality_info%data(6)%key = BADPIXEL_THRESHOLD_CORRELATION
      self%quality_info%data(7)%key = BAD_PIXEL_THRESHOLD_SLOPE
   
   end subroutine init_quality_info_LabelFile


!---------------------------------------------------------------------!

   subroutine read_main_values_from_buff(self)
      use :: base_m
      implicit none
      class(LabelFile) :: self

      character(LBL_KEY_LENGTH+LBL_VALUE_LENGTH) :: line
      logical :: isContinue
      
      isContinue = .true.

      do while(isContinue)

         call head_single_line(self%buff, line)

         select case (trim(adjustl(line)))
         case ('OBJECT = IMAGE_MAP_PROJECTION')
            call read_subobject_values_from_buff(self%projection, self%buff, 'END_OBJECT = IMAGE_MAP_PROJECTION')

         case ('OBJECT = PROCESSING_PARAMETERS')
            call read_subobject_values_from_buff(self%process_para, self%buff, 'END_OBJECT = PROCESSING_PARAMETERS')

         case ('OBJECT = IMAGE')
            call read_subobject_values_from_buff(self%image, self%buff, 'END_OBJECT = IMAGE')

         case ('OBJECT = QUALITY_INFO')
            call read_subobject_values_from_buff(self%quality_info, self%buff, 'END_OBJECT = QUALITY_INFO')

         case default
            call self%main%read_line(line)
            
         end select

         isContinue = scan(self%buff, CRLF) /= 0 
         
         if (trim(line) == LBL_END_STATEMENT) isContinue = .false.
         
      end do

   end subroutine read_main_values_from_buff


   subroutine read_subobject_values_from_buff (self, buff, endstring)
      implicit none
      type(LabelObject), intent(inout) :: self
      character(*), intent(inout) :: buff
      character(*), intent(in) :: endstring

      character(LBL_KEY_LENGTH+LBL_VALUE_LENGTH) :: line
      logical :: isContinue
      
      isContinue = .true.

      do while (isContinue)

         call head_single_line(buff, line)

         if (trim(adjustl(line)) == endstring) then
            return 
         end if

         call self%read_line(line)

      end do

   end subroutine read_subobject_values_from_buff
   


   subroutine read_line_LableOject(self, line)
      implicit none
      class(LabelObject), intent(inout) :: self
      character(*), intent(inout) :: line

      character(LBL_KEY_LENGTH) :: key
      character(LBL_VALUE_LENGTH) :: val
      integer :: keylen, i


      line = adjustl(line)

      keylen = scan(line, ' ') - 1

      key = line(1:keylen)

      line = line(keylen+1:)

      if (line(1:3)== ' = ') then
         line = line(4:)
         val = line(:)
      else
         return
      end if

      do i = 1, size(self%data, dim=1)
         if (trim(self%data(i)%key) == trim(key)) then
            self%data(i)%value = trim(adjustl(val))
            return
         end if
      end do

   end subroutine read_line_LableOject



   pure function get_value_LabelObject(self, key) result(res)
      implicit none
      class(LabelObject), intent(in) :: self
      character(*), intent(in) :: key
      character(LBL_VALUE_LENGTH) :: res
      logical :: isHit
      integer :: i

      isHit = .false.
      res = LBL_NULL_STRING

      if (.not. allocated(self%data)) return

      if (trim(key) == '') return
      
      do i = 1, size(self%data, dim=1)

         isHit = trim(key) == trim(self%data(i)%key)

         if (isHit) then
            res = trim(self%data(i)%value)
            return
         end if
      end do

   end function get_value_LabelObject


   !>　LabelObjectのkeyのデータをvalueに設定する。
   subroutine set_value_LableObject(self, key, value)
      implicit none
      class(LabelObject), intent(inout) :: self
      character(LBL_KEY_LENGTH) :: key
      character(LBL_VALUE_LENGTH) :: value

      integer :: i

      if (.not. allocated(self%data)) return 
      if (trim(key) == '') return

      ! 線形探索して、引数のキーとオブジェクトのキーが一致したオブジェクトに対して、値を代入する。
      do i = 1, size(self%data, dim=1)
         if (trim(self%data(i)%key) == key) then
            self%data(i)%value = trim(value)
            return
         end if
      end do
   
   end subroutine set_value_LableObject


!------------------------------------------------------------!
!  class LabelFile procedures
!------------------------------------------------------------!
   pure function get_value_LabelFile (self, key) result(res)
      implicit none
      class(LabelFile), intent(in) :: self
      character(*), intent(in) :: key
      character(LBL_VALUE_LENGTH) :: res
      character(LBL_VALUE_LENGTH) :: res_local(5)
      integer(int32) :: i

      res = LBL_NULL_STRING
      res_local = LBL_NULL_STRING

      res_local(1) = self%main%get_value(trim(key))
      res_local(2) = self%projection%get_value(trim(key))
      res_local(3) = self%process_para%get_value(trim(key))
      res_local(4) = self%image%get_value(trim(key))
      res_local(5) = self%quality_info%get_value(trim(key))

      do i = 1, 5 
         if (trim(res_local(i)) == LBL_NULL_STRING .or. trim(res_local(i)) == '') then
            cycle
         else
            res = res_local(i)
            return
         end if
      end do

   end function get_value_LabelFile


   subroutine read_file_buffered_LabelFile(self, unit, ios)
      implicit none
      class(LabelFile), intent(inout) :: self
      integer(int32), intent(in) :: unit
      integer(int32), intent(out), optional :: ios
 
      integer :: ios_

      call load_file_buffered(unit, self%buff, ios_)

      if (present(ios)) then
         ios = ios_
      end if


   end subroutine read_file_buffered_LabelFile
    

end module Label_m