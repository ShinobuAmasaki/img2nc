program main
   use :: keys_m
   use :: base_m
   use :: label_m
   implicit none
   
   integer :: i
   type(LabelFile) :: filedata

   open (10, file='dat/lon000/DTM_MAP_01_N00E000S01E001SC.lbl', form='unformatted', action='read', access='stream')
   call filedata%init()
   call filedata%load_file(10)
   call filedata%read_buff()


   print *, trim(filedata%get_value(UPPER_LEFT_LATITUDE))
   print *, trim(filedata%get_value(COORDINATE_SYSTEM_NAME))
   print *, trim(filedata%get_value(PARAMETER_SET_NAME))
   print *, trim(filedata%get_value(BADPIXEL_THRESHOLD_CORRELATION))
   print *, trim(filedata%get_value(UNIT))



end program main