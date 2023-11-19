program main
   use :: keys_m
   use :: base_m
   use :: label_m
   implicit none
   
   integer :: i
   type(LabelFile) :: filedata

   open (10, file='mola-megdr/meg128/megr00n090hb.lbl', form='unformatted', action='read', access='stream')
   ! open (10, file='sldem2013/lon000/DTM_MAP_01_N00E000S01E001SC.lbl', form='unformatted', action='read', access='stream')
   call filedata%init()
   call filedata%load_file(10)
   call filedata%read_buff()

   print *, '=========='
   print *, trim(filedata%get_value(PDS_VERSION_ID))
   print *, trim(filedata%get_value(COORDINATE_SYSTEM_NAME))
   print *, trim(filedata%get_value(OFFSET))
   print *, trim(filedata%get_value(LINES))
   print *, trim(filedata%get_value(LINE_SAMPLES))
   print *, trim(filedata%get_value(DESCRIPTION))
   print *, trim(filedata%image%get_value(DESCRIPTION))



end program main