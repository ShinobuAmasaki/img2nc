program check_lbl_get_value   
   use base_m
   use label_m, only: LabelFile
   implicit none

   type(LabelFile) :: filedata

   character(4096) :: buff

   buff = 'hoge'

   block !stab
      allocate(filedata%main%data(2), filedata%process_para%data(2))
      filedata%main%data(1)%key = 'key1'
      filedata%main%data(1)%value = 'value1'
      filedata%main%data(2)%key = 'key2'
      filedata%main%data(2)%value = 'value2'
   end block !stab

   print *, trim(filedata%get_value('key1'))

   open (10, file='dat/lon000/DTM_MAP_01_N00E000S01E001SC.lbl', form='unformatted', action='read', access='stream')
   call load_file_buffered(10, buff)

   print '(a)', trim(adjustl(buff))
   close (10)


end program check_lbl_get_value
