program check_head_single_line
   use base_m
   use Label_m 
   implicit none
   
   character(LBL_BUFF_SIZE) :: buff
   character(LBL_KEY_LENGTH) :: line

   buff = 'hoge'

   open(10, file='dat/lon000/DTM_MAP_01_N00E000S01E001SC.lbl', form='unformatted', action='read', access='stream')
   call load_file_buffered(10, buff)

   call head_single_line(buff, line)
   call head_single_line(buff, line)

   print *, trim(line)

end program check_head_single_line