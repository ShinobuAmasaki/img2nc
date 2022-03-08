program main
   use iso_fortran_env
   use img2nc
   implicit none
   
   type(LunarNC) :: nc
   type(Image) :: dem
   character(len=256) :: tilecode

   

   tilecode = '/home/shin/WORK/dat/DTM_MAP_01_N27E341N26E342SC'

   call dem%label%set_name(tilecode)
   call dem%set_name(tilecode)
   call dem%read_lbl()
   call dem%load_image()
   print *, 'progress: load_image'

   call nc%set_name(tilecode)
   call nc%set_length(dem)
   call nc%set_step(dem)
   call nc%set_grid(dem)
   print *, 'progress: set_grid'

   call nc%define_nc()
   print *, 'progress: define_nc'
   call nc%load_data(dem)
   print *, 'progress: load_data'
   call nc%write_var()
   print *, 'progress: write_var'

   call nc%deallocate()
   call nc%close()
   print *, 'complete: nc closed.'

   stop

end program main
