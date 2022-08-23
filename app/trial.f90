program trial
   use, intrinsic :: iso_fortran_env
   use mod_preprocess
   use mod_read_img
   use mod_boundary
   use img2nc
   use netcdf
   implicit none


   character(len=256) :: filename, data_dir, range, outfile
   character(len=256), allocatable :: name_list(:,:)

   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single_this, out_tile
   type(Image) :: img
   type(boundary) :: edge

   integer(int32) :: samples

   integer(int32) :: i, j, k
   integer(int32) :: n_img, this_img
   integer(int32) :: nx_this, ny_this, n_div
   integer(int32) :: nx_each[*], ny_each[*]
   integer(int32) :: nx_all, ny_all
   integer(int32) :: siz_lon, siz_lat

   integer(int32), allocatable :: i_begin_this, i_end_this, i_begin_global(:), i_end_global(:)

   integer(int16), allocatable, target :: coarray(:,:)[:]

   
   ! arguments
   data_dir = '/mnt/data/DATA/SLDEM2013'
   outfile = '/home/shin0/WORK/img2nc/out.nc'
   range = '0/4/-1/3'

   ! call preprocess(data_dir, outfile, range, edge)
   n_img = num_images()
   this_img = this_image()

   call edge%set_west(0)
   call edge%set_east(4)
   call edge%set_south(-1)
   call edge%set_north(3)

   call create_name_list(data_dir, edge, name_list)

   siz_lon = size(name_list, 1)
   siz_lat = size(name_list, 2)

   n_div = siz_lon/n_img

   i_begin_this = n_div*(this_img - 1) + 1
   if (this_img == n_img) then
      i_end_this = siz_lon
   else
      i_end_this = i_begin_this + n_div - 1
   end if 

   allocate( array(siz_lon, siz_lat) )

! ----------------------------------------------------------------- !
   if (this_img == 1) then
      print *, 'start loading .img'
   end if
   sync all

   do i = i_begin_this, i_end_this
      do j = 1, siz_lat

         call img%label%set_name(name_list(i,j))
         call img%set_name(name_list(i,j))
         call img%read_lbl()
         call img%load_image()

         array(i,j) = img%img2tile()

         print '(a)', 'loaded: ', trim(name_list(i,j))
         call img%clear()

      end do
   end do 
   sync all 

!-- TESTED: reading img on multi processes (4CPU) 

end program trial