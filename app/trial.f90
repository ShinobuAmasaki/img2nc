program trial
   use, intrinsic :: iso_fortran_env
   use mod_preprocess
   use mod_read_img
   use mod_boundary
   use img2nc
   use netcdf
   implicit none

   type global_area
      integer(int32) :: west, east, south, north
      integer(int32) :: n_div
      integer(int32) :: nx_img, ny_img ! number of imgs.
      integer(int32) :: nx, ny ! number of total elements. use these to allocate coarray(:,:)[:]
   end type global_area

   type local_area
      integer(int32) :: nx, ny
      integer(int32) :: i_begin, i_end
   end type local_area

   integer(int32) :: i, j, k
   integer(int32) :: n_img, this_img

   character(len=256) :: filename, data_dir, range, outfile
   
   character(len=256), allocatable :: name_list(:,:)
   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single, out_tile
   type(Image) :: img

   ! gathering array
   integer(int16), allocatable, target :: coarray(:,:)[:]

   type(local_area) :: local[*]  !-> 集計に使う
   type(global_area) :: global
   type(boundary) :: edge

   ! integer(int32) :: nx_each[*], ny_each[*]
   
   ! arguments
   data_dir = '/home/shin0/WORK/img2nc/dat'
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

   global%nx_img = size(name_list, 1)
   global%ny_img = size(name_list, 2)

   global%n_div = global%nx_img / n_img

   local%i_begin = global%n_div * (this_img - 1) + 1
   if (this_img == n_img) then
      local%i_end = global%nx_img
   else
      local%i_end = local%i_begin + global%n_div - 1
   end if 

   allocate( array(global%nx_img, global%ny_img) )

! ----------------------------------------------------------------- !
   if (this_img == 1) then
      print *, 'start loading .img'
   end if
   sync all

   do i = local%i_begin, local%i_end
      do j = 1, global%ny_img

         call img%label%set_name(name_list(i,j))
         call img%set_name(name_list(i,j))
         call img%read_lbl()
         call img%load_image()

         array(i,j) = img%img2tile()

         print '(a)', 'loaded: '// trim(name_list(i,j))
         call img%clear()

      end do
   end do 
   sync all 

!-- TESTED: reading img on multi processes (4CPU) 

   ! single: one tile on each images
   call merge_tiles(array(local%i_begin:local%i_end, :), single)
   sync all

   local%nx = size(single%data, dim=1)
   local%ny = size(single%data, dim=2)

   global%ny = local%ny
   global%nx = 0
   do k = 1, n_img
      global%nx = global%nx + local[k]%nx
   end do
   sync all 

   ! define local x index on each image
   do k = 1, n_img
      if (k == this_img) then

         if (k == 1) then
            local%i_begin  = 1
            local%i_end    = local%nx
         else
            local%i_begin  = local[k-1]%i_end + 1
            local%i_end    = local%i_begin + local%nx - 1
         end if

      end if

      sync all
   end do
   sync all
   print *, this_img, local%i_begin, local%i_end

!----------------------------!
!-- Single-process Forking --!
   if (n_img == 1) then

      call set_lonlat_to_tile_from_edge(single, edge)
      call nc_output(outfile, single)

      print *, 'single-process: nc outputted.'
      stop
      
   end if

!----------------------!
!-- Multi-processing --!

   ! allocate coarray for aggregation
   allocate(coarray(global%nx, global%ny)[*], source=int2(0))
   print *, 'allocate coarray'   
   sync all


   ! gather into image 1.
   do k = 2, n_img
      if (this_img /= k) then
         continue
      else
         ! image numberの順にシリアル実行
         
         print *, 'gather:', k, 'to', 1
         do i = local%i_begin, local%i_end

            coarray(i,:)[1] = single%data(i,:)

            ! print progress
            if ( mod(i,1024) == 0 ) then
               print *, 'image:', k, 'write column:', i
            end if

         end do
         print *, 'image:', k, ' gathered.'
      end if
      sync all

   end do
   sync all





end program trial