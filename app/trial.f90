program trial
   use, intrinsic :: iso_fortran_env
   use mod_boundary
   use mod_array_division
   use mod_preprocess
   use mod_read_img
   use img2nc
   use netcdf
   implicit none

   integer(int32) :: i, j, k
   integer(int32) :: n_img, this_img

   character(len=256) :: filename, data_dir, range, outnc
   character(len=256), allocatable :: name_list(:,:)
   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single    ! image local
   type(Tile) :: final_tile  ! on 1st image for output nc
   type(Image) :: img

   ! gathering array
   integer(int16), allocatable, target :: coarray(:,:)[:]

   type(global_area) :: global
   type(local_area) :: local[*]
   type(boundary) :: edge


   ! arguments
   data_dir = '/home/shin0/WORK/img2nc/dat'
   outnc = '/home/shin0/WORK/img2nc/out.nc'
   range = '90/94/-4/0'

   ! call preprocess(data_dir, outnc, range, edge)
   n_img = num_images()
   this_img = this_image()

   call edge%set_west(90)
   call edge%set_east(94)
   call edge%set_south(-4)
   call edge%set_north(0)

   call create_name_list(data_dir, edge, name_list)

   call global%preload_global_area_setting(name_list, num_images())
   call local%preload_local_area_setting(global, this_image())
   call local%divide_array_index(global, this_image())

   allocate( array(global%nx_img, global%ny_img) )

! ----------------------------------------------------------------- !
   if (this_img == 1) then
      print *, 'start: loading .img'
   end if
   sync all

   do j = 1, local%ny_img
      do i = local%i_begin, local%i_end

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

! ----------------------------------------------------------------- !

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
            local%i_e_begin  = 1
            local%i_e_end    = local%nx
         else
            local%i_e_begin  = local[k-1]%i_e_end + 1
            local%i_e_end    = local%i_e_begin + local%nx - 1
         end if

      end if
   end do
   sync all 

   ! print *, this_img, local%i_e_begin, local%i_e_end

!----------------------------!
!-- Single-process Forking --!
   if (n_img == 1) then

      call single%read_boundary(edge)
      call nc_output(outnc, single)

      print *, 'single-process: nc outputted.'

      if (allocated(single%data)) then
         deallocate(single%data)
      end if

      stop
      
   end if

!----------------------!
!-- Multi-processing --!

   ! allocate coarray for aggregation
   allocate(coarray(global%nx, global%ny)[*], source=int2(0))
   ! print *, 'allocate coarray'   
   sync all


   ! gather into 1st image.
   do k = 1, n_img
      if (this_img /= k) then
         
         continue
      else if (this_img == 1) then
         
         coarray(local%i_e_begin:local%i_e_end, 1:local%ny) = single%data(local%i_e_begin:local%i_e_end, 1:local%ny)
      else

         ! serial gather processes order by image number
         print *, 'gather: image', k, 'into', 1
         coarray(local%i_e_begin:local%i_e_end, 1:global%ny)[1] = single%data(local%i_e_begin:local%i_e_end, 1:global%ny) 
         print *, 'gather:', k, ' image gathered'
      end if

   end do
   sync all

   if (allocated(single%data)) then
      deallocate(single%data)
   end if

!--------------------!
!-- Output NC file --!
   if (this_img == 1) then

      call final_tile%read_boundary(edge)
      
      final_tile%p_data => coarray

      call nc_output(outnc, final_tile)

      !finalize
      print *, 'multi-process: nc outputted.'

   end if

!------------------!
!-- deallocation --!
   if (allocated(array)) then
      deallocate(array)
   end if

   if (allocated(coarray)) then
      deallocate(coarray)
   end if

   if (allocated(single%data)) then
      deallocate(single%data)
   end if

   if (allocated(name_list)) then
      deallocate(name_list)
   end if

end program trial