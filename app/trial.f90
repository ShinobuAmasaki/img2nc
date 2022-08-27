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
   range = '0/4/-1/3'

   ! call preprocess(data_dir, outnc, range, edge)
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
   global%n_mod = mod(global%nx_img, n_img)

   local%ny_img = global%ny_img
   
   ! local%nx_img definition
   do k = 1, n_img
      if (k == this_img) then
         
         local%nx_img = global%n_div

         if (k <= global%n_mod) then
            local%nx_img = local%nx_img + 1
         end if

      end if
   end do

   if (global%n_mod /= 0) then 
      ! 割り切れない場合
      if (this_img <= global%n_mod) then
         !余りを分配する前方のイメージについて
         local%i_begin = (global%n_div + 1) * (this_img - 1) + 1
            ! (商 + 余り分配1)*(this_img - 1) + 1
         local%i_end = local%i_begin + global%n_div

      else
         !余りを分配しない後方のイメージについて
         local%i_begin = (global%n_div + 1)*global%n_mod + global%n_div*(this_img - global%n_mod - 1) + 1
            ! (商 + 余り分配1)*(余りを分配したイメージの数) + 商*(余り分配してないイメージ1つ目からの順序番号 - 1) + 1
         local%i_end = local%i_begin + global%n_div - 1
      end if 
   
   else
      !割り切れる場合
      local%i_begin = global%n_div * (this_img - 1) + 1
      local%i_end = local%i_begin + global%n_div - 1   
   end if

   
   allocate( array(global%nx_img, global%ny_img) )

! ----------------------------------------------------------------- !
   if (this_img == 1) then
      print *, 'start loading .img'
   end if
   sync all

   do i = local%i_begin, local%i_end
      do j = 1, local%ny_img

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

      sync all
   end do
   sync all
   print *, this_img, local%i_e_begin, local%i_e_end

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
   print *, 'allocate coarray'   
   sync all


   ! gather into 1st image.
   do k = 1, n_img
      if (this_img /= k) then
         continue
      else if (this_img == 1) then
         
         coarray(local%i_e_begin:local%i_e_end, 1:local%ny) = single%data(local%i_e_begin:local%i_e_end, 1:local%ny)

      else
         ! serial gather processes order by image number
         print *, 'gather:', k, 'to', 1
         do i = local%i_e_begin, local%i_e_end

            coarray(i,:)[1] = single%data(i,:)

            ! print progress
            if ( mod(i,1024) == 0 ) then
               print *, 'image:', k, 'write column:', i
            end if

         end do
         print *, 'image:', k, ' gathered.'
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