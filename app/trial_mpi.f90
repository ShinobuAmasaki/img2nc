program trial_mpi
   use, intrinsic :: iso_fortran_env
   use mpi_f08
   use netcdf

   use mod_boundary
   use mod_array_division
   use mod_preprocess
   use mod_read_img
   use img2nc
   implicit none


   integer(int32) :: petot, this_rank, this, ierr ! petot = 'processor element total'
   integer(int32) :: i, j, k
   integer(int32) :: n_send
   integer(int16), allocatable, target :: data_array(:,:)

   character(len=256) :: filename, data_dir, range, outnc
   character(len=256), allocatable :: name_list(:,:)
   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single, load_img
   type(Tile) :: final_tile   ! in order to out nc
   type(Image) :: img

   type(global_area) :: global
   type(local_area) :: local
   type(boundary) :: edge

   type(mpi_datatype) :: t_integrate

   character(len=3) :: priority
   
!--------------------------------------------------------!
   call mpi_init(ierr)
   call mpi_comm_size(mpi_comm_world, petot, ierr)
   call mpi_comm_rank(mpi_comm_world, this_rank, ierr)
   this = this_rank + 1 ! for MPI

!--------------------------------------------------------!
   data_dir = '/home/shin0/WORK/img2nc/dat' 
   outnc = '/home/shin0/WORK/img2nc/out_04-13.nc'
   range = '0/4/-1/3'
   priority = 'lon'

   call edge%set_four(0, 4, -1, 3)

   call create_name_list(data_dir, edge, name_list)
   
   call global%preload_global_area_setting(name_list, petot)
   call local%preload_local_area_setting(global, this)
   call local%divide_array_index(global, this, priority=priority)

   allocate( array(global%nlon_img, global%nlat_img))
   call mpi_barrier(mpi_comm_world, ierr)   

!----
   ! loading img files
   if (this == 1) then
      print *, 'start: loading img files'
   end if

   do j = local%lat_begin, local%lat_end
      do i = local%lon_begin, local%lon_end

         call img%label%set_name(name_list(i,j))
         call img%set_name(name_list(i,j))
         call img%read_lbl()

         call img%load_image()

         array(i,j) = img%img2tile(16)

         print '(i2, a)', this, 'loaded: ' // trim(name_list(i,j))
         call img%clear()

      end do 
   end do
   call mpi_barrier(mpi_comm_world, ierr)

!----
   ! merge tiles on each rank
   call merge_tiles(array(local%lon_begin:local%lon_end, local%lat_begin:local%lat_end), single)
   call mpi_barrier(mpi_comm_world, ierr)

!----
   call local%set_nlon_nlat(single)

   allocate(global%local_nlon(petot), source=0)
   allocate(global%local_nlat(petot), source=0)

   ! globalに各localのnlon,nlatをallgatherする。
   call global%gather_local_nlon_nlat(local, mpi_comm_world, ierr)
   call global%set_nlon_nlat(priority=priority)

   call mpi_barrier(mpi_comm_world, ierr)

!---------------------!
!  Single processing  !
   if (petot == 1) then
      call single%read_boundary(edge)
      call nc_output(outnc, single)

      print *, 'single-process: nc outputted.'

      call all_deallocate() 
      call mpi_finalize(ierr)
      stop
   end if 

!-----------------------!
!  Multiple processing  !
   if (this == 1) then
      ! recieving data array on master
      allocate(data_array(global%nlon, global%nlat), source=int2(0))
   end if
   n_send = local%nlon*local%nlat
   call mpi_barrier(mpi_comm_world, ierr)

   if (this == 1) then
      print *, 'gather: start gather.'
   end if

   ! -*- PASS: test_22, n=4 -*-
   call mpi_gather(single%data(1,1), n_send, mpi_integer2, data_array(1,1), n_send, mpi_integer2, 0, mpi_comm_world, ierr)
   ! call mpi_gather(single%data(1,1), 1, t_integrate, data_array(1,1), 1, t_integrate, 0, mpi_comm_world, ierr)
 
   if (this == 1) then
      print *, 'gather: end gather.'
   end if


   if (this == 1) then
      print *, 'multiple-process: nc making.'
      final_tile%p_data => data_array
      call final_tile%read_boundary(edge)
      call nc_output(outnc, final_tile)

      print *, 'multiple-process: nc outputted.'
   end if
!--------------------------------------------------------!
   call all_deallocate()
   call mpi_finalize(ierr)
   stop

contains

   subroutine all_deallocate()
      if (allocated(single%data)) then
         deallocate(single%data)
      end if

      if (allocated(global%local_nlon)) then
         deallocate(global%local_nlon)
      end if

      if (allocated(global%local_nlat)) then
         deallocate(global%local_nlat)
      end if

      if (allocated(name_list)) then
         deallocate(name_list)
      end if

      if (allocated(final_tile%data)) then
         deallocate(final_tile%data)
      end if
   end subroutine all_deallocate
      
   subroutine on_test_finalize()

      call all_deallocate()
      call mpi_finalize(ierr)
      stop
   end subroutine on_test_finalize



end program trial_mpi
   