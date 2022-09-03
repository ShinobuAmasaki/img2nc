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
   
!--------------------------------------------------------!
   call mpi_init(ierr)
   call mpi_comm_size(mpi_comm_world, petot, ierr)
   call mpi_comm_rank(mpi_comm_world, this_rank, ierr)
   this = this_rank + 1 ! for MPI

!--------------------------------------------------------!
   data_dir = '/home/shin0/WORK/img2nc/dat'
   outnc = '/home/shin0/WORK/img2nc/out_04-13.nc'
   range = '0/4/-1/3'
   call edge%set_four(0, 4, -1, 3)

   call create_name_list(data_dir, edge, name_list)
   
   call global%preload_global_area_setting(name_list, petot)
   call local%preload_local_area_setting(global, this)
   call local%divide_array_index(global, this)

   allocate( array(global%nlon_img, global%nlat_img))
   call mpi_barrier(mpi_comm_world, ierr)   

!----
   ! loading img files
   if (this == 1) then
      print *, 'start: loading img files'
   end if

   
   do j = 1, local%nlat_img
      do i = local%i_begin, local%i_end
       !-*- DO NOT exchange i,j order for valid processing of function total_size_of_tile_array -*-
      
         call img%label%set_name(name_list(i,j))
         call img%set_name(name_list(i,j))
         call img%read_lbl()

         call img%load_image()

         array(i,j) = img%img2tile(16)

         print '(a)', 'loaded: ' // trim(name_list(i,j))
         call img%clear()

      end do 
   end do
   call mpi_barrier(mpi_comm_world, ierr)

!----
   ! merge tiles on each rank
   ! print *, this, ':', local%i_begin, local%i_end : PASS

   call merge_tiles(array(local%i_begin:local%i_end, :), single)
   call mpi_barrier(mpi_comm_world, ierr)

!----
   local%nlon = size(single%data, dim=1)
   local%nlat = size(single%data, dim=2)

   allocate(global%local_nlon(petot), source=0)
   allocate(global%local_nlat(petot), source=0)

   ! globalに各localのnlon,nlatをallgatherする。
   global%local_nlon(1) = local%nlon
   global%local_nlat(1) = local%nlat
   call mpi_allgather(global%local_nlon(1), 1, mpi_integer4, global%local_nlon(1), 1, mpi_integer4, mpi_comm_world, ierr)
   call mpi_allgather(global%local_nlat(1), 1, mpi_integer4, global%local_nlat(1), 1, mpi_integer4, mpi_comm_world, ierr)
   
   global%nlon = sum(global%local_nlon(:), dim=1)
   global%nlat = sum(global%local_nlat(:), dim=1)


   ! define local lon-direction index on each rank
   if (this == 1) then
      local%i_e_begin = 1
      local%i_e_end = local%nlon
   else
      local%i_e_begin = sum(global%local_nlon(1:this-1), dim=1) + 1
      local%i_e_end = local%i_e_begin + local%nlon - 1
   end if
   call mpi_barrier(mpi_comm_world, ierr)
   ! print *, this, ':', local%i_e_begin, local%i_e_end

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
      allocate(data_array(global%nlon, global%nlat), source=int2(0))
      print *, 'final_tile%data allocated.'
      print *, 'global:', global%nlon*global%nlat, global%nlon, global%nlat
   end if

   n_send = local%nlon*local%nlat
   print *, this, ':', n_send, local%nlon, local%nlat
   call mpi_barrier(mpi_comm_world, ierr)

   !-- MPI Comm derived type definition
   ! call mpi_type_vector()

   ! call mpi_type_commit(t_integrate, ierr)
   !--





   if (this == 1) then
      print *, 'start data gather.'
   end if
   ! ERROR test_07: invalid gather
   call mpi_gather(single%data(1,1), n_send, mpi_integer2, data_array(1,1), n_send, mpi_integer2, 0, mpi_comm_world, ierr  )



   call mpi_barrier(mpi_comm_world, ierr)
   if (this == 1) then
      print *, 'end data gather.'
   end if

   if (allocated(single%data)) then
      deallocate(single%data)
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
   