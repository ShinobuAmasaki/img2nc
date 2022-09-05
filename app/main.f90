program main
   use,intrinsic :: iso_fortran_env
   use mpi_f08
   use mod_boundary
   use mod_array_division
   use mod_preprocess
   use img2nc
   implicit none
   
   integer(int32) :: i, j

   ! MPI variables
   integer(int32) :: petot, this_rank, this, ierr ! petot = 'processor element total'
   type(mpi_datatype) :: t_integrate

   character(len=256) :: filename, data_dir, range, outnc
   character(len=256), allocatable :: name_list(:,:)

   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single, final_tile
   type(Image) :: img
   integer(int32) :: count, total, lon_size

   type(global_area) :: global
   type(local_area) :: local
   type(boundary) :: edge
   character(len=3) :: priority='lon'

   integer(int32) :: n_send
   integer(int16), allocatable, target :: data_array(:,:)

!-- MPI init
   call mpi_init(ierr)
   call mpi_comm_size(mpi_comm_world, petot, ierr)
   call mpi_comm_rank(mpi_comm_world, this_rank, ierr)
   this = this_rank + 1

!---------------------------------------------------------!

   call preprocess(data_dir, outnc, range, edge)

   call create_name_list(data_dir, edge, name_list)

   !-- maximum parallelization
   lon_size = size(name_list, dim=1)
   if (lon_size < petot) then
      
      if (this == 1) then
         print *, 'Error: The number of processor elements is too large than longitude width.'
      end if
      call mpi_finalize()
      stop
   end if

   !-- global & local settings
   call global%init()
   call local%init()

   call global%preload_global_area_setting(name_list, petot)
   call local%preload_local_area_setting(global, this)
   call local%divide_array_index(global, this, priority=priority)


   !-- array of tiles
   allocate( array(global%nlon_img, global%nlat_img))

!---------------------------------------------------------!
   !-- loading img files
   count = 0
   total = local%nlon_img * local%nlat_img

   do j = local%lat_begin, local%lat_end
      do i = local%lon_begin, local%lon_end

         call img%label%set_name(name_list(i,j))
         call img%set_name(name_list(i,j))
         call img%read_lbl()

         call img%load_image()

         array(i,j) = img%img2tile(16)
         count = count + 1
         print '(a,i3,a,i3,a,i3)', 'loaded: ' // trim(name_list(i,j))// ', on Process No.', this, ': ', count, '/', total
         call img%clear()

      end do 
   end do

   !-- merge tiles on each rank
   call merge_tiles(array(local%lon_begin:local%lon_end, local%lat_begin:local%lat_end), single)

!---------------------------------------------------------!
!---                  Single processing                ---!
!---------------------------------------------------------!
   if (petot == 1) then
      call single%read_boundary(edge)
      call nc_output(outnc, single)

      print *, 'single-process: nc outputted.'

      call all_deallocate() 
      call mpi_finalize(ierr)
      stop
   end if 

!---------------------------------------------------------!
!---                 Multiple processing               ---!
!---------------------------------------------------------!
   !-- global & local settings before gather
   call local%set_nlon_nlat(single)

   allocate(global%local_nlon(petot), source=0)
   allocate(global%local_nlat(petot), source=0)

   ! globalに各localのnlon,nlatをallgatherする。
   call global%gather_local_nlon_nlat(local, mpi_comm_world, ierr)
   call global%set_nlon_nlat(priority=priority)


   !-- gather into the rank=0 process
  
   ! allocate recieving array
   if (this == 1) then
      allocate(data_array(global%nlon, global%nlat), source=int2(0))
   end if

   ! size of sending data
   n_send = local%nlon*local%nlat

   call mpi_gather(single%data(1,1), n_send, mpi_integer2, data_array(1,1), n_send, mpi_integer2, 0, mpi_comm_world, ierr)

!---------------------------------------------------------!
   ! Make NetCDF file
   if (this == 1) then
      final_tile%p_data => data_array
      call final_tile%read_boundary(edge)
      call nc_output(outnc, final_tile)
   end if

!---------------------------------------------------------!
   ! Finalize
   call all_deallocate()
   call mpi_finalize(ierr)
   
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

      if (allocated(data_array)) then
         deallocate(data_array)
      end if
   end subroutine all_deallocate

end program main
