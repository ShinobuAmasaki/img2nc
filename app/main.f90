program main
   use,intrinsic :: iso_fortran_env
   use mpi_f08
   use mod_boundary
   use mod_array_division
   use mod_preprocess
   use img2nc
   implicit none
   
   integer(int32) :: i, j, k

   ! MPI variables
   integer(int32) :: petot, this_rank, this, ierr, itag ! petot = 'processor element total'
   type(mpi_status) :: istatus

   character(len=256) :: filename, data_dir, range, outnc
   character(len=256), allocatable :: name_list(:,:)

   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single, final_tile
   type(Image) :: img
   integer(int32) :: count, local_img_total, non_priority_direction_size

   type(global_area) :: global
   type(local_area) :: local
   type(boundary) :: edge
   character(len=3) :: priority='lon' ! priority='lat' is still unacceptable

   integer(int32) :: n_send, offset, n_recv
   integer(int16), allocatable, target :: data_array(:,:)

   integer(int32) :: coarse

!-- MPI init
   call mpi_init(ierr)
   call mpi_comm_size(mpi_comm_world, petot, ierr)
   call mpi_comm_rank(mpi_comm_world, this_rank, ierr)
   this = this_rank + 1
   itag = 0

   coarse = 16 ! this is default setting for coarse grainning

!---------------------------------------------------------!

   call preprocess(data_dir, outnc, range, edge, coarse)

   call allocate_name_list(name_list, edge)
   call create_name_list(data_dir, name_list, edge)

   !-- maximum parallelization
   if (priority == 'lon') then
      non_priority_direction_size = size(name_list, dim=2) ! lat_size

      if (non_priority_direction_size < petot) then
         if (this == 1) then
            print *, 'Error: The number of processor elements is too large than latitude width.'
         end if
         call mpi_finalize()
         stop
      end if

   else if (priority == 'lat') then
      non_priority_direction_size = size(name_list, dim=1) ! lon_size

      if (non_priority_direction_size < petot) then
         if (this == 1) then
            print *, 'ERROR:  The number of processor elements is too large than longitude width.'
         end if
         call mpi_finalize()
         stop
      end if

   end if

   !-- global & local settings
   call global%init()
   call local%init()

   call global%preload_global_area_setting(edge, petot, priority=priority)
   call local%preload_local_area_setting(global, this, petot, priority=priority)
   call local%divide_array_index(global, this, petot, priority=priority)


   !-- array of tiles
   allocate( array(global%nlon_img, global%nlat_img))

!---------------------------------------------------------!
   !-- loading img files
   count = 0
   local_img_total = local%nlon_img * local%nlat_img

   do j = local%lat_begin, local%lat_end
      do i = local%lon_begin, local%lon_end

         call img%label%set_name(name_list(i,j))
         call img%set_name(name_list(i,j))
         call img%read_lbl()

         call img%load_image()

         array(i,j) = img%img2tile(coarse)
         count = count + 1
         print '(a,i3,a,i3,a,i3)', 'loaded: ' // trim(name_list(i,j))// ', on Process No.', this, ': ', count, '/', local_img_total
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

   if (this == 1) then
      ! on proc with rank num 0, copy data into data_array
      data_array(1:local%nlon, 1:local%nlat) = single%data(:,:)
      
      ! recv loop
      do k = 2, petot
         offset = global%recv_offset(k, priority=priority)
         n_recv = global%recv_num(k)

         ! recieve data from　proc with rank num from 1 to petot  
         call mpi_recv(data_array(1,offset), n_recv, mpi_integer2, k-1, itag, mpi_comm_world, istatus, ierr)
      end do

   else
      ! send data to rank 0 if var this is from 2 to petot
      call mpi_send(single%data(1,1), n_send, mpi_integer2, 0, itag, mpi_comm_world, ierr)

   end if

   ! deallocate data after gather completed.
   if (allocated(single%data)) then
      deallocate(single%data)
   end if

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
