program four_tiles
   use iso_fortran_env
   use img2nc
   implicit none
   
   type(Image), allocatable :: img
   type(Tile) :: array(2,2), single
   type(LunarNC):: nc
   character(len=256) :: tiles(2,2)
   integer(int32) :: i, j, nlon, nlat
   integer(int32) :: start_i, end_i, start_j, end_j, ii, jj
   ! integer(int32) :: samples=4096

   tiles(1,1) = '/home/shin/WORK/dat/DTM_MAP_01_N51E350N50E351SC'
   tiles(2,1) = '/home/shin/WORK/dat/DTM_MAP_01_N51E351N50E352SC'
   tiles(1,2) = '/home/shin/WORK/dat/DTM_MAP_01_N50E350N49E351SC'
   tiles(2,2) = '/home/shin/WORK/dat/DTM_MAP_01_N50E351N49E352SC'
   
   do i = 1, 2
      do j = 1, 2
         !イメージを割り付け
         allocate(img)

         !ラベルを読み込む
         call img%label%set_name(tiles(i,j))
         call img%set_name(tiles(i,j))
         call img%read_lbl()

         !イメージを読み込む
         call img%load_image()

         !タイル配列に書き込む
         array(i,j) = img%img2tile()
      
         !イメージを解放
         deallocate(img)

         !ログ出力
         print *, 'Loaded: ', trim(tiles(i,j))
      enddo
   enddo


   !Tileの連結
   nlon = size_of_tile_array(array, 1)
   nlat = size_of_tile_array(array, 2)

   allocate(single%data(nlon, nlat))

   single%west_lon = array(1,1)%west_lon
   single%east_lon = array(2,2)%east_lon
   single%south_lat = array(2,2)%south_lat
   single%north_lat = array(1,1)%north_lat

   do i = 1, size(array,1)
      start_i = 1 + (i-1)*samples
      end_i = i*samples
      do j = 1, size(array,2)
         start_j = 1 + (j-1)*samples
         end_j = j*samples

         do ii = 1, samples
            do jj = 1, samples
               single%data(start_i+ii-1,end_j-jj+1) = array(i,j)%data(ii,jj)
            end do
         end do

      end do
   end do

   !NCファイルの作成
   call nc%set_name("/home/shin/WORK/dat/four_tiles")
   call nc%set_length(single)
   print *, 'progress: set_length'
   call nc%set_step(single)
   print *, 'progress: set_step'
   call nc%set_grid(single)
   print *, 'progress: set_grid'

   call nc%define_nc()
   print *, 'progress: define_nc'

   call nc%load_data(single)
   print *, 'progress: load_data'

   call nc%write_var()
   print *, 'progress: write_var'

   call nc%deallocate()
   call nc%close()
   print *, 'progress: nc closed.'

end program four_tiles
 

