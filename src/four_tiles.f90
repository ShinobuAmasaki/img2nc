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
   integer(int32) :: samples=4096

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

   allocate(single%lon(nlon), single%lat(nlat), single%data(nlon, nlat))

   do i = 1, size(array,1)
      start_i = 1 + (i-1)*samples
      end_i = i*samples
      do j = 1, size(array,2)
         start_j = 1 + (j-1)*samples
         end_j = j*samples

         ! 経度の入力（第1行のみ）
         if (j == 1) then
            single%lon(start_i:end_i) = array(i,j)%lon(1:samples)
            ! print *, start_j, single%lon(start_j), single%lon(end_j)
         end if

         ! 緯度の入力（第1列のみ）
         if (i == 1) then
            do jj = 1, samples
               single%lat(end_j-jj+1) = array(i,j)%lat(jj)
            end do
         end if
         

         do ii = 1, samples
            do jj = 1, samples
               single%data(start_i+ii-1,end_j-jj+1) = array(i,j)%data(ii,jj)
            end do
         end do

      end do
   end do

   print *, single%lon(1),single%lon(samples),single%lon(samples+1),single%lon(samples*2)
   print *, single%lat(1),single%lat(samples),single%lat(samples+1),single%lat(samples*2)

   !NCファイルの作成
   call nc%set_name("/home/shin/WORK/dat/four_tiles")
   call nc%set_length(single)
   print *, 'progress: set_length'
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
 

