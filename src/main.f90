program main
   use iso_fortran_env
   use img2nc
   implicit none
   
   type(Image), allocatable :: img
   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single
   type(LunarNC):: nc

   integer(int32) :: siz_lon, siz_lat, unit
   character(len=256) :: filename
   character(len=256), allocatable :: tiles(:,:)

   integer(int32) :: i, j, nlon, nlat
   integer(int32) :: start_i, end_i, start_j, end_j, ii, jj

   
!---------------------------------------------------------!
!リストファイル読み込み
   ! リストファイルを開く。
   filename='/home/shin/WORK/sldem2013_code_list'
   open(file=filename, status='old', newunit=unit)
   read(unit, *) siz_lon, siz_lat

   !エラー処理
   call tile_size_check(siz_lon, siz_lat, unit)

   !タイル配列の割り付ける。
   allocate( tiles(siz_lon, siz_lat) )

   !リストの読み込み(列優先)
   do i = 1, siz_lon
      do j = 1, siz_lat
         read(unit, '(a)') tiles(i,j)
         ! print *, trim(tiles(i,j))
      end do
   end do
   
   !ファイルを閉じる。
   close(unit)
!---------------------------------------------------------!
!イメージファイル読み込み
   !読み込み先配列を割り付ける。
   allocate(array(siz_lon,siz_lat))

   !イメージ読み込み
   do i = 1, siz_lon
      do j = 1, siz_lat

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

      end do
   end do

!---------------------------------------------------------!
!タイルの連結処理

   nlon = size_of_tile_array(array, 1)
   nlat = size_of_tile_array(array, 2)
   print *, 'Define: nlon, nlat'

   !シートを割り付ける。
   allocate(single%data(nlon, nlat))

   !東西南北端の緯度経度をコピーする。
   single%west_lon  = array(1,1)%west_lon
   single%north_lat = array(1,1)%north_lat
   single%east_lon  = array(siz_lon,siz_lat)%east_lon
   single%south_lat = array(siz_lon,siz_lat)%south_lat
 
   !タイル配列をシートにコピーして連結する。
   print *, 'Progress: Start merging.'
   do i = 1, siz_lon
      start_i = 1 + (i-1)*samples
      end_i = i*samples
      do j = 1, siz_lat
         start_j = 1 + (j-1)*samples
         end_j = j*samples

         do ii = 1, samples
            do jj = 1, samples
               single%data(start_i+ii-1,end_j-jj+1) = array(i,j)%data(ii,jj)
            end do
         end do

      end do
   end do
   print *, 'Progress: End merging.'

!---------------------------------------------------------!
!NCファイルの作成と出力
   call nc%set_name('/home/shin/WORK/out')
   print *, 'Progress: set_name'

   call nc%set_length(single)
   print *, 'Progress: set_length'

   call nc%set_step(single)
   print *, 'Progress: set_step'

   call nc%set_grid(single)
   print *, 'Progress: set_grid'

   call nc%define_nc()
   print *, 'Progress: define_nc'

   call nc%load_data(single)
   print *, 'Progress: load_data'

   call nc%write_var()
   print *, 'Progress: write_var'

   call nc%deallocate()
   call nc%close()
   print *, 'Progress: nc closed.'

   if (allocated(array)) then
      deallocate(array)
   end if

   if (allocated(img)) then
      deallocate(img)
   end if

   if (allocated(tiles)) then
      deallocate(tiles)
   end if

   
contains

   !リストファイルのヘッダーの数値をチェックする。
   subroutine tile_size_check(nx, ny, unit)
      integer(int32), intent(in) :: nx, ny, unit

      if (nx <= 0 .or. ny <= 0) then
         write(0, *) 'ERROR: Invalid tile array size.'
         close(unit)
         stop
      end if
   
   end subroutine tile_size_check

end program main
