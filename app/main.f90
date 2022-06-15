program main
   use iso_fortran_env
   use img2nc
   implicit none
   
   ! type(Image), allocatable :: img
   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single
   ! type(LunarNC):: nc

   ! integer(int32) :: siz_lon, siz_lat
   character(len=256) :: filename, outfile
   character(len=256), allocatable :: name_list(:,:)

   filename='/home/shin/WORK/img2nc/sldem2013_code_list.test'
   outfile='/home/shin/WORK/out_16.nc'

!---------------------------------------------------------!
!リストファイル読み込み

   call read_tile_list(filename, name_list)

!---------------------------------------------------------!
!イメージファイル読み込み

   call load_img_to_tile(name_list, array)

!---------------------------------------------------------!
!タイル粗視化処理
   ! call tile_halve_shrink(array(1,1))
   ! call tile_quarter_shrink(array(1,1))
   ! call tile_eighth_shrink(array(1,1))
   ! call tile_sixteenth_shrink(array(1,1))
   
!---------------------------------------------------------!
!タイルの連結処理

   call merge_tiles(array, single)

!---------------------------------------------------------!
!NCファイルの作成と出力

   call nc_output(outfile, single)

!---------------------------------------------------------!
!割り付け解除

   if (allocated(array)) then
      deallocate(array)
   end if

   if (allocated(name_list)) then
      deallocate(name_list)
   end if
   
end program main
