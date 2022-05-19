program main
   use iso_fortran_env
   use img2nc
   implicit none
   
   ! type(Image), allocatable :: img
   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single
   ! type(LunarNC):: nc

   ! integer(int32) :: siz_lon, siz_lat
   character(len=256) :: filename
   character(len=256), allocatable :: name_list(:,:)
   
!---------------------------------------------------------!
!リストファイル読み込み

   filename='/home/shin/WORK/sldem2013_code_list'

   call read_tile_list(filename, name_list)

!---------------------------------------------------------!
!イメージファイル読み込み

   call load_img_to_tile(name_list, array)

!---------------------------------------------------------!
!タイルの連結処理

   call merge_tiles(array, single)

!---------------------------------------------------------!
!NCファイルの作成と出力

   call nc_output('/home/shin/WORK/out.nc', single)

!---------------------------------------------------------!
!割り付け解除

   if (allocated(array)) then
      deallocate(array)
   end if

   if (allocated(name_list)) then
      deallocate(name_list)
   end if
   
end program main
