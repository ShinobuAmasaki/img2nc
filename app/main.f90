program main
   use,intrinsic :: iso_fortran_env
   use mod_preprocess
   use img2nc
   implicit none
   
   type(Tile), allocatable :: array(:,:)
   type(Tile) :: single

   character(len=256) :: data_dir, outfile, range
   integer(int32) :: west, east, south, north
   character(len=256), allocatable :: name_list(:,:)

!---------------------------------------------------------!
!前処理
   
   call preprocess(data_dir, outfile, range, west, east, south, north)

!---------------------------------------------------------!
!リストファイル

   call create_name_list(data_dir, west, east, south, north, name_list)

!---------------------------------------------------------!
!イメージファイル読み込み

   call load_img_to_tile(name_list, array, 1)

!---------------------------------------------------------!
!タイル粗視化処理
   ! call array_16_shrink(array)

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
