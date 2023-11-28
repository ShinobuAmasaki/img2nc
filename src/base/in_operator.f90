! This module provides operator(.in.).
! Inplemented only for the combination:
!     char .in. char
!
! Copyright 2023, Amasaki Shinobu

module in_operator_m
   use, intrinsic :: iso_fortran_env, only: real64, int64
   implicit none
   private
   
   public :: operator(.in.)

   interface operator(.in.)
      procedure :: caller_char
   end interface

contains

   ! Wrapper function for a couple of chars
   pure function caller_char (keyinput, target_input) result(res)
      implicit none
      character(*), intent(in) :: keyinput
      character(*), intent(in) :: target_input
      character(:), allocatable :: key_tmp, target_tmp
      logical :: res

      ! Preprocess strings
      ! コマンドラインオプションの解析に便利のため、空白を除去しない。
      key_tmp = keyinput
      target_tmp = target_input

      res = in_operator__char_in_char(key_tmp, target_tmp)
      return

   end function caller_char


   ! Implementation for a couple of chars 
   pure logical function in_operator__char_in_char(key, target_) result(res)
      implicit none
      character(*), intent(in) :: key
      character(*), intent(in) :: target_
      
      character(len=1) :: head
      integer(int64) :: i, keyLen
      logical, allocatable :: index_table(:)

      keyLen = len(key)

      !キーの長さが、ターゲット文字列よりも長い場合、偽を返す。
      if (len(target_) < keyLen) then
         res = .false.
         return
      end if

      ! Prepare index table
      allocate(index_table(len(target_)))
      index_table(:) = .false.

      head = key(1:1)

      ! 先頭1文字の線形探索で実行し、論理型配列index_tableに結果を格納する。
      do i = 1, len(target_)-keyLen+1
         if (head(1:1) == target_(i:i)) then
            index_table(i) = .true.
         end if 
      end do

      ! index_tableを走査する
      i = 1
      do while (i <= len(target_) - keyLen+1)

         ! テーブルのフラグが真の場合、
         if (index_table(i)) then

            block
               character(len=keyLen) :: sub
               ! ターゲットから部分文字列を取り出す。
               sub = target_(i:i+keyLen-1)
               ! 部分文字列がキーと一致する場合、真を返す。
               if (sub(:) == key(:)) then
                  res = .true.
                  return
               end if
            end block

         end if
         
         i = i + 1
      end do

      res = .false.

   end function in_operator__char_in_char

end module in_operator_m

