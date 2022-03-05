module string_operation
   use iso_fortran_env
   implicit none

contains

   function change_extension(filename, extension)
      character(len=*) :: filename, extension
      character(len=1024) :: change_extension
      integer(int32) :: index, rslash, lbound, i

      ! 拡張子変更の要件
      ! - 文字列を'/'で分割し、最も右の部分文字列を対象とする。
      ! - 対象部分の、先頭から連続する'.'は検索しない。
      ! - 検索部分に'.'を含まない場合、指示された拡張子を語尾に付加する。

      ! 変数rslashに、変数filenameの最も右の斜線の位置を検索して、代入する
      rslash = scan(filename, '/', back=.true.)

      ! 検索範囲左端を設定する。
      ! 対象部分の左端のインデックスを代入する。
      lbound = rslash + 1

      ! ループ変数にlboundを代入する。
      i = lbound
      ! ループ変数が検索範囲左端のインデックスと等しい場合、処理をループする。
      do while (i == lbound)

         ! 左端の右隣の文字が.の場合、処理を分岐して、左端を右隣に移す。
         if (filename(lbound+1:lbound+1) == '.') then
            lbound = lbound + 1
         end if

         ! ループ変数をインクリメントする。
         i = i + 1
      end do

      ! 変数filenameの検索範囲の内、最も右のドットの位置を変数indexに代入する。
      index = scan(filename(lbound+1:), '.', back=.true.) + lbound

      ! 検索されたindexが左端と等しい場合、拡張子は無い。
      if (index == lbound) then
         ! filenameに拡張子をつけて戻り値に代入する。
         change_extension = trim(filename) // '.' // extension
      
      ! 拡張子がある場合の処理。
      else
         ! 戻り値に、ファイルの語幹部分をトリムして、extensionで指定された拡張子を末尾につけて代入する。
         change_extension = trim(filename(1:index)) // extension
      end if

   end function change_extension


end module string_operation


