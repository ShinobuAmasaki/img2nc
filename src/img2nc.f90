module img2nc
   use iso_fortran_env
   use netcdf
   use read_img
   use string_operation

   type LunarNC
      private
      ! Internal variable
      real(real64), allocatable :: lon(:), lat(:)
      real(real64), public, allocatable :: data(:,:)
      real(real64) :: step_lon, step_lat
      integer(int32) :: nx, ny, nz

      ! .nc file interface variable
      integer(int32) :: ncid
      integer(int32) :: lon_dim_id, lat_dim_id, elev_dim_id
      integer(int32) :: lon_id, lat_id, elev_id
      integer(int32) :: start_nc(2), count_nc(2)

      ! for Output .nc
      character(len=256) :: outfile
   contains

      procedure :: set_name => lnc_set_name_from_string
      procedure :: set_length => lnc_set_length
      procedure :: set_step => lnc_set_step
      procedure :: set_grid => lnc_set_grid
      procedure :: define_nc => lnc_define_nc
      procedure :: load_data => lnc_load_data_nc
      procedure :: write_var => lnc_write_var_nc
      procedure :: deallocate => lnc_deallocate_nc
      procedure :: close => lnc_close_nc

   end type LunarNC

contains
      
   subroutine lnc_set_name_from_string(self, name)
      class(LunarNC) :: self
      character(*), intent(in) :: name

      self%outfile = trim(name)

      return
   end subroutine lnc_set_name_from_string
   

   subroutine lnc_set_length(self, single)
      class(LunarNC) :: self
      type(Tile), intent(in) :: single

      self%nx = size(single%data, dim=1)
      self%ny = size(single%data, dim=2)
      self%nz = 1

   end subroutine lnc_set_length


   subroutine lnc_set_step(self, single)
      class(LunarNC) :: self
      type(Tile), intent(in) :: single
      integer(int32) :: east, west, north, south

      east = single%east_lon
      west = single%west_lon
      south = single%south_lat
      north = single%north_lat

      self%step_lon = (dble(east) - dble(west))/dble(self%nx)
      self%step_lat = (dble(north) - dble(south))/dble(self%ny)
   end subroutine lnc_set_step

   
   subroutine lnc_set_grid(self,single)
      class(LunarNC) :: self
      type(Tile) :: single
      integer(int32) :: i

      allocate(self%lon(self%nx), self%lat(self%ny))

      do i = 1, self%nx
         ! self%lon(i) = single%lon(i)
         self%lon(i) = self%step_lon*(i-1) + dble(single%west_lon)
      end do

      do i = 1, self%ny
         ! self%lat(i) = single%lat(i)
         self%lat(i) = -self%step_lat*(i-1) + dble(single%north_lat)
      end do

   end subroutine lnc_set_grid


   subroutine lnc_define_nc(self)
      class(LunarNC) :: self

      ! 書き込み先ファイルを定義する。
      call check( nf90_create(trim(self%outfile), NF90_HDF5, self%ncid) )
      ! 次元を定義する。
      call check( nf90_def_dim(self%ncid, 'longitude', self%nx, self%lon_dim_id) )
      call check( nf90_def_dim(self%ncid, 'latitude', self%nx, self%lat_dim_id) )
      ! 変数を定義する。
      call check( nf90_def_var(self%ncid, 'longitude', NF90_DOUBLE, self%lon_dim_id, self%lon_id) )
      call check( nf90_def_var(self%ncid, 'latitude', NF90_DOUBLE, self%lat_dim_id, self%lat_id) )
      call check( nf90_def_var(self%ncid, 'elevetion', NF90_DOUBLE, (/self%lon_dim_id, self%lat_dim_id/), &
         self%elev_id, deflate_level=1 ) )

      ! 変数にattributionを付け、単位を記述する。
      call check( nf90_put_att(self%ncid, self%lon_id, 'units', 'deg.') )
      call check( nf90_put_att(self%ncid, self%lat_id, 'units', 'deg.') )
      call check( nf90_put_att(self%ncid, self%elev_id, 'units', 'meters') )
      ! defineモードを終了する。
      call check( nf90_enddef(self%ncid) )
      return

   end subroutine lnc_define_nc
   

   subroutine lnc_load_data_nc(self, single)
      class(LunarNC) :: self
      type(Tile) :: single

      allocate(self%data(self%nx, self%ny))

      do i = 1, self%nx
         do j = 1, self%ny
            self%data(i,j) = dble(single%data(i,j))
         end do
      end do

   end subroutine lnc_load_data_nc


   subroutine lnc_write_var_nc(self)
      class(LunarNC) :: self

      call check( nf90_put_var(self%ncid, self%lon_id, self%lon(1:self%nx) ) )
      print *, 'Progress: put_var lon'
      call check( nf90_put_var(self%ncid, self%lat_id, self%lat(1:self%ny) ) )
      print *, 'Progress: put_var lat'

      self%start_nc = [1, 1]
      self%count_nc = [self%nx, self%ny]
      call check( nf90_put_var(self%ncid, self%elev_id, self%data, start=self%start_nc, count=self%count_nc) )
      print *, 'Progress: put_var elev'
      return
   end subroutine lnc_write_var_nc


   subroutine lnc_deallocate_nc(self)
      class(LunarNC) :: self

      deallocate(self%lon, self%lat, self%data)
      return
   end subroutine lnc_deallocate_nc


   subroutine lnc_close_nc(self)
      class(LunarNC) :: self

      call check( nf90_close(self%ncid) )

      return
   end subroutine lnc_close_nc

!-------------------------------------------------------------------!

   ! netCDFの関数の引数を受け取るsubroutineを用意する
   subroutine check(status)
      integer, intent(in) :: status

      ! 変数statusが変数nf90_noerrではない場合、処理を分岐する。
      if(status .ne. nf90_noerr) then

         ! ステータスコードから対応するエラーメッセージを取得し、コンソールに表示する。
         print *, trim(nf90_strerror(status))
         stop "Stopped"
      end if
   end subroutine check

   
   subroutine read_tile_list(filename, name_list)
      character(len=256), intent(in) :: filename
      character(len=256), intent(out), allocatable :: name_list(:,:)
      integer(int32) :: siz_lon, siz_lat
      integer(int32) :: unit

      !リストファイルを開く。
      open(file=filename, status='old', newunit=unit)
      !ヘッダーの読み取り
      read(unit, *) siz_lon, siz_lat

      !エラー処理
      call tile_size_check(siz_lon, siz_lat, unit)

      !ファイル名の読み込み先を割り付ける。
      allocate( name_list(siz_lon, siz_lat) )

      !リストの読み込み(列優先)
      do i = 1, siz_lon
         do j = 1, siz_lat
            read(unit, '(a)') name_list(i,j)
         end do
      end do
      
      !ファイルを閉じる。
      close(unit)
   
   end subroutine read_tile_list

   !リストファイルのヘッダーの数値をチェックする。
   subroutine tile_size_check(nx, ny, unit)
      integer(int32), intent(in) :: nx, ny, unit

      if (nx <= 0 .or. ny <= 0) then
         write(0, *) 'ERROR: Invalid tile array size.'
         close(unit)
         stop
      end if
   
   end subroutine tile_size_check


   subroutine load_img_to_tile(name_list, array)
      character(len=*), intent(in) :: name_list(:,:)
      type(Tile), intent(inout), allocatable :: array(:,:)
      type(Image), allocatable :: img
      integer(int32) :: siz_lon, siz_lat

      !配列の縦横サイズを取得する。
      siz_lon = size(name_list, 1)
      siz_lat = size(name_list, 2)

      !読み込み先配列を割り付ける。
      allocate( array(siz_lon, siz_lat) )

      !イメージの読み込み
      do i = 1, siz_lon
         do j = 1, siz_lat

            !イメージの割り付け
            allocate(img)

            !ラベルを読み込む
            call img%label%set_name(name_list(i,j))
            call img%set_name(name_list(i,j))
            call img%read_lbl()
   
            !イメージを読み込む
            call img%load_image()
   
            !タイル配列に書き込む
            array(i,j) = img%img2tile()

            !イメージを解放
            deallocate(img)
   
            !ログ出力
            print *, 'Loaded: ', trim(name_list(i,j))
         end do
      end do
   end subroutine load_img_to_tile


   subroutine merge_tiles(array, result)
      type(Tile), intent(in) :: array(:,:)   !タイル配列
      type(Tile), intent(out) :: result      !集約タイル
      integer(int32) :: nlon, nlat, siz_lon, siz_lat
      integer(int32) :: i, j, start_i, end_i, start_j, end_j, ii, jj

      !集約タイルの大きさを取得する。
      nlon = size_of_tile_array(array, 1)
      nlat = size_of_tile_array(array, 2)

      !タイル配列のサイズを取得する。
      siz_lon = size(array, 1)
      siz_lat = size(array, 2)

      !集約タイルを割り付ける。
      allocate(result%data(nlon, nlat))

      !東西南北端の緯度経度をコピーする。
      result%west_lon  = array(1,1)%west_lon
      result%north_lat = array(1,1)%north_lat
      result%east_lon  = array(siz_lon,siz_lat)%east_lon
      result%south_lat = array(siz_lon,siz_lat)%south_lat

      !タイル配列の値を集約タイルにコピーして連結する。
      print *, 'Progress: Start merging.'
      do i = 1, siz_lon
         !ローカルの始点・終点の列を定義する。
         start_i = 1 + (i-1)*samples
         end_i = i*samples
         
         do j = 1, siz_lat
            !ローカルの始点・終点の行を定義する。
            start_j = 1 + (j-1)*samples
            end_j = j*samples

            !ローカルのループをして、データを集約タイルにコピーする。
            do ii = 1, samples
               do jj = 1, samples
                  result%data(start_i+ii-1,end_j-jj+1) = array(i,j)%data(ii,jj)
               end do
            end do

         end do
      end do
      print *, 'Progress: End merging.'

   end subroutine merge_tiles


   subroutine nc_output(outname, single)
      character(len=*) :: outname
      type(Tile), intent(in) :: single
      type(LunarNC) :: nc

      !ncの出力名をoutnameに設定する。
      call nc%set_name(outname)

      !ncの縦横長さをタイルから設定する。
      call nc%set_length(single)

      !ncの格子間隔をタイルから取得して設定する。
      call nc%set_step(single)

      !ncの経度緯度グリッドを作成する。
      call nc%set_grid(single)

      !ncを定義する。
      call nc%define_nc()

      !ncにタイルからデータをコピーする。
      call nc%load_data(single)

      !ncの出力ファイルに変数を書き込む。
      call nc%write_var()

      !ncの書き込み用変数の割り付けを解除する。
      call nc%deallocate()

      !ncの出力ファイルを閉じる。
      call nc%close()

   end subroutine nc_output


end module img2nc