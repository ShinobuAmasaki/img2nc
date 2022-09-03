module img2nc
   use, intrinsic :: iso_fortran_env
   use mod_boundary
   use netcdf
   use mod_read_img

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
      procedure :: set_step => lnc_set_step_from_tile
      procedure :: set_grid => lnc_set_grid_from_tile
      procedure :: define_nc => lnc_define_nc
      procedure :: load_data => lnc_load_data_nc_from_tile
      procedure :: load_data_array => lnc_load_data_nc_from_array
      procedure :: write_var => lnc_write_var_nc
      procedure :: deallocate => lnc_deallocate_nc
      procedure :: close => lnc_close_nc

   end type LunarNC

   interface create_name_list
      module procedure create_name_list_edge, create_name_list_4var
   end interface


contains
      
   subroutine lnc_set_name_from_string(self, name)
      class(LunarNC) :: self
      character(*), intent(in) :: name

      self%outfile = trim(name)

      return
   end subroutine lnc_set_name_from_string
   
!-- receive type: Tile
   subroutine lnc_set_length(self, single)
      class(LunarNC) :: self
      type(Tile), intent(in) :: single

      !ポインタが割り当てられている場合、分岐する。
      if (associated(single%p_data)) then
         self%nx = size(single%p_data, dim=1)
         self%ny = size(single%p_data, dim=2)
      else
         self%nx = size(single%data, dim=1)
         self%ny = size(single%data, dim=2)
      end if

      self%nz = 1

      ! print *, "Grid size: ", self%nx, self%ny

   end subroutine lnc_set_length


   subroutine lnc_set_step_from_tile(self, single)
      class(LunarNC) :: self
      type(Tile), intent(in) :: single
      integer(int32) :: east, west, north, south

      east = single%east_lon
      west = single%west_lon
      south = single%south_lat
      north = single%north_lat

      self%step_lon = (dble(east) - dble(west))/dble(self%nx)
      self%step_lat = (dble(north) - dble(south))/dble(self%ny)

      ! print *, "Grid step: ", self%step_lon, self%step_lat

   end subroutine lnc_set_step_from_tile

   
   subroutine lnc_set_grid_from_tile(self,single)
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

   end subroutine lnc_set_grid_from_tile


   subroutine lnc_define_nc(self)
      class(LunarNC) :: self

      ! 書き込み先ファイルを定義する。
      call check( nf90_create(trim(self%outfile), NF90_HDF5, self%ncid) )
      ! 次元を定義する。
      call check( nf90_def_dim(self%ncid, 'longitude', self%nx, self%lon_dim_id) )
      call check( nf90_def_dim(self%ncid, 'latitude', self%ny, self%lat_dim_id) )
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
   

   subroutine lnc_load_data_nc_from_tile(self, single)
      class(LunarNC) :: self
      type(Tile) :: single

      allocate(self%data(self%nx, self%ny))

      if (associated(single%p_data)) then
         self%data(1:self%nx, 1:self%ny) = dble(single%p_data(1:self%nx, 1:self%ny))

      else  
         self%data(1:self%nx, 1:self%ny) = dble(single%data(1:self%nx, 1:self%ny))
      
      end if
   end subroutine lnc_load_data_nc_from_tile

   subroutine lnc_load_data_nc_from_array(self, array)
      class(LunarNC) :: self
      integer(int16) :: array(:,:)
      integer(int32) :: j

      ! allocate(self%data(self%nx, self%ny))
      ! do i = 1, self%nx
      !    self%data(i,:) = dble(array(i,:))
      ! end do
      allocate(self%data(self%nx, self%ny))
      do j = 1, self%ny
         self%data(:,j) = dble(array(:,j))
      end do

   end subroutine lnc_load_data_nc_from_array


   subroutine lnc_write_var_nc(self)
      class(LunarNC) :: self

      call check( nf90_put_var(self%ncid, self%lon_id, self%lon(1:self%ny) ) )
      print *, 'nc: put_var lon'
      call check( nf90_put_var(self%ncid, self%lat_id, self%lat(1:self%nx) ) )
      print *, 'nc: put_var lat'

      self%start_nc = [1, 1]
      self%count_nc = [self%nx, self%ny]
      call check( nf90_put_var(self%ncid, self%elev_id, self%data, start=self%start_nc, count=self%count_nc) )
      print *, 'nc: put_var elev'
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

   
   ! subroutine read_tile_list(filename, name_list)
   ! !-- DEPRECATED --!
   !    character(len=256), intent(in) :: filename
   !    character(len=256), intent(out), allocatable :: name_list(:,:)
   !    integer(int32) :: siz_lon, siz_lat
   !    integer(int32) :: unit

   !    !リストファイルを開く。
   !    open(file=filename, status='old', newunit=unit)
   !    !ヘッダーの読み取り
   !    read(unit, *) siz_lon, siz_lat

   !    !エラー処理
   !    call tile_size_check(siz_lon, siz_lat, unit)

   !    !ファイル名の読み込み先を割り付ける。
   !    allocate( name_list(siz_lon, siz_lat) )

   !    !リストの読み込み(列優先)
   !    do i = 1, siz_lon
   !       do j = 1, siz_lat
   !          read(unit, '(a)') name_list(i,j)
   !       end do
   !    end do
      
   !    !ファイルを閉じる。
   !    close(unit)
   
   ! end subroutine read_tile_list


   subroutine create_data_name(west, north, name)
      integer(int32), intent(in) :: west, north
      integer(int32) :: east, south
      character(len=*), intent(out) :: name
      character(len=11) :: prefix
      character(len=2) :: postfix
      character(len=4) :: e_west, e_east
      character(len=3) :: e_north, e_south

      !e.g. DTM_MAP_01_N44E323N43E324SC.img
      if (west == 359) then
         !東端が360度となる場合は、変数eastに整数0を代入する。
         east = 0
      else
         east = west + 1
      end if

      south = north - 1 

      prefix = 'DTM_MAP_01_'
      postfix = 'SC'

      ! 西端と東端の記述
      write(e_west, '(a, i3.3)') 'E', west 
      write(e_east, '(a, i3.3)') 'E', east

      !北端の記述
      if ( north >= 0 ) then
         write(e_north, '(a, i2.2)') 'N', abs(north)

      else if ( north < 0 ) then
         write(e_north, '(a, i2.2)') 'S', abs(north)
      
      end if

      !南端の記述
      if ( south >= 0 ) then
         write(e_south, '(a, i2.2)') 'N', abs(south)

      else if ( south < 0 ) then
         write(e_south, '(a, i2.2)') 'S', abs(south)

      end if

      name = prefix // e_north // e_west // e_south // e_east // postfix

   end subroutine create_data_name

!
   subroutine create_name_list_4var(data_root, west, east, south, north, name_list)
      character(len=*), intent(in) :: data_root
      integer(int32), intent(in) :: west, east, south, north
      character(len=256), intent(out), allocatable :: name_list(:,:)
      character(len=27) :: code
      integer(int32) :: siz_lon, siz_lat, i, j, e_west, e_north
      character(len=6) :: lon_dir

      !配列のサイズを求める。
      siz_lon = east - west
      siz_lat = north - south

      ! print *, siz_lon, siz_lat

      !配列サイズのチェック
      call tile_size_check(siz_lon, siz_lat)

      !配列の割り付け
      allocate( name_list(siz_lon, siz_lat) )

      do j = 1, siz_lat
         e_north = south + j

         do i = 1, siz_lon
            !東端と北端の経緯度を計算する。
            e_west = east - i

            call create_data_name(e_west, e_north, code)

            write(lon_dir, '(a,i3.3)') 'lon', e_west

            !南東の角から順番に書き込む
            name_list(siz_lon-i+1,siz_lat-j+1) = trim(data_root) // '/' // trim(lon_dir) // '/' // code
            ! インデックスの指定を変更しない：マージ時のタイル配置に影響する
         end do
      end do

   end subroutine create_name_list_4var


   subroutine create_name_list_edge(data_root, edge, name_list)
      character(len=*), intent(in) :: data_root
      type(boundary), intent(in) :: edge
      character(len=256), intent(out), allocatable :: name_list(:,:)

      call create_name_list_4var(data_root, edge%get_west(), edge%get_east(), edge%get_south(), edge%get_north(), name_list)

   end subroutine create_name_list_edge

   !リストファイルのヘッダーの数値をチェックする。
   subroutine tile_size_check(nx, ny, unit)
      integer(int32), intent(in) :: nx, ny
      integer(int32), optional, intent(in) :: unit

      if (nx <= 0 .or. ny <= 0) then
         write(0, *) 'ERROR: Invalid tile array size.'
         
         if (present(unit)) then
            close(unit)
         end if
         
         stop
      end if
   
   end subroutine tile_size_check


   subroutine load_img_to_tile(name_list, array, shrink)
      character(len=*), intent(in) :: name_list(:,:)
      type(Tile), intent(inout), allocatable :: array(:,:)
      type(Image), allocatable :: img
      integer(int32) :: siz_lon, siz_lat, total, current
      integer(int32), optional, intent(in) :: shrink

      !配列の縦横サイズを取得する。
      siz_lon = size(name_list, 1)
      siz_lat = size(name_list, 2)

      !読み込み先配列を割り付ける。
      allocate( array(siz_lon, siz_lat) )

      !読み込み総数の取得
      total = size(name_list, dim=1) * size(name_list, dim=2)

      !イメージの読み込み
      current = 1      
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
   
            if ( present(shrink) ) then
            
               !縮小指定してimgをTileに変換してarrayのi-j成分に書き込む
               array(i,j) = img%img2tile(shrink)

            else
               !タイル配列に書き込む
               array(i,j) = img%img2tile()
            
            end if
            
            !イメージを解放
            deallocate(img)
   
            !ログ出力
            ! print *, 'Loaded: ', trim(name_list(i,j))
            write(*, '("Loaded ", i5, "/", i5, ": " a)') current, total, trim(name_list(i,j))
            
            !インクリメント
            current = current + 1
         end do
      end do
   end subroutine load_img_to_tile

   ! set lon/lat edge value into a single tile
   function north_lat_from_tile_array(array, i_begin) result(north)
      implicit none
      type(Tile), intent(in) :: array(:,:)
      integer(int32), intent(in) :: i_begin
      integer(int32) :: north
      
      north = array(i_begin,1)%north_lat

   end function north_lat_from_tile_array


   function west_lon_from_tile_array(array, i_begin) result(west)
      implicit none
      type(Tile), intent(in) :: array(:,:)
      integer(int32), intent(in) :: i_begin
      integer(int32) :: west


      west = array(i_begin,1)%west_lon

   end function west_lon_from_tile_array


   function south_lat_from_tile_array(array, i_begin) result(south)
      implicit none
      type(Tile), intent(in) :: array(:,:)
      integer(int32), intent(in) :: i_begin
      integer(int32) :: south_end, south

      south_end = size(array, dim=2)

      south = array(i_begin, south_end)%south_lat

   end function south_lat_from_tile_array


   function east_lon_from_tile_array(array, i_end) result(east)
      implicit none
      type(Tile), intent(in) :: array(:,:)
      integer(int32), intent(in) :: i_end
      integer(int32) :: east_end, east

      east_end = i_end
      east = array(east_end, 1)%east_lon

   end function east_lon_from_tile_array
 

   subroutine merge_tiles(array, result)
      type(Tile), intent(in) :: array(:,:)   !タイル配列
      type(Tile), intent(out) :: result      !集約タイル
      integer(int32) :: nlon, nlat, siz_lon, siz_lat
      integer(int32) :: i, j, start_i, end_i, start_j, end_j, ii, jj
      integer(int32) :: samples, lines

      !1タイルの1辺の大きさを取得する
      samples = size(array(1,1)%data, dim=2)
      lines = size(array(1,1)%data, dim=1)

      !集約タイルの大きさを取得する。
      nlon = size_of_tile_array(array, 1)
      nlat = size_of_tile_array(array, 2)

      !タイル配列のサイズを取得する。
      siz_lon = size(array, 1)
      siz_lat = size(array, 2)

      !集約タイルを割り付ける。
      allocate(result%data(nlon, nlat))

      !東西南北端の緯度経度をコピーする。
      !tileの北西端・南東端 -> resultの端
      result%west_lon  = array(1,1)%west_lon
      result%north_lat = array(1,1)%north_lat
      result%east_lon  = array(siz_lat, siz_lon)%east_lon
      result%south_lat = array(siz_lat, siz_lon)%south_lat

      if (siz_lon==1 .and. siz_lat==1) then
         do j = 1, nlon
            do i = 1, nlat
               result%data(i,j) = array(1,1)%data(i,j)

               ! result%data(i,j) = array(1,1)%data(i,nlat-j+1)
            end do
         end do
         return
      end if

      !タイル配列の値を集約タイルにコピーして連結する。
      print *, 'merge: Start merging.'
      ! do i = 1, siz_lon
      !    !ローカルの始点・終点の列を定義する。
      !    start_i = 1 + (i-1)*samples
      !    end_i = i*samples
         
      !    do j = 1, siz_lat
      !       !ローカルの始点・終点の行を定義する。
      !       start_j = 1 + (j-1)*samples
      !       end_j = j*samples

      !       !ローカルのループをして、データを集約タイルにコピーする。
      !       do ii = 1, samples
      !          do jj = 1, samples
      !             result%data(start_i+ii-1,end_j-jj+1) = array(i,j)%data(ii,jj)
      !          end do
      !       end do

      !    end do
      ! end do

      do j = 1, siz_lon
         start_j = 1 + (j-1)*samples
         end_j = j*samples

         do i = 1, siz_lat
            start_i = 1 + (i-1)*lines
            end_i = i*lines

            result%data(start_i:end_i, start_j:end_j) = array(i,j)%data(1:lines, 1:samples)
         end do
      end do 
      print *, 'merge: End merging.'

   end subroutine merge_tiles


   subroutine nc_output(outname, single)
      character(len=*) :: outname
      type(Tile), intent(in) :: single
      type(LunarNC) :: nc

      !ncの出力名をoutnameに設定する。
      call nc%set_name(outname)
      print *, 'nc: set name.'

      !ncの縦横長さをタイルから設定する。
      call nc%set_length(single)
      print *, 'nc: set length.'

      !ncの格子間隔をタイルから取得して設定する。
      call nc%set_step(single)
      print *, 'nc: set step.'

      !ncの経度緯度グリッドを作成する。
      call nc%set_grid(single)
      print *, 'nc: set grid.'

      !ncを定義する。
      call nc%define_nc()
      print *, 'nc: define nc.'

      !ncにタイルからデータをコピーする。
      call nc%load_data(single)
      print *, 'nc: load data.'

      !ncの出力ファイルに変数を書き込む。
      call nc%write_var()
      print *, 'nc: write var'

      !ncの書き込み用変数の割り付けを解除する。
      call nc%deallocate()
      print *, 'nc: deallocate.'

      !ncの出力ファイルを閉じる。
      call nc%close()
      print *, 'nc: close.'

   end subroutine nc_output


end module img2nc