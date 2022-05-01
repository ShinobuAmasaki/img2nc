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
      procedure :: set_length => lnc_set_length_of_dim
      procedure :: set_step => lnc_set_step
      procedure :: set_grid => lnc_set_grid
      procedure :: define_nc => lnc_define_nc
      procedure :: load_data => lnc_load_data_nc
      procedure :: write_var => lnc_write_var_nc
      procedure :: deallocate => lnc_deallocate_nc
      procedure :: close => lnc_close_nc

   end type LunarNC

contains

   ! subroutine lnc_set_name_from_image(self, img)
   !    class(LunarNC) :: self
   !    type(Image), intent(in) :: img
   !    character(len=256) :: label_name

   !    call img%label%get_name(label_name)

   !    self%outfile = trim(change_extension(label_name, 'nc'))
   !    return
   ! end subroutine lnc_set_name_from_image
      
   subroutine lnc_set_name_from_string(self, code)
      class(LunarNC) :: self
      character(*), intent(in) :: code

      self%outfile = trim(code) // '.nc'
      ! self%image%set_name(code)
      return
   end subroutine lnc_set_name_from_string
   
   subroutine lnc_set_length_of_dim(self, img)
      class(LunarNC) :: self
      type(Image), intent(in) :: img

      self%nx = img%size_dem(dim=1)
      self%ny = img%size_dem(dim=2)
      self%nz = 1

      return
   end subroutine lnc_set_length_of_dim

   subroutine lnc_set_step(self, img)
      class(LunarNC) :: self
      type(Image), intent(in) :: img
      integer(int32) :: east, west, north, south

      east = nint(img%label%get_east())
      west = nint(img%label%get_west())
      north = nint(img%label%get_north())
      south = nint(img%label%get_south())


      self%step_lon = (dble(east) - dble(west))/4096d0
      self%step_lat = (dble(north) - dble(south))/4096d0
      return
   end subroutine lnc_set_step

   subroutine lnc_set_grid(self,img)
      class(LunarNC) :: self
      type(Image) :: img
      integer(int32) :: i

      allocate(self%lon(self%nx), self%lat(self%ny))

      do i = 1, self%nx
         self%lon(i) = self%step_lon*(i-1) + dble(nint(img%west_lon))
      end do

      do i = 1, self%ny
         self%lat(i) = self%step_lat*(i-1) + dble(nint(img%south_lat))
      end do

      print *, self%lon(1:10)
      print *, self%lat(1:10)

      return

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

   subroutine lnc_load_data_nc(self, img)
      class(LunarNC) :: self
      type(Image) :: img

      allocate(self%data(self%nx, self%ny))
      ! self%data(1:self%nx,1:self%ny) = dble(img%dem(1:self%nx,1:self%ny))
      ! self%data(:,:) = dble(img%dem(:,:))
      do j = 1, self%ny
         do i = 1, self%nx
            self%data(i,j) = dble(img%dem(i,j))
         end do
      end do

      return
   end subroutine lnc_load_data_nc

   subroutine lnc_write_var_nc(self)
      class(LunarNC) :: self

      call check( nf90_put_var(self%ncid, self%lon_id, self%lon(1:self%nx) ) )
      print *, 'progress: put_var lon'
      call check( nf90_put_var(self%ncid, self%lat_id, self%lat(1:self%ny) ) )
      print *, 'progress: put_var lat'

      self%start_nc = [1, 1]
      self%count_nc = [self%nx, self%ny]
      call check( nf90_put_var(self%ncid, self%elev_id, self%data, start=self%start_nc, count=self%count_nc) )
      print *, 'progress: put_var elev'
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

!-------------------------------------------------------------------!
   subroutine img2tile(single)
      class(Image) :: self
      class(Tile), intent(out) :: single

   end subroutine img2tile

end module img2nc