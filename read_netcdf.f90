subroutine read_netcdf(filename, im, jm, x, y, Rad)
  use netcdf
  use netcdf_utils, only: chkerr
  implicit none

  character(len=*), intent(in) :: filename
  integer :: file_id, ierr
  integer :: x_var,y_var,rad_var
  integer, intent(in) :: im, jm
  integer(kind=2), dimension(im,jm), intent(out) :: Rad 
  integer(kind=2), dimension(jm), intent(out) :: y
  integer(kind=2), dimension(im), intent(out) :: x
  character(len=NF90_MAX_NAME) :: dimname,varname
  integer :: vartype, natts, dimlen, dimid
  integer :: i, ndims, dimids(2)

  ! Open the NetCDF file
  ierr = nf90_open(filename, nf90_nowrite, file_id)
  call chkerr( ierr, 'Error opening file: ' )

  ! Get the variable ID for the Rad variable 
  ierr = nf90_inq_varid(file_id, 'Rad', rad_var)
  call chkerr( ierr, 'Error getting Rad ID: ' )
  ierr = nf90_inq_varid(file_id, 'x', x_var)
  call chkerr( ierr, 'Error getting x ID: ' )
  ierr = nf90_inq_varid(file_id, 'y', y_var)
  call chkerr( ierr, 'Error getting y ID: ' )

 ! Inquire about the variable
  ierr = NF90_INQUIRE_VARIABLE(file_id, rad_var, varname, vartype, ndims, dimids, natts)
  print *, 'Variable:', trim(varname)
  print *, 'Variable type:', vartype
  print *, 'Number of dimensions:', ndims
  print *, 'dimids:', dimids 
  print *, 'Number of attributes:', natts 

  ! Inquire about each dimension
  do i = 1, ndims
    dimid = dimids(i)
    ierr = NF90_INQUIRE_DIMENSION(file_id, dimid, dimname, dimlen)
    print *, '  Dimension', i, ':', trim(dimname), 'Length:', dimlen
  end do

  ! Read the Rad data
  ierr = nf90_get_var(file_id, rad_var, Rad)
  call chkerr( ierr, 'Error reading Rad: ' )
  ! Read x 
  ierr = nf90_get_var(file_id, x_var, x)
  call chkerr( ierr, 'Error reading x: ' )
  !! Read y 
  ierr = nf90_get_var(file_id, y_var, y)
  call chkerr( ierr, 'Error reading y: ' )

  ! Close the NetCDF file
  ierr = nf90_close(file_id)
  call chkerr( ierr, 'Error closing file: ' )

  print *, 'Successfully read data from ', filename 

end subroutine read_netcdf

