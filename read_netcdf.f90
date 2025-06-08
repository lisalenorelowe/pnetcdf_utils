subroutine read_netcdf(filename, x, y, Rad)
  use netcdf
  use netcdf_utils, only: chkerr
  implicit none

  character(len=*), intent(in) :: filename
  integer :: file_id, ierr
  integer :: x_var,y_var,rad_var
  integer(kind=2), dimension(:,:), intent(out) :: Rad 
  integer(kind=2), dimension(:), intent(out) :: y
  integer(kind=2), dimension(:), intent(out) :: x

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

  ! Read the Rad data
  ierr = nf90_get_var(file_id, rad_var, Rad)
  call chkerr( ierr, 'Error reading Rad: ' )
  ! Read x 
  ierr = nf90_get_var(file_id, x_var, x)
  call chkerr( ierr, 'Error reading x: ' )
  ! Read y 
  ierr = nf90_get_var(file_id, y_var, y)
  call chkerr( ierr, 'Error reading y: ' )

  ! Close the NetCDF file
  ierr = nf90_close(file_id)
  call chkerr( ierr, 'Error closing file: ' )

  print *, 'Successfully read data from ', filename 

end subroutine read_netcdf

