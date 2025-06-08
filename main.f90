program main 
  use pnetcdf_utils

  implicit none

  integer, parameter :: FD = 12
  integer, parameter :: im=12
  integer, parameter :: jm=12 
  integer :: im_dim, jm_dim 
  integer :: myim, myjm
  integer :: im_start, im_end
  integer :: jm_start, jm_end
  character(len=200) :: filename 
  integer :: myid, numprocs, ierr, info
  real, dimension(3, 12) :: Rad
  real, dimension(3) :: x,y 

  ! --- Initialize MPI ---
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, numprocs, ierr)
  !write(6,*) "Hello from proc # ", myid, "of ", numprocs

  !Just decomp in x
  call Decomp1D( FD, numprocs, myid, im_start, im_end)
  !write(6,*) "With nx=", FD, ", proc ", myid, "goes from ", im_start, " to ", im_end
  jm_start = 1
  jm_end = FD
  myim = 3
  myjm = 12

  !Define chunk of data 
  Rad = myid
  x = myid
  y = myid*10.

  ! --- Create the NetCDF file ---
  filename = "example.nc"

  ! Only one process creates the file 
  if(myid.eq.0) then
    ! --- Write the data ---
    call create_file(filename, im, jm)
  endif

  !All process open the file
  call open_file(filename)

  !All write their data
  call write_data(im_start, myim, jm_start, myjm, x, y, rad)
 
  ! --- Close the file ---
  !All close the file
  call close_file()

  ! --- Print a success message ---
  if (myid .eq. 0) print *, "SUCCESS: wrote parallel NetCDF file ", filename 

  ! --- Finalize MPI ---
  call MPI_Finalize(ierr)

end program 
