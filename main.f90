program main 
  use pnetcdf_utils

  implicit none

  integer, parameter :: FD = 5424 
  integer, parameter :: im= 5424
  integer, parameter :: jm= 5424 
  integer :: myim, myjm
  integer :: im_start, im_end
  integer :: jm_start, jm_end
  character(len=200) :: filename 
  character(len=13) :: myfile
  integer :: myid, numprocs, ierr
  integer(kind=2), dimension(:, :), allocatable :: Rad
  integer(kind=2), dimension(:), allocatable :: x,y 

  ! --- Initialize MPI ---
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, numprocs, ierr)
  !write(6,*) "Hello from proc # ", myid, "of ", numprocs

  !Just decomp in x
  call Decomp1D( FD, numprocs, myid, im_start, im_end)
  myim = im_end - im_start + 1
  write(6,*) "Proc ", myid, "goes from ", im_start, " to ", im_end, "with size=",myim
  !stop
  jm_start = 1
  jm_end = jm 
  myjm = jm  
  allocate( Rad(myim,myjm),stat=ierr )
  if(ierr.ne.0) write(6,*) "error in allocating:Rad"
  allocate( x(myim),stat=ierr )
  if(ierr.ne.0) write(6,*) "error in allocating:x"
  allocate( y(myjm),stat=ierr )
  if(ierr.ne.0) write(6,*) "error in allocating:y"

  !Define chunk of data with input files
  write(myfile,'(''input/Rad'',I1.1,''.nc'')') myid+1
  write(6,*) myfile
  call read_netcdf(myfile, myim, myjm, x, y, Rad)

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
