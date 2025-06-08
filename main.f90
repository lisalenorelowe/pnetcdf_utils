program main 
  use pnetcdf
  use mpi
  use NETCDF_UTILITIES
  use p

  implicit none

  integer :: IM_DIM, JM_DIM, im, jm 
  character(len=20) :: FILE_NAME
  character(len=10) :: message
  integer :: myid, mysize, ierr, info
  integer :: RAD_VAR, X_VAR, Y_VAR
  real, dimension(30, 30) :: Rad
  real, dimension(30) :: x,y 
  INTEGER,SAVE :: FILE_ID ! NetCDF ID for file.

  ! --- Initialize MPI ---
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, mysize, ierr)

  CALL MPI_Info_Create( info, ierr )
  CALL MPI_Info_Set( info, 'ind_wr_buffer_size', '16777216', ierr )

  ! --- Create the NetCDF file ---
  FILE_NAME = "example.nc"

  ! CREATE_FILE: Create output NetCDF file with given header data.
  ! In a concurrent program, only one process should call this routine
  ! then call CLOSE_FILE then
  ! worker processes should call OPEN_FILE, WRITE_DATA, CLOSE_FILE.
  if(myid.eq.0) then
    FILE_ID=-1
    CALL MPI_Info_Create( info, ierr )
    CALL MPI_Info_Set( info, 'ind_wr_buffer_size', '16777216', ierr )
    ierr = nfmpi_CREATE( MPI_COMM_SELF,trim(FILE_NAME), IOR( NF_CLOBBER, NF_64BIT_OFFSET ), info, FILE_ID)
    CALL CHKERR( ierr, 'create NetCDF output file ' // FILE_NAME )
    CALL MPI_INFO_FREE( INFO, ierr )
 
    ! --- Define dimensions ---
    im=30
    jm=30
    CALL DEFDIM( FILE_ID, IM_DIM, 'x', im )
    CALL DEFDIM( FILE_ID, JM_DIM, 'y', jm )
  
    ! --- Define attributes
    CALL DEFTAT( FILE_ID, 'Conventions','CF-1.6')
  
    ! --- Define a variable ---
    CALL DEFVR1( FILE_ID, IM_DIM, X_VAR, 'X', 'x length','km')
    CALL DEFVR1( FILE_ID, JM_DIM, Y_VAR, 'Y', 'y length','km')
    CALL DEFVR2( FILE_ID, IM_DIM, JM_DIM, RAD_VAR, 'Rad', 'Radiation','W/m2')
    ! --- End of definition mode --- 
  
    ! --- Write the data ---
    ierr = nfmpi_enddef(FILE_ID)
    CALL CHKERR( ierr, 'create NetCDF output header' )
    ierr = nfmpi_begin_indep_data( FILE_ID )
    CALL CHKERR( ierr, 'begin independent data access mode' ) 
 
    !Write variable
    x=1.
    y=2.
    Rad=3.
    ierr = nfmpi_put_var_real( FILE_ID, X_VAR, x )
    call chkerr( ierr, 'write variable x' )
    ierr = nfmpi_put_var_real( FILE_ID, Y_VAR, y )
    call chkerr( ierr, 'write variable y' )
    ierr = nfmpi_put_var_real( FILE_ID, RAD_VAR, Rad )
    call chkerr( ierr, 'write variable Rad' )
 
    !call FLUSH_FILE()
    ierr = nfmpi_SYNC( FILE_ID )
    CALL CHKERR( ierr, 'flush buffers to disk ' )

  endif
  ! --- Close the file ---

  ! --- Print a success message ---
  if (myid .eq. 0) print *, "SUCCESS: wrote parallel NetCDF file ", FILE_NAME

  ! --- Finalize MPI ---
  call MPI_Finalize(ierr)

end program parallel_netcdf_example
