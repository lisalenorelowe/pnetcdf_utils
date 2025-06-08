!Modified from OUTPUT_NETCDF_CGEM by Todd, Wei, and me

module pnetcdf_utils

use pnetcdf
use mpi
use NETCDF_UTILITIES

implicit none

  integer, save :: file_id ! NetCDF ID for file.

public create_file, open_file, write_data, close_file, flush_file 

contains

  ! create_file: Create output NetCDF file with given header data.
  ! In a concurrent program, only one process should call this routine
  ! then call close_file then
  ! worker processes should call open_file, write_data, close_file
  subroutine create_file( filename, im, jm )  

    integer, intent(in) :: im, jm 
    character(len=*), intent(in) :: filename 
    integer ierr, info
    file_id = -1

    call MPI_Info_Create( info, ierr )
    call MPI_Info_Set( info, 'ind_wr_buffer_size', '16777216', ierr )
    ierr = nfmpi_create( MPI_COMM_SELF,trim(filename), IOR( NF_CLOBBER, NF_64BIT_OFFSET ), info, file_id)
    call chkerr( ierr, 'create NetCDF output file ' // file_name )
    call MPI_Info_Free( info, ierr )
   
    ! --- Define dimensions ---
    call defdim( file_id, im_dim, 'x', im )
    call defdim( file_id, jm_dim, 'y', jm )
  
    ! --- Define attributes
    call deftat( file_id, 'Conventions','CF-1.6')
  
    ! --- Define a variable ---
    call defvr1( file_id, im_dim, x_var, 'X', 'x length','km')
    call defvr1( file_id, jm_dim, y_var, 'Y', 'y length','km')
    call defvr2( file_id, im_dim, jm_dim, rad_var, 'Rad', 'Radiation','W/m2')
    ! --- End of definition mode --- 
  
    ! --- Write the data ---
    ierr = nfmpi_enddef(file_id)
    call chkerr( ierr, 'create NetCDF output header' )
    ierr = nfmpi_begin_indep_data( file_id )
    call chkerr( ierr, 'begin independent data access mode' ) 

    call flush_file()
    
    return
  end subroutine create_file

  ! open_file: Open existing output NetCDF file for shared writing.
  ! In a concurrent program, each worker process should call
  ! open_file, write_data, close_file.
  subroutine open_file( filename )
    character(len=*), intent(in) :: filename  
    integer :: info, ierr

    call MPI_Info_Create( info, ierr )
    call MPI_Info_Set( info, 'ind_wr_buffer_size', '16777216', ierr )
    ierr = nfmpi_open( MPI_COMM_WORLD,trim(filename), IOR( NF_WRITE, NF_64BIT_OFFSET ), info, file_id)
    call MPI_Info_Free( info, ierr)

    ierr = nfmpi_inq_varid( file_id, 'x', x_var )
    call chkerr( ierr, 'inquire NetCDF variable ID ' )
    ierr = nfmpi_inq_varid( file_id, 'y', y_var )
    call chkerr( ierr, 'inquire NetCDF variable ID ' )
    ierr = nfmpi_inq_varid( file_id, 'Rad', rad_var )
    call chkerr( ierr, 'inquire NetCDF variable ID ' )

    return
  end subroutine open_file

  !close_file: close output file
  subroutine close_file
    integer ierr

    ierr=nfmpi_close( file_id )
    call chkerr( ierr, 'close NetCDF output file ')
    file_id = -1

    return
  end subroutine close_file

  !write_data: Write data to output file.
  ! Called by non-concurrent programs to write all data
  subroutine write_data( imstart, im, jmstart, jm, x, y, rad)
    integer, intent(in) :: imstart, jmstart
    integer, intent(in) :: im, jm
    real, dimension(im, jm) :: rad 
    real, dimension(im) :: x
    real, dimension(jm) :: y
    integer ierr, variable
    integer(kind=MPI_OFFSET_KIND) STARTS(2),COUNTS(2)
    integer(kind=MPI_OFFSET_KIND) xstart(1),ystart(1)
    integer request_count, request
    integer, dimension(1) :: requests, statuses

    STARTS( 1 ) = IMSTART
    STARTS( 2 ) = JMSTART
    COUNTS( 1 ) = IM
    COUNTS( 2 ) = JM

    xstart( 1 ) = imstart
    ystart( 1 ) = jmstart
    xcount( 1 ) = im
    ycount( 1 ) = jm

    request_count = 1

    request_count = request_count + 1
    ierr = nfmpi_put_vara_real( file_id, y_var, ystart, ycount, y, requests(request_count) )  
    call chkerr( ierr, 'write output variable y')

    request_count = request_count + 1
    ierr = nfmpi_put_vara_real( file_id, x_var, xstart, xcount, x, requests(request_count) )
    call chkerr( ierr, 'write output variable x')

    request_count = request_count + 1
    ierr = nfmpi_put_vara_real( file_id, rad_var, starts, counts, rad, requests(request_count) )
    call chkerr( ierr, 'write output variable Rad')

    iERR = nfmpi_WAIT_ALL( FILE_ID, REQUEST_COUNT, REQUESTS, STATUSES )
    CALL CHKERR( iERR, 'implement non-blocking interface' )

    DO REQUEST = 1, REQUEST_COUNT
      CALL CHKERR( STATUSES( REQUEST ), 'nonblocking call ' )
    END DO

    ! check status of each nonblocking call

    CALL FLUSH_FILE() ! Flush buffers to disk in case of crash.

    RETURN
  END SUBROUTINE WRITE_DATA

  ! FLUSH_FILE: Flush buffers to disk in case of crash.
  !
  SUBROUTINE FLUSH_FILE() 
    IMPLICIT NONE    
    ! External NetCDF routines:
    INTEGER ERR
    ERR = nfmpi_SYNC( FILE_ID )
    CALL CHKERR( ERR, 'flush buffers to disk ' )

    RETURN 
  END SUBROUTINE FLUSH_FILE

end module pnetcdf_utils
