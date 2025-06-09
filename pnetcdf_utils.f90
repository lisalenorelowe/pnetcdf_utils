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
    integer, save :: x_var, y_var, rad_var
    integer :: im_dim, jm_dim  
    integer :: ierr, info
    file_id = -1

    call MPI_Info_Create( info, ierr )
    call MPI_Info_Set( info, 'ind_wr_buffer_size', '16777216', ierr )
    ierr = nfmpi_create( MPI_COMM_SELF,trim(filename), IOR( NF_CLOBBER, NF_64BIT_OFFSET ), info, file_id)
    call chkerr( ierr, 'create NetCDF output file ' // filename )
    call MPI_Info_Free( info, ierr )
   
    ! --- Define dimensions ---
    call defdim( file_id, im_dim, 'x', im )
    call defdim( file_id, jm_dim, 'y', jm )
  
    ! --- Define attributes
    call deftat( file_id, '_FillValue','1023s')
    call deftat( file_id, 'long_name', 'ABI L1b Radiances')
    call deftat( file_id, 'standard_name', 'toa_outgoing_radiance_per_unit_wavenumber')
    call deftat( file_id, '_Unsigned', 'true')
    call deftat( file_id, 'sensor_band_bit_depth', '10b')
    call deftat( file_id, 'valid_range', '0s, 1022s')
    call deftat( file_id, 'scale_factor', '0.1760585f' )
    call deftat( file_id, 'add_offset', '-5.2392f')
    call deftat( file_id, 'units', 'mW m-2 sr-1 (cm-1)-1')
    call deftat( file_id, 'resolution', 'y: 0.000056 rad x: 0.000056 rad')
    call deftat( file_id, 'coordinates', 'band_id band_wavelength t y x')
    call deftat( file_id, 'grid_mapping', 'goes_imager_projection')
    call deftat( file_id, 'cell_methods', 't: point area: point')
    call deftat( file_id, 'ancillary_variable', 'DQF')


    ! --- Define a variable ---
    call defvr1( file_id, im_dim, x_var, 'x', 'x length','km')
    call defvr1( file_id, jm_dim, y_var, 'y', 'y length','km')
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
    integer :: x_var, y_var, rad_var
    integer :: info, ierr

    x_var = 1
    y_var = 2
    rad_var = 3

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
    integer :: x_var, y_var, rad_var
    integer(kind=2), dimension(im, jm) :: rad 
    integer(kind=2), dimension(im) :: x
    integer(kind=2), dimension(im) :: y
    integer ierr
    integer(kind=MPI_OFFSET_KIND) STARTS(2),COUNTS(2)
    integer(kind=MPI_OFFSET_KIND) xstart(1),ystart(1)
    integer(kind=MPI_OFFSET_KIND) xcount(1),ycount(1)

    x_var = 1
    y_var = 2
    rad_var = 3

    STARTS( 1 ) = IMSTART
    STARTS( 2 ) = JMSTART
    COUNTS( 1 ) = IM
    COUNTS( 2 ) = JM

    xstart( 1 ) = imstart
    ystart( 1 ) = jmstart
    xcount( 1 ) = im
    ycount( 1 ) = jm

    !write(6,*) file_id, y_var, ystart, ycount, y
    ierr = nfmpi_put_vara_int2_all( file_id, y_var, ystart,ycount, y ) 
    call chkerr( ierr, 'write output variable y')

    ierr = nfmpi_put_vara_int2_all( file_id, x_var, xstart, xcount, x )
    call chkerr( ierr, 'write output variable x')

    ierr = nfmpi_put_vara_int2_all( file_id, rad_var, starts, counts, Rad )
    call chkerr( ierr, 'write output variable Rad')

    !CALL FLUSH_FILE() ! Flush buffers to disk in case of crash.

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
