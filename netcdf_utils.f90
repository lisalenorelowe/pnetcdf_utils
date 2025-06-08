module netcdf_utils
use netcdf
implicit none

contains

  SUBROUTINE CHKERR( ERR, MESSAGE )
    IMPLICIT NONE
    INTEGER,INTENT(IN):: ERR
    CHARACTER(LEN=*),INTENT(IN):: MESSAGE

    IF ( ERR .NE. 0 ) THEN
      PRINT *, 'PROBLEM: Failed to ', MESSAGE
      PRINT *, NF90_STRERROR( ERR )
      STOP
    END IF

    RETURN
  END SUBROUTINE CHKERR

end module netcdf_utils
