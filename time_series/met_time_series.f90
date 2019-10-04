! ----------------------------------------------------------------
! file: met_time_series.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March  1, 2013 by William A. Perkins
! Last Change: 2019-10-04 13:34:11 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE met_time_series
! ----------------------------------------------------------------
MODULE met_time_series

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  ! The number of met data values (usually)
  INTEGER, PARAMETER, PUBLIC :: met_fields = 5

  TYPE met_time_series_rec
     TYPE (time_series_rec), POINTER :: ts
     DOUBLE PRECISION, POINTER :: current(:)
  END type met_time_series_rec

  ! A more readable way to refer to the fields 
  INTEGER, PARAMETER, PUBLIC :: &
       &MET_AIRT = 1, &
       &MET_DEWT = 2, &
       &MET_WIND = 3, &
       &MET_BARO = 4, &
       &MET_SWRAD = 5, &
       &MET_LWRAD = 6

CONTAINS

  ! ----------------------------------------------------------------
  ! TYPE(MET_TIME_SERIES_REC) FUNCTION met_time_series_alloc
  ! ----------------------------------------------------------------
  TYPE(MET_TIME_SERIES_REC) FUNCTION met_time_series_alloc(id, len, dolw) RESULT (metts)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: id, len
    LOGICAL, INTENT(IN), OPTIONAL :: dolw
    POINTER metts

    INTEGER :: nfld

    nfld = met_fields
    IF (PRESENT(dolw)) THEN
       IF (dolw) THEN
          nfld = nfld + 1
       END IF
    END IF

    ALLOCATE(metts)
    metts%ts => time_series_alloc(id, nfld, len)
    metts%current => metts%ts%current
    
  END FUNCTION met_time_series_alloc


  ! ----------------------------------------------------------------
  ! TYPE(MET_TIME_SERIES_REC) FUNCTION met_time_series_read
  ! ----------------------------------------------------------------
  TYPE(MET_TIME_SERIES_REC) FUNCTION met_time_series_read(filename, dolw) RESULT (metts)

    IMPLICIT NONE

    POINTER metts
    CHARACTER (LEN=*), INTENT(IN) :: filename
    LOGICAL, INTENT(IN), OPTIONAL :: dolw
    INTEGER :: nfld

    nfld = met_fields
    IF (PRESENT(dolw)) THEN
       IF (dolw) THEN
          nfld = nfld + 1
       END IF
    END IF

    ALLOCATE(metts)
    metts%ts => time_series_read(filename, nfld, 50)
    metts%current => metts%ts%current

  END FUNCTION met_time_series_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_time_series_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE met_time_series_destroy(metts)

    IMPLICIT NONE

    TYPE (met_time_series_rec), POINTER :: metts

    IF (.NOT. ASSOCIATED(metts)) RETURN

    CALL time_series_destroy(metts%ts)
    DEALLOCATE(metts)
    NULLIFY(metts)

  END SUBROUTINE met_time_series_destroy


  ! ----------------------------------------------------------------
  ! SUBROUTINE met_time_series_update
  ! ----------------------------------------------------------------
  SUBROUTINE met_time_series_update(metts, datetime)

    IMPLICIT NONE

    TYPE (met_time_series_rec), POINTER :: metts
    DOUBLE PRECISION, INTENT(IN) :: datetime

    CALL time_series_interp(metts%ts, datetime)

  END SUBROUTINE met_time_series_update


END MODULE met_time_series
