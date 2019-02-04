! ----------------------------------------------------------------
! file: const_series.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March  4, 2013 by William A. Perkins
! Last Change: 2017-06-22 09:21:57 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE const_series
! ----------------------------------------------------------------
MODULE const_series

  USE time_series

  IMPLICIT NONE
  
  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE const_series_rec
     DOUBLE PRECISION :: defvalue
     TYPE (time_series_rec), POINTER :: ts
     DOUBLE PRECISION, POINTER :: current
  END type const_series_rec

CONTAINS

  ! ----------------------------------------------------------------
  ! TYPE (CONST_SERIES_REC) FUNCTION const_series_alloc
  ! ----------------------------------------------------------------
  TYPE (CONST_SERIES_REC) FUNCTION const_series_alloc(defvalue) RESULT (cts)

    IMPLICIT NONE

    POINTER cts
    DOUBLE PRECISION, INTENT(IN) :: defvalue

    ALLOCATE(cts)
    NULLIFY(cts%ts)
    cts%defvalue = defvalue
    cts%current => cts%defvalue

  END FUNCTION const_series_alloc

  ! ----------------------------------------------------------------
  ! TYPE (CONST_SERIES_REC) FUNCTION const_series_read
  ! ----------------------------------------------------------------
  TYPE (CONST_SERIES_REC) FUNCTION const_series_read(filename) RESULT (cts)

    IMPLICIT NONE

    POINTER cts
    CHARACTER (LEN=*), INTENT(IN) :: filename

    ALLOCATE(cts)
    cts%ts => time_series_read(filename, 1)
    cts%current => cts%ts%current(1)

  END FUNCTION const_series_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE const_series_update
  ! ----------------------------------------------------------------
  SUBROUTINE const_series_update(cts, datetime)

    IMPLICIT NONE

    TYPE (const_series_rec), POINTER :: cts
    DOUBLE PRECISION, INTENT(IN) :: datetime

    IF (ASSOCIATED(cts%ts)) THEN
       CALL time_series_interp(cts%ts, datetime)
    END IF

  END SUBROUTINE const_series_update


  ! ----------------------------------------------------------------
  ! SUBROUTINE const_series_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE const_series_destroy(cts)

    IMPLICIT NONE

    TYPE (const_series_rec), POINTER :: cts

    IF (.NOT. ASSOCIATED(cts)) RETURN
    CALL time_series_destroy(cts%ts)
    DEALLOCATE(cts)
    NULLIFY(cts)

  END SUBROUTINE const_series_destroy


END MODULE const_series
