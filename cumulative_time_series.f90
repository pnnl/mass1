! ----------------------------------------------------------------
! file: cumulative_time_series.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 28, 2013 by William A. Perkins
! Last Change: 2017-06-22 09:22:17 d3g096
! ----------------------------------------------------------------
  
! RCS ID: $Id$ Battelle PNL


! ----------------------------------------------------------------
! MODULE cumulative_time_series
! ----------------------------------------------------------------
MODULE cumulative_time_series

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE cumulative_time_series_rec
     TYPE (time_series_rec), POINTER :: ts
     DOUBLE PRECISION :: last_time, current_time
     DOUBLE PRECISION :: last_value, current_value
     DOUBLE PRECISION :: rate
  END type cumulative_time_series_rec

  DOUBLE PRECISION, PRIVATE, PARAMETER :: bogus = -9999.0

  CHARACTER (LEN=1024), PRIVATE :: buf

CONTAINS

  ! ----------------------------------------------------------------
  ! TYPE(CUMULATIVE_TIME_SERIES_REC) FUNCTION cumulative_time_series_read
  ! ----------------------------------------------------------------
  TYPE(cumulative_time_series_rec) FUNCTION cumulative_time_series_read(filename) RESULT (cumts)
  
    IMPLICIT NONE

    POINTER cumts
    CHARACTER (LEN=*) :: filename
    INTEGER, PARAMETER :: fields = 1
    
    ALLOCATE(cumts)
    cumts%ts => time_series_read(filename, fields, 50)
    cumts%last_time = bogus
    cumts%last_value = bogus
    cumts%current_time = bogus
    cumts%current_value = bogus
    cumts%rate = 0.0
    
  END FUNCTION cumulative_time_series_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE cumulative_time_series_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE cumulative_time_series_destroy(cumts)

    IMPLICIT NONE

    TYPE (cumulative_time_series_rec), POINTER :: cumts

    IF (.NOT. ASSOCIATED(cumts)) RETURN

    CALL time_series_destroy(cumts%ts)
    DEALLOCATE(cumts)
    NULLIFY(cumts)

  END SUBROUTINE cumulative_time_series_destroy

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION time_series_index
  ! ----------------------------------------------------------------
  INTEGER FUNCTION time_series_index(ts, datetime, hint) RESULT(i)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER :: ts
    DOUBLE PRECISION, INTENT(IN) :: datetime
    INTEGER, INTENT(IN), OPTIONAL :: hint

    INTEGER :: startidx

    IF (datetime .LT. ts%series(1)%time) THEN
       i = 0
    ELSE IF (datetime .GE. ts%series(ts%length)%time) THEN
       i = ts%length
    ELSE 

       IF (PRESENT(hint)) THEN
          startidx = hint
       ELSE 
          startidx = ts%start
       END IF

       i = startidx
       DO i = ts%start, ts%length - 1
          IF (datetime .GE. ts%series(i)%time .AND.&
               &datetime .LT. ts%series(i+1)%time) EXIT
       END DO
       IF (i .GT. 1) ts%start = i - 1
    END IF

  END FUNCTION time_series_index

  ! ----------------------------------------------------------------
  ! SUBROUTINE cumulative_time_series_update
  ! ----------------------------------------------------------------
  SUBROUTINE cumulative_time_series_update(cumts, datetime)

    IMPLICIT NONE

    TYPE (cumulative_time_series_rec), POINTER :: cumts
    DOUBLE PRECISION, INTENT(IN) :: datetime
    
    INTEGER :: i
    DOUBLE PRECISION :: t0, t1, c0, c1
    CHARACTER (LEN=1024) :: dstr

    IF (cumts%ts%length .LT. 2) THEN
       WRITE(buf, *) 'one point cumulative time series are not allowed'
       CALL time_series_error(buf, ts = cumts%ts, fatal = .TRUE.)
    ELSE IF (datetime .LT. cumts%ts%series(1)%time) THEN
       CALL date_format(datetime, dstr)
       WRITE (buf,*) 'date (', TRIM(dstr), ' out of range'
       CALL time_series_error(buf, ts = cumts%ts, fatal = .TRUE.)
    ELSE IF (datetime .GT. cumts%ts%series(cumts%ts%length)%time) THEN
       CALL date_format(datetime, dstr)
       WRITE (buf,*) 'date (', TRIM(dstr), ' out of range'
       CALL time_series_error(buf, ts = cumts%ts, fatal = .TRUE.)
    END IF

    IF (.NOT. (datetime .GE. cumts%last_time .AND. &
         &datetime .LT. cumts%current_time)) THEN
       i = time_series_index(cumts%ts, datetime)
       cumts%ts%start = i

       c0 = cumts%ts%series(i)%field(1)
       c1 = cumts%ts%series(i+1)%field(1)
       
       t0 = cumts%ts%series(i)%time
       t1 = cumts%ts%series(i+1)%time

       ! if the time series is level or decreases during this interval
       ! it's assumed to be a reset and the sum at the start of the
       ! interval is zero.

       IF (c1 - c0 .LT. 0.0) THEN 
          cumts%rate = 0.0
       ELSE 
          cumts%rate = (c1 - c0)/(t1 - t0)/24.0/3600.0
       END IF
       cumts%last_time = t0
       cumts%last_value = c0
       cumts%current_time = t1
       cumts%current_value = c1

    END IF

  END SUBROUTINE cumulative_time_series_update



END MODULE cumulative_time_series
