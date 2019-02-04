! ----------------------------------------------------------------
! file: tstest3.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created May 14, 2003 by William A. Perkins
! Last Change: 2018-05-03 06:28:22 d3g096
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

! ----------------------------------------------------------------
! MODULE bogus
! ----------------------------------------------------------------
MODULE bogus

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  SUBROUTINE printit(ts)

    IMPLICIT NONE
    TYPE (time_series_rec), POINTER, INTENT(INOUT) :: ts
    DOUBLE PRECISION :: t, deltat = 0.1, start
    INTEGER :: steps, step, i

    start = ts%series(1)%time
    steps = INT((ts%series(ts%length)%time - start)/deltat + 0.5)

    DO step = 0, steps
       t = MIN(start + step*deltat, ts%series(ts%length)%time)
       CALL time_series_interp(ts, t)
       WRITE(*,'(F5.2, 1X, 15F17.8)') t, (ts%current(i), i = 1, ts%fields)
    END DO
    WRITE (*, *)

  END SUBROUTINE printit


END MODULE bogus


PROGRAM tstest4

  USE time_series
  USE bogus

  IMPLICIT NONE

  INTEGER, PARAMETER :: tslen = 3

  TYPE (time_series_rec), POINTER :: ts
  DOUBLE PRECISION :: t, deltat = 0.1, start
  INTEGER :: k, i, steps, step
  DOUBLE PRECISION :: field(10)

  CALL date_time_flags()
  CALL time_series_module_init(debug=10, limit=TS_LIMIT_NONE, mode=TS_REAL_MODE)

  ts => time_series_alloc(1, 1, tslen)
  DO i = 1, tslen
     t = REAL(i)
     field(:) = t**2
     CALL time_series_push(ts, t, field)
  END DO

  CALL printit(ts)

  DO i = 1, tslen
     t = REAL(i + tslen)
     field(:) = t**2
     CALL time_series_push(ts, t, field)
     CALL printit(ts)
  END DO

  CALL time_series_destroy(ts)

  CALL time_series_module_done()


END PROGRAM tstest4
