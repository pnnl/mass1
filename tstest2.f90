! ----------------------------------------------------------------
! file: tstest2.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 24, 2002 by William A. Perkins
! Last Change: 2017-06-22 09:24:38 d3g096
! ----------------------------------------------------------------

PROGRAM tstest

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  
  TYPE (time_series_rec), POINTER :: ts
  DOUBLE PRECISION :: t0, t, tstep
  INTEGER :: i, istep

  CALL time_series_module_init(mode = TS_REAL_MODE, &
       &limit = TS_LIMIT_FLAT, debug = 15)

  ts => time_series_read('tstest2.dat', fields = 2)

  t0 = 10.0*DAYS
  tstep = 6.0*HOURS
  istep = 0
  DO WHILE (.TRUE.)
     t = t0 + istep*tstep
     IF (t .GT. 15.0*DAYS + 0.1*SECONDS) EXIT
     CALL time_series_interp(ts, t)
     WRITE(*,100) t, (ts%current(i), i = 1, ts%fields)
     istep = istep + 1
  END DO
  CALL time_series_module_done()
  
100 FORMAT(3(F10.3, 1X))
END PROGRAM tstest
