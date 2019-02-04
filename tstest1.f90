! ----------------------------------------------------------------
! file: tstest.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 16, 2002 by William A. Perkins
! Last Change: 2017-06-22 09:24:31 d3g096
! ----------------------------------------------------------------

PROGRAM tstest

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  
  TYPE (time_series_rec), POINTER :: ts
  CHARACTER (LEN=1024) :: dstr
  DOUBLE PRECISION :: t, deltat = 15.0d0*SECONDS
  INTEGER :: i, steps, step

  CALL date_time_flags(sdigits=3)
  CALL time_series_module_init(debug=10, limit = TS_LIMIT_NONE)

  ts => time_series_read('tstest1.dat', fields = 10)

  t = ts%series(ts%length)%time - ts%series(1)%time
  steps = INT(t/deltat + 0.5)

  DO step = 0, steps
     t = ts%series(1)%time + step*deltat
     CALL time_series_interp(ts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,'(A, 1X, 15F17.8)') TRIM(dstr), t, (ts%current(i), i = 1, ts%fields)
  END DO
  CALL time_series_module_done()
  

END PROGRAM tstest
