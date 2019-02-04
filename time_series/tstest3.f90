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
! Last Change: 2017-06-22 09:24:44 d3g096
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

PROGRAM tstest3

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  
  TYPE (time_series_rec), POINTER :: ts, ts2
  CHARACTER (LEN=1024) :: dstr
  DOUBLE PRECISION :: t, deltat = 0.5d0*HOURS, start
  INTEGER :: i, steps, step
  LOGICAL :: flag

  CALL date_time_flags()
  CALL time_series_module_init(debug=10, limit = TS_LIMIT_NONE)

  ts => time_series_read('tstest3a.dat', fields = 2)
  ts%limit_mode = TS_LIMIT_FLAT
  
  flag = time_series_increases(ts, field = 1, fix = .TRUE.)
  flag = time_series_increases(ts, field = 2, fix = .TRUE.)

  start = ts%series(1)%time - 0.5
  steps = INT((ts%series(ts%length)%time+0.5 - start)/deltat + 0.5)

  DO step = 0, steps
     t = start + step*deltat
     CALL time_series_interp(ts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,'(A, 1X, 15F17.8)') TRIM(dstr), t, (ts%current(i), i = 1, ts%fields)
  END DO

  CALL time_series_destroy(ts)

  ts => time_series_read('tstest3b.dat', fields = 2)
  ts%limit_mode = TS_LIMIT_FLAT
  DO step = 0, steps
     t = start + step*deltat
     CALL time_series_interp(ts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,'(A, 1X, 15F17.8)') TRIM(dstr), t, (ts%current(i), i = 1, ts%fields)
  END DO

  CALL time_series_destroy(ts)

                                ! this should crash the program

  ts => time_series_read('tstest3b.dat', fields = 2)
  CALL time_series_interp(ts, t)

  CALL time_series_module_done()


END PROGRAM tstest3
