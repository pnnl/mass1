! ----------------------------------------------------------------
! file: cumtstest1.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 28, 2013 by William A. Perkins
! Last Change: 2017-06-22 09:22:07 d3g096
! ----------------------------------------------------------------
PROGRAM cumtstest

  USE cumulative_time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id: tstest1.f90 18 2003-04-14 17:48:14Z perk $"
  
  TYPE (cumulative_time_series_rec), POINTER :: ts
  CHARACTER (LEN=1024) :: dstr
  DOUBLE PRECISION :: t, deltat = 1.0d0*HOURS
  INTEGER :: i, steps, step

  CALL date_time_flags()
  CALL time_series_module_init(debug=10, limit = TS_LIMIT_NONE)

  ts => cumulative_time_series_read('cumtstest1.dat')

  t = ts%ts%series(ts%ts%length)%time - ts%ts%series(1)%time
  steps = INT(t/deltat + 0.5)

  DO step = 0, steps
     t = ts%ts%series(1)%time + step*deltat
     CALL cumulative_time_series_update(ts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,'(A, 1X, 15F17.8)') TRIM(dstr), t, ts%current_value, ts%rate
  END DO
  CALL time_series_module_done()

END PROGRAM cumtstest
