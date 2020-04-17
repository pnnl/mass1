! ----------------------------------------------------------------
! file: mettest1.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March  1, 2013 by William A. Perkins
! Last Change: 2020-04-17 07:15:57 d3g096
! ----------------------------------------------------------------
PROGRAM mettest1
  
  USE met_time_series

  IMPLICIT NONE

  TYPE (met_time_series_rec), POINTER :: metts
  CHARACTER (LEN=1024) :: dstr
  DOUBLE PRECISION :: t, deltat = 0.25d0*HOURS
  INTEGER :: i, steps, step

  CALL date_time_flags()
  CALL time_series_module_init(debug=10, limit = TS_LIMIT_NONE)

  metts => met_time_series_read('mettest1.dat')

  t = metts%ts%series(metts%ts%length)%time - metts%ts%series(1)%time
  steps = INT(t/deltat + 0.5)

  DO step = 0, steps
     t = metts%ts%series(1)%time + step*deltat
     CALL met_time_series_update(metts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,'(A, 1X, 15F17.8)') TRIM(dstr), &
          &(metts%current(i), i = 1, met_fields)
  END DO

  CALL met_time_series_destroy(metts)

  CALL time_series_module_done()

END PROGRAM mettest1
