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
! Last Change: 2020-05-07 12:48:08 d3g096
! ----------------------------------------------------------------

PROGRAM tstest

  USE table_module
  

  IMPLICIT NONE

  CLASS (table), POINTER :: tbl
  DOUBLE PRECISION :: t0, t, tstep
  INTEGER :: i, istep

  ALLOCATE (tbl, SOURCE = table('tstest2.dat', fields=2, lmode=TBL_LIMIT_FLAT))

  t0 = 10.0
  tstep = 0.1
  istep = 0
  DO WHILE (.TRUE.)
     t = t0 + istep*tstep
     IF (t .GT. 15.0) EXIT
     CALL tbl%interp(t)
     WRITE(*,100) t, (tbl%current(i), i = 1, tbl%fields)
     istep = istep + 1
  END DO
  CALL tbl%destroy()

  DEALLOCATE(tbl)
  
100 FORMAT(3(F10.3, 1X))
END PROGRAM tstest
