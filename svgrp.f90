! ----------------------------------------------------------------
! file: svgrp.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created November 17, 1998 by William A. Perkins
! Last Change: Mon Dec 21 11:31:34 1998 by William A. Perkins <perk@erebus.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! SUBROUTINE SVRGP
! 
! ----------------------------------------------------------------
SUBROUTINE SVRGP(n, xin, xout, iperm)

  INTEGER :: n
  REAL :: xin(n), xout(n)
  INTEGER :: iperm(n)
  REAL :: xtmp
  INTEGER :: itmp, i, j

  xout(1:n) = xin(1:n)

  DO i = 1, n - 1
     DO j = i + 1, n
        IF (xout(j) .le. xout(i)) THEN
           xtmp = xout(j)
           xout(j) = xout(i)
           xout(i) = xtmp
           itmp = iperm(j)
           iperm(j) = iperm(i)
           iperm(i) = itmp
        END IF
     END DO
  END DO

END SUBROUTINE SVRGP
