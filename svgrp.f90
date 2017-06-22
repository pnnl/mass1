! ----------------------------------------------------------------
! file: svgrp.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created November 17, 1998 by William A. Perkins
! Last Change: 2017-06-21 14:44:57 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! SUBROUTINE SVRGP
! 
! ----------------------------------------------------------------
SUBROUTINE SVRGP(n, xin, xout, iperm)

  INTEGER :: n
  DOUBLE PRECISION :: xin(n), xout(n)
  INTEGER :: iperm(n)
  DOUBLE PRECISION :: xtmp
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
