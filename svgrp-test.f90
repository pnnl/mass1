! ----------------------------------------------------------------
! file: svgrp-test.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 21, 1998 by William A. Perkins
! Last Change: 2017-06-21 13:06:23 d3g096
! ----------------------------------------------------------------

PROGRAM svgrp_test

  PARAMETER  (N=10)
  DOUBLE PRECISION       RA(N), RB(N)
  INTEGER    IPERM(N)
  ! Set values for  RA and IPERM 
  DATA RA/10.0, -9.0, 8.0, -7.0, 6.0, 5.0, 4.0, -3.0, -2.0, -1.0/
  DATA IPERM/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/

  ! Sort RA by algebraic value into RB

  CALL SVRGP (N, RA, RB, IPERM)

  ! Print results

  WRITE (*, 99998) (RB(J),J=1,N)
  WRITE (*, 99999) (IPERM(J),J=1,N)

99998 FORMAT ('  The output vector is:', /, 10(1X,F5.1))
99999 FORMAT ('  The permutation vector is:', /, 10(1X,I5))
END PROGRAM svgrp_test

