
! ----------------------------------------------------------------
! file: flow_coeff.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  8, 2018 by William A. Perkins
! Last Change: 2018-01-08 10:54:08 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE flow_coeff
! ----------------------------------------------------------------
MODULE flow_coeff

  IMPLICIT NONE

  TYPE, PUBLIC :: coeff
     DOUBLE PRECISION :: a, b, c, d, g
     DOUBLE PRECISION :: ap, bp, cp, dp, gp
  END TYPE coeff

CONTAINS

END MODULE flow_coeff
