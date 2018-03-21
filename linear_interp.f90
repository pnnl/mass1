
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: linear_interp
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: does a linear interpolation. called from table_iterp
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

DOUBLE PRECISION FUNCTION linear_interp(x0,y0,x1,y1,y)

  ! returns x value
  ! given a y value and end points of a line
  ! origin is at x0,y0

  ! y - y0 = slope*(x - x0)

  DOUBLE PRECISION :: x0,y0,x1,y1,slope,y

  ! check for a vertical line
  IF(x0 == x1)THEN
     linear_interp = x0
  ELSE
     slope = (y1 - y0)/(x1 - x0)
     IF(slope /= 0.0)THEN
        linear_interp = x0 + (y - y0)/slope
     ELSE
        linear_interp = x0
     ENDIF
  ENDIF
END FUNCTION linear_interp


DOUBLE PRECISION FUNCTION dlinear_interp(x0,y0,x1,y1,y)

  ! returns x value
  ! given a y value and end points of a line
  ! origin is at x0,y0

  ! y - y0 = slope*(x - x0)

  DOUBLE PRECISION :: x0,y0,x1,y1,slope,y
  DOUBLE PRECISION :: dx, rdx, xtest
  DOUBLE PRECISION, PARAMETER :: eps = 1.0D-100

  dx = x1 - x0

  IF (ABS(dx) .LT. eps) dx = 0.0

  xtest = MAX(ABS(x1), ABS(x0))
  IF (xtest .GT. eps) THEN
     rdx = ABS(dx/xtest)
  ELSE
     rdx = 0.0
  END IF
  
  ! the relative difference of x1 and x0 is 10 orders of magnitude
  ! lower than x1 and x0 themselves; let's just make that zero

  IF (rdx .LT. 1.0e-10) THEN
     dlinear_interp = x0
  ELSE
     slope = (y1 - y0)/dx
     IF(slope /= 0.0)THEN
        dlinear_interp = x0 + (y - y0)/slope
     ELSE
        dlinear_interp = x0
     ENDIF
  ENDIF
END FUNCTION dlinear_interp
