
!***************************************************************
!            Pacific Northwest National Laboratory
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

REAL FUNCTION linear_interp(x0,y0,x1,y1,y)

! returns x value
! given a y value and end points of a line
! origin is at x0,y0

! y - y0 = slope*(x - x0)

REAL :: x0,y0,x1,y1,slope,y

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