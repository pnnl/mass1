
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	nonfluvial_coeff
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: sets double-sweep coeffs for special link types
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

SUBROUTINE nonfluvial_coeff(link,point,bcval,a,b,c,d,g,ap,bp,cp,dp,gp)

USE	point_vars
USE link_vars, ONLY : linktype

IMPLICIT NONE

REAL :: a,b,c,d,g,ap,bp,cp,dp,gp,bcval
REAL :: eps = 1.0e-9
INTEGER :: link,point


SELECT CASE(linktype(link))

!--------------------------------------------------------------------
! Imposed Flow Rate Q(t)
CASE(2)
	a = 0.0
	b = 1.0
	c = 0.0 !eps
	d = 1.0
	g = q(link,point) - q(link,point+1)
	!g = bcval - q(link,point+1)

	ap = 0.0
	bp = 0.0
	cp = eps
	dp = 1.0
	!gp = bcval - q(link,point)
	gp = q(link,point) - bcval

! Imposed Upstream Stage Yus(t)
CASE(3)
	a = 0.0
	b = 1.0
	c = 0.0
	d = 1.0
	g = q(link,point) - q(link,point+1)

	ap = 0.0
	bp = 0.0
	cp = 1.0
	dp = eps
	gp = y(link,point) - bcval

! Imposed Downstream Stage Yds(t)
CASE(4)
	a = 1.0
	b = eps
	c = 0.0
	d = 0.0
	g = bcval - y(link,point+1)

	ap = 0.0
	bp = 1.0
	cp = 0.0
	dp = 1.0
	gp = q(link,point) - q(link,point+1)


! Tributary Inflow at a point Qtrib(t)
CASE(5)
	a = 0.0
	b = 1.0
	c = 0.0
	d = 1.0
	g = q(link,point) + bcval - q(link,point+1)

	ap = 1.0
	bp = 0.0
	cp = 1.0
	dp = 0.0
	gp = y(link,point) - y(link,point+1)

! Hydroelectric Powerhouse with Qgen(t) and Qspill(t)
! (same as case(2) just add the Q together in flow_sim
CASE(6)
	a = 0.0
	b = 1.0
	c = 0.0 !eps
	d = 1.0
	g = q(link,point) - q(link,point+1)
	!g = bcval - q(link,point+1)

	ap = 0.0
	bp = 0.0
	cp = eps
	dp = 1.0
	!gp = bcval - q(link,point)
	gp = q(link,point) - bcval



END SELECT

END SUBROUTINE nonfluvial_coeff