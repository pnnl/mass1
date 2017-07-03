
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
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

  USE mass1_config
  USE cross_section
  USE section_handler_module
  USE point_vars
  USE link_vars, ONLY : linktype, crest
  USE pidlink

  IMPLICIT NONE

  DOUBLE PRECISION :: a,b,c,d,g,ap,bp,cp,dp,gp,bcval, p
  DOUBLE PRECISION :: eps = 1.0e-9
  INTEGER :: link,point
  DOUBLE PRECISION :: twid, cw, hwmin, ycrest, oldycrest
  DOUBLE PRECISION :: maxtravel, sensitivity
  TYPE (xsection_prop) :: props
  
  maxtravel = 20.0*config%time%delta_t/3600.0
  sensitivity = 0.01

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

     ! Imposed Discharge w/ PID process control
  CASE(12)
     CALL pidlink_coeff(link, point, bcval, a, b, c, d, g, ap, bp, cp, dp, gp)
     
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
     
     ! Imposed Stage w/ PID process control
  CASE(13)
     CALL pidlink_coeff(link, point, bcval, a, b, c, d, g, ap, bp, cp, dp, gp)
     
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

  CASE (7)
     ! a special sharp-crested weir: assumes a length equal to the
     ! topwitdth, assumes no submergence, no reversal, allows crest
     ! height to vary with time
     
     ! use the section top width as the crest length, call section to
     ! get it (throw everything else away
   
     d = y(link,point) - thalweg(link,point)

     CALL ptsection(link, point)%p%props(d, props)

     twid = props%topwidth
     
     a = 0.0
     b = 1.0
     c = 0.0
     d = 1.0
     g = q(link,point) - q(link,point+1)
     
     
     ap = 0.0
     bp = 0.0
     dp = 1.0
     
     SELECT CASE(config%units)
     CASE (ENGLISH_UNITS)
        cw = 3.33
        hwmin = 1.0e-05
     CASE (METRIC_UNITS)
        cw = 1.85
        hwmin = 1.0e-05
     END SELECT
     
     IF (q(link, point) .GT. 0.0) THEN
        ycrest = bcval - ((q(link, point))/cw/twid)**(2.0/3.0)
     ELSE
        ycrest = bcval
     END IF
     
     IF (crest(link) .LE. -999.0) THEN 
        oldycrest = ycrest
     ELSE
        oldycrest = crest(link)
     END IF
     
     IF (ABS(y(link, point) - bcval) .LE. sensitivity) THEN
        ycrest = oldycrest
     ELSE
        
        ! limit to a maximum rate of crest travel
        
        IF (ABS(ycrest - oldycrest) > maxtravel) THEN
           IF (ycrest > oldycrest) THEN
              ycrest = oldycrest + maxtravel
           ELSE 
              ycrest = oldycrest - maxtravel
           END IF
        END IF
        
     END IF

     WRITE (1, '(I5,6F10.1)') link, y(link,point), q(link,point), q_old(link,point), &
          &bcval, ycrest, oldycrest

     ! ycrest = 0.5*(ycrest + oldycrest)
     
     IF (y(link,point) > ycrest) THEN
        gp = cw*twid*(y(link,point) - ycrest)**(1.5)
        cp = -1.5*cw*twid*SQRT(y(link,point) - ycrest)
     ELSE
        gp = cw*twid*(hwmin)**(1.5)
        cp = -1.5*cw*twid*SQRT(hwmin)
     ENDIF
     gp = q(link,point) - gp
     
     
     crest(link) = ycrest
  END SELECT

END SUBROUTINE nonfluvial_coeff
