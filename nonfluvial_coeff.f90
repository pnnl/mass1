
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

SUBROUTINE nonfluvial_coeff(link,point,bcval,cf)

  USE mass1_config
  USE flow_coeff
  USE fluvial_coeffs
  USE cross_section
  USE section_handler_module
  USE point_vars
  USE link_vars, ONLY : linktype, crest
  USE pidlink

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: link,point
  DOUBLE PRECISION, INTENT(IN) :: bcval
  TYPE (coeff), INTENT(OUT) :: cf
  
  DOUBLE PRECISION :: eps = 1.0e-9
  DOUBLE PRECISION :: twid, cw, hwmin, ycrest, oldycrest
  DOUBLE PRECISION :: maxtravel, sensitivity
  DOUBLE PRECISION :: depth
  TYPE (xsection_prop) :: props
  
  maxtravel = 20.0*config%time%delta_t/3600.0
  sensitivity = 0.01

  SELECT CASE(linktype(link))

     !--------------------------------------------------------------------
     ! Imposed Flow Rate Q(t)
  CASE(2)
     cf%a = 0.0
     cf%b = 1.0
     cf%c = 0.0 !eps
     cf%d = 1.0
     cf%g = q(link,point) - q(link,point+1)
     !g = bcval - q(link,point+1)
     
     cf%ap = 0.0
     cf%bp = 0.0
     cf%cp = eps
     cf%dp = 1.0
     !gp = bcval - q(link,point)
     cf%gp = q(link,point) - bcval

     ! Imposed Discharge w/ PID process control
  CASE(12)
     CALL pidlink_coeff(link, point, bcval, cf)
     
     ! Imposed Upstream Stage Yus(t)
  CASE(3)
     cf%a = 0.0
     cf%b = 1.0
     cf%c = 0.0
     cf%d = 1.0
     cf%g = q(link,point) - q(link,point+1)
     
     cf%ap = 0.0
     cf%bp = 0.0
     cf%cp = 1.0
     cf%dp = eps
     cf%gp = y(link,point) - bcval
     
     ! Imposed Stage w/ PID process control
  CASE(13)
     CALL pidlink_coeff(link, point, bcval, cf)
     
     ! Imposed Downstream Stage Yds(t)
  CASE(4)
     cf%a = 1.0
     cf%b = eps
     cf%c = 0.0
     cf%d = 0.0
     cf%g = bcval - y(link,point+1)
     
     cf%ap = 0.0
     cf%bp = 1.0
     cf%cp = 0.0
     cf%dp = 1.0
     cf%gp = q(link,point) - q(link,point+1)
     

     ! Tributary Inflow at a point Qtrib(t)
  CASE(5)
     cf%a = 0.0
     cf%b = 1.0
     cf%c = 0.0
     cf%d = 1.0
     cf%g = q(link,point) + bcval - q(link,point+1)
     
     cf%ap = 1.0
     cf%bp = 0.0
     cf%cp = 1.0
     cf%dp = 0.0
     cf%gp = y(link,point) - y(link,point+1)

     ! Hydroelectric Powerhouse with Qgen(t) and Qspill(t)
     ! (same as case(2) just add the Q together in flow_sim
  CASE(6)
     cf%a = 0.0
     cf%b = 1.0
     cf%c = 0.0 !eps
     cf%d = 1.0
     cf%g = q(link,point) - q(link,point+1)
     !g = bcval - q(link,point+1)
     
     cf%ap = 0.0
     cf%bp = 0.0
     cf%cp = eps
     cf%dp = 1.0
     !gp = bcval - q(link,point)
     cf%gp = q(link,point) - bcval

  CASE (7)
     ! a special sharp-crested weir: assumes a length equal to the
     ! topwitdth, assumes no submergence, no reversal, allows crest
     ! height to vary with time
     
     ! use the section top width as the crest length, call section to
     ! get it (throw everything else away
   
     depth = y(link,point) - thalweg(link,point)

     CALL ptsection(link, point)%p%props(depth, props)

     twid = props%topwidth
     
     cf%a = 0.0
     cf%b = 1.0
     cf%c = 0.0
     cf%d = 1.0
     cf%g = q(link,point) - q(link,point+1)
     
     
     cf%ap = 0.0
     cf%bp = 0.0
     cf%dp = 1.0
     
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
        cf%gp = cw*twid*(y(link,point) - ycrest)**(1.5)
        cf%cp = -1.5*cw*twid*SQRT(y(link,point) - ycrest)
     ELSE
        cf%gp = cw*twid*(hwmin)**(1.5)
        cf%cp = -1.5*cw*twid*SQRT(hwmin)
     ENDIF
     cf%gp = q(link,point) - cf%gp
     
     
     crest(link) = ycrest
  END SELECT

END SUBROUTINE nonfluvial_coeff
