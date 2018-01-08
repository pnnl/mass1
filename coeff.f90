
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: fluvial_coeff
!
! VERSION and DATE: MASS1 v0.75 3/25/98
!
! PURPOSE: computes the coefficients in the flow solution
!          difference equations. double sweep method
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
! MOD HISTORY: changed g coeff in continuit for lateral inflow; mcr 3/25/98
!
!
!***************************************************************
!
!----------------------------------------------------------
MODULE fluvial_coeffs

  USE flow_coeff

  TYPE, PUBLIC :: fluvial_state
     DOUBLE PRECISION :: d, y, q, a, b, k, ky, fr
  END TYPE fluvial_state

  DOUBLE PRECISION, PRIVATE, SAVE :: alpha=1.0,beta=0.5,theta=1.0

CONTAINS

  SUBROUTINE fluvial_coeff(s1,s2,cf,dx,dt,gr,latq_old,latq_new,&
       &lpiexp, depth_threshold)

    IMPLICIT NONE

    TYPE (fluvial_state), INTENT(IN) :: s1, s2
    TYPE (coeff), INTENT(OUT) :: cf
    DOUBLE PRECISION, INTENT(IN) :: latq_old,latq_new
    DOUBLE PRECISION, INTENT(IN) :: dx, dt, gr
    DOUBLE PRECISION, INTENT(IN) :: lpiexp, depth_threshold

    DOUBLE PRECISION :: gp1,gp2,gp3,gp4
    DOUBLE PRECISION :: sigma, fravg

    ! Normal fluvial link

    ! continuity equation coefficients      with lateral inflow     (flow per length of channel)

    cf%a = s2%b/2.0/dt
    cf%b = theta/dx
    cf%c = -s1%b/2.0/dt
    cf%d = cf%b
    cf%g = (s1%q - s2%q)/dx + theta*latq_new + (1 - theta)*latq_old

    !--------------------------------------------------------
    ! momemtum equation coefficients

    sigma = 1.0

    ! check for trans-critical flow and adjust inertial terms according
    ! to the LPI method of Fread, Jin, and Lewis, 1996

    IF (lpiexp .GE. 1.0) THEN
       fravg = 0.5*(s1%fr + s2%fr)
       IF (fravg .LE. 1.0) THEN
          sigma = 1.0 - fravg**lpiexp
       ELSE 
          sigma = 0.0
       END IF
    END IF

    ! if the depth is small, completely turn off the inertial terms and
    ! change the friction weighting
    IF ( (s1%d .LT. depth_threshold) .OR. (s2%d .LT. depth_threshold)) THEN
       sigma = 0.0
       beta = 1.0
    ELSE 
       beta = 0.5
    END IF


    cf%ap = alpha*theta*sigma*(-s2%q*s2%b*(s2%q-s1%q)/dx/s2%a**2           &
         + s2%b*(s2%q/s2%a**2)*(s2%a-s1%a)*(s2%q/s2%a+s1%q/s1%a)/dx/2.0   &
         - s2%b*(s2%q/s2%a + s1%q/s1%a)**2/dx/4.0)                            &
         + gr*theta*(s2%b*(s2%y-s1%y)/dx/2.0 + (s2%a+s1%a)/2.0/dx   &
         + s2%b*(beta*s1%q*abs(s1%q)/s1%k**2 + (1.0 - beta)*s2%q    &
         *abs(s2%q)/s2%k**2)/2.0                                                              &
         -(1.0 - beta)*s2%q*abs(s2%q)*(s2%ky/s2%k**3)*(s2%a+s1%a))

    cf%cp = alpha*theta*sigma*(s1%b*s1%q*(s2%q-s1%q)/dx/s1%a**2       &
         - s1%b*(s1%q/s1%a**2)*(s2%a-s1%a)*(s2%q/s2%a+s1%q/s1%a)/dx/2.0  &
         - s1%b*(s2%q/s2%a+s1%q/s1%a)**2/dx/4.0)                             &
         + gr*theta*(-s1%b*(s2%y-s1%y)/dx/2.0 + (s2%a+s1%a)/2.0/dx  &
         - s1%b*(beta*s1%q*abs(s1%q)/s1%k**2+(1.0-beta)*s2%q                &
         *abs(s2%q)/s2%k**2)/2.0                                                              &
         + beta*s1%q*abs(s1%q)*(s1%ky/s1%k**3)*(s2%a+s1%a))

    cf%bp = sigma*1.0/2.0/dt+alpha*theta*sigma*(2.0*s2%q/s2%a+s1%q*(1.0/s1%a-1.0/s2%a) &
         -0.5*(1.0-s1%a/s2%a)*(s2%q/s2%a+s1%q/s1%a))/dx                                              &
         +gr*theta*(1.0- beta)*abs(s2%q)*(s2%a+s1%a)/s2%k**2

    cf%dp = -sigma*1.0/2.0/dt-alpha*theta*sigma*(-2.0*s1%q/s1%a+s2%q*(1.0/s1%a-1.0/s2%a) &
         -0.5*(s2%a/s1%a-1.0)*(s2%q/s2%a + s1%q/s1%a))/dx                                      &
         -gr*theta*beta*(s2%a+s1%a)*abs(s1%q)/s1%k**2

    gp1 = -alpha*sigma*(s2%q/s2%a+s1%q/s1%a)*(s2%q-s1%q)/dx                      
    gp2 = alpha*sigma*((s2%q/s2%a+s1%q/s1%a)**2)*(s2%a-s1%a)/dx/4.0
    gp3 = -gr*(s2%a+s1%a)*(s2%y-s1%y)/2.0/dx
    gp4 = -gr*(s2%a+s1%a)*(beta*s1%q*abs(s1%q)/s1%k**2+(1.0-beta)*s2%q  &
         *abs(s2%q)/s2%k**2)/2.0
    cf%gp = gp1+gp2+gp3+gp4

  END SUBROUTINE fluvial_coeff
END MODULE fluvial_coeffs



