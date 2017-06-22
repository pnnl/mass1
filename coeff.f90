
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

SUBROUTINE fluvial_coeff(a,b,c,d,g,ap,bp,cp,dp,gp,dx,dt,gr,latq_old,latq_new,lpiexp)

  USE fluvial_coeffs
  USE general_vars

  IMPLICIT NONE

  DOUBLE PRECISION :: a,b,c,d,g,ap,bp,cp,dp,gp
  DOUBLE PRECISION :: gp1,gp2,gp3,gp4
  DOUBLE PRECISION :: dx,gr
  DOUBLE PRECISION :: latq_old,latq_new
  DOUBLE PRECISION, INTENT(IN) :: lpiexp
  DOUBLE PRECISION :: dt
  DOUBLE PRECISION :: sigma, fravg

  ! Normal fluvial link

  ! continuity equation coefficients      with lateral inflow     (flow per length of channel)

  a = b2/2.0/dt
  b = theta/dx
  c = -b1/2.0/dt
  d = b
  g = (q1 - q2)/dx + theta*latq_new + (1 - theta)*latq_old

  !--------------------------------------------------------
  ! momemtum equation coefficients

  sigma = 1.0

  ! check for trans-critical flow and adjust inertial terms according
  ! to the LPI method of Fread, Jin, and Lewis, 1996

  IF (lpiexp .GE. 1.0) THEN
     fravg = 0.5*(fr1 + fr2)
     IF (fravg .LE. 1.0) THEN
        sigma = 1.0 - fravg**lpiexp
     ELSE 
        sigma = 0.0
     END IF
  END IF

  ! if the depth is small, completely turn off the inertial terms and
  ! change the friction weighting
  IF ( (d1 .LT. depth_threshold) .OR. (d2 .LT. depth_threshold)) THEN
     sigma = 0.0
     beta = 1.0
  END IF


  ap = alpha*theta*sigma*(-q2*b2*(q2-q1)/dx/a2**2           &
       + b2*(q2/a2**2)*(a2-a1)*(q2/a2+q1/a1)/dx/2.0   &
       - b2*(q2/a2 + q1/a1)**2/dx/4.0)                            &
       + gr*theta*(b2*(y2-y1)/dx/2.0 + (a2+a1)/2.0/dx   &
       + b2*(beta*q1*abs(q1)/k1**2 + (1.0 - beta)*q2    &
       *abs(q2)/k2**2)/2.0                                                              &
       -(1.0 - beta)*q2*abs(q2)*(ky2/k2**3)*(a2+a1))
  
  cp = alpha*theta*sigma*(b1*q1*(q2-q1)/dx/a1**2       &
       - b1*(q1/a1**2)*(a2-a1)*(q2/a2+q1/a1)/dx/2.0  &
       - b1*(q2/a2+q1/a1)**2/dx/4.0)                             &
       + gr*theta*(-b1*(y2-y1)/dx/2.0 + (a2+a1)/2.0/dx  &
       - b1*(beta*q1*abs(q1)/k1**2+(1.0-beta)*q2                &
       *abs(q2)/k2**2)/2.0                                                              &
       + beta*q1*abs(q1)*(ky1/k1**3)*(a2+a1))

  bp = sigma*1.0/2.0/dt+alpha*theta*sigma*(2.0*q2/a2+q1*(1.0/a1-1.0/a2) &
       -0.5*(1.0-a1/a2)*(q2/a2+q1/a1))/dx                                              &
       +gr*theta*(1.0- beta)*abs(q2)*(a2+a1)/k2**2
  
  dp = -sigma*1.0/2.0/dt-alpha*theta*sigma*(-2.0*q1/a1+q2*(1.0/a1-1.0/a2) &
       -0.5*(a2/a1-1.0)*(q2/a2 + q1/a1))/dx                                      &
       -gr*theta*beta*(a2+a1)*abs(q1)/k1**2
  
  gp1 = -alpha*sigma*(q2/a2+q1/a1)*(q2-q1)/dx                      
  gp2 = alpha*sigma*((q2/a2+q1/a1)**2)*(a2-a1)/dx/4.0
  gp3 = -gr*(a2+a1)*(y2-y1)/2.0/dx
  gp4 = -gr*(a2+a1)*(beta*q1*abs(q1)/k1**2+(1.0-beta)*q2  &
       *abs(q2)/k2**2)/2.0
  gp = gp1+gp2+gp3+gp4

END SUBROUTINE fluvial_coeff
