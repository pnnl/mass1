! ----------------------------------------------------------------
! file: fluvial_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July  3, 2017 by William A. Perkins
! Last Change: 2017-07-21 13:41:01 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE fluvial_link_module
! ----------------------------------------------------------------
MODULE fluvial_link_module

  USE bc_module
  USE point_module
  USE link_module
  USE linear_link_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  PRIVATE

  TYPE, PUBLIC, EXTENDS(linear_link_t) :: fluvial_link
     TYPE (bc_ptr) :: latbc
     DOUBLE PRECISION :: latq, latqold
     DOUBLE PRECISION :: lpiexp
   CONTAINS
     PROCEDURE :: initialize => fluvial_link_initialize
     PROCEDURE :: coeff => fluvial_link_coeff
     PROCEDURE :: hydro_update => fluvial_link_hupdate
  END type fluvial_link

  DOUBLE PRECISION, PARAMETER :: alpha = 1.0
  DOUBLE PRECISION, PARAMETER :: theta = 1.0

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION fluvial_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION fluvial_link_initialize(this, ldata, bcman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (fluvial_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CHARACTER (LEN=1024) :: msg

    ierr = this%linear_link_t%initialize(ldata, bcman)

  END FUNCTION fluvial_link_initialize


  ! ----------------------------------------------------------------
  ! SUBROUTINE fluvial_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE fluvial_link_coeff(this, dt, pt1, pt2, c, cp)

    IMPLICIT NONE
    CLASS (fluvial_link), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: c, cp

    DOUBLE PRECISION :: y1, y2, d1, d2, q1, q2, a1, a2, b1, b2, k1, k2, ky1, ky2
    DOUBLE PRECISION :: fr1, fr2
    DOUBLE PRECISION :: dx
    DOUBLE PRECISION :: beta, fravg, gp1, gp2, gp3, gp4, sigma

    ! FIXME: These need to come from the configuration:
    DOUBLE PRECISION :: gr, depth_threshold

    CALL pt1%assign(y1, d1, q1, a1, b1, k1, ky1, fr1)
    CALL pt2%assign(y2, d2, q2, a2, b2, k2, ky2, fr2)

    dx = ABS(pt1%x - pt2%x)

    c%a = b2/2.0/dt
    c%b = theta/dx
    c%c = -b1/2.0/dt
    c%d = c%b
    c%g = (q1 - q2)/dx + theta*this%latq + (1 - theta)*this%latqold

    !--------------------------------------------------------
    ! momemtum equation coefficients

    sigma = 1.0

    ! check for trans-critical flow and adjust inertial terms according
    ! to the LPI method of Fread, Jin, and Lewis, 1996

    IF (this%lpiexp .GE. 1.0) THEN
       fravg = 0.5*(fr1 + fr2)
       IF (fravg .LE. 1.0) THEN
          sigma = 1.0 - fravg**this%lpiexp
       ELSE 
          sigma = 0.0
       END IF
    END IF

    ! if the depth is small, completely turn off the inertial terms and
    ! change the friction weighting
    IF ( (d1 .LT. depth_threshold) .OR. (d2 .LT. depth_threshold)) THEN
       sigma = 0.0
       beta = 1.0
    ELSE 
       beta = 0.5
    END IF


    cp%a = alpha*theta*sigma*(-q2*b2*(q2-q1)/dx/a2**2           &
         + b2*(q2/a2**2)*(a2-a1)*(q2/a2+q1/a1)/dx/2.0   &
         - b2*(q2/a2 + q1/a1)**2/dx/4.0)                            &
         + gr*theta*(b2*(y2-y1)/dx/2.0 + (a2+a1)/2.0/dx   &
         + b2*(beta*q1*abs(q1)/k1**2 + (1.0 - beta)*q2    &
         *abs(q2)/k2**2)/2.0                                                              &
         -(1.0 - beta)*q2*abs(q2)*(ky2/k2**3)*(a2+a1))

    cp%c = alpha*theta*sigma*(b1*q1*(q2-q1)/dx/a1**2       &
         - b1*(q1/a1**2)*(a2-a1)*(q2/a2+q1/a1)/dx/2.0  &
         - b1*(q2/a2+q1/a1)**2/dx/4.0)                             &
         + gr*theta*(-b1*(y2-y1)/dx/2.0 + (a2+a1)/2.0/dx  &
         - b1*(beta*q1*abs(q1)/k1**2+(1.0-beta)*q2                &
         *abs(q2)/k2**2)/2.0                                                              &
         + beta*q1*abs(q1)*(ky1/k1**3)*(a2+a1))

    cp%b = sigma*1.0/2.0/dt+alpha*theta*sigma*(2.0*q2/a2+q1*(1.0/a1-1.0/a2) &
         -0.5*(1.0-a1/a2)*(q2/a2+q1/a1))/dx                                              &
         +gr*theta*(1.0- beta)*abs(q2)*(a2+a1)/k2**2

    cp%d = -sigma*1.0/2.0/dt-alpha*theta*sigma*(-2.0*q1/a1+q2*(1.0/a1-1.0/a2) &
         -0.5*(a2/a1-1.0)*(q2/a2 + q1/a1))/dx                                      &
         -gr*theta*beta*(a2+a1)*abs(q1)/k1**2

    gp1 = -alpha*sigma*(q2/a2+q1/a1)*(q2-q1)/dx                      
    gp2 = alpha*sigma*((q2/a2+q1/a1)**2)*(a2-a1)/dx/4.0
    gp3 = -gr*(a2+a1)*(y2-y1)/2.0/dx
    gp4 = -gr*(a2+a1)*(beta*q1*abs(q1)/k1**2+(1.0-beta)*q2  &
         *abs(q2)/k2**2)/2.0
    cp%g = gp1+gp2+gp3+gp4

  END SUBROUTINE fluvial_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE fluvial_link_hupdate
  ! ----------------------------------------------------------------
  SUBROUTINE fluvial_link_hupdate(this, res_coeff)

    IMPLICIT NONE
    CLASS (fluvial_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: res_coeff

    ! do depth check here

    CALL this%linear_link_t%hydro_update(res_coeff)

  END SUBROUTINE fluvial_link_hupdate

END MODULE fluvial_link_module
