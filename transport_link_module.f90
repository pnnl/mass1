  ! ----------------------------------------------------------------
  ! file: transport_link_module.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created February 18, 2019 by William A. Perkins
  ! Last Change: 2019-04-01 12:29:02 d3g096
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE transport_link_module
!
! This module contains a (logically abstract) link class implements a
! TVD transport scheme.  Links types that actually do transport
! (fluvial) should inherit from this.
! ----------------------------------------------------------------
MODULE transport_link_module

  USE utility
  USE link_module
  USE bc_module
  USE linear_link_module
  USE scalar_module
  USE met_zone
  USE transport_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE transport_link_t
  !
  ! This is a (logically abstract) link class implements a TVD
  ! transport scheme.  Links types that actually do transport
  ! (fluvial) should inherit from this.
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: transport_link_t
     ! transport cell lengths
     DOUBLE PRECISION, DIMENSION(:), POINTER :: dxx
     ! an array to hold concentration during transport calculations
     DOUBLE PRECISION, DIMENSION(:), POINTER :: c
     ! an array to hold part of the advection calculation
     DOUBLE PRECISION, DIMENSION(:), POINTER :: f
   CONTAINS
     PROCEDURE :: initialize => transport_link_initialize
     PROCEDURE :: advection => transport_link_advection
     PROCEDURE :: diffusion => transport_link_diffusion
     PROCEDURE :: source => transport_link_source
     PROCEDURE :: transport => transport_link_transport
     PROCEDURE :: destroy => transport_link_destroy
  END type transport_link_t


CONTAINS


  ! ----------------------------------------------------------------
  !  FUNCTION transport_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION transport_link_initialize(this, ldata, bcman, sclrman, metman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (transport_link_t), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman

    ierr = this%linear_link_t%initialize(ldata, bcman, sclrman, metman)
    NULLIFY(this%c)
    NULLIFY(this%dxx)
    NULLIFY(this%f)
  END FUNCTION transport_link_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_link_advection
  ! ----------------------------------------------------------------
  SUBROUTINE transport_link_advection(this, ispec, deltat)

    IMPLICIT NONE
    CLASS (transport_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(IN) :: deltat

    DOUBLE PRECISION :: velave, ave_vel, cflx, s, corr, tmp, phi, dtdx
    DOUBLE PRECISION :: a, aold
    INTEGER :: i

    DO i = 1, this%npoints - 1

       IF (i .EQ. this%npoints) THEN
          velave = this%pt(i)%trans%hold%q
          ave_vel = this%pt(i)%trans%hnow%v
       ELSE
          velave = 0.5*(this%pt(i)%trans%hold%q + this%pt(i+1)%trans%hold%q)
          ave_vel = 0.5*(this%pt(i)%trans%hnow%v + this%pt(i+1)%trans%hnow%v)
       END IF

       cflx = ave_vel*deltat/this%dxx(i)

       IF (velave .GE. 0.0) THEN
          this%f(i) = this%c(i+1) - this%c(i)
          IF (ABS(this%f(i)) .GT. 1.0D-40) THEN
             corr = 0.5*(this%dxx(i+1) + this%dxx(i))/ &
                  &(0.5*(this%dxx(i) + this%dxx(i-1)))
             s = corr*(this%c(i) - this%c(i-1))/this%f(i)
             tmp = MIN(&
                  &2.0D+00, &
                  &2.0D+00*s, &
                  &0.33333333333333333d0*(2.0D+00 - cflx+(1.0D+00 + cflx)*s)&
                  &)
             phi = MAX(0.0D+00, tmp)
          ELSE
             phi = 0.0
          END IF
          this%f(i) = velave*(this%c(i) + 0.5*(1.0 - cflx)*phi*this%f(i))
       ELSE
          this%f(i) = velave*(this%c(i) - this%c(i+1))
          IF (ABS(this%f(i)) .GT. 1.0D-40) THEN
             corr = 0.5*(this%dxx(i+1)+this%dxx(i))/&
                  &(0.5*(this%dxx(i+1)+this%dxx(i+2))) 
             s = corr*(this%c(i+1) - this%c(i+2))/this%f(i)
             tmp = MIN(&
                  &2.0D+00, &
                  &2.0D+00*s, &
                  &0.33333333333333333d0*(2.0D+00 - cflx+(1.0D+00 + cflx)*s)&
                  &)
             phi = MAX(0.0D+00, tmp)
          ELSE
             phi = 0.0
          END IF
       END IF
    END DO

    DO i = 2, this%npoints - 1
       dtdx = deltat/this%dxx(i)
       a = this%pt(i)%trans%xsprop%area
       aold = this%pt(i)%trans%xspropold%area
       this%c(i) = this%c(i)*aold/a - dtdx*(this%f(i) - this%f(i-1))/a
    END DO

  END SUBROUTINE transport_link_advection


  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_link_diffusion
  ! ----------------------------------------------------------------
  SUBROUTINE transport_link_diffusion(this, ispec, deltat)

    IMPLICIT NONE
    CLASS (transport_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(IN) :: deltat

    DOUBLE PRECISION :: k_e, k_w
    DOUBLE PRECISION :: area_e, area_w
    DOUBLE PRECISION :: flux_e, flux_w
    DOUBLE PRECISION :: dtdx

    INTEGER :: i
    DOUBLE PRECISION :: c

    DO i = 2, this%npoints - 1
       ASSOCIATE (&
            &pt0 => this%pt(i), &
            &ptp1 => this%pt(i+1), &
            &ptm1 => this%pt(i-1))
         IF (pt0%trans%xspropold%area .GT. 0.0) THEN
            k_e = 0.5*(ptp1%k_diff + pt0%k_diff)
            k_w = 0.5*(pt0%k_diff + ptm1%k_diff)
            area_e = 0.5*(ptp1%trans%xspropold%area + pt0%trans%xspropold%area)
            area_w = 0.5*(pt0%trans%xspropold%area + ptm1%trans%xspropold%area)
            flux_e = k_e*area_e*(ptp1%trans%cnow(ispec) - pt0%trans%cnow(ispec))/&
                 &(ABS(ptp1%x - pt0%x))
            flux_w = k_w*area_w*(pt0%trans%cnow(ispec) - ptm1%trans%cnow(ispec))/&
                 &(ABS(pt0%x - ptm1%x))
            dtdx = deltat/this%dxx(i)
            c = pt0%trans%cnow(ispec)
            c = c + dtdx*(flux_e - flux_w)/pt0%trans%xspropold%area
            this%c(i) = c
         END IF
       END ASSOCIATE
    END DO

  END SUBROUTINE transport_link_diffusion


  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_link_source
  ! ----------------------------------------------------------------
  SUBROUTINE transport_link_source(this, ispec, tdeltat, met)

    IMPLICIT NONE
    CLASS (transport_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(IN) :: tdeltat
    TYPE (met_zone_t), INTENT(INOUT) :: met

    INTEGER :: i

    DOUBLE PRECISION :: c, latc

    DO i = 2, this%npoints
       c = this%c(i) 

       IF (ASSOCIATED(this%species(ispec)%latbc)) THEN
          latc = this%species(ispec)%latbc%current_value
       ELSE
          latc = c
       END IF

       c = this%species(ispec)%scalar%source(&
            &c, this%pt(i)%trans, latc, tdeltat, met)
       this%pt(i)%trans%cnow(ispec) = c
    END DO

  END SUBROUTINE transport_link_source


  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_link_transport
  ! ----------------------------------------------------------------
  SUBROUTINE transport_link_transport(this, ispec, tdeltat)

    IMPLICIT NONE
    CLASS (transport_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(IN) :: tdeltat

    INTEGER :: n, i
    CHARACTER (LEN=1024) :: msg
    DOUBLE PRECISION :: c

    IF (.NOT. ASSOCIATED(this%c)) THEN
       ALLOCATE(this%c(0:this%npoints + 2))
    END IF
    IF (.NOT. ASSOCIATED(this%f)) THEN
       ALLOCATE(this%f(this%npoints))
    END IF
    IF (.NOT. ASSOCIATED(this%dxx)) THEN
       ALLOCATE(this%dxx(0:this%npoints+2))

       ! Cell lengths used for transport
    
       this%dxx(1) = ABS(this%pt(2)%x - this%pt(1)%x)
       this%dxx(0) = this%dxx(1)
       DO i = 2, this%npoints - 1
          this%dxx(i) = ABS(0.5*(this%pt(i)%x - this%pt(i-1)%x)) + &
               &ABS(0.5*(this%pt(i+1)%x - this%pt(i)%x))
       END DO
       i = this%npoints
       this%dxx(i) = ABS(this%pt(i)%x - this%pt(i-1)%x)
       this%dxx(i+1) = this%dxx(i)
       this%dxx(i+2) = this%dxx(i)
    END IF
    
    DO i = 1, this%npoints
       this%pt(i)%trans%cold(ispec) = this%pt(i)%trans%cnow(ispec)
    END DO

    ! FIXME: boundary conditions
    IF (this%q_up() .GT. 0.0) THEN
       c = 0.0
       IF (ASSOCIATED(this%species(ispec)%usbc)) THEN
          c = this%species(ispec)%getusbc()
       ELSE IF (ASSOCIATED(this%ucon)) THEN
          c = this%ucon%conc(ispec)
       ELSE 
          WRITE(msg, *) 'link ', this%id, &
               &': error: upstream inflow w/o conc BC for species ', &
               &ispec
          CALL error_message(msg)
       END IF
       this%pt(1)%trans%cnow = c
    END IF
    IF (this%q_down() .LT. 0.0) THEN
       c = 0.0
       IF (ASSOCIATED(this%dcon)) THEN
          c = this%dcon%conc(ispec)
       ELSE
          WRITE(msg, *) 'link ', this%id, &
               &': error: reverse flow at downstream boundary w/o conc BC for species ', &
               &ispec
          CALL error_message(msg)
       END IF
       this%pt(1)%trans%cnow(ispec) = c
    END IF
      
    
    DO i = 1, this%npoints
       this%c(i) = this%pt(i)%trans%cnow(ispec)
    END DO
    this%c(0) = this%c(1)
    this%c(this%npoints+1) = this%c(this%npoints)
    this%c(this%npoints+2) = this%c(this%npoints)

    CALL this%advection(ispec, tdeltat)

    IF (this%species(ispec)%scalar%dodiffusion) THEN
       ! store advection results for diffusion calculations
       DO i = 1, this%npoints
          this%pt(i)%trans%cnow(ispec) = this%c(i)
       END DO
       CALL this%diffusion(ispec, tdeltat)
    END IF

    DO i = 1, this%npoints
       this%pt(i)%trans%cnow(ispec) = this%c(i)
    END DO
    
    CALL this%source(ispec, tdeltat, this%species(ispec)%met)
    
    ! adjust boundary concentrations to have zero concentration
    ! gradient w/ outflow

    n = this%npoints
    IF (this%q_up() .LT. 0.0) THEN
       this%pt(1)%trans%cnow(ispec) = this%pt(2)%trans%cnow(ispec)
    END IF
    IF (this%q_down() .GT. 0.0) THEN
       this%pt(n)%trans%cnow(ispec) = this%pt(n-1)%trans%cnow(ispec)
    END IF
    
  END SUBROUTINE transport_link_transport


  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE transport_link_destroy(this)

    IMPLICIT NONE
    CLASS (transport_link_t), INTENT(INOUT) :: this

    IF (ASSOCIATED(this%dxx)) THEN
       DEALLOCATE(this%dxx)
    END IF

    IF (ASSOCIATED(this%c)) THEN
       DEALLOCATE(this%c)
    END IF

    IF (ASSOCIATED(this%f)) THEN
       DEALLOCATE(this%f)
    END IF

    CALL this%linear_link_t%destroy()

  END SUBROUTINE transport_link_destroy

END MODULE transport_link_module
  
