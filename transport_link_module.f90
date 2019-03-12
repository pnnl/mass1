  ! ----------------------------------------------------------------
  ! file: transport_link_module.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created February 18, 2019 by William A. Perkins
  ! Last Change: 2019-03-12 06:40:22 d3g096
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE transport_link_module
!
! This module contains a (logically abstract) link class implements a
! TVD transport scheme.  Links types that actually do transport
! (fluvial) should inherit from htis.
! ----------------------------------------------------------------
MODULE transport_link_module

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
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: transport_link_t
   CONTAINS
     PROCEDURE :: initialize => transport_link_initialize
     PROCEDURE :: advection => transport_link_advection
     PROCEDURE :: diffusion => transport_link_diffusion
     PROCEDURE :: source => transport_link_source
     PROCEDURE :: transport => transport_link_transport
  END type transport_link_t


CONTAINS


  ! ----------------------------------------------------------------
  !  FUNCTION transport_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION transport_link_initialize(this, ldata, bcman, sclrman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (transport_link_t), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman

    ierr = this%linear_link_t%initialize(ldata, bcman, sclrman)

  END FUNCTION transport_link_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE transport_link_advection
  ! ----------------------------------------------------------------
  SUBROUTINE transport_link_advection(this, ispec, deltat)

    IMPLICIT NONE
    CLASS (transport_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(IN) :: deltat

    

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

    DO i = 2, this%npoints - 1
       ASSOCIATE (pt0 => this%pt(i), ptp1 => this%pt(i+1), ptm1 => this%pt(i-1), &
            &c_old => this%species(ispec)%cold)
         k_e = 0.5*(ptp1%k_diff + pt0%k_diff)
         k_w = 0.5*(pt0%k_diff + ptm1%k_diff)
         area_e = 0.5*(ptp1%trans%xspropold%area + pt0%trans%xspropold%area)
         area_w = 0.5*(pt0%trans%xspropold%area + ptm1%trans%xspropold%area)
         flux_e = k_e*area_e*(c_old(i+1) - c_old(i))/&
              &(ABS(ptp1%x - pt0%x))
         flux_w = k_w*area_w*(c_old(i) - c_old(i-1))/&
              &(ABS(pt0%x - ptm1%x))
         ! dtdx = 
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

    DOUBLE PRECISION :: latc

    latc = this%species(ispec)%latbc%current_value

    DO i = 2, this%npoints
       ASSOCIATE (sp => this%species(ispec))
         sp%cnow(i) = this%species(ispec)%scalar%source(&
              &sp%cnow(i), this%pt(i)%trans, latc, tdeltat, met)
       END ASSOCIATE
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

    

    CALL this%advection(ispec, tdeltat)
    IF (this%species(ispec)%scalar%dodiffusion) THEN
       CALL this%diffusion(ispec, tdeltat)
    END IF
    IF (this%species(ispec)%scalar%dosource) THEN
       CALL this%source(ispec, tdeltat, this%species(ispec)%met)
    END IF
    ! adjust boundary concentrations

  END SUBROUTINE transport_link_transport


END MODULE transport_link_module
  
