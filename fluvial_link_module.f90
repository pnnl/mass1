! ----------------------------------------------------------------
! file: fluvial_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July  3, 2017 by William A. Perkins
! Last Change: 2017-07-14 13:56:12 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE fluvial_link_module
! ----------------------------------------------------------------
MODULE fluvial_link_module

  USE bc_module
  USE point_module
  USE linear_link_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  PRIVATE

  TYPE, PUBLIC, EXTENDS(linear_link_t) :: fluvial_link
     TYPE (bc_ptr) :: latbc
     DOUBLE PRECISION :: latq, latqold
     DOUBLE PRECISION :: lpiexp
   CONTAINS
     PROCEDURE :: coeff => fluvial_link_coeff
     PROCEDURE :: hydro_update => fluvial_link_hupdate
  END type fluvial_link

  DOUBLE PRECISION, PARAMETER :: alpha = 1.0
  DOUBLE PRECISION, PARAMETER :: beta = 0.5

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE fluvial_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE fluvial_link_coeff(this, dt, pt1, pt2, c, cp)

    IMPLICIT NONE
    CLASS (fluvial_link), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: c, cp

    

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
