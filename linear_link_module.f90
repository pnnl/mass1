! ----------------------------------------------------------------
! file: linear_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June 28, 2017 by William A. Perkins
! Last Change: 2017-07-03 13:17:43 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE linear_link_module
! ----------------------------------------------------------------
MODULE linear_link_module
  USE link_module
  USE bc_module
  USE cross_section

  IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC :: point_hydro_state
     DOUBLE PRECISION :: y, q, area
     DOUBLE PRECISION :: lateral_inflow
     DOUBLE PRECISION :: froude_num
     DOUBLE PRECISION :: friction_slope, bed_shear
     DOUBLE PRECISION :: courant_num, diffuse_num
  END type point_hydro_state

  TYPE :: point_sweep_coeff
     DOUBLE PRECISION :: e,f,l,m,n
  END type point_sweep_coeff

  TYPE :: point_t
     DOUBLE PRECISION :: x, thalweg
     DOUBLE PRECISION :: manning, k_diff
     CLASS (xsection_ptr), POINTER :: xsection
     TYPE (xsection_prop) :: xsprop
     TYPE (point_hydro_state) :: hstate
     TYPE (point_sweep_coeff) :: coeff
  END type point_t

  TYPE, PUBLIC, EXTENDS(link_t) :: linear_link_t
     INTEGER :: npoints
     INTEGER :: input_option
     DOUBLE PRECISION :: lpiexp
     TYPE (point_t), DIMENSION(:),ALLOCATABLE :: pt
   CONTAINS
     PROCEDURE :: initialize => linear_link_initialize
     PROCEDURE :: q_up => linear_link_q_up
     PROCEDURE :: q_down => linear_link_q_down
     PROCEDURE :: y_up => linear_link_y_up
     PROCEDURE :: y_down => linear_link_y_down
     PROCEDURE :: c_up => linear_link_c_up
     PROCEDURE :: c_down => linear_link_c_down
     PROCEDURE :: forward_sweep => linear_link_forward
     PROCEDURE :: backward_sweep => linear_link_backward
     PROCEDURE :: destroy => linear_link_destroy
  END type linear_link_t

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_initialize(this, id, dsid)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: id, dsid

    this%id = id
    this%dsid = dsid

  END SUBROUTINE linear_link_initialize


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_q_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_q_up(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    linear_link_q_up = 0.0
  END FUNCTION linear_link_q_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_q_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_q_down(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    linear_link_q_down = 0.0
  END FUNCTION linear_link_q_down


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_up(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    linear_link_y_up = 0.0
  END FUNCTION linear_link_y_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_down(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    linear_link_y_down = 0.0
  END FUNCTION linear_link_y_down


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_c_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_c_up(this, ispecies)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: ispecies
    linear_link_c_up = 0.0
  END FUNCTION linear_link_c_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_c_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_c_down(this, ispecies)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: ispecies
    linear_link_c_down = 0.0
  END FUNCTION linear_link_c_down

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_forward
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_forward(this)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this


    IF (ASSOCIATED(this%ucon%p)) THEN
    ELSEIF (ASSOCIATED(this%usbc%p)) THEN
    ELSE 
       CALL error_message("This should not happen in linear_link_forward", &
            &fatal=.TRUE.)
    END IF



  END SUBROUTINE linear_link_forward


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_backward
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_backward(this)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this


    IF (ASSOCIATED(this%ucon%p)) THEN
    ELSEIF (ASSOCIATED(this%usbc%p)) THEN
    ELSE 
       CALL error_message("This should not happen in linear_link_backward", &
            &fatal=.TRUE.)
    END IF



  END SUBROUTINE linear_link_backward




    
  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_destroy(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this

    DEALLOCATE(this%pt)

  END SUBROUTINE linear_link_destroy


END MODULE linear_link_module
