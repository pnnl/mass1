! ----------------------------------------------------------------
! file: linear_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June 28, 2017 by William A. Perkins
! Last Change: 2017-07-03 15:04:52 d3g096
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
     TYPE (point_sweep_coeff) :: sweep
  END type point_t

  TYPE :: coeff
     DOUBLE PRECISION :: a, b, c, d, g
  END type coeff

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
     PROCEDURE :: coeff => linear_link_coeff
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
    INTEGER :: n
    n = 1
    linear_link_q_up = this%pt(n)%hstate%q
  END FUNCTION linear_link_q_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_q_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_q_down(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = this%npoints
    linear_link_q_down = this%pt(n)%hstate%q
  END FUNCTION linear_link_q_down


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_up(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = 1
    linear_link_y_up = this%pt(n)%hstate%y
  END FUNCTION linear_link_y_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_down(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = this%npoints
    linear_link_y_down = this%pt(n)%hstate%y
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
  ! SUBROUTINE linear_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_coeff(this, pt1, pt2, c, cp)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: c, cp

    CALL error_message("This should not happen: linear_link_coeff should be overridden", &
         &fatal=.TRUE.)
  END SUBROUTINE linear_link_coeff


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_forward
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_forward(this)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this

    INTEGER :: point
    DOUBLE PRECISION :: bcval, denom
    TYPE (coeff) :: c, cp
    
    point = 1
    IF (ASSOCIATED(this%ucon%p)) THEN
       this%pt(point)%sweep%e = this%ucon%p%coeff_e()
       this%pt(point)%sweep%f = this%ucon%p%coeff_f()
    ELSE
       IF (ASSOCIATED(this%usbc%p)) THEN
          bcval = this%usbc%p%current_value
       ELSE 
          bcval = 0.0
       END IF
       this%pt(point)%sweep%e = 0.0
       this%pt(point)%sweep%f = bcval - this%pt(point)%hstate%q
    END IF

    DO point = 1, this%npoints - 1
       CALL this%coeff(this%pt(point), this%pt(point + 1), c, cp)
       denom = (c%c*cp%d - cp%c*c%d)
       this%pt(point)%sweep%l = (c%a*cp%d - cp%a*c%d)/denom
       this%pt(point)%sweep%m = (c%b*cp%d - cp%b*c%d)/denom
       this%pt(point)%sweep%n = (c%d*cp%g - cp%d*c%g)/denom

       denom = c%b - this%pt(point)%sweep%m*(c%c + c%d*this%pt(point)%sweep%e)
       this%pt(point+1)%sweep%e = &
            &(this%pt(point)%sweep%l*(c%c + c%d*this%pt(point)%sweep%e) - c%a)/denom
       this%pt(point+1)%sweep%f = &
            &(this%pt(point)%sweep%n*(c%c + c%d*this%pt(point)%sweep%e) + &
            &c%d*this%pt(point)%sweep%f + c%g)/denom

    END DO


  END SUBROUTINE linear_link_forward


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_backward
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_backward(this)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION :: bcval, dy, dq
    INTEGER :: point

    point = this%npoints
    
    IF (ASSOCIATED(this%dcon%p)) THEN

       dy = this%dcon%p%elev() - this%pt(point)%hstate%y
       dq = this%pt(point)%sweep%e*dy + this%pt(point)%sweep%f
       this%pt(point)%hstate%y = this%pt(point)%hstate%y + dy
       this%pt(point)%hstate%q = this%pt(point)%hstate%q + dq

    ELSE IF (ASSOCIATED(this%dsbc%p)) THEN


    ELSE 
       CALL error_message("This should not happen in linear_link_backward", &
            &fatal=.TRUE.)
    END IF

    DO point = this%npoints - 1, 1, -1
       dy = this%pt(point)%sweep%l*dy + this%pt(point)%sweep%m*dq + this%pt(point)%sweep%n
       dq = this%pt(point)%sweep%e*dy + this%pt(point)%sweep%f

       this%pt(point)%hstate%y = this%pt(point)%hstate%y + dy
       this%pt(point)%hstate%q = this%pt(point)%hstate%q + dq
       
    END DO

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
