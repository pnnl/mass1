! ----------------------------------------------------------------
! file: linear_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June 28, 2017 by William A. Perkins
! Last Change: 2017-07-27 07:26:39 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE linear_link_module
! ----------------------------------------------------------------
MODULE linear_link_module
  USE link_module
  USE point_module
  USE bc_module
  USE cross_section
  USE general_vars, ONLY: depth_threshold, depth_minimum

  IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC :: coeff
     DOUBLE PRECISION :: a, b, c, d, g
  END type coeff

  TYPE, PUBLIC, EXTENDS(link_t) :: linear_link_t
     INTEGER :: npoints
     INTEGER :: input_option
     TYPE (point_t), DIMENSION(:),ALLOCATABLE :: pt
   CONTAINS
     PROCEDURE :: initialize => linear_link_initialize
     PROCEDURE :: readpts => linear_link_readpts
     PROCEDURE :: q_up => linear_link_q_up
     PROCEDURE :: q_down => linear_link_q_down
     PROCEDURE :: y_up => linear_link_y_up
     PROCEDURE :: y_down => linear_link_y_down
     PROCEDURE :: c_up => linear_link_c_up
     PROCEDURE :: c_down => linear_link_c_down
     PROCEDURE :: coeff => linear_link_coeff
     PROCEDURE :: forward_sweep => linear_link_forward
     PROCEDURE :: backward_sweep => linear_link_backward
     PROCEDURE :: hydro_update => linear_link_hupdate
     PROCEDURE :: destroy => linear_link_destroy
  END type linear_link_t

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION linear_link_initialize(this, ldata, bcman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (linear_link_t), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    this%id = ldata%linkid
    this%dsid = ldata%dsid

    ! find the "link" bc, if any; children can set this and it will be preserved

    IF (.NOT. ASSOCIATED(this%usbc%p)) THEN
       IF (ldata%lbcid .NE. 0) THEN
          this%usbc%p => bcman%find(LINK_BC_TYPE, ldata%lbcid)
          IF (.NOT. ASSOCIATED(this%usbc%p) ) THEN
             WRITE (msg, *) 'link ', ldata%linkid, ': unknown link BC id: ', ldata%lbcid
             CALL error_message(msg)
             ierr = ierr + 1
          END IF
       END IF
    END IF

    ! find the downstream bc, if any; children can set this and it will be preserved

    IF (.NOT. ASSOCIATED(this%dsbc%p)) THEN
       IF (ldata%dsbcid .NE. 0) THEN
          this%dsbc%p => bcman%find(LINK_BC_TYPE, ldata%dsbcid)
          IF (.NOT. ASSOCIATED(this%dsbc%p) ) THEN
             WRITE (msg, *) 'link ', ldata%linkid, &
                  &': unknown downstream BC id: ', ldata%dsbcid
             CALL error_message(msg)
             ierr = ierr + 1
          END IF
       END IF
    END IF

  END FUNCTION linear_link_initialize

  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_readpts
  ! ----------------------------------------------------------------
  FUNCTION linear_link_readpts(this, punit, lineno) RESULT(ierr)
    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: punit
    INTEGER, INTENT(INOUT) :: lineno

    ierr = 0

  END FUNCTION linear_link_readpts



  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_q_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_q_up(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = 1
    linear_link_q_up = this%pt(n)%hnow%q
  END FUNCTION linear_link_q_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_q_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_q_down(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = this%npoints
    linear_link_q_down = this%pt(n)%hnow%q
  END FUNCTION linear_link_q_down


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_up(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = 1
    linear_link_y_up = this%pt(n)%hnow%y
  END FUNCTION linear_link_y_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_down(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = this%npoints
    linear_link_y_down = this%pt(n)%hnow%y
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
  SUBROUTINE linear_link_coeff(this, dt, pt1, pt2, c, cp)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: c, cp

    CALL error_message("This should not happen: linear_link_coeff should be overridden", &
         &fatal=.TRUE.)
  END SUBROUTINE linear_link_coeff


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_forward
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_forward(this, deltat)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat

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
       this%pt(point)%sweep%f = bcval - this%pt(point)%hnow%q
    END IF

    DO point = 1, this%npoints - 1
       CALL this%coeff(deltat, this%pt(point), this%pt(point + 1), c, cp)
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

       dy = this%dcon%p%elev() - this%pt(point)%hnow%y
       dq = this%pt(point)%sweep%e*dy + this%pt(point)%sweep%f
       this%pt(point)%hnow%y = this%pt(point)%hnow%y + dy
       this%pt(point)%hnow%q = this%pt(point)%hnow%q + dq

    ELSE IF (ASSOCIATED(this%dsbc%p)) THEN

       bcval = this%dsbc%p%current_value

    ELSE 
       CALL error_message("This should not happen in linear_link_backward", &
            &fatal=.TRUE.)
    END IF

    DO point = this%npoints - 1, 1, -1
       dy = this%pt(point)%sweep%l*dy + this%pt(point)%sweep%m*dq + this%pt(point)%sweep%n
       dq = this%pt(point)%sweep%e*dy + this%pt(point)%sweep%f

       this%pt(point)%hnow%y = this%pt(point)%hnow%y + dy
       this%pt(point)%hnow%q = this%pt(point)%hnow%q + dq
       
    END DO

    

  END SUBROUTINE linear_link_backward


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_hupdate
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_hupdate(this, res_coeff)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: res_coeff

    INTEGER :: p

    DO p = 1, this%npoints
       CALL this%pt(p)%hydro_update(res_coeff)
    END DO

  END SUBROUTINE linear_link_hupdate



    
  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_destroy(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this

    ! DEALLOCATE(this%pt)

  END SUBROUTINE linear_link_destroy


END MODULE linear_link_module
