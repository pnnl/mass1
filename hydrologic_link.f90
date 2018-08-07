! ----------------------------------------------------------------
! MODULE hydrologic_link_module
! ----------------------------------------------------------------
MODULE hydrologic_link_module

  USE utility
  USE point_module
  USE bc_module
  USE link_module
  USE cross_section
  USE section_handler_module
  USE mass1_config
  USE linear_link_module

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  ! ----------------------------------------------------------------
  ! TYPE hydrologic_link
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: hydrologic_link
     DOUBLE PRECISION :: y
     DOUBLE PRECISION :: L, So
     DOUBLE PRECISION :: K
     DOUBLE PRECISION :: inflow, outflow, storage
     DOUBLE PRECISION :: inflow_old, outflow_old, storage_old
     DOUBLE PRECISION :: qin, qout
   CONTAINS
     PROCEDURE :: readpts => hydrologic_link_readpts
     PROCEDURE :: set_initial => hydrologic_link_set_initial
     PROCEDURE :: forward_sweep => hydrologic_link_forward
     PROCEDURE :: backward_sweep => hydrologic_link_backward
     PROCEDURE :: read_restart => hydrologic_link_read_restart
     PROCEDURE :: write_restart => hydrologic_link_write_restart
     PROCEDURE :: hydro_update => hydrologic_link_hupdate
  END type hydrologic_link

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION hydrologic_link_readpts
  ! ----------------------------------------------------------------
  FUNCTION hydrologic_link_readpts(this, theconfig, sectman, punit, lineno) RESULT(ierr)
    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(IN) :: theconfig
    CLASS (section_handler), INTENT(INOUT) :: sectman
    INTEGER, INTENT(IN) :: punit
    INTEGER, INTENT(INOUT) :: lineno
    CHARACTER (LEN=1024) :: msg

    SELECT CASE (this%input_option)
    CASE (1)
       WRITE(msg, *) "Link ", this%id, &
            &": readpts: hydrologic links cannot use point-based input (option 1)" 
       CALL error_message(msg)
       ierr = ierr + 1
    CASE (2)
       ierr = this%linear_link_t%readpts(theconfig, sectman, punit, lineno)
    END SELECT
    RETURN
  END FUNCTION hydrologic_link_readpts

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_set_initial
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_set_initial(this, stage, discharge, c)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: stage, discharge, c(:)
    INTEGER :: i
    DOUBLE PRECISION :: dx, a, depth
    TYPE (xsection_prop) :: props

    CALL this%linear_link_t%set_initial(stage, discharge, c)

    this%inflow = discharge
    this%outflow = discharge
    this%inflow_old = this%inflow
    this%outflow_old = this%outflow

    ! assume normal stage throughout the link; compute storage based
    ! on that
    this%storage = 0.0

    DO i = 1, this%npoints
       this%pt(i)%hnow%y = this%pt(i)%xsection%p%normal_depth(&
            &this%pt(i)%hnow%q, this%So, this%pt(i)%kstrick,&
            &1.49D00, this%pt(i)%hnow%y) + this%pt(i)%thalweg
       IF (i .EQ. 1) THEN
          dx = ABS(this%pt(i)%x - this%pt(i+1)%x)/2.0
       ELSE IF (i .GE. this%npoints) THEN
          dx = ABS(this%pt(i)%x - this%pt(i-1)%x)/2.0
       ELSE 
          dx = ABS(this%pt(i-1)%x - this%pt(i+1)%x)/2.0
       END IF
       depth = this%pt(i)%hnow%y - this%pt(i)%thalweg
       a = this%pt(i)%xsection%p%area(depth)
       this%storage = this%storage + dx*a
    END DO

    this%storage_old = this%storage
    this%y = this%pt(this%npoints)%hnow%y - this%pt(this%npoints)%thalweg

  END SUBROUTINE hydrologic_link_set_initial


  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_forward
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_forward(this, deltat)

    IMPLICIT NONE

    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat

    DOUBLE PRECISION :: X, kstrick, latflow

    this%y = this%pt(this%npoints)%hnow%y - this%pt(this%npoints)%thalweg
    kstrick = this%pt(this%npoints)%kstrick
    this%K = SQRT(this%So)*kstrick*this%y**(2.0/3.0)/this%L

    X = EXP(-this%K*deltat)

    ! get upstream inflow (volume)

    this%inflow = this%inflow*deltat

    ! get lateral inflow (volume)

    latflow = latflow*this%L*deltat

    this%storage = (1/this%K)*(this%inflow + latflow) + &
         &X*(this%storage_old - (1/this%K)*(this%inflow + latflow))

    this%outflow = this%inflow + latflow + (this%storage - this%storage_old)

    ! compute discharge rates and sweep coefficients for confluences

    this%qout = this%outflow / deltat;
    this%pt(this%npoints)%hnow%q = this%qout
    this%qin = this%inflow / deltat;
    this%pt(1)%hnow%q = this%qin


  END SUBROUTINE hydrologic_link_forward

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_backward
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_backward(this, dsbc_type)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: dsbc_type

    DOUBLE PRECISION :: x0, x, q, y
    INTEGER :: i

    x0 = this%pt(this%npoints)%x

    DO i = 1, this%npoints
       x = this%pt(this%npoints)%x
       q = (x - x0)/this%L*(this%qin - this%qout) + this%qout
       this%pt(i)%hnow%q = q
       y = this%pt(i)%hnow%y - this%pt(i)%thalweg
       this%pt(i)%hnow%y = &
            &this%pt(i)%xsection%p%normal_depth(q, this%So, this%pt(i)%kstrick, 1.49D00, y)
    END DO


  END SUBROUTINE hydrologic_link_backward

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_hupdate
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_hupdate(this, res_coeff, grav, dt)

    IMPLICIT NONE
    
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: res_coeff, grav, dt

    


  END SUBROUTINE hydrologic_link_hupdate

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_read_restart(this, iunit)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    CHARACTER (LEN=1024) :: msg

    

  END SUBROUTINE hydrologic_link_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_write_restart(this, iunit)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit

  END SUBROUTINE hydrologic_link_write_restart


END MODULE hydrologic_link_module
