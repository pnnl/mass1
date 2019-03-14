! ----------------------------------------------------------------
! file: pid_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January 30, 2018 by William A. Perkins
! Last Change: 2019-03-14 11:50:34 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE pid_link_module
! ----------------------------------------------------------------
MODULE pid_link_module

  USE utility
  USE point_module
  USE link_module
  USE scalar_module
  USE linear_link_module
  USE flow_coeff
  USE bc_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE pidlink_lag_rec
  ! This is used to hold lagged flow/stage signal information
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: pidlink_lag_rec

     ! Lagged signal identifier, either a BC (< 0) or a link (> 0)
     INTEGER :: id

     ! BC providing lagged signal
     CLASS (bc_t), POINTER :: bc

     ! Link providing lagged signal (from upstream end)
     CLASS (link_t), POINTER :: link

     ! Lag time (days)
     DOUBLE PRECISION :: lag

     ! this is used to keep the lagged flows in an FIFO queue, only
     ! the number needed are saved (lag/time_step)
     INTEGER :: nlag
     DOUBLE PRECISION, POINTER :: flow(:)

  END TYPE pidlink_lag_rec
  
  INTERFACE pidlink_lag_rec
     MODULE PROCEDURE new_pidlink_lag_rec
  END INTERFACE pidlink_lag_rec

  INTEGER, PUBLIC, PARAMETER :: max_pid_lag = 10

  ! ----------------------------------------------------------------
  ! TYPE pid_link
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: pid_link

     ! if .TRUE., the PID error term is discharge, rather than stage
     LOGICAL :: followflow

     DOUBLE PRECISION :: kc, ti, tr         ! constant coefficients
     DOUBLE PRECISION :: errsum             ! integral term
     DOUBLE PRECISION :: oldsetpt

                                ! this is a list of flows to be lagged
     LOGICAL :: lagready
     INTEGER :: numflows
     TYPE (pidlink_lag_rec), POINTER :: lagged(:)
   CONTAINS
     PROCEDURE :: initialize => pid_link_initialize
     PROCEDURE :: lag_initialize => pid_link_lag_initialize
     PROCEDURE, PRIVATE :: lag => pid_link_lag
     PROCEDURE :: coeff => pid_link_coeff
     PROCEDURE :: hydro_update => pid_link_hupdate
  END type pid_link

  ! ----------------------------------------------------------------
  ! TYPE pid_flow_link
  ! "Flow following" PID link
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(pid_link) :: pid_flow_link
   CONTAINS
     PROCEDURE :: initialize => pid_flow_link_initialize
  END type pid_flow_link

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_pidlink_lag_rec
  ! ----------------------------------------------------------------
  FUNCTION new_pidlink_lag_rec() RESULT(rec)

    IMPLICIT NONE
    TYPE (pidlink_lag_rec) :: rec
    rec%id = 0
    NULLIFY(rec%bc)
    NULLIFY(rec%link)
    rec%lag = 0.0
    rec%nlag = 0
    NULLIFY(rec%flow)
  END FUNCTION new_pidlink_lag_rec

  ! ----------------------------------------------------------------
  !  FUNCTION pid_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION pid_link_initialize(this, ldata, bcman, sclrman, metman) RESULT(ierr)

    IMPLICIT NONE

    INTEGER :: ierr
    CLASS (pid_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman

    CHARACTER (LEN=1024) :: msg

    this%followflow = .FALSE.
    this%numflows = 0
    this%errsum = 0.0
    NULLIFY(this%lagged)
    this%lagready = .FALSE.

    ierr = this%linear_link_t%initialize(ldata, bcman, sclrman, metman)

    IF (.NOT. ASSOCIATED(this%usbc)) THEN 
       WRITE(msg, *) 'link ', this%id, &
            &': PID links must have an upstream boundary condition'
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END FUNCTION pid_link_initialize

  ! ----------------------------------------------------------------
  !  FUNCTION pid_flow_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION pid_flow_link_initialize(this, ldata, bcman, sclrman, metman) RESULT(ierr)

    IMPLICIT NONE

    INTEGER :: ierr
    CLASS (pid_flow_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman

    ierr = this%pid_link%initialize(ldata, bcman, sclrman, metman)
    this%followflow = .TRUE.

  END FUNCTION pid_flow_link_initialize




  ! ----------------------------------------------------------------
  !  FUNCTION pid_link_lag
  ! ----------------------------------------------------------------
  FUNCTION pid_link_lag(this) RESULT(lag)

    IMPLICIT NONE
    DOUBLE PRECISION :: lag
    CLASS (pid_link), INTENT(IN) :: this

    INTEGER :: i

    lag = 0.0
    DO i = 1, this%numflows
       lag = lag + this%lagged(i)%flow(1)
    END DO
  END FUNCTION pid_link_lag



  ! ----------------------------------------------------------------
  ! SUBROUTINE pid_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE pid_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (pid_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: setpt, lag, eval, lval
    DOUBLE PRECISION :: delta_t

    delta_t = dt/3600.0/24.0

                                ! continuity is the same in all cases

    cf%a = 0.0
    cf%b = 1.0
    cf%c = 0.0
    cf%d = 1.0
    cf%g = pt1%hnow%q - pt2%hnow%q

    setpt = this%oldsetpt
    this%errsum = this%errsum + (pt1%hnow%y -  setpt)*delta_t
    lag = this%lag()

                                ! momentum coefficients 
    
    cf%ap = 0.0
    cf%bp = 0.0

    setpt = this%usbc%current_value


    IF (this%followflow) THEN
                                ! when using discharge as the error term

       cf%cp = -1.0
       eval = pt1%hnow%q
       lval = pt1%hnow%y
       IF (this%ti .GT. 0.0) THEN
          cf%dp = this%kc*(1.0 + delta_t/this%ti + this%tr/delta_t)
       ELSE
          cf%dp = this%kc*(1.0 + this%tr/delta_t)
       END IF

    ELSE
                                ! when using stage as the error term

       cf%dp = -1.0
       eval = pt1%hnow%y
       lval = pt1%hnow%q
       IF (this%ti .GT. 0.0) THEN
          cf%cp = this%kc*(1.0 + delta_t/this%ti + this%tr/delta_t)
       ELSE
          cf%cp = this%kc*(1.0 + this%tr/delta_t)
       END IF

    END IF

    IF (this%ti .GT. 0.0) THEN
       cf%gp = lag - lval + &
            & this%kc*eval*(1.0 + delta_t/this%ti) - &
            & this%kc*setpt*(1.0 + delta_t/this%ti + this%tr/delta_t) + &
            & this%kc/this%ti*this%errsum + this%kc*this%tr/delta_t*this%oldsetpt
    ELSE
       cf%gp = lag - lval + &
            & this%kc*eval - &
            & this%kc*setpt*(1.0 + this%tr/delta_t) + &
            & this%kc*this%tr/delta_t*this%oldsetpt
    END IF
       

    this%oldsetpt = setpt

    WRITE (1,100) this%id, pt1%hnow%y, pt1%hnow%q, setpt, this%oldsetpt, lag, this%errsum
100 FORMAT(I5, 6(1X,F10.2))
  END SUBROUTINE pid_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE pid_link_lag_initialize
  !
  ! This routine allocates space for and initialized lagged flows. It
  ! must be called after an initial state is available, but before
  ! hydro_update()
  ! ----------------------------------------------------------------
  SUBROUTINE pid_link_lag_initialize(this)

    IMPLICIT NONE
    CLASS (pid_link), INTENT(INOUT) ::this

    INTEGER :: i
    CLASS (point_t), POINTER :: pt
    DOUBLE PRECISION :: setpt

    setpt = this%usbc%current_value
    this%oldsetpt = setpt

    DO i = 1, this%numflows
       
       ASSOCIATE(rec => this%lagged(i))
         ALLOCATE(rec%flow(rec%nlag))
         IF (ASSOCIATED(rec%bc)) THEN
            rec%flow = rec%bc%current_value
         ELSE
            pt => rec%link%point(1)
            rec%flow = pt%hnow%q
         END IF
       END ASSOCIATE

       this%lagready = .TRUE.

    END DO
  END SUBROUTINE pid_link_lag_initialize


  ! ----------------------------------------------------------------
  ! SUBROUTINE pid_link_hupdate
  ! ----------------------------------------------------------------
  SUBROUTINE pid_link_hupdate(this, grav, unitwt, dt)

    IMPLICIT NONE
    CLASS (pid_link), INTENT(INOUT) ::this
    DOUBLE PRECISION, INTENT(IN) :: grav, unitwt, dt

    INTEGER :: i, j
    CLASS (point_t), POINTER :: pt

    IF (.NOT. this%lagready) CALL this%lag_initialize()

    CALL this%linear_link_t%hydro_update(grav, unitwt, dt)

    DO i = 1, this%numflows

       ASSOCIATE(rec => this%lagged(i))

         ! index 1 holds the oldest flow/stage, get rid of it and put
         ! the newest at the end of the queue
         
         DO j = 2, rec%nlag
            rec%flow(j - 1) = rec%flow(j)
         END DO

         ! at this point, we should have made sure that stages were
         ! specified as BC's
       
         IF (ASSOCIATED(rec%bc)) THEN
            rec%flow = rec%bc%current_value
         ELSE
            pt => rec%link%point(1)
            rec%flow(rec%nlag) = pt%hnow%q
         END IF
       END ASSOCIATE
    END DO
  END SUBROUTINE pid_link_hupdate


END MODULE pid_link_module
