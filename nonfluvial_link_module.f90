! ----------------------------------------------------------------
! file: nonfluvial_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 17, 2017 by William A. Perkins
! Last Change: 2018-02-06 09:43:10 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE nonfluvial_link_module
! ----------------------------------------------------------------
MODULE nonfluvial_link_module
  USE utility
  USE point_module
  USE link_module
  USE linear_link_module
  USE flow_coeff
  USE bc_module
  IMPLICIT NONE

  PRIVATE 

  ! ----------------------------------------------------------------
  ! TYPE discharge_link
  ! Imposed discharge (type = 2)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: discharge_link
   CONTAINS
     PROCEDURE :: coeff => discharge_link_coeff
  END type discharge_link

  ! ----------------------------------------------------------------
  ! TYPE hydro_link
  ! Like discharge link, but with discharge split into spill and
  ! generation (type = 6)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(discharge_link) :: hydro_link
   CONTAINS
     PROCEDURE :: initialize => hydro_link_initialize
     PROCEDURE :: coeff => hydro_link_coeff
  END type hydro_link

  ! ----------------------------------------------------------------
  ! TYPE ustage_link
  ! Imposed stage upstream (type = 3)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: ustage_link
   CONTAINS
     PROCEDURE :: coeff => ustage_link_coeff
  END type ustage_link

  ! ----------------------------------------------------------------
  ! TYPE dstage_link
  ! Imposed stage downstream (type = 4)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: dstage_link
   CONTAINS
     PROCEDURE :: coeff => dstage_link_coeff
  END type dstage_link

  ! ----------------------------------------------------------------
  ! TYPE trib_inflow_link
  ! Tributary inflow (type = 5)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: trib_inflow_link
   CONTAINS
     PROCEDURE :: coeff => trib_inflow_link_coeff
  END type trib_inflow_link


  DOUBLE PRECISION, PARAMETER :: eps = 1.0D-09

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE discharge_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE discharge_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (discharge_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval

    bcval = this%usbc%current_value

    cf%a = 0.0
    cf%b = 1.0
    cf%c = 0.0 !eps
    cf%d = 1.0
    cf%g = pt1%hnow%q - pt2%hnow%q

    cf%ap = 0.0
    cf%bp = 0.0
    cf%cp = eps
    cf%dp = 1.0
    cf%gp = pt1%hnow%q - bcval

  END SUBROUTINE discharge_link_coeff

  ! ----------------------------------------------------------------
  !  FUNCTION hydro_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION hydro_link_initialize(this, ldata, bcman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (hydro_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    IF (ldata%bcid .GT. 0) THEN
       this%usbc => bcman%find(HYDRO_BC_TYPE, ldata%bcid)
       IF (.NOT. ASSOCIATED(this%usbc) ) THEN
          WRITE (msg, *) 'link ', ldata%linkid, ': unknown hydro BC id: ', ldata%bcid
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    ELSE 
       WRITE (msg, *) 'hydro link ', ldata%linkid, ' requires a hydro BC, none specified'
       CALL error_message(msg)
       ierr = ierr + 1
    END IF
    
    ierr = ierr + this%linear_link_t%initialize(ldata, bcman)
  END FUNCTION hydro_link_initialize


  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (hydro_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf
    
    CALL this%discharge_link%coeff(dt, pt1, pt2, cf)

  END SUBROUTINE hydro_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE ustage_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE ustage_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (ustage_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval
    bcval = this%usbc%current_value

    cf%a = 0.0
    cf%b = 1.0
    cf%c = 0.0
    cf%d = 1.0
    cf%g = pt1%hnow%q - pt2%hnow%q
    
    cf%ap = 0.0
    cf%bp = 0.0
    cf%cp = 1.0
    cf%dp = eps
    cf%gp = pt1%hnow%y - bcval

  END SUBROUTINE ustage_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE dstage_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE dstage_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (dstage_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval
    bcval = this%usbc%current_value

    cf%a = 1.0
    cf%b = eps
    cf%c = 0.0
    cf%d = 0.0
    cf%g = bcval - pt2%hnow%y

    cf%ap = 0.0
    cf%bp = 1.0
    cf%cp = 0.0
    cf%dp = 1.0

    cf%gp = pt1%hnow%q - pt2%hnow%q

  END SUBROUTINE dstage_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE trib_inflow_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE trib_inflow_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE

    CLASS (trib_inflow_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval
    bcval = this%usbc%current_value

     cf%a = 0.0
     cf%b = 1.0
     cf%c = 0.0
     cf%d = 1.0
     cf%g = pt1%hnow%q + bcval - pt2%hnow%q
     
     cf%ap = 1.0
     cf%bp = 0.0
     cf%cp = 1.0
     cf%dp = 0.0
     cf%gp = pt1%hnow%y - pt2%hnow%y

  END SUBROUTINE trib_inflow_link_coeff



END MODULE nonfluvial_link_module
