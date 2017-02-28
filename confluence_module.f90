  ! ----------------------------------------------------------------
  ! file: confluence_module.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created February  2, 2017 by William A. Perkins
  ! Last Change: 2017-02-28 10:07:59 d3g096
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE confluence_module
! ----------------------------------------------------------------
MODULE confluence_module

  IMPLICIT NONE

  PRIVATE

  INTEGER, PARAMETER :: maxulink = 5
  
  TYPE, PUBLIC :: confluence_t
     INTEGER :: n_ulink
     INTEGER :: ulink(maxulink)
     INTEGER :: dlink
   CONTAINS
     PROCEDURE :: coeff_e => confluence_coeff_e
     PROCEDURE :: coeff_f => confluence_coeff_f
     PROCEDURE :: elev => confluence_elev
     PROCEDURE :: conc => confluence_conc
  END type confluence_t

  INTERFACE confluence_t
     MODULE PROCEDURE new_confluence_t
  END INTERFACE

  TYPE, PUBLIC :: confluence_ptr
     TYPE (confluence_t), POINTER :: p
  END type confluence_ptr

  INTERFACE confluence_ptr
     MODULE PROCEDURE new_confluence_ptr
  END INTERFACE confluence_ptr

  TYPE (confluence_ptr), ALLOCATABLE, PUBLIC :: ucon(:), dcon(:)

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_confluence_t
  ! ----------------------------------------------------------------
  FUNCTION new_confluence_t(dlink)

    IMPLICIT NONE
    TYPE (confluence_t) :: new_confluence_t
    INTEGER, INTENT(IN) :: dlink
    new_confluence_t%n_ulink = 0
    new_confluence_t%dlink = dlink
  END FUNCTION new_confluence_t

  ! ----------------------------------------------------------------
  ! FUNCTION confluence_coeff_e
  !
  ! This is called by the downstream link and returns the sum of the
  ! upstream link "e" momentum cofficients
  ! ----------------------------------------------------------------
  FUNCTION confluence_coeff_e(this) RESULT(ue)
    USE link_vars
    USE point_vars
    USE flow_coeffs
    IMPLICIT NONE
    DOUBLE PRECISION :: ue
    CLASS (confluence_t), INTENT(IN) :: this
    INTEGER :: i, ulink

    ue = 0.0
    DO i = 1, this%n_ulink
       ulink = this%ulink(i)
       ue = ue + e(ulink, maxpoints(ulink))
    END DO
  END FUNCTION confluence_coeff_e

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_coeff_f
  ! ----------------------------------------------------------------
  FUNCTION confluence_coeff_f(this) RESULT(uf)
    USE link_vars
    USE point_vars
    USE flow_coeffs

    IMPLICIT NONE
    DOUBLE PRECISION :: uf
    CLASS (confluence_t), INTENT(IN) :: this
    INTEGER :: i, ulink, upmax

    uf = 0.0
    DO i = 1, this%n_ulink
       ulink = this%ulink(i)
       upmax = maxpoints(ulink)
       uf = uf + &
            &q(ulink, upmax) + f(ulink, upmax) + &
            &e(ulink, upmax)*(y(this%dlink, 1) - y(ulink, upmax))
    END DO
    uf = -q(this%dlink, 1) + uf

  END FUNCTION confluence_coeff_f

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_elev
  ! ----------------------------------------------------------------
  FUNCTION confluence_elev(this) RESULT(dsy)
    USE link_vars
    USE point_vars
    USE flow_coeffs

    IMPLICIT NONE
    DOUBLE PRECISION :: dsy
    CLASS (confluence_t), INTENT(IN) :: this

    dsy = y(this%dlink, 1)

  END FUNCTION confluence_elev

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_conc
  ! ----------------------------------------------------------------
  FUNCTION confluence_conc(this, c) RESULT(uconc)
    USE link_vars
    USE point_vars
    USE flow_coeffs

    IMPLICIT NONE
    DOUBLE PRECISION :: uconc
    CLASS (confluence_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: c(:, 0:)
    DOUBLE PRECISION :: qi, ci
    INTEGER :: i, ulink, upmax

    uconc = 0.0
    DO i = 1, this%n_ulink
       ulink = this%ulink(i)
       upmax = maxpoints(ulink)
       qi = q(ulink, upmax)
       ci = c(ulink, upmax)
       uconc = uconc + qi*ci
    END DO
    qi = q(this%dlink, 1)
    uconc = uconc/qi
  END FUNCTION confluence_conc


  ! ----------------------------------------------------------------
  !  FUNCTION new_confluence_ptr
  ! ----------------------------------------------------------------
  FUNCTION new_confluence_ptr()

    IMPLICIT NONE
    TYPE (confluence_ptr) :: new_confluence_ptr
    NULLIFY(new_confluence_ptr%p)
  END FUNCTION new_confluence_ptr

END MODULE confluence_module
  
