! ----------------------------------------------------------------
! file: confluence_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February  2, 2017 by William A. Perkins
! Last Change: 2018-01-08 10:03:24 d3g096
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

    IMPLICIT NONE
    DOUBLE PRECISION :: uconc
    CLASS (confluence_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: c(:, 0:)
    DOUBLE PRECISION :: qi, qin, qout, ci, cavg
    INTEGER :: i, nc, ulink, upmax

    qin = 0.0
    qout = 0.0
    uconc = 0.0
    cavg = 0
    n = 0

    DO i = 1, this%n_ulink
       ulink = this%ulink(i)
       upmax = maxpoints(ulink)
       qi = q(ulink, upmax)
       IF (qi .GE. 0.0) THEN
          ci = c(ulink, upmax)
          uconc = uconc + qi*ci
          qin = qin + qi
       ELSE
          qout = qout - qi
       END IF
       cavg = cavg + c(ulink, upmax)
       nc = nc + 1
    END DO

    qi = q(this%dlink, 1)
    ci = c(this%dlink, 1)
    cavg = cavg + ci
    cavg = cavg/REAL(nc+1)
    IF (qi .LT. 0.0) THEN
       qin = qin - qi
       uconc = uconc - qi*ci
    ELSE
       qout = qout + qi
    END IF

    IF (qout .GT. 0.0) THEN
       uconc = uconc/qout
    ELSE
       uconc = cavg
    END IF
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
  
