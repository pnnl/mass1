! ----------------------------------------------------------------
! file: link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March  8, 2017 by William A. Perkins
! Last Change: 2018-08-21 15:02:06 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE link_module
! ----------------------------------------------------------------
MODULE link_module

  USE utility
  USE dlist_module
  USE bc_module
  USE mass1_config
  USE section_handler_module
  USE point_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE link_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_ptr
     CLASS (link_t), POINTER :: p
  END type link_ptr

  ! ----------------------------------------------------------------
  ! link_list
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(dlist) :: link_list
   CONTAINS
     PROCEDURE :: push => link_list_push
     PROCEDURE :: pop => link_list_pop
     PROCEDURE :: clear => link_list_clear
     PROCEDURE :: find => link_list_find
     PROCEDURE :: current => link_list_current
  END type link_list

  INTERFACE link_list
     MODULE PROCEDURE new_link_list
  END INTERFACE link_list

  PUBLIC new_link_list

  ! ----------------------------------------------------------------
  ! TYPE confluence_t
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: confluence_t
     TYPE (link_list) :: ulink
     TYPE (link_ptr) :: dlink
   CONTAINS
     PROCEDURE :: coeff_e => confluence_coeff_e
     PROCEDURE :: coeff_f => confluence_coeff_f
     PROCEDURE :: elev => confluence_elev
     PROCEDURE :: conc => confluence_conc
     PROCEDURE :: set_order => confluence_set_order
     PROCEDURE :: discharge => confluence_discharge
  END type confluence_t

  INTERFACE confluence_t
     MODULE PROCEDURE new_confluence_t
  END INTERFACE

  ! ----------------------------------------------------------------
  ! TYPE link_input_data
  ! Fields expected in link input data
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_input_data
    INTEGER :: linkid, inopt, npt, lorder, ltype
    INTEGER :: nup, dsid
    INTEGER :: bcid, dsbcid, gbcid, tbcid, mzone, lbcid, lgbcid, ltbcid
    DOUBLE PRECISION :: lpiexp
  CONTAINS 
    PROCEDURE :: defaults => link_input_defaults
  END type link_input_data

  ! ----------------------------------------------------------------
  ! TYPE link_t
  ! ----------------------------------------------------------------
  TYPE, ABSTRACT, PUBLIC :: link_t
     INTEGER :: id
     INTEGER :: order
     INTEGER :: dsid, usbcid, dsbcid
     CLASS (bc_t), POINTER :: usbc, dsbc, latbc
     CLASS (confluence_t), POINTER :: ucon, dcon
   CONTAINS

     PROCEDURE :: construct => link_construct
     PROCEDURE (init_proc), DEFERRED :: initialize
     PROCEDURE (readpts_proc), DEFERRED :: readpts
     PROCEDURE :: points => link_points
     PROCEDURE :: length => link_length
     PROCEDURE (destroy_proc), DEFERRED :: destroy

     PROCEDURE, NON_OVERRIDABLE :: set_order => link_set_order

     ! the up/down routines are required by confluence

     PROCEDURE (up_down_proc), DEFERRED :: q_up
     PROCEDURE (up_down_proc), DEFERRED :: q_down
     PROCEDURE (up_down_proc), DEFERRED :: y_up
     PROCEDURE (up_down_proc), DEFERRED :: y_down
     PROCEDURE (c_up_down_proc), DEFERRED :: c_up
     PROCEDURE (c_up_down_proc), DEFERRED :: c_down

     ! state initialization

     PROCEDURE (set_initial_proc), DEFERRED :: set_initial
     PROCEDURE (read_restart_proc), DEFERRED :: read_restart
     PROCEDURE (write_restart_proc), DEFERRED :: write_restart

     ! hydrodynamics are computed with two sweeps

     PROCEDURE (fsweep_proc), DEFERRED :: forward_sweep
     PROCEDURE (bsweep_proc), DEFERRED :: backward_sweep
     PROCEDURE (hupdate_proc), DEFERRED :: hydro_update

     ! transport related routines

     PROCEDURE (max_tnumber_proc), DEFERRED :: max_courant
     PROCEDURE (max_tnumber_proc), DEFERRED :: max_diffuse
     ! PROCEDURE (trans_interp_proc, DEFERRED :: trans_interp

     ! get a point on a link (if any)

     PROCEDURE :: point => link_point

     ! perform sanity checks 

     PROCEDURE :: check => link_check

  END type link_t

  ABSTRACT INTERFACE
     FUNCTION init_proc(this, ldata, bcman) RESULT(ierr)
       IMPORT :: link_t, link_input_data, bc_manager_t
       IMPLICIT NONE
       INTEGER :: ierr
       CLASS (link_t), INTENT(INOUT) :: this
       CLASS (link_input_data), INTENT(IN) :: ldata
       CLASS (bc_manager_t), INTENT(IN) :: bcman
     END FUNCTION init_proc

     FUNCTION readpts_proc(this, theconfig, sectman, punit, lineno) RESULT (ierr)
       IMPORT :: link_t, configuration_t, section_handler
       IMPLICIT NONE
       INTEGER :: ierr
       CLASS (link_t), INTENT(INOUT) :: this
       TYPE (configuration_t), INTENT(IN) :: theconfig
       CLASS (section_handler), INTENT(INOUT) :: sectman
       INTEGER, INTENT(IN) :: punit
       INTEGER, INTENT(INOUT) :: lineno

     END FUNCTION readpts_proc

     DOUBLE PRECISION FUNCTION up_down_proc(this)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(IN) :: this
     END FUNCTION up_down_proc
     
     DOUBLE PRECISION FUNCTION c_up_down_proc(this, ispecies)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(IN) :: this
       INTEGER, INTENT(IN) :: ispecies
     END FUNCTION c_up_down_proc
     
     SUBROUTINE set_initial_proc(this, stage, discharge, c)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       DOUBLE PRECISION, INTENT(IN) :: stage, discharge, c(:)
     END SUBROUTINE set_initial_proc

     SUBROUTINE read_restart_proc(this, iunit)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       INTEGER, INTENT(IN) :: iunit
     END SUBROUTINE read_restart_proc

     SUBROUTINE write_restart_proc(this, iunit)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(IN) :: this
       INTEGER, INTENT(IN) :: iunit
     END SUBROUTINE write_restart_proc

     SUBROUTINE fsweep_proc(this, deltat)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       DOUBLE PRECISION, INTENT(IN) :: deltat
     END SUBROUTINE fsweep_proc

     SUBROUTINE bsweep_proc(this, dsbc_type)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       INTEGER, INTENT(IN) :: dsbc_type
     END SUBROUTINE bsweep_proc

     SUBROUTINE hupdate_proc(this, grav, dt)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       DOUBLE PRECISION, INTENT(IN) :: grav, dt
     END SUBROUTINE hupdate_proc

     DOUBLE PRECISION FUNCTION max_tnumber_proc(this, dt)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(IN) :: this
       DOUBLE PRECISION, INTENT(IN) :: dt
     END FUNCTION max_tnumber_proc

     SUBROUTINE destroy_proc(this)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
     END SUBROUTINE destroy_proc

  END INTERFACE

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_input_defaults
  ! ----------------------------------------------------------------
  SUBROUTINE link_input_defaults(this)
    IMPLICIT NONE
    CLASS (link_input_data), INTENT(INOUT) :: this

    this%linkid = 0
    this%inopt = 1
    this%npt = 0
    this%lorder = 0
    this%ltype = 0
    this%nup = 0
    this%dsbcid = 0
    this%gbcid = 0
    this%tbcid = 0
    this%mzone = 0
    this%lbcid = 0
    this%lgbcid = 0
    this%ltbcid = 0
    this%lpiexp = 0.0

    this%dsid = 0

  END SUBROUTINE link_input_defaults


  ! ! ----------------------------------------------------------------
  ! !  FUNCTION new_link_t
  ! ! ----------------------------------------------------------------
  ! FUNCTION new_link_t(id, dsid) RESULT(link)
  !   IMPLICIT NONE
  !   INTEGER, INTENT(IN) :: id, dsid
  !   TYPE (link_t) :: link

  !   link%id = id
  !   link%dsid = dsid
  !   NULLIFY(link%usbc)
  !   NULLIFY(link%dsbc)
  !   NULLIFY(link%ucon)
  !   NULLIFY(link%dcon)

  ! END FUNCTION new_link_t

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_construct
  ! ----------------------------------------------------------------
  SUBROUTINE link_construct(this)

    IMPLICIT NONE
    CLASS (link_t), INTENT(INOUT) :: this

    NULLIFY(this%usbc)
    NULLIFY(this%dsbc)
    NULLIFY(this%latbc)
    NULLIFY(this%ucon)
    NULLIFY(this%dcon)

  END SUBROUTINE link_construct


  ! ----------------------------------------------------------------
  !  FUNCTION link_initialize
  ! ----------------------------------------------------------------
  FUNCTION link_initialize(this, ldata, bcman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (link_t), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CHARACTER (LEN=1024) :: msg


  END FUNCTION link_initialize

  ! ----------------------------------------------------------------
  !  FUNCTION link_points
  ! ----------------------------------------------------------------
  FUNCTION link_points(this) RESULT(n)

    IMPLICIT NONE
    CLASS (link_t), INTENT(IN) :: this
    INTEGER :: n
    n = 0

  END FUNCTION link_points

  ! ----------------------------------------------------------------
  !  FUNCTION link_length
  ! ----------------------------------------------------------------
  FUNCTION link_length(this) RESULT (len)

    IMPLICIT NONE
    DOUBLE PRECISION :: len
    CLASS (link_t), INTENT(IN) :: this

    len = 0

  END FUNCTION link_length


  ! ----------------------------------------------------------------
  !  FUNCTION link_point
  ! ----------------------------------------------------------------
  FUNCTION link_point(this, idx) RESULT(pt)
    IMPLICIT NONE
    TYPE (point_t), POINTER :: pt
    CLASS (link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: idx

    NULLIFY(pt)
  END FUNCTION link_point

  ! ----------------------------------------------------------------
  !  FUNCTION new_link_list
  ! ----------------------------------------------------------------
  FUNCTION new_link_list()
    IMPLICIT NONE
    TYPE (link_list) :: new_link_list
    NULLIFY(new_link_list%head)
    NULLIFY(new_link_list%tail)
  END FUNCTION new_link_list


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE link_list_push(this, alink)
    IMPLICIT NONE
    CLASS (link_list), INTENT(INOUT) :: this
    CLASS (link_t), POINTER, INTENT(IN) :: alink
    TYPE (link_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    ALLOCATE(ptr)
    ptr%p => alink
    p => ptr
    CALL this%genpush(p)
  END SUBROUTINE link_list_push

  ! ----------------------------------------------------------------
  !  FUNCTION link_list_pop
  ! ----------------------------------------------------------------
  FUNCTION link_list_pop(this) RESULT(link)
    IMPLICIT NONE
    CLASS (link_list), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link
    TYPE (link_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(link)
    p => this%genpop()

    IF (ASSOCIATED(p)) THEN
       SELECT TYPE (p)
       TYPE IS (link_ptr)
          ptr => p
          link => ptr%p
          DEALLOCATE(ptr)
       END SELECT
    END IF
    RETURN
  END FUNCTION link_list_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_list_clear
  ! ----------------------------------------------------------------
  SUBROUTINE link_list_clear(this)
    IMPLICIT NONE
    CLASS (link_list), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link

    DO WHILE (.TRUE.)
       link => this%pop()
       IF (ASSOCIATED(link)) THEN
          CALL link%destroy()
          DEALLOCATE(link)
       ELSE 
          EXIT
       END IF
    END DO
  END SUBROUTINE link_list_clear

  ! ----------------------------------------------------------------
  !  FUNCTION link_list_find
  ! ----------------------------------------------------------------
  FUNCTION link_list_find(this, linkid) RESULT(link)
    IMPLICIT NONE
    CLASS (link_t), POINTER :: link
    CLASS (link_list) :: this
    INTEGER, INTENT(IN) :: linkid

    NULLIFY(link)

    CALL this%begin()
    link => this%current()
    DO WHILE (ASSOCIATED(link)) 
       IF (link%id .EQ. linkid) THEN
          EXIT
       END IF
       CALL this%next()
       link => this%current()
    END DO
  END FUNCTION link_list_find

  ! ----------------------------------------------------------------
  !  FUNCTION link_list_current
  ! ----------------------------------------------------------------
  FUNCTION link_list_current(this) RESULT(link)
    IMPLICIT NONE
    CLASS (link_t), POINTER :: link
    CLASS (link_list) :: this
    TYPE (link_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(link)

    IF (ASSOCIATED(this%cursor)) THEN
       p => this%cursor%data
       IF (ASSOCIATED(p)) THEN
          SELECT TYPE (p)
          TYPE IS (link_ptr)
             ptr => p
             link => ptr%p
          END SELECT
       END IF
    END IF
  END FUNCTION link_list_current

  ! ----------------------------------------------------------------
  !  FUNCTION new_confluence_t
  ! ----------------------------------------------------------------
  FUNCTION new_confluence_t(dlink)
    IMPLICIT NONE
    TYPE (confluence_t) :: new_confluence_t
    CLASS (link_t), POINTER, INTENT(IN) :: dlink
    new_confluence_t%ulink = new_link_list()
    new_confluence_t%dlink%p => dlink
  END FUNCTION new_confluence_t

  ! ----------------------------------------------------------------
  ! FUNCTION confluence_coeff_e
  !
  ! This is called by the downstream link and returns the sum of the
  ! upstream link "e" momentum cofficients
  ! ----------------------------------------------------------------
  FUNCTION confluence_coeff_e(this) RESULT(ue)
    IMPLICIT NONE
    DOUBLE PRECISION :: ue
    CLASS (confluence_t), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link
    CLASS (point_t), POINTER :: pt
    INTEGER :: pidx

    ue = 0.0

    CALL this%ulink%begin()
    link => this%ulink%current()

    DO WHILE (ASSOCIATED(link))
       pidx = link%points()
       pt => link%point(pidx)
       ue = ue + pt%sweep%e

       CALL this%ulink%next()
       link => this%ulink%current()
    END DO
    
  END FUNCTION confluence_coeff_e

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_coeff_f
  ! ----------------------------------------------------------------
  FUNCTION confluence_coeff_f(this) RESULT(uf)

    IMPLICIT NONE
    DOUBLE PRECISION :: uf
    CLASS (confluence_t), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link
    CLASS (point_t), POINTER :: pt
    INTEGER :: pidx
    DOUBLE PRECISION :: q, dq, y, dy, e, f
    
    pt => this%dlink%p%point(1)
    dy = pt%hnow%y
    dq = pt%hnow%q

    uf = 0.0

    CALL this%ulink%begin()
    link => this%ulink%current()

    DO WHILE (ASSOCIATED(link))
       pidx = link%points()
       pt => link%point(pidx)
       q = pt%hnow%q
       y = pt%hnow%y
       e = pt%sweep%e
       f = pt%sweep%f
       uf = uf + q + f + e*(dy - y)

       CALL this%ulink%next()
       link => this%ulink%current()
    END DO

    uf = uf - dq
    

  END FUNCTION confluence_coeff_f

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_elev
  ! ----------------------------------------------------------------
  FUNCTION confluence_elev(this) RESULT(dsy)

    IMPLICIT NONE
    DOUBLE PRECISION :: dsy
    CLASS (confluence_t), INTENT(INOUT) :: this

    dsy = this%dlink%p%y_up()

  END FUNCTION confluence_elev

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_conc
  ! ----------------------------------------------------------------
  FUNCTION confluence_conc(this, ispecies) RESULT(uconc)

    IMPLICIT NONE
    DOUBLE PRECISION :: uconc
    CLASS (confluence_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispecies
    CLASS (link_t), POINTER :: link
    DOUBLE PRECISION :: qin, qout, cavg
    INTEGER :: n
    
    qin = 0.0
    qout = 0.0
    uconc = 0.0
    cavg = 0.0
    
    CALL this%ulink%begin()
    link => this%ulink%current()
    DO WHILE (ASSOCIATED(link))
       cavg = cavg + link%c_down(ispecies)
       IF (link%q_down() .GE. 0.0) THEN
          qin = qin + link%q_down()
          uconc = link%q_down()*link%c_down(ispecies)
       ELSE 
          qout = qout + link%q_down()
       END IF

       CALL this%ulink%next()
       link => this%ulink%current()

       n = n + 1
    END DO
    
    link => this%dlink%p
    cavg = cavg +  link%c_up(ispecies)
    IF (link%q_up() .LT. 0.0) THEN
       qin = qin + link%q_up()
       uconc = link%q_up()*link%c_up(ispecies)
    ELSE 
       qout = qout + link%q_up()
    END IF
       
    IF (qout .GT. 0.0) THEN
       uconc = uconc/qout
    ELSE 
       uconc = cavg/REAL(n+1)
    END IF
  END FUNCTION confluence_conc

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_set_order
  ! ----------------------------------------------------------------
  RECURSIVE FUNCTION confluence_set_order(this, order0) RESULT(order)

    IMPLICIT NONE
    INTEGER :: order
    CLASS (confluence_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: order0
    CLASS (link_t), POINTER :: link
    INTEGER :: o

    o = order0

    CALL this%ulink%begin()
    link => this%ulink%current()
    DO WHILE (ASSOCIATED(link))
       o = link%set_order(o)
       CALL this%ulink%next()
       link => this%ulink%current()
    END DO
    order = o

  END FUNCTION confluence_set_order

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_discharge
  !
  ! Computes the discharge to the *downstream* link, i.e. the sum of
  ! the upstream link discharge(s).  This should probably only be used
  ! by hydrologic links.
  ! ----------------------------------------------------------------
  FUNCTION confluence_discharge(this) RESULT(q)
    IMPLICIT NONE
    DOUBLE PRECISION :: q
    CLASS (confluence_t), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link

    q = 0.0

    CALL this%ulink%begin()
    link => this%ulink%current()
    DO WHILE (ASSOCIATED(link))
       q = q + link%q_down()
       CALL this%ulink%next()
       link => this%ulink%current()
    END DO

  END FUNCTION confluence_discharge

  ! ----------------------------------------------------------------
  !  FUNCTION link_set_order
  ! ----------------------------------------------------------------
  RECURSIVE FUNCTION link_set_order(this, order0) RESULT(order)
    IMPLICIT NONE
    INTEGER :: order
    CLASS (link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: order0
    INTEGER :: o

    o = order0
    IF (ASSOCIATED(this%ucon)) THEN
       o = this%ucon%set_order(o)
    END IF
    this%order = o
    order = o + 1

  END FUNCTION link_set_order

  ! ----------------------------------------------------------------
  !  FUNCTION link_check
  ! ----------------------------------------------------------------
  FUNCTION link_check(this) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (link_t), INTENT(INOUT) :: this

    ierr = 0

  END FUNCTION link_check


END MODULE link_module
