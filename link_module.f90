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
! Last Change: 2017-07-13 13:00:20 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE link_module
! ----------------------------------------------------------------
MODULE link_module

  USE dlist_module
  USE bc_module
  USE utility

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE confluence_ptr (forward)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: confluence_ptr
     TYPE (confluence_t), POINTER :: p
  END type confluence_ptr

  ! ----------------------------------------------------------------
  ! TYPE link_t
  ! ----------------------------------------------------------------
  TYPE, ABSTRACT, PUBLIC :: link_t
     INTEGER :: id
     INTEGER :: dsid
     TYPE (bc_ptr) :: usbc, dsbc
     TYPE (confluence_ptr) :: ucon, dcon
   CONTAINS

     PROCEDURE (init_proc), DEFERRED :: initialize
     PROCEDURE (destroy_proc), DEFERRED :: destroy

     ! the up/down routines are required by confluence

     PROCEDURE (up_down_proc), DEFERRED :: q_up
     PROCEDURE (up_down_proc), DEFERRED :: q_down
     PROCEDURE (up_down_proc), DEFERRED :: y_up
     PROCEDURE (up_down_proc), DEFERRED :: y_down
     PROCEDURE (c_up_down_proc), DEFERRED :: c_up
     PROCEDURE (c_up_down_proc), DEFERRED :: c_down

     ! hydrodynamics are computed with two sweeps

     PROCEDURE (fsweep_proc), DEFERRED :: forward_sweep
     PROCEDURE (bsweep_proc), DEFERRED :: backward_sweep
     PROCEDURE (hupdate_proc), DEFERRED :: hydro_update


  END type link_t

  ABSTRACT INTERFACE
     SUBROUTINE init_proc(this, id, dsid)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       INTEGER, INTENT(IN) :: id, dsid
     END SUBROUTINE init_proc

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
     
     SUBROUTINE fsweep_proc(this, deltat)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       DOUBLE PRECISION, INTENT(IN) :: deltat
     END SUBROUTINE fsweep_proc

     SUBROUTINE bsweep_proc(this)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
     END SUBROUTINE bsweep_proc

     SUBROUTINE hupdate_proc(this, res_coeff)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
       DOUBLE PRECISION, INTENT(IN) :: res_coeff
     END SUBROUTINE hupdate_proc

     SUBROUTINE destroy_proc(this)
       IMPORT :: link_t
       IMPLICIT NONE
       CLASS (link_t), INTENT(INOUT) :: this
     END SUBROUTINE destroy_proc

  END INTERFACE

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

  ! ----------------------------------------------------------------
  ! link_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_manager_t
     TYPE (link_list) :: links
     CLASS (link_t), POINTER :: dslink
   CONTAINS
     PROCEDURE :: find => link_manager_find
     PROCEDURE :: connect => link_manager_connect
     PROCEDURE :: flow_sim => link_manager_flow_sim
     PROCEDURE :: destroy => link_manager_destroy
  END type link_manager_t

  INTERFACE link_manager_t
     MODULE PROCEDURE new_link_manager
  END INTERFACE link_manager_t

  TYPE, PUBLIC :: confluence_t
     TYPE (link_list) :: ulink
     TYPE (link_ptr) :: dlink
   CONTAINS
     PROCEDURE :: coeff_e => confluence_coeff_e
     PROCEDURE :: coeff_f => confluence_coeff_f
     PROCEDURE :: elev => confluence_elev
     PROCEDURE :: conc => confluence_conc
  END type confluence_t

  INTERFACE confluence_t
     MODULE PROCEDURE new_confluence_t
  END INTERFACE

  INTERFACE confluence_ptr
     MODULE PROCEDURE new_confluence_ptr
  END INTERFACE confluence_ptr

  PUBLIC :: new_link_manager
  
CONTAINS

  ! ! ----------------------------------------------------------------
  ! !  FUNCTION new_link_t
  ! ! ----------------------------------------------------------------
  ! FUNCTION new_link_t(id, dsid) RESULT(link)
  !   IMPLICIT NONE
  !   INTEGER, INTENT(IN) :: id, dsid
  !   TYPE (link_t) :: link

  !   link%id = id
  !   link%dsid = dsid
  !   NULLIFY(link%usbc%p)
  !   NULLIFY(link%dsbc%p)
  !   NULLIFY(link%ucon%p)
  !   NULLIFY(link%dcon%p)

  ! END FUNCTION new_link_t


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
  !  FUNCTION new_link_manager
  ! ----------------------------------------------------------------
  FUNCTION new_link_manager() RESULT (man)
    IMPLICIT NONE
    TYPE (link_manager_t) :: man
    man%links = new_link_list()
  END FUNCTION new_link_manager

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_connect
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_connect(this)

    IMPLICIT NONE

    CLASS (link_manager_t), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link, dlink

    INTEGER :: ierr
    INTEGER :: nds
    TYPE (confluence_t), POINTER :: con
    CHARACTER(LEN=1024) :: msg

    ierr = 0

    CALL this%links%begin()
    link => this%links%current()
    
    DO WHILE (ASSOCIATED(link))
       NULLIFY(link%ucon%p)
       NULLIFY(link%dcon%p)
       CALL this%links%next()
       link => this%links%current()
    END DO

    nds = 0

    CALL this%links%begin()
    link => this%links%current()
    
    DO WHILE (ASSOCIATED(link))
       IF (link%dsid .GT. 0) THEN 
          dlink => this%find(link%dsid)
          IF (.NOT. ASSOCIATED(dlink)) THEN
             WRITE(msg, '("link , I4, : invalid downstream link id (",I4,")")')&
                  & link%id, link%dsid
             CALL error_message(msg)
             ierr = ierr + 1
          END IF
          IF (.NOT. ASSOCIATED(dlink%ucon%p)) THEN 
             ALLOCATE(con)
             con = confluence_t(dlink)
             dlink%ucon%p => con
             NULLIFY(con)
          END IF
          CALL dlink%ucon%p%ulink%push(link)
       ELSE 
          this%dslink => link
          nds = nds + 1
       END IF
       
       CALL this%links%next()
       link => this%links%current()
    END DO

    
    IF (nds .EQ. 0) THEN
       CALL error_message("No downstream link found, there must be one")
       ierr = ierr + 1
    ELSE IF (nds .GT. 1) THEN
       CALL error_message("Too many ownstream links found, there can be only one")
       ierr = ierr + 1
    END IF

    IF (ierr .GT. 0) THEN
       CALL error_message("Network connectivity errors, cannot continue", &
            &fatal=.TRUE.)
    END IF

  ! spit out connectivity and order information 

    CALL this%links%begin()
    link => this%links%current()
    
    ! DO WHILE (ASSOCIATED(link))
    !  WRITE(msg, '("link ", I4, "(order = ", I4, ") upstream links:")') &
    !       &link%id, 0
    !  IF (ASSOCIATED(link%ucon%p)) THEN
    !     DO i = 1, ucon(link)%p%n_ulink
    !        WRITE(lbl, '(I4)') ucon(link)%p%ulink(i)
    !        msg = TRIM(msg) // " " // TRIM(lbl)
    !     END DO
    !  ELSE 
    !     msg = TRIM(msg) // " none"
    !  END IF
    !  CALL status_message(msg)
    ! END DO

    

  END SUBROUTINE link_manager_connect


  ! ----------------------------------------------------------------
  !  FUNCTION link_manager_find
  ! ----------------------------------------------------------------
  FUNCTION link_manager_find(this, linkid) RESULT(link)
    IMPLICIT NONE
    CLASS (link_t), POINTER :: link
    CLASS (link_manager_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: linkid
    link => this%links%find(linkid)
  END FUNCTION link_manager_find

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_flow_sim
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_flow_sim(this, deltat, res_coeff)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat, res_coeff

    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()
    DO WHILE (ASSOCIATED(link))
       CALL link%forward_sweep(deltat)
       CALL this%links%next()
       link => this%links%current()
    END DO

    CALL this%links%begin()
    link => this%links%current()
    DO WHILE (ASSOCIATED(link))
       CALL link%backward_sweep()
       CALL link%hydro_update(res_coeff)
       CALL this%links%next()
       link => this%links%current()
    END DO
    

  END SUBROUTINE link_manager_flow_sim


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_destroy(this)
    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    CALL this%links%clear()
  END SUBROUTINE link_manager_destroy


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
  !  FUNCTION new_confluence_ptr
  ! ----------------------------------------------------------------
  FUNCTION new_confluence_ptr()

    IMPLICIT NONE
    TYPE (confluence_ptr) :: new_confluence_ptr
    NULLIFY(new_confluence_ptr%p)
  END FUNCTION new_confluence_ptr

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

    ue = 0.0
    
  END FUNCTION confluence_coeff_e

  ! ----------------------------------------------------------------
  !  FUNCTION confluence_coeff_f
  ! ----------------------------------------------------------------
  FUNCTION confluence_coeff_f(this) RESULT(uf)

    IMPLICIT NONE
    DOUBLE PRECISION :: uf
    CLASS (confluence_t), INTENT(INOUT) :: this
    uf = 0.0

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

END MODULE link_module
