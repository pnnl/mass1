
! ----------------------------------------------------------------
! file: link_manager_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 20, 2017 by William A. Perkins
! Last Change: 2018-01-22 12:41:36 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE link_manager_module
! ----------------------------------------------------------------
MODULE link_manager_module
  USE utility
  USE mass1_config
  USE link_module
  USE fluvial_link_module
  USE nonfluvial_link_module
  USE bc_module
  USE section_handler_module

  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! link_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_manager_t
     TYPE (link_list) :: links
     CLASS (link_t), POINTER :: dslink
     INTEGER :: maxorder
   CONTAINS
     PROCEDURE :: read => link_manager_read
     PROCEDURE, PRIVATE :: readpts => link_manager_readpts
     PROCEDURE :: find => link_manager_find
     PROCEDURE :: connect => link_manager_connect
     PROCEDURE :: flow_sim => link_manager_flow_sim
     PROCEDURE :: hyupdate => link_manager_hyupdate
     PROCEDURE :: read_restart => link_manager_read_restart
     PROCEDURE :: write_restart => link_manager_write_restart
     PROCEDURE :: destroy => link_manager_destroy
  END type link_manager_t

  INTERFACE link_manager_t
     MODULE PROCEDURE new_link_manager
  END INTERFACE link_manager_t

  PUBLIC :: new_link_manager

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_link_manager
  ! ----------------------------------------------------------------
  FUNCTION new_link_manager() RESULT (man)
    IMPLICIT NONE
    TYPE (link_manager_t) :: man
    man%links = new_link_list()
  END FUNCTION new_link_manager

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_readpts
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_readpts(this, theconfig, sectman)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(INOUT) :: theconfig
    CLASS (section_handler), INTENT(INOUT) :: sectman
    CLASS (link_t), POINTER :: link
    INTEGER :: linkid, lastid, lineno, ierr, iostat
    INTEGER, PARAMETER :: punit = 22
    CHARACTER (LEN=1024) :: msg

    
    lineno = 0
    ierr = 0
    lastid = 0

    CALL open_existing(theconfig%point_file, punit, fatal=.TRUE.)
  
    ! FIXME: CALL print_output("POINTS", 0.0)

    DO WHILE(.TRUE.)

       READ(punit,*, IOSTAT=iostat) linkid

       lineno = lineno + 1

       IF (IS_IOSTAT_END(iostat)) THEN
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) TRIM(theconfig%point_file) // ': read error near line ', lineno
          CALL error_message(msg, fatal=.TRUE.)
       END IF

       IF (linkid .EQ. lastid) THEN
          WRITE(msg, *) TRIM(theconfig%point_file), ': error, line', lineno, &
               &': extra point for link ', linkid, ' ?'
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END IF

       link => this%find(linkid)
       IF (.NOT. ASSOCIATED(link)) THEN
          IF (linkid .NE. lastid) THEN
             WRITE(msg,*) TRIM(theconfig%point_file), ': error, line ', lineno, &
                  &': unknown link id: ', linkid
             CALL error_message(msg)
          END IF
          ierr = ierr + 1
          CYCLE
       END IF
     
       lastid = linkid

       BACKSPACE(punit)
       lineno = lineno - 1
       
       IF (link%readpts(theconfig, sectman, punit, lineno) .NE. 0) THEN
          WRITE(msg,*) TRIM(theconfig%point_file), ': error, line ', lineno, &
            &': problem with points for link ', link%id
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    END DO
    CLOSE(punit)
    RETURN

  END SUBROUTINE link_manager_readpts


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_read
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_read(this, theconfig, bcman, sectman)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(INOUT) :: theconfig
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (link_t), POINTER :: link
    CLASS (section_handler), INTENT(INOUT) :: sectman
    INTEGER, PARAMETER :: lunit = 21
    INTEGER :: recno, ierr, iostat
    TYPE (link_input_data) :: ldata
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    recno = 0

    CALL open_existing(theconfig%link_file, lunit, fatal=.TRUE.)
    ! FIXME: CALL print_output("LINKS ", 0.0)
    
    DO WHILE (.TRUE.) 
       recno = recno + 1
       CALL ldata%defaults()
       READ(lunit,*, IOSTAT=iostat) &
            & ldata%linkid, &
            & ldata%inopt, &
            & ldata%npt, &
            & ldata%lorder, &
            & ldata%ltype, &
            & ldata%nup, &
            & ldata%bcid, &
            & ldata%dsbcid, &
            & ldata%gbcid, &
            & ldata%tbcid, &
            & ldata%mzone, &
            & ldata%lbcid, &
            & ldata%lgbcid, &
            & ldata%ltbcid, &
            & ldata%lpiexp

       IF (IS_IOSTAT_END(iostat)) THEN
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) TRIM(theconfig%link_file) // &
               &': error in or near link record ', recno
          CALL error_message(msg, fatal=.TRUE.)
       END IF
       
       READ(lunit,*, IOSTAT=iostat) ldata%dsid

       IF (IS_IOSTAT_END(iostat)) THEN
          WRITE(msg, *) TRIM(theconfig%link_file), ': link record ', recno, &
               & ', link id = ', ldata%linkid, &
               &', is incomplete (second line missing)'
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) TRIM(theconfig%link_file) // &
               &': error in or near link record ', recno
          CALL error_message(msg, fatal=.TRUE.)
       END IF

       WRITE(msg, *) TRIM(theconfig%link_file), ": record ", recno, &
            &": id = ", ldata%linkid, ", dsid = ", ldata%dsid
       CALL status_message(msg)

       ! set any unneeded BC ids to zero
       IF (.NOT. theconfig%do_latflow) THEN
          ldata%lbcid = 0
          ldata%lgbcid = 0
          ldata%ltbcid = 0
       END IF
       IF (.NOT. theconfig%do_temp) THEN
          ldata%tbcid = 0
          ldata%ltbcid = 0
       END IF
       IF (.NOT. theconfig%do_gas) THEN
          ldata%gbcid = 0
          ldata%lgbcid = 0
       END IF
       
       SELECT CASE (ldata%ltype)
       CASE (1)
          ALLOCATE(fluvial_link :: link)
       CASE (20) 
          ! FIXME: transport
          ALLOCATE(fluvial_link :: link)
       CASE (21)
          ALLOCATE(fluvial_hydro_link :: link)
       CASE (2)
          ALLOCATE(discharge_link :: link)
       CASE (6)
          ALLOCATE(hydro_link :: link)
       CASE (3)
          ALLOCATE(ustage_link :: link)
       CASE (5)
          ALLOCATE(trib_inflow_link :: link)
       CASE DEFAULT
          WRITE(msg, *) TRIM(theconfig%link_file), ': link record ', recno, &
               &': link type unknown (', ldata%ltype, ')'
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END SELECT

       IF (link%initialize(ldata, bcman) .NE. 0) THEN
          WRITE(msg, *) TRIM(theconfig%link_file), ': link record ', recno, &
               & ', link id = ', ldata%linkid, ': error'
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
       CALL this%links%push(link)
       NULLIFY(link)
    END DO

    CLOSE (lunit)

    IF (ierr .GT. 0) THEN
       msg = TRIM(theconfig%link_file) // ': too many errors in link file'
       CALL error_message(msg, fatal=.TRUE.)
    END IF

    WRITE(msg, *) TRIM(theconfig%link_file), &
         &': successfully read ', this%links%size(), ' links'
    CALL status_message(msg)

    CALL this%readpts(theconfig, sectman)

    RETURN

  END SUBROUTINE link_manager_read


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
    CHARACTER(LEN=1024) :: msg, lbl

    ierr = 0

    WRITE(msg, *) 'Connecting ', this%links%size(), ' links ...'
    CALL status_message(msg)

    CALL this%links%begin()
    link => this%links%current()
    
    DO WHILE (ASSOCIATED(link))
       NULLIFY(link%ucon%p)
       NULLIFY(link%dcon%p)
       CALL this%links%next()
       link => this%links%current()
    END DO

    ! find the downstream link: there can be only one, highlander

    nds = 0

    CALL this%links%begin()
    link => this%links%current()
    
    DO WHILE (ASSOCIATED(link))
       IF (link%dsid .LE. 0) THEN 
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

    ! connect each link with it's downstream neighbor

    CALL this%links%begin()
    link => this%links%current()
    
    DO WHILE (ASSOCIATED(link))
       IF (link%dsid .GT. 0) THEN 
          CALL this%links%save()
          dlink => this%links%find(link%dsid)
          CALL this%links%restore()
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
          link%dcon%p => dlink%ucon%p
       END IF
       
       CALL this%links%next()
       link => this%links%current()
    END DO

    
    IF (ierr .GT. 0) THEN
       CALL error_message("Network connectivity errors, cannot continue", &
            &fatal=.TRUE.)
    END IF

    ! compute computational order

    this%maxorder = this%dslink%set_order(1)
    IF (this%maxorder-1 .NE. this%links%size()) THEN
       CALL error_message("link_manager_connect: this should not happen")
    END IF

    ! spit out connectivity and order information 

    CALL this%links%begin()
    dlink => this%links%current()

    DO WHILE (ASSOCIATED(dlink))
       WRITE(msg, '("link ", I4, "(order = ", I4, ") upstream links:")') &
            &dlink%id, dlink%order
       IF (ASSOCIATED(dlink%ucon%p)) THEN
          CALL dlink%ucon%p%ulink%begin()
          link => dlink%ucon%p%ulink%current()
          DO WHILE (ASSOCIATED(link))
             WRITE(lbl, '(I4)') link%id
             msg = TRIM(msg) // " " // TRIM(lbl)
             CALL dlink%ucon%p%ulink%next()
             link => dlink%ucon%p%ulink%current()
          END DO
       ELSE 
          msg = TRIM(msg) // " none"
       END IF
       CALL status_message(msg)
       CALL this%links%next()
       dlink => this%links%current()
    END DO
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
  SUBROUTINE link_manager_flow_sim(this, deltat, grav, res_coeff, dsbc_type)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat, grav, res_coeff
    INTEGER, INTENT(IN) :: dsbc_type

    CLASS (link_t), POINTER :: link
    INTEGER :: l

    DO l = 1, this%maxorder
       CALL this%links%begin()
       link => this%links%current()
       DO WHILE (ASSOCIATED(link))
          IF (link%order .EQ. l) THEN
             CALL link%forward_sweep(deltat)
          END IF
          CALL this%links%next()
          link => this%links%current()
       END DO
    END DO

    DO l = this%maxorder, 1, -1
       CALL this%links%begin()
       link => this%links%current()
       DO WHILE (ASSOCIATED(link))
          IF (link%order .EQ. l) THEN
             CALL link%backward_sweep(dsbc_type)
             CALL link%hydro_update(res_coeff, grav, deltat)
          END IF
          CALL this%links%next()
          link => this%links%current()
       END DO
    END DO

  END SUBROUTINE link_manager_flow_sim

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_hyupdate
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_hyupdate(this, res_coeff, grav, dt)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: res_coeff, grav, dt
    CLASS (link_t), POINTER :: link
    
    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%hydro_update(res_coeff, grav, dt)
       CALL this%links%next()
       link => this%links%current()
    END DO

  END SUBROUTINE link_manager_hyupdate

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_read_restart(this, iounit, res_coeff, grav, dt)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    DOUBLE PRECISION, INTENT(IN) :: res_coeff, grav, dt
    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%read_restart(iounit)
       CALL this%links%next()
       link => this%links%current()
    END DO
    
    CALL this%hyupdate(res_coeff, grav, dt)
    

  END SUBROUTINE link_manager_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_write_restart(this, iounit)

    IMPLICIT NONE

    CLASS (link_manager_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%write_restart(iounit)
       CALL this%links%next()
       link => this%links%current()
    END DO

  END SUBROUTINE link_manager_write_restart




  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_destroy(this)
    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    CALL this%links%clear()
  END SUBROUTINE link_manager_destroy


  
END MODULE link_manager_module
