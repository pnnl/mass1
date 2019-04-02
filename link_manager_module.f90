
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
! Last Change: 2019-03-14 11:52:53 d3g096
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
  USE pid_link_module
  USE hydrologic_link_module
  USE bc_module
  USE section_handler_module
  USE scalar_module

  IMPLICIT NONE

  ! ENUM, BIND(C)
  !    ENUMERATOR :: LINK_ENUM = 0
  !    ENUMERATOR :: FLUVIAL_TYPE = 1
  !    ENUMERATOR :: DISCHARGE_TYPE = 2
  !    ENUMERATOR :: UPSTREAM_STAGE_TYPE = 3
  !    ENUMERATOR :: DOWNSTREAM_STAGE_TYPE = 4
  !    ENUMERATOR :: TRIBUTARY_TYPE = 5
  !    ENUMERATOR :: HYDRO_TYPE = 6
  !    ENUMERATOR :: DISCHARGE_PID_TYPE = 12
  !    ENUMERATOR :: STAGE_PID_TYPE = 13
  !    ENUMERATOR :: FLUVIAL_TDGSAT_TYPE = 20
  !    ENUMERATOR :: FLUVIAL_HYDRO_TYPE = 21
  ! END ENUM

  ! ----------------------------------------------------------------
  ! link_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_manager_t
     TYPE (link_list) :: links
     CLASS (link_t), POINTER :: dslink
     INTEGER :: maxorder
   CONTAINS
     PROCEDURE, NOPASS :: scan => link_manager_scan
     PROCEDURE :: read => link_manager_read
     PROCEDURE, PRIVATE :: readpts => link_manager_readpts
     PROCEDURE, PRIVATE :: readpid => link_manager_readpid
     PROCEDURE :: maxid => link_manager_maxid
     PROCEDURE :: find => link_manager_find
     PROCEDURE :: connect => link_manager_connect
     PROCEDURE :: forward => link_manager_flow_forward
     PROCEDURE :: backward => link_manager_flow_backward
     PROCEDURE :: hyupdate => link_manager_hyupdate
     PROCEDURE :: read_restart => link_manager_read_restart
     PROCEDURE :: write_restart => link_manager_write_restart
     PROCEDURE :: read_trans_restart => link_manager_read_trans_restart
     PROCEDURE :: write_trans_restart => link_manager_write_trans_restart
     PROCEDURE :: transport_steps => link_manager_transport_steps
     PROCEDURE :: transport_interp => link_manager_transport_interp
     PROCEDURE :: transport => link_manager_transport
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
  ! SUBROUTINE link_manager_readpid
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_readpid(this, theconfig, bcman, npid)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(INOUT) :: theconfig
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    INTEGER, INTENT(IN) :: npid

    INTEGER, PARAMETER :: iounit = 33
    DOUBLE PRECISION, PARAMETER :: bogus = -999.0
  

    CLASS (link_t), POINTER :: link
    CLASS (pid_link), POINTER :: pidlink

    INTEGER :: recno, ierr, iostat
    INTEGER :: linkid
    DOUBLE PRECISION :: kc, ti, tr, lagvalues(2*max_pid_lag)
    DOUBLE PRECISION :: lagtime

    INTEGER :: i, lagid
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    iostat = 0


    CALL open_existing(theconfig%pid_file, iounit, fatal=.TRUE.)

    msg = 'Reading PID link information from ' // TRIM(theconfig%pid_file)
    CALL status_message(msg)

    DO recno = 1, npid

       lagvalues = bogus

       READ (iounit, *, IOSTAT=iostat) linkid, kc, ti, tr, lagvalues

       IF (IS_IOSTAT_END(iostat)) THEN
          WRITE(msg, *) TRIM(theconfig%pid_file) // &
               &', record ', recno, ': premature end of file, expected ', npid,&
               &' records'
          ierr = ierr + 1
          CALL error_message(msg)
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) TRIM(theconfig%pid_file) // &
               &': error in or near record ', recno
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF

       link => this%find(linkid)
       IF (.NOT. ASSOCIATED(link)) THEN
          WRITE(msg, *) TRIM(theconfig%pid_file) // &
               &', record ', recno, ': unknown link ', linkid
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END IF
       
       SELECT TYPE(link)
       CLASS IS (pid_link)
          pidlink => link
       CLASS DEFAULT
          WRITE (msg, *) TRIM(theconfig%pid_file) // &
               &', record ', recno, ': link ', linkid, &
               &' is not a PID link'
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END SELECT
       
       pidlink%kc = kc
       pidlink%ti = ti
       pidlink%tr = tr
       pidlink%errsum = 0.0

       IF (pidlink%followflow) THEN
          WRITE (msg,*) 'PID link #', recno, ' is link ', pidlink%id, ' and follows dischange'
       ELSE
          WRITE (msg,*) 'PID link #', recno, ' is link ', pidlink%id, ' and follows stage'
       END IF
       CALL status_message(msg)
       WRITE (msg, *) 'PID Coefficients are as follows: '
       CALL status_message(msg)
       WRITE (msg, *) '   Kc = ', pidlink%kc
       CALL status_message(msg)
       WRITE (msg, *) '   Ti = ', pidlink%ti
       CALL status_message(msg)
       WRITE (msg, *) '   Tr = ', pidlink%tr
       CALL status_message(msg)
       
       ! count the number of flows (signals) that are to be lagged
       
       DO i = 1, max_pid_lag
          IF (lagvalues(i*2 - 1) .LE. -999.0) EXIT
       END DO
       pidlink%numflows = i - 1

       IF (pidlink%numflows .LE. 0) THEN
          WRITE(msg,*) TRIM(theconfig%pid_file),  &
               &': no lagged signals specified for link ', pidlink%id
          CALL error_message(msg)
          ierr = ierr + 1
       ELSE IF (pidlink%followflow .AND. pidlink%numflows .GT. 1) THEN
          WRITE(msg,*) TRIM(theconfig%pid_file),&
               &': too many lagged stages specified for link ', pidlink%id
          CALL error_message(msg)
          ierr = ierr + 1
       END IF

       ALLOCATE(pidlink%lagged(pidlink%numflows))
       
       IF (pidlink%followflow) THEN
          WRITE (msg,*) 'PID link #', pidlink%id, 'uses the following lagged stage data: '
       ELSE
          WRITE (msg,*) 'PID link #', pidlink%id, 'uses the following lagged flow data: '
       END IF
       CALL status_message(msg)

       DO i = 1, pidlink%numflows

          ASSOCIATE (rec => pidlink%lagged(i))
            
            ! identify and check the specified link
            
            NULLIFY(rec%bc)
            NULLIFY(rec%link)
            lagid = INT(lagvalues(i*2 - 1))

            IF (lagid .LT. 0) THEN
               lagid = -lagid
               rec%bc => bcman%find(LINK_BC_TYPE, lagid)
               IF (.NOT. ASSOCIATED(rec%bc)) THEN
                  WRITE(msg, *) TRIM(theconfig%pid_file), ': invalid link BC ', lagid, &
                       &' for link ', pidlink%id
                  CALL error_message(msg)
                  ierr = ierr + 1
               END IF
            ELSE IF (pidlink%followflow) THEN
               WRITE(msg, *) TRIM(theconfig%pid_file), ': record ', recno, &
                    &': link ', pidlink%id, ' must specify a lagged stage link BC'
               CALL error_message(msg)
               ierr = ierr + 1
            ELSE
               rec%link => this%find(lagid)
               IF (.NOT. ASSOCIATED(rec%link)) THEN
                  WRITE(msg, *) TRIM(theconfig%pid_file), ': record ', recno, &
                       &': link ', pidlink%id, 'uses lagged flow from link ', lagid, &
                       &', which is not a valid link '
                  CALL error_message(msg)
                  ierr = ierr + 1
               END IF
            END IF

            ! check the specified lag

            lagtime = lagvalues(i*2)
            IF (lagtime .LT. 0) THEN
               WRITE (msg,*) TRIM(theconfig%pid_file), ': record ', recno, &
                    &': link ', pidlink%id, ': invalid lag time: ', lagtime
               CALL error_message(msg)
               ierr = ierr + 1
            END IF
            rec%lag = lagtime

            ! initialize the remainder of the
            ! record

            rec%nlag = MAX(INT(rec%lag/theconfig%time%step + 0.5), 1)
            ALLOCATE(rec%flow(rec%nlag))

            IF (ASSOCIATED(rec%bc)) THEN
               WRITE (msg,*) '     Link Boundary Condition #', rec%link%id, &
                    &' lagged ', rec%lag, ' days'
            ELSE
               WRITE (msg,*) '     Point 1 on Link #', rec%link%id, &
                    &' lagged ', rec%lag, ' days'
            END IF
            CALL status_message(msg)
          END ASSOCIATE
       END DO

    END DO

    CLOSE(iounit)

    IF (ierr .GT. 0) THEN
       WRITE(msg, *) TRIM(theconfig%pid_file) // &
            &': too many errors'
       CALL error_message(msg, fatal=.TRUE.)

    ELSE 
       WRITE(msg, *) TRIM(theconfig%pid_file) // &
            &': read completed'
       CALL status_message(msg)
    END IF

  END SUBROUTINE link_manager_readpid


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_scan
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_scan(theconfig)

    IMPLICIT NONE

    TYPE (configuration_t), INTENT(INOUT) :: theconfig
    
    INTEGER, PARAMETER :: lunit = 21
    INTEGER :: recno, ierr, iostat
    TYPE (link_input_data) :: ldata
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    recno = 0

    CALL open_existing(theconfig%link_file, lunit, fatal=.TRUE.)
    
    DO WHILE (.TRUE.) 
       recno = recno + 1
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
          ierr = ierr + 1
          EXIT
       END IF

       SELECT CASE (ldata%ltype)
       CASE (1)
       CASE (20) 
       CASE (21)
          theconfig%do_hydro_bc = .TRUE.
       CASE (2)
       CASE (6)
          theconfig%do_hydro_bc = .TRUE.
       CASE (3)
       CASE (5)
       CASE (12)
       CASE (13)
       CASE (60)
       CASE DEFAULT
          WRITE(msg, *) TRIM(theconfig%link_file), ': link record ', recno, &
               &': link type unknown (', ldata%ltype, ')'
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END SELECT

    END DO

    CLOSE (lunit)

    IF (ierr .GT. 0) THEN
       msg = TRIM(theconfig%link_file) // ': too many errors in link file'
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE link_manager_scan



  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_read
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_read(this, theconfig, bcman, sectman, sclrman, metman)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(INOUT) :: theconfig
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (link_t), POINTER :: link
    CLASS (section_handler), INTENT(INOUT) :: sectman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman
    INTEGER, PARAMETER :: lunit = 21
    INTEGER :: recno, ierr, iostat, npid
    TYPE (link_input_data) :: ldata
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    recno = 0
    npid = 0

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

       ldata%gravity = theconfig%grav

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
          theconfig%do_hydro_bc = .TRUE.
       CASE (2)
          ALLOCATE(discharge_link :: link)
       CASE (6)
          ALLOCATE(hydro_link :: link)
          theconfig%do_hydro_bc = .TRUE.
       CASE (3)
          ALLOCATE(ustage_link :: link)
       CASE (5)
          ALLOCATE(trib_inflow_link :: link)
       CASE (12)
          ALLOCATE(pid_flow_link :: link)
          npid = npid + 1
       CASE (13)
          ALLOCATE(pid_link :: link)
          npid = npid + 1
       CASE (60)
          ALLOCATE(hydrologic_link :: link)
       CASE DEFAULT
          WRITE(msg, *) TRIM(theconfig%link_file), ': link record ', recno, &
               &': link type unknown (', ldata%ltype, ')'
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END SELECT

       ! Wouldn't it be nice to have constructors?

       CALL link%construct()

       IF (link%initialize(ldata, bcman, sclrman, metman) .NE. 0) THEN
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

    IF (npid .GT. 0) THEN
       CALL this%readpid(theconfig, bcman, npid)
    END IF

    ! have all the links do a sanity check

    ierr = 0
    CALL this%links%begin()
    link => this%links%current()
    
    DO WHILE (ASSOCIATED(link))
       ierr = ierr + link%check()
       CALL this%links%next()
       link => this%links%current()
    END DO

    IF (ierr .GT. 0) THEN
       msg = TRIM(theconfig%link_file) // ': link errors'
       CALL error_message(msg, fatal = .TRUE.)
    END IF
   
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
       NULLIFY(link%ucon)
       NULLIFY(link%dcon)
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
       CALL error_message("Too many downstream links found, there can be only one")
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
          IF (.NOT. ASSOCIATED(dlink%ucon)) THEN 
             ALLOCATE(con)
             con = confluence_t(dlink)
             dlink%ucon => con
             NULLIFY(con)
          END IF
          CALL dlink%ucon%ulink%push(link)
          link%dcon => dlink%ucon
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
       IF (ASSOCIATED(dlink%ucon)) THEN
          CALL dlink%ucon%ulink%begin()
          link => dlink%ucon%ulink%current()
          DO WHILE (ASSOCIATED(link))
             WRITE(lbl, '(I4)') link%id
             msg = TRIM(msg) // " " // TRIM(lbl)
             CALL dlink%ucon%ulink%next()
             link => dlink%ucon%ulink%current()
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
  ! INTEGER FUNCTION link_manager_maxid
  ! ----------------------------------------------------------------
  INTEGER FUNCTION link_manager_maxid(this)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this

    CLASS (link_t), POINTER :: link

    link_manager_maxid = 0

    CALL this%links%begin()
    link => this%links%current()
    DO WHILE (ASSOCIATED(link))
       link_manager_maxid = MAX(link_manager_maxid, link%id)
       CALL this%links%next()
       link => this%links%current()
    END DO
    
    
  END FUNCTION link_manager_maxid
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_flow_forward
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_flow_forward(this, deltat)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat

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

  END SUBROUTINE link_manager_flow_forward

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_flow_backward
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_flow_backward(this, deltat, grav, unitwt, dsbc_type)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat, grav, unitwt
    INTEGER, INTENT(IN) :: dsbc_type

    CLASS (link_t), POINTER :: link
    INTEGER :: l

    DO l = this%maxorder, 1, -1
       CALL this%links%begin()
       link => this%links%current()
       DO WHILE (ASSOCIATED(link))
          IF (link%order .EQ. l) THEN
             CALL link%backward_sweep(dsbc_type)
             CALL link%hydro_update(grav, unitwt, deltat)
          END IF
          CALL this%links%next()
          link => this%links%current()
       END DO
    END DO

  END SUBROUTINE link_manager_flow_backward

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_hyupdate
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_hyupdate(this, grav, unitwt, dt)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: grav, unitwt, dt
    CLASS (link_t), POINTER :: link
    
    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%hydro_update(grav, unitwt, dt)
       CALL this%links%next()
       link => this%links%current()
    END DO

  END SUBROUTINE link_manager_hyupdate

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_read_restart(this, iounit)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%read_restart(iounit)
       CALL this%links%next()
       link => this%links%current()
    END DO
    
  END SUBROUTINE link_manager_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_read_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_read_trans_restart(this, iounit, nspecies)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit, nspecies
    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%read_trans_restart(iounit, nspecies)
       CALL this%links%next()
       link => this%links%current()
    END DO
    
  END SUBROUTINE link_manager_read_trans_restart

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
  ! SUBROUTINE link_manager_write_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_write_trans_restart(this, iounit, nspecies)

    IMPLICIT NONE

    CLASS (link_manager_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit, nspecies
    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%write_trans_restart(iounit, nspecies)
       CALL this%links%next()
       link => this%links%current()
    END DO

  END SUBROUTINE link_manager_write_trans_restart

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION link_manager_transport_steps
  ! ----------------------------------------------------------------
  FUNCTION link_manager_transport_steps(this, dt) RESULT(nstep)

    IMPLICIT NONE
    INTEGER :: nstep
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt

    DOUBLE PRECISION :: cmax, dmax, a_transdt, d_transdt
    CLASS (link_t), POINTER :: link

    DOUBLE PRECISION, PARAMETER :: max_courant = 0.99, max_diffuse = 0.99

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       cmax = MAX(cmax, link%max_courant(dt))
       dmax = MAX(dmax, link%max_diffuse(dt))

       CALL this%links%next()
       link => this%links%current()
    END DO

    IF (cmax .GT. max_courant) THEN 
       a_transdt = max_courant/cmax*dt
    ELSE 
       a_transdt = dt
    END IF

    IF (dmax .GT. max_diffuse) THEN 
       d_transdt = max_diffuse/dmax*dt
    ELSE 
       d_transdt = dt
    END IF

    a_transdt = MIN(a_transdt, d_transdt)

    nstep = FLOOR(dt/a_transdt)

    IF (FRACTION(dt/a_transdt) .GT. EPSILON(a_transdt)) &
         &nstep = nstep + 1

    nstep = MAX(nstep, 1)

  END FUNCTION link_manager_transport_steps


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_transport_interp
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_transport_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1
    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%trans_interp(tnow, htime0, htime1)

       CALL this%links%next()
       link => this%links%current()
    END DO
  END SUBROUTINE link_manager_transport_interp


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_transport
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_transport(this, ispec, tdeltat)

    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(IN) :: tdeltat
    
    CLASS (link_t), POINTER :: link

    CALL this%links%begin()
    link => this%links%current()

    DO WHILE (ASSOCIATED(link))
       CALL link%transport(ispec, tdeltat)

       CALL this%links%next()
       link => this%links%current()
    END DO

  END SUBROUTINE link_manager_transport


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_destroy(this)
    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    CALL this%links%clear()
  END SUBROUTINE link_manager_destroy


  
END MODULE link_manager_module
