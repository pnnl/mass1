! ----------------------------------------------------------------
! file: network.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 10, 2017 by William A. Perkins
! Last Change: 2018-01-08 14:00:33 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE network_module
! ----------------------------------------------------------------
MODULE network_module

  USE utility
  USE date_time
  USE mass1_config
  USE link_manager_module
  USE bc_module
  USE section_handler_module
  USE met_zone

  IMPLICIT NONE

  PRIVATE 

  TYPE, PUBLIC :: network
     CHARACTER (LEN=path_length) :: basedir
     TYPE (configuration_t) :: config
     TYPE (bc_manager_t) :: bcs
     TYPE (met_zone_manager_t) :: met
     TYPE (section_handler) :: sections
     TYPE (link_manager_t) :: links

   CONTAINS
     PROCEDURE :: readbcs => network_read_bcs
     PROCEDURE :: read => network_read
     PROCEDURE :: set_initial => network_set_initial
     PROCEDURE :: read_restart => network_read_restart
     PROCEDURE :: initialize => network_initialize
     PROCEDURE :: flow => network_flow
     PROCEDURE :: run => network_run
     PROCEDURE :: destroy => network_destroy
  END type network

  INTERFACE network
     MODULE PROCEDURE new_network
  END INTERFACE network

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_network
  ! ----------------------------------------------------------------
  FUNCTION new_network() RESULT(net)

    IMPLICIT NONE
    TYPE (network) :: net
    net%bcs = new_bc_manager()
    net%met = met_zone_manager_t()
    net%sections = new_section_handler()
    net%links = new_link_manager()

  END FUNCTION new_network


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_read_bcs
  ! ----------------------------------------------------------------
  SUBROUTINE network_read_bcs(this)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%bcs%read(LINK_BC_TYPE, this%config%linkbc_file)
    IF (this%config%do_hydro_bc) THEN
       CALL this%bcs%read(HYDRO_BC_TYPE, this%config%hydrobc_file)
    END IF
    IF(this%config%debug_print)WRITE(11,*)'link BC data done'

    IF(this%config%do_latflow)THEN
       CALL this%bcs%read(LATFLOW_BC_TYPE, this%config%lateral_file)
       IF(this%config%debug_print)WRITE(11,*)'lateral inflow BC data done'
    ENDIF

    IF(this%config%do_gas)THEN
       CALL this%bcs%read(TRANS_BC_TYPE, this%config%transbc_file)
       IF(this%config%debug_print) WRITE(11,*)'done reading gas transport table'

       ! CALL allocate_tdg_coeff(this%config%maxlinks,utility_status_iounit, utility_error_iounit)
       ! ! read tdg spill coefficient tables
       ! ! if linktype 6,20, or 21 is there, then open and read file
       ! DO link=1,this%config%maxlinks
       !    SELECT CASE(linktype(link)) 
       !    CASE(6,21)
       !       CALL tdg_coeff_read(utility_status_iounit, utility_error_iounit)
       !       EXIT
       !    END SELECT
       ! END DO

       IF(this%config%gas_exchange)THEN
          CALL open_existing(this%config%tdg_coeff_file, 88, fatal=.TRUE.)
          READ(88,*)gasx_a,gasx_b,gasx_c, gasx_d
          CLOSE(88)
       ENDIF
    ENDIF

    IF(this%config%do_temp)THEN
       CALL this%bcs%read(TEMP_BC_TYPE, this%config%tempbc_file)
       IF(this%config%debug_print) WRITE(11,*)'done reading temp transport table'
    ENDIF


    IF (this%config%met_required) THEN
       CALL this%met%read(this%config%weather_file)
    END IF




  END SUBROUTINE network_read_bcs


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_read

  ! ----------------------------------------------------------------
  SUBROUTINE network_read(this, base)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: base
    INTEGER :: istatus
    CHARACTER(LEN=path_length) :: cwd, mybase


    istatus = getcwd(cwd)       ! FIXME: GNU specfic
    IF (istatus .NE. 0) THEN 
       CALL error_message("network_read: cannot get current working directory")
    END IF
    istatus = chdir(base)
    IF (istatus .NE. 0) then
       WRITE(mybase, *) "network_read: cannot change to ", TRIM(base), " from ", TRIM(cwd)
       CALL error_message(mybase, fatal=.TRUE.)
    END IF
    istatus = getcwd(cwd)       ! FIXME: GNU specfic
    IF (istatus .NE. 0) THEN 
       CALL error_message("network_read: cannot get current working directory")
    END IF
    this%basedir = cwd

    CALL this%config%read()
    CALL this%readbcs()
    CALL this%sections%read(this%config%section_file)
    CALL this%links%read(this%config, this%bcs, this%sections)
    CALL this%links%connect()

  END SUBROUTINE network_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_set_initial
  ! ----------------------------------------------------------------
  SUBROUTINE network_set_initial(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    INTEGER :: linkid, recno, iostat, ierr
    CLASS (link_t), POINTER :: link
    DOUBLE PRECISION :: stage, discharge
    DOUBLE PRECISION, ALLOCATABLE :: c(:)
    INTEGER, PARAMETER :: iunit = 25
    CHARACTER (LEN=1024) :: msg

    ! FIXME: transport
    ALLOCATE(c(2))

    ierr = 0
    recno = 0

    CALL open_existing(this%config%initial_file, iunit)

    ! FIXME: test to make sure all links are initialized
    DO WHILE (.TRUE.)
       recno = recno + 1
       READ (iunit, *, IOSTAT=iostat) linkid, stage, discharge, c(1), c(2)
       IF (IS_IOSTAT_END(iostat)) THEN
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) TRIM(this%config%initial_file),&
               &': read error near line ', recno
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF

       NULLIFY(link)
       link => this%links%find(linkid)

       IF (ASSOCIATED(link)) THEN
          WRITE(msg, *) TRIM(this%config%initial_file), &
               &': setting initial conditions for link ', link%id
          CALL status_message(msg)
          CALL link%set_initial(stage, discharge, c)
       ELSE 
          WRITE(msg, *) TRIM(this%config%initial_file),&
               &': error, line ', recno, &
               &': unknown link id (', linkid, ')'
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    END DO

    CLOSE(iunit)

    IF (ierr .GT. 0) THEN
       WRITE(msg, *) TRIM(this%config%initial_file), &
            &': too many errors reading initial conditions'
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE network_set_initial


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE network_read_restart(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link
    INTEGER, PARAMETER :: runit = 31
    CHARACTER (LEN=1024) :: msg

    CALL open_existing(this%config%restart_load_file, runit, form='UNFORMATTED')

    WRITE(msg, *) 'reading restart from ', TRIM(this%config%restart_load_file)
    CALL status_message(msg)

    CALL this%links%links%begin()
    link => this%links%links%current()

    DO WHILE (ASSOCIATED(link)) 
       WRITE(msg, *) 'reading intial state for link ', link%id
       CALL status_message(msg)
       CALL link%read_restart(runit)
       CALL this%links%links%next()
       link => this%links%links%current()
    END DO

    WRITE (msg, *) 'done reading restart from ', TRIM(this%config%restart_load_file)
    CALL status_message(msg)
    CLOSE(runit)

  END SUBROUTINE network_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE network_initialize(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    IF (this%config%do_hotstart) THEN
       CALL this%read_restart()
    ELSE
       CALL this%set_initial()
    END IF

  END SUBROUTINE network_initialize


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_flow
  ! ----------------------------------------------------------------
  SUBROUTINE network_flow(this)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%links%flow_sim(this%config%time%time, this%config%time%delta_t)

  END SUBROUTINE network_flow

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_run
  ! ----------------------------------------------------------------
  SUBROUTINE network_run(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    INTEGER :: nstep
    CHARACTER (LEN=20) :: date_string, time_string

    nstep = 0
    this%config%time%time = this%config%time%begin

    DO WHILE(this%config%time%time .LT. this%config%time%end)

       ! print out initial conditions

       ! IF (time == config%time%begin) THEN
       !    IF (config%do_printout) CALL print_output("RESULT", time)
       !    IF (config%do_profileout) CALL profile_output
       !    IF (config%do_gageout) CALL gage_output
       ! END IF

       CALL this%bcs%update(this%config%time%time)
       CALL this%met%update(this%config%time%time)

       IF (config%do_flow) THEN
          CALL this%flow()
       ENDIF


       ! update time step here so correct
       ! time is placed in output

       ! model_time = model_time + time_step
       nstep = nstep + 1
       this%config%time%time = this%config%time%begin + nstep*this%config%time%step

       CALL decimal_to_date(this%config%time%time, date_string, time_string)
       WRITE(*,*) 'Done Crunching through ** Date: ', &
            &TRIM(date_string),'  Time: ', TRIM(time_string)
    END DO

  END SUBROUTINE network_run


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE network_destroy(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%bcs%destroy()
    CALL this%sections%destroy()
    CALL this%links%destroy()

  END SUBROUTINE network_destroy




END MODULE network_module

