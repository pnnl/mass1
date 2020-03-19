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
! Last Change: 2020-03-19 07:53:38 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE network_module
! ----------------------------------------------------------------
MODULE network_module

#ifdef __INTEL_COMPILER
  ! for getcwd() and chdir()
  USE ifport
#endif
  USE utility
  USE date_time
  USE mass1_config
  USE link_manager_module
  USE bc_module
  USE section_handler_module
  USE met_zone
  USE gage_module
  USE profile_module
  USE scalar_module

  IMPLICIT NONE

  PRIVATE 

  TYPE, PUBLIC :: network
     CHARACTER (LEN=path_length) :: basedir
     TYPE (configuration_t) :: config
     TYPE (bc_manager_t) :: bcs
     TYPE (met_zone_manager_t) :: met
     TYPE (section_handler) :: sections
     TYPE (link_manager_t) :: links
     TYPE (gage_manager) :: gages
     TYPE (profile_manager) :: profiles
     TYPE (scalar_manager) :: scalars
   CONTAINS
     PROCEDURE :: readbcs => network_read_bcs
     PROCEDURE :: read => network_read
     PROCEDURE :: set_initial => network_set_initial
     PROCEDURE :: read_restart => network_read_restart
     PROCEDURE :: write_restart => network_write_restart
     PROCEDURE :: initialize => network_initialize
     PROCEDURE :: forward => network_forward
     PROCEDURE :: backward => network_backward
     PROCEDURE :: flow => network_flow
     PROCEDURE :: transport => network_transport
     PROCEDURE :: run => network_run
     PROCEDURE :: run_to => network_run_to_time
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
    net%bcs = bc_manager_t()
    net%met = met_zone_manager_t()
    net%sections = section_handler()
    net%links = link_manager_t()
    net%gages = gage_manager()

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


    CALL this%bcs%update(this%config%time%begin)
    CALL this%met%update(this%config%time%begin)


  END SUBROUTINE network_read_bcs


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_read
  ! 
  ! ----------------------------------------------------------------
  SUBROUTINE network_read(this, base, dotemp_override, dobed_override)
    USE general_vars
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: base
    LOGICAL, INTENT(IN), OPTIONAL :: dotemp_override, dobed_override
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
    istatus = getcwd(mybase)       ! FIXME: GNU specfic
    IF (istatus .NE. 0) THEN 
       CALL error_message("network_read: cannot get current working directory")
    END IF
    this%basedir = mybase

    CALL this%config%read()

    ! Some things can be overridden (i.e. DHSVM) can override some
    ! things in the configuration
    IF (PRESENT(dotemp_override)) THEN
       this%config%do_temp = dotemp_override
       this%config%do_transport = this%config%do_temp
       this%config%met_required = &
            &(this%config%do_temp .AND. this%config%temp_exchange)
       
       IF (PRESENT(dobed_override) .AND. this%config%do_temp) THEN
          this%config%do_temp_bed = dobed_override
       END IF
    END IF

    ! things that should be in the configuration

    depth_minimum = 0.003           ! m
    depth_threshold = 0.1          ! m
    
    SELECT CASE(this%config%units)
       
    CASE(ENGLISH_UNITS)
       depth_minimum = depth_minimum/0.3048
       depth_threshold = depth_threshold/0.3048
       
    END SELECT

    CALL this%scalars%initialize(this%config)
    CALL this%links%scan(this%config)
    CALL this%readbcs()
    CALL this%sections%read(this%config%section_file)
    CALL this%links%read(this%config, this%bcs, this%sections, this%scalars, this%met)
    CALL this%links%connect()
    IF (this%config%do_gageout) &
         &CALL this%gages%read(this%config%gage_file, this%links)
    IF (this%config%do_profileout) THEN
       this%profiles = profile_manager()
       CALL this%profiles%read(this%config, this%links)
    END IF

    istatus = chdir(cwd)
    IF (istatus .NE. 0) then
       WRITE(mybase, *) "network_read: cannot change to ", TRIM(cwd), " from ", TRIM(base)
       CALL error_message(mybase, fatal=.TRUE.)
    END IF

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

    ! FIXME: transport: arbitrary scalars
    ALLOCATE(c(2))

    ierr = 0
    recno = 0

    CALL open_existing(this%config%initial_file, iunit)

    ! FIXME: test to make sure all links are initialized
    DO WHILE (.TRUE.)
       recno = recno + 1
       READ (iunit, *, IOSTAT=iostat) linkid, discharge, stage, c(1), c(2)
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

       ! Backward compatibility: c(1) tdg conc, c(2) is temperature
       ! If TDG is simulated the concentrations are correct.
       ! If only temperature is simulated, put the initial temp in c(1)
       IF ((.NOT. this%config%do_gas) .AND. this%config%do_temp) THEN
          c(1) = c(2)
       END IF

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

    DEALLOCATE(c)

    CALL this%links%hyupdate(this%config%grav, &
         &this%config%unit_weight_h2o, &
         &this%config%time%delta_t)

  END SUBROUTINE network_set_initial


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE network_read_restart(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    INTEGER, PARAMETER :: runit = 31
    INTEGER :: iostat, nspecies, s
    INTEGER (KIND=KIND(BC_ENUM)) :: stype(10) ! FIXME: fixed array length
    CHARACTER (LEN=1024) :: msg

    CALL open_existing(this%config%restart_load_file, runit, form='UNFORMATTED')

    WRITE(msg, *) 'reading restart from ', TRIM(this%config%restart_load_file)
    CALL status_message(msg)

    CALL this%links%read_restart(runit)

    IF (this%config%do_transport) THEN
       READ (runit, IOSTAT=iostat) nspecies
       IF (IS_IOSTAT_END(iostat)) THEN
          WRITE(msg, *) TRIM(this%config%restart_load_file), &
               &": no transport state found, hoping for the best"
          CALL error_message(msg)
       ELSE
          READ (runit, IOSTAT=iostat) (stype(s), s = 1, nspecies)
          IF (iostat .NE. 0) THEN
             WRITE(msg, *) TRIM(this%config%restart_load_file), &
                  &": premature end of file in transport"
             CALL error_message(msg, fatal=.TRUE.)
          END IF
          IF (nspecies .NE. this%scalars%nspecies) THEN
             WRITE(msg, *) TRIM(this%config%restart_load_file), &
                  &":  number of trannsported species do not match"
             CALL error_message(msg, fatal=.TRUE.)
          END IF

          DO s = 1, nspecies
             IF (stype(s) .NE. this%scalars%species(s)%p%bctype) THEN
                WRITE(msg, *)  TRIM(this%config%restart_load_file), &
                     &": type mismatch for species ", s
                CALL error_message(msg, fatal=.TRUE.)
             END IF
          END DO
          CALL this%links%read_trans_restart(runit, nspecies)
       END IF
    END IF


    WRITE (msg, *) 'done reading restart from ', TRIM(this%config%restart_load_file)
    CALL status_message(msg)
    CLOSE(runit)

    CALL this%links%hyupdate(this%config%grav, &
         &this%config%unit_weight_h2o, &
         &this%config%time%delta_t)

  END SUBROUTINE network_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE network_write_restart(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    INTEGER, PARAMETER :: runit = 31
    INTEGER :: status, i
    CHARACTER (LEN=1024) :: msg

    OPEN(runit, file=this%config%restart_save_file, form='UNFORMATTED', iostat=status)

    IF (status .EQ. 0) THEN
       WRITE(msg, *) 'writing restart te ', TRIM(this%config%restart_save_file)
       CALL status_message(msg)
    ELSE 
       WRITE(msg, *) 'cannot open restart file: ', TRIM(this%config%restart_save_file)
       CALL error_message(msg, fatal=.TRUE.)
    END IF

    CALL this%links%write_restart(runit)

    IF (this%config%do_transport) THEN
       WRITE(runit, IOSTAT=status) this%scalars%nspecies
       IF (status .NE. 0) THEN
          WRITE(msg, *) TRIM(this%config%restart_save_file), &
               &": error writing hotstart"
          CALL error_message(msg, fatal=.TRUE.)
       END IF
       WRITE(runit, IOSTAT=status) &
            &(this%scalars%species(i)%p%bctype, i = 1, this%scalars%nspecies)
       CALL this%links%write_trans_restart(runit, this%scalars%nspecies)
    END IF

    WRITE (msg, *) 'done writing restart to ', TRIM(this%config%restart_save_file)
    CALL status_message(msg)
    CLOSE(runit)

  END SUBROUTINE network_write_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE network_initialize(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    INTEGER :: istatus
    CHARACTER(LEN=path_length) :: cwd, msg


    istatus = getcwd(cwd)       ! FIXME: GNU specfic
    IF (istatus .NE. 0) THEN 
       CALL error_message("network_initialize: cannot get current working directory")
    END IF
    istatus = chdir(this%basedir)
    IF (istatus .NE. 0) then
       WRITE(msg, *) "network_initialize: cannot change to ", &
            &TRIM(this%basedir), " from ", TRIM(cwd)
       CALL error_message(msg, fatal=.TRUE.)
    END IF

    IF (this%config%do_hotstart) THEN
       CALL this%read_restart()
    ELSE
       CALL this%set_initial()
    END IF

    istatus = chdir(cwd)
    IF (istatus .NE. 0) then
       WRITE(msg, *) "network_initialize: cannot change to ", &
            &TRIM(cwd), " from ", TRIM(this%basedir)
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE network_initialize

  
  ! ----------------------------------------------------------------
  ! SUBROUTINE network_forward
  ! ----------------------------------------------------------------
  SUBROUTINE network_forward(this)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%links%forward(this%config%time%delta_t)

  END SUBROUTINE network_forward

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_backward
  ! ----------------------------------------------------------------
  SUBROUTINE network_backward(this)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%links%backward(this%config%time%delta_t, &
         &this%config%grav, this%config%unit_weight_h2o, &
         &this%config%dsbc_type)

  END SUBROUTINE network_backward


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_flow
  ! ----------------------------------------------------------------
  SUBROUTINE network_flow(this)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    CALL this%forward()
    CALL this%backward()

  END SUBROUTINE network_flow

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_transport
  ! ----------------------------------------------------------------
  SUBROUTINE network_transport(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    DOUBLE PRECISION :: htime0, htime1
    DOUBLE PRECISION :: tnow, tdeltat
    INTEGER :: tsteps, i, ispec

    htime0 = this%config%time%time
    htime1 = htime0 + this%config%time%step

    tnow = htime0

    CALL this%links%pre_transport()
    
    IF (this%config%scalar_steps .GT. 0) THEN
       tsteps = this%config%scalar_steps
    ELSE
       tsteps = this%links%transport_steps(this%config%time%step)
       IF (.NOT. this%config%quiet) THEN
          WRITE(*, '(" Using ", I5, " transport steps")') tsteps
       END IF
    END IF
    tdeltat = this%config%time%delta_t
    tdeltat = tdeltat/DBLE(tsteps)

    DO i = 1, tsteps
       CALL this%bcs%update(tnow)
       CALL this%met%update(tnow)
       CALL this%links%transport_interp(&
            &tnow + this%config%time%step/DBLE(tsteps), htime0, htime1)
       DO ispec = 1, this%scalars%nspecies
          CALL this%links%transport(ispec, tdeltat)
       END DO
       tnow = htime0 + i*this%config%time%step/DBLE(tsteps)
    END DO
    
    

  END SUBROUTINE network_transport


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_run_to_time
  ! ----------------------------------------------------------------
  SUBROUTINE network_run_to_time(this, tend)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tend

    CHARACTER (LEN=20) :: date_string, time_string
    LOGICAL :: dooutput

    dooutput = .FALSE.

    DO WHILE(this%config%time%time .LT. tend)

       ! print out initial conditions

       CALL this%bcs%update(this%config%time%time)
       CALL this%met%update(this%config%time%time)

       IF (this%config%do_flow) THEN
          CALL this%flow()
       ENDIF

       IF (this%config%do_transport) THEN
          CALL this%transport()
       END IF

       ! update time step here so correct
       ! time is placed in output

       ! model_time = model_time + time_step
       this%config%time%current_step = this%config%time%current_step + 1
       this%config%time%time = this%config%time%begin + &
            &this%config%time%current_step*this%config%time%step

       dooutput = (MOD(this%config%time%current_step, this%config%print_freq) == 0)
       
       IF (dooutput) THEN
          IF (this%config%do_gageout) &
               &CALL this%gages%output(this%config%time%time, this%scalars)
          IF (this%config%do_profileout) &
               &CALL this%profiles%output(this%config%time%time, this%scalars)
       END IF
          
       CALL decimal_to_date(this%config%time%time, date_string, time_string)
       IF (.NOT. this%config%quiet) THEN
          WRITE(*,*) 'Done Crunching through ** Date: ', &
               &TRIM(date_string),'  Time: ', TRIM(time_string)
       END IF
    END DO
    

  END SUBROUTINE network_run_to_time


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_run
  ! ----------------------------------------------------------------
  SUBROUTINE network_run(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    LOGICAL :: dooutput

    this%config%time%time = this%config%time%begin

    ! make sure initial conditions are in the output
    IF (this%config%do_gageout) &
         &CALL this%gages%output(this%config%time%time, this%scalars)
    IF (this%config%do_profileout) &
         &CALL this%profiles%output(this%config%time%time, this%scalars)

    CALL this%run_to(this%config%time%end)

    ! always write the last state, if not done already
    dooutput = (MOD(this%config%time%current_step, this%config%print_freq) == 0)
    IF (.NOT. dooutput) THEN
       IF (this%config%do_gageout) &
            &CALL this%gages%output(this%config%time%time, this%scalars)
       IF (this%config%do_profileout) &
            &CALL this%profiles%output(this%config%time%time, this%scalars)
    END IF

    IF (this%config%do_restart) THEN
       CALL this%write_restart()
    END IF


  END SUBROUTINE network_run

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE network_destroy(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%gages%destroy()
    CALL this%bcs%destroy()
    CALL this%sections%destroy()
    CALL this%links%destroy()

  END SUBROUTINE network_destroy




END MODULE network_module

