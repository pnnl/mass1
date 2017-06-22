!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: mass1
!
! VERSION and DATE: MASS1 v0.80 10/08/1998
!
! PURPOSE: manages mass1 model startup and run for non-dll use
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY:  added console output; mcr 12/10/97
!                                                               merged the old gastrans routine into this routine; mcr 10-8-98
!
!
!***************************************************************
!

PROGRAM mass1

  ! set units and time constant


  USE utility
  USE date_time
  USE time_series
  USE general_vars
  USE link_vars, ONLY : linktype
  USE section_handler_module

  USE scalars
  USE tdg_equation_coeff
  USE profile_output_module
  USE pidlink
  USE mass1_config
  USE bc_module

  IMPLICIT NONE

  DOUBLE PRECISION  model_time
  INTEGER, SAVE :: time_step_count = 0
  INTEGER :: species_num, i
  INTEGER :: link
  INTEGER :: tsteps
  LOGICAL run
  CHARACTER (LEN=10) :: date_string, time_string

  utility_error_iounit = 11
  utility_status_iounit = 99

  CALL date_time_flags()
  CALL time_series_module_init()

  ! open the status file - this file records progress through the
  ! input stream and errors
  !
  CALL open_new('status.out', utility_status_iounit)
  CALL open_new('error-warning.out', utility_error_iounit)

  CALL banner()

  ! read in configuration file
  CALL config%read()

  CALL print_output("HEADER", time)
  CALL print_output("CONFIG", time)


  IF(config%debug_print) WRITE(11,*)'done reading configuration file'

  SELECT CASE (config%time%option)
  CASE (DATE_TIME_OPTION)
     WRITE(*,1120) config%time%date_run_begins, config%time%time_run_begins
1120 FORMAT(//' Simulation Starts on Date: ',a10,'  Time: ',a8/)
     WRITE(*,1130) config%time%date_run_ends, config%time%time_run_ends
1130 FORMAT(' Simulation Ends on Date: ',a10,'  Time: ',a8//)
  END SELECT

  !-----------------------------------------------------------------------
  ! call startup routines 

  ! CALL point_data_scan   ! or something to determine maxlinks and maxpoint prior to allocation
  CALL array_alloc
  CALL allocate_species(utility_error_iounit, utility_status_iounit)
  DO i=1,max_species
     CALL allocate_species_components(i, config%maxlinks, config%maxpoint, &
          &utility_status_iounit, utility_error_iounit)
  END DO
  IF(config%debug_print)WRITE(11,*)'done with array alloc'

  met_zone_manager = met_zone_manager_t()
  IF (config%met_required) THEN
     CALL met_zone_manager%read(config%weather_file)
  END IF

  sections = section_handler()
  CALL sections%read(config%section_file)

  IF(config%debug_print)WRITE(11,*)'done with section data'

  CALL link_data
  IF(config%debug_print)WRITE(11,*)'done link data'

  CALL point_data
  IF(config%debug_print)WRITE(11,*)'done with point data'

  bc_manager = new_bc_manager()
  CALL bc_manager%read(LINK_BC_TYPE, config%linkbc_file)
  IF (config%do_hydro_bc) THEN
     CALL bc_manager%read(HYDRO_BC_TYPE, config%hydrobc_file)
  END IF
  IF(config%debug_print)WRITE(11,*)'link BC data done'

  IF(config%do_latflow)THEN
     CALL bc_manager%read(LATFLOW_BC_TYPE, config%lateral_file)
     IF(config%debug_print)WRITE(11,*)'lateral inflow BC data done'
  ENDIF

  IF(config%do_hotstart)THEN
     CALL read_hotstart
  ELSE
     CALL initial_cond
  ENDIF
  IF(config%debug_print) WRITE(11,*)'done with ICs or hotstart read'

  IF(config%do_gas)THEN
     CALL bc_manager%read(TRANS_BC_TYPE, config%transbc_file)
     species_num = 1
     IF(config%debug_print) WRITE(11,*)'done reading gas transport table'
     CALL allocate_tdg_coeff(config%maxlinks,utility_status_iounit, utility_error_iounit)
     ! read tdg spill coefficient tables
     ! if linktype 6,20, or 21 is there, then open and read file
     DO link=1,config%maxlinks
        SELECT CASE(linktype(link)) 
        CASE(6,21)
           CALL tdg_coeff_read(utility_status_iounit, utility_error_iounit)
           EXIT
        END SELECT
     END DO
     IF(config%gas_exchange)THEN
        CALL open_existing(config%tdg_coeff_file, 88, fatal=.TRUE.)
        READ(88,*)gasx_a,gasx_b,gasx_c, gasx_d
        CLOSE(88)
     ENDIF
  ENDIF

  IF(config%do_temp)THEN
     CALL bc_manager%read(TEMP_BC_TYPE, config%tempbc_file)
     species_num = 2
     IF(config%debug_print) WRITE(11,*)'done reading temp transport table'
  ENDIF

  CALL kick_off
  IF(config%debug_print) WRITE(11,*)'done with kick_off'

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  ! execute time loop in model
  !------------------------------------------------------------------------------

  model_time = config%time%begin
  time = model_time/config%time%mult

  CALL bc_manager%update(time)
  CALL met_zone_manager%update(time)

  IF(model_time <= config%time%end) run = .true.

  DO WHILE(run)

     IF((config%debug_print).AND.(time == config%time%begin))THEN
        WRITE(11,*)'main computational loop in mass1'
        WRITE(11,*)time,config%time%begin,config%time%end,&
             &config%time%delta_t,config%time%mult
        WRITE(11,*)' '
     ENDIF

     ! print out initial conditions

     IF (time == config%time%begin) THEN
        IF (config%do_printout) CALL print_output("RESULT", time)
        IF (config%do_profileout) CALL profile_output
        IF (config%do_gageout) CALL gage_output
     END IF

     CALL bc_manager%update(model_time/config%time%mult)

     IF (config%do_flow) THEN
        CALL flow_sim
     ENDIF

     ! transport simulation is performed at
     ! integral divisions of the hydrodynamic
     ! time step

     IF (config%do_gas .OR. config%do_temp) THEN

        scalar_time = model_time

        IF (config%scalar_steps .GT. 0) THEN
           tsteps = config%scalar_steps
        ELSE
           tsteps = tvd_steps(config%time%delta_t)
           WRITE(*, '(" Using ", I5, " transport steps")') tsteps
        END IF

        scalar_delta_t = config%time%delta_t/DBLE(tsteps)

        DO i = 1, tsteps

           IF((config%debug_print))THEN
              WRITE(11,*)'transport sub loop in mass1'
              WRITE(11,*)i, scalar_time,model_time
              WRITE(11,*)' '
           ENDIF

           CALL bc_manager%update(scalar_time/config%time%mult)

           CALL tvd_interp(scalar_time, model_time, model_time + config%time%delta_t)

           CALL met_zone_manager%update(scalar_time)

           IF(config%do_gas)THEN
              species_num = 1
              CALL tvd_transport(species_num, &
                   &species(species_num)%conc,&
                   &species(species_num)%concold)
           END IF

           IF(config%do_temp)THEN
              species_num = 2
              CALL tvd_transport(species_num, &
                   &species(species_num)%conc,&
                   &species(species_num)%concold)
           END IF

           scalar_time = model_time + DBLE(i)/DBLE(tsteps)*config%time%step

        END DO

     END IF

     ! update time step here so correct
     ! time is placed in output

     ! model_time = model_time + time_step
     time_step_count = time_step_count + 1    
     model_time = config%time%begin + time_step_count*config%time%step
     time = model_time/config%time%mult
     IF(model_time >= (config%time%end)) run = .false.

     CALL decimal_to_date(time, date_string, time_string)
     WRITE(*,1020)date_string,time_string
1020 FORMAT(' Done Crunching through ** Date: ',a10,'  Time: ',a8)

     ! do output as specified

     IF (MOD(time_step_count, config%print_freq) == 0) THEN
        IF (config%do_printout)CALL print_output("RESULT", time)
        IF (config%do_gageout) CALL gage_output 
        IF (config%do_profileout) CALL profile_output
     ENDIF

     IF(config%debug_print) WRITE(11,*)'simulation time = ',time/config%time%mult

     IF((config%do_restart).AND.(time >= config%time%end))THEN
        CALL write_restart
     ENDIF

     IF((config%debug_print).AND.(time >= config%time%end))THEN
        WRITE(11,*)'ALL DONE!!'
        WRITE(11,*)time,config%time%begin,config%time%end,config%time%delta_t,config%time%mult
        CLOSE(11)
     ENDIF

     CALL pidlink_assemble_lagged()

  END DO ! end main time loop

  CALL met_zone_manager%destroy()
  CALL sections%destroy()
  CALL array_dealloc


  CLOSE(99)

END PROGRAM mass1
