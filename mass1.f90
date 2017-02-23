!***************************************************************
!            Pacific Northwest National Laboratory
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
! MOD HISTORY: 	added console output; mcr 12/10/97
!								merged the old gastrans routine into this routine; mcr 10-8-98
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
USE date_vars
USE file_vars
USE logicals
USE link_vars, ONLY : linktype
USE section_handler_module

USE scalars
USE met_data_module, ONLY : read_met_data
USE tdg_equation_coeff
USE profile_output_module
USE accumulator
USE pidlink

IMPLICIT NONE

DOUBLE PRECISION  model_time
INTEGER, SAVE :: time_step_count = 0
INTEGER :: species_num, i
INTEGER :: link
INTEGER :: tsteps
LOGICAL run

CALL date_time_flags()
CALL time_series_module_init()
utility_error_iounit = 11
utility_status_iounit = 99

version = 'MASS1 Version 0.84 Date: 08-15-1999'
WRITE(*,*)'Modular Aquatic Simulation System 1D (MASS1)'
WRITE(*,*)'Pacific Northwest National Laboratory'
WRITE(*,*)' '
WRITE(*,*)version



! open the status file - this file records progress through the
! input stream and errors
!
CALL open_new('status.out', utility_status_iounit)

! read in configuration file
CALL read_config

CALL print_output("HEADER")
CALL print_output("CONFIG")


IF(debug_print == 1) WRITE(11,*)'done reading configuration file'

IF(time_option == 2)THEN
    date_string = date_run_begins
    time_string = time_run_begins
    time_begin = date_to_decimal(date_string, time_string)

    date_string = date_run_ends
    time_string = time_run_ends
		time_end = date_to_decimal(date_string, time_string)
    WRITE(*,1120)date_run_begins,time_run_begins
1120 FORMAT(//' Simulation Starts on Date: ',a10,'  Time: ',a8/)
	 WRITE(*,1130)date_run_ends,time_run_ends
1130 FORMAT(' Simulation Ends on Date: ',a10,'  Time: ',a8//)

!assume given in hours need to convert to decimal days
    time_step = delta_t/24.0

!assume given in hours need to convert to seconds
    delta_t= delta_t*3600.0   
ENDIF

!-----------------------------------------------------------------------
! call startup routines 

! CALL point_data_scan   ! or something to determine maxlinks and maxpoint prior to allocation
CALL array_alloc
CALL allocate_species(utility_error_iounit, utility_status_iounit)
DO i=1,max_species
	CALL allocate_species_components(i, maxlinks, maxpoint, utility_status_iounit, utility_error_iounit)
END DO
	IF(debug_print == 1)WRITE(11,*)'done with array alloc'

sections = section_handler()
CALL sections%read(filename(4))

	IF(debug_print == 1)WRITE(11,*)'done with section data'

CALL link_data
	IF(debug_print == 1)WRITE(11,*)'done link data'

CALL point_data
	IF(debug_print == 1)WRITE(11,*)'done with point data'

CALL link_bc
	IF(debug_print == 1)WRITE(11,*)'link BC data done'

IF(do_latflow)THEN
   CALL latflow_bc
   IF(debug_print == 1)WRITE(11,*)'lateral inflow BC data done'
ENDIF
    
IF(do_hotstart)THEN
	CALL read_hotstart
ELSE
	CALL initial_cond
ENDIF
	IF(debug_print == 1) WRITE(11,*)'done with ICs or hotstart read'

CALL kick_off
	IF(debug_print == 1) WRITE(11,*)'done with kick_off'

IF(time_option == 1)THEN
		time_begin = time_begin*time_mult
		time_end = time_end*time_mult
		delta_t = delta_t*time_mult
		time_step = delta_t
ENDIF

IF(do_gas)THEN
	species_num = 1
	CALL transport_bc(species_num)
	IF(debug_print == 1) WRITE(11,*)'done reading gas transport table'
	CALL allocate_tdg_coeff(maxlinks,utility_status_iounit, utility_error_iounit)
			! read tdg spill coefficient tables
			! if linktype 6,20, or 21 is there, then open and read file
	DO link=1,maxlinks
			SELECT CASE(linktype(link)) 
			CASE(6,21)
			CALL tdg_coeff_read(utility_status_iounit, utility_error_iounit)
			EXIT
		END SELECT
	END DO
    IF(gas_exchange)THEN
       CALL open_existing('gas_exchange_coeff.dat', 88, fatal=.TRUE.)
    ENDIF
ENDIF

IF(do_temp)THEN
	species_num = 2
	CALL transport_bc(species_num)
	IF(temp_exchange)&
             & CALL read_met_data(filename(18))
	IF(debug_print == 1) WRITE(11,*)'done reading temp transport table'
ENDIF

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! execute time loop in model
!------------------------------------------------------------------------------

model_time = time_begin
time = model_time/time_mult

IF(model_time <= time_end) run = .true.

DO WHILE(run)

	IF((debug_print == 1).AND.(time == time_begin))THEN
		WRITE(11,*)'main computational loop in mass1'
		WRITE(11,*)time,time_begin,time_end,delta_t,time_mult
		WRITE(11,*)' '
	ENDIF

                                ! print out initial conditions

    IF (time == time_begin) THEN
       CALL accum_initialize()
       CALL accum_reset(time)
       CALL accumulate()
       CALL accum_calc(time)
       IF (do_printout) CALL print_output("RESULT")
       IF (do_profileout) CALL profile_output
       IF (do_gageout) CALL gage_output
    END IF
       
	IF(do_flow)THEN
		CALL flow_sim
	ENDIF

                                ! transport simulation is performed at
                                ! integral divisions of the hydrodynamic
                                ! time step

    IF (do_gas .OR. do_temp) THEN

       scalar_time = model_time
       IF (scalar_steps .GT. 0) THEN
          tsteps = scalar_steps
       ELSE
          tsteps = tvd_steps(delta_t)
          WRITE(*, '(" Using ", I5, " transport steps")') tsteps
       END IF

       scalar_delta_t = delta_t/DBLE(tsteps)

       DO i = 1, tsteps

          IF((debug_print == 1))THEN
             WRITE(11,*)'transport sub loop in mass1'
             WRITE(11,*)i, scalar_time,model_time
             WRITE(11,*)' '
          ENDIF

          CALL tvd_interp(scalar_time, model_time, model_time + delta_t)
          
          IF(do_gas)THEN
             species_num = 1
             CALL tvd_transport(species_num, species(species_num)%conc,species(species_num)%concold &
                  & ,utility_status_iounit, utility_error_iounit)
          END IF

          IF(do_temp)THEN
             species_num = 2
             CALL tvd_transport(species_num, species(species_num)%conc,species(species_num)%concold &
                  & , utility_status_iounit, utility_error_iounit)
          END IF

          scalar_time = model_time + DBLE(i)/DBLE(tsteps)*time_step

       END DO

    END IF

                                ! update time step here so correct
                                ! time is placed in output

    ! model_time = model_time + time_step
	time_step_count = time_step_count + 1    
	model_time = time_begin + time_step_count*time_step
	time = model_time/time_mult
	IF(model_time >= (time_end)) run = .false.

	CALL decimal_to_date(time, date_string, time_string)
	WRITE(*,1020)date_string,time_string
1020 FORMAT(' Done Crunching through ** Date: ',a10,'  Time: ',a8)

                                ! do output as specified

    IF (MOD(time_step_count,print_freq) == 0) THEN
       IF (.NOT. do_accumulate) THEN 
          CALL accum_reset(time)
          CALL accumulate()
       END IF
       CALL accum_calc(time)
       IF (do_printout)CALL print_output("RESULT")
       IF (do_gageout) CALL gage_output	
       IF (do_profileout) CALL profile_output
       CALL accum_reset(time)
	ENDIF

	IF(debug_print == 1) WRITE(11,*)'simulation time = ',time/time_mult

	IF((do_restart).AND.(time >= time_end))THEN
		CALL write_restart
	ENDIF

	IF((debug_print == 1).AND.(time >= time_end))THEN
		WRITE(11,*)'ALL DONE!!'
		WRITE(11,*)time,time_begin,time_end,delta_t,time_mult
		CLOSE(11)
	ENDIF

    IF (do_accumulate) CALL accumulate()

    CALL pidlink_assemble_lagged()

END DO ! end main time loop

CALL sections%destroy()
CALL array_dealloc


CLOSE(99)

END PROGRAM mass1
