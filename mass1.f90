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


USE general_vars
USE date_vars
USE file_vars
USE logicals
USE section_vars , ONLY : total_sections
USE link_vars, ONLY : linktype

USE scalars
USE met_data_module, ONLY : read_met_data
USE tdg_equation_coeff

IMPLICIT NONE

DOUBLE PRECISION  model_time, time_step, date_to_decimal
INTEGER, SAVE :: time_step_count = 0
INTEGER :: error_iounit = 11,status_iounit = 99, species_num, i
INTEGER :: link
LOGICAL run

version = 'MASS1 Version 0.81 Date: 11-15-1998'
WRITE(*,*)'Modular Aquatic Simulation System 1D (MASS1)'
WRITE(*,*)'Pacific Northwest National Laboratory'
WRITE(*,*)' '
WRITE(*,*)version

!
OPEN(unit=99,file='status.out')


! read in configuration file
CALL read_config

IF(debug_print == 1) WRITE(11,*)'done reading configuration file'

IF(time_option == 2)THEN
    date_string = date_run_begins
    time_string = time_run_begins
    time_begin = date_to_decimal()

    date_string = date_run_ends
    time_string = time_run_ends
		time_end = date_to_decimal()
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
    
CALL array_alloc
CALL allocate_species(error_iounit,status_iounit)
DO i=1,max_species
	CALL allocate_species_components(i, maxlinks, maxpoint, status_iounit, error_iounit)
END DO
	IF(debug_print == 1)WRITE(11,*)'done with array alloc'

CALL link_data
	IF(debug_print == 1)WRITE(11,*)'done link data'

CALL point_data
	IF(debug_print == 1)WRITE(11,*)'done with point data'

CALL section_data
	IF(debug_print == 1)WRITE(11,*)'done with section data'

CALL link_bc
	IF(debug_print == 1)WRITE(11,*)'link BC data done'

CALL latflow_bc
	IF(debug_print == 1)WRITE(11,*)'lateral inflow BC data done'
    
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
	CALL allocate_tdg_coeff(maxlinks,status_iounit, error_iounit)
			! read tdg spill coefficient tables
			! if linktype 6,20, or 21 is there, then open and read file
	DO link=1,maxlinks
			SELECT CASE(linktype(link)) 
			CASE(6,21)
			CALL tdg_coeff_read(status_iounit, error_iounit)
			EXIT
		END SELECT
	END DO
ENDIF

IF(do_temp)THEN
	species_num = 2
	CALL transport_bc(species_num)
	IF(temp_exchange)&
             & CALL read_met_data(filename(18), maxtimes, status_iounit, error_iounit)
	IF(debug_print == 1) WRITE(11,*)'done reading temp transport table'
ENDIF

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! execute time loop in model
!------------------------------------------------------------------------------

model_time = time_begin

IF(model_time <= time_end) run = .true.

DO WHILE(run)
	time = model_time/time_mult
	IF(model_time >= (time_end)) run = .false.

	IF((debug_print == 1).AND.(time == time_begin))THEN
		WRITE(11,*)'main computational loop in mass1'
		WRITE(11,*)time,time_begin,time_end,delta_t,time_mult
		WRITE(11,*)' '
	ENDIF

	IF(do_flow)THEN
		CALL flow_sim
	ENDIF

	IF(do_gas)THEN
		species_num = 1
		CALL tvd_transport(species_num, species(species_num)%conc,species(species_num)%concold &
			& ,status_iounit, error_iounit)
	END IF

	IF(do_temp)THEN
		species_num = 2
		CALL tvd_transport(species_num, species(species_num)%conc,species(species_num)%concold &
			& , status_iounit, error_iounit)
	END IF

	IF(do_printout .OR. (time == time_begin))THEN
		IF(MOD(time_step_count,print_freq) == 0)CALL print_output
	END IF

	IF(do_gageout)THEN
		IF(MOD(time_step_count,print_freq) == 0)CALL gage_output	
	ENDIF

	IF(do_profileout)THEN
		IF(MOD(time_step_count,print_freq) == 0)CALL profile_output
	ENDIF

	IF(debug_print == 1) WRITE(11,*)'simulation time = ',time/time_mult

	IF((do_restart).AND.(time >= time_end))THEN
		CALL write_restart
	ENDIF

! deallocation of arrays 
	IF(time >= time_end)THEN
		CALL array_dealloc
	ENDIF

	IF((debug_print == 1).AND.(time >= time_end))THEN
		WRITE(11,*)'ALL DONE!!'
		WRITE(11,*)time,time_begin,time_end,delta_t,time_mult
		CLOSE(11)
	ENDIF

	CALL decimal_to_date
	WRITE(*,1020)date_string,time_string
1020 FORMAT(' Done Crunching through ** Date: ',a10,'  Time: ',a8)

! update time
	model_time = model_time + time_step
	time_step_count = time_step_count + 1    

END DO ! end main time loop

CLOSE(99)

END PROGRAM mass1
