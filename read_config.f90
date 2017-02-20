!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME: mass1
!
! VERSION and DATE: MASS1 v0.75 3/25/1998
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
! MOD HISTORY:
!	added read for print_freq; mcr 1/7/98
!   added stuff laterla inflows; mcr 3/25/98 
!
!
!***************************************************************
!

SUBROUTINE read_config

  USE utility
  USE file_vars
  USE general_vars
  USE section_vars
  USE point_vars
  USE date_vars
  USE logicals


  IMPLICIT NONE

  CHARACTER(LEN=*), PARAMETER :: config_name = 'mass1.cfg'
  CHARACTER(LEN=1024) :: msg
  INTEGER :: dumlog, dumlog0
  INTEGER :: ignored
  INTEGER :: line

  do_accumulate = .FALSE.


  CALL open_existing(config_name, 10, fatal=.TRUE.)

  line = 0

  READ(10,1000, ERR=110) msg
  line = line + 1
1000 FORMAT(a100)
  WRITE(*,*) msg

  READ(10,*, ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_flow	= .true.
  ELSE
     do_flow = .false.
  ENDIF

  READ(10,*, ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_latflow	= .true.
  ELSE
     do_latflow = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_gas	= .true.
  ELSE
     do_gas = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_temp	= .true.
  ELSE
     do_temp = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_printout	= .true.
  ELSE
     do_printout = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_gageout	= .true.
  ELSE
     do_gageout = .false.
  ENDIF

  dumlog0 = 0
  READ(10,*,ERR=110)dumlog, dumlog0 
  line = line + 1
  IF(dumlog == 1)THEN
     do_profileout	= .true.
  ELSE
     do_profileout = .false.
  ENDIF
  IF(dumlog0 == 1)THEN
     do_accumulate = .true.
  ELSE
     do_accumulate = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     gas_diffusion	= .true.
  ELSE
     gas_diffusion = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     gas_exchange	= .true.
  ELSE
     gas_exchange = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     temp_diffusion	= .true.
  ELSE
     temp_diffusion = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     temp_exchange	= .true.
  ELSE
     temp_exchange = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_hotstart	= .true.
  ELSE
     do_hotstart = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     do_restart	= .true.
  ELSE
     do_restart = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     print_sections	= .true.
  ELSE
     print_sections = .false.
  ENDIF

  READ(10,*,ERR=110) ignored ! write_sections
  line = line + 1

  READ(10,*,ERR=110) ignored ! read_sections 
  line = line + 1

  READ(10,*,ERR=110)units
  line = line + 1
  units = units + 1

  READ(10,*,ERR=110)time_option
  line = line + 1
  time_option = time_option + 1

  READ(10,*,ERR=110)time_units
  line = line + 1
  time_units = time_units + 1

  READ(10,*,ERR=110)channel_length_units
  line = line + 1
  channel_length_units = channel_length_units + 1

  READ(10,*,ERR=110)dsbc_type
  line = line + 1
  dsbc_type = dsbc_type + 1

  READ(10,*,ERR=110)maxlinks
  line = line + 1

  READ(10,*,ERR=110)maxpoint
  line = line + 1

  READ(10,*,ERR=110) ignored ! maxtable
  line = line + 1

  READ(10,*,ERR=110) ignored ! maxtimes
  line = line + 1

  READ(10,*,ERR=110)total_sections ! overwritten by section_data_count()
  line = line + 1

  READ(10,*,ERR=110)scalar_steps
  line = line + 1

  READ(10,*,ERR=110)debug_print
  line = line + 1

  READ(10,*,ERR=110)filename(2)         ! links
  line = line + 1

  READ(10,*,ERR=110)filename(3)         ! points
  line = line + 1

  READ(10,*,ERR=110)filename(4)         ! sections
  line = line + 1

  READ(10,*,ERR=110)filename(5)         ! link BC
  line = line + 1

  READ(10,*,ERR=110)filename(6)         ! initial conditions
  line = line + 1

  READ(10,*,ERR=110)filename(7)         ! output.out
  line = line + 1

  READ(10,*,ERR=110)filename(9)		! gas transport bc file
  line = line + 1

  READ(10,*,ERR=110)filename(17)	! temperature bc file
  line = line + 1

  READ(10,*,ERR=110)filename(18)	! weather bc file
  line = line + 1

  READ(10,*,ERR=110)filename(10)        ! hydro BC file
  line = line + 1

  READ(10,*,ERR=110)filename(11)        ! TDG coefficients
  line = line + 1

  READ(10,*,ERR=110)filename(12)        ! restart file to read
  line = line + 1

  READ(10,*,ERR=110)filename(13)        ! hotstart file to write
  line = line + 1

  READ(10,*,ERR=110)filename(14)        ! gage control
  line = line + 1

  READ(10,*,ERR=110)filename(15)        ! profile control
  line = line + 1

  READ(10,*,ERR=110)filename(16)        ! lateral inflow data file
  line = line + 1

  READ(10,*,ERR=110)date_run_begins
  line = line + 1

  READ(10,*,ERR=110)time_run_begins
  line = line + 1

  READ(10,*,ERR=110)date_run_ends
  line = line + 1

  READ(10,*,ERR=110)time_run_ends
  line = line + 1

  msg = ""
  READ(10,*,ERR=110) delta_t, msg
  line = line + 1

  ! make sure delta_t is in hours

  IF (LEN(TRIM(msg)) .EQ. 0) THEN
     ! assume it's in hours
  ELSE IF (msg(1:2) .EQ. 'hr') THEN
     ! assume it's in hours
  ELSE IF (msg(1:3) .EQ. 'min') THEN
     delta_t = delta_t / 60.0
  ELSE IF (msg(1:3) .EQ. 'day') THEN
     delta_t = delta_t * 24.0
  ELSE IF (msg(1:3) .EQ. 'sec') THEN
     delta_t = delta_t / 3600.0
  ELSE
     WRITE(msg, *) 'time step units (', TRIM(msg), ') not understood'
     CALL error_message(msg, fatal=.FALSE.)
     GOTO 110
  END IF

  READ(10,*,ERR=110)print_freq
  line = line + 1

  CLOSE(10)

  ! Some things that need to be set, but are not read (yet)

  depth_minimum = 0.001           ! m
  depth_threshold = 0.01          ! m

  IF(debug_print == 1)THEN 

     OPEN(11,file='debug.txt')

     WRITE(11,*)'units : ',units
     WRITE(11,*)'time_option : ',time_option
     WRITE(11,*)time_units
     WRITE(11,*)channel_length_units
     WRITE(11,*)dsbc_type
     WRITE(11,*)maxlinks
     WRITE(11,*)maxpoint
     WRITE(11,*)'maxtable ignored'
     WRITE(11,*)'maxtimes ignored'
     WRITE(11,*)total_sections
     WRITE(11,*)scalar_steps
     WRITE(11,*)debug_print
     WRITE(11,*)'accumulate: ', do_accumulate
     WRITE(11,*)'done reading logicals and ints'
     WRITE(11,*)filename(2)
     WRITE(11,*)filename(3)
     WRITE(11,*)filename(4)
     WRITE(11,*)filename(5)
     WRITE(11,*)filename(6)
     WRITE(11,*)filename(7)
     WRITE(11,*)filename(9)
     WRITE(11,*)filename(10)
     WRITE(11,*)filename(11)
     WRITE(11,*)filename(12)
     WRITE(11,*)filename(13)
     WRITE(11,*)filename(14)
     WRITE(11,*)filename(15)
     WRITE(11,*)filename(16)

     WRITE(11,*)'delta_t : ',delta_t

  ENDIF

  RETURN

110 CONTINUE

  WRITE (msg, *) TRIM(config_name), ': line ', line, ': read error'
  CALL error_message(msg, .TRUE.)
  RETURN

END SUBROUTINE read_config
