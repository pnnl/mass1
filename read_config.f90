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
  INTEGER :: line

  do_accumulate = .FALSE.


  CALL open_existing(config_name, 10, fatal=.TRUE.)

  line = 0

  READ(10,1000, ERR=110)config_version
  line = line + 1
1000 FORMAT(a100)
  WRITE(*,*)config_version

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

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     write_sections	= .true.
  ELSE
     write_sections = .false.
  ENDIF

  READ(10,*,ERR=110)dumlog
  line = line + 1
  IF(dumlog == 1)THEN
     read_sections	= .true.
  ELSE
     read_sections = .false.
  ENDIF


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

  READ(10,*,ERR=110)maxtable
  line = line + 1

  READ(10,*,ERR=110)maxtimes
  line = line + 1

  READ(10,*,ERR=110)total_sections
  line = line + 1

  READ(10,*,ERR=110)scalar_steps
  line = line + 1

  READ(10,*,ERR=110)debug_print
  line = line + 1

  READ(10,*,ERR=110)filename(2)
  line = line + 1

  READ(10,*,ERR=110)filename(3)
  line = line + 1

  READ(10,*,ERR=110)filename(4)
  line = line + 1

  READ(10,*,ERR=110)filename(5)
  line = line + 1

  READ(10,*,ERR=110)filename(6)
  line = line + 1

  READ(10,*,ERR=110)filename(7)
  line = line + 1

  READ(10,*,ERR=110)filename(9)		! gas transport bc file
  line = line + 1

  READ(10,*,ERR=110)filename(17)	! temperature bc file
  line = line + 1

  READ(10,*,ERR=110)filename(18)	! weather bc file
  line = line + 1

  READ(10,*,ERR=110)filename(10)
  line = line + 1

  READ(10,*,ERR=110)filename(11)
  line = line + 1

  READ(10,*,ERR=110)filename(12)
  line = line + 1

  READ(10,*,ERR=110)filename(13)
  line = line + 1

  READ(10,*,ERR=110)filename(14)
  line = line + 1

  READ(10,*,ERR=110)filename(15)
  line = line + 1

  READ(10,*,ERR=110)filename(16) ! lateral inflow data file
  line = line + 1

  READ(10,*,ERR=110)date_run_begins
  line = line + 1

  READ(10,*,ERR=110)time_run_begins
  line = line + 1

  READ(10,*,ERR=110)date_run_ends
  line = line + 1

  READ(10,*,ERR=110)time_run_ends
  line = line + 1

  READ(10,*,ERR=110)delta_t
  line = line + 1

  READ(10,*,ERR=110)print_freq
  line = line + 1

  CLOSE(10)

  IF(debug_print == 1)THEN 

     OPEN(11,file='debug.txt')

     WRITE(11,*)'units : ',units
     WRITE(11,*)'time_option : ',time_option
     WRITE(11,*)time_units
     WRITE(11,*)channel_length_units
     WRITE(11,*)dsbc_type
     WRITE(11,*)maxlinks
     WRITE(11,*)maxpoint
     WRITE(11,*)maxtable
     WRITE(11,*)maxtimes
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
