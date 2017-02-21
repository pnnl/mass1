! ----------------------------------------------------------------
! file: mass1_config.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 17, 2017 by William A. Perkins
! Last Change: 2017-02-20 14:54:57 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE mass1_config
! ----------------------------------------------------------------
MODULE mass1_config

  USE utility
  USE date_time

  IMPLICIT NONE

  PUBLIC

  ENUM, BIND(C)
     ENUMERATOR :: DECIMAL_TIME_OPTION = 1
     ENUMERATOR :: DATE_TIME_OPTION = 2
  END ENUM

  ENUM, BIND(C)
     ENUMERATOR :: SECOND_UNITS = 1
     ENUMERATOR :: MINUTE_UNITS = 2
     ENUMERATOR :: HOUR_UNITS = 3
     ENUMERATOR :: DAY_UNITS = 4
  END ENUM

  ENUM, BIND(C)
     ENUMERATOR :: ENGLISH_UNITS = 1
     ENUMERATOR :: METRIC_UNITS = 2
  END ENUM

  ENUM, BIND(C)
     ENUMERATOR :: CHANNEL_FOOT = 1
     ENUMERATOR :: CHANNEL_METER = 2
     ENUMERATOR :: CHANNEL_MILE = 3
     ENUMERATOR :: CHANNEL_KM = 4
  END ENUM

  TYPE, PUBLIC :: time_frame_t
     INTEGER(KIND(DECIMAL_TIME_OPTION)) :: option
     INTEGER(KIND(SECOND_UNITS)) :: units
     DOUBLE PRECISION :: time
     DOUBLE PRECISION :: begin, end 
     DOUBLE PRECISION :: delta_t
     DOUBLE PRECISION :: step
     DOUBLE PRECISION :: mult
     CHARACTER (LEN=10) :: date_run_begins, date_run_ends
     CHARACTER (LEN=10) :: time_run_begins, time_run_ends
     CONTAINS 
       ! PROCEDURE read
       ! PROCEDURE internal 
    END type time_frame_t

  INTEGER, PARAMETER :: path_length = 1024

  TYPE, PUBLIC :: configuration_t
     CHARACTER(LEN=256) :: config_version
     LOGICAL :: do_flow
     LOGICAL :: do_gas
     LOGICAL :: do_temp
     LOGICAL :: do_printout
     LOGICAL :: do_gageout
     LOGICAL :: do_profileout
     LOGICAL :: do_restart
     LOGICAL :: do_hotstart
     LOGICAL :: temp_diffusion, temp_exchange
     LOGICAL :: gas_diffusion, gas_exchange
     LOGICAL :: do_latflow
     LOGICAL :: do_accumulate
     INTEGER :: print_freq
     LOGICAL :: print_sections
     LOGICAL :: write_sections
     LOGICAL :: read_sections
     LOGICAL :: debug_print
     INTEGER :: maxlinks
     INTEGER :: maxpoint
     INTEGER :: scalar_steps
     INTEGER(KIND=ENGLISH_UNITS) :: units
     INTEGER(KIND=CHANNEL_FOOT) :: channel_length_units
     INTEGER :: dsbc_type
     TYPE (time_frame_t) :: time
     CHARACTER(LEN=path_length) :: link_file
     CHARACTER(LEN=path_length) :: point_file
     CHARACTER(LEN=path_length) :: section_file
     CHARACTER(LEN=path_length) :: linkbc_file
     CHARACTER(LEN=path_length) :: initial_file
     CHARACTER(LEN=path_length) :: output_file
     CHARACTER(LEN=path_length) :: transbc_file
     CHARACTER(LEN=path_length) :: tempbc_file
     CHARACTER(LEN=path_length) :: weather_file
     CHARACTER(LEN=path_length) :: hydrobc_file
     CHARACTER(LEN=path_length) :: tdg_coeff_file
     CHARACTER(LEN=path_length) :: restart_load_file
     CHARACTER(LEN=path_length) :: restart_save_file
     CHARACTER(LEN=path_length) :: gage_file
     CHARACTER(LEN=path_length) :: profile_file
     CHARACTER(LEN=path_length) :: lateral_file
   CONTAINS
     PROCEDURE :: read => configuration_read
  END type configuration_t

  CHARACTER(LEN=path_length), PARAMETER :: config_name = 'mass1.cfg'

  TYPE (configuration_t), PUBLIC :: config

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE configuration_read
  ! ----------------------------------------------------------------
  SUBROUTINE configuration_read(this)
    IMPLICIT NONE
    CLASS (configuration_t), INTENT(INOUT) :: this

    CHARACTER(LEN=1024) :: msg
    INTEGER :: dumlog, dumlog0
    INTEGER :: ignored
    INTEGER :: iunit
    INTEGER :: line

    iunit = 10
    line = 0

    this%do_accumulate = .FALSE.

    CALL open_existing(config_name, iunit, fatal=.TRUE.)

    READ(iunit,1000, ERR=110) this%config_version
    line = line + 1
1000 FORMAT(a100)

    READ(iunit,*, ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_flow = .true.
    ELSE
       this%do_flow = .false.
    ENDIF

    READ(iunit,*, ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_latflow = .true.
    ELSE
       this%do_latflow = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_gas = .true.
    ELSE
       this%do_gas = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_temp = .true.
    ELSE
       this%do_temp = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_printout = .true.
    ELSE
       this%do_printout = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_gageout       = .true.
    ELSE
       this%do_gageout = .false.
    ENDIF

    dumlog0 = 0
    READ(iunit,*,ERR=110)dumlog, dumlog0 
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_profileout    = .true.
    ELSE
       this%do_profileout = .false.
    ENDIF
    IF(dumlog0 == 1)THEN
       this%do_accumulate = .true.
    ELSE
       this%do_accumulate = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%gas_diffusion    = .true.
    ELSE
       this%gas_diffusion = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%gas_exchange     = .true.
    ELSE
       this%gas_exchange = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%temp_diffusion   = .true.
    ELSE
       this%temp_diffusion = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%temp_exchange = .true.
    ELSE
       this%temp_exchange = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_hotstart      = .true.
    ELSE
       this%do_hotstart = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%do_restart       = .true.
    ELSE
       this%do_restart = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%print_sections   = .true.
    ELSE
       this%print_sections = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%write_sections   = .true.
    ELSE
       this%write_sections = .false.
    ENDIF

    READ(iunit,*,ERR=110)dumlog
    line = line + 1
    IF(dumlog == 1)THEN
       this%read_sections    = .true.
    ELSE
       this%read_sections = .false.
    ENDIF

    ! units option (0 = English, 1 = SI)
    READ(iunit,*,ERR=110) dumlog
    line = line + 1
    SELECT CASE (dumlog) 
    CASE (0)
       this%units = ENGLISH_UNITS
    CASE (1) 
       this%units = METRIC_UNITS
    CASE DEFAULT
       WRITE(msg, *) 'simulation units (', dumlog, ') not understood'
       CALL error_message(msg, fatal=.FALSE.)
       GOTO 110
    END SELECT

    READ(iunit,*,ERR=110) dumlog
    line = line + 1
    SELECT CASE (dumlog+1) 
    CASE (1)
       this%time%option = DECIMAL_TIME_OPTION
    CASE (2)
       this%time%option = DATE_TIME_OPTION
    CASE DEFAULT 
       WRITE(msg, *) 'time option (', dumlog, ') not understood'
       CALL error_message(msg, fatal=.FALSE.)
       GOTO 110
    END SELECT

    READ(iunit,*,ERR=110) dumlog
    line = line + 1
    SELECT CASE (dumlog) 
    CASE (1)
       this%time%units = SECOND_UNITS
    CASE (2)
       this%time%units = MINUTE_UNITS
    CASE (3)
       this%time%units = HOUR_UNITS
    CASE (4)
       this%time%units = DAY_UNITS
    CASE DEFAULT 
       WRITE(msg, *) 'time units (', dumlog, ') not understood'
       CALL error_message(msg, fatal=.FALSE.)
       GOTO 110
    END SELECT

    READ(iunit,*,ERR=110) dumlog
    line = line + 1
    SELECT CASE (dumlog+1)
    CASE (1)
       this%channel_length_units = CHANNEL_FOOT
    CASE (2)
       this%channel_length_units = CHANNEL_METER
    CASE (3)
       this%channel_length_units = CHANNEL_MILE
    CASE (4)
       this%channel_length_units = CHANNEL_KM
    CASE DEFAULT
       WRITE(msg, *) 'channel length units (', dumlog, ') not understood'
       CALL error_message(msg, fatal=.FALSE.)
       GOTO 110
    END SELECT

    READ(iunit,*,ERR=110) dumlog
    line = line + 1
    this%dsbc_type = dumlog + 1

    READ(iunit,*,ERR=110) this%maxlinks
    line = line + 1

    READ(iunit,*,ERR=110) this%maxpoint
    line = line + 1

    READ(iunit,*,ERR=110) ignored ! maxtable
    line = line + 1

    READ(iunit,*,ERR=110) ignored ! maxtimes
    line = line + 1

    READ(iunit,*,ERR=110) ignored ! total_sections 
    line = line + 1

    READ(iunit,*,ERR=110) this%scalar_steps
    line = line + 1

    READ(iunit,*,ERR=110) dumlog
    line = line + 1
    IF (dumlog .EQ. 1) THEN 
       this%debug_print = .true.
    ELSE 
       this%debug_print = .false.
    END IF

    READ(iunit,*,ERR=110) this%link_file
    line = line + 1

    READ(iunit,*,ERR=110) this%point_file
    line = line + 1

    READ(iunit,*,ERR=110) this%section_file
    line = line + 1

    READ(iunit,*,ERR=110) this%linkbc_file
    line = line + 1

    READ(iunit,*,ERR=110) this%initial_file
    line = line + 1

    READ(iunit,*,ERR=110) this%output_file
    line = line + 1

    READ(iunit,*,ERR=110) this%transbc_file
    line = line + 1

    READ(iunit,*,ERR=110) this%tempbc_file
    line = line + 1

    READ(iunit,*,ERR=110) this%weather_file
    line = line + 1

    READ(iunit,*,ERR=110) this%hydrobc_file
    line = line + 1

    READ(iunit,*,ERR=110) this%tdg_coeff_file
    line = line + 1

    READ(iunit,*,ERR=110) this%restart_load_file
    line = line + 1

    READ(iunit,*,ERR=110) this%restart_save_file
    line = line + 1

    READ(iunit,*,ERR=110) this%gage_file
    line = line + 1

    READ(iunit,*,ERR=110) this%profile_file
    line = line + 1

    READ(iunit,*,ERR=110) this%lateral_file
    line = line + 1

    READ(iunit,*,ERR=110) this%time%date_run_begins
    line = line + 1

    READ(iunit,*,ERR=110) this%time%time_run_begins
    line = line + 1

    READ(iunit,*,ERR=110) this%time%date_run_ends
    line = line + 1

    READ(iunit,*,ERR=110) this%time%time_run_ends
    line = line + 1

    msg = ""
    READ(iunit,*,ERR=110) this%time%delta_t, msg
    line = line + 1

    ! make sure delta_t is in hours

    IF (LEN(TRIM(msg)) .EQ. 0) THEN
       ! assume it's in hours
    ELSE IF (msg(1:2) .EQ. 'hr') THEN
       ! assume it's in hours
    ELSE IF (msg(1:3) .EQ. 'min') THEN
       this%time%delta_t = this%time%delta_t / 60.0
    ELSE IF (msg(1:3) .EQ. 'day') THEN
       this%time%delta_t = this%time%delta_t * 24.0
    ELSE IF (msg(1:3) .EQ. 'sec') THEN
       this%time%delta_t = this%time%delta_t / 3600.0
    ELSE
       WRITE(msg, *) 'time step units (', TRIM(msg), ') not understood'
       CALL error_message(msg, fatal=.FALSE.)
       GOTO 110
    END IF

    READ(iunit,*,ERR=110) this%print_freq
    line = line + 1

    CLOSE(iunit)

    
    ! figure out time constant
    ! code works in seconds internally if date option = 1
    ! code works in decimal julian days if date_option = 2

    SELECT CASE(this%time%option)
    CASE(DECIMAL_TIME_OPTION)
       SELECT CASE(this%time%units)
       CASE(SECOND_UNITS)
          this%time%mult = 1.00d0
       CASE(MINUTE_UNITS)
          this%time%mult = 60.00d0
       CASE(HOUR_UNITS)
          this%time%mult = 3600.00d0
       CASE(4)
          this%time%mult = 86400.00d0 !days
       END SELECT
       this%time%begin = this%time%begin*this%time%mult
       this%time%end = this%time%end*this%time%mult
       this%time%delta_t = this%time%delta_t*this%time%mult
       this%time%step = this%time%step*this%time%mult
    CASE(2)
       this%time%mult = 1.00d0 ! things are converted to decimal julian day
       this%time%begin = &
            &date_to_decimal(this%time%date_run_begins, this%time%time_run_begins)
       this%time%end = &
            &date_to_decimal(this%time%date_run_ends, this%time%time_run_ends)
    END SELECT

    this%time%step = this%time%delta_t/24.0
    this%time%delta_t = this%time%delta_t*3600.0

    RETURN 

110 CONTINUE

    WRITE (msg, *) TRIM(config_name), ': line ', line, ': read error'
    CALL error_message(msg, .TRUE.)
    RETURN

  END SUBROUTINE configuration_read

END MODULE mass1_config
  
