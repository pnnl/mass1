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


USE file_vars
USE general_vars
USE section_vars
USE point_vars
USE date_vars
USE logicals


IMPLICIT NONE


INTEGER :: dumlog 

OPEN(10,file='mass1.cfg')

READ(10,1000)config_version
1000 FORMAT(a100)
WRITE(*,*)config_version

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_flow	= .true.
ELSE
do_flow = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_latflow	= .true.
ELSE
do_latflow = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_gas	= .true.
ELSE
do_gas = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_temp	= .true.
ELSE
do_temp = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_printout	= .true.
ELSE
do_printout = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_gageout	= .true.
ELSE
do_gageout = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_profileout	= .true.
ELSE
do_profileout = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
gas_diffusion	= .true.
ELSE
gas_diffusion = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
gas_exchange	= .true.
ELSE
gas_exchange = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
temp_diffusion	= .true.
ELSE
temp_diffusion = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
temp_exchange	= .true.
ELSE
temp_exchange = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_hotstart	= .true.
ELSE
do_hotstart = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
do_restart	= .true.
ELSE
do_restart = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
print_sections	= .true.
ELSE
print_sections = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
write_sections	= .true.
ELSE
write_sections = .false.
ENDIF

READ(10,*)dumlog
IF(dumlog == 1)THEN
read_sections	= .true.
ELSE
read_sections = .false.
ENDIF


READ(10,*)units
units = units + 1
READ(10,*)time_option
time_option = time_option + 1
READ(10,*)time_units
time_units = time_units + 1
READ(10,*)channel_length_units
channel_length_units = channel_length_units + 1
READ(10,*)dsbc_type
dsbc_type = dsbc_type + 1

READ(10,*)maxlinks
READ(10,*)maxpoint
READ(10,*)maxtable
READ(10,*)maxtimes
READ(10,*)total_sections
READ(10,*)scalar_steps
READ(10,*)debug_print



READ(10,*)filename(2)
READ(10,*)filename(3)
READ(10,*)filename(4)
READ(10,*)filename(5)
READ(10,*)filename(6)
READ(10,*)filename(7)
READ(10,*)filename(9)		! gas transport bc file
READ(10,*)filename(17)	! temperature bc file
READ(10,*)filename(18)	! weather bc file
READ(10,*)filename(10)
READ(10,*)filename(11)
READ(10,*)filename(12)
READ(10,*)filename(13)
READ(10,*)filename(14)
READ(10,*)filename(15)
READ(10,*)filename(16) ! lateral inflow data file


READ(10,*)date_run_begins
READ(10,*)time_run_begins
READ(10,*)date_run_ends
READ(10,*)time_run_ends

READ(10,*)delta_t
READ(10,*)print_freq

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




END SUBROUTINE read_config
