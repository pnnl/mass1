!-----------------------------------------------------------------------------
!
! module to read in and update meteorlogical data needed for air-water heat exchnage
! and dissolved gas concentration to pressure/saturation conversions
!
!-----------------------------------------------------------------------------

MODULE met_data_module
  
  USE utility
  USE time_series
  USE date_time
  USE date_vars
  USE logicals, ONLY : file_exist

  IMPLICIT NONE

  INTEGER, PARAMETER, PRIVATE :: met_nfields = 5
  INTEGER, PARAMETER, PUBLIC :: met_ncoeff = 4

  TYPE met_zone_rec
     INTEGER :: id
     ! coefficients: 1,2 wind function, 3 conduction, 4 brunt
     DOUBLE PRECISION :: coeff(met_ncoeff) 
     TYPE (time_series_rec), POINTER :: met
  END type met_zone_rec

  INTEGER, ALLOCATABLE, PRIVATE :: met_idx_lookup(:)
  TYPE (met_zone_rec), ALLOCATABLE, PRIVATE :: met_zones(:)

  ! This is where the current met data for the current met zone is
  ! stored.  This needs to be different.

  DOUBLE PRECISION, PUBLIC :: net_solar = 0.0  ! net incoming solar radiation W/m2
  DOUBLE PRECISION, PUBLIC :: t_air     = 10.0 ! air temperature deg. C
  DOUBLE PRECISION, PUBLIC :: t_dew     = 0.0  ! dewpoint temperature deg. C
  DOUBLE PRECISION, PUBLIC :: t_water   = 10.0 ! water temperature deg. C (from model simulation)
  DOUBLE PRECISION, PUBLIC :: windspeed	= 0.0  ! wind speed m/s
  DOUBLE PRECISION, PUBLIC :: baro_press= 760  ! barometric pressure mm Hg

CONTAINS

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION met_zone_index
  ! ----------------------------------------------------------------
  INTEGER FUNCTION met_zone_index(met_zone) RESULT(idx)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: met_zone
    CHARACTER(LEN=1024) :: msg

    idx = met_idx_lookup(met_zone)
    IF (idx .LE. 0) THEN
       WRITE(msg, *) 'unknown met zone id: ', met_zone
       CALL error_message(msg, .TRUE.)
    END IF
  END FUNCTION met_zone_index


  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_coeff(met_zone, coeff)

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: met_zone
    DOUBLE PRECISION, INTENT(OUT) :: coeff(:)

    INTEGER :: idx

    idx = met_zone_index(met_zone)

    coeff = met_zones(idx)%coeff

  END SUBROUTINE met_zone_coeff


  ! ----------------------------------------------------------------
  ! SUBROUTINE read_met_data
  ! ----------------------------------------------------------------
  SUBROUTINE read_met_data(met_files)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN) :: met_files

    CHARACTER(LEN=1024) :: msg
    INTEGER, PARAMETER :: iounit1 = 50, iounit2 = 51
    INTEGER :: istat
    INTEGER :: count, maxid, i
    INTEGER :: zoneid
    CHARACTER(LEN=1024) :: zonefile
    DOUBLE PRECISION :: coeff(4)

    CALL open_existing(met_files, iounit1, fatal=.TRUE.)

    count = 0
    maxid = -1
    DO WHILE(.TRUE.)
       READ (iounit1,*,END=100) zoneid, zonefile
       count = count + 1
       IF (zoneid .GT. maxid) maxid = zoneid
    END DO
100 CONTINUE

    IF (count .LE. 0) THEN
       WRITE(msg, *) 'no met zones specified in ', TRIM(met_files)
       CALL error_message(msg, .TRUE.)
    END IF

    ALLOCATE(met_zones(count), STAT=istat)
    IF (istat .NE. 0) THEN
       CALL error_message('memory allocation error in read_met_data', .TRUE.)
    END IF

    ALLOCATE(met_idx_lookup(maxid), STAT=istat)
    IF (istat .NE. 0) THEN
       CALL error_message('memory allocation error in read_met_data', .TRUE.)
    END IF
    met_idx_lookup = 0

    REWIND(iounit1)

    i = 0
    DO WHILE(.TRUE.)

       ! Default coefficients for this met zone

       coeff(1) = 0.46 ! wind function multiplier
       coeff(2) = 9.2  ! wind function offset
       coeff(3) = 0.47 ! conduction coefficient
       coeff(4) = 0.65 ! "brunt" coefficient for lw atm radiation

       READ(iounit1,*,END=200) zoneid, zonefile, coeff
       i = i + 1
       met_idx_lookup(zoneid) = i
       met_zones(i)%id = zoneid
       met_zones(i)%coeff = coeff
       met_zones(i)%met => time_series_read(zonefile, met_nfields, iounit2)

       WRITE (msg, *) 'Coefficients for weather zone ', zoneid
       CALL status_message(msg)
       WRITE (msg, *) '        Wind a = ' , met_zones(i)%coeff(1)
       CALL status_message(msg)
       WRITE (msg, *) '        Wind b = ' , met_zones(i)%coeff(2)
       CALL status_message(msg)
       WRITE (msg, *) '    Conduction = ' , met_zones(i)%coeff(3)
       CALL status_message(msg)
       WRITE (msg, *) '         Brunt = ' , met_zones(i)%coeff(4)
       CALL status_message(msg)
    END DO

    CLOSE(iounit1)

200 CONTINUE

  END SUBROUTINE read_met_data

  ! ----------------------------------------------------------------
  ! SUBROUTINE update_met_data
  ! ----------------------------------------------------------------
  SUBROUTINE update_met_data(time, met_zone)

    USE logicals, ONLY: do_gas

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: time
    INTEGER, INTENT(IN) :: met_zone

    CHARACTER(LEN=1024) :: msg
    INTEGER :: idx

    idx = met_zone_index(met_zone)

    CALL time_series_interp(met_zones(idx)%met, time)
    t_air = met_zones(idx)%met%current(1)
    t_dew = met_zones(idx)%met%current(2)
    windspeed = met_zones(idx)%met%current(3)
    IF (do_gas) THEN
       baro_press = met_zones(idx)%met%current(4)
    ELSE 
       baro_press = 760         ! a standard atmosphere
    END IF
    net_solar = met_zones(idx)%met%current(5)

  END SUBROUTINE update_met_data

END MODULE met_data_module
