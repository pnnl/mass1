!-----------------------------------------------------------------------------
!
! module to read in and update meteorlogical data needed for air-water heat exchnage
! and dissolved gas concentration to pressure/saturation conversions
!
!-----------------------------------------------------------------------------

MODULE met_data_module

USE date_vars
USE logicals, ONLY : file_exist

IMPLICIT NONE
INTEGER, PARAMETER  :: max_cell_values = 5

TYPE datetime_struct
	CHARACTER (LEN=10) :: date_string
	CHARACTER (LEN=8)	 :: time_string
	DOUBLE PRECISION :: time
END TYPE datetime_struct

TYPE table_entry_struct
	TYPE(datetime_struct) :: datetime
	DOUBLE PRECISION :: value(max_cell_values)
END TYPE table_entry_struct

TYPE table_bc_struct
	CHARACTER (LEN=100) :: file_name
	CHARACTER (LEN=10) :: table_type
    DOUBLE PRECISION :: coeff(4) ! coefficients: 1,2 wind function, 3 conduction, 4 brunt
	INTEGER :: max_entries
        INTEGER :: start_entry

	TYPE(table_entry_struct), POINTER :: table_entry(:)
END TYPE table_bc_struct

DOUBLE PRECISION :: net_solar = 0.0  ! net incoming solar radiation W/m2
DOUBLE PRECISION :: t_air			= 10.0 ! air temperature deg. C
DOUBLE PRECISION :: t_dew			= 0.0  !	dewpoint temperature deg. C
DOUBLE PRECISION :: t_water		= 10.0 ! water temperature deg. C (from model simulation)
DOUBLE PRECISION :: windspeed	= 0.0  ! wind speed m/s
DOUBLE PRECISION :: baro_press  = 760 ! barometric pressure mm Hg

! TYPE(table_bc_struct), ALLOCATABLE :: met_data(:)
TYPE(table_bc_struct), ALLOCATABLE :: met_data(:)

CONTAINS

!##################################################################################################
SUBROUTINE read_met_data(met_files, max_times, status_iounit, error_iounit)
  IMPLICIT NONE
  CHARACTER(LEN=100) :: met_files, weather_filename
  INTEGER :: max_zones, max_times, status_iounit, error_iounit, alloc_stat
  INTEGER :: iounit1 = 50, iounit2 = 51, i, j = 0, met_zone
  DOUBLE PRECISION :: date_to_decimal

  INQUIRE(FILE=met_files,EXIST=file_exist)
  IF(file_exist)THEN
     OPEN(iounit2, file = met_files)
     WRITE(status_iounit,*)'weather file list opened',met_files
  ELSE
     WRITE(*,*)'weather file list does not exist - ABORT',met_files
     WRITE(status_iounit,*)'weather file list does not exist - ABORT',met_files
     CALL EXIT(2)
  ENDIF

  max_zones = 0
  DO WHILE(.TRUE.)
     max_zones = max_zones + 1
     READ (iounit2,*,END=50) met_zone, weather_filename
  END DO
50 REWIND (iounit2)

  max_zones = max_zones - 1

  IF (max_zones <= 0) THEN
     WRITE (error_iounit,*) 'no met zones specified in ', met_files
     CALL EXIT(2)
  ELSE 
     WRITE (status_iounit,*) 'allocating ', max_zones, ' met zones ...'
  END IF

  ALLOCATE(met_data(max_zones), STAT = alloc_stat)
  IF (alloc_stat /= 0) THEN
     WRITE (error_iounit,*) 'allocation failed for met data zones'
     CALL EXIT(2)
  ELSE
     WRITE (status_iounit,*) 'allocation successful for met zones'
  END IF

  DO i = 1, max_zones
                                ! default wind function coefficients

     met_data(i)%coeff(1) = 0.46 ! wind function multiplier
     met_data(i)%coeff(2) = 9.2  ! wind function offset
     met_data(i)%coeff(3) = 0.47 ! conduction coefficient
     met_data(i)%coeff(4) = 0.65 ! "brunt" coefficient for lw atm radiation

     ALLOCATE(met_data(i)%table_entry(max_times), STAT = alloc_stat)
     IF (alloc_stat /= 0) THEN
        WRITE (error_iounit,*) 'allocation failed for met zone table ', i
        CALL EXIT(2)
     ELSE
        WRITE (status_iounit,*) 'allocation successful for met zone table ', i
     END IF
  END DO
	
  DO WHILE(.TRUE.)
     READ(iounit2,*,END=200)met_zone, weather_filename, met_data(met_zone)%coeff
     met_data(met_zone)%start_entry = 0
     met_data(met_zone)%max_entries = 0
     
     WRITE (status_iounit, *) 'Coefficients for weather zone ', met_zone
     WRITE (status_iounit, *) '        Wind a = ' , met_data(met_zone)%coeff(1)
     WRITE (status_iounit, *) '        Wind b = ' , met_data(met_zone)%coeff(2)
     WRITE (status_iounit, *) '    Conduction = ' , met_data(met_zone)%coeff(3)
     WRITE (status_iounit, *) '         Brunt = ' , met_data(met_zone)%coeff(4)
		
     INQUIRE(FILE=weather_filename,EXIST=file_exist)
     IF(file_exist)THEN
        OPEN(iounit1, file = weather_filename)
        WRITE(status_iounit,*)'weather file opened: ',weather_filename
     ELSE
        WRITE(*,*)'weather file does not exist - ABORT: ',weather_filename
        WRITE(status_iounit,*)'weather file does not exist - ABORT: ',weather_filename
        CALL EXIT(2)
     ENDIF

     j = 0
     DO WHILE(.TRUE.)
       j = j + 1
       READ(iounit1,*,END=100)met_data(met_zone)%table_entry(j)%datetime%date_string,&
          & met_data(met_zone)%table_entry(j)%datetime%time_string,met_data(met_zone)%table_entry(j)%value(:)
       met_data(met_zone)%max_entries = met_data(met_zone)%max_entries + 1
			 date_string = met_data(met_zone)%table_entry(j)%datetime%date_string
			 time_string = met_data(met_zone)%table_entry(j)%datetime%time_string
       met_data(met_zone)%table_entry(j)%datetime%time = date_to_decimal()

		END DO
100		CLOSE(iounit1)
		END DO
200 CLOSE(iounit2)


END SUBROUTINE read_met_data

!#####################################################################################################
SUBROUTINE update_met_data(time, met_zone)
  USE logicals, ONLY: do_gas
	IMPLICIT NONE
	DOUBLE PRECISION :: interp(5)
	DOUBLE PRECISION :: time
	INTEGER ::  i, j, num_values = 5, met_zone, jstart
	DOUBLE PRECISION :: factor

        jstart = MAX(met_data(met_zone)%start_entry, 1)
	DO j=jstart,met_data(met_zone)%max_entries-1

           IF((time >= met_data(met_zone)%table_entry(j)%datetime%time)&
                & .AND. (time <= met_data(met_zone)%table_entry(j+1)%datetime%time)) EXIT

	END DO
        met_data(met_zone)%start_entry =&
             & MAX(MIN(j - 1, met_data(met_zone)%max_entries - 2), 0)
        factor = (time - met_data(met_zone)%table_entry(j)%datetime%time)/ &
             (met_data(met_zone)%table_entry(j+1)%datetime%time - met_data(met_zone)%table_entry(j)%datetime%time)

	interp(1:num_values) = met_data(met_zone)%table_entry(j)%value(1:num_values) +&
             & factor*(met_data(met_zone)%table_entry(j+1)%value(1:num_values) -&
             & met_data(met_zone)%table_entry(j)%value(1:num_values) )
	
	t_air = interp(1)
	t_dew = interp(2)
	windspeed = interp(3)

                                ! let's ignore the barometric pressure
                                ! if we are not doing gas

    IF (do_gas) THEN
       baro_press = interp(4)
    END IF

	net_solar = interp(5)


END SUBROUTINE update_met_data


END MODULE met_data_module
