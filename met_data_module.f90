!-----------------------------------------------------------------------------
!
! module to read in and update meteorlogical data needed for air-water heat exchnage
! and dissolved gas concentration to pressure/saturation conversions
!
!-----------------------------------------------------------------------------

MODULE met_data_module

USE date_vars

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
	CHARACTER (LEN=80) :: file_name
	CHARACTER (LEN=10) :: table_type
	INTEGER :: max_entries

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
  CHARACTER(LEN=80) :: met_files, weather_filename
  INTEGER :: max_zones, max_times, status_iounit, error_iounit, alloc_stat
  INTEGER :: iounit1 = 50, iounit2 = 51, i, j = 0, met_zone
  DOUBLE PRECISION :: date_to_decimal

  OPEN(iounit2, file = met_files)
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
     ALLOCATE(met_data(i)%table_entry(max_times), STAT = alloc_stat)
     IF (alloc_stat /= 0) THEN
        WRITE (error_iounit,*) 'allocation failed for met zone table ', i
        CALL EXIT(2)
     ELSE
        WRITE (status_iounit,*) 'allocation successful for met zone table ', i
     END IF
  END DO
	
  DO WHILE(.TRUE.)
     READ(iounit2,*,END=200)met_zone, weather_filename
     met_data(met_zone)%max_entries = 0
     OPEN(iounit1, file = weather_filename)
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
	IMPLICIT NONE
	DOUBLE PRECISION :: interp(5)
	DOUBLE PRECISION :: time
	INTEGER ::  i, j, num_values = 5, met_zone
	DOUBLE PRECISION :: factor

	DO j=1,met_data(met_zone)%max_entries-1

           IF((time >= met_data(met_zone)%table_entry(j)%datetime%time)&
                & .AND. (time <= met_data(met_zone)%table_entry(j+1)%datetime%time)) EXIT

	END DO
        factor = (time - met_data(met_zone)%table_entry(j)%datetime%time)/ &
             (met_data(met_zone)%table_entry(j+1)%datetime%time - met_data(met_zone)%table_entry(j)%datetime%time)

	interp(1:num_values) = met_data(met_zone)%table_entry(j)%value(1:num_values) +&
             & factor*(met_data(met_zone)%table_entry(j+1)%value(1:num_values) -&
             & met_data(met_zone)%table_entry(j)%value(1:num_values) )
	
	t_air = interp(1)
	t_dew = interp(2)
	windspeed = interp(3)
	baro_press = interp(4)
	net_solar = interp(5)


END SUBROUTINE update_met_data


END MODULE met_data_module
