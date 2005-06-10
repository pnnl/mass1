
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	transport_bc
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: read in transport (gas and temp) BCs at input nodes
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:	need to spily this up into a separate file for
!           each constituent...
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

SUBROUTINE transport_bc(species_num)

USE utility
USE date_time
USE linkbc_vars
USE file_vars
USE general_vars, ONLY: units
USE date_vars
USE logicals, ONLY : file_exist

IMPLICIT NONE

INTEGER :: count, species_num, transbc_num, tempbc_num

CHARACTER(LEN=100) :: transbc_filename, tempbc_filename
INTEGER :: iounit1 = 50, iounit2 = 51

count = 0

SELECT CASE(species_num)
CASE(1) ! species = 1 is total dissolved gas

iounit2 = fileunit(9)
CALL open_existing(filename(9), iounit2, fatal=.TRUE.)

SELECT CASE(time_option)

	!CASE(1) ! time units are sec,hours,minutes, or days in file

    !DO WHILE(.NOT. EOF(fileunit(9)))
    !count = count + 1
    !READ(fileunit(9),*)transbc_time(count),transbc(count,:)

	! SELECT CASE(units)
	! CASE(2)
	!   transbc(count,:) = transbc(count,:)
	! END SELECT

    !   END DO


CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

		DO WHILE(.TRUE.)
			READ(iounit2,*,END=200)transbc_num,transbc_filename
            
            CALL open_existing(transbc_filename, iounit1, fatal=.TRUE.)
               
			READ(iounit1,*,END=100)transbc_header(transbc_num)
			count = 0
			DO WHILE(.TRUE.)
				count = count + 1
				READ(iounit1,*,END=100)date_string,time_string,transbc(count,transbc_num)
				transbc_time(count,transbc_num) = date_to_decimal(date_string, time_string)
				
			END DO
100		CLOSE(iounit1)
		END DO

		!SELECT CASE(units)
		!CASE(2)
		!   linkbc(count,:) = linkbc(count,:)
		! END SELECT

200 CLOSE(iounit2)

	END SELECT

CASE(2) !speices = 2 is Temperature

	iounit2 = fileunit(17)
    CALL open_existing(filename(17), iounit2, fatal=.TRUE.)

	SELECT CASE(time_option)

		!CASE(1) ! time units are sec,hours,minutes, or days in file

    !DO WHILE(.NOT. EOF(fileunit(17)))
    !count = count + 1
    !READ(fileunit(17),*)tempbc_time(count),tempbc(count,:)

		! SELECT CASE(units)
		! CASE(2)
		!   tempbc(count,:) = tempbc(count,:)
		! END SELECT

    !    END DO

CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

		DO WHILE(.TRUE.)
			READ(iounit2,*,END=400)tempbc_num,tempbc_filename

            CALL open_existing(tempbc_filename, iounit1, fatal=.TRUE.)

			READ(iounit1,*,END=300)tempbc_header(tempbc_num)
			count = 0
			DO WHILE(.TRUE.)
				count = count + 1
				READ(iounit1,*,END=300)date_string,time_string,tempbc(count,tempbc_num)
				tempbc_time(count,tempbc_num) = date_to_decimal(date_string, time_string)
				
			END DO
300		CLOSE(iounit1)
		END DO

		!SELECT CASE(units)
		!CASE(2)
		!   linkbc(count,:) = linkbc(count,:)
		! END SELECT

400 CLOSE(iounit2)

END SELECT

END SELECT

END SUBROUTINE transport_bc
