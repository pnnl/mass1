
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

USE linkbc_vars
USE file_vars
USE general_vars, ONLY: units
USE date_vars
USE logicals, ONLY : file_exist

IMPLICIT NONE

INTEGER :: count, species_num, transbc_num, tempbc_num

DOUBLE PRECISION :: date_to_decimal

CHARACTER(LEN=100) :: transbc_filename, tempbc_filename
INTEGER :: iounit1 = 50, iounit2 = 51

count = 0

SELECT CASE(species_num)
CASE(1) ! species = 1 is total dissolved gas

iounit2 = fileunit(9)
INQUIRE(FILE=filename(9),EXIST=file_exist)
IF(file_exist)THEN
   OPEN(fileunit(9),file=filename(9))
   WRITE(99,*)'transport BC file list opened: ',filename(9)
ELSE
   WRITE(*,*)'transport BC file list does not exist - ABORT: ',filename(9)
   WRITE(99,*)'transport BC file list does not exist - ABORT: ',filename(9)
   CALL EXIT(1)
ENDIF

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
            
            INQUIRE(FILE=transbc_filename,EXIST=file_exist)
            IF(file_exist)THEN
               OPEN(iounit1, file = transbc_filename)
               WRITE(99,*)'transport BC file opened: ',transbc_filename
            ELSE
               WRITE(*,*)'transport BC file does not exist - ABORT: ',transbc_filename
               WRITE(99,*)'transport BC file does not exist - ABORT: ',transbc_filename
               CALL EXIT(1)
            ENDIF
               
			READ(iounit1,*,END=100)transbc_header(transbc_num)
			count = 0
			DO WHILE(.TRUE.)
				count = count + 1
				READ(iounit1,*,END=100)date_string,time_string,transbc(count,transbc_num)
				transbc_time(count,transbc_num) = date_to_decimal()
				
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
    INQUIRE(FILE=filename(17),EXIST=file_exist)
    IF(file_exist)THEN
       OPEN(fileunit(17),file=filename(17))
       WRITE(99,*)'temperature file list opened: ',filename(17)
    ELSE
       WRITE(*,*)'temperature BC file list does not exist - ABORT: ',filename(17)
       WRITE(99,*)'temperature BC file list does not exist - ABORT: ',filename(17)
       CALL EXIT(1)
    ENDIF

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

            INQUIRE(FILE=tempbc_filename,EXIST=file_exist)
            IF(file_exist)THEN
               OPEN(iounit1, file = tempbc_filename)
               WRITE(99,*)'temperature BC file opened: ',tempbc_filename
            ELSE
               WRITE(*,*)'temperature BC file does not exist - ABORT: ',tempbc_filename
               WRITE(99,*)'temperature BC file does not exist - ABORT: ',tempbc_filename
               CALL EXIT(1)
            ENDIF

			READ(iounit1,*,END=300)tempbc_header(tempbc_num)
			count = 0
			DO WHILE(.TRUE.)
				count = count + 1
				READ(iounit1,*,END=300)date_string,time_string,tempbc(count,tempbc_num)
				tempbc_time(count,tempbc_num) = date_to_decimal()
				
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
