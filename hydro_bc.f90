
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	hydro_bc
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: reads a table of generation and spill discharges for
!          internal BCs for hydropower links
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
!
!
!***************************************************************
!

SUBROUTINE hydro_bc


USE linkbc_vars
USE file_vars
USE general_vars, ONLY: units
USE date_vars
USE logicals, ONLY: file_exist

IMPLICIT NONE

INTEGER :: count, linkbc_num
CHARACTER(LEN=100) :: hydrobc_filename
INTEGER :: iounit1 = 50, iounit2 = 51, i, j = 0
DOUBLE PRECISION :: date_to_decimal

iounit2 = fileunit(10)

INQUIRE(FILE=filename(10),EXIST=file_exist)
IF(file_exist)THEN
   OPEN(fileunit(10),file=filename(10))
   WRITE(99,*)'opening hydro BC file list: ',filename(10)
ELSE
   WRITE(*,*)'hydro BC file list does not exist - ABORT: ',filename(10)
   WRITE(99,*)'hydro BC file list does not exist - ABORT: ',filename(10)
   CALL EXIT(1)
ENDIF

count = 0

SELECT CASE(time_option)
	!need to overhaul this; use date/time for now
	CASE(1) ! time units are sec,hours,minutes, or days in file

    !DO WHILE(.NOT. EOF(fileunit(10)))
    ! READ(fileunit(10),*)gen_time(count),gen_flow(count,:)
		! READ(fileunit(10),*)spill_time(count),spill_flow(count,:)

		! SELECT CASE(units)
		! CASE(2)
		!   gen_flow(count,:) = gen_flow(count,:)
		!   spill_flow(count,:) = spill_flow(count,:)
		! END SELECT
		!END DO

	CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day
	
		DO WHILE(.TRUE.)
			READ(iounit2,*,END=200)linkbc_num,hydrobc_filename

			INQUIRE(FILE=hydrobc_filename,EXIST=file_exist)
            IF(file_exist)THEN
               OPEN(iounit1, file = hydrobc_filename)
               WRITE(99,*)'opening hydro BC file: ',hydrobc_filename
			ELSE
               WRITE(*,*)'hydro BC file does not exist - ABORT: ',hydrobc_filename
               WRITE(99,*)'hydro BC file does not exist - ABORT: ',hydrobc_filename
               CALL EXIT(1)
            ENDIF
            READ(iounit1,*,END=100)hydrobc_header(linkbc_num)
			count = 0
			DO WHILE(.TRUE.)
				count = count + 1
				READ(iounit1,*,END=100)date_string,time_string,gen_flow(count,linkbc_num),spill_flow(count,linkbc_num)
				gen_time(count,linkbc_num) = date_to_decimal()
				spill_time(count,linkbc_num) = date_to_decimal()
			END DO
100		CLOSE(iounit1)
		END DO

		 !SELECT CASE(units)
		 !CASE(2)
		 !  gen_flow(count,:) = gen_flow(count,:)
		 !  spill_flow(count,:) = spill_flow(count,:)
		 !END SELECT

200 CLOSE(iounit2)
        
	END SELECT

END SUBROUTINE hydro_bc
