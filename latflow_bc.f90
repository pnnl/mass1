
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	latflow_bc
!
! VERSION and DATE: MASS1 v0.75 3/25/98
!
! PURPOSE: reads a file for a table of uniform lateral inflows for
!          a link BCs. 
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

SUBROUTINE latflow_bc


USE linkbc_vars
USE link_vars
USE file_vars
USE general_vars, ONLY: units,maxlinks
USE date_vars
USE logicals, ONLY : file_exist

IMPLICIT NONE

INTEGER :: i,count, latflowbc_num
DOUBLE PRECISION :: date_to_decimal
CHARACTER(LEN=100) :: latflowbc_filename
INTEGER :: iounit1 = 50, iounit2 = 51

! read in general lateral inlfow for link-related boundary condition table
  INQUIRE(FILE=filename(16), EXIST=file_exist)
  IF(file_exist)THEN
     OPEN(fileunit(16),file=filename(16))
     WRITE(99,*)'opened lateral inflow file list: ',filename(16)
  ELSE
     WRITE(*,*)'lateral inflow file list does not exist - ABORT: ',filename(16)
     WRITE(99,*)'lateral inflow file list does not exist - ABORT: ',filename(16)
     CALL EXIT
  ENDIF
  iounit2 = fileunit(16)

  count = 0

	SELECT CASE(time_option)

		!CASE(1) ! time units are sec,hours,minutes, or days in file

    !DO WHILE(.NOT. EOF(fileunit(16)))
    !count = count + 1
    !     READ(fileunit(16),*)latflowbc_time(count),latflowbc(count,:)

		 !SELECT CASE(units)
		! CASE(2)
		!  latflowbc(count,:) = latflowbc(count,:)
		 !END SELECT

     !   END DO

CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

		DO WHILE(.TRUE.)
			READ(iounit2,*,END=200)latflowbc_num,latflowbc_filename

			INQUIRE(FILE=latflowbc_filename,EXIST=file_exist)
            IF(file_exist)THEN
               OPEN(iounit1, file = latflowbc_filename)
               WRITE(99,*)'lateral inflow BC file opened: ',latflowbc_filename
			ELSE
               WRITE(*,*)'lateral inflow BC file does not exist - ABORT: ',latflowbc_filename
               WRITE(99,*)'lateral inflow BC file does not exist - ABORT: ',latflowbc_filename
               CALL EXIT
            ENDIF

            READ(iounit1,*,END=100)latflowbc_header(latflowbc_num)
			count = 0
			DO WHILE(.TRUE.)
				count = count + 1
				READ(iounit1,*,END=100)date_string,time_string,latflowbc(count,latflowbc_num)
				latflowbc_time(count,latflowbc_num) = date_to_decimal()
				
			END DO
100		CLOSE(iounit1)
		END DO

		!SELECT CASE(units)
		!CASE(2)
		!   linkbc(count,:) = linkbc(count,:)
		! END SELECT

200 CLOSE(iounit2)

END SELECT



END SUBROUTINE latflow_bc
