
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	link_bc
!
! VERSION and DATE: MASS1 v0.81 10/8/97
!
! PURPOSE: reads a files for a table of link BCs. Upstream inflows
!          downstream stage,...
!						also read hydro project BC file
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
! MOD HISTORY: added checks for linktype=21; mcr 11/14/98
!
!
!***************************************************************
!

SUBROUTINE link_bc


USE linkbc_vars
USE link_vars
USE file_vars
USE general_vars, ONLY: units,maxlinks
USE date_vars
USE logicals, ONLY : file_exist

IMPLICIT NONE

INTEGER :: i,count, link, linkbc_num
DOUBLE PRECISION :: date_to_decimal

CHARACTER(LEN=100) :: linkbc_filename
INTEGER :: iounit1 = 50, iounit2 = 51

! read in general link-related boundary condition table

   INQUIRE(FILE=filename(5), EXIST=file_exist)
   IF(file_exist)THEN
      OPEN(fileunit(5),file=filename(5))
      WRITE(99,*)'link BC list file opened: ',filename(5)
   ELSE
      WRITE(*,*)'link BC list file does not exist - ABORT: ',filename(5)
      WRITE(99,*)'link BC list file does not exist - ABORT: ',filename(5)
      CALL EXIT
  ENDIF

iounit2 = fileunit(5)
count = 0

SELECT CASE(time_option)

CASE(1) ! time units are sec,hours,minutes, or days in file

	!DO WHILE(.NOT. EOF(fileunit(5)))
  !   count = count + 1
  !   READ(fileunit(5),*)linkbc_time(count),linkbc(count,:)

	!	 SELECT CASE(units)
	!	 CASE(2)
	!	   linkbc(count,:) = linkbc(count,:)
	!	 END SELECT
	!END DO

CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

		DO WHILE(.TRUE.)
			READ(iounit2,*,END=200)linkbc_num,linkbc_filename

            INQUIRE(FILE=linkbc_filename,EXIST=file_exist)
            IF(file_exist)THEN
               OPEN(iounit1, file = linkbc_filename)
               WRITE(99,*)'link BC file opened: ',linkbc_filename
            ELSE
               WRITE(*,*)'link BC file does not exist - ABORT: ',linkbc_filename
               WRITE(99,*)'link BC file does not exist - ABORT: ',linkbc_filename
               CALL EXIT
            ENDIF

			READ(iounit1,*,END=100)linkbc_header(linkbc_num)
			count = 0
			DO WHILE(.TRUE.)
				count = count + 1
				READ(iounit1,*,END=100)date_string,time_string,linkbc(count,linkbc_num)
				linkbc_time(count,linkbc_num) = date_to_decimal()
				
			END DO
100		CLOSE(iounit1)
		END DO

		!SELECT CASE(units)
		!CASE(2)
		!   linkbc(count,:) = linkbc(count,:)
		! END SELECT

200 CLOSE(iounit2)
        
END SELECT

! read in link specific boundary condition tables

! read hydro powerhouse spill and generation tables

! you can replace all that junk with this cool array stuff
! but it seems just as easy to loop when you have several conditions
! that can trip this
!		IF( ANY( MASK=(linktype == 6) ) ) CALL hydro_bc

! read tdg spill coefficient tables
! if linktype 6, or 21 is there then open and read file
			DO link=1,maxlinks
				SELECT CASE(linktype(link)) 
					CASE(6,21)
					CALL hydro_bc
					EXIT
				END SELECT
			END DO


END SUBROUTINE link_bc
