
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	decimal_to_date
!
! VERSION and DATE: MASS1 v0.80 10/4/97
!
! PURPOSE: coverts a decimal date to a date/time string
!
! RETURNS: time_string & time_string
!
! REQUIRED:	Uses IMSL routine NDYIN
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY: fixed time roundoff problem; mcr 10-4-98
!
!
!***************************************************************
!

SUBROUTINE decimal_to_date

USE JULIAN
USE date_vars
USE general_vars, ONLY : time

IMPLICIT NONE

DOUBLE PRECISION :: sec
INTEGER :: days,month,day,year,hours,min, isec

CALL CALCDATE(time, month, day, year, hours, min, sec)
isec = sec

WRITE(date_string(7:10),'(i4.4)')year
WRITE(date_string(4:5),'(i2.2)')day
WRITE(date_string(1:2),'(i2.2)')month

WRITE(time_string(1:2),'(i2.2)')hours
WRITE(time_string(4:5),'(i2.2)')min
WRITE(time_string(7:8),'(i2.2)')isec

END SUBROUTINE decimal_to_date
