
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	date_to_decimal
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: coverts a date/time string to decimal julian days
!
! RETURNS: decimal date
!
! REQUIRED:	IMSL routine NDAYS
!
! LOCAL VARIABLES:
!
! COMMENTS:	some luking precision problems that round things off
!	to seconds if a non-rational time increment is used.
!	eg. 1.333333. may be related to interaction with C++B
! fixed the time problem; mcr 10-4-98
!
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

DOUBLE PRECISION FUNCTION date_to_decimal()

USE julian
USE date_vars
 
DOUBLE PRECISION :: dec_hr,dec_mm,dec_ss 
INTEGER :: days,mon,dd,yr,hh,mm,ss

READ(date_string(1:2),'(i2)')mon
READ(date_string(4:5),'(i2)')dd
READ(date_string(7:10),'(i4)')yr
READ(time_string(1:2),'(i2)')hh
READ(time_string(4:5),'(i2)')mm
READ(time_string(7:8),'(i2)')ss

date_to_decimal = juldays(mon, dd, yr, hh, mm, DBLE(ss))

END FUNCTION date_to_decimal
