! ----------------------------------------------------------------
! file: julian.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June  3, 1998 by William A. Perkins
! Last Change: Fri Jun 19 08:52:44 1998 by William A. Perkins <d3g096@WA_PERKINS>
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

MODULE julian

IMPLICIT NONE

CONTAINS

! ----------------------------------------------------------------
! FUNCTION JULDAYS
! 
! This function is a recoding of the julday function that came with
! older versions of XMGR. The basis of the code is algorithm 199 of
! the Collected Algorithms of CACM.  Original comment in code:
!
! Takes a date, and returns a Julian day. A Julian day is the number
! of days since some base date (in the very distant past). Handy for
! getting date of x number of days after a given Julian date (use
! jdate to get that from the Gregorian date). Author: Robert
! G. Tantzen, translator: Nat Howard Translated from the algol
! original in Collected Algorithms of CACM (This and jdate are
! algorithm 199).
!
! ----------------------------------------------------------------
DOUBLE PRECISION FUNCTION JULDAYS(month, day, year, hour, minute, second)

  IMPLICIT NONE
  INTEGER :: month, day, year, hour, minute
  DOUBLE PRECISION :: second
  INTEGER(4) :: m, d, y
  DOUBLE PRECISION :: s
  INTEGER(4) :: c, ya, j
  
  m = month
  d = day
  y = year
  s = hour*3600.0 + minute*60.0 + second

  IF (m .GT. 2) THEN
     m = m - 3
  ELSE
     m = m + 9
     y = y - 1
  ENDIF

  c = y/100
  ya = y - (100 * c)
  j = (146097 * c) / 4 + (1461 * ya) / 4 + (153 * m + 2) / 5 + d + 1721119;
  
  IF (s .LT. 12.0*3600.0) THEN
     j = j - 1
     s = s + 12.0 * 3600.0
  ELSE
     s = s - 12.0 * 3600.0
  ENDIF

  JULDAYS = (REAL(j) + (s / 3600.0)/24.0)

END FUNCTION JULDAYS

! ----------------------------------------------------------------
! FUNCTION NDAYS
! ----------------------------------------------------------------
INTEGER FUNCTION NDAYS(day, month, year)
  IMPLICIT NONE
  INTEGER :: day, month, year
  NDAYS = juldays(month, day, year, 0, 0, 0.0d0)
END FUNCTION NDAYS

! ----------------------------------------------------------------
! SUBROUTINE CALCDATE
!
! This routine is a recoding of the calcdate function which came with
! older versions of XMGR.  Original comment in code:
!
! Julian date converter. Takes a julian date (the number of days since
! some distant epoch or other), and returns an int pointer to static space.
! ip[0] = month;
! ip[1] = day of month;
! ip[2] = year (actual year, like 1977, not 77 unless it was  77 a.d.);
! ip[3] = day of week (0->Sunday to 6->Saturday)
! These are Gregorian.
! Copied from Algorithm 199 in Collected algorithms of the CACM
! Author: Robert G. Tantzen, Translator: Nat Howard
! ----------------------------------------------------------------
SUBROUTINE CALCDATE(jd, m, d, y, h, mi, sec)
  IMPLICIT NONE
  DOUBLE PRECISION :: jd
  INTEGER :: m, d, y, h, mi
  DOUBLE PRECISION :: sec

  INTEGER(4) :: j
  DOUBLE PRECISION :: tmp, frac

  j = jd
  frac = jd - REAL(j) + 1/(2.0*86400) ! add half of one second for rounding
  IF (frac .GE. 0.5) THEN
     frac = frac - 0.5
     j = j + 1
  ELSE
     frac = frac + 0.5
  ENDIF

  j = j - 1721119
  y = (4 * j - 1) / 146097
  j = 4*j - 1 - 146097*y;
  d = j/4
  j = (4*d + 3) /1461
  d = 4*d + 3 - 1461*j
  d = (d + 4)/4
  m = (5*d - 3)/153
  d = 5*d - 3 - 153*m
  d = (d + 5)/5
  y = 100*y + j
  IF (m < 10) THEN
     m = m + 3
  ELSE
     m = m - 9
     y = y + 1
  ENDIF
  tmp = 3600.0*(frac*24.0)
  h = INT(tmp/3600.0)
  tmp = tmp - h*3600.0
  mi = INT(tmp/60.0)
  sec = tmp - mi*60.0
END SUBROUTINE CALCDATE

SUBROUTINE NDYIN(jd, d, m, y)
  IMPLICIT NONE
  DOUBLE PRECISION :: jd
  INTEGER :: d, m, y
  INTEGER :: h, min
  DOUBLE PRECISION :: sec

  CALL CALCDATE(jd, m, d, y, h, min, sec)

END SUBROUTINE NDYIN

END MODULE JULIAN
