! ----------------------------------------------------------------
! file: julian.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June  3, 1998 by William A. Perkins
! Last Change: 2017-06-22 09:23:33 d3g096
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

MODULE julian

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

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
  
  IF (s .LT. 12.0*3600.0d0) THEN
     j = j - 1
     s = s + 12.0 * 3600.0d0
  ELSE
     s = s - 12.0 * 3600.0d0
  ENDIF

  JULDAYS = (DBLE(j) + (s / 3600.0d0)/24.0d0)

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

  INTEGER(KIND=4) :: j,mm, dd, yy
  DOUBLE PRECISION :: tmp, frac
  INTEGER(KIND=4), PARAMETER :: LONG = 1

  j = jd
  frac = jd - DBLE(j)
  IF (frac .GE. 0.5) THEN
     frac = frac - 0.5
     j = j + 1
  ELSE
     frac = frac + 0.5
  ENDIF

  j = j - 1721119
  yy = (4 * j - 1) / 146097
  j = 4*j - 1 - 146097*yy;
  dd = j/4
  j = (4*dd + 3) /1461
  dd = 4*dd + 3 - 1461*j
  dd = (dd + 4)/4
  mm = (5*dd - 3)/153
  dd = 5*dd - 3 - 153*mm
  dd = (dd + 5)/5
  yy = 100*yy + j
  y = yy
  m = mm
  d = dd
  IF (m < 10) THEN
     m = m + 3
  ELSE
     m = m - 9
     y = y + 1
  ENDIF
  tmp = 3600.0d0*(frac*24.0d0) + 5d-5
  h = INT(tmp/3600.0d0)
  tmp = tmp - DBLE(h)*3600.0d0
  mi = INT(tmp/60.0d0)
  sec = tmp - DBLE(mi)*60.0d0
END SUBROUTINE CALCDATE

SUBROUTINE NDYIN(jd, d, m, y)
  IMPLICIT NONE
  DOUBLE PRECISION :: jd
  INTEGER :: d, m, y
  INTEGER :: h, min
  DOUBLE PRECISION :: sec

  CALL CALCDATE(jd, m, d, y, h, min, sec)

END SUBROUTINE NDYIN

! ----------------------------------------------------------------
! INTEGER FUNCTION dayofyear
! ----------------------------------------------------------------
INTEGER FUNCTION dayofyear(jdate)
  IMPLICIT NONE

  DOUBLE PRECISION :: jdate
  INTEGER :: m, d, y, h, mi
  DOUBLE PRECISION :: sec

  CALL calcdate(jdate, m, d, y, h, mi, sec)
  m = 1
  d = 1
  h = 0
  mi = 0
  sec = 0.0
  dayofyear = INT(jdate - juldays(m, d, y, h, m, sec)) + 1
END FUNCTION dayofyear

! ----------------------------------------------------------------
! INTEGER FUNCTION year
! ----------------------------------------------------------------
INTEGER FUNCTION year(jdate)

  IMPLICIT NONE

  DOUBLE PRECISION :: jdate
  INTEGER :: m, d, y, h, mi
  DOUBLE PRECISION :: sec

  CALL calcdate(jdate, m, d, y, h, mi, sec)
  year = y

END FUNCTION year



END MODULE JULIAN
