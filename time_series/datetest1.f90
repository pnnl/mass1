! ----------------------------------------------------------------
! file: datetest1.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January 23, 2003 by William A. Perkins
! Last Change: 2017-06-22 09:22:33 d3g096
! ----------------------------------------------------------------
! RCS ID: $Id$ Battelle PNL

PROGRAM datetest1

  USE date_time
  USE utility

  IMPLICIT NONE

  CHARACTER (LEN=80) :: ds, ts, buf1, buf2
  DOUBLE PRECISION :: time
  INTEGER :: i

  DO i = 1, 2

     IF (i .EQ. 1) THEN
        CALL date_time_flags()
        WRITE (*,*) "Using default options ..."
        WRITE (*,*) 
     ELSE
        CALL date_time_flags(ydigits=5,sdigits=2)
        WRITE (*,*) "Using different digits settings"
        WRITE (*,*) 
     END IF

     CALL open_existing('datetest1.dat', 15)

     DO WHILE (.TRUE.)
        READ (15, *, END=100) ds, ts
        time = date_to_decimal(ds, ts)
        buf1 = '"' // TRIM(ds) // " " // TRIM(ts) // '"'
        IF (time .GT. 0.0) THEN
           CALL date_format(time, buf2)
           WRITE(*, 200) TRIM(buf1), time, TRIM(buf2)
        ELSE
           WRITE(*, 205) TRIM(buf1), 'ERROR'
        END IF
     END DO

100  CLOSE(15)

  END DO

200 FORMAT(A25, ' = ', F16.7, ' ', A25)
205 FORMAT(A25, ' = ', A25) 
END PROGRAM datetest1
