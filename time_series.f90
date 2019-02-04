! ----------------------------------------------------------------
! file: time_series.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January 21, 2002 by William A. Perkins
! Last Change: 2018-05-02 14:39:54 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE time_series
! ----------------------------------------------------------------
MODULE time_series

  USE date_time
  USE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PRIVATE, PARAMETER :: max_fields = 10
  DOUBLE PRECISION, PRIVATE, PARAMETER :: bogus = -9999.0

  CHARACTER (LEN=1024), PRIVATE :: buf, buf1

  ! ----------------------------------------------------------------
  ! TYPE time_series_point
  ! stores a single time with multiple values
  ! ----------------------------------------------------------------
  TYPE time_series_point
     DOUBLE PRECISION :: time
     DOUBLE PRECISION, POINTER :: field(:)
  END TYPE time_series_point

  ! ----------------------------------------------------------------
  ! TYPE time_series_rec
  ! ----------------------------------------------------------------
  TYPE time_series_rec
     INTEGER :: id
     CHARACTER (LEN=1024) :: filename
     INTEGER :: fields, length
     INTEGER :: start
     INTEGER :: limit_mode
     TYPE (time_series_point), POINTER :: series(:)
     DOUBLE PRECISION, POINTER :: current(:)
  END TYPE time_series_rec

                                ! mode determines interpolated value
                                ! when outside the series range

  INTEGER, PUBLIC, PARAMETER :: &
       &TS_LIMIT_NONE = 0, &    ! do not allow time outside the series range
       &TS_LIMIT_FLAT = 1, &    ! flatline when outside the series range
       &TS_LIMIT_EXTRAP = 2     ! extrapolate when outside the series range

  INTEGER, PRIVATE :: default_limit_mode 

                                ! mode determines the form dates take
                                ! in the input file

  INTEGER, PUBLIC, PARAMETER :: &
       &TS_DATE_MODE = 0, &     ! dates are expected
       &TS_REAL_MODE = 1        ! real numbers are expected

  INTEGER, PRIVATE :: time_series_mode

                                ! each time series is assigned an
                                ! integer id
  INTEGER, PRIVATE :: nextid

                                ! the module can be initialized to
                                ! report error and status messages to
                                ! an open file unit

  ! INTEGER, PRIVATE :: logunit, errunit

  ! ----------------------------------------------------------------

  ! The debug_level is used to get various messages in the log file
  ! about the goings on within the module.  Current values are as follows: 

  !     debug_level .LE. 0: (the default) only errors reported
  !     0 .LT. debug_level .LE. 5: in addition, file operations are reported and summarized
  !     5 .LT. debug_level .LE. 10: in addition, interpolation is reported
  !     10 .LT. debug_level: in addition, all module calls and some other stuff are reported 
  ! ----------------------------------------------------------------

  INTEGER, PRIVATE :: debug_level

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_error
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_error(msg, fatal, ts)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    TYPE (time_series_rec), POINTER, OPTIONAL :: ts
    CHARACTER (LEN=1024) :: buf, buf1

    LOGICAL :: myfatal

    myfatal = .FALSE.
    IF (PRESENT(fatal)) myfatal = fatal

    IF (PRESENT(ts)) THEN
       WRITE (buf1, *) ts%id
       WRITE (buf, *) 'ERROR: time series ', TRIM(buf1), ' (', TRIM(ts%filename), &
            &'): ', TRIM(msg)
    ELSE
       WRITE (buf, *) 'ERROR: time series: ', TRIM(msg)
    END IF
    IF (myfatal) buf = 'FATAL ' // TRIM(buf)

    CALL error_message(buf, myfatal)
  END SUBROUTINE time_series_error
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_log
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_log(msg, ts)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER, OPTIONAL :: ts
    CHARACTER (LEN=*), INTENT(IN) :: msg
    CHARACTER (LEN=1024) :: buf, buf1

    IF (PRESENT(ts)) THEN
       WRITE (buf1, *) ts%id
       WRITE (buf, *) 'STATUS: time series ', TRIM(buf1), ': ', TRIM(ts%filename), &
            &': ', TRIM(msg)
    ELSE
       WRITE (buf, *) 'STATUS: time series: ', TRIM(msg)
    END IF

    CALL status_message(buf)

  END SUBROUTINE time_series_log

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_module_init
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_module_init(debug, limit, mode)

    IMPLICIT NONE

    INTEGER, OPTIONAL, INTENT(IN) :: limit, mode
    INTEGER, OPTIONAL, INTENT(IN) :: debug
    CHARACTER (LEN=1024) :: buf

    debug_level = 0
    default_limit_mode = TS_LIMIT_NONE
    time_series_mode = TS_DATE_MODE

    IF (PRESENT(debug))  debug_level = debug

    IF (debug_level .GT. 10) &
         &CALL time_series_log('Entering time_series_module_init ...')

    IF (PRESENT(limit)) THEN
       SELECT CASE (limit)
       CASE (TS_LIMIT_NONE, TS_LIMIT_FLAT, TS_LIMIT_EXTRAP)
          default_limit_mode = limit
       CASE DEFAULT
          WRITE (buf, *) 'Invalid limit mode: ', limit
          CALL time_series_error(buf, fatal = .TRUE.)
       END SELECT
    END IF

    IF (PRESENT(mode)) THEN
       SELECT CASE (mode)
       CASE (TS_DATE_MODE, TS_REAL_MODE)
          time_series_mode = mode
       CASE DEFAULT
          WRITE (buf, *) 'Invalid date/time format mode: ', mode
          CALL time_series_error(buf, fatal = .TRUE.)
       END SELECT
    END IF

    nextid = 1

    IF (debug_level .GT. 5) THEN
       CALL time_series_log("module initialized")
       buf = 'default limit mode: '
       SELECT CASE (default_limit_mode)
       CASE (TS_LIMIT_NONE)
          buf = TRIM(buf) // ' Times not allowed outside series'
       CASE (TS_LIMIT_FLAT)
          buf = TRIM(buf) // ' Flat line for times outside series'
       CASE (TS_LIMIT_EXTRAP)
          buf = TRIM(buf) // ' Extrapolate for times outside series'
       CASE DEFAULT
          WRITE (buf, *) 'limit mode not understood ', default_limit_mode
       END SELECT
       CALL time_series_log(buf)

       buf = 'date/time format: '
       SELECT CASE (time_series_mode)
       CASE (TS_DATE_MODE)
          buf = TRIM(buf) // ' Calendar dates'
       CASE (TS_REAL_MODE)
          buf = TRIM(buf) // ' Real numbers'
       CASE DEFAULT
          WRITE (buf, *) 'date/time format mode not understood ', time_series_mode
       END SELECT
       CALL time_series_log(buf)
    END IF

    IF (debug_level .GT. 10) &
         &CALL time_series_log("Leaving time_series_module_init ...")

  END SUBROUTINE time_series_module_init

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_module_done
  ! clean up anything that needs to be cleaned up
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_module_done()

    IMPLICIT NONE

    IF (debug_level .GT. 5) CALL time_series_log("module finished")

  END SUBROUTINE time_series_module_done


  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_point_init
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_point_init(pt, fields)

    IMPLICIT NONE
    TYPE (time_series_point) :: pt
    INTEGER, INTENT(IN), OPTIONAL :: fields

    INTEGER :: nfld

    IF (debug_level .GT. 10) &
         &CALL time_series_log("Entering time_series_point_init ...")

    nfld = 1
    IF (PRESENT(fields)) nfld = fields

    pt%time = 0.0
    ALLOCATE(pt%field(fields))
    pt%field = bogus

    IF (debug_level .GT. 10) &
         &CALL time_series_log("Leaving time_series_point_init ...")

  END SUBROUTINE time_series_point_init

  ! ----------------------------------------------------------------
  ! TYPE (TIME_SERIES_REC) FUNCTION time_series_alloc
  ! ----------------------------------------------------------------
  TYPE (TIME_SERIES_REC) FUNCTION time_series_alloc(id, fields, length) RESULT (ts)

    IMPLICIT NONE
    
    POINTER ts
    INTEGER, INTENT(IN) :: length, id, fields
    INTEGER :: i
    CHARACTER (LEN=1024) :: buf

    ALLOCATE(ts)
    ts%filename = '(none)'
    ts%length = length
    ts%fields = fields
    ts%start = 1
    ts%id = id
    ts%limit_mode = default_limit_mode
    ALLOCATE(ts%current(fields))
    ts%current = 0.0
    ALLOCATE(ts%series(length))

    DO i = 1, length
       CALL time_series_point_init(ts%series(i), fields)
    END DO
       
    IF (debug_level .GT. 0) THEN
       WRITE(buf, *) 'created with ', fields, ' fields and ', length, ' times'
       CALL time_series_log(buf, ts)
    END IF
    
  END FUNCTION time_series_alloc

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_destroy(ts)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER :: ts
    INTEGER :: i

    IF (.NOT. ASSOCIATED(ts)) RETURN

    DO i = 1, ts%length
       DEALLOCATE(ts%series(i)%field)
    END DO
    DEALLOCATE(ts%series, ts%current)
    DEALLOCATE(ts)

    NULLIFY(ts)

  END SUBROUTINE time_series_destroy

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_point_push
  !
  ! This routine can be used to push a point onto the end of a time
  ! series.  The time series length remains the same.  All points are
  ! pushed to the beginning of the series.  This should NEVER be used
  ! with long series. Maximum length should probably be 2 or 3. 
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_push(ts, datetime, field)

    IMPLICIT NONE
    TYPE (time_series_rec), INTENT(INOUT), POINTER :: ts
    DOUBLE PRECISION, INTENT(IN) :: datetime
    DOUBLE PRECISION, INTENT(IN) :: field(:)

    INTEGER :: i, n

    n = ts%length

    DO i = 2, n
       ts%series(i-1)%time = ts%series(i)%time
       ts%series(i-1)%field(1:ts%fields) =  ts%series(i)%field(1:ts%fields) 
    END DO
    ts%series(n)%time = datetime
    ts%series(n)%field(1:ts%fields) = field(1:ts%fields)

    ts%start = MAX(1, ts%start - 1)

  END SUBROUTINE time_series_push



  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION time_series_point_read
  ! This routine reads a single time series point from the specified
  ! file.  It returns the number of fields read, zero if end of file
  ! occurs, -1 if a date error occurs, or -2 if any other error occurs.
  ! ----------------------------------------------------------------
  INTEGER FUNCTION time_series_point_read(iounit, pt, onefield) RESULT (nfld)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iounit
    TYPE (time_series_point), INTENT(INOUT), OPTIONAL :: pt
    LOGICAL, INTENT(IN), OPTIONAL :: onefield

    CHARACTER (LEN=20) :: sdate
    CHARACTER (LEN=20) :: stime
    DOUBLE PRECISION :: x(max_fields), datetime
    LOGICAL :: fld1

    fld1 = .FALSE.
    IF (PRESENT(onefield)) fld1 = onefield

    nfld = 0
    x = bogus

    SELECT CASE (time_series_mode)
    CASE (TS_DATE_MODE)

                                ! kludge so a slash does not have to
                                ! appear at the end of the line
       IF (fld1) THEN
          READ(iounit, *, END=100, ERR=200) sdate, stime, x(1)
       ELSE
          READ(iounit, *, END=100, ERR=200) sdate, stime, x
       END IF

                                ! convert date/time to number
                                ! check to make sure it's valid

       datetime =  date_to_decimal(sdate, stime)

                                ! if date_to_decimal returns 0.0, the
                                ! date string is wrong
       IF (datetime .EQ. 0.0) THEN
          nfld = -1 
          RETURN
       END IF
    CASE (TS_REAL_MODE)
                                ! kludge so a slash does not have to
                                ! appear at the end of the line
       IF (fld1) THEN
          READ(iounit, *, END=100, ERR=200) datetime, x(1)
       ELSE
          READ(iounit, *, END=100, ERR=200) datetime, x
       END IF
    CASE DEFAULT
       CALL time_series_error('Module error: unknown time mode')
    END SELECT

                                ! count the number of fields read, and
                                ! make sure it is correct

    DO WHILE (x(nfld+1) .NE. bogus) 
       nfld = nfld + 1
    END DO

                                ! if there is someplace to put it,
                                ! pass on the data read

    IF (PRESENT(pt)) THEN
       pt%time = datetime
       pt%field(1:nfld) = x(1:nfld)
    END IF
    RETURN
200 nfld = -2
100 RETURN
  END FUNCTION time_series_point_read

  
  ! ----------------------------------------------------------------
  ! TYPE (time_series_rec) FUNCTION read_time_series
  ! ----------------------------------------------------------------
  TYPE (time_series_rec)  FUNCTION time_series_read(filename, fields, id, unit)

    IMPLICIT NONE

    POINTER time_series_read

    CHARACTER (LEN=*) :: filename
    INTEGER, INTENT(IN), OPTIONAL :: fields
    INTEGER, INTENT(IN), OPTIONAL :: id
    INTEGER, INTENT(IN), OPTIONAL :: unit

    INTEGER :: myfld, myid, length, iounit, ierr, nfld, i

    IF (.NOT. PRESENT(fields)) THEN
       myfld = max_fields
    ELSE
       myfld = fields
    END IF
    IF (.NOT. PRESENT(id)) THEN
       myid = nextid
       nextid = nextid + 1
    ELSE 
       myid = id
    END IF
    IF (.NOT. PRESENT(unit)) THEN
       iounit = 1
    ELSE
       iounit = unit
    END IF
    

    IF (debug_level .GT. 10) &
         &CALL time_series_log("Entering time_series_read ...")

    IF (debug_level .GT. 0) &
         &CALL time_series_log("Attempting to open " // TRIM(filename))


    CALL open_existing(filename, iounit)

    IF (debug_level .GT. 0) &
         &CALL time_series_log(TRIM(filename) // " successfully opened")

    READ (iounit, *) buf       ! throw away first line in file

    length = 0
    ierr = 0
    i = 1
    DO WHILE (.TRUE.)
       nfld = time_series_point_read(iounit, onefield=(fields .EQ. 1))
       i = i + 1
       IF (nfld .LE. myfld .AND. nfld .GT. 0) THEN
          length = length + 1
       ELSE IF (nfld .EQ. 0) THEN
          EXIT
       ELSE
          WRITE (buf, *) TRIM(filename), ': line ', i, &
               &': fields read = ', nfld, ' (expected ', myfld, ')'
          CALL time_series_error(buf)
          ierr = ierr + 1
       END IF
    END DO

    IF (ierr .GT. 0) THEN
       WRITE (buf, *) TRIM(filename), ': too many errors'
       CALL time_series_error(buf, fatal = .TRUE.)
    END IF

                                ! now that we know how many points we
                                ! have, allocate the time series
                                ! record

    time_series_read => time_series_alloc(myid, myfld, length)
    time_series_read%filename = filename

    REWIND(iounit)

    READ (iounit, *) buf       ! throw away first line in file

    ierr = 0
    DO i = 1, time_series_read%length
       nfld = time_series_point_read(iounit, time_series_read%series(i), onefield=(fields .EQ. 1))
       ! IF (nfld .LT. time_series_read%fields) THEN
       !    WRITE (buf, *) 'unexpected error, line ', i, '(', nfld, ')'
       !    CALL time_series_error(buf, ts = time_series_read, fatal = .TRUE.)
       ! END IF
    END DO

    CLOSE (iounit)

    IF (debug_level .GT. 0) THEN 
       WRITE (buf, *) 'successfully read ', time_series_read%length , ' points'
       CALL time_series_log(buf, ts = time_series_read)

       SELECT CASE (time_series_mode)
       CASE (TS_DATE_MODE) 
          CALL date_format(time_series_read%series(1)%time, buf)
          WRITE(buf, *) 'start = ', TRIM(buf)
          CALL time_series_log(buf, ts = time_series_read)
          CALL date_format(time_series_read%series(time_series_read%length )%time, buf)
          WRITE(buf, *) 'end = ', TRIM(buf)
          CALL time_series_log(buf, ts = time_series_read)
       CASE (TS_REAL_MODE)
          WRITE(buf, *) 'start = ', time_series_read%series(1)%time
          CALL time_series_log(buf, ts = time_series_read)
          WRITE(buf, *) 'end = ', time_series_read%series(time_series_read%length)%time
          CALL time_series_log(buf, ts = time_series_read)
       END SELECT
    END IF

    IF (debug_level .GT. 10) &
         &CALL time_series_log("Leaving time_series_read ...")

  END FUNCTION time_series_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_interp
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_interp(ts, datetime)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER :: ts
    DOUBLE PRECISION, INTENT(IN) :: datetime

    CHARACTER (LEN=1024) :: dstr
    INTEGER :: i
    DOUBLE PRECISION :: factor

    IF (debug_level .GT. 10) THEN
       WRITE(buf,*) 'Entering time_series_interp with time = ', datetime
       CALL time_series_log(buf)
    END IF

    IF (ts%length .LT. 2 .AND. ts%limit_mode .EQ. TS_LIMIT_FLAT) THEN
       ts%current = ts%series(1)%field
       RETURN
    ELSE IF (ts%length .LT. 2) THEN
       WRITE(buf, *) 'one point time series are allowed only with TS_LIMIT_FLAT'
       CALL time_series_error(buf, ts = ts, fatal = .TRUE.)
    ELSE IF (datetime .LT. ts%series(1)%time) THEN
       SELECT CASE (ts%limit_mode)
       CASE (TS_LIMIT_NONE)
          SELECT CASE(time_series_mode)
          CASE (TS_DATE_MODE)
             CALL date_format(datetime, dstr)
             WRITE (buf,*) 'date (', TRIM(dstr), ' out of range'
          CASE (TS_REAL_MODE)
             WRITE (buf,*) 'ordinate (', datetime, ' out of range'
          END SELECT
          CALL time_series_error(buf, ts = ts, fatal = .TRUE.)
       CASE (TS_LIMIT_FLAT)
          ts%current = ts%series(1)%field
          RETURN
       CASE (TS_LIMIT_EXTRAP)
          i = 1
       END SELECT
    ELSE IF (datetime .GT. ts%series(ts%length)%time) THEN
       SELECT CASE (ts%limit_mode)
       CASE (TS_LIMIT_NONE)
          SELECT CASE(time_series_mode)
          CASE (TS_DATE_MODE)
             CALL date_format(datetime, dstr)
             WRITE (buf,*) 'date (', TRIM(dstr), ' out of range'
          CASE (TS_REAL_MODE)
             WRITE (buf,*) 'ordinate (', datetime, ' out of range'
          END SELECT
          CALL time_series_error(buf, ts = ts, fatal = .TRUE.)
       CASE (TS_LIMIT_FLAT)
          ts%current = ts%series(ts%length)%field
          RETURN
       CASE (TS_LIMIT_EXTRAP)
          i = ts%length - 1
       END SELECT
    ELSE 
       i = ts%start
       DO i = ts%start, ts%length - 1
          IF (datetime .GE. ts%series(i)%time .AND.&
               &datetime .LE. ts%series(i+1)%time) EXIT
       END DO
       IF (i .GT. 1) ts%start = i - 1
    END IF

    factor = (datetime - ts%series(i)%time)/&
         &(ts%series(i + 1)%time - ts%series(i)%time)

    IF (debug_level .GT. 10) THEN
       WRITE(buf,*) 'interpolating between ', &
            &ts%series(i + 1)%time, ' and ', ts%series(i)%time
       CALL time_series_log(buf)
    END IF

    ts%current = factor*(ts%series(i+1)%field - ts%series(i)%field) +&
         &ts%series(i)%field

    IF (debug_level .GT. 10) &
         &CALL time_series_log('Leaving time_series_interp')

  END SUBROUTINE time_series_interp

  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION time_series_increases
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION time_series_increases(ts, field, fix)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER :: ts
    INTEGER, INTENT(IN), OPTIONAL :: field
    LOGICAL, INTENT(IN), OPTIONAL :: fix

    INTEGER :: myfld, i, nfix
    LOGICAL :: flag, myfix
    CHARACTER (LEN=1024) :: msg, buf

    nfix = 0
    myfld = 1
    myfix = .TRUE.

    IF (PRESENT(field)) myfld = field
    IF (PRESENT(fix)) myfix = fix

    time_series_increases = .TRUE.

    IF (ts%length < 2) RETURN

    DO i = 2, ts%length
       flag = (ts%series(i)%field(myfld) .GE. ts%series(i-1)%field(myfld))
       IF (.NOT. flag) THEN
          SELECT CASE (time_series_mode)
          CASE (TS_DATE_MODE)
             CALL date_format(ts%series(i-1)%time, buf)
             WRITE(msg, 100)  myfld, buf, ts%series(i-1)%field(myfld), ts%series(i)%field(myfld)
          CASE DEFAULT
             WRITE(msg, 102)  myfld, ts%series(i-1)%time, ts%series(i-1)%field(myfld), ts%series(i)%field(myfld)
          END SELECT
          CALL time_series_error(msg, ts=ts, fatal=.FALSE.)
          IF (myfix) THEN
             SELECT CASE (time_series_mode)
             CASE (TS_DATE_MODE)
                WRITE(msg, 110) buf, ts%series(i)%field(myfld), ts%series(i-1)%field(myfld)
             CASE DEFAULT
                WRITE(msg, 112) ts%series(i-1)%time, ts%series(i)%field(myfld), ts%series(i-1)%field(myfld)
             END SELECT
             CALL time_series_error(msg, ts=ts, fatal=.FALSE.)
             ts%series(i)%field(myfld) = ts%series(i-1)%field(myfld)
             nfix = nfix + 1
          END IF
          time_series_increases = .FALSE.
       END IF
    END DO

    IF (.NOT. time_series_increases) THEN
       WRITE(msg,*) 'field ', myfld, ' does not increase always'
       CALL time_series_error(msg, ts=ts, fatal=.FALSE.)
       IF (myfix) THEN
          WRITE(msg,*) nfix, ' values modified'
          CALL time_series_error(msg, ts=ts, fatal=.FALSE.)
       END IF
    END IF

100 FORMAT('field ', I2, ' does not monotonically increase at time ', A25, '(', G10.4, '>', G10.4, ')')
102 FORMAT('field ', I2, ' does not monotonically increase at time ', G10.4, '(', G10.4, '>', G10.4, ')')
110 FORMAT('changed value at time ', A25, ' from ', G10.4, ' to ', G10.4)
112 FORMAT('changed value at time ', G10.4, ' from ', G10.4, ' to ', G10.4)
  END FUNCTION time_series_increases



END MODULE time_series
