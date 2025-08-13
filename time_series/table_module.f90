! ----------------------------------------------------------------
! file: table_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 30, 2020 by Perkins
! Last Change: 2020-05-07 12:06:59 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE table_module
! ----------------------------------------------------------------
MODULE table_module

  USE utility
  
  IMPLICIT NONE
  PRIVATE

                                ! mode determines interpolated value
                                ! when outside the series range

  INTEGER, PUBLIC, PARAMETER :: &
       & TBL_LIMIT_NONE = 0, &  ! do not allow ordinate outside the table range
       & TBL_LIMIT_FLAT = 1, &  ! flatline when outside the table range
       & TBL_LIMIT_EXTRAP = 2   ! extrapolate when outside the table range

  ! ----------------------------------------------------------------
  ! TYPE table
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: table
     INTEGER :: fields, rows
     INTEGER :: limit_mode
     INTEGER :: idx
     DOUBLE PRECISION, POINTER :: x(:)
     DOUBLE PRECISION, POINTER :: data(:,:)
     DOUBLE PRECISION, POINTER :: current(:)
   CONTAINS
     PROCEDURE :: search => table_search
     PROCEDURE, NOPASS :: scan => table_scan_rows
     PROCEDURE :: read => table_read_rows
     PROCEDURE :: interp => table_interp
     PROCEDURE :: destroy => table_destroy
  END type table

  INTERFACE table
     MODULE PROCEDURE table_new
     MODULE PROCEDURE table_new_from_file
  END INTERFACE table

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION table_new
  ! ----------------------------------------------------------------
  FUNCTION table_new(length, fields, lmode) RESULT(tbl)

    IMPLICIT NONE
    TYPE (table) :: tbl
    INTEGER, INTENT(IN) :: length, fields
    INTEGER, INTENT(IN), OPTIONAL :: lmode

    tbl%rows = length
    tbl%fields = fields
    IF (PRESENT(lmode)) THEN
       tbl%limit_mode = lmode
    ELSE
       tbl%limit_mode = TBL_LIMIT_NONE
    END IF

    ALLOCATE(tbl%x(tbl%rows))
    ALLOCATE(tbl%current(tbl%fields))
    ALLOCATE(tbl%data(tbl%rows, tbl%fields))
    
  END FUNCTION table_new

  ! ----------------------------------------------------------------
  !  FUNCTION table_new_from_file
  ! ----------------------------------------------------------------
  FUNCTION table_new_from_file(fname, fields, lmode) RESULT(tbl)

    IMPLICIT NONE
    TYPE (table) :: tbl
    CHARACTER (LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: fields
    INTEGER, INTENT(IN), OPTIONAL :: lmode

    INTEGER :: nrows, iunit, mylmode
    CHARACTER (LEN=1024) :: msg

    IF (PRESENT(lmode)) THEN
       mylmode = lmode
    ELSE
       mylmode = TBL_LIMIT_NONE
    END IF

    CALL open_existing(fname, iunit)
    nrows = tbl%scan(iunit, fields)
    IF (nrows .LE. 0) THEN
       CLOSE(iunit)
       WRITE(msg, *) 'Error reading table from ', TRIM(fname)
       CALL error_message(msg, fatal=.TRUE.)
    END IF
    
    tbl = table(nrows, fields, mylmode)

    REWIND(iunit)

    CALL tbl%read(iunit)

    CLOSE(iunit)

  END FUNCTION table_new_from_file


  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION table_scan_rows
  ! ----------------------------------------------------------------
  FUNCTION table_scan_rows(iunit, fields) RESULT(nrow)

    IMPLICIT NONE
    INTEGER :: nrow
    INTEGER, INTENT(IN) :: iunit, fields

    DOUBLE PRECISION :: x, fld(fields)
    CHARACTER (LEN=1024) :: msg
    INTEGER :: iostat

    nrow = 0

    READ(iunit, *)              ! skip the first line

    DO
       IF (fields .EQ. 1) THEN
          READ(iunit, *, IOSTAT=iostat) x, fld(1)
       ELSE 
          READ(iunit, *, IOSTAT=iostat) x, fld
       END IF
       IF (IS_IOSTAT_END(iostat)) THEN
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) 'Table read error at row ', nrow
          CALL error_message(msg)
          nrow = 0
          EXIT
       END IF
       nrow = nrow + 1
    END DO

  END FUNCTION table_scan_rows

  ! ----------------------------------------------------------------
  ! SUBROUTINE table_read_rows
  ! ----------------------------------------------------------------
  SUBROUTINE table_read_rows(this, iunit)

    IMPLICIT NONE
    CLASS(table), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit

    INTEGER :: i, iostat
    DOUBLE PRECISION :: x
    CHARACTER (LEN=1024) :: msg

    READ(iunit, *)              ! skip the first line

    DO i = 1, this%rows

       IF (this%fields .EQ. 1) THEN
          READ(iunit, *, IOSTAT=iostat) x, this%current(1)
       ELSE 
          READ(iunit, *, IOSTAT=iostat) x, this%current
       END IF

       IF (IS_IOSTAT_END(iostat)) THEN
          WRITE(msg, *) 'Premature end of table file at row ', i
          CALL error_message(msg, fatal=.TRUE.)
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) 'Table read error at row ', i
          CALL error_message(msg, fatal=.TRUE.)
       END IF

       this%x(i) = x
       this%data(i,:) = this%current(:)
    END DO
  END SUBROUTINE table_read_rows


  ! ----------------------------------------------------------------
  ! FUNCTION table_search
  !
  ! Your basic binary search. Assumes the row ordinates are sorted
  ! ascending. Looks for x between two ordinates. Returns table
  ! ordinate less than or equal to x.
  ! ----------------------------------------------------------------
  FUNCTION table_search(this, x) RESULT(idx)

    IMPLICIT NONE
    INTEGER :: idx
    CLASS (table), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: x

    INTEGER :: idx_min, idx_max
    LOGICAL :: found

    found = .FALSE.

    IF (x .LT. this%x(1)) THEN
       idx = 0
       RETURN
    ELSE IF (x .GT. this%x(this%rows)) THEN
       idx = this%rows
       RETURN
    END IF
    
    
    idx_min = 1
    idx_max = this%rows

    DO
       idx = (idx_max + idx_min)/2
       IF (idx_max - idx_min .LE. 1 ) THEN
          idx = idx_min
          EXIT
       ELSE IF (this%x(idx) .EQ. x) THEN
          EXIT
       ELSE IF (x .LT. this%x(idx)) THEN
          IF (x .GE. this%x(idx-1)) THEN
             idx = idx - 1
             EXIT
          END IF
          idx_max = idx
       ELSE
          IF (x .LT. this%x(idx+1)) THEN
             EXIT
          END IF
          idx_min = idx
       END IF
    END DO

  END FUNCTION table_search


  ! ----------------------------------------------------------------
  ! SUBROUTINE table_interp
  ! ----------------------------------------------------------------
  SUBROUTINE table_interp(this, x)

    IMPLICIT NONE

    CLASS (table), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: x

    INTEGER :: i
    DOUBLE PRECISION :: factor
    CHARACTER (LEN=1024) :: buf

    i = this%search(x)

    IF ((1 .LE. i .AND. i .LT. this%rows) .OR. &
         & this%limit_mode .EQ. TBL_LIMIT_EXTRAP) THEN
       i = MAX(i, 1)
       i = MIN(i, this%rows - 1)

       factor = (x - this%x(i))/(this%x(i+1) - this%x(i))
       
       this%current = &
            &(this%data(i+1,:) - this%data(i,:))*factor + this%data(i,:)

    ELSE
       SELECT CASE (this%limit_mode)
       CASE (TBL_LIMIT_NONE)
          WRITE(buf, *) 'Table ordinate ', x, ' out of range'
          CALL error_message(buf, fatal=.TRUE.)
       CASE (TBL_LIMIT_FLAT)
          IF (i .LT. 1) THEN
             this%current = this%data(1, :)
          ELSE IF (i .GE. this%rows) THEN
             this%current = this%data(this%rows, :)
          ELSE
             CALL error_message("This should not happen", fatal=.TRUE.)
          END IF
       CASE DEFAULT
          CALL error_message("This should not happen", fatal=.TRUE.)
       END SELECT
    END IF
  END SUBROUTINE table_interp


  ! ----------------------------------------------------------------
  ! SUBROUTINE table_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE table_destroy(this)

    IMPLICIT NONE
    CLASS (table), INTENT(INOUT) :: this

    DEALLOCATE(this%x, this%current, this%data)
    this%rows = 0
    this%fields = 0

  END SUBROUTINE table_destroy


END MODULE table_module
