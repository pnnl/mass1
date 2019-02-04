! ----------------------------------------------------------------
! file: utility.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 21, 2002 by William A. Perkins
! Last Change: 2017-08-21 12:19:08 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE utility
! ----------------------------------------------------------------
MODULE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PUBLIC :: utility_error_iounit=13, utility_status_iounit=14

CONTAINS 

  ! ----------------------------------------------------------------
  ! SUBROUTINE error_message
  ! ----------------------------------------------------------------
  SUBROUTINE error_message(msg, fatal)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    CHARACTER (LEN=1024) :: buffer

    LOGICAL :: die = .FALSE.

    IF (PRESENT(fatal)) die = fatal

    IF (die) THEN
       buffer = "FATAL ERROR: " // msg
    ELSE 
       buffer = "ERROR: " // msg
    END IF

    WRITE(utility_error_iounit, *) TRIM(buffer)
    WRITE(*,*) TRIM(buffer)

    IF (die) CALL EXIT(1)
  END SUBROUTINE error_message

  ! ----------------------------------------------------------------
  ! SUBROUTINE status_message
  ! ----------------------------------------------------------------
  SUBROUTINE status_message(msg)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg

    WRITE(utility_status_iounit, *) TRIM(msg)

  END SUBROUTINE status_message



  ! ----------------------------------------------------------------
  ! SUBROUTINE open_existing
  ! ----------------------------------------------------------------
  SUBROUTINE open_existing(fname, iunit, fatal, form, result)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: iunit
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    CHARACTER (LEN=*), OPTIONAL :: form
    LOGICAL, INTENT(OUT), OPTIONAL :: result

    LOGICAL :: file_exist, myfatal = .TRUE., myresult = .FALSE.
    CHARACTER (LEN=1024) :: myform
    INTEGER :: status

    CHARACTER (LEN=1024) :: msg

    myform='FORMATTED'
    file_exist = .TRUE.

    IF (PRESENT(fatal)) myfatal = fatal
    IF (PRESENT(form)) myform = form

    INQUIRE(FILE=TRIM(fname), EXIST=file_exist)
    IF(file_exist)THEN
       OPEN(iunit, file=fname, action='READ', form=myform, iostat=status)
       IF (status .EQ. 0) THEN
          CALL status_message('Opened ' // TRIM(fname) // ' for reading')
          IF (PRESENT(result)) result = .TRUE.
          RETURN
       END IF
       WRITE(msg, *) TRIM(fname), ': cannot open for reading: ', status
       CALL error_message(msg, fatal=myfatal)
    ELSE
       WRITE(msg, *) TRIM(fname), ': cannot open for reading: file does not exist'
       CALL error_message(msg, fatal=myfatal)
    ENDIF
    IF (PRESENT(result)) result = .FALSE.
  END SUBROUTINE open_existing

  ! ----------------------------------------------------------------
  ! SUBROUTINE open_new
  ! ----------------------------------------------------------------
  SUBROUTINE open_new(fname, iunit)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: iunit

    INTEGER :: status

    OPEN(unit=iunit, file=fname, action='WRITE', iostat=status)
    IF (status .EQ. 0) THEN
       CALL status_message('Opened ' // TRIM(fname) // ' for writing')
       RETURN
    END IF
    CALL error_message(TRIM(fname) // ': cannot open for writing', fatal=.TRUE.)

  END SUBROUTINE open_new



END MODULE utility
