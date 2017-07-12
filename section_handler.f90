! ----------------------------------------------------------------
! file: cross_section_handler.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  4, 2017 by William A. Perkins
! Last Change: 2017-07-12 14:09:24 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE section_handler_module
! ----------------------------------------------------------------
MODULE section_handler_module
  USE utility
  USE cross_section
  USE dlist_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE section_list
  ! ----------------------------------------------------------------
  TYPE, EXTENDS(dlist) :: section_list
   CONTAINS 
     PROCEDURE :: push => section_list_push
     PROCEDURE :: pop => section_list_pop
     PROCEDURE :: clear => section_list_clear
     PROCEDURE :: find => section_list_find
     PROCEDURE :: current => section_list_current
  END type section_list

  INTERFACE section_list
     MODULE PROCEDURE new_section_list
  END INTERFACE section_list

  ! ----------------------------------------------------------------
  ! TYPE section_handler
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: section_handler
     TYPE (section_list) :: xslist
   CONTAINS
     PROCEDURE :: read => section_handler_read
     PROCEDURE :: write_geometry => section_handler_write_geometry
     PROCEDURE :: find => section_handler_find
     PROCEDURE :: size => section_handler_size
     PROCEDURE :: props => section_handler_props
     PROCEDURE :: destroy => section_handler_destroy
  END type section_handler

  INTERFACE section_handler
     MODULE PROCEDURE new_section_handler
  END INTERFACE section_handler

  PUBLIC new_section_handler

  TYPE (section_handler), PUBLIC :: sections

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE section_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE section_list_push(this, xsect)
    IMPLICIT NONE
    CLASS (section_list), INTENT(INOUT) :: this
    CLASS (xsection_t), POINTER, INTENT(IN) :: xsect
    TYPE (xsection_ptr), POINTER :: ptr
    CLASS (*), POINTER :: p
    ALLOCATE(ptr)
    ptr%p => xsect
    p => ptr
    CALL this%genpush(p)
  END SUBROUTINE section_list_push

  ! ----------------------------------------------------------------
  !  FUNCTION section_list_pop
  ! ----------------------------------------------------------------
  FUNCTION section_list_pop(this) RESULT(xsect)
    IMPLICIT NONE
    CLASS (xsection_t), POINTER :: xsect
    CLASS (section_list), INTENT(INOUT) :: this
    TYPE (xsection_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p
    
    NULLIFY(xsect)
    p => this%genpop()

    IF (ASSOCIATED(p)) THEN
       SELECT TYPE (p)
       TYPE IS (xsection_ptr)
          ptr => p
          xsect => ptr%p
          DEALLOCATE(ptr)
       END SELECT
    END IF
    RETURN
  END FUNCTION section_list_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE section_list_clear
  ! ----------------------------------------------------------------
  SUBROUTINE section_list_clear(this)
    IMPLICIT NONE
    CLASS (section_list), INTENT(INOUT) :: this
    CLASS (xsection_t), POINTER :: xsect

    DO WHILE (.TRUE.)
       xsect => this%pop()
       IF (ASSOCIATED(xsect)) THEN
          CALL xsect%destroy()
          DEALLOCATE(xsect)
       ELSE 
          EXIT
       END IF
    END DO
  END SUBROUTINE section_list_clear


  ! ----------------------------------------------------------------
  !  FUNCTION section_list_find
  ! ----------------------------------------------------------------
  FUNCTION section_list_find(this, id) RESULT(xsect)
    IMPLICIT NONE
    CLASS (xsection_t), POINTER :: xsect
    CLASS (section_list), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: id

    CALL this%begin()
    xsect => this%current()
    DO WHILE (ASSOCIATED(xsect)) 
       IF (xsect%id .EQ. id) THEN
          EXIT
       END IF
       CALL this%next()
       xsect => this%current()
    END DO
    RETURN
  END FUNCTION section_list_find

  ! ----------------------------------------------------------------
  !  FUNCTION section_list_current
  ! ----------------------------------------------------------------
  FUNCTION section_list_current(this) RESULT(xsect)

    IMPLICIT NONE

    CLASS (xsection_t), POINTER :: xsect
    CLASS (section_list), INTENT(IN) :: this
    TYPE (xsection_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(xsect)
    IF (ASSOCIATED(this%cursor)) THEN
       p => this%cursor%data
       IF (ASSOCIATED(p)) THEN
          SELECT TYPE (p)
          TYPE IS (xsection_ptr)
             ptr => p
             xsect => ptr%p
          END SELECT
       END IF
    END IF
  END FUNCTION section_list_current


  ! ----------------------------------------------------------------
  !  FUNCTION new_section_list
  ! ----------------------------------------------------------------
  FUNCTION new_section_list()
    IMPLICIT NONE
    TYPE (section_list) :: new_section_list
    NULLIFY(new_section_list%head)
    NULLIFY(new_section_list%tail)
  END FUNCTION new_section_list

  ! ----------------------------------------------------------------
  ! SUBROUTINE section_handler_read
  ! ----------------------------------------------------------------
  SUBROUTINE section_handler_read(this, fname)
    IMPLICIT NONE
    CLASS (section_handler), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: fname
    CLASS (xsection_t), POINTER :: x
    INTEGER, PARAMETER :: iounit = 24
    CHARACTER(LEN=256) :: msg
    INTEGER :: ierr
    
    CALL status_message(TRIM(fname) // ': Reading sections ...')
    CALL open_existing(fname, iounit, fatal=.TRUE.)

    DO WHILE (.TRUE.)
       x => read_cross_section(iounit, ierr)
       IF (.NOT. ASSOCIATED(x)) EXIT
       CALL this%xslist%push(x)
    END DO
    CLOSE(iounit)

    IF (ierr .NE. 0) THEN
       WRITE (msg, *) TRIM(fname) // ': error occured while reading sections'
       CALL error_message(msg, fatal=.TRUE.)
    ELSE 
       WRITE (msg, *) TRIM(fname) // ': successully read ', this%xslist%size(), ' sections'
       CALL status_message(msg)
    END IF

  END SUBROUTINE section_handler_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE section_handler_write_geometry
  ! ----------------------------------------------------------------
  SUBROUTINE section_handler_write_geometry(this, fname)
    IMPLICIT NONE
    CLASS (section_handler), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: fname
    CLASS (xsection_t), POINTER :: sect
    CHARACTER(LEN=256) :: msg
    INTEGER, PARAMETER :: iounit = 24
    INTEGER :: count, ioerr

    count = 0
    ioerr = 0

    WRITE(msg, *) "Writing ", this%xslist%size(), " sections to ", TRIM(fname)
    CALL status_message(msg)
    CALL open_new(fname, iounit)

    CALL this%xslist%begin()
    sect => this%xslist%current()
    DO WHILE (ASSOCIATED(sect)) 
       CALL sect%print(iounit, ioerr)
       CALL this%xslist%next()
       sect => this%xslist%current()
    END DO

    CLOSE(iounit)

  END SUBROUTINE section_handler_write_geometry

  ! ----------------------------------------------------------------
  !  FUNCTION section_handler_find
  ! ----------------------------------------------------------------
  FUNCTION section_handler_find(this, id) RESULT(xsect)
    IMPLICIT NONE
    CLASS (xsection_t), POINTER :: xsect
    CLASS (section_handler), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: id
    CHARACTER(LEN=256) :: msg
    
    xsect => this%xslist%find(id)
    IF (.NOT. ASSOCIATED(xsect)) THEN
       WRITE(msg, *) 'Could not find cross section with ID = ', id
       CALL error_message(msg)
    END IF
    RETURN
  END FUNCTION section_handler_find

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION section_handler_size
  ! ----------------------------------------------------------------
  INTEGER FUNCTION section_handler_size(this)
    IMPLICIT NONE
    CLASS (section_handler), INTENT(IN) :: this
    section_handler_size = this%xslist%size()
  END FUNCTION section_handler_size

  ! ----------------------------------------------------------------
  ! SUBROUTINE section_handler_props
  ! ----------------------------------------------------------------
  SUBROUTINE section_handler_props(this, sectid, depth, props)
    IMPLICIT NONE
    CLASS (section_handler), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: sectid
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(OUT) :: props
    CLASS (xsection_t), POINTER :: xsect
    CHARACTER(LEN=256) :: msg
    
    xsect => this%find(sectid)
    IF (ASSOCIATED(xsect)) THEN
       CALL xsect%props(depth, props)
    ELSE 
       WRITE(msg, *) 'Section ', sectid, ' not found'
       CALL error_message(msg)
    END IF
  END SUBROUTINE section_handler_props



  ! ----------------------------------------------------------------
  ! SUBROUTINE section_handler_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE section_handler_destroy(this)
    IMPLICIT NONE
    CLASS (section_handler), INTENT(INOUT) :: this
    CALL this%xslist%clear()
  END SUBROUTINE section_handler_destroy

  ! ----------------------------------------------------------------
  !  FUNCTION new_section_handler
  ! ----------------------------------------------------------------
  FUNCTION new_section_handler()
    IMPLICIT NONE
    TYPE (section_handler) :: new_section_handler
    new_section_handler%xslist = section_list()
  END FUNCTION new_section_handler


END MODULE section_handler_module

