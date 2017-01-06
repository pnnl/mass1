! ----------------------------------------------------------------
! file: cross_section_handler.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  4, 2017 by William A. Perkins
! Last Change: 2017-01-05 15:57:09 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE section_handler_module
! ----------------------------------------------------------------
MODULE section_handler_module
  USE utility
  USE cross_section

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE section_list_node
  ! ----------------------------------------------------------------
  TYPE :: section_list_node
     CLASS (xsection_t), POINTER :: section
     TYPE (section_list_node), POINTER :: prev
     TYPE (section_list_node), POINTER :: next
  END type section_list_node

  INTERFACE section_list_node
     MODULE PROCEDURE new_section_list_node
  END INTERFACE section_list_node

  ! ----------------------------------------------------------------
  ! TYPE section_list
  ! ----------------------------------------------------------------
  TYPE :: section_list
     TYPE (section_list_node), POINTER :: head
     TYPE (section_list_node), POINTER :: tail
   CONTAINS 
     PROCEDURE :: size => section_list_size
     PROCEDURE :: push => section_list_push
     PROCEDURE :: pop => section_list_pop
     PROCEDURE :: clear => section_list_clear
     PROCEDURE :: find => section_list_find
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

  TYPE (section_handler), PUBLIC :: sections

CONTAINS
  ! ----------------------------------------------------------------
  !  FUNCTION new_section_list_node
  ! ----------------------------------------------------------------
  FUNCTION new_section_list_node()
    IMPLICIT NONE
    TYPE (section_list_node) :: new_section_list_node
    NULLIFY(new_section_list_node%prev)
    NULLIFY(new_section_list_node%next)
  END FUNCTION new_section_list_node

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION section_list_size
  ! ----------------------------------------------------------------
  INTEGER FUNCTION section_list_size(this)
    IMPLICIT NONE
    CLASS (section_list), INTENT(IN) :: this
    TYPE (section_list_node), POINTER :: node

    section_list_size = 0
    node => this%head
    DO WHILE (ASSOCIATED(node)) 
       section_list_size = section_list_size + 1
       node => node%next
    END DO
  END FUNCTION section_list_size


  ! ----------------------------------------------------------------
  ! SUBROUTINE section_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE section_list_push(this, xsect)
    IMPLICIT NONE
    CLASS (section_list), INTENT(INOUT) :: this
    CLASS (xsection_t), POINTER, INTENT(IN) :: xsect
    TYPE (section_list_node), POINTER :: node
    ALLOCATE(node)
    NULLIFY(node%prev)
    NULLIFY(node%next)
    node%section => xsect

    IF (.NOT. ASSOCIATED(this%head)) THEN
       this%head => node
       this%tail => node
    ELSE
       node%prev => this%tail
       this%tail%next => node
       this%tail => node
    END IF
  END SUBROUTINE section_list_push

  ! ----------------------------------------------------------------
  !  FUNCTION section_list_pop
  ! ----------------------------------------------------------------
  FUNCTION section_list_pop(this) RESULT(xsect)
    IMPLICIT NONE
    CLASS (xsection_t), POINTER :: xsect
    CLASS (section_list), INTENT(INOUT) :: this

    TYPE (section_list_node), POINTER :: node

    NULLIFY(xsect)
    IF (ASSOCIATED(this%tail)) THEN
       node => this%tail
       xsect => node%section
       IF (.NOT. ASSOCIATED(node%prev)) THEN
          NULLIFY(this%tail)
          NULLIFY(this%head)
       ELSE 
          this%tail => node%prev
       END IF
       DEALLOCATE(node)
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

    DO WHILE (ASSOCIATED(this%tail))
       xsect => this%pop()
       CALL xsect%destroy()
       DEALLOCATE(xsect)
    END DO
  END SUBROUTINE section_list_clear


  ! ----------------------------------------------------------------
  !  FUNCTION section_list_find
  ! ----------------------------------------------------------------
  FUNCTION section_list_find(this, id) RESULT(xsect)
    IMPLICIT NONE
    CLASS (xsection_t), POINTER :: xsect
    CLASS (section_list), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: id
    TYPE (section_list_node), POINTER :: node
    
    NULLIFY(xsect)
    node => this%head
    DO WHILE (ASSOCIATED(node)) 
       IF (node%section%id .EQ. id) THEN
          xsect => node%section
          EXIT
       END IF
       node => node%next
    END DO
    RETURN
  END FUNCTION section_list_find

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
    CLASS (section_handler), INTENT(IN) :: this
    CHARACTER(LEN=*), INTENT(IN) :: fname
    CLASS (xsection_t), POINTER :: x
    CHARACTER(LEN=256) :: msg
    TYPE (section_list_node), POINTER :: node
    INTEGER, PARAMETER :: iounit = 24
    INTEGER :: count, ioerr

    count = 0
    ioerr = 0

    WRITE(msg, *) "Writing ", this%xslist%size(), " sections to ", TRIM(fname)
    CALL status_message(msg)
    CALL open_new(fname, iounit)

    node => this%xslist%head
    DO WHILE (ASSOCIATED(node)) 
       x => node%section
       CALL x%print(iounit, ioerr)
       count = count + 1
       node => node%next
    END DO
    CLOSE(iounit)

  END SUBROUTINE section_handler_write_geometry

  ! ----------------------------------------------------------------
  !  FUNCTION section_handler_find
  ! ----------------------------------------------------------------
  FUNCTION section_handler_find(this, id) RESULT(xsect)
    IMPLICIT NONE
    CLASS (xsection_t), POINTER :: xsect
    CLASS (section_handler), INTENT(IN) :: this
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
  SUBROUTINE section_handler_props(this, sectid, &
       &depth, area, hydrad, topwidth, conveyance, dkdy)
    IMPLICIT NONE
    CLASS (section_handler), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: sectid
    DOUBLE PRECISION, INTENT(IN) :: depth
    DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy
    CLASS (xsection_t), POINTER :: xsect
    CHARACTER(LEN=256) :: msg
    
    xsect => this%find(sectid)
    IF (ASSOCIATED(xsect)) THEN
       CALL xsect%props(depth, area, hydrad, topwidth, conveyance, dkdy)
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

