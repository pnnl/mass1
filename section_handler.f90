! ----------------------------------------------------------------
! file: cross_section_handler.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  4, 2017 by William A. Perkins
! Last Change: 2017-01-04 15:54:50 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE section_handler_module
! ----------------------------------------------------------------
MODULE section_handler_module
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

  TYPE section_list
     TYPE (section_list_node), POINTER :: head
     TYPE (section_list_node), POINTER :: tail
   CONTAINS 
     PROCEDURE :: push => section_list_push
     PROCEDURE :: pop => section_list_pop
     PROCEDURE :: clear => section_list_clear
  END type section_list


  ! ----------------------------------------------------------------
  ! TYPE section_handler
  ! ----------------------------------------------------------------
  ! TYPE, PUBLIC :: section_handler
  !    CLASS (section_list) :: slist
  !  CONTAINS
  !    PROCEDURE :: read => section_read
  !    PROCEDURE :: find => section_find
  !    PROCEDURE :: destroy => section_destroy
  ! END type section_handler


CONTAINS

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
  FUNCTION section_list_pop(this)
    IMPLICIT NONE
    CLASS (xsection_t), POINTER :: section_list_pop
    CLASS (section_list), INTENT(INOUT) :: this

    TYPE (section_list_node), POINTER :: node

    NULLIFY(section_list_pop)
    IF (ASSOCIATED(this%tail)) THEN
       node => this%tail
       section_list_pop => node%section
       DEALLOCATE(node)
       IF (.NOT. ASSOCIATED(node%prev)) THEN
          NULLIFY(this%tail)
          NULLIFY(this%head)
       ELSE 
          this%tail => node%prev
       END IF
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
       DEALLOCATE(xsect)
    END DO
  END SUBROUTINE section_list_clear

  

END MODULE section_handler_module

