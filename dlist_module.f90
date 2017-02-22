! ----------------------------------------------------------------
! file: dlist.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 21, 2017 by William A. Perkins
! Last Change: 2017-02-22 08:18:04 d3g096
! ----------------------------------------------------------------
MODULE dlist_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE dlist_node
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: dlist_node
     CLASS (*), POINTER :: data
     TYPE (dlist_node), POINTER :: prev
     TYPE (dlist_node), POINTER :: next
  END type dlist_node

  INTERFACE dlist_node
     MODULE PROCEDURE new_dlist_node
  END INTERFACE dlist_node

  ! ----------------------------------------------------------------
  ! TYPE dlist
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: dlist
     TYPE (dlist_node), POINTER :: head
     TYPE (dlist_node), POINTER :: tail
   CONTAINS 
     PROCEDURE, NON_OVERRIDABLE :: size => dlist_size
     PROCEDURE, NON_OVERRIDABLE :: genpush => dlist_push
     PROCEDURE, NON_OVERRIDABLE :: genpop => dlist_pop
     PROCEDURE :: clear => dlist_clear
  END type dlist

  INTERFACE dlist
     MODULE PROCEDURE new_dlist
  END INTERFACE dlist

CONTAINS
  ! ----------------------------------------------------------------
  !  FUNCTION new_dlist_node
  ! ----------------------------------------------------------------
  FUNCTION new_dlist_node()
    IMPLICIT NONE
    TYPE (dlist_node) :: new_dlist_node
    NULLIFY(new_dlist_node%prev)
    NULLIFY(new_dlist_node%next)
  END FUNCTION new_dlist_node

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION dlist_size
  ! ----------------------------------------------------------------
  INTEGER FUNCTION dlist_size(this)
    IMPLICIT NONE
    CLASS (dlist), INTENT(IN) :: this
    TYPE (dlist_node), POINTER :: node

    dlist_size = 0
    node => this%head
    DO WHILE (ASSOCIATED(node)) 
       dlist_size = dlist_size + 1
       node => node%next
    END DO
  END FUNCTION dlist_size


  ! ----------------------------------------------------------------
  ! SUBROUTINE dlist_push
  ! ----------------------------------------------------------------
  SUBROUTINE dlist_push(this, p)
    IMPLICIT NONE
    CLASS (dlist), INTENT(INOUT) :: this
    CLASS (*), POINTER, INTENT(IN) :: p
    TYPE (dlist_node), POINTER :: node
    ALLOCATE(node)
    NULLIFY(node%prev)
    NULLIFY(node%next)
    node%data => p

    IF (.NOT. ASSOCIATED(this%head)) THEN
       this%head => node
       this%tail => node
    ELSE
       node%prev => this%tail
       this%tail%next => node
       this%tail => node
    END IF
  END SUBROUTINE dlist_push

  ! ----------------------------------------------------------------
  !  FUNCTION dlist_pop
  ! ----------------------------------------------------------------
  FUNCTION dlist_pop(this) RESULT(data)
    IMPLICIT NONE
    CLASS (*), POINTER :: data
    CLASS (dlist), INTENT(INOUT) :: this

    TYPE (dlist_node), POINTER :: node

    NULLIFY(data)
    IF (ASSOCIATED(this%tail)) THEN
       node => this%tail
       data => node%data
       IF (.NOT. ASSOCIATED(node%prev)) THEN
          NULLIFY(this%tail)
          NULLIFY(this%head)
       ELSE 
          this%tail => node%prev
       END IF
       DEALLOCATE(node)
    END IF
    RETURN
  END FUNCTION dlist_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE dlist_clear
  ! ----------------------------------------------------------------
  SUBROUTINE dlist_clear(this)
    IMPLICIT NONE
    CLASS (dlist), INTENT(INOUT) :: this
    CLASS (*), POINTER :: data

    DO WHILE (ASSOCIATED(this%tail))
       data => this%genpop()
    END DO
  END SUBROUTINE dlist_clear


  ! ----------------------------------------------------------------
  !  FUNCTION new_dlist
  ! ----------------------------------------------------------------
  FUNCTION new_dlist()
    IMPLICIT NONE
    TYPE (dlist) :: new_dlist
    NULLIFY(new_dlist%head)
    NULLIFY(new_dlist%tail)
  END FUNCTION new_dlist



END MODULE dlist_module

