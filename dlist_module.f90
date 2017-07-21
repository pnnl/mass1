! ----------------------------------------------------------------
! file: dlist.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 21, 2017 by William A. Perkins
! Last Change: 2017-07-21 13:12:14 d3g096
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
     TYPE (dlist_node), POINTER :: cursor
     TYPE (dlist_node), POINTER :: saved_cursor
   CONTAINS 
     PROCEDURE, NON_OVERRIDABLE :: size => dlist_size
     PROCEDURE, NON_OVERRIDABLE :: begin => dlist_begin
     PROCEDURE, NON_OVERRIDABLE :: next => dlist_next
     PROCEDURE, NON_OVERRIDABLE :: genpush => dlist_push
     PROCEDURE, NON_OVERRIDABLE :: genpop => dlist_pop
     PROCEDURE, NON_OVERRIDABLE :: gencurrent => dlist_current
     PROCEDURE, NON_OVERRIDABLE :: save => dlist_save
     PROCEDURE, NON_OVERRIDABLE :: restore => dlist_restore
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
  ! SUBROUTINE dlist_begin
  ! ----------------------------------------------------------------
  SUBROUTINE dlist_begin(this)

    IMPLICIT NONE
    CLASS (dlist), INTENT(INOUT) :: this

    this%cursor => this%head
  END SUBROUTINE dlist_begin

  ! ----------------------------------------------------------------
  ! SUBROUTINE dlist_next
  ! ----------------------------------------------------------------
  SUBROUTINE dlist_next(this)
    IMPLICIT NONE
    CLASS (dlist), INTENT(INOUT) :: this
    
    IF (ASSOCIATED(this%cursor)) THEN
       this%cursor => this%cursor%next
    END IF
  END SUBROUTINE dlist_next

  ! ----------------------------------------------------------------
  !  FUNCTION dlist_current
  ! ----------------------------------------------------------------
  FUNCTION dlist_current(this) RESULT(data)
    IMPLICIT NONE
    CLASS (*), POINTER :: data
    CLASS (dlist), INTENT(INOUT) :: this
    
    NULLIFY(data)
    IF (ASSOCIATED(this%cursor)) THEN
       data => this%cursor%data
    END IF
  END FUNCTION dlist_current

  ! ----------------------------------------------------------------
  ! SUBROUTINE dlist_save
  ! ----------------------------------------------------------------
  SUBROUTINE dlist_save(this)
    IMPLICIT NONE
    CLASS (dlist), INTENT(INOUT) :: this

    this%saved_cursor => this%cursor

  END SUBROUTINE dlist_save

  ! ----------------------------------------------------------------
  ! SUBROUTINE dlist_restore
  ! ----------------------------------------------------------------
  SUBROUTINE dlist_restore(this)

    IMPLICIT NONE

    CLASS (dlist), INTENT(INOUT) :: this
    
    this%cursor => this%saved_cursor
    NULLIFY(this%saved_cursor)

  END SUBROUTINE dlist_restore

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

