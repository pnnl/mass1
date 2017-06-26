! ----------------------------------------------------------------
! file: link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March  8, 2017 by William A. Perkins
! Last Change: 2017-06-26 13:44:11 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE link_module
! ----------------------------------------------------------------
MODULE link_module

  USE dlist_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE link_t
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_t
     INTEGER :: id
   CONTAINS
     PROCEDURE :: destroy => link_destroy
  END type link_t

  ! ----------------------------------------------------------------
  ! TYPE link_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_ptr
     CLASS (link_t), POINTER :: p
  END type link_ptr

  ! ----------------------------------------------------------------
  ! link_list
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(dlist) :: link_list
   CONTAINS
     PROCEDURE :: push => link_list_push
     PROCEDURE :: pop => link_list_pop
     PROCEDURE :: clear => link_list_clear
     PROCEDURE :: find => link_list_find
  END type link_list

  INTERFACE link_list
     MODULE PROCEDURE new_link_list
  END INTERFACE link_list

  ! ----------------------------------------------------------------
  ! link_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_manager_t
     TYPE (link_list) :: links
   CONTAINS
     PROCEDURE :: find => link_manager_find
     PROCEDURE :: destroy => link_manager_destroy
  END type link_manager_t

  INTERFACE link_manager_t
     MODULE PROCEDURE new_link_manager
  END INTERFACE link_manager_t

  PUBLIC :: new_link_manager
  
CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE link_destroy(this)
    IMPLICIT NONE
    CLASS (link_t), INTENT(INOUT) :: this
    

  END SUBROUTINE link_destroy

  ! ----------------------------------------------------------------
  !  FUNCTION new_link_list
  ! ----------------------------------------------------------------
  FUNCTION new_link_list()
    IMPLICIT NONE
    TYPE (link_list) :: new_link_list
    NULLIFY(new_link_list%head)
    NULLIFY(new_link_list%tail)
  END FUNCTION new_link_list

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE link_list_push(this, alink)
    IMPLICIT NONE
    CLASS (link_list), INTENT(INOUT) :: this
    CLASS (link_t), POINTER, INTENT(IN) :: alink
    TYPE (link_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    ALLOCATE(ptr)
    ptr%p => alink
    p => ptr
    CALL this%genpush(p)
  END SUBROUTINE link_list_push

  ! ----------------------------------------------------------------
  !  FUNCTION link_list_pop
  ! ----------------------------------------------------------------
  FUNCTION link_list_pop(this) RESULT(link)
    IMPLICIT NONE
    CLASS (link_list), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link
    TYPE (link_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(link)
    p => this%genpop()

    IF (ASSOCIATED(p)) THEN
       SELECT TYPE (p)
       TYPE IS (link_ptr)
          ptr => p
          link => ptr%p
          DEALLOCATE(ptr)
       END SELECT
    END IF
    RETURN
  END FUNCTION link_list_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_list_clear
  ! ----------------------------------------------------------------
  SUBROUTINE link_list_clear(this)
    IMPLICIT NONE
    CLASS (link_list), INTENT(INOUT) :: this
    CLASS (link_t), POINTER :: link

    DO WHILE (.TRUE.)
       link => this%pop()
       IF (ASSOCIATED(link)) THEN
          CALL link%destroy()
          DEALLOCATE(link)
       ELSE 
          EXIT
       END IF
    END DO
  END SUBROUTINE link_list_clear

  ! ----------------------------------------------------------------
  !  FUNCTION link_list_find
  ! ----------------------------------------------------------------
  FUNCTION link_list_find(this, linkid) RESULT(link)
    IMPLICIT NONE
    CLASS (link_t), POINTER :: link
    CLASS (link_list) :: this
    INTEGER, INTENT(IN) :: linkid

    TYPE (dlist_node), POINTER :: node
    TYPE (link_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(link)
    
    node => this%head
    DO WHILE (ASSOCIATED(node)) 
       p => node%data
       IF (ASSOCIATED(p)) THEN
          SELECT TYPE (p)
          TYPE IS (link_ptr)
             ptr => p
             link => ptr%p
             IF (link%id .EQ. linkid) THEN
                EXIT
             END IF
          END SELECT
       END IF
       node => node%next
    END DO
  END FUNCTION link_list_find


  ! ----------------------------------------------------------------
  !  FUNCTION new_link_manager
  ! ----------------------------------------------------------------
  FUNCTION new_link_manager() RESULT (man)
    IMPLICIT NONE
    TYPE (link_manager_t) :: man
    man%links = new_link_list()
  END FUNCTION new_link_manager

  ! ----------------------------------------------------------------
  !  FUNCTION link_manager_find
  ! ----------------------------------------------------------------
  FUNCTION link_manager_find(this, linkid) RESULT(link)
    IMPLICIT NONE
    CLASS (link_t), POINTER :: link
    CLASS (link_manager_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: linkid
    link => this%links%find(linkid)
  END FUNCTION link_manager_find

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_manager_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE link_manager_destroy(this)
    IMPLICIT NONE
    CLASS (link_manager_t), INTENT(INOUT) :: this
    CALL this%links%clear()
  END SUBROUTINE link_manager_destroy


END MODULE link_module
