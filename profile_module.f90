  ! ----------------------------------------------------------------
  ! file: profile_module.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created January 10, 2018 by William A. Perkins
  ! Last Change: 2018-01-10 14:44:36 d3g096
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE profile_module
! ----------------------------------------------------------------
MODULE profile_module

  USE utility
  USE date_time
  USE dlist_module
  USE point_module
  USE link_manager_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE profile_point
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: profile_pt
     INTEGER :: link_id, point_idx
     DOUBLE PRECISION :: profx
     CLASS (point_t), POINTER :: pt
   CONTAINS
     PROCEDURE :: destroy => profile_pt_destroy
  END type profile_pt

  ! ----------------------------------------------------------------
  ! TYPE profile_pt_ptr
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: profile_pt_ptr
     CLASS (profile_pt), POINTER :: p
  END type profile_pt_ptr

  ! ----------------------------------------------------------------
  ! TYPE profile_pt_list
  ! ----------------------------------------------------------------
  TYPE, PRIVATE, EXTENDS(dlist) :: profile_pt_list
   CONTAINS
     PROCEDURE :: push => profile_pt_list_push
     PROCEDURE :: pop => profile_pt_list_pop
     PROCEDURE :: clear => profile_pt_list_clear
     PROCEDURE :: current => profile_pt_list_current
  END type profile_pt_list
  
  ! ----------------------------------------------------------------
  ! TYPE profile
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: profile_t
     INTEGER :: id
     TYPE (profile_pt_list) :: pts
     CHARACTER (LEN=1024), PRIVATE :: filename
   CONTAINS
     PROCEDURE :: fill => profile_fill
     PROCEDURE :: output => profile_output
     PROCEDURE :: destroy => profile_destroy
  END type profile_t

  ! ----------------------------------------------------------------
  ! TYPE profile_ptr
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: profile_ptr
     CLASS (profile_t), POINTER :: p
  END type profile_ptr

  ! ----------------------------------------------------------------
  ! TYPE profile_list
  ! ----------------------------------------------------------------
  TYPE, PRIVATE, EXTENDS(dlist) :: profile_list
   CONTAINS
     PROCEDURE :: push => profile_list_push
     PROCEDURE :: pop => profile_list_pop
     PROCEDURE :: clear => profile_list_clear
     PROCEDURE :: current => profile_list_current
  END type profile_list

  ! ----------------------------------------------------------------
  ! TYPE profile_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: profile_manager
     TYPE (profile_list) :: profs
   CONTAINS
     PROCEDURE :: read => profile_manager_read
     PROCEDURE :: output => profile_manager_output
     PROCEDURE :: destroy => profile_manager_destroy
  END type profile_manager
  

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_pt_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE profile_pt_destroy(this)

    IMPLICIT NONE
    CLASS (profile_pt) :: this
    NULLIFY(this%pt)

  END SUBROUTINE profile_pt_destroy


  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_pt_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE profile_pt_list_push(this, pt)
    IMPLICIT NONE
    CLASS (profile_pt_list), INTENT(INOUT) :: this
    CLASS (profile_pt), POINTER, INTENT(IN) :: pt
    TYPE (profile_pt_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    ALLOCATE(ptr)
    ptr%p => pt
    p => ptr
    CALL this%genpush(p)
  END SUBROUTINE profile_pt_list_push

  ! ----------------------------------------------------------------
  !  FUNCTION profile_pt_list_pop
  ! ----------------------------------------------------------------
  FUNCTION profile_pt_list_pop(this) RESULT(pt)
    IMPLICIT NONE
    CLASS (profile_pt_list), INTENT(INOUT) :: this
    CLASS (profile_pt), POINTER :: pt
    TYPE (profile_pt_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(pt)
    p => this%genpop()

    IF (ASSOCIATED(p)) THEN
       SELECT TYPE (p)
       TYPE IS (profile_pt_ptr)
          ptr => p
          pt => ptr%p
          CALL pt%destroy()
          DEALLOCATE(ptr)
       END SELECT
    END IF
    RETURN
  END FUNCTION profile_pt_list_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_pt_list_clear
  ! ----------------------------------------------------------------
  SUBROUTINE profile_pt_list_clear(this)
    IMPLICIT NONE
    CLASS (profile_pt_list), INTENT(INOUT) :: this
    CLASS (profile_pt), POINTER :: pt

    DO WHILE (.TRUE.)
       pt => this%pop()
       IF (ASSOCIATED(pt)) THEN
          DEALLOCATE(pt)
       ELSE 
          EXIT
       END IF
    END DO
  END SUBROUTINE profile_pt_list_clear

  ! ----------------------------------------------------------------
  !  FUNCTION profile_pt_list_current
  ! ----------------------------------------------------------------
  FUNCTION profile_pt_list_current(this) RESULT(pt)
    IMPLICIT NONE
    CLASS (profile_pt), POINTER :: pt
    CLASS (profile_pt_list) :: this
    TYPE (profile_pt_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(pt)
    IF (ASSOCIATED(this%cursor)) THEN
       p => this%cursor%data
       IF (ASSOCIATED(p)) THEN
          SELECT TYPE (p)
          TYPE IS (profile_pt_ptr)
             ptr => p
             pt => ptr%p
          END SELECT
       END IF
    END IF
  END FUNCTION profile_pt_list_current

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_fill
  ! ----------------------------------------------------------------
  SUBROUTINE profile_fill(this, linkman, num_links, plink, xstart, xunits)

    IMPLICIT NONE
    CLASS (profile_t), INTENT(INOUT) :: this
    CLASS (link_manager_t), INTENT(IN) :: linkman
    INTEGER, INTENT(IN) :: num_links
    INTEGER, INTENT(IN) :: plink(:)
    DOUBLE PRECISION, INTENT(IN) :: xstart
    CHARACTER (LEN=*), INTENT(IN) :: xunits

    INTEGER :: l, p, ierr
    CLASS (link_t), POINTER :: link
    CLASS (point_t), POINTER :: pt
    CLASS (profile_pt), POINTER :: prfpt
    DOUBLE PRECISION :: x0, x
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    x0 = xstart
    IF (xunits .EQ. 'RM') x0 = x0*5280.0

    DO l = 1, num_links
       link => linkman%find(plink(l))
       IF (.NOT. ASSOCIATED(link)) THEN
          WRITE (msg, *) "profile ", this%id, ": unknown link id: ", plink(l)
          CALL error_message(msg, FATAL=.FALSE.)
          ierr = ierr + 1
          CYCLE
       END IF
       DO p = 1, link%points()
          ALLOCATE(prfpt)
          prfpt%pt => link%point(p)
          
       END DO
    END DO
    

  END SUBROUTINE profile_fill


  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_output
  ! ----------------------------------------------------------------
  SUBROUTINE profile_output(this, date_string, time_string)

    IMPLICIT NONE
    CLASS (profile_t), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: date_string, time_string

    CLASS (profile_pt), POINTER :: pt

    CALL this%pts%begin()
    pt => this%pts%current()

    DO WHILE (ASSOCIATED(pt))
       CALL this%pts%next()
       pt => this%pts%current()
    END DO

  END SUBROUTINE profile_output

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE profile_destroy(this)

    IMPLICIT NONE
    CLASS (profile_t), INTENT(INOUT) :: this
    CALL this%pts%clear()
    

  END SUBROUTINE profile_destroy



  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE profile_list_push(this, pt)
    IMPLICIT NONE
    CLASS (profile_list), INTENT(INOUT) :: this
    CLASS (profile_t), POINTER, INTENT(IN) :: pt
    TYPE (profile_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    ALLOCATE(ptr)
    ptr%p => pt
    p => ptr
    CALL this%genpush(p)
  END SUBROUTINE profile_list_push

  ! ----------------------------------------------------------------
  !  FUNCTION profile_list_pop
  ! ----------------------------------------------------------------
  FUNCTION profile_list_pop(this) RESULT(pt)
    IMPLICIT NONE
    CLASS (profile_list), INTENT(INOUT) :: this
    CLASS (profile_t), POINTER :: pt
    TYPE (profile_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(pt)
    p => this%genpop()

    IF (ASSOCIATED(p)) THEN
       SELECT TYPE (p)
       TYPE IS (profile_ptr)
          ptr => p
          pt => ptr%p
          CALL pt%destroy()
          DEALLOCATE(ptr)
       END SELECT
    END IF
    RETURN
  END FUNCTION profile_list_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_list_clear
  ! ----------------------------------------------------------------
  SUBROUTINE profile_list_clear(this)
    IMPLICIT NONE
    CLASS (profile_list), INTENT(INOUT) :: this
    CLASS (profile_t), POINTER :: pt

    DO WHILE (.TRUE.)
       pt => this%pop()
       IF (ASSOCIATED(pt)) THEN
          DEALLOCATE(pt)
       ELSE 
          EXIT
       END IF
    END DO
  END SUBROUTINE profile_list_clear

  ! ----------------------------------------------------------------
  !  FUNCTION profile_list_current
  ! ----------------------------------------------------------------
  FUNCTION profile_list_current(this) RESULT(pt)
    IMPLICIT NONE
    CLASS (profile_t), POINTER :: pt
    CLASS (profile_list) :: this
    TYPE (profile_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(pt)
    IF (ASSOCIATED(this%cursor)) THEN
       p => this%cursor%data
       IF (ASSOCIATED(p)) THEN
          SELECT TYPE (p)
          TYPE IS (profile_ptr)
             ptr => p
             pt => ptr%p
          END SELECT
       END IF
    END IF
  END FUNCTION profile_list_current

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_manager_read
  ! ----------------------------------------------------------------
  SUBROUTINE profile_manager_read(this, filename, linkman)

    IMPLICIT NONE
    CLASS (profile_manager), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: filename
    CLASS (link_manager_t), INTENT(IN) :: linkman

    INTEGER, PARAMETER :: punit = 34, maxlink = 100
    INTEGER :: line
    INTEGER :: pid, numlinks
    CHARACTER (LEN=8) :: xunits
    DOUBLE PRECISION :: xstart
    INTEGER :: plink(maxlink)
    CLASS (profile_t), POINTER :: profile
    CHARACTER (LEN=1024) :: msg

    line = 0
    pid = 0

    CALL open_existing(filename, punit, fatal=.TRUE.)
    DO WHILE(.TRUE.)
       line = line + 1
       READ(punit, *, END=100, ERR=200) numlinks, xunits, xstart
       plink = -1
       READ(punit, *, END=100, ERR=200) plink
       pid = pid + 1

       ALLOCATE(profile)
       profile%id = pid
       CALL profile%fill(linkman, numlinks, plink, xstart, xunits)

       CALL this%profs%push(profile)
       NULLIFY(profile)
       
    END DO

200 CONTINUE
    CLOSE(punit)
    WRITE (msg, *) TRIM(filename), ", line ", line, ": I/O error in profile control file"
    CALL error_message(msg, FATAL=.TRUE.)
    RETURN

100 CONTINUE 
    CLOSE(punit)
    WRITE (msg, *) TRIM(filename), ": ", pid, " profiles read"
    CALL status_message(msg)
    RETURN

  END SUBROUTINE profile_manager_read
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_manager_output
  ! ----------------------------------------------------------------
  SUBROUTINE profile_manager_output(this, time)

    IMPLICIT NONE
    CLASS (profile_manager), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: time

    CLASS (profile_t), POINTER :: p
    CHARACTER (LEN=20) :: date_string, time_string

    CALL decimal_to_date(time, date_string, time_string)

    CALL this%profs%begin()
    p => this%profs%current()

    DO WHILE (ASSOCIATED(p))
       CALL p%output(date_string, time_string)
       CALL this%profs%next()
       p => this%profs%current()
    END DO
    

  END SUBROUTINE profile_manager_output


  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_manager_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE profile_manager_destroy(this)

    IMPLICIT NONE
    CLASS (profile_manager), INTENT(INOUT) :: this
    CALL this%profs%clear()

  END SUBROUTINE profile_manager_destroy


END MODULE profile_module
  
