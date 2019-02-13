  ! ----------------------------------------------------------------
  ! file: profile_module.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created January 10, 2018 by William A. Perkins
  ! Last Change: 2019-02-13 10:16:44 d3g096
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
  USE mass1_config

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

  INTERFACE profile_pt
     MODULE PROCEDURE new_profile_pt
  END INTERFACE profile_pt

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

  INTERFACE profile_pt_list
     PROCEDURE :: new_profile_pt_list
  END INTERFACE profile_pt_list
  
  ! ----------------------------------------------------------------
  ! TYPE profile
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: profile_t
     INTEGER :: id
     TYPE (profile_pt_list) :: pts
     CHARACTER (LEN=1024), PRIVATE :: filename = ""
     INTEGER :: punit
     LOGICAL, PRIVATE :: firstwrite = .TRUE.
   CONTAINS
     PROCEDURE :: fill => profile_fill
     PROCEDURE :: name => profile_name
     PROCEDURE :: output => profile_output
     PROCEDURE :: destroy => profile_destroy
  END type profile_t

  INTERFACE profile_t
     MODULE PROCEDURE new_profile_t
  END INTERFACE profile_t

  ! ----------------------------------------------------------------
  ! TYPE profile_ptr
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: profile_ptr
     CLASS (profile_t), POINTER :: p
  END type profile_ptr

  INTERFACE profile_ptr
     MODULE PROCEDURE new_profile_ptr
  END INTERFACE profile_ptr

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

  INTERFACE profile_list
     MODULE PROCEDURE new_profile_list
  END INTERFACE profile_list

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
  
  ! This is base for stupid Fortran I/O unit numbers for profile output. 
  INTEGER, PARAMETER :: punit_base = 501
  INTEGER :: punit_current

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_profile_pt
  ! ----------------------------------------------------------------
  FUNCTION new_profile_pt()

    IMPLICIT NONE
    TYPE (profile_pt) :: new_profile_pt
    new_profile_pt%link_id = 0
    new_profile_pt%point_idx = 0
    new_profile_pt%profx = 0.0
    NULLIFY(new_profile_pt%pt)

  END FUNCTION new_profile_pt


  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_pt_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE profile_pt_destroy(this)

    IMPLICIT NONE
    CLASS (profile_pt) :: this
    NULLIFY(this%pt)

  END SUBROUTINE profile_pt_destroy

  ! ----------------------------------------------------------------
  !  FUNCTION new_profile_pt_list
  ! ----------------------------------------------------------------
  FUNCTION new_profile_pt_list()

    IMPLICIT NONE
    TYPE (profile_pt_list) :: new_profile_pt_list
    NULLIFY(new_profile_pt_list%head)
    NULLIFY(new_profile_pt_list%tail)
  END FUNCTION new_profile_pt_list

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
  !  FUNCTION new_profile_t
  ! ----------------------------------------------------------------
  FUNCTION new_profile_t(id)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: id
    TYPE (profile_t) :: new_profile_t
    new_profile_t%id = id
    new_profile_t%pts = profile_pt_list()
    new_profile_t%filename = ""
    new_profile_t%punit = punit_current
    punit_current = punit_current + 1
    new_profile_t%firstwrite = .TRUE.
  END FUNCTION new_profile_t

  ! ----------------------------------------------------------------
  !  FUNCTION new_profile_ptr
  ! ----------------------------------------------------------------
  FUNCTION new_profile_ptr() 
    IMPLICIT NONE
    TYPE (profile_ptr) :: new_profile_ptr
    NULLIFY(new_profile_ptr%p)
  END FUNCTION new_profile_ptr


  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_fill
  ! ----------------------------------------------------------------
  SUBROUTINE profile_fill(this, linkman, num_links, plink, xstart, xfactor)

    IMPLICIT NONE
    CLASS (profile_t), INTENT(INOUT) :: this
    CLASS (link_manager_t), INTENT(IN) :: linkman
    INTEGER, INTENT(IN) :: num_links
    INTEGER, INTENT(IN) :: plink(:)
    DOUBLE PRECISION, INTENT(IN) :: xstart, xfactor

    INTEGER :: l, p, n, ierr
    CLASS (link_t), POINTER :: link
    CLASS (profile_pt), POINTER :: prfpt
    DOUBLE PRECISION :: xprf, x, xold
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    xprf = xstart

    xprf = xprf*xfactor

    DO l = 1, num_links
       link => linkman%find(plink(l))
       IF (.NOT. ASSOCIATED(link)) THEN
          WRITE (msg, *) "profile ", this%id, ": unknown link id: ", plink(l)
          CALL error_message(msg, FATAL=.FALSE.)
          ierr = ierr + 1
          CYCLE
       END IF
       n = link%points()
       
       DO p = n, 1, -1
          ALLOCATE(prfpt)
          prfpt%link_id = link%id
          prfpt%point_idx = p
          prfpt%pt => link%point(p)
          x = prfpt%pt%x
          IF (p .LT. n) THEN
             xprf = xprf + ABS(x - xold)
          END IF
          prfpt%profx = xprf
          xold = prfpt%pt%x
          CALL this%pts%push(prfpt)
          NULLIFY(prfpt)
       END DO
    END DO

    CALL this%pts%begin()
    prfpt => this%pts%current()
    DO WHILE (ASSOCIATED(prfpt)) 
       prfpt%profx = prfpt%profx/xfactor
       CALL this%pts%next()
       prfpt => this%pts%current()
    END DO

  END SUBROUTINE profile_fill

  ! ----------------------------------------------------------------
  !  FUNCTION profile_name
  ! ----------------------------------------------------------------
  FUNCTION profile_name(this) RESULT(name)

    IMPLICIT NONE
    CLASS (profile_t), INTENT(INOUT) :: this
    CHARACTER (LEN=1024) :: name
    CHARACTER (LEN=256) :: s

    IF (LEN(TRIM(this%filename)) .EQ. 0) THEN
       WRITE(s, *) this%id
       s = ADJUSTL(s)
       this%filename = 'profile' // TRIM(s) // '.out'
    END IF
    name = this%filename
  END FUNCTION profile_name

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_output
  ! ----------------------------------------------------------------
  SUBROUTINE profile_output(this, date_string, time_string)

    IMPLICIT NONE
    CLASS (profile_t), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: date_string, time_string

    CLASS (profile_pt), POINTER :: pt
    INTEGER :: j
    DOUBLE PRECISION :: depth

    IF (this%firstwrite) THEN
       CALL open_new(this%name(), this%punit)
    END IF
    this%firstwrite = .FALSE.

    ! profile header
    WRITE(this%punit, 1110)
    WRITE(this%punit, 1010) this%id, date_string, time_string, this%pts%size()
    WRITE(this%punit, 1005)
    WRITE(this%punit, 1110)

    CALL this%pts%begin()
    pt => this%pts%current()

    j = 0

    DO WHILE (ASSOCIATED(pt))
       j = j + 1
       depth = pt%pt%hnow%y - pt%pt%thalweg

       WRITE(this%punit,1000) &
            &pt%link_id, pt%point_idx, j, pt%profx, &
            &pt%pt%hnow%y, pt%pt%hnow%q, pt%pt%hnow%v, depth, &
            &0.0, 0.0, 0.0, 0.0, &
            &pt%pt%thalweg, pt%pt%xsprop%area, pt%pt%xsprop%topwidth,&
            &pt%pt%xsprop%hydrad, pt%pt%hnow%froude_num, pt%pt%hnow%courant_num,&
            &pt%pt%hnow%diffuse_num, pt%pt%hnow%friction_slope, pt%pt%hnow%bed_shear
       CALL this%pts%next()
       pt => this%pts%current()
    END DO

1110 FORMAT('#',160('-'))
1010 FORMAT('#Profile Number - ',i3,'   for Date: ',a10,'  Time: ',a8,'  Max number of points on profile = ',i6/)
1005 FORMAT('#link',8x,'point',2x,'distance',2x,'water elev',3x,'discharge',5x,'vel',2x,'depth', &
          6x,'conc',6x,'temp' ,2x,'%Sat',3x,'TDG P', &
          5x,'thalweg el',2x,'area ',2x,'top width',2x,'hyd rad',2x,'Fr #',2x,'Cr #',2x,'D #',2x,'frict slope', &
          2x,'bed shear')
1000      FORMAT(i5,1x,i5,1x,i5,1x,f9.3,1x,f8.3,2x,f14.4,2x,f8.3,2x,f8.3,2x,f10.2,2x,f6.2,2x,f6.2,2x,f6.1,2x, &
               f8.2,2x,es10.2,2x, &
               f8.2,2x,f6.2,f6.2,f6.2,f6.2,es10.2,2x,es10.2)

  END SUBROUTINE profile_output

  ! ----------------------------------------------------------------
  ! SUBROUTINE profile_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE profile_destroy(this)

    IMPLICIT NONE
    CLASS (profile_t), INTENT(INOUT) :: this

    CLOSE(this%punit)
    CALL this%pts%clear()
    

  END SUBROUTINE profile_destroy

  ! ----------------------------------------------------------------
  !  FUNCTION new_profile_list
  ! ----------------------------------------------------------------
  FUNCTION new_profile_list() 

    IMPLICIT NONE
    TYPE (profile_list) :: new_profile_list
    NULLIFY(new_profile_list%head)
    NULLIFY(new_profile_list%tail)
  END FUNCTION new_profile_list

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
  SUBROUTINE profile_manager_read(this, theconfig, linkman)

    IMPLICIT NONE
    CLASS (profile_manager), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(INOUT) :: theconfig
    CLASS (link_manager_t), INTENT(IN) :: linkman

    INTEGER, PARAMETER :: punit = 34, maxlink = 100
    INTEGER :: line
    INTEGER :: pid, numlinks
    CHARACTER (LEN=8) :: xunits
    DOUBLE PRECISION :: xstart, xfactor
    INTEGER :: plink(maxlink)
    CLASS (profile_t), POINTER :: profile
    CHARACTER (LEN=1024) :: msg

    line = 0
    pid = 0
    punit_current = punit_base

    CALL open_existing(theconfig%profile_file, punit, fatal=.TRUE.)
    DO WHILE(.TRUE.)
       line = line + 1
       READ(punit, *, END=100, ERR=200) numlinks, xunits, xstart
       plink = -1
       READ(punit, *, END=100, ERR=200) plink
       pid = pid + 1

       ALLOCATE(profile, SOURCE=profile_t(pid))
       !profile = profile_t(pid)

       ! get a factor to convert internal length units to the desired
       ! profile output units
    
       IF (xunits .EQ. 'FT') THEN
          xfactor = theconfig%channel_len_factor(CHANNEL_FOOT)
       ELSE IF (xunits .EQ. 'M') THEN
          xfactor = theconfig%channel_len_factor(CHANNEL_METER)
       ELSE IF (xunits .EQ. 'RM') THEN
          xfactor = theconfig%channel_len_factor(CHANNEL_MILE)
       ELSE IF (xunits .EQ. 'RKM') THEN
          xfactor = theconfig%channel_len_factor(CHANNEL_KM)
       ELSE
          WRITE (msg, *) theconfig%profile_file, ": line ", line, &
               &": error: unknown length units: ", TRIM(xunits)
          CALL error_message(msg, FATAL=.TRUE.)
       END IF
       
       CALL profile%fill(linkman, numlinks, plink, xstart, xfactor)

       CALL this%profs%push(profile)
       NULLIFY(profile)
       
    END DO

200 CONTINUE
    CLOSE(punit)
    WRITE (msg, *) TRIM(theconfig%profile_file), ", line ", &
         &line, ": I/O error in profile control file"
    CALL error_message(msg, FATAL=.TRUE.)
    RETURN

100 CONTINUE 
    CLOSE(punit)
    WRITE (msg, *) TRIM(theconfig%profile_file), ": ", pid, " profiles read"
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
  
