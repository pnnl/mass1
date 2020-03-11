! ----------------------------------------------------------------
! file: gage_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  8, 2018 by William A. Perkins
! Last Change: 2020-03-11 09:36:20 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE gage_module
! ----------------------------------------------------------------
MODULE gage_module

  USE utility
  USE date_time
  USE dlist_module
  USE point_module
  USE link_manager_module
  USE scalar_module
  
  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE gage_t
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: gage_t
     INTEGER :: link_id, point_idx
     CHARACTER (LEN=256) :: gagename
     INTEGER :: gunit
     CLASS (point_t), POINTER :: pt
     CHARACTER (LEN=1024), PRIVATE :: filename
     LOGICAL, PRIVATE :: firstwrite = .TRUE.
   CONTAINS
     PROCEDURE, PRIVATE :: name => gage_name
     PROCEDURE, PRIVATE :: header => gage_header
     PROCEDURE :: output => gage_output
     PROCEDURE :: destroy => gage_destroy
  END type gage_t

  ! ----------------------------------------------------------------
  ! TYPE gage_ptr
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: gage_ptr
     CLASS (gage_t), POINTER :: p
  END type gage_ptr

  ! ----------------------------------------------------------------
  ! TYPE gage_list
  ! ----------------------------------------------------------------
  TYPE, PRIVATE, EXTENDS(dlist) :: gage_list
   CONTAINS 
     PROCEDURE :: push => gage_list_push
     PROCEDURE :: pop => gage_list_pop
     PROCEDURE :: clear => gage_list_clear
     PROCEDURE :: current => gage_list_current
  END type gage_list

  INTERFACE gage_list
     MODULE PROCEDURE new_gage_list
  END INTERFACE gage_list

  ! ----------------------------------------------------------------
  ! TYPE gage_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: gage_manager
     TYPE (gage_list) :: gages
   CONTAINS
     PROCEDURE :: read => gage_manager_read
     PROCEDURE :: output => gage_manager_output
     PROCEDURE :: destroy => gage_manager_destroy
  END type gage_manager

  INTERFACE gage_manager
     MODULE PROCEDURE new_gage_manager
  END INTERFACE gage_manager

  ! This is base for stupid Fortran I/O unit numbers for gage output. 
  INTEGER, PARAMETER :: gunit_base = 201
  INTEGER :: gunit_current
  
CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION gage_name
  ! ----------------------------------------------------------------
  FUNCTION gage_name(this) RESULT (oname)

    IMPLICIT NONE
    CLASS (gage_t), INTENT(INOUT) :: this
    CHARACTER (LEN=1024) :: oname
    CHARACTER (LEN=80) :: string1, string2

    IF (LEN(TRIM(this%filename)) .LE. 0) THEN
       this%filename = ''
       IF (LEN(TRIM(this%gagename)) .GT. 0) THEN
          this%filename = 'ts' // TRIM(this%gagename) // '.out'
       ELSE 
          WRITE(string1, *) this%link_id
          string1 = ADJUSTL(string1)
          WRITE(string2, *) this%point_idx
          string2 = ADJUSTL(string2)
          this%filename = 'ts' // TRIM(string1) // TRIM(string2) // '.out'
       END IF
    END IF
    oname = this%filename
  END FUNCTION gage_name

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_header
  ! ----------------------------------------------------------------
  SUBROUTINE gage_header(this)
    IMPLICIT NONE
    CLASS (gage_t), INTENT(INOUT) :: this
    WRITE(this%gunit, 100)&
         &"#Date", "Time", &
         &"WSE", "Q", "Vel", "LatQ", "Depth", &
         &"TDG", "T", "Tbed", "TDGSat", "TDGPr", &
         &"Thalweg", "Area", "TopW", "HydRad", &
         &"Froude", "FSlope", "Shear"
100 FORMAT(A10, 2X, A8, 2X, &
         &A12, 2X, A12, 2X, A6, 2X, A11, 2X, A7, 2X, &
         &A10, 2X, A6, 2X, A6, 2X, A6, 2X, A6, 2X, &
         &A8, 2X, A14, 2X, A10, 2X, A6, 2X, A6, 2X, A10, 2X, A10)
    

  END SUBROUTINE gage_header


  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_output
  ! ----------------------------------------------------------------
  SUBROUTINE gage_output(this, date_string, time_string, sclrman)
    IMPLICIT NONE
    CLASS (gage_t), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: date_string, time_string
    CLASS (scalar_manager), INTENT(IN) :: sclrman

    DOUBLE PRECISION :: tout, tbed, tdgout, tdgsat, tdgpress, cout(10)
    INTEGER :: ncout, s
    DOUBLE PRECISION :: depth

    IF (this%firstwrite) THEN
       this%gunit = gunit_current
       gunit_current = gunit_current + 1
       CALL open_new(this%name(), this%gunit)
       CALL this%header()
    END IF
    this%firstwrite = .FALSE.
    
    ASSOCIATE( pt => this%pt, h => this%pt%hnow, pr => this%pt%xsprop )

      ! FIXME: this copies original MASS1 output. it needs to be made
      ! more general
      cout = 0.0
      tout = 0.0
      tbed = 0.0
      tdgout = 0.0
      tdgsat = 0.0
      tdgpress = 0.0
      DO s = 1, sclrman%nspecies
         CALL sclrman%species(s)%p%output(s, pt, cout, ncout)
         SELECT CASE (sclrman%species(s)%p%bctype)
         CASE (TRANS_BC_TYPE)
            tdgout = cout(1)
            IF (ncout .GT. 1) THEN
               tdgsat = cout(2)
               tdgpress = cout(3)
            END IF
         CASE DEFAULT
            tout = cout(1)
            tbed = pt%trans%bedtemp
         END SELECT
      END DO

      depth = h%y - pt%thalweg
      WRITE(this%gunit,1010) date_string, time_string, &
           &h%y, h%q, h%v, h%lateral_inflow, depth, &
           &tdgout , tout, tbed, tdgsat, tdgpress, &
           &pt%thalweg, pr%area, pr%topwidth, pr%hydrad,&
           &h%froude_num, h%friction_slope, h%bed_shear
    END ASSOCIATE
    
1010 FORMAT(a10,2x,a8,2x,&
          &f12.3,2x, f12.2,2x, f6.2,2x, es11.4,2x, f7.2,2x, &
          &f10.2,2x, f6.2,2x, f6.2,2x, f6.2,2x, f6.1,2x, &
          &f8.2,2x, es14.2,2x, es10.4,2x, f6.2,2x, &
          &f6.2,2x, es10.4,2x, es10.4)
  END SUBROUTINE gage_output


  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE gage_destroy(this)

    IMPLICIT NONE
    CLASS (gage_t), INTENT(INOUT) :: this

    CLOSE(this%gunit)
    NULLIFY(this%pt)

  END SUBROUTINE gage_destroy


  ! ----------------------------------------------------------------
  !  FUNCTION new_gage_list
  ! ----------------------------------------------------------------
  FUNCTION new_gage_list()

    IMPLICIT NONE
    TYPE (gage_list) :: new_gage_list
    NULLIFY(new_gage_list%head)
    NULLIFY(new_gage_list%tail)
  END FUNCTION new_gage_list

  
  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE gage_list_push(this, gage)
    IMPLICIT NONE
    CLASS (gage_list), INTENT(INOUT) :: this
    CLASS (gage_t), POINTER, INTENT(IN) :: gage
    TYPE (gage_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    ALLOCATE(ptr)
    ptr%p => gage
    p => ptr
    CALL this%genpush(p)
  END SUBROUTINE gage_list_push

  ! ----------------------------------------------------------------
  !  FUNCTION gage_list_pop
  ! ----------------------------------------------------------------
  FUNCTION gage_list_pop(this) RESULT(gage)
    IMPLICIT NONE
    CLASS (gage_list), INTENT(INOUT) :: this
    CLASS (gage_t), POINTER :: gage
    TYPE (gage_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(gage)
    p => this%genpop()

    IF (ASSOCIATED(p)) THEN
       SELECT TYPE (p)
       TYPE IS (gage_ptr)
          ptr => p
          gage => ptr%p
          DEALLOCATE(ptr)
       END SELECT
    END IF
    RETURN
  END FUNCTION gage_list_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_list_clear
  ! ----------------------------------------------------------------
  SUBROUTINE gage_list_clear(this)
    IMPLICIT NONE
    CLASS (gage_list), INTENT(INOUT) :: this
    CLASS (gage_t), POINTER :: gage

    DO WHILE (.TRUE.)
       gage => this%pop()
       IF (ASSOCIATED(gage)) THEN
          CALL gage%destroy()
          DEALLOCATE(gage)
       ELSE 
          EXIT
       END IF
    END DO
  END SUBROUTINE gage_list_clear

  ! ----------------------------------------------------------------
  !  FUNCTION gage_list_current
  ! ----------------------------------------------------------------
  FUNCTION gage_list_current(this) RESULT(gage)
    IMPLICIT NONE
    CLASS (gage_t), POINTER :: gage
    CLASS (gage_list) :: this
    TYPE (gage_ptr), POINTER :: ptr
    CLASS(*), POINTER :: p

    NULLIFY(gage)
    IF (ASSOCIATED(this%cursor)) THEN
       p => this%cursor%data
       IF (ASSOCIATED(p)) THEN
          SELECT TYPE (p)
          TYPE IS (gage_ptr)
             ptr => p
             gage => ptr%p
          END SELECT
       END IF
    END IF
  END FUNCTION gage_list_current

  ! ----------------------------------------------------------------
  !  FUNCTION new_gage_manager
  ! ----------------------------------------------------------------
  FUNCTION new_gage_manager() RESULT(man)

    IMPLICIT NONE
    TYPE (gage_manager) :: man
    man%gages = new_gage_list()
  END FUNCTION new_gage_manager


  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_manager_read
  ! ----------------------------------------------------------------
  SUBROUTINE gage_manager_read(this, fname, linkman)

    IMPLICIT NONE
    CLASS (gage_manager), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: fname
    CLASS (link_manager_t), INTENT(IN) :: linkman

    INTEGER, PARAMETER :: gcunit = 33
    INTEGER :: line, ierr
    INTEGER :: linkid, pointidx
    CHARACTER (LEN=256) :: gname
    CLASS (gage_t), POINTER :: gage
    CLASS (link_t), POINTER :: link
    CLASS (point_t), POINTER :: point
    CHARACTER (LEN=1024) :: msg

    ! assume the list is empty but pointers need nullification

    NULLIFY(this%gages%head)
    NULLIFY(this%gages%tail)
    
    gunit_current = gunit_base

    CALL open_existing(fname, gcunit, fatal=.TRUE.)

    line = 0
    ierr = 0
    DO WHILE(.TRUE.)
       line = line + 1
       gname = ""
       READ(gcunit,*, END=100, ERR=200) linkid, pointidx, gname
       link => linkman%find(linkid)
       IF (.NOT. ASSOCIATED(link)) THEN
          WRITE (msg, *) TRIM(fname), ", line ", line, ": unknown link id: ", linkid
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END IF

       point => link%point(pointidx)
       IF (.NOT. ASSOCIATED(point)) THEN
          WRITE (msg, *) TRIM(fname), ", line ", line, ": unknown point index: ", pointidx
          CALL error_message(msg)
          ierr = ierr + 1
          CYCLE
       END IF
          
       ALLOCATE(gage)

       gage%filename = ""
       gage%gagename = ""
       gage%link_id = linkid
       gage%point_idx = pointidx
       IF (LEN(TRIM(gname)) .GT. 0) THEN
          gage%gagename = gname
       END IF
       gage%pt => point

       CALL this%gages%push(gage)
       
    END DO

200 CONTINUE
    WRITE (msg, *) TRIM(fname), ", line ", line, ": I/O error in gage control file"
    CALL error_message(msg, FATAL=.TRUE.)
100 CONTINUE
    CLOSE(gcunit)
    IF (ierr .GT. 0) THEN
       WRITE (msg, *) TRIM(fname), ": error(s) reading"
       CALL error_message(msg, FATAL=.TRUE.)
    END IF
    RETURN
  END SUBROUTINE gage_manager_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_manager_output
  ! ----------------------------------------------------------------
  SUBROUTINE gage_manager_output(this, time, sclrman)

    IMPLICIT NONE
    CLASS (gage_manager), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: time
    CLASS (scalar_manager), INTENT(IN) :: sclrman

    CHARACTER (LEN=20) :: date_string, time_string
    CLASS (gage_t), POINTER :: gage

    CALL decimal_to_date(time, date_string, time_string)
    
    CALL this%gages%begin()
    gage => this%gages%current()

    DO WHILE (ASSOCIATED(gage))
       CALL gage%output(date_string, time_string, sclrman)
       CALL this%gages%next()
       gage => this%gages%current()
    END DO

  END SUBROUTINE gage_manager_output

  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_manager_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE gage_manager_destroy(this)

    IMPLICIT NONE
    CLASS (gage_manager), INTENT(INOUT) :: this
    CALL this%gages%clear()

  END SUBROUTINE gage_manager_destroy


END MODULE gage_module
