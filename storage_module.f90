! ----------------------------------------------------------------
! file: storage_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April  3, 2020 by William A. Perkins
! Last Change: 2020-08-05 08:42:44 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE storage_module
! ----------------------------------------------------------------
MODULE storage_module

  USE table_module
  USE utility

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! ABSTRACT TYPE storage_t
  !
  ! The basis for a "storage" or bucket.  This has no state (stage),
  ! which is expected to be managed by the client. This only defines
  ! the relationship between stage, surface, area, volume, etc.
  ! ----------------------------------------------------------------
  
  TYPE, PUBLIC, ABSTRACT :: storage_t
   CONTAINS
     ! surface area as a function of stage
     PROCEDURE (area_proc), DEFERRED :: area

     ! total volume as a function of stage
     PROCEDURE (area_proc), DEFERRED :: volume

     ! average deptha as a function of stage
     PROCEDURE (area_proc), DEFERRED :: depth

     ! rate of change in volume per change in stage, also a function
     ! of stage
     PROCEDURE (area_proc), DEFERRED :: dvdy
  END type storage_t

  ABSTRACT INTERFACE
     FUNCTION area_proc(this, y)
       IMPORT :: storage_t
       IMPLICIT NONE
       DOUBLE PRECISION :: area_proc
       CLASS (storage_t), INTENT(IN) :: this
       DOUBLE PRECISION, INTENT(IN) :: y
     END FUNCTION area_proc
  END INTERFACE

  ! ----------------------------------------------------------------
  ! TYPE simple_storage
  ! A very simple storage defined by constant area and bottom elev
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(storage_t) :: simple_storage
     DOUBLE PRECISION :: ybottom
     DOUBLE PRECISION :: the_area
   CONTAINS
     PROCEDURE :: area => simple_storage_area
     PROCEDURE :: volume => simple_storage_volume
     PROCEDURE :: depth => simple_storage_depth
     PROCEDURE :: dvdy => simple_storage_dvdy
  END type simple_storage

  INTERFACE simple_storage
     MODULE PROCEDURE new_simple_storage
  END INTERFACE simple_storage

  ! ----------------------------------------------------------------
  ! TYPE storage_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: storage_ptr
     CLASS (storage_t), POINTER :: p
  END type storage_ptr
     
CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_simple_storage
  ! ----------------------------------------------------------------
  FUNCTION new_simple_storage(area, ybottom) RESULT(s)

    IMPLICIT NONE
    TYPE(simple_storage) :: s
    DOUBLE PRECISION, INTENT(IN) :: area, ybottom
    s%ybottom = ybottom
    s%the_area = area
  END FUNCTION new_simple_storage

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_area
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_area(this, y) RESULT(a)

    IMPLICIT NONE
    DOUBLE PRECISION :: a
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    a = this%the_area
    IF (y .LE. this%ybottom) THEN
       a = 0.0
    ENDIF
  END FUNCTION simple_storage_area

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_volume
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_volume(this, y) RESULT(v)

    IMPLICIT NONE
    DOUBLE PRECISION :: v
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    v = 0.0
    IF (y .GT. this%ybottom) THEN
       v = (y - this%ybottom)*this%the_area
    ENDIF
  END FUNCTION simple_storage_volume

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_depth
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_depth(this, y) RESULT(d)

    IMPLICIT NONE
    DOUBLE PRECISION :: d
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    d = 0.0
    IF (y .GT. this%ybottom) THEN
       d = (y - this%ybottom)*this%the_area
    ENDIF
  END FUNCTION simple_storage_depth

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_dvdy
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_dvdy(this, y) RESULT(dvdy)

    IMPLICIT NONE
    DOUBLE PRECISION :: dvdy
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    dvdy = this%area(y)

  END FUNCTION simple_storage_dvdy
END MODULE storage_module

