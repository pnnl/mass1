! ----------------------------------------------------------------
! file: transport_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created August  8, 2018 by William A. Perkins
! Last Change: 2019-03-06 11:02:28 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE transport
! ----------------------------------------------------------------
MODULE transport_module

  USE point_module
  USE bc_module
  USE scalar_module
  
  IMPLICIT NONE
  

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE link_scalar
  !
  ! Instantiated for each link and transported scalar. Should be
  ! specialized for specific scalar and/or link types.
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_scalar
     CLASS (scalar_t), POINTER :: scalar
     CLASS (bc_t), POINTER :: usbc
     CLASS (bc_t), POINTER :: latbc
     CLASS (met_zone_t), POINTER :: met
     INTEGER :: npts
     DOUBLE PRECISION, DIMENSION(:), POINTER :: cnow, cold
   CONTAINS
     PROCEDURE :: getusbc => link_scalar_getusbc
     PROCEDURE :: getlatbc => link_scalar_getlatbc
     PROCEDURE :: source => link_scalar_source
  END type link_scalar

  INTERFACE link_scalar
     MODULE PROCEDURE new_link_scalar
  END INTERFACE link_scalar
  
CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_link_scalar
  ! ----------------------------------------------------------------
  FUNCTION new_link_scalar(npts, scalar) RESULT(l)

    IMPLICIT NONE
    TYPE (link_scalar) :: l
    INTEGER, INTENT(IN) :: npts
    CLASS (scalar_t), POINTER, INTENT(IN) :: scalar

    l%npts = npts
    l%scalar => scalar
    NULLIFY(l%usbc)
    NULLIFY(l%latbc)
    NULLIFY(l%met)

    ALLOCATE(l%cnow(l%npts), l%cold(l%npts))
    
  END FUNCTION new_link_scalar


  ! ----------------------------------------------------------------
  !  FUNCTION link_scalar_getusbc
  ! ----------------------------------------------------------------
  FUNCTION link_scalar_getusbc(this) RESULT(c0)

    IMPLICIT NONE
    DOUBLE PRECISION :: c0
    CLASS (link_scalar), INTENT(IN) :: this

    c0 = this%usbc%current_value
    
  END FUNCTION link_scalar_getusbc

  ! ----------------------------------------------------------------
  !  FUNCTION link_scalar_getlatbc
  ! ----------------------------------------------------------------
  FUNCTION link_scalar_getlatbc(this) RESULT(c0)

    IMPLICIT NONE
    DOUBLE PRECISION :: c0
    CLASS (link_scalar), INTENT(IN) :: this

    c0 = this%latbc%current_value
    
  END FUNCTION link_scalar_getlatbc


  ! ----------------------------------------------------------------
  ! SUBROUTINE link_scalar_source
  ! ----------------------------------------------------------------
  SUBROUTINE link_scalar_source(this, deltat)

    IMPLICIT NONE
    CLASS(link_scalar), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat
    INTEGER :: i
    DOUBLE PRECISION :: latc

    DO i = 2, this%npts - 1
       IF (ASSOCIATED(this%latbc)) THEN
          latc = this%latbc%current_value
       ELSE
          latc = this%cnow(i)
       END IF
       ! this%cnow(i) = this%scalar%source(cnow(i), pt, latc, deltat, this%met)
    END DO
  END SUBROUTINE link_scalar_source


END MODULE transport_module
