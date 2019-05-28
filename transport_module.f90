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
! Last Change: 2019-03-12 11:03:23 d3g096
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
  ! Instantiated for each link and transported scalar. May need
  ! specialization for specific scalar and/or link types.
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_scalar
     CLASS (scalar_t), POINTER :: scalar
     CLASS (bc_t), POINTER :: usbc
     CLASS (bc_t), POINTER :: latbc
     CLASS (met_zone_t), POINTER :: met
   CONTAINS
     PROCEDURE :: getusbc => link_scalar_getusbc
     PROCEDURE :: getlatbc => link_scalar_getlatbc
  END type link_scalar

  INTERFACE link_scalar
     MODULE PROCEDURE new_link_scalar
  END INTERFACE link_scalar
  
CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_link_scalar
  ! ----------------------------------------------------------------
  FUNCTION new_link_scalar(scalar) RESULT(l)

    IMPLICIT NONE
    TYPE (link_scalar) :: l
    CLASS (scalar_t), POINTER, INTENT(IN) :: scalar

    l%scalar => scalar
    NULLIFY(l%usbc)
    NULLIFY(l%latbc)
    NULLIFY(l%met)

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


END MODULE transport_module
