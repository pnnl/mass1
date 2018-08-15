! ----------------------------------------------------------------
! file: transport_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created August  8, 2018 by William A. Perkins
! Last Change: 2018-08-13 14:57:54 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE transport
! ----------------------------------------------------------------
MODULE transport_module

  IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC :: scalar
     LOGICAL :: needmet
     CONTAINS
       PROCEDURE :: source 
    END type scalar

  ! ----------------------------------------------------------------
  ! TYPE link_scalar
  !
  ! Instantiated for each link and transported scalar. Should be
  ! specialized for specific scalars.
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: link_scalar
     
     CLASS (bc_t), POINTER :: usbc
     CLASS (bc_t), POINTER :: latbc
   CONTAINS
     PROCEDURE :: getusbc => link_transport_getusbc
     PROCEDURE :: latbc => link_transport_getlatbc
     PROCEDURE :: 
  END type link_scalar

  
  ! ----------------------------------------------------------------
  ! TYPE link_met_scalar
  ! 
  ! For those scalars that require met data (e.g. temperature, TDG)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(link_scalar) :: link_met_scalar
     CLASS (met_zone_t) :: met
   CONTAINS 
  END type link_met_scalar


CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION scalar_source
  ! ----------------------------------------------------------------
  FUNCTION scalar_source(pt, latqbc, latcbc) RESULT (cout)

    IMPLICIT NONE
    DOUBLE PRECISION :: cout
    CLASS (bc_t), POINTER :: latqbc
    CLASS (bc_t), POINTER :: latcbc

    

    scalar_source = 
  END FUNCTION scalar_source


END MODULE transport_module
