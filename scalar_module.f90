!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
! ----------------------------------------------------------------
! file: scalar_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  7, 2019 by William A. Perkins
! Last Change: 2019-03-06 11:52:04 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE scalar_module
! ----------------------------------------------------------------
MODULE scalar_module

  USE mass1_config
  USE point_module
  USE bc_module
  USE met_zone

  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! TYPE scalar_t
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: scalar_t
     LOGICAL :: dometric
     LOGICAL :: dodiffusion
     LOGICAL :: dolatinflow
     LOGICAL :: dosource
     LOGICAL :: needmet
     LOGICAL :: istemp
     INTEGER (KIND=KIND(BC_ENUM)) :: bctype
   CONTAINS
     PROCEDURE :: source => scalar_source
  END type scalar_t

  INTERFACE scalar_t
     MODULE PROCEDURE new_scalar_t
  END INTERFACE scalar_t

  ! ----------------------------------------------------------------
  ! TYPE scalar_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: scalar_ptr
     CLASS (scalar_t), POINTER :: p
  END type scalar_ptr

  ! ----------------------------------------------------------------
  ! TYPE scalar_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: scalar_manager
     INTEGER :: nspecies
     TYPE (scalar_ptr), POINTER :: species(:)
   CONTAINS
     PROCEDURE :: initialize => scalar_manager_init
  END type scalar_manager
  
  ! ----------------------------------------------------------------
  ! TYPE temp_scalar
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(scalar_t) :: temperature
   CONTAINS
     PROCEDURE :: source => temperature_source
  END type temperature

  INTERFACE temperature
     MODULE PROCEDURE new_temperature
  END INTERFACE temperature

  ! ----------------------------------------------------------------
  ! TYPE tdg_scalar
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(scalar_t) :: tdg
   CONTAINS
     PROCEDURE :: source => tdg_source
  END type tdg

  INTERFACE tdg
     MODULE PROCEDURE new_tdg
  END INTERFACE tdg


CONTAINS


  ! ----------------------------------------------------------------
  !  FUNCTION new_scalar_t
  ! ----------------------------------------------------------------
  FUNCTION new_scalar_t(dometric, dodiff, dolatq, dosrc) RESULT(s)

    IMPLICIT NONE
    TYPE (scalar_t) :: s
    LOGICAL, INTENT(IN) :: dometric, dodiff, dolatq, dosrc

    s%dometric = dometric
    s%dodiffusion = dodiff
    s%dolatinflow = dolatq
    s%dosource = dosrc
    s%bctype = BC_ENUM

  END FUNCTION new_scalar_t

  ! ----------------------------------------------------------------
  !  FUNCTION scalar_source
  ! ----------------------------------------------------------------
  FUNCTION scalar_source(this, cin, pt, latc, deltat, met) RESULT (cout)

    IMPLICIT NONE
    DOUBLE PRECISION :: cout
    CLASS(scalar_t), INTENT(IN) :: this
    TYPE (point_transport_state), INTENT(IN) :: pt
    DOUBLE PRECISION, INTENT(IN) :: cin, latc, deltat
    TYPE (met_zone_t), INTENT(INOUT) :: met

    DOUBLE PRECISION :: avg_area, avg_latq, c

    cout = cin
    
    ! change in concentration due to lateral inflow

    IF (this%dolatinflow) THEN
       avg_area = (pt%xsprop%area + pt%xspropold%area)/2.0
       IF (avg_area .GT. 0.0) THEN
          avg_latq = (pt%hnow%lateral_inflow + pt%hold%lateral_inflow)/2.0
          IF (avg_latq < 0.0) THEN
             c = cin
          ELSE
             c = latc
          END IF
          cout = (cin*avg_area + latc*avg_latq*deltat)/avg_area
       END IF
    END IF

  END FUNCTION scalar_source

  ! ----------------------------------------------------------------
  !  FUNCTION new_temperature
  ! ----------------------------------------------------------------
  FUNCTION new_temperature(dometric, dodiff, dolatq, dosrc) RESULT(t)

    IMPLICIT NONE
    TYPE (temperature) :: t
    LOGICAL, INTENT(IN) :: dometric, dodiff, dolatq, dosrc

    t%dometric = dometric
    t%dodiffusion = dodiff
    t%dolatinflow = dolatq
    t%dosource = dosrc
    t%bctype = TEMP_BC_TYPE
    t%needmet = t%dosource

  END FUNCTION new_temperature


  ! ----------------------------------------------------------------
  !  FUNCTION temperature_source
  ! ----------------------------------------------------------------
  FUNCTION temperature_source(this, cin, pt, latc, deltat, met) RESULT (tout)

    IMPLICIT NONE

    DOUBLE PRECISION :: tout
    CLASS(temperature), INTENT(IN) :: this
    TYPE (point_transport_state), INTENT(IN) :: pt
    DOUBLE PRECISION, INTENT(IN) :: cin, latc, deltat
    TYPE (met_zone_t), INTENT(INOUT) :: met

    DOUBLE PRECISION :: area, width

    tout = this%scalar_t%source(cin, pt, latc, deltat, met)

    area = pt%xsprop%area
    width = pt%xsprop%topwidth

    IF (this%dosource) THEN
       IF (area .GT. 0.0) THEN
          ! FIXME: metric units
          tout = tout + met%energy_flux(tout)*deltat*width/area
       END IF
    END IF
  END FUNCTION temperature_source

  ! ----------------------------------------------------------------
  !  FUNCTION new_tdg
  ! ----------------------------------------------------------------
  FUNCTION new_tdg(dometric, dodiff, dolatq, dosrc) RESULT(t)

    IMPLICIT NONE
    TYPE (tdg) :: t
    LOGICAL, INTENT(IN) :: dometric, dodiff, dolatq, dosrc

    t%dometric = dometric
    t%dodiffusion = dodiff
    t%dolatinflow = dolatq
    t%dosource = dosrc
    t%bctype = TRANS_BC_TYPE

    t%needmet = t%dosource

  END FUNCTION new_tdg

  ! ----------------------------------------------------------------
  !  FUNCTION tdg_source
  ! ----------------------------------------------------------------
  FUNCTION tdg_source(this, cin, pt, latc, deltat, met) RESULT (cout)

    IMPLICIT NONE
    DOUBLE PRECISION :: cout
    CLASS(tdg), INTENT(IN) :: this
    TYPE (point_transport_state), INTENT(IN) :: pt
    DOUBLE PRECISION, INTENT(IN) :: cin, latc, deltat
    TYPE (met_zone_t), INTENT(INOUT) :: met

    DOUBLE PRECISION :: area, width, twater, salinity

    cout = this%scalar_t%source(cin, pt, latc, deltat, met)
    
    IF (this%dosource) THEN
       area = pt%xsprop%area
       width = pt%xsprop%topwidth
       twater = pt%twater
       salinity = 0.0
       IF (area .GT. 0.0) THEN
          cout = cout + met%gas_exchange(twater, cout, salinity)*deltat*width/area
       END IF
    END IF
  END FUNCTION tdg_source

  ! ----------------------------------------------------------------
  ! SUBROUTINE scalar_manager_init
  ! ----------------------------------------------------------------
  SUBROUTINE scalar_manager_init(this, cfg)

    IMPLICIT NONE
    CLASS (scalar_manager), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(IN) :: cfg

    INTEGER :: i

    IF (cfg%do_transport) THEN
       this%nspecies = 2

       ALLOCATE(this%species(this%nspecies))
       DO i = 1, this%nspecies
          NULLIFY(this%species(i)%p)
       END DO
       IF (cfg%do_gas) THEN
          ALLOCATE(this%species(1)%p, &
               &SOURCE=tdg((cfg%units .EQ. METRIC_UNITS), &
               &           cfg%gas_diffusion, &
               &           cfg%do_latflow, cfg%gas_exchange))
       END IF
       IF (cfg%do_temp) THEN
          ALLOCATE(this%species(1)%p, &
               &SOURCE=temperature((cfg%units .EQ. METRIC_UNITS), &
               &                    cfg%gas_diffusion, &
               &                    cfg%do_latflow, cfg%gas_exchange))
       END IF
    ELSE
       this%nspecies = 0
    END IF
       
  END SUBROUTINE scalar_manager_init


END MODULE scalar_module


