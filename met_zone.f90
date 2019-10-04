! ----------------------------------------------------------------
! file: met_zone.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 21, 2017 by William A. Perkins
! Last Change: 2019-10-04 13:02:02 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE met_zone
! ----------------------------------------------------------------
MODULE met_zone

  USE utility
  USE met_time_series
  USE dlist_module
  USE gas_functions
  USE tdg_equation_coeff
  USE energy_flux

  IMPLICIT NONE

  PRIVATE 

  INTEGER, PARAMETER, PUBLIC :: met_ncoeff = 4

  ! ----------------------------------------------------------------
  ! TYPE met_data
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: met_data
     DOUBLE PRECISION :: temp
     DOUBLE PRECISION :: dew
     DOUBLE PRECISION :: wind
     DOUBLE PRECISION :: bp
     DOUBLE PRECISION :: rad
     DOUBLE PRECISION :: lwrad
  END type met_data

  ! ----------------------------------------------------------------
  ! TYPE met_zone_t
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: met_zone_t
     INTEGER :: id
     LOGICAL :: dolw
     DOUBLE PRECISION :: coeff(met_ncoeff) 
     TYPE (met_data) :: current
     TYPE (met_time_series_rec), POINTER :: met
   CONTAINS
     PROCEDURE :: update => met_zone_update
     PROCEDURE :: energy_flux => met_zone_energy_flux
     PROCEDURE :: gas_exchange => met_zone_gas_exchange
     PROCEDURE :: destroy => met_zone_destroy
  END type met_zone_t

  INTERFACE met_zone_t
     MODULE PROCEDURE new_met_zone
  END INTERFACE met_zone_t

  ! ----------------------------------------------------------------
  ! TYPE met_zone_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: met_zone_ptr
     TYPE (met_zone_t), POINTER :: p
  END type met_zone_ptr

  ! ----------------------------------------------------------------
  ! met_zone_list
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(dlist) :: met_zone_list
   CONTAINS
     PROCEDURE :: push => met_list_push
     PROCEDURE :: pop => met_list_pop
     PROCEDURE :: clear => met_list_clear
     PROCEDURE :: find => met_list_find
     PROCEDURE :: update => met_list_update
     PROCEDURE :: current => met_list_current
  END type met_zone_list

  INTERFACE met_zone_list
     MODULE PROCEDURE new_met_zone_list
  END INTERFACE met_zone_list

  ! ----------------------------------------------------------------
  ! TYPE met_zone_manager
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: met_zone_manager_t
     TYPE (met_zone_list), POINTER :: zonelist
   CONTAINS 
     PROCEDURE :: read => met_zone_manager_read
     PROCEDURE :: find => met_zone_manager_find
     PROCEDURE :: size => met_zone_manager_size
     PROCEDURE :: update => met_zone_manager_update
     PROCEDURE :: destroy => met_zone_manager_destroy
  END type met_zone_manager_t

  INTERFACE met_zone_manager_t
     MODULE PROCEDURE new_met_zone_manager
  END INTERFACE met_zone_manager_t

  PUBLIC new_met_zone_manager

  TYPE (met_zone_manager_t), PUBLIC :: met_zone_manager

  ! air-water gas exchange coefficient parameters
  DOUBLE PRECISION, PUBLIC :: gasx_a = 0.0, gasx_b = 0.0 , gasx_c = 0.0, gasx_d = 0.0

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_met_zone
  ! ----------------------------------------------------------------
  FUNCTION new_met_zone(id, fname, dolw)
    IMPLICIT NONE
    TYPE (met_zone_t) :: new_met_zone
    INTEGER, INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: fname
    LOGICAL, INTENT(IN), OPTIONAL :: dolw
    new_met_zone%id = id
    new_met_zone%dolw = .FALSE.
    IF (PRESENT(dolw)) THEN
       new_met_zone%dolw = dolw
    END IF
    new_met_zone%met => met_time_series_read(fname, dolw)
  END FUNCTION new_met_zone

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_update
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_update(this, datetime)
    IMPLICIT NONE
    CLASS (met_zone_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: datetime
    CALL met_time_series_update(this%met, datetime)
    this%current%temp = this%met%current(MET_AIRT)
    this%current%dew = this%met%current(MET_DEWT)
    this%current%wind = this%met%current(MET_WIND)
    this%current%bp = this%met%current(MET_BARO)
    this%current%rad = this%met%current(MET_SWRAD)
    IF (this%dolw) THEN
       this%current%lwrad = this%met%current(MET_LWRAD)
    END IF
  END SUBROUTINE met_zone_update

  ! ----------------------------------------------------------------
  !  FUNCTION met_zone_energy_flux
  ! ----------------------------------------------------------------
  FUNCTION met_zone_energy_flux(this, twater) RESULT(flux)
    IMPLICIT NONE
    CLASS (met_zone_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: twater
    DOUBLE PRECISION :: flux

    IF (this%dolw) THEN
       flux = net_heat_flux(this%coeff, this%current%rad, &
            &twater, this%current%temp, this%current%dew, &
            &this%current%wind, this%current%lwrad)
    ELSE
       flux = net_heat_flux(this%coeff, this%current%rad, &
            &twater, this%current%temp, this%current%dew, &
            &this%current%wind)
    END IF
    
    ! FIXME: Metric units
    flux = flux/(1000.0*4186.0/3.2808) ! rho*specifc heat*depth in feet

    ! WRITE(*,*) this%current%rad, this%current%temp, this%current%dew, &
    !      &this%current%wind
    
  END FUNCTION met_zone_energy_flux

  ! ----------------------------------------------------------------
  !  FUNCTION met_zone_gas_exchange
  ! ----------------------------------------------------------------
  FUNCTION met_zone_gas_exchange(this, twater, cwater, salinity) RESULT(flux)
    IMPLICIT NONE
    DOUBLE PRECISION :: flux
    CLASS (met_zone_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: twater, cwater, salinity
    DOUBLE PRECISION :: cstar
    DOUBLE PRECISION :: baro_press, windspeed, coeff

    baro_press = this%current%bp
    windspeed = this%current%wind

    ! saturation concentration
    cstar = TDGasConc(baro_press, twater, salinity)

    coeff = &
         &gasx_a + &
         &gasx_b*windspeed + &
         &gasx_c*windspeed**2 + &
         &gasx_d*windspeed**3
    flux = coeff*(cstar - cwater)*3.2808/86400.0
  END FUNCTION met_zone_gas_exchange

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_destroy(this)
    IMPLICIT NONE
    CLASS (met_zone_t), INTENT(OUT) :: this

    CALL met_time_series_destroy(this%met)

  END SUBROUTINE met_zone_destroy

  ! ----------------------------------------------------------------
  ! FUNCTION new_met_zone_list
  ! ----------------------------------------------------------------
  FUNCTION new_met_zone_list() RESULT(list)
    IMPLICIT NONE
    TYPE (met_zone_list) :: list
    NULLIFY(list%head)
    NULLIFY(list%tail)
  END FUNCTION new_met_zone_list

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_list_push
  ! ----------------------------------------------------------------
  SUBROUTINE met_list_push(this, zone)
    IMPLICIT NONE
    CLASS (met_zone_list), INTENT(INOUT) :: this
    TYPE (met_zone_t), INTENT(IN), POINTER :: zone
    TYPE (met_zone_ptr), POINTER :: ptr
    CLASS (*), POINTER :: p

    ALLOCATE(ptr)
    ptr%p => zone
    p => ptr
    CALL this%genpush(p)

  END SUBROUTINE met_list_push


  ! ----------------------------------------------------------------
  !  FUNCTION met_list_pop
  ! ----------------------------------------------------------------
  FUNCTION met_list_pop(this) RESULT(zone)
    IMPLICIT NONE
    TYPE (met_zone_t), POINTER :: zone
    CLASS (met_zone_list), INTENT(INOUT) :: this
    TYPE (met_zone_ptr), POINTER :: ptr
    CLASS (*), POINTER :: p

    NULLIFY(zone)
    p => this%genpop()
    IF (ASSOCIATED(p)) THEN
       SELECT TYPE (p)
       TYPE IS (met_zone_ptr)
          ptr => p
          zone => ptr%p
          DEALLOCATE(ptr)
       END SELECT
    END IF
  END FUNCTION met_list_pop

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_list_clear
  ! ----------------------------------------------------------------
  SUBROUTINE met_list_clear(this)
    IMPLICIT NONE
    CLASS (met_zone_list), INTENT(INOUT) :: this
    TYPE (met_zone_t), POINTER :: zone

    DO WHILE (.TRUE.)
       zone => this%pop()
       IF (ASSOCIATED(zone)) THEN
          CALL zone%destroy()
          DEALLOCATE(zone)
       ELSE 
          EXIT
       END IF
    END DO
  END SUBROUTINE met_list_clear

  ! ----------------------------------------------------------------
  !  FUNCTION met_list_find
  ! ----------------------------------------------------------------
  FUNCTION met_list_find(this, id) RESULT (zone)
    IMPLICIT NONE
    TYPE (met_zone_t), POINTER :: zone
    CLASS (met_zone_list), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: id
    
    CALL this%begin()
    zone => this%current()
    DO WHILE (ASSOCIATED(zone)) 
       IF (zone%id .EQ. id) THEN
          EXIT
       END IF
       CALL this%next()
       zone => this%current()
    END DO
  END FUNCTION met_list_find

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_list_update
  ! ----------------------------------------------------------------
  SUBROUTINE met_list_update(this, datetime)
    IMPLICIT NONE
    CLASS (met_zone_list), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: datetime

    TYPE (met_zone_t), POINTER :: zone
    
    CALL this%begin()
    zone => this%current()
    DO WHILE (ASSOCIATED(zone)) 
       CALL zone%update(datetime)
       CALL this%next()
       zone => this%current()
    END DO
  END SUBROUTINE met_list_update

  ! ----------------------------------------------------------------
  !  FUNCTION met_list_current
  ! ----------------------------------------------------------------
  FUNCTION met_list_current(this) RESULT (zone)
    IMPLICIT NONE
    TYPE (met_zone_t), POINTER :: zone
    CLASS (met_zone_list), INTENT(INOUT) :: this
    TYPE (met_zone_ptr), POINTER :: ptr
    CLASS (*), POINTER :: p

    NULLIFY(zone)
    IF (ASSOCIATED(this%cursor)) THEN
       p => this%cursor%data
       IF (ASSOCIATED(p)) THEN
          SELECT TYPE (p)
          TYPE IS (met_zone_ptr)
             ptr => p
             zone => ptr%p
          END SELECT
       END IF
    END IF
  END FUNCTION met_list_current



  ! ----------------------------------------------------------------
  !  FUNCTION new_met_zone_manager
  ! ----------------------------------------------------------------
  FUNCTION new_met_zone_manager() RESULT(man)
    IMPLICIT NONE
    TYPE (met_zone_manager_t) :: man
    ALLOCATE(man%zonelist)
    man%zonelist = met_zone_list()
  END FUNCTION new_met_zone_manager

  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_manager_read
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_manager_read(this, met_files)
    IMPLICIT NONE
    CLASS (met_zone_manager_t), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: met_files
    
    CHARACTER(LEN=1024) :: msg
    INTEGER, PARAMETER :: iounit1 = 50, iounit2 = 51
    TYPE (met_zone_t), POINTER :: zone
    INTEGER :: zoneid, dolw
    CHARACTER(LEN=1024) :: zonefile
    DOUBLE PRECISION :: coeff(4)
    INTEGER :: line

    CALL open_existing(met_files, iounit1, fatal=.TRUE.)

    line = 0
    DO WHILE(.TRUE.)

       ! Default coefficients for this met zone

       coeff(1) = 0.46 ! wind function multiplier
       coeff(2) = 9.2  ! wind function offset
       coeff(3) = 0.47 ! conduction coefficient
       coeff(4) = 0.65 ! "brunt" coefficient for lw atm radiation
       dolw = 0        ! if > 0, met data file contains net incoming long wave

       READ(iounit1,*,END=200, ERR=300) zoneid, zonefile, &
            &coeff(1), coeff(2), coeff(3), coeff(4), dolw
       line = line + 1

       ALLOCATE(zone)
       zone = met_zone_t(zoneid, zonefile, (dolw .GT. 0) )
       zone%coeff = coeff
       CALL this%zonelist%push(zone)
       

       WRITE (msg, *) 'Coefficients for weather zone ', zone%id
       CALL status_message(msg)
       WRITE (msg, *) '        Wind a = ' , zone%coeff(1)
       CALL status_message(msg)
       WRITE (msg, *) '        Wind b = ' , zone%coeff(2)
       CALL status_message(msg)
       WRITE (msg, *) '    Conduction = ' , zone%coeff(3)
       CALL status_message(msg)
       WRITE (msg, *) '         Brunt = ' , zone%coeff(4)
       CALL status_message(msg)
    END DO

    CLOSE(iounit1)

200 CONTINUE
    RETURN
300 CONTINUE
    WRITE (msg, *) TRIM(met_files), ": line ", line, ": read error"
    CALL error_message(msg, FATAL=.TRUE.)

  END SUBROUTINE met_zone_manager_read

  ! ----------------------------------------------------------------
  !  FUNCTION met_zone_manager_find
  ! ----------------------------------------------------------------
  FUNCTION met_zone_manager_find(this, id) RESULT (zone)
    IMPLICIT NONE
    TYPE (met_zone_t), POINTER :: zone
    CLASS (met_zone_manager_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: id

    NULLIFY(zone)
    zone => this%zonelist%find(id)
  END FUNCTION met_zone_manager_find

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION met_zone_manager_size
  ! ----------------------------------------------------------------
  INTEGER FUNCTION met_zone_manager_size(this)
    IMPLICIT NONE
    CLASS (met_zone_manager_t), INTENT(IN) :: this
    met_zone_manager_size = this%zonelist%size()
  END FUNCTION met_zone_manager_size


  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_manager_update
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_manager_update(this, datetime)
    IMPLICIT NONE
    CLASS (met_zone_manager_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: datetime
    CALL this%zonelist%update(datetime)
  END SUBROUTINE met_zone_manager_update


  ! ----------------------------------------------------------------
  ! SUBROUTINE met_zone_manager_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE met_zone_manager_destroy(this)
    IMPLICIT NONE
    CLASS (met_zone_manager_t), INTENT(INOUT) :: this
    CALL this%zonelist%clear()
  END SUBROUTINE met_zone_manager_destroy



END MODULE met_zone
