! ----------------------------------------------------------------
! file: network.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 10, 2017 by William A. Perkins
! Last Change: 2017-07-20 08:00:32 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE network_module
! ----------------------------------------------------------------
MODULE network_module

  USE utility
  USE mass1_config
  USE link_manager_module
  USE bc_module
  USE section_handler_module
  USE met_zone

  IMPLICIT NONE

  PRIVATE 

  TYPE, PUBLIC :: network
     CHARACTER (LEN=path_length) :: basedir
     TYPE (configuration_t) :: config
     TYPE (bc_manager_t) :: bcs
     TYPE (met_zone_manager_t) :: met
     TYPE (section_handler) :: sections
     TYPE (link_manager_t) :: links

   CONTAINS
     PROCEDURE :: readbcs => network_read_bcs
     PROCEDURE :: read => network_read
     PROCEDURE :: flow => network_flow
     PROCEDURE :: destroy => network_destroy
  END type network

  INTERFACE network
     MODULE PROCEDURE new_network
  END INTERFACE network

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_network
  ! ----------------------------------------------------------------
  FUNCTION new_network() RESULT(net)

    IMPLICIT NONE
    TYPE (network) :: net
    net%bcs = new_bc_manager()
    net%met = met_zone_manager_t()
    net%sections = new_section_handler()
    net%links = new_link_manager()

  END FUNCTION new_network


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_read_bcs
  ! ----------------------------------------------------------------
  SUBROUTINE network_read_bcs(this)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%bcs%read(LINK_BC_TYPE, this%config%linkbc_file)
    IF (this%config%do_hydro_bc) THEN
       CALL this%bcs%read(HYDRO_BC_TYPE, this%config%hydrobc_file)
    END IF
    IF(this%config%debug_print)WRITE(11,*)'link BC data done'
    
    IF(this%config%do_latflow)THEN
       CALL this%bcs%read(LATFLOW_BC_TYPE, this%config%lateral_file)
       IF(this%config%debug_print)WRITE(11,*)'lateral inflow BC data done'
    ENDIF

    IF(this%config%do_gas)THEN
       CALL bc_manager%read(TRANS_BC_TYPE, this%config%transbc_file)
       IF(this%config%debug_print) WRITE(11,*)'done reading gas transport table'

       ! CALL allocate_tdg_coeff(this%config%maxlinks,utility_status_iounit, utility_error_iounit)
       ! ! read tdg spill coefficient tables
       ! ! if linktype 6,20, or 21 is there, then open and read file
       ! DO link=1,this%config%maxlinks
       !    SELECT CASE(linktype(link)) 
       !    CASE(6,21)
       !       CALL tdg_coeff_read(utility_status_iounit, utility_error_iounit)
       !       EXIT
       !    END SELECT
       ! END DO

       IF(this%config%gas_exchange)THEN
          CALL open_existing(this%config%tdg_coeff_file, 88, fatal=.TRUE.)
          READ(88,*)gasx_a,gasx_b,gasx_c, gasx_d
          CLOSE(88)
       ENDIF
    ENDIF

    IF(this%config%do_temp)THEN
       CALL bc_manager%read(TEMP_BC_TYPE, this%config%tempbc_file)
       IF(this%config%debug_print) WRITE(11,*)'done reading temp transport table'
    ENDIF


    IF (this%config%met_required) THEN
       CALL this%met%read(this%config%weather_file)
    END IF


  END SUBROUTINE network_read_bcs


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_read

  ! ----------------------------------------------------------------
  SUBROUTINE network_read(this, base)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: base
    INTEGER :: istatus
    CHARACTER(LEN=path_length) :: cwd, mybase

    
    istatus = getcwd(cwd)       ! FIXME: GNU specfic
    IF (istatus .NE. 0) THEN 
       CALL error_message("network_read: cannot get current working directory")
    END IF
    istatus = chdir(base)
    IF (istatus .NE. 0) then
       WRITE(mybase, *) "network_read: cannot change to ", TRIM(base), " from ", TRIM(cwd)
       CALL error_message(mybase, fatal=.TRUE.)
    END IF
    istatus = getcwd(cwd)       ! FIXME: GNU specfic
    IF (istatus .NE. 0) THEN 
       CALL error_message("network_read: cannot get current working directory")
    END IF
    this%basedir = cwd

    CALL this%config%read()
    CALL this%readbcs()
    CALL this%sections%read(this%config%section_file)

  END SUBROUTINE network_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE network_initialize(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%links%connect();

  END SUBROUTINE network_initialize


  ! ----------------------------------------------------------------
  ! SUBROUTINE network_flow
  ! ----------------------------------------------------------------
  SUBROUTINE network_flow(this)
    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%links%flow_sim(this%config%time%time, this%config%time%delta_t)

  END SUBROUTINE network_flow

  ! ----------------------------------------------------------------
  ! SUBROUTINE network_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE network_destroy(this)

    IMPLICIT NONE
    CLASS (network), INTENT(INOUT) :: this

    CALL this%bcs%destroy()
    CALL this%sections%destroy()
    CALL this%links%destroy()

  END SUBROUTINE network_destroy




END MODULE network_module
  
