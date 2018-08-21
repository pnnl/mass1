! ----------------------------------------------------------------
! file: mass1_new.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 12, 2017 by William A. Perkins
! Last Change: 2018-08-17 11:53:18 d3g096
! ----------------------------------------------------------------
PROGRAM mass1
  USE utility
  USE date_time
  USE time_series
  USE network_module
  IMPLICIT NONE

  TYPE (network) :: thenet
  thenet = network()

  utility_error_iounit = 11
  utility_status_iounit = 99

  CALL date_time_flags()
  CALL time_series_module_init()

  ! open the status file - this file records progress through the
  ! input stream and errors
  !
  CALL open_new('status.out', utility_status_iounit)
  CALL open_new('error-warning.out', utility_error_iounit)

  CALL banner()

  CALL thenet%read(".")
  CALL thenet%initialize()
  CALL thenet%run()
  CALL thenet%destroy()

  CLOSE(utility_error_iounit)
  CLOSE(utility_status_iounit)

END PROGRAM mass1
