! ----------------------------------------------------------------
! file: mass1_met_add_lw.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October  4, 2019 by William A. Perkins
! Last Change: 2019-10-04 12:37:37 d3g096
! ----------------------------------------------------------------
PROGRAM met_add_lw
  USE utility
  USE met_zone
  USE met_time_series
  USE energy_flux

  IMPLICIT NONE

  INTEGER, PARAMETER :: iunit = 5, ounit = 6
  DOUBLE PRECISION :: coeff(met_ncoeff) = &
       &(/0.46, 9.2, 0.47, 0.80/)
  CHARACTER (LEN=1024) :: buf
  CHARACTER (LEN=20) :: datestr, timestr
  TYPE (met_data) :: met
  DOUBLE PRECISION :: lwrad

  utility_error_iounit = ounit
  utility_status_iounit = ounit

  ! read and print first line
  READ(iunit, '(A)') buf
  WRITE(ounit, '(A)') TRIM(buf)
  DO
     READ(iunit, *, END=100) datestr, timestr, &
          &met%temp, met%dew, met%wind, met%bp, met%rad
     lwrad = net_longwave(coeff, met%temp, met%dew)
     WRITE(ounit, 10) datestr, timestr, met%temp, met%dew, met%wind,&
          &met%bp, met%rad, lwrad
10   FORMAT(A10, 1X, A8, 6(1X, F6.1), 1X, '/')
  END DO
100 CONTINUE


END PROGRAM MET_ADD_LW
