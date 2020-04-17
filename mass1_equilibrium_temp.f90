! ----------------------------------------------------------------
! file: mass1_equilibrium_temp.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April 15, 2020 by  William Perkins 
! Last Change: 2020-04-15 13:02:54 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! PROGRAM mass1_equilibrium_temp
! ----------------------------------------------------------------
PROGRAM mass1_equilibrium_temp

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
  DOUBLE PRECISION :: lwrad, teq

  utility_error_iounit = ounit
  utility_status_iounit = ounit

  teq = 5.0

  ! read and print first line
  READ(iunit, '(A)') buf
  WRITE(ounit, '(A)') TRIM(buf)
  DO
     READ(iunit, *, END=100) datestr, timestr, &
          &met%temp, met%dew, met%wind, met%bp, met%rad
     lwrad = atm_longwave(coeff, met%temp, met%dew)
     teq = equilibrium_temperature(coeff, met%rad, teq, met%temp, &
          &met%dew, met%wind, lwrad)
     WRITE(ounit, 10) datestr, timestr, met%temp, met%dew, met%wind,&
          &met%bp, met%rad, lwrad, teq
10   FORMAT(A10, 1X, A8, 7(1X, F6.1), 1X, '/')
  END DO
100 CONTINUE



END PROGRAM mass1_equilibrium_temp
