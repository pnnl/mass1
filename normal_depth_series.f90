  ! ----------------------------------------------------------------
  ! file: normal_depth_series.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created February  3, 2020 by William A. Perkins
  ! Last Change: 2020-02-03 14:56:36 d3g096
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! PROGRAM normal_depth_series
!
! Compute normal depth for a cross sections and a time series of
! discharges.
! ----------------------------------------------------------------
PROGRAM normal_depth_series

  USE utility
  USE cross_section

  IMPLICIT NONE

  CLASS(xsection_t), POINTER :: x
  DOUBLE PRECISION, PARAMETER :: res_coeff = 1.00
  DOUBLE PRECISION :: kstrick, manning, slope
  INTEGER, PARAMETER :: iunit = 5, ounit = 6

  DOUBLE PRECISION :: dguess, d, a, dinv, q, qout
  TYPE (xsection_prop) :: prop
  INTEGER :: ierr
  CHARACTER (LEN=256) :: label

  utility_error_iounit = ounit
  utility_status_iounit = ounit

  x => read_cross_section(iunit, ierr)
  IF (.NOT. ASSOCIATED(x)) THEN
     CALL error_message("Cross section not understood", fatal=.TRUE.)
  END IF
  READ (iunit, *) slope, manning
  kstrick = res_coeff/manning
  dguess = 1.0

  DO WHILE (.TRUE.)

     READ(iunit, *, IOSTAT=ierr) label, q
     IF (ierr .NE. 0) EXIT

     d = x%normal_depth(q, slope, kstrick, dguess)
     a = x%area(d)
     dinv = x%invarea(a)
     CALL x%props(dinv, prop)
     qout = prop%area*kstrick*SQRT(slope)*prop%hydrad**(2.0/3.0)
     WRITE(*, 100) q, d, a, dinv, qout
     dguess = d
  END DO
  
  CALL x%destroy()
  DEALLOCATE(x)

100 FORMAT('qin = ', F8.2, ' dnormal = ', F8.2, &
         &', a = ', F8.2, ', d = ', F8.2, ", q = ", F8.2)
  

END PROGRAM normal_depth_series

