! ----------------------------------------------------------------
! file: trapezoid_test.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  6, 2017 by William A. Perkins
! Last Change: 2017-01-06 08:49:12 d3g096
! ----------------------------------------------------------------
PROGRAM trapezoid_test
  USE utility
  USE cross_section
  IMPLICIT NONE

  CLASS (xsection_t), POINTER :: x
  DOUBLE PRECISION, PARAMETER :: ymax = 20, dy = 0.5
  INTEGER, PARAMETER :: iunit = 5, ounit = 6
  INTEGER :: ierr

  utility_error_iounit = ounit
  utility_status_iounit = ounit

  DO WHILE (.TRUE.)
     x => read_cross_section(iunit, ierr)
     IF (.NOT. ASSOCIATED(x)) EXIT
     CALL cross_section_table(ounit, x, ymax, dy)
     CALL x%destroy()
     DEALLOCATE(x)
  END DO

END PROGRAM trapezoid_test


