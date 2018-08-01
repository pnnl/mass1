! ----------------------------------------------------------------
! file: trapezoid_test.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  6, 2017 by William A. Perkins
! Last Change: 2018-07-31 14:15:08 d3g096
! ----------------------------------------------------------------
PROGRAM cross_section_table_test
  USE utility
  USE cross_section
  IMPLICIT NONE

  CLASS (xsection_t), POINTER :: x
  DOUBLE PRECISION, PARAMETER :: ymax = 100, dy = 0.5
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

END PROGRAM cross_section_table_test


