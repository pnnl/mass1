! ----------------------------------------------------------------
! PROGRAM normal_depth_test
! ----------------------------------------------------------------
PROGRAM normal_depth_test

  USE utility
  USE cross_section

  IMPLICIT NONE

  CLASS(xsection_t), POINTER :: x
  DOUBLE PRECISION, PARAMETER :: kstrick = 1.0/0.016, slope = 0.0001, q = 300.0
  DOUBLE PRECISION, PARAMETER :: dmin = 1.0, dmax = 64.0, dstep = 2.0
  INTEGER, PARAMETER :: iunit = 5, ounit = 6

  DOUBLE PRECISION :: dguess, d
  INTEGER :: ierr

  utility_error_iounit = ounit
  utility_status_iounit = ounit

  DO WHILE (.TRUE.)
     x => read_cross_section(iunit, ierr)
     IF (.NOT. ASSOCIATED(x)) EXIT
     WRITE(ounit, '("########### Section ", I4, " ###########")') x%ID
     dguess = dmin
     DO WHILE (dguess .LE. dmax)
        d = x%normal_depth(q, slope, kstrick, 1.49D00, dguess)
        WRITE(*, "('dguess = ', F8.2, ' dnormal = ', F8.2)") dguess, d
        dguess = dguess*dstep
     END DO
     CALL x%destroy()
     DEALLOCATE(x)
  END DO

END PROGRAM normal_depth_test
