! ----------------------------------------------------------------
! PROGRAM normal_depth_test
! ----------------------------------------------------------------
PROGRAM normal_depth_test

  USE utility
  USE cross_section

  IMPLICIT NONE

  CLASS(xsection_t), POINTER :: x
  DOUBLE PRECISION, PARAMETER :: res_coeff = 1.49
  DOUBLE PRECISION, PARAMETER :: kstrick = res_coeff/0.016, slope = 0.0001, q = 800.0
  DOUBLE PRECISION, PARAMETER :: dmin = 1.0, dmax = 64.0, dstep = 2.0
  INTEGER, PARAMETER :: iunit = 5, ounit = 6

  DOUBLE PRECISION :: dguess, d, a, dinv, qout
  TYPE (xsection_prop) :: prop
  INTEGER :: ierr

  utility_error_iounit = ounit
  utility_status_iounit = ounit

  DO WHILE (.TRUE.)
     x => read_cross_section(iunit, ierr)
     IF (.NOT. ASSOCIATED(x)) EXIT
     WRITE(ounit, '("########### Section ", I4, " ###########")') x%ID
     dguess = dmin
     DO WHILE (dguess .LE. dmax)
        d = x%normal_depth(q, slope, kstrick, dguess)
        a = x%area(d)
        dinv = x%invarea(a)
        CALL x%props(dinv, prop)
        qout = prop%area*kstrick*SQRT(slope)*prop%hydrad**(2.0/3.0)
        WRITE(*, 100) dguess, d, a, dinv, qout
        dguess = dguess*dstep
     END DO
     CALL x%destroy()
     DEALLOCATE(x)
  END DO

100 FORMAT('dguess = ', F8.2, ' dnormal = ', F8.2, &
         &', a = ', F8.2, ', d = ', F8.2, ", q = ", F8.2)

END PROGRAM normal_depth_test
