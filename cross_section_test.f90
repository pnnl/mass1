  ! ----------------------------------------------------------------
  ! file: cross_section_test.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created January  3, 2017 by William A. Perkins
  ! Last Change: 2017-01-03 14:55:12 d3g096
  ! ----------------------------------------------------------------
PROGRAM cross_section_test
  USE cross_section
  IMPLICIT NONE

  TYPE (rectangular_section), TARGET :: r1
  TYPE (rectangular_flood_section), TARGET :: r2
  CLASS (xsection_t), POINTER :: x
  DOUBLE PRECISION :: d, ks, a, R, W, K, dkdy
  DOUBLE PRECISION :: n
  INTEGER :: i

  n = 0.024
  ks = 1.0/n

  r1%id = 1
  r1%bottom_width = 10.0
  r2%id = 2
  r2%bottom_width = 10.0
  r2%depth_main = 5.0
  r2%bottom_width_flood = 50.0

  x => r2

  DO i = 1,20 
     d = REAL(i)
     CALL x%props(d, ks, a, R, W, K, dkdy)
     WRITE (*,1) x%id, d, ks, a, R, W, K, dkdy
  END DO
1 FORMAT(I2,7(X,F8.1))
END PROGRAM cross_section_test
