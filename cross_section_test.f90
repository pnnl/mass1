  ! ----------------------------------------------------------------
  ! file: cross_section_test.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created January  3, 2017 by William A. Perkins
  ! Last Change: 2017-01-04 14:51:51 d3g096
  ! ----------------------------------------------------------------
PROGRAM cross_section_test
  USE utility
  USE cross_section
  IMPLICIT NONE

  CLASS (xsection_t), POINTER :: x
  INTEGER :: iounit, ioerr 

  iounit = 52

  CALL open_existing("cross_section_test.dat", iounit, fatal=.TRUE.)

  DO WHILE (.TRUE.)
     x => read_cross_section(iounit)
     IF (.NOT. ASSOCIATED(x)) EXIT
     WRITE (6, *) "Read cross section ", x%id
     CALL x%print(6, ioerr)
  END DO
END PROGRAM cross_section_test
