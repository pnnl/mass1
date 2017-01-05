  ! ----------------------------------------------------------------
  ! file: cross_section_test.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created January  3, 2017 by William A. Perkins
  ! Last Change: 2017-01-05 11:29:46 d3g096
  ! ----------------------------------------------------------------
PROGRAM cross_section_test
  USE utility
  USE cross_section
  USE section_handler_module
  IMPLICIT NONE

  TYPE (section_handler) :: sections
  CLASS (xsection_t), POINTER :: x

  sections = section_handler()

  CALL sections%read("cross_section_test.dat")

  WRITE (6, *) "A total of ", sections%size(), " sections were read"

  x => sections%find(10000)
  
  IF (ASSOCIATED(x)) THEN
     WRITE (6, *) 'Found section ', x%id
  END IF

  CALL sections%write_geometry("cross_section_test.out")
  CALL sections%destroy()

END PROGRAM cross_section_test
