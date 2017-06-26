! ----------------------------------------------------------------
! file: link_test.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June 26, 2017 by William A. Perkins
! Last Change: 2017-06-26 13:47:04 d3g096
! ----------------------------------------------------------------
PROGRAM link_test  
  USE link_module
  IMPLICIT NONE

  TYPE (link_manager_t) :: lman
  lman = new_link_manager()

END PROGRAM link_test
