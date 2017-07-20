! ----------------------------------------------------------------
! file: link_test.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June 26, 2017 by William A. Perkins
! Last Change: 2017-07-20 08:10:48 d3g096
! ----------------------------------------------------------------
PROGRAM link_test  
  USE link_manager_module
  IMPLICIT NONE

  TYPE (link_manager_t) :: lman
  lman = new_link_manager()

END PROGRAM link_test
