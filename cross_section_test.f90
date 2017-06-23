! ----------------------------------------------------------------
! file: cross_section_test.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  3, 2017 by William A. Perkins
! Last Change: 2017-06-23 13:51:20 d3g096
! ----------------------------------------------------------------
PROGRAM cross_section_test
  USE utility
  USE cross_section
  USE section_handler_module
  IMPLICIT NONE

  CLASS (xsection_t), POINTER :: x

  utility_error_iounit = 6
  utility_status_iounit = 6

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
