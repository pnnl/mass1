! ----------------------------------------------------------------
! file: fptraptest.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April 14, 2004 by William A. Perkins
! Last Change: 2017-06-22 09:23:26 d3g096
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

PROGRAM fptraptest
  
  USE fptrap

  IMPLICIT NONE 

  DOUBLE PRECISION :: x = 0.0, y

  CALL fptrap_common()

  y = 1.0/x

  WRITE (*, *) 'Should have crashed by now.'

END PROGRAM fptraptest

  
