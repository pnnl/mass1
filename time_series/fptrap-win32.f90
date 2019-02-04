! ----------------------------------------------------------------
! file: fptrap-win32.f90
!
! This module is used to turn on floating point exception handling
! when using the Digitial Visual Fortran Compiler.
!
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April 14, 2004 by William A. Perkins
! Last Change: 2017-06-22 09:23:08 d3g096
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL
! ----------------------------------------------------------------
! MODULE fptrap
! ----------------------------------------------------------------
MODULE fptrap

  USE dflib
  USE utility, ONLY: status_message

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE fptrap_common
  ! ----------------------------------------------------------------
  SUBROUTINE fptrap_common()

    IMPLICIT NONE

    INTEGER(2) :: oldc, newc
    CHARACTER (LEN=1024) :: msg

    newc = #0000
    newc = IOR(newc, FPCW$INVALID)
    newc = IOR(newc, FPCW$ZERODIVIDE)
    newc = IOR(newc, FPCW$OVERFLOW)
    newc = NOT(newc)

    CALL getcontrolfpqq(oldc)

    WRITE (msg, 9000) oldc 
    CALL status_message(msg)

    oldc = IAND(oldc, newc)

    CALL setcontrolfpqq(oldc)

    CALL getcontrolfpqq(oldc)

    WRITE (msg, 9000) oldc 
    CALL status_message(msg)

9000 FORMAT ('fptrap_common (win32): current FP control word: ', Z4)
9001 FORMAT ('fptrap_common (win32): updated FP control word: ', Z4)
  END SUBROUTINE fptrap_common


END MODULE fptrap
