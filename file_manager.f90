
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	file_manager
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE:	in a non-GUI version this reads the input files from
!  a master file list stored in 'files.dat'
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

SUBROUTINE file_manager

USE file_vars

IMPLICIT NONE

INTEGER :: count=3

OPEN(10,file='files.dat')

DO WHILE(.TRUE.)

	READ(10,*, END=100)filename(count)

	count = count + 1

END DO

100 CLOSE(10)


END SUBROUTINE file_manager
