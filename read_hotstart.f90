
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	read_hotstart
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE:reads a binary hotstart file to start model run from
!         a previous saved restart file.
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

SUBROUTINE read_hotstart

USE utility
USE link_vars
USE general_vars
USE point_vars
USE file_vars
USE transport_vars
USE scalars
USE logicals, ONLY : file_exist

IMPLICIT NONE

INTEGER :: link,point,i,j

!OPEN(91,file='hotstart.dat',form='binary')
CALL open_existing(filename(12), fileunit(12), fatal=.TRUE.)

DO link=1,maxlinks
DO point=1,maxpoints(link)
READ(fileunit(12))i,j,q(link,point),y(link,point),species(1)%conc(link,point),species(2)%conc(link,point)
END DO
END DO

lateral_inflow = 0.0
lateral_inflow_old = 0.0

CLOSE(fileunit(12))


END SUBROUTINE read_hotstart
