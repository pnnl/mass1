
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
INQUIRE(FILE=filename(12),EXIST=file_exist)
IF(file_exist)THEN
   OPEN(fileunit(12),file=filename(12),form='unformatted')
   WRITE(99,*)'hotstart file opened: ',filename(12)
ELSE
   WRITE(*,*)'hotstart file does not exist - ABORT: ',filename(12)
   WRITE(99,*)'hotstart file does not exist - ABORT: ',filename(12)
   CALL EXIT(1)
ENDIF

DO link=1,maxlinks
DO point=1,maxpoints(link)
READ(fileunit(12))i,j,q(link,point),y(link,point),species(1)%conc(link,point),species(2)%conc(link,point)
END DO
END DO

lateral_inflow = 0.0
lateral_inflow_old = 0.0

CLOSE(fileunit(12))


END SUBROUTINE read_hotstart
