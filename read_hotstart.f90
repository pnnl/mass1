
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
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
USE mass1_config
USE link_vars
USE point_vars
USE transport_vars
USE scalars

IMPLICIT NONE

INTEGER :: link,point,i,j
LOGICAL :: file_exist
INTEGER, PARAMETER :: runit = 31

!OPEN(91,file='hotstart.dat',form='binary')
INQUIRE(FILE=config%restart_load_file,EXIST=file_exist)
IF(file_exist)THEN
   OPEN(runit,file=config%restart_load_file,form='unformatted')
   WRITE(99,*)'hotstart file opened: ',config%restart_load_file
ELSE
   WRITE(*,*)'hotstart file does not exist - ABORT: ',config%restart_load_file
   WRITE(99,*)'hotstart file does not exist - ABORT: ',config%restart_load_file
   CALL EXIT(1)
ENDIF

DO link=1,config%maxlinks
DO point=1,maxpoints(link)
READ(runit)i,j,q(link,point),y(link,point),species(1)%conc(link,point),species(2)%conc(link,point)
END DO
END DO

lateral_inflow = 0.0
lateral_inflow_old = 0.0

CLOSE(runit)


END SUBROUTINE read_hotstart
