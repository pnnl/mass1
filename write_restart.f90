
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME:	write_restart
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: write a binary file of the model values at the
!          end of a simulation
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:	needs to be changed if an arbitrary link naming
!           convention is allowed.
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

SUBROUTINE write_restart

USE mass1_config
USE link_vars
USE point_vars
USE transport_vars
USE scalars
USE utility

IMPLICIT NONE

INTEGER :: link,point
INTEGER :: status
INTEGER, PARAMETER :: ounit = 32

OPEN(ounit, file=config%restart_save_file,form='unformatted', iostat=status)

IF (status .EQ. 0) THEN
   CALL status_message('Writing hot start to ' // TRIM(config%restart_save_file))
ELSE 
   CALL error_message(TRIM(config%restart_save_file) // ': cannot open for writing', fatal=.TRUE.)
END IF

DO link=1,config%maxlinks
DO point=1,maxpoints(link)
WRITE(ounit)link,point,q(link,point),y(link,point),species(1)%conc(link,point),species(2)%conc(link,point)
END DO
END DO

CLOSE(ounit)

END SUBROUTINE write_restart
