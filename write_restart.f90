
!***************************************************************
!            Pacific Northwest National Laboratory
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
USE general_vars
USE point_vars
USE file_vars
USE transport_vars
USE scalars
USE utility

IMPLICIT NONE

INTEGER :: link,point
INTEGER :: status

OPEN(fileunit(13), file=config%restart_save_file,form='unformatted', iostat=status)

IF (status .EQ. 0) THEN
   CALL status_message('Writing hot start to ' // TRIM(config%restart_save_file))
ELSE 
   CALL error_message(TRIM(config%restart_save_file) // ': cannot open for writing', fatal=.TRUE.)
END IF

DO link=1,config%maxlinks
DO point=1,maxpoints(link)
WRITE(fileunit(13))link,point,q(link,point),y(link,point),species(1)%conc(link,point),species(2)%conc(link,point)
END DO
END DO

CLOSE(fileunit(13))

END SUBROUTINE write_restart
