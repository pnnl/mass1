
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

OPEN(fileunit(13),file=filename(13),form='unformatted', iostat=status)

IF (status .EQ. 0) THEN
   CALL status_message('Writing hot start to ' // TRIM(filename(13)))
ELSE 
   CALL error_message(TRIM(filename(13)) // ': cannot open for writing', fatal=.TRUE.)
END IF

DO link=1,maxlinks
DO point=1,maxpoints(link)
WRITE(fileunit(13))link,point,q(link,point),y(link,point),species(1)%conc(link,point),species(2)%conc(link,point)
END DO
END DO

CLOSE(fileunit(13))

END SUBROUTINE write_restart
