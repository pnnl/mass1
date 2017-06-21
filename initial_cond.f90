
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME:	initial_cond
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: reads a set of initial conditions for each link
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

SUBROUTINE initial_cond

  USE utility
  USE general_vars
	USE file_vars
    USE point_vars
	USE transport_vars
    USE link_vars
    USE scalars, ONLY: species
    USE general_vars, ONLY : maxlinks,units

    IMPLICIT NONE


        INTEGER :: link,point, i
        DOUBLE PRECISION :: junk3,depth,junk2,junk4

        CALL open_existing(filename(6), fileunit(6))
        
        lateral_inflow = 0.0;
        lateral_inflow_old = 0.0;

        DO WHILE(.TRUE.) !dangerous, assumes just right stuff is in initial.dat

        READ(fileunit(6),*,END=100)link, junk3, depth,junk2,junk4

        DO i=1,maxpoints(link)
        
		q(link,i) = junk3
		y(link,i) = depth
		c(link,i) = junk2
		temp(link,i) = junk4

		SELECT CASE(units)
		CASE(2)
		 
		 q(link,i) = q(link,i)
		 y(link,i) = y(link,i)
		 c(link,i) = c(link,i)
		 temp(link,i) = temp(link,i)

		END SELECT

                IF (y(link, i) .LT. (thalweg(link, i) + depth_minimum)) THEN
                   q(link, i) = 0.0
                   y(link, i) = thalweg(link, i) + depth_minimum
                END IF

                species(1)%conc(link,i) = c(link,i)
                species(1)%concold(link,i) = c(link,i)
                species(2)%conc(link,i) = temp(link,i)
                species(2)%concold(link,i) = temp(link,i)
        END DO  

        END DO
!----------------------------------------------------------------------
        

100     CLOSE(fileunit(6))



END SUBROUTINE initial_cond
