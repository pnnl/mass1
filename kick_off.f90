
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: initial_cond
!
! VERSION and DATE: MASS1 v0.61 11/21/1997
!
! PURPOSE: does some initial problem set-up and coefficient
!          assigment. Figures out the order to process the links.
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
! MOD HISTORY: added unit_weight_h2o, density_h2o; mcr 11/21/1997
!
!
!***************************************************************
!

SUBROUTINE kick_off

! set units and time constant


USE general_vars
USE link_vars
USE date_vars
USE pidlink

IMPLICIT NONE

INTEGER :: i,j

SELECT CASE(units)

	CASE(1)
		res_coeff = 1.49   !manning unit factor used in conveyance calculation
		grav = 32.2
		unit_weight_h2o = 62.4 ! lb/ft3
		density_h2o = 1.94	   ! slugs/ft3
                depth_minimum = depth_minimum/0.3048
                depth_threshold = depth_threshold/0.3048

	CASE(2)
		res_coeff = 1.0	   !manning unit factor used in conveyance calculation
		grav = 9.81
		unit_weight_h2o = 9810.0  ! N/m3
		density_h2o = 1000.0 	  ! kg/m3

END SELECT

! Figure out the order in which to process the links according
! to the input data
!

        DO i=1,maxlinks
        DO j=1,maxlinks
          IF(linkorder(j) == i)THEN
            comporder(i) = linkname(j)   !link number to do
          ENDIF
        END DO
        END DO

! figure out time constant
! code works in seconds internally if date option = 1
! code works in decimal julian days if date_option = 2

SELECT CASE(time_option)

CASE(1)

	SELECT CASE(time_units)

	CASE(1)
        time_mult = 1.00d0     !seconds

	CASE(2)
        time_mult = 60.00d0   !minutes
	CASE(3)
        time_mult = 3600.00d0 !hours

	CASE(4)
        time_mult = 86400.00d0 !days

	END SELECT

CASE(2)
	
	time_mult = 1.00d0 ! things are converted to decimal julian day

END SELECT

CALL read_pidlink_info()
CALL pidlink_initialize()

END SUBROUTINE kick_off
