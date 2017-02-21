
!***************************************************************
!            Pacific Northwest National Laboratory
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


USE mass1_config
USE general_vars
USE link_vars
USE pidlink

IMPLICIT NONE

INTEGER :: i,j

! This stuff needs to be set in config%read()

depth_minimum = 0.001           ! m
depth_threshold = 0.01          ! m

SELECT CASE(config%units)

	CASE(ENGLISH_UNITS)
		res_coeff = 1.49   !manning unit factor used in conveyance calculation
		grav = 32.2
		unit_weight_h2o = 62.4 ! lb/ft3
		density_h2o = 1.94	   ! slugs/ft3
                depth_minimum = depth_minimum/0.3048
                depth_threshold = depth_threshold/0.3048

	CASE(METRIC_UNITS)
		res_coeff = 1.0	   !manning unit factor used in conveyance calculation
		grav = 9.81
		unit_weight_h2o = 9810.0  ! N/m3
		density_h2o = 1000.0 	  ! kg/m3

END SELECT

! Figure out the order in which to process the links according
! to the input data
!

        DO i=1,config%maxlinks
        DO j=1,config%maxlinks
          IF(linkorder(j) == i)THEN
            comporder(i) = linkname(j)   !link number to do
          ENDIF
        END DO
        END DO

CALL read_pidlink_info()
CALL pidlink_initialize()

END SUBROUTINE kick_off
