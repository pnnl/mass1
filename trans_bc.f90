
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	transport_bc
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: read in transport (gas and temp) BCs at input nodes
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:	need to spily this up into a separate file for
!           each constituent...
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

SUBROUTINE transport_bc(species_num)

  USE utility
  USE bctable
  USE file_vars, ONLY: filename
  USE general_vars, ONLY: units
  USE date_vars, ONLY: time_option

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: species_num

  SELECT CASE(time_option)
     
  CASE(1) ! time units are sec,hours,minutes, or days in file
     
     CALL error_message("Time option not supported in trans_bc", .TRUE.)

  CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

     SELECT CASE(species_num)
     CASE(1) ! species = 1 is total dissolved gas

        transbc => bc_table_read(filename(9), 1)

     CASE(2) !speices = 2 is Temperature

        tempbc => bc_table_read(filename(17), 1)

     END SELECT

  END SELECT

END SUBROUTINE transport_bc
