
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
  USE mass1_config
  USE bctable

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: species_num

  SELECT CASE(config%time%option)
     
  CASE(DECIMAL_TIME_OPTION) ! time units are sec,hours,minutes, or days in file
     
     CALL error_message("Time option not supported in trans_bc", .TRUE.)

  CASE(DATE_TIME_OPTION) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

     SELECT CASE(species_num)
     CASE(1) ! species = 1 is total dissolved gas

        transbc => bc_table_read(config%transbc_file, 1)

     CASE(2) !speices = 2 is Temperature

        tempbc => bc_table_read(config%tempbc_file, 1)

     END SELECT

  END SELECT

END SUBROUTINE transport_bc
