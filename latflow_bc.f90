
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	latflow_bc
!
! VERSION and DATE: MASS1 v0.75 3/25/98
!
! PURPOSE: reads a file for a table of uniform lateral inflows for
!          a link BCs. 
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

SUBROUTINE latflow_bc

  USE utility
  USE bctable
  USE general_vars, ONLY: maxlinks
  USE link_vars, ONLY: linktype
  USE file_vars, ONLY: filename
  USE date_vars, ONLY: time_option
  
  IMPLICIT NONE

  SELECT CASE(time_option)

  CASE(1) ! time units are sec,hours,minutes, or days in file

     CALL error_message("Time option not supported in link_bc", .TRUE.)

  CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

     latflowbc => bc_table_read(filename(16), 1)

  END SELECT

END SUBROUTINE latflow_bc

