
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
  USE mass1_config
  USE bctable
  USE link_vars, ONLY: linktype
  
  IMPLICIT NONE

  SELECT CASE(config%time%option)
  CASE(DECIMAL_TIME_OPTION)
     CALL error_message("Time option not supported in link_bc", .TRUE.)
  CASE(DATE_TIME_OPTION)
     latflowbc => bc_table_read(config%lateral_file, 1)
  END SELECT

END SUBROUTINE latflow_bc

