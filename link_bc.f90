
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	link_bc
!
! VERSION and DATE: MASS1 v0.81 10/8/97
!
! PURPOSE: reads a files for a table of link BCs. Upstream inflows
!          downstream stage,...
!						also read hydro project BC file
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
! MOD HISTORY: added checks for linktype=21; mcr 11/14/98
!
!
!***************************************************************
!

SUBROUTINE link_bc
  
  USE utility
  USE mass1_config
  USE bctable
  USE link_vars, ONLY: linktype

  IMPLICIT NONE

  INTEGER :: link

  SELECT CASE (config%time%option)
  CASE (DECIMAL_TIME_OPTION)
     CALL error_message("Time option not supported in link_bc", .TRUE.)
  CASE (DATE_TIME_OPTION)
     linkbc => bc_table_read(config%linkbc_file, 1)

     DO link=1,config%maxlinks
        SELECT CASE(linktype(link)) 
        CASE(6,21)
           hydrobc => bc_table_read(config%hydrobc_file, 2)
           EXIT
        END SELECT
     END DO
     
  END SELECT

END SUBROUTINE link_bc
