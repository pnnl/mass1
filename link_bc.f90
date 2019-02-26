
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
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
  USE bctable
  USE general_vars, ONLY: maxlinks
  USE link_vars, ONLY: linktype
  USE file_vars, ONLY: filename
  USE date_vars, ONLY: time_option

  IMPLICIT NONE

  INTEGER :: link

  SELECT CASE(time_option)

  CASE(1) ! time units are sec,hours,minutes, or days in file

     CALL error_message("Time option not supported in link_bc", .TRUE.)

  CASE(2) ! date/time format is used mm:dd:yyyy hh:mm:ss converted to decimal julian day

     linkbc => bc_table_read(filename(5), 1)

     DO link=1,maxlinks
        SELECT CASE(linktype(link)) 
        CASE(6,21)
           hydrobc => bc_table_read(filename(10), 2)
           EXIT
        END SELECT
     END DO
     
  END SELECT

END SUBROUTINE link_bc
