
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	link_bc
!
! VERSION and DATE: MASS1 v0.81 11/10/98
!
! PURPOSE: reads a data file containing the link information to
!          define a system
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
! MOD HISTORY:	added latflowbc_table read; mcr 3/25/98
!								added tempbc, met_zone; mcr 11/10/98
!
!
!***************************************************************
!

SUBROUTINE link_data

! read in link-related input data
!
! input_option == 1 is point-style; properties set for each point
! input_option == 2 is link-style; quick, uniform properties on each link


USE link_vars
USE general_vars, ONLY : maxlinks
USE file_vars

IMPLICIT NONE

INTEGER :: i,link,junk


OPEN(fileunit(2),file=filename(2))

DO i=1,maxlinks

   READ(fileunit(2),*)link
   BACKSPACE(fileunit(2))

   linkname(link) = link   

   READ(fileunit(2),*)junk,input_option(link),maxpoints(link),linkorder(link),&
        & linktype(link),num_con_links(link),linkbc_table(link),dsbc_table(link), &
        & transbc_table(link),tempbc_table(link),met_zone(link),latflowbc_table(link)
   READ(fileunit(2),*)ds_conlink(link),con_links(link,:)
        
END DO

CLOSE(fileunit(2))

END SUBROUTINE link_data
