
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
USE logicals, ONLY : file_exist

IMPLICIT NONE

INTEGER :: i,link,junk,io_unit

io_unit = fileunit(7)

INQUIRE(FILE=filename(2),EXIST=file_exist)
IF(file_exist)THEN
   OPEN(fileunit(2),file=filename(2))
   WRITE(99,*)'link specification file opened: ',filename(2)
ELSE
   WRITE(*,*)'link specification file does not exist - ABORT: ',filename(2)
   WRITE(99,*)'link specification file does not exist - ABORT: ',filename(2)
   CALL EXIT
ENDIF

CALL print_output("LINKS ")

DO i=1,maxlinks

   READ(fileunit(2),*)link
   BACKSPACE(fileunit(2))

   linkname(link) = link   

  
   WRITE(io_unit,*)'link number -',link

   READ(fileunit(2),*)junk,input_option(link),maxpoints(link),linkorder(link),&
        & linktype(link),num_con_links(link),linkbc_table(link),dsbc_table(link), &
        & transbc_table(link),tempbc_table(link),met_zone(link),latflowbc_table(link)
   READ(fileunit(2),*)ds_conlink(link),con_links(link,:)

   WRITE(io_unit,*)link,input_option(link),maxpoints(link),linkorder(link),&
        & linktype(link),num_con_links(link),linkbc_table(link),dsbc_table(link), &
        & transbc_table(link),tempbc_table(link),met_zone(link),latflowbc_table(link)
   WRITE(io_unit,*)ds_conlink(link),con_links(link,:)
        
END DO

CLOSE(fileunit(2))

END SUBROUTINE link_data
