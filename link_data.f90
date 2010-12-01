
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

  USE utility
  USE logicals, ONLY: do_latflow, do_gas, do_temp
  USE link_vars
  USE general_vars, ONLY : maxlinks
  USE file_vars
  USE logicals, ONLY : file_exist

  IMPLICIT NONE

  INTEGER :: i,link,junk,io_unit
  CHARACTER(LEN=1024) :: msg

  io_unit = fileunit(7)
  CALL open_existing(filename(2), fileunit(2), fatal=.TRUE.)

  CALL print_output("LINKS ")

  DO i=1,maxlinks

     READ(fileunit(2),*)link
     BACKSPACE(fileunit(2))

     linkname(link) = link   


     WRITE(io_unit,*)'link number -',link

     READ(fileunit(2),*)junk,input_option(link),maxpoints(link),linkorder(link),&
          & linktype(link),num_con_links(link),linkbc_table(link),dsbc_table(link), &
          & transbc_table(link),tempbc_table(link),met_zone(link),latflowbc_table(link), &
          & lattransbc_table(link),lattempbc_table(link)
     READ(fileunit(2),*)ds_conlink(link),con_links(link,:)

     WRITE(io_unit,*)link,input_option(link),maxpoints(link),linkorder(link),&
          & linktype(link),num_con_links(link),linkbc_table(link),dsbc_table(link), &
          & transbc_table(link),tempbc_table(link),met_zone(link),latflowbc_table(link), &
          & lattransbc_table(link),lattempbc_table(link)
     WRITE(io_unit,*)ds_conlink(link),con_links(link,:)

     IF (do_latflow .AND. do_gas) THEN 
        IF (latflowbc_table(link) .NE. 0 .AND. lattransbc_table(link) .EQ. 0) THEN
           WRITE(msg, *) "Link ", link, ": lateral inflow specified without transport (gas) values"
           CALL error_message(msg, .FALSE.)
        END IF
     ELSE IF (do_latflow .AND. do_temp) THEN
        IF (latflowbc_table(link) .NE. 0 .AND. lattempbc_table(link) .EQ. 0) THEN
           WRITE(msg, *) "Link ", link, ": lateral inflow specified without transport (temp) values"
           CALL error_message(msg, .FALSE.)
        END IF
     END IF
  END DO

  CLOSE(fileunit(2))

END SUBROUTINE link_data
