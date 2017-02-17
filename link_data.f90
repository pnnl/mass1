
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

  IMPLICIT NONE

  INTEGER :: i,link,junk,io_unit
  CHARACTER(LEN=1024) :: msg

  io_unit = fileunit(7)
  CALL open_existing(filename(2), fileunit(2), fatal=.TRUE.)

  CALL print_output("LINKS ")

  DO i=1,maxlinks

     READ(fileunit(2),*,END=100,ERR=200)link
     BACKSPACE(fileunit(2))

     linkname(link) = link   

     ! FIXME: the following fields are now ignored, and computed elsewhere:
     !
     ! linkorder(link), num_con_links(link), con_links(link,:)
     !
     ! the format needs to be reworked


     WRITE(io_unit,*)'link number -',link

     READ(fileunit(2),*,ERR=200)junk,input_option(link),maxpoints(link),linkorder(link),&
          & linktype(link),num_con_links(link),linkbc_table(link),dsbc_table(link), &
          & transbc_table(link),tempbc_table(link),met_zone(link),latflowbc_table(link), &
          & lattransbc_table(link),lattempbc_table(link),lpiexp(link)
     READ(fileunit(2),*,ERR=200)ds_conlink(link),con_links(link,:)

     WRITE(io_unit,*)link,input_option(link),maxpoints(link),linkorder(link),&
          & linktype(link),num_con_links(link),linkbc_table(link),dsbc_table(link), &
          & transbc_table(link),tempbc_table(link),met_zone(link),latflowbc_table(link), &
          & lattransbc_table(link),lattempbc_table(link),lpiexp(link)
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

100 CONTINUE

  CLOSE(fileunit(2))

  CALL link_connect

  RETURN

200 CONTINUE

  WRITE(msg, *) TRIM(filename(2)) // ': error in or near link record ', link, ' of ', maxlinks
  CALL error_message(msg, fatal=.TRUE.)

END SUBROUTINE link_data

! ----------------------------------------------------------------
! INTEGER FUNCTION link_set_order
! ----------------------------------------------------------------
RECURSIVE FUNCTION link_set_order(link, order0) RESULT(order)

  USE link_vars

  IMPLICIT NONE
  INTEGER :: order
  INTEGER, INTENT(IN) :: link, order0
  INTEGER :: i, ulink, o
  
  o = order0

  DO i = 1, num_con_links(link)
     ulink = con_links(link, i)
     o = link_set_order(ulink, o)
  END DO
  linkorder(link) = o
  order = o + 1
END FUNCTION link_set_order

! ----------------------------------------------------------------
! SUBROUTINE link_connect
! ----------------------------------------------------------------
SUBROUTINE link_connect()

  USE utility
  USE link_vars
  USE general_vars, ONLY : maxlinks

  IMPLICIT NONE

  CHARACTER(LEN=1024) :: msg, lbl
  INTEGER :: link, dlink, ulink, i
  INTEGER :: nds
  LOGICAL :: done
  INTEGER :: ierr
  INTEGER :: link_set_order

  nds = 0
  ierr = 0

  CALL status_message("Connecting Links ...")

  DO link = 1, maxlinks
     num_con_links(link) = 0  
     con_links(link,:) = 0
  END DO

  DO link = 1, maxlinks

     dlink = ds_conlink(link)
     IF (dlink .NE. 0) THEN 
        ! Is specified downstream link id valid?
        IF (dlink .LT. 1 .OR. dlink .GT. maxlinks) THEN
           WRITE(msg, '("link , I4, : invalid downstream link id (",I4,")")')&
                & link, dlink
           CALL error_message(msg)
           ierr = ierr + 1
        END IF
        i = num_con_links(dlink)
        i = i + 1
        con_links(dlink,i) = link
        num_con_links(dlink) = i
     ELSE 
        nds = nds + 1
     END IF
     
  END DO

  IF (nds .EQ. 0) THEN
     CALL error_message("No downstream link found, there must be one")
     ierr = ierr + 1
  ELSE IF (nds .GT. 1) THEN
     CALL error_message("Too many ownstream links found, there can be only one")
     ierr = ierr + 1
  END IF

  IF (ierr .GT. 0) THEN
     CALL error_message("Network connectivity errors, cannot continue", fatal=.TRUE.)
  END IF

  ! determine computational order: 

  linkorder(:) = 0
  DO link = 1, maxlinks
     IF (ds_conlink(link) .EQ. 0) THEN
        i = link_set_order(link, 1)
        EXIT
     END IF
  END DO


  ! minorder = 1
  ! DO link = 1, maxlinks
  !    IF (num_con_links(link) .EQ. 0) THEN
  !       linkorder(link) = minorder
  !       minorder = minorder + 1
  !    END IF
  ! END DO

  ! ! all other links need to have a higher order than in (1)
  ! DO link = 1, maxlinks
  !    IF (num_con_links(link) .NE. 0) THEN
  !       linkorder(link) = minorder
  !    END IF
  ! END DO

  ! ! (2) make sure connected links' order is 1 higher than those above

  ! done = .FALSE.
  ! DO WHILE (.NOT. done) 
  !    done = .TRUE.
  !    DO link = 1, maxlinks
  !       DO i = 1, num_con_links(link)
  !          ulink = con_links(link, i)
  !          IF (linkorder(ulink) .GE. linkorder(link)) THEN
  !             linkorder(link) = linkorder(ulink) + 1
  !             done = .FALSE.
  !          END IF
  !       END DO
  !    END DO
  ! END DO


  ! spit out connectivity and order information 

  DO link = 1, maxlinks
     WRITE(msg, '("link ", I4, "(order = ", I4, ") upstream links:")') &
          &linkname(link), linkorder(link)
     IF (num_con_links(link) .GT. 0) THEN
        DO i = 1, num_con_links(link)
           WRITE(lbl, '(I4)') con_links(link, i)
           msg = TRIM(msg) // " " // TRIM(lbl)
        END DO
     ELSE 
        msg = TRIM(msg) // " none"
     END IF
     CALL status_message(msg)
  END DO

END SUBROUTINE link_connect



