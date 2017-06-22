
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
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
  USE mass1_config
  USE link_vars

  IMPLICIT NONE

  INTEGER :: i,link,junk, metzone_idx
  INTEGER, PARAMETER :: io_unit = 26, lunit = 21
  CHARACTER(LEN=1024) :: msg

  CALL open_existing(config%link_file, lunit, fatal=.TRUE.)

  CALL print_output("LINKS ", 0.0)

  config%do_hydro_bc = .FALSE.
  DO i=1,config%maxlinks

     READ(lunit,*,END=100,ERR=200)link
     BACKSPACE(lunit)

     linkname(link) = link   

     ! FIXME: the following fields are now ignored, and computed elsewhere:
     !
     ! linkorder(link), num_con_links(link), con_links(link,:)
     !
     ! the format needs to be reworked, mainly because the second line is not needed
     ! consider moving boundary condition information to another file


     WRITE(io_unit,*)'link number -',link

     READ(lunit,*,ERR=200)junk,input_option(link),maxpoints(link),linkorder(link),&
          & linktype(link),junk,linkbc_table(link),dsbc_table(link), &
          & transbc_table(link),tempbc_table(link), metzone_idx,latflowbc_table(link), &
          & lattransbc_table(link),lattempbc_table(link),lpiexp(link)
     READ(lunit,*,ERR=200)ds_conlink(link)

     WRITE(io_unit,*)link,input_option(link),maxpoints(link),linkorder(link),&
          & linktype(link),junk,linkbc_table(link),dsbc_table(link), &
          & transbc_table(link),tempbc_table(link),metzone_idx,latflowbc_table(link), &
          & lattransbc_table(link),lattempbc_table(link),lpiexp(link)
     WRITE(io_unit,*)ds_conlink(link)

     ! See if there are any links requiring a "hydro" BC
     SELECT CASE (linktype(link))
     CASE (21, 6)
        config%do_hydro_bc = .TRUE.
     END SELECT

     IF (config%do_latflow .AND. config%do_gas) THEN 
        IF (latflowbc_table(link) .NE. 0 .AND. lattransbc_table(link) .EQ. 0) THEN
           WRITE(msg, *) "Link ", link, ": lateral inflow specified without transport (gas) values"
           CALL error_message(msg, .FALSE.)
        END IF
     END IF
     IF (config%do_latflow .AND. config%do_temp) THEN
        IF (latflowbc_table(link) .NE. 0 .AND. lattempbc_table(link) .EQ. 0) THEN
           WRITE(msg, *) "Link ", link, ": lateral inflow specified without transport (temp) values"
           CALL error_message(msg, .FALSE.)
        END IF
     END IF

     IF (config%met_required) THEN
        metzone(link)%p => met_zone_manager%find(metzone_idx)
     END IF
  END DO

100 CONTINUE

  CLOSE(lunit)

  CALL link_connect

  RETURN

200 CONTINUE

  WRITE(msg, *) TRIM(config%link_file) // ': error in or near link record ', &
       &link, ' of ', config%maxlinks
  CALL error_message(msg, fatal=.TRUE.)

END SUBROUTINE link_data

! ----------------------------------------------------------------
! INTEGER FUNCTION link_set_order
! ----------------------------------------------------------------
RECURSIVE FUNCTION link_set_order(link, order0) RESULT(order)

  USE link_vars
  USE confluence_module

  IMPLICIT NONE
  INTEGER :: order
  INTEGER, INTENT(IN) :: link, order0
  INTEGER :: i, ulink, o
  
  o = order0

  IF (ASSOCIATED(ucon(link)%p)) THEN
     DO i = 1, ucon(link)%p%n_ulink
        ulink = ucon(link)%p%ulink(i)
        o = link_set_order(ulink, o)
     END DO
  END IF
  linkorder(link) = o
  order = o + 1
END FUNCTION link_set_order

! ----------------------------------------------------------------
! SUBROUTINE link_connect
! ----------------------------------------------------------------
SUBROUTINE link_connect()

  USE utility
  USE mass1_config
  USE link_vars
  USE confluence_module

  IMPLICIT NONE

  CHARACTER(LEN=1024) :: msg, lbl
  INTEGER :: link, dlink, i
  INTEGER :: nds
  INTEGER :: ierr
  INTEGER :: link_set_order
  TYPE (confluence_t), POINTER :: con

  NULLIFY(con)
  nds = 0
  ierr = 0

  CALL status_message("Connecting Links ...")

  DO link = 1, config%maxlinks
     NULLIFY(ucon(link)%p)
     NULLIFY(dcon(link)%p)
  END DO

  DO link = 1, config%maxlinks

     dlink = ds_conlink(link)
     IF (dlink .NE. 0) THEN 
        ! Is specified downstream link id valid?
        IF (dlink .LT. 1 .OR. dlink .GT. config%maxlinks) THEN
           WRITE(msg, '("link , I4, : invalid downstream link id (",I4,")")')&
                & link, dlink
           CALL error_message(msg)
           ierr = ierr + 1
        END IF
        IF (.NOT. ASSOCIATED(ucon(dlink)%p)) THEN 
           ALLOCATE(con)
           con = confluence_t(dlink)
           ucon(dlink)%p => con
           NULLIFY(con)
        END IF
        dcon(link)%p => ucon(dlink)%p
        i = dcon(link)%p%n_ulink
        i = i + 1
        dcon(link)%p%ulink(i) = link
        dcon(link)%p%n_ulink = i
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
  DO link = 1, config%maxlinks
     IF (ds_conlink(link) .EQ. 0) THEN
        i = link_set_order(link, 1)
        EXIT
     END IF
  END DO


  ! spit out connectivity and order information 

  DO link = 1, config%maxlinks
     WRITE(msg, '("link ", I4, "(order = ", I4, ") upstream links:")') &
          &linkname(link), linkorder(link)
     IF (ASSOCIATED(ucon(link)%p)) THEN
        DO i = 1, ucon(link)%p%n_ulink
           WRITE(lbl, '(I4)') ucon(link)%p%ulink(i)
           msg = TRIM(msg) // " " // TRIM(lbl)
        END DO
     ELSE 
        msg = TRIM(msg) // " none"
     END IF
     CALL status_message(msg)
  END DO

END SUBROUTINE link_connect



