
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME: initial_cond
!
! VERSION and DATE: MASS1 v0.61 11/21/1997
!
! PURPOSE: does some initial problem set-up and coefficient
!          assigment. Figures out the order to process the links.
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
! MOD HISTORY: added unit_weight_h2o, density_h2o; mcr 11/21/1997
!
!
!***************************************************************
!

SUBROUTINE kick_off

  ! set units and time constant


  USE utility
  USE mass1_config
  USE general_vars
  USE link_vars
  USE pidlink

  IMPLICIT NONE

  INTEGER :: i,j, ierr
  TYPE (bc_ptr) :: ptr
  INTEGER(KIND(BC_ENUM)) :: bckind
  CHARACTER (LEN=1024) :: msg

  ! This stuff needs to be set in config%read()

  depth_minimum = 0.001           ! m
  depth_threshold = 0.01          ! m

  SELECT CASE(config%units)

  CASE(ENGLISH_UNITS)
     res_coeff = 1.49   !manning unit factor used in conveyance calculation
     grav = 32.2
     unit_weight_h2o = 62.4 ! lb/ft3
     density_h2o = 1.94    ! slugs/ft3
     depth_minimum = depth_minimum/0.3048
     depth_threshold = depth_threshold/0.3048

  CASE(METRIC_UNITS)
     res_coeff = 1.0       !manning unit factor used in conveyance calculation
     grav = 9.81
     unit_weight_h2o = 9810.0  ! N/m3
     density_h2o = 1000.0         ! kg/m3

  END SELECT

  ! Figure out the order in which to process the links according
  ! to the input data
  !

  DO i=1,config%maxlinks
     DO j=1,config%maxlinks
        IF(linkorder(j) == i)THEN
           comporder(i) = linkname(j)   !link number to do
        ENDIF
     END DO
  END DO

  ! look up and assign hydodynamic boundary conditions to each link
  
  ierr = 0
  DO i = 1, config%maxlinks
     SELECT CASE (linktype(i))
     CASE (1)
        bckind = LINK_BC_TYPE
     CASE (21)
        bckind = HYDRO_BC_TYPE
     END SELECT
     IF (linkbc_table(i) .GT. 0) THEN
        usbc(i)%p => bc_manager%find(bckind, linkbc_table(i))
        IF (.NOT. ASSOCIATED(usbc(i)%p)) THEN
           WRITE(msg, *) 'Link ', i, ': cannot find US BC, ', linkbc_table(i)
           CALL error_message(msg)
           ierr = ierr + 1
        END IF
     END IF

     IF (dsbc_table(i) .GT. 0) THEN 
        dsbc(i)%p => bc_manager%find(LINK_BC_TYPE, dsbc_table(i))
        IF (.NOT. ASSOCIATED(dsbc(i)%p)) THEN
           WRITE(msg, *) 'Link ', i, ': cannot find DS BC, ', dsbc_table(i)
           CALL error_message(msg)
           ierr = ierr + 1
        END IF
     END IF

     IF (config%do_latflow .AND. latflowbc_table(i) .GT. 0)  THEN
        latbc(i)%p => bc_manager%find(LATFLOW_BC_TYPE, latflowbc_table(i))
        IF (.NOT. ASSOCIATED(latbc(i)%p)) THEN
           WRITE(msg, *) 'Link ', i, ': cannot find Lateral BC, ', latflowbc_table(i)
           CALL error_message(msg)
           ierr = ierr + 1
        END IF
     END IF
  END DO

  IF (ierr .GT. 0) THEN
     CALL error_message("Too many errors assigning BCs", fatal=.TRUE.)
  END IF
  CALL read_pidlink_info()
  CALL pidlink_initialize()

END SUBROUTINE kick_off
