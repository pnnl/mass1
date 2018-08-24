! ----------------------------------------------------------------
! MODULE hydrologic_link_module
! ----------------------------------------------------------------
MODULE hydrologic_link_module

  USE utility
  USE point_module
  USE bc_module
  USE link_module
  USE cross_section
  USE section_handler_module
  USE mass1_config
  USE linear_link_module

  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! TYPE hydrologic_link
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: hydrologic_link
     DOUBLE PRECISION :: y
     DOUBLE PRECISION :: L, So
     DOUBLE PRECISION :: K

     ! inflow and outflow are stored as discharge, ft^3/s
     DOUBLE PRECISION :: latq, inflow, outflow
     DOUBLE PRECISION :: latq_old, inflow_old, outflow_old

     ! storage is in volume units, ft^3
     DOUBLE PRECISION :: storage, storage_old
   CONTAINS
     PROCEDURE :: construct => hydrologic_link_construct
     PROCEDURE :: initialize => hydrologic_link_initialize
     PROCEDURE :: readpts => hydrologic_link_readpts
     PROCEDURE :: set_initial => hydrologic_link_set_initial
     PROCEDURE :: forward_sweep => hydrologic_link_forward
     PROCEDURE :: backward_sweep => hydrologic_link_backward
     PROCEDURE :: read_restart => hydrologic_link_read_restart
     PROCEDURE :: write_restart => hydrologic_link_write_restart
     PROCEDURE :: hydro_update => hydrologic_link_hupdate
  END type hydrologic_link

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_construct
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_construct(this)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    CALL this%linear_link_t%construct()
    NULLIFY(this%latbc)
    this%inflow = 0.0
    this%outflow = 0.0
    this%inflow_old = 0.0
    this%outflow_old = 0.0
    this%storage = 0.0
    this%storage_old = 0.0
    this%latq = 0.0
    this%latq_old = 0.0
    

  END SUBROUTINE hydrologic_link_construct


  ! ----------------------------------------------------------------
  !  FUNCTION hydrologic_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION hydrologic_link_initialize(this, ldata, bcman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CHARACTER (LEN=1024) :: msg

    ierr = this%linear_link_t%initialize(ldata, bcman)

    IF (ldata%lbcid .GT. 0) THEN
       this%latbc => bcman%find(LATFLOW_BC_TYPE, ldata%lbcid)
       IF (.NOT. ASSOCIATED(this%latbc)) THEN
          WRITE (msg, *) 'link ', ldata%linkid, ': unknown lateral inflow id: ', &
               &ldata%lbcid
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    END IF

  END FUNCTION hydrologic_link_initialize


  ! ----------------------------------------------------------------
  !  FUNCTION hydrologic_link_readpts
  ! ----------------------------------------------------------------
  FUNCTION hydrologic_link_readpts(this, theconfig, sectman, punit, lineno) RESULT(ierr)
    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(IN) :: theconfig
    CLASS (section_handler), INTENT(INOUT) :: sectman
    INTEGER, INTENT(IN) :: punit
    INTEGER, INTENT(INOUT) :: lineno
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    SELECT CASE (this%input_option)
    CASE (1)
       WRITE(msg, *) "Link ", this%id, &
            &": readpts: hydrologic links cannot use point-based input (option 1)" 
       CALL error_message(msg)
       ierr = ierr + 1
    CASE (2)
       ierr = this%linear_link_t%readpts(theconfig, sectman, punit, lineno)
    END SELECT

    this%L = ABS(this%pt(1)%x - this%pt(this%npoints)%x)
    this%So = ABS(this%pt(1)%thalweg - this%pt(this%npoints)%thalweg)
    this%So = this%So/this%L

    ! WRITE(*,*) "Hydrologic link ", this%id, ": L = ", this%L, ", So = ", this%So

    RETURN
  END FUNCTION hydrologic_link_readpts

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_set_initial
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_set_initial(this, stage, discharge, c)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: stage, discharge, c(:)
    INTEGER :: i
    DOUBLE PRECISION :: dx, a, depth

    CALL this%linear_link_t%set_initial(stage, discharge, c)

    ! initialize the discharges
    
    this%inflow = discharge
    this%outflow = discharge
    this%inflow_old = this%inflow
    this%outflow_old = this%outflow

    ! assume normal stage throughout the link initial compute storage
    ! based on that

    this%storage = 0.0

    DO i = 1, this%npoints
       depth = this%pt(i)%hnow%y - this%pt(i)%thalweg
       depth = this%pt(i)%xsection%p%normal_depth(&
            &this%pt(i)%hnow%q, this%So, this%pt(i)%kstrick,&
            &depth)
       this%pt(i)%hnow%y = depth + this%pt(i)%thalweg
       IF (i .EQ. 1) THEN
          dx = ABS(this%pt(i)%x - this%pt(i+1)%x)/2.0
       ELSE IF (i .GE. this%npoints) THEN
          dx = ABS(this%pt(i)%x - this%pt(i-1)%x)/2.0
       ELSE 
          dx = ABS(this%pt(i-1)%x - this%pt(i+1)%x)/2.0
       END IF
       a = this%pt(i)%xsection%p%area(depth)
       this%storage = this%storage + dx*a
    END DO

    this%storage_old = this%storage
    this%y = this%pt(this%npoints)%hnow%y - this%pt(this%npoints)%thalweg

    ! WRITE(*,*) "Hydrologic link ", this%id, ": Q = ", discharge, ", S = ", this%storage

  END SUBROUTINE hydrologic_link_set_initial


  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_forward
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_forward(this, deltat)

    IMPLICIT NONE

    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat

    DOUBLE PRECISION :: X, kstrick
    DOUBLE PRECISION :: invol, outvol, latvol, q, x0
    INTEGER :: i

    ! get upstream inflow (volume)

    IF (ASSOCIATED(this%ucon)) THEN
       ! do something to get a discharge (there should only be
       ! hydrologic links upstream)
       invol = this%ucon%discharge()
    ELSE
       IF (ASSOCIATED(this%usbc)) THEN
          invol = this%usbc%current_value
       ELSE 
          invol = 0.0
       END IF
    END IF

    this%inflow = invol
    latvol = this%latq

    ! use the downstream section to compute conveyance
    
    this%y = this%pt(this%npoints)%xsprop%depth
    this%y = this%pt(this%npoints)%xsprop%hydrad
    ! this%y = 0.75*this%y
    kstrick = this%pt(this%npoints)%kstrick
    this%K = SQRT(this%So)*kstrick*this%y**(2.0/3.0)/this%L
    X = EXP(-this%K*deltat)

    ! WRITE (*,*) "Hydrologic link ", this%id, ": ", &
    !      &"y = ", this%y, ", "&
    !      &"K = ", this%K, ", "&
    !      &"X = ", X 

    this%storage = (invol + latvol)/this%K + &
         &X*(this%storage_old - (invol + latvol)/this%K)

    outvol = invol + latvol - (this%storage - this%storage_old)/deltat
    this%outflow = outvol

    ! compute some coefficients that can be used by confluences

    ! compute discharge rates and sweep coefficients for confluences

    x0 = this%pt(this%npoints)%x

    DO i = 1, this%npoints
       x = this%pt(i)%x
       q = ABS(x - x0)/this%L*(this%inflow - this%outflow) + this%outflow
       this%pt(i)%hnow%q = q
    END DO

    this%pt(:)%sweep%e = 0.0
    this%pt(:)%sweep%f = 0.0

    ! WRITE(*,*) 'Hydrologic link ', this%id, &
    !      &": I = ", this%inflow, &
    !      &", S = ", this%storage, &
    !      &", O = ", this%outflow

  END SUBROUTINE hydrologic_link_forward

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_backward
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_backward(this, dsbc_type)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: dsbc_type
    INTEGER :: i
    DOUBLE PRECISION :: q, depth

    ! assume normal depth at all cross sections, given the
    ! interpolated discharge

    DO i = 1, this%npoints
       q = this%pt(i)%hnow%q
       depth = this%pt(i)%hnow%y - this%pt(i)%thalweg
       depth = &
            &this%pt(i)%xsection%p%normal_depth(q, this%So, this%pt(i)%kstrick, depth)
       this%pt(i)%hnow%y = depth + this%pt(i)%thalweg
    END DO

  END SUBROUTINE hydrologic_link_backward

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_hupdate
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_hupdate(this, grav, dt)

    IMPLICIT NONE
    
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: grav, dt

    CALL this%linear_link_t%hydro_update(grav, dt)

    this%storage_old = this%storage
    this%inflow_old = this%inflow
    this%outflow_old = this%outflow

    IF (ASSOCIATED(this%latbc)) THEN
       this%latq_old = this%latq
       this%latq = this%latbc%current_value*this%L
    END IF



  END SUBROUTINE hydrologic_link_hupdate

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_read_restart(this, iunit)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER :: ierr, iostat
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    CALL this%linear_link_t%read_restart(iunit)
    
    READ(iunit, IOSTAT=iostat) this%y, this%K, &
         &this%inflow, this%outflow, this%latq, &
         &this%inflow_old, this%outflow_old, this%latq_old, &
         &this%storage, this%storage_old
    IF (IS_IOSTAT_END(iostat)) THEN
       WRITE(msg, *) 'link ', this%id, &
            &': premature end of file for hydrologic link'
       CALL error_message(msg)
       ierr = ierr + 1
    ELSE IF (iostat .NE. 0) THEN
       WRITE(msg, *) 'link ', this%id, &
            &': error reading restart for hydrologic link '
       CALL error_message(msg)
       ierr = ierr + 1
    END IF

    IF (ierr .GT. 0) THEN
       WRITE(msg, *) 'problem reading restart for link', this%id
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE hydrologic_link_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_write_restart(this, iunit)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit

    CALL this%linear_link_t%write_restart(iunit)

    WRITE(iunit) this%y, this%K, &
         &this%inflow, this%outflow, this%latq, &
         &this%inflow_old, this%outflow_old, this%latq_old, &
         &this%storage, this%storage_old

  END SUBROUTINE hydrologic_link_write_restart


END MODULE hydrologic_link_module
