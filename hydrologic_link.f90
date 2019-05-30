! ----------------------------------------------------------------
! MODULE hydrologic_link_module
! ----------------------------------------------------------------
MODULE hydrologic_link_module

  USE utility
  USE point_module
  USE bc_module
  USE link_module
  USE scalar_module
  USE cross_section
  USE section_handler_module
  USE mass1_config
  USE linear_link_module

  IMPLICIT NONE

  INTEGER, PRIVATE, PARAMETER :: hydrologic_link_maxpt = 2
  DOUBLE PRECISION, PRIVATE, PARAMETER :: hydrologic_min_storage = 1.0D-10

  ! ----------------------------------------------------------------
  ! TYPE hydrologic_link
  !
  ! The hydrologic link has a number of restrictions:
  ! 1. Points can only be specified by average slope (option 2).
  ! 2. A hydrologic link has 2 points: one for inflow, one for
  !    outflow. A separate point is maintained internally for link
  !    "average" conditions.
  ! 3. No reverse flow.
  ! 4. A fluvial link cannot be upstream of a hydrologic link.
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: hydrologic_link
     DOUBLE PRECISION :: y
     DOUBLE PRECISION :: L, So
     DOUBLE PRECISION :: K

     ! inflow and outflow are stored as discharge
     DOUBLE PRECISION :: inflow, outflow
     DOUBLE PRECISION :: inflow_old, outflow_old

     ! storage is in volume units
     DOUBLE PRECISION :: storage, storage_old

     ! for link "average" conditions for transport
     TYPE (point_t) :: avgpt

     ! for transport
     DOUBLE PRECISION :: trans_storage, trans_storage_old
     
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
     PROCEDURE :: trans_interp => hydrologic_link_trans_interp
     PROCEDURE :: transport => hydrologic_link_transport
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
    

  END SUBROUTINE hydrologic_link_construct


  ! ----------------------------------------------------------------
  !  FUNCTION hydrologic_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION hydrologic_link_initialize(this, ldata, bcman, sclrman, metman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman
    CHARACTER (LEN=1024) :: msg
    CLASS (link_input_data), ALLOCATABLE :: my_ldata

    ! a hydrologic link can only have 2 points: inflow and outflow

    ALLOCATE(my_ldata)
    my_ldata = ldata
    IF (my_ldata%npt .NE. hydrologic_link_maxpt) THEN
       WRITE (msg, *) 'link ', ldata%linkid, &
            &': hydrologic link can only have ', hydrologic_link_maxpt, ' points, got ', &
            &my_ldata%npt, " -- continuing with ", hydrologic_link_maxpt, " points"
       CALL error_message(msg)
       my_ldata%npt = hydrologic_link_maxpt
    END IF

    ierr = this%linear_link_t%initialize(my_ldata, bcman, sclrman, metman)

    IF (my_ldata%lbcid .GT. 0) THEN
       this%latbc => bcman%find(LATFLOW_BC_TYPE, my_ldata%lbcid)
       IF (.NOT. ASSOCIATED(this%latbc)) THEN
          WRITE (msg, *) 'link ', my_ldata%linkid, ': unknown lateral inflow id: ', &
               &my_ldata%lbcid
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    END IF

    ALLOCATE(this%avgpt%trans%cnow(sclrman%nspecies))
    ALLOCATE(this%avgpt%trans%cold(sclrman%nspecies))
    this%avgpt%trans%cnow = 0.0
    this%avgpt%trans%cold = 0.0

    DEALLOCATE(my_ldata)
    
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
       this%avgpt%xsection%p => this%pt(1)%xsection%p
       this%avgpt%manning = this%pt(1)%manning
       this%avgpt%kstrick = this%pt(1)%kstrick
       this%avgpt%k_diff = this%pt(1)%k_diff
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
          dx = ABS(this%pt(i)%x - this%pt(i+1)%x)/2.00
       ELSE IF (i .GE. this%npoints) THEN
          dx = ABS(this%pt(i)%x - this%pt(i-1)%x)/2.0
       ELSE 
          dx = ABS(this%pt(i-1)%x - this%pt(i+1)%x)/2.0
       END IF
       a = this%pt(i)%xsection%p%area(depth)
       this%storage = this%storage + dx*a
    END DO

    CALL hydro_average(this%pt(1)%hnow, this%pt(2)%hnow, this%avgpt%hnow)

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

    DO i = 1, this%npoints
       ASSOCIATE (pt => this%pt(i))
         pt%hold = pt%hnow
         pt%xspropold = pt%xsprop
         IF (ASSOCIATED(this%species)) THEN
            pt%trans%hold = pt%hold
            pt%trans%xspropold = pt%xsprop
         END IF
       END ASSOCIATE
    END DO
    this%avgpt%hold = this%avgpt%hnow
    IF (ASSOCIATED(this%species)) THEN
       this%avgpt%trans%hold = this%avgpt%hold
       this%avgpt%trans%xspropold = this%avgpt%xsprop
    END IF

    ! get upstream inflow (volume)

    invol = 0.0
    IF (ASSOCIATED(this%ucon)) THEN
       ! do something to get a discharge (there should only be
       ! hydrologic links upstream)
       invol = this%ucon%discharge()
    ELSE
       IF (ASSOCIATED(this%usbc)) THEN
          invol = this%usbc%current_value
       END IF
    END IF
    
    IF (ASSOCIATED(this%latbc)) THEN
       this%latqold = this%latq
       this%latq = this%latbc%current_value
       DO i = 1, this%npoints
          this%pt(i)%hnow%lateral_inflow = this%latq
       END DO
       this%avgpt%hnow%lateral_inflow = this%latq
    END IF

    this%inflow = invol
    latvol = this%latq*this%L

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
       depth = MAX(depth, 0.0)
       depth = &
            &this%pt(i)%xsection%p%normal_depth(q, this%So, this%pt(i)%kstrick, depth)
       this%pt(i)%hnow%y = depth + this%pt(i)%thalweg
    END DO

  END SUBROUTINE hydrologic_link_backward

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_hupdate
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_hupdate(this, grav, unitwt, dt)

    IMPLICIT NONE
    
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: grav, unitwt, dt

    CALL this%linear_link_t%hydro_update(grav, unitwt, dt)

    this%storage_old = this%storage
    this%inflow_old = this%inflow
    this%outflow_old = this%outflow

    CALL hydro_average(this%pt(1)%hnow, this%pt(this%npoints)%hnow, this%avgpt%hnow)
    
    CALL this%avgpt%hydro_update(grav, unitwt, dt, this%L)

    WRITE(*,*) 'Hydrologic link ', this%id, &
         &": I = ", this%inflow, &
         &", S = ", this%storage, &
         &", O = ", this%outflow, &
         &", V = ", this%volume()

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
         &this%inflow_old, this%outflow_old, this%latqold, &
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

    CALL hydro_average(this%pt(1)%hnow, this%pt(this%npoints)%hnow, this%avgpt%hnow)

  END SUBROUTINE hydrologic_link_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_read_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_read_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE

    CLASS (hydrologic_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    INTEGER :: i, s, iostat, ierr = 0
    CHARACTER (LEN=1024) :: msg
    
    DOUBLE PRECISION :: c(nspecies), cold(nspecies)

    ierr = 0
    
    CALL this%linear_link_t%read_trans_restart(iunit, nspecies)
    
    READ(iunit, IOSTAT=iostat) &
         &(c(s), s = 1, nspecies), &
         &(cold(s), s = 1, nspecies)

    IF (IS_IOSTAT_END(iostat)) THEN
       WRITE(msg, *) 'link ', this%id, &
            &': error reading (transport) restart for point ', i
       CALL error_message(msg)
       ierr = ierr + 1
    ELSE IF (iostat .NE. 0) THEN
       WRITE(msg, *) 'link ', this%id, &
            &': error reading (transport) restart for point ', i
       CALL error_message(msg)
       ierr = ierr + 1
    END IF
    DO s = 1, nspecies
       this%avgpt%trans%cnow(s) = c(s)
       this%avgpt%trans%cold(s) = cold(s)
    END DO


    IF (ierr .GT. 0) THEN
       WRITE(msg, *) 'problem reading restart (transport) for link', this%id
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE hydrologic_link_read_trans_restart


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
         &this%inflow_old, this%outflow_old, this%latqold, &
         &this%storage, this%storage_old

  END SUBROUTINE hydrologic_link_write_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_write_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_write_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    INTEGER :: s, iostat
    DOUBLE PRECISION :: c(nspecies), cold(nspecies)
    CHARACTER (LEN=1024) :: msg

    CALL this%linear_link_t%write_trans_restart(iunit, nspecies)
    
    WRITE(iunit, IOSTAT=iostat) &
         &(this%avgpt%trans%cnow(s), s = 1, nspecies), &
         &(this%avgpt%trans%cold(s), s = 1, nspecies)
    IF (iostat .NE. 0) THEN
       WRITE(msg, *) 'problem reading restart (transport) for link', this%id
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE hydrologic_link_write_trans_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_trans_interp
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_trans_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1
    INTEGER :: i
    CLASS (point_t), POINTER :: pt
    DOUBLE PRECISION :: depth
    DOUBLE PRECISION :: dlinear_interp

    CALL this%linear_link_t%trans_interp(tnow, htime0, htime1)
    this%avgpt%trans%hold = this%avgpt%trans%hnow
    this%avgpt%xspropold = this%avgpt%xsprop
    CALL this%avgpt%transport_interp(tnow, htime0, htime1)

    this%trans_storage_old = this%trans_storage
    this%trans_storage = dlinear_interp(this%storage_old, htime0, this%storage, htime1, tnow)

  END SUBROUTINE hydrologic_link_trans_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydrologic_link_transport
  ! ----------------------------------------------------------------
  SUBROUTINE hydrologic_link_transport(this, ispec, tdeltat)

    IMPLICIT NONE
    CLASS (hydrologic_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(IN) :: tdeltat

    DOUBLE PRECISION :: inflow, outflow, latflow
    DOUBLE PRECISION :: ci, co, csnow, csold, clat

    INTEGER :: i
    CHARACTER (LEN=1024) :: msg

    ! if there is no storage, just copy the upstream concencentration 
    IF (this%trans_storage .LT. hydrologic_min_storage) THEN
       CALL this%linear_link_t%transport(ispec, tdeltat)
       this%avgpt%trans%cnow(ispec) = this%pt(1)%trans%cnow(ispec)
       RETURN
    END IF

    DO i = 1, this%npoints
       this%pt(i)%trans%cold(ispec) = this%pt(i)%trans%cnow(ispec)
    END DO
    this%avgpt%trans%cold(ispec) = this%avgpt%trans%cnow(ispec)

    inflow = this%pt(1)%trans%hnow%q
    outflow = this%pt(this%npoints)%trans%hnow%q
    latflow = this%avgpt%trans%hnow%lateral_inflow*this%L

    ! if lateral outflow, just add it to outflow
    IF (latflow .LE. 0.0) THEN
       outflow = outflow - latflow
       latflow = 0.0
    END IF

    ! reverse flow is not allow, so just get upstream conc
    
    ci = 0.0
    IF (ASSOCIATED(this%species(ispec)%usbc)) THEN
       ci = this%species(ispec)%getusbc()
    ELSE IF (ASSOCIATED(this%ucon)) THEN
       ci = this%ucon%conc(ispec)
    ELSE
       IF (inflow .GT. 0.0) THEN
          WRITE(msg, *) 'link ', this%id, &
               &': error: upstream inflow w/o conc BC for species ', &
               &ispec
          CALL error_message(msg)
       END IF
    END IF
    this%pt(1)%trans%cnow(ispec) = ci

    csold = this%avgpt%trans%cold(ispec)
    
    IF (this%species(ispec)%scalar%dolatinflow) THEN
       clat = csold
       IF (latflow .GE. 0.0) THEN
          IF (ASSOCIATED(this%species(ispec)%latbc)) THEN
             clat = this%species(ispec)%latbc%current_value
          END IF
       END IF
    END IF

    csnow = csold               ! fall back
    
#if 0
    co = csold                  ! explicit upwind
    IF (this%trans_storage .GT. hydrologic_min_storage) THEN
       csnow = tdeltat*inflow*ci &
            &+ tdeltat*latflow*clat &
            &- tdeltat*outflow*co &
            &+ csold*this%trans_storage_old
       csnow = csnow/this%trans_storage
    END IF
#else
    ! implicit upwind
    IF ((tdeltat*outflow + this%trans_storage) .GT. hydrologic_min_storage) THEN
       csnow = tdeltat*inflow*ci &
            &+ tdeltat*latflow*clat &
            &+ csold*this%trans_storage_old
       csnow = csnow/(tdeltat*outflow+this%trans_storage)
       co = csnow
    END IF
#endif
    this%avgpt%trans%cnow(ispec) = csnow
    this%pt(this%npoints)%trans%cnow = co

  END SUBROUTINE hydrologic_link_transport


END MODULE hydrologic_link_module
