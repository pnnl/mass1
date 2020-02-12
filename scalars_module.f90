!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME:  MASS1 scalars module file
!
! VERSION and DATE: 0.84 08-15-99
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
! MOD HISTORY: 10-5-98 added to MASS1 from MASS2
!                                                       fixed delta x bug in diffusion calc if you reverse distance
!                                                       reference frame; mcr 10-10-98
!              august 99 - added direct internal bc input, overides upstream C if active
!
!***************************************************************
!
MODULE scalars

USE utility
USE bc_module

IMPLICIT NONE

INTEGER  :: max_species = 2

TYPE scalar_struct
   DOUBLE PRECISION, POINTER :: conc(:,:)                               ! c depth-ave concentration
        DOUBLE PRECISION, POINTER :: concold(:,:)                       ! c old depth-ave concentration
END TYPE scalar_struct

TYPE(scalar_struct), ALLOCATABLE :: species(:)

                                ! these are used for transport
                                ! sub-time-stepping

DOUBLE PRECISION :: scalar_time, scalar_delta_t
DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE, PRIVATE :: vel, area, width, area_old, q, q_old, y, y_old
DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE, PRIVATE :: latq, latq_old

DOUBLE PRECISION, PRIVATE, PARAMETER :: max_courant = 0.99
DOUBLE PRECISION, PRIVATE, PARAMETER :: max_diffuse = 0.99

TYPE (bc_ptr), PUBLIC, DIMENSION(:,:), ALLOCATABLE :: sclrbc, latsclrbc

CONTAINS
!#####################################################################################################

SUBROUTINE allocate_species_components(i, imax, jmax, status_iounit, error_iounit)
  ! this routine allocates each component in the array of blocks
  ! allows minimal memory use for each block
  IMPLICIT NONE
  INTEGER :: i, imax, jmax, status_iounit, error_iounit, alloc_stat     ! block number, max i elements, max j elements

  WRITE(status_iounit,*)'starting component allocation for species number - ', i
  WRITE(status_iounit,*)'         maximum number of i elements = ', imax
  WRITE(status_iounit,*)'         maximum number of j elements = ', jmax

  ALLOCATE(species(i)%conc(imax,0:jmax+2), STAT = alloc_stat)           ! c depth-ave concentration
  IF(alloc_stat /= 0)THEN
     WRITE(error_iounit,*)'allocation failed for the concentration'
     CALL EXIT(1)
  ELSE
     WRITE(status_iounit,*)'allocation successful for concentration'
  ENDIF

  ALLOCATE(species(i)%concold(imax,0:jmax+2), STAT = alloc_stat)        ! c old depth-ave concentration
  IF(alloc_stat /= 0)THEN
     WRITE(error_iounit,*)'allocation failed for the old concentration'
     CALL EXIT(1)
  ELSE
     WRITE(status_iounit,*)'allocation successful for old concentration'
  ENDIF

  WRITE(status_iounit,*)'completed component allocation for species number - ', i

END SUBROUTINE allocate_species_components
    
!#####################################################################################################

SUBROUTINE allocate_species(error_iounit,status_iounit)

  USE mass1_config

  IMPLICIT NONE
  INTEGER :: alloc_stat,error_iounit,status_iounit
  INTEGER :: maxlinks, maxpoint, i, j

  maxlinks = config%maxlinks
  maxpoint = config%maxpoint

  ALLOCATE(species(max_species), STAT = alloc_stat)
  IF(alloc_stat /= 0)THEN
     WRITE(error_iounit,*)'allocation failed for the array of species - max_species=', max_species
     CALL EXIT(1)
  ELSE
     WRITE(status_iounit,*)'allocation successful for array of species - max_species=', max_species
  ENDIF

                                ! allocate module PRIVATE hydrodynamic
                                ! variables

  ALLOCATE(vel(maxlinks,maxpoint), width(maxlinks,maxpoint), &
       &area(maxlinks,maxpoint), area_old(maxlinks,maxpoint),&
       &q(maxlinks,maxpoint), q_old(maxlinks,maxpoint), &
       &y(maxlinks,maxpoint), y_old(maxlinks,maxpoint), &
       &latq(maxlinks,maxpoint), latq_old(maxlinks,maxpoint), &
       &sclrbc(maxlinks, max_species), latsclrbc(maxlinks, max_species),&
       &STAT = alloc_stat)

  IF(alloc_stat /= 0)THEN
     WRITE(error_iounit,*)'allocation failed for scalar hydrodynamic variables'
     CALL EXIT(1)
  ELSE
     WRITE(status_iounit,*)'allocation successful for scalar hydrodynamic variables'
  ENDIF

  DO i = 1, maxlinks
     DO j = 1, max_species
        NULLIFY(sclrbc(i, j)%p)
        NULLIFY(latsclrbc(i, j)%p)
     END DO
  END DO

END SUBROUTINE allocate_species

! ----------------------------------------------------------------
! INTEGER FUNCTION tvd_steps
!
! This computes the number of transport steps that should be taken in
! this hydrodynamic time step (delta_t) in order satisfy Courant
! number constraints.  The maximum courant number is used to
! determine.  The transport time step is computed so that the maximum
! Courant number is reduced to an exceptable target.
!
! The transport time step is also adjusted for diffusion stability.
! ----------------------------------------------------------------
INTEGER FUNCTION tvd_steps(delta_t)

  USE point_vars, ONLY: courant_num, diffuse_num

  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(IN) :: delta_t
  DOUBLE PRECISION :: cmax, dmax, a_transdt, d_transdt

  cmax = MAXVAL(courant_num)

  IF (cmax .GT. max_courant) THEN 
     a_transdt = max_courant/cmax*delta_t
  ELSE 
     a_transdt = delta_t
  END IF

  dmax = MAXVAL(diffuse_num) 
  IF (dmax .GT. max_diffuse) THEN 
     d_transdt = max_diffuse/dmax*delta_t
  ELSE 
     d_transdt = delta_t
  END IF

  a_transdt = MIN(a_transdt, d_transdt)
  
  tvd_steps = FLOOR(delta_t/a_transdt)

  IF (FRACTION(delta_t/a_transdt) .GT. EPSILON(a_transdt)) &
       &tvd_steps = tvd_steps + 1

  tvd_steps = MAX(tvd_steps, 1)

END FUNCTION tvd_steps


! ----------------------------------------------------------------
! SUBROUTINE tvd_interp
! This routine is used to interpolate the hydrodynamics for the
! current transport time step and store them in the PRIVATE module
! variables
! ----------------------------------------------------------------
SUBROUTINE tvd_interp(time, htime0, htime1)

  USE mass1_config
  USE cross_section
  USE section_handler_module
  USE link_vars, ONLY: maxpoints, linktype
  USE point_vars, ONLY: thalweg, &
       &harea_old=>area_old, hq=>q, hq_old=>q_old, &
       &hy=>y, hy_old=>y_old, hlatq=>lateral_inflow, &
       &hlatq_old=>lateral_inflow_old, ptsection

  IMPLICIT NONE

  DOUBLE PRECISION :: htime_begin, htime_end
  DOUBLE PRECISION :: time, htime0, htime1
  DOUBLE PRECISION :: val, val0, val1
  INTEGER :: link, point
  EXTERNAL dlinear_interp
  DOUBLE PRECISION dlinear_interp
  TYPE (xsection_prop) :: props


  htime_begin = config%time%begin
  htime_end = config%time%end
  

  IF (time .eq. htime_begin) THEN
     DO link = 1, config%maxlinks
        y(link,:) = hy_old(link,:)
        area(link,:) = harea_old(link,:)
        q(link,:) = hq_old(link,:)
        latq(link,:) = hlatq_old(link,:)
     END DO
  END IF

  DO link = 1, config%maxlinks
     area_old(link,:) = area(link,:)
     q_old(link,:) = q(link,:)
     y_old(link,:) = y(link,:)
     latq_old(link,:) = latq(link,:)
     DO point = 1, maxpoints(link)
        val0 = hq_old(link, point)
        val1 = hq(link, point)
        val = dlinear_interp(val0, htime0, val1, htime1, time)
        q(link,point) = val

        val0 = hlatq_old(link, point)
        val1 = hlatq(link, point)
        val = dlinear_interp(val0, htime0, val1, htime1, time)
        latq(link,point) = val

        SELECT CASE (linktype(link))
        CASE(1,20,21)              ! do fluvial links only
           val0 = hy_old(link, point)
           val1 = hy(link, point)
           val = dlinear_interp(val0, htime0, val1, htime1, time)
           y(link,point) = val

           val = y(link,point) - thalweg(link,point)

           CALL ptsection(link,point)%p%props(val, props)
           area(link,point) = props%area
           width(link,point) = props%topwidth

           ! val0 = DBLE(harea_old(link, point))
           ! val1 = DBLE(harea(link, point))
           ! val = dlinear_interp(val0, htime0, val1, htime1, time)
           ! area(link,point) = SNGL(val)

           vel(link, point) = q_old(link,point)/area_old(link,point)
        END SELECT
     END DO
  END DO
END SUBROUTINE tvd_interp
!###################################################################################

!----------------------------------------------------------------------------------
! uses EXPLICIT tvd scheme and split operator method
! needs "false" points before and after start and endpoints of link
!
! species = 1 is Total Dissolved Gas
! species = 2 is Ave. Water Temperature
! 
!----------------------------------------------------------------------------------
SUBROUTINE tvd_transport(species_num, c, c_old)

  USE mass1_config
  USE transport_vars , ONLY : dxx
  USE link_vars
  USE point_vars, ONLY: x, k_diff, thalweg
  
  USE energy_flux
  USE tdg_equation_coeff
  USE gas_functions
  USE hydro_output_module

  USE confluence_module

  IMPLICIT NONE

  INTEGER :: species_num
  DOUBLE PRECISION :: c(:,0:), c_old(:,0:)

  INTEGER :: i,link,point
  DOUBLE PRECISION :: velave,sum,cflx,s,corr,tmp,phi,dtdx,ave_vel
  DOUBLE PRECISION :: f(config%maxpoint)
  DOUBLE PRECISION :: k_e,k_w,area_e,area_w,flux_e,flux_w
  DOUBLE PRECISION :: qspill,qgen 
  DOUBLE PRECISION :: depth
  DOUBLE PRECISION :: tdg_saturation = 100.0, upstream_c
  DOUBLE PRECISION :: avg_area, avg_latq
  DOUBLE PRECISION :: salinity = 0.0, baro_press, t_water, energy_source

  LOGICAL :: diffusion, fluvial, nonfluvial

  DOUBLE PRECISION :: time, delta_t

  CHARACTER (LEN=256) :: msg

  time = scalar_time
  delta_t = scalar_delta_t
  
  !WRITE(*,*)'done with alloc; link loop'
  !WRITE(*,*)c(1,0),c(1,151),dxx(1,0),dxx(1,151)
  ! run through links from top to bottom
  
  links_forward: DO i=1,config%maxlinks

     link = comporder(i)

     !fluvial or nonfluvial link
     SELECT CASE(linktype(link))
     CASE(1,20,21)
        fluvial = .TRUE.
        nonfluvial = .FALSE.
     CASE(2,3,4,5,6,7,12,13)
        fluvial = .FALSE.
        nonfluvial = .TRUE.
     END SELECT

     ! special things required by TDG
     SELECT CASE (species_num)
     CASE (1)

        ! get the upstream temperature, if available
        IF(config%do_temp)THEN
           IF ((.NOT. ASSOCIATED(ucon(link)%p)) .AND. &
                &ASSOCIATED(sclrbc(link, 2)%p)) THEN
              t_water = sclrbc(link, 2)%p%current_value
           ELSE
              t_water = species(2)%conc(link,1) ! FIXME
           ENDIF
        ENDIF

        ! have a reasonable value for barometric pressure
        IF (config%met_required) THEN
           baro_press = metzone(link)%p%current%bp
        ELSE 
           baro_press = 760.0
        END IF

     END SELECT

     !----------------------------------------------------------------------------
     !do nonfluvial first just pass through concentrations
     ! 
     !IF(linktype(link) /= 1 )THEN
     IF( nonfluvial )THEN
        point = 1

        ! FIXME: check association first
        c(link,point) = ucon(link)%p%conc(c)
        
        IF((linktype(i) == 6) .AND. (species_num == 1))THEN
           
           IF (ASSOCIATED(usbc(link)%p)) THEN
              CALL hydro_bc_discharge(usbc(link)%p, qgen, qspill)
           ELSE 
              WRITE(msg, *) 'Link ', link, ' requires a upstream hydro BC'
              CALL error_message(msg, fatal=.TRUE.)
           END IF

           IF(qspill > 0.0)THEN
              !                 equations are for %Sat and effective Spill Q in Kcfs
              !                 Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
              !                 gas_eqn_type = 1 general quadratic function
              !                 tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
              !                 OR 
              !                 gas_eqn_type = 2 exponetial equation
              !                 tdg = a_gas + b_gas*EXP(c_gas*Qspill)
              !                 OR 
              !                 gas_eqn_type = 3 exponetial equation
              !                 tdg = a_gas + b_gas*EXP(c_gas*Qspill)
              !                 OR 
              !                 gas_eqn_type = 4 logaritmic equation
              !                 tdg = a_gas + b_gas*LOG(c_gas*Qspill)
              
              qspill = qspill + qgen_frac(link)*qgen
              qgen   = qgen - qgen_frac(link)*qgen
              
              SELECT CASE(gas_eqn_type(link))
                 
              CASE(1)
                 tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + c_gas(link)*(qspill/1000.0)**2 
                 c(link,2) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
              CASE(2)
                 tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                 c(link,2) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
              CASE(3)
                 tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                 c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
              CASE(4) 
                 tdg_saturation = a_gas(link) + b_gas(link)*LOG(c_gas(link)*qspill/1000.0)
                 c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
              CASE DEFAULT
                 WRITE(*,*)'ABORT - no gas eqn error in type ', linktype(link), ' link BCs at link = ',link
                 WRITE(99,*)'ABORT - no gas eqn error in type ', linktype(link), ' link BCs at link = ',link
                 CALL EXIT(1)
              END SELECT
              hydro_sat(link) = tdg_saturation
              hydro_conc(link) = c(link,2)
           ELSE
              hydro_sat(link) = 100.0
              hydro_conc(link) = TDGasConcfromSat(hydro_sat(link), t_water, salinity, baro_press)
           ENDIF
           hydro_temp(link) = t_water
           hydro_baro(link) = baro_press
           hydro_gen(link) = qgen
           hydro_spill(link) = qspill
           hydro_disch(link) = q(link, 1)
           
           IF (qspill + qgen .gt. 0.0) THEN
              ! full mixing of spill and generation waters
              c(link,2) = (c(link,2)*qspill + c(link,1)*qgen)/(qspill+qgen)
           ELSE
              c(link,2) = TDGasConcfromSat( DBLE(100.0), t_water, &
                   &salinity, baro_press)              
           END IF
           
        ELSE
           c(link,2) =  c(link,1)
        ENDIF
        
        
        !------ do fluvial links --------------------------------------
     ELSE
        
        
        !set distance difference array
        IF(time == config%time%begin)THEN
           point = 1
           dxx(link,1) = ABS(x(link,point+1) - x(link,point))
           dxx(link,0) = dxx(link,1)
           DO point=2,maxpoints(link)-1
              
              dxx(link,point) = ABS(0.5*(x(link,point) - x(link,point-1))) +  &
                   ABS(0.5*(x(link,point+1) - x(link,point)))
           END DO
           dxx(link,maxpoints(link)) =  ABS(x(link,maxpoints(link)) - x(link,maxpoints(link)-1))
           dxx(link,maxpoints(link)+1) = dxx(link,maxpoints(link)) 
           dxx(link,maxpoints(link)+2) = dxx(link,maxpoints(link))
        ENDIF
        
        ! set upstream influx for the link from c(t) bc table or junction conditions
        point = 1
        
        SELECT CASE(species_num)
        CASE(1) ! gas species
           IF ((.NOT. ASSOCIATED(ucon(link)%p)) .AND. &
                &ASSOCIATED(sclrbc(link, species_num)%p)) THEN
              upstream_c = sclrbc(link, species_num)%p%current_value
              
              SELECT CASE(linktype(link))
              CASE(1)
                 c(link,point) = upstream_c

              CASE(20) ! %TDG Saturation is specified
                 c(link,point) = TDGasConcfromSat(upstream_c, t_water, salinity, baro_press)

              CASE(21) ! Hydro project inflow link
                 IF (ASSOCIATED(usbc(link)%p)) THEN
                    CALL hydro_bc_discharge(usbc(link)%p, qgen, qspill)
                 ELSE 
                    WRITE(msg, *) 'Link ', link, ' requires a upstream hydro BC'
                    CALL error_message(msg, fatal=.TRUE.)
                 END IF

                 IF(qspill > 0.0)THEN
                    
                    !                   equations are for %Sat and effective Spill Q in Kcfs
                    !                   Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
                    !                   gas_eqn_type = 2 general quadratic function
                    !                   tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
                    !                   OR 
                    !                   gas_eqn_type = 3 exponetial equation
                    !                   tdg = a_gas + b_gas*EXP(c_gas*Qspill)
                    !                   OR 
                    !                   gas_eqn_type = 4 logaritmic equation
                    !                   tdg = a_gas + b_gas*LOG(c_gas*Qspill)
                    
                    qspill = qspill + qgen_frac(link)*qgen
                    qgen   = qgen - qgen_frac(link)*qgen
                    
                    SELECT CASE(gas_eqn_type(link))
                    CASE(2)
                       tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + c_gas(link)*(qspill/1000.0)**2 
                       c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                    CASE(3)
                       tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                       c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                    CASE(4) 
                       tdg_saturation = a_gas(link) + b_gas(link)*LOG(c_gas(link)*qspill/1000.0)
                       c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                    CASE DEFAULT
                       WRITE(*,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                       WRITE(99,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                       CALL EXIT(1)
                    END SELECT
                    hydro_sat(link) = tdg_saturation
                    hydro_conc(link) = c(link,point)
                 ELSE
                    hydro_sat(link) = 100.0
                    hydro_conc(link) = TDGasConcfromSat(hydro_sat(link), t_water, salinity, baro_press)
                 ENDIF

                 hydro_temp(link) = t_water
                 hydro_baro(link) = baro_press
                 hydro_gen(link) = qgen
                 hydro_spill(link) = qspill
                 hydro_disch(link) = q(link, 1)

                 upstream_c = TDGasConcfromSat(upstream_c, t_water, salinity, baro_press)
                 
                 IF (qspill + qgen .gt. 0.0) THEN
                    ! full mixing of spill and generation waters
                    c(link,point) = (c(link,point)*qspill + upstream_c*qgen)/(qspill+qgen)
                 ELSE 
                    c(link,point) = TDGasConcfromSat( DBLE(100.0), t_water, &
                         &salinity, baro_press)              
                 END IF
                    
                 !-----hydro inflow end
                 
              END SELECT
           !
           ! pure internal connection between fluvial links - just mix and pass through
           ELSE IF ((ASSOCIATED(ucon(link)%p)) .AND. &
                & (.NOT. ASSOCIATED(sclrbc(link, species_num)%p))) THEN
              c(link,point) = ucon(link)%p%conc(c)
           
           ! internal fluvial link that has an active table BC
           ELSE IF ((ASSOCIATED(ucon(link)%p)) .AND. &
                &(ASSOCIATED(sclrbc(link, species_num)%p))) THEN ! internal link with table spec
              upstream_c = sclrbc(link, species_num)%p%current_value
              SELECT CASE(linktype(link))
              CASE(1) ! % internal C (mg/L) specified
                 c(link,point) = upstream_c
                 
              CASE(20) ! internal %TDG Saturation is specified
                 c(link,point) = TDGasConcfromSat(upstream_c, t_water, salinity, baro_press)
                              
              CASE(21) ! Hydro project inflow for an internal link
                 IF (ASSOCIATED(usbc(link)%p)) THEN
                    CALL hydro_bc_discharge(usbc(link)%p, qgen, qspill)
                 ELSE 
                    WRITE(msg, *) 'Link ', link, ' requires a upstream hydro BC'
                    CALL error_message(msg, fatal=.TRUE.)
                 END IF
                 
                 IF(qspill > 0.0)THEN
                    
                    !                   equations are for %Sat and effective Spill Q in Kcfs
                    !                   Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
                    !           gas_eqn_type = 0 No eqn given - read specified gas Conc (mg/L)
                    !           gas_eqn_type = 1 No eqn given - read specified gas %Saturation
                    !                   gas_eqn_type = 2 general quadratic function
                    !                   tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
                    !                   OR 
                    !                   gas_eqn_type = 3 exponetial equation
                    !                   tdg = a_gas + b_gas*EXP(c_gas*Qspill)
                    !                   OR 
                    !                   gas_eqn_type = 4 logaritmic equation
                    !                   tdg = a_gas + b_gas*LOG(c_gas*Qspill)
                    
                    qspill = qspill + qgen_frac(link)*qgen
                    qgen   = qgen - qgen_frac(link)*qgen
                    
                    SELECT CASE(gas_eqn_type(link))
                    CASE(0)
                       c(link,point) = upstream_c
                       hydro_conc(link) = c(link,point)
                       hydro_sat(link) = &
                            &TDGasSaturation(hydro_conc(link), t_water, salinity, baro_press)
                    CASE(1)
                       tdg_saturation = upstream_c
                       c(link,point) = &
                            &TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                       hydro_sat(link) = tdg_saturation
                       hydro_conc(link) = c(link,point)
                    CASE(2)
                       tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + &
                            &c_gas(link)*(qspill/1000.0)**2 
                       c(link,point) = &
                            &TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                       hydro_conc(link) = c(link,point)
                       hydro_sat(link) = tdg_saturation
                    CASE(3)
                       tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                       c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                       hydro_conc(link) = c(link,point)
                       hydro_sat(link) = tdg_saturation
                    CASE(4) 
                       tdg_saturation = a_gas(link) + b_gas(link)*LOG(c_gas(link)*qspill/1000.0)
                       c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                       hydro_conc(link) = c(link,point)
                       hydro_sat(link) = tdg_saturation
                    CASE DEFAULT
                       WRITE(*,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                       WRITE(99,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                       CALL EXIT(1)
                    END SELECT
                 ELSE
                    hydro_sat(link) = 100.0
                    hydro_conc(link) = TDGasConcfromSat(hydro_sat(link), t_water, salinity, baro_press)
                 ENDIF

                 hydro_gen(link) = qgen
                 hydro_temp(link) = t_water
                 hydro_baro(link) = baro_press
                 hydro_spill(link) = qspill
                 hydro_disch(link) = q(link, 1)
                 
                 sum = 0.0
                 
                 upstream_c = 0.0
                 IF (ASSOCIATED(ucon(link)%p)) THEN
                    upstream_c = ucon(link)%p%conc(c)
                 END IF

                 IF (qspill + qgen .gt. 0.0) THEN
                    ! full mixing of spill and generation waters
                    c(link,point) = (c(link,point)*qspill + upstream_c*qgen)/(qspill+qgen)
                 ELSE 
                    c(link,point) = TDGasConcfromSat( DBLE(100.0), t_water, &
                         &salinity, baro_press)
                 END IF

                 !-----hydro inflow end

              END SELECT
           ELSE 
              WRITE(*,*)'no trans BC specification for link',link,' -- ABORT'
              WRITE(99,*)'no trans BC specification for link',link,' -- ABORT'
              CALL EXIT(1)
           ENDIF
           
        CASE(2) ! temperature species
           IF (ASSOCIATED(sclrbc(link, species_num)%p)) THEN
              c(link, point) = sclrbc(link, species_num)%p%current_value
           ELSE IF (ASSOCIATED(ucon(link)%p)) THEN
              c(link, point) = ucon(link)%p%conc(c)
           ELSE 
              WRITE(*,*)'no temp BC specification for link',link,' -- ABORT'
              WRITE(99,*)'no temp BC specification for link',link,' -- ABORT'
              CALL EXIT(1)
           ENDIF
        END SELECT
        
        c(link,point-1) = c(link,point)
        c_old(link,point) = c(link,point)
        c_old(link,point-1) = c_old(link,point)
        
        ! compute fluxes
        
        
        
        DO point = 1,maxpoints(link)-1
           
           IF(point == maxpoints(link))THEN
              velave = q_old(link,point)
              ave_vel = vel(link,point)
              !velave = 0.5*(q_old(link,point)+q_old(link,point-1))
              !ave_vel = 0.5*(vel(link,point)+vel(link,point-1))
           ELSE
              !velave = q_old(link,point)
              !ave_vel = vel(link,point)
              
              velave = 0.5*(q_old(link,point)+q_old(link,point+1))
              ave_vel = 0.5*(vel(link,point)+vel(link,point+1))      
           ENDIF
           
           !velave = 1.0
           
           cflx =  ave_vel*delta_t/dxx(link,point)
           !cflx = 0.0
           
           if(velave.ge.0)then   !!! +ve velocity case 
              f(point)=c(link,point+1)-c(link,point)
              if(abs(f(point)).gt.1.d-40)then
                 corr = 0.5*(dxx(link,point+1)+dxx(link,point))/         &
                      (0.5*(dxx(link,point)+dxx(link,point-1)))
                 s = corr *     &
                      (c(link,point)-c(link,point-1))/f(point)
                 tmp = min(2.d0,2.d0*s,0.33333333333333333d0*    &
                      (2.d0-cflx+(1.d0+cflx)*s))
                 phi = max(0.0,tmp)
              else
                 phi=0
              end if
              f(point)=velave*   &
                   ( c(link,point) + 0.5*(1.0-cflx)*phi*f(point) )
              
           else        !!! -ve velocity case
              f(point)=c(link,point)-c(link,point+1)
              if(abs(f(point)).gt.1.d-40)then
                 corr = 0.5*(dxx(link,point+1)+dxx(link,point))/        &
                      (0.5*(dxx(link,point+1)+dxx(link,point+2)))         
                 s=corr*                                                 &
                      (c(link,point+1)-c(link,point+2))/f(point)         
                 phi=max(0.0d0,min(2.d0,2.d0*s,  &
                      0.33333333333333333d0*              &
                      (2.d0-cflx+(1.d0+cflx)*s)))
              else
                 phi=0
              end if
              f(point)=velave*( c(link,point+1) +        &
                   0.5*(1.0-cflx)*phi*f(point) )
           end if

        END DO
        
        !update concentration values
        DO point=2,maxpoints(link)-1
           !area = q(link,point)/vel(link,point)           
           dtdx = delta_t/(dxx(link,point))
           
           c(link,point)=c(link,point)*area_old(link,point)/area(link,point) - &
                &dtdx*(f(point) - f(point-1))/area(link,point)
           c_old(link,point) = c(link,point)                    
           ! IF (point .EQ. 100) THEN
           !    WRITE(*, '(*(1X,E10.3))') dtdx, &
           !         &q(link, point), q_old(link,point),&
           !         &area(link,point), area_old(link,point), &
           !         &f(point), f(point-1)
           ! END IF
        END DO
        
        c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
        c_old(link,maxpoints(link)) = c_old(link,maxpoints(link)-1)
        ! c(link,maxpoints(link)) = &
        !      &(c(link,maxpoints(link)-1) - c(link,maxpoints(link)-2))/dxx(link, maxpoints(link)-2)*&
        !      &(dxx(link, maxpoints(link)-2) + dxx(link, maxpoints(link)-1) + c(link,maxpoints(link)-2))
        ! c_old(link,maxpoints(link)) = c(link,maxpoints(link))
        !c(link,maxpoints(link)) = 0.0
        
        
        !---------------------------------------------------------------------------
        ! Diffusion Step
        !       explicit finite-volume solution
        !
        diffusion = .FALSE.
        IF( (species_num == 1) .AND. config%gas_diffusion ) diffusion = .TRUE.
        IF( (species_num == 2) .AND. config%temp_diffusion ) diffusion = .TRUE.
        IF(diffusion)THEN
           DO point = 2, maxpoints(link)-1
              
              k_e = 0.5*(k_diff(link,point+1)+k_diff(link,point))
              k_w = 0.5*(k_diff(link,point)+k_diff(link,point-1))
              area_e =0.5*(area_old(link,point+1)+area_old(link,point))
              area_w =0.5*(area_old(link,point)+area_old(link,point-1))
              !area_e = area_old(link,point)
              !area_w = area_e
              flux_e = k_e*area_e*(c_old(link,point+1)-c_old(link,point))/ABS(x(link,point+1)-x(link,point))
              flux_w = k_w*area_w*(c_old(link,point)-c_old(link,point-1))/ABS(x(link,point)-x(link,point-1))
              dtdx = delta_t/(dxx(link,point))
              
              c(link,point)=c(link,point) + dtdx*(flux_e - flux_w)/area_old(link,point)
              !c(link,point)=c(link,point)*area_old(link,point)/area(link,point) + dtdx*(flux_e - flux_w)/area(link,point)
              
           END DO
           c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
           
        ENDIF !diffusion step if
        
        !-----------------------------------------------------------------------
        ! Source Term Step - could put in a RK4 routine to do reactions
        ! degassing, surface heat exchange, or other source/sink processes


                                ! Correction for lateral inflow -- the
                                ! concentration of for outgoing
                                ! lateral inflow is the same as the
                                ! current concentration

        IF (config%do_latflow) THEN
           DO point=2,maxpoints(link)-1
              avg_area = (area(link,point) + area_old(link,point))/2.0
              avg_latq = (latq(link,point) + latq_old(link,point))/2.0
              IF (avg_latq < 0.0) THEN
                 upstream_c = c(link,point)
              ELSE IF (ASSOCIATED(latsclrbc(link, species_num)%p)) THEN
                 upstream_c = latsclrbc(link, species_num)%p%current_value
              ELSE 
                 upstream_c = c(link,point)
                 ! FIXME: should this be reported as an error?
              END IF
              c(link,point) = (c(link,point)*avg_area + upstream_c*avg_latq*delta_t)/avg_area
              ! IF(c(link,point) < 0.0) c(link,point) = 0.0
              ! WRITE(*,'(2I4,1X,9(1X,E11.4))') link, point, c(link,point), upstream_c, avg_area, &
              !      &latq_old(link,point), latq(link,point), avg_latq, &
              !      &lateral_inflow_old(link,point), lateral_inflow(link,point), delta_t
           END DO
           c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
        END IF


                                ! Gas exchange

        IF((species_num == 1) .AND. (config%gas_exchange) )THEN
           DO point=2,maxpoints(link)-1
              t_water = species(2)%conc(link,point)
              c(link,point) = c(link,point) + &
                   &metzone(link)%p%gas_exchange(t_water, c(link, point), salinity)* &
                   &width(link,point)*delta_t/area(link,point)
              IF(c(link,point) < 0.0) c(link,point) = 0.0
           END DO
           c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
        ENDIF !degass if

                                ! Heating/Cooling to Atmosphere
        
        !-------------------------------------------------------------------------
        IF((species_num == 2) .AND. (config%temp_exchange) )THEN
           DO point=2,maxpoints(link)-1
              ! CALL update_met_data(config%do_gas, time, met_zone(link))
              t_water = c(link,point)
              depth = y(link,point) - thalweg(link,point)
              !call met_zone_coeff(met_zone(link), met_coeff)
              ! energy_source = net_heat_flux(met_coeff,&
              !      &net_solar, t_water, t_air, t_dew, windspeed) &
              !      /(1000.0*4186.0/3.2808) ! rho*specifc heat*depth in feet

              energy_source = metzone(link)%p%energy_flux(t_water, 1.0d00)/&
                   &(1000.0*4186.0/3.2808)
              c(link,point) = c(link,point) + &
                   &energy_source*delta_t*width(link,point)/area(link,point)

              ! IF(c(link,point) <   0.0) c(link,point) =   0.0 ! frozen - should add warning printout
              ! IF(c(link,point) > 100.0) c(link,point) = 100.0 ! boiling - should add warning printout
           END DO
           c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
        ENDIF
        
     ENDIF !fluvial links if
     
  END DO  links_forward
  
  !-------------------------------------------------------------------------------
  ! 
  
  !IF(time==time_end)THEN
  !     DEALLOCATE(b,m)
  !ENDIF
  
END SUBROUTINE tvd_transport

END MODULE scalars
