!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS1 scalars module file
!
! VERSION and DATE: 0.80 10-5-98
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
!							fixed delta x bug in diffusion calc if you reverse distance
!							reference frame; mcr 10-10-98
!
!
!***************************************************************
!
MODULE scalars

IMPLICIT NONE

INTEGER  :: max_species = 2


TYPE scalar_struct
	REAL, POINTER :: conc(:,:)				! c depth-ave concentration
	REAL, POINTER :: concold(:,:)			! c old depth-ave concentration
END TYPE scalar_struct

TYPE(scalar_struct), ALLOCATABLE :: species(:)



CONTAINS
!#####################################################################################################

SUBROUTINE allocate_species_components(i, imax, jmax, status_iounit, error_iounit)
! this routine allocates each component in the array of blocks
! allows minimal memory use for each block
IMPLICIT NONE
INTEGER :: i, imax, jmax, status_iounit, error_iounit, alloc_stat	! block number, max i elements, max j elements

WRITE(status_iounit,*)'starting component allocation for species number - ', i
WRITE(status_iounit,*)'         maximum number of i elements = ', imax
WRITE(status_iounit,*)'         maximum number of j elements = ', jmax

ALLOCATE(species(i)%conc(imax,0:jmax+2), STAT = alloc_stat)		! c depth-ave concentration
IF(alloc_stat /= 0)THEN
	WRITE(error_iounit,*)'allocation failed for the concentration'
	CALL EXIT
ELSE
	WRITE(status_iounit,*)'allocation successful for concentration'
ENDIF

ALLOCATE(species(i)%concold(imax,0:jmax+2), STAT = alloc_stat)	! c old depth-ave concentration
IF(alloc_stat /= 0)THEN
	WRITE(error_iounit,*)'allocation failed for the old concentration'
	CALL EXIT
ELSE
	WRITE(status_iounit,*)'allocation successful for old concentration'
ENDIF

WRITE(status_iounit,*)'completed component allocation for species number - ', i

END SUBROUTINE allocate_species_components

!#####################################################################################################

SUBROUTINE allocate_species(error_iounit,status_iounit)

	IMPLICIT NONE
	INTEGER :: alloc_stat,error_iounit,status_iounit

	ALLOCATE(species(max_species), STAT = alloc_stat)
	IF(alloc_stat /= 0)THEN
		WRITE(error_iounit,*)'allocation failed for the array of species - max_species=', max_species
		CALL EXIT
	ELSE
		WRITE(status_iounit,*)'allocation successful for array of species - max_species=', max_species
	ENDIF

END SUBROUTINE allocate_species

!###################################################################################

SUBROUTINE tvd_transport(species_num, c, c_old,status_iounit, error_iounit)

!----------------------------------------------------------------------------------
! uses EXPLICIT tvd scheme and split operator method
! needs "false" points before and after start and endpoints of link
!
! species = 1 is Total Dissolved Gas
! species = 2 is Ave. Water Temperature
! 
!----------------------------------------------------------------------------------
USE transport_vars , ONLY : k_surf, dxx
USE general_vars
USE link_vars
USE point_vars
USE linkbc_vars
USE logicals

USE met_data_module
USE energy_flux
USE tdg_equation_coeff
USE gas_functions

IMPLICIT NONE

INTEGER :: species_num
INTEGER :: status_iounit, error_iounit
REAL :: c(:,0:), c_old(:,0:)

INTEGER :: i,j,link,point,table_type
REAL :: velave,sum,cflx,s,corr,tmp,phi,dtdx,ave_vel
REAL :: f(maxpoint),table_interp
REAL :: k_e,k_w,area_e,area_w,flux_e,flux_w
REAL :: qspill,qgen 
REAL :: energy_source, depth
REAL :: tdg_saturation = 100.0, upstream_c
DOUBLE PRECISION :: salinity = 0.0

LOGICAL :: diffusion, fluvial, nonfluvial



!WRITE(*,*)'done with alloc; link loop'
!WRITE(*,*)c(1,0),c(1,151),dxx(1,0),dxx(1,151)
! run through links from top to bottom

links_forward: DO i=1,maxlinks

link = comporder(i)

!fluvial or nonfluvial link
SELECT CASE(linktype(link))
	CASE(1,20,21)
		fluvial = .TRUE.
		nonfluvial = .FALSE.
	CASE(2,3,4,5,6)
		fluvial = .FALSE.
		nonfluvial = .TRUE.
END SELECT
!----------------------------------------------------------------------------
!do nonfluvial first just pass through concentrations
! 
!IF(linktype(link) /= 1 )THEN
IF( nonfluvial )THEN
	DO j=1,num_con_links(link)
		c(link,1) =  c(con_links(link,j),maxpoints(con_links(link,j)))
	END DO

	IF((linktype(i) == 6) .AND. (species_num == 1))THEN
		table_type = 3 !generation flow
    qgen = table_interp(time,table_type,linkbc_table(link),time_mult)

    table_type = 4 !spill flow
    qspill = table_interp(time,table_type,linkbc_table(link),time_mult)

	IF(qspill > 0.0)THEN
		IF(do_temp .AND. temp_exchange) CALL update_met_data(time, met_zone(link))
		IF(do_temp) t_water = species(2)%conc(link,2)
		
!			equations are for %Sat and effective Spill Q in Kcfs
!			Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
!			gas_eqn_type = 1 general quadratic function
!			tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
!			OR 
!			gas_eqn_type = 2 exponetial equation
!			tdg = a_gas + b_gas*EXP(c_gas*Qspill)

		qspill = qspill + qgen_frac(link)*qgen
		qgen   = qgen - qgen_frac(link)*qgen

		SELECT CASE(gas_eqn_type(link))

		CASE(1)
			tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + c_gas(link)*(qspill/1000.0)**2 
			c(link,2) = SNGL(TDGasConcfromSat( DBLE(tdg_saturation), t_water, salinity, baro_press))
		CASE(2)
			tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
			c(link,2) = SNGL(TDGasConcfromSat( DBLE(tdg_saturation), t_water, salinity, baro_press))
		END SELECT
	ENDIF

! full mixing of spill and generation waters
	c(link,2) = (c(link,2)*qspill + c(link,1)*qgen)/(qspill+qgen)

	ELSE
	  c(link,2) =	c(link,1)
	ENDIF


!------ do fluvial links --------------------------------------
ELSE


!set distance difference array
IF(time == time_begin)THEN
	point = 1
	dxx(link,1) = ABS(x(link,point+1) - x(link,point))
	dxx(link,0) = dxx(link,1)
	DO point=2,maxpoints(link)-1

	dxx(link,point) = ABS(0.5*(x(link,point) - x(link,point-1))) +  &
	                  ABS(0.5*(x(link,point+1) - x(link,point)))
	END DO
	dxx(link,maxpoints(link)) =	ABS(x(link,maxpoints(link)) - x(link,maxpoints(link)-1))
	dxx(link,maxpoints(link)+1) = dxx(link,maxpoints(link)) 
	dxx(link,maxpoints(link)+2) = dxx(link,maxpoints(link))
ENDIF

! set upstream influx for the link from c(t) bc table or junction conditions
point = 1

SELECT CASE(species_num)
CASE(1) ! gas species
	IF(do_temp .AND. temp_exchange) CALL update_met_data(time, met_zone(link))
	IF(do_temp)THEN
		IF((num_con_links(link) == 0) .AND. (tempbc_table(link) /= 0))THEN
			table_type = 6
			t_water = table_interp(time,table_type,tempbc_table(link),time_mult)
		ELSE
			t_water = species(2)%conc(link,1)
		ENDIF
	ENDIF

	IF((num_con_links(link) == 0) .AND. (transbc_table(link) /= 0))THEN
		SELECT CASE(linktype(link))
		CASE(1)
			table_type = 2
			c(link,point) = table_interp(time,table_type,transbc_table(link),time_mult)
		
		CASE(20) ! %TDG Saturation is specified
			table_type = 2
			upstream_c = table_interp(time,table_type,transbc_table(link),time_mult)
			c(link,point) = SNGL(TDGasConcfromSat( DBLE(upstream_c), t_water, salinity, baro_press))


!-------------------------------------------------------------------
		CASE(21) ! Hydro project inflow link
			table_type = 3 !generation flow
			qgen = table_interp(time,table_type,linkbc_table(link),time_mult)

			table_type = 4 !spill flow
			qspill = table_interp(time,table_type,linkbc_table(link),time_mult)

		IF(qspill > 0.0)THEN
		
!			equations are for %Sat and effective Spill Q in Kcfs
!			Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
!			gas_eqn_type = 1 general quadratic function
!			tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
!			OR 
!			gas_eqn_type = 2 exponetial equation
!			tdg = a_gas + b_gas*EXP(c_gas*Qspill)

		qspill = qspill + qgen_frac(link)*qgen
		qgen   = qgen - qgen_frac(link)*qgen

		SELECT CASE(gas_eqn_type(link))
		CASE(1)
			tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + c_gas(link)*(qspill/1000.0)**2 
			c(link,point) = SNGL(TDGasConcfromSat( DBLE(tdg_saturation), t_water, salinity, baro_press))
		CASE(2)
			tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
			c(link,point) = SNGL(TDGasConcfromSat( DBLE(tdg_saturation), t_water, salinity, baro_press))
		END SELECT
	ENDIF
	table_type = 2
	upstream_c = table_interp(time,table_type,transbc_table(link),time_mult)
	upstream_c = SNGL(TDGasConcfromSat( DBLE(upstream_c), t_water, salinity, baro_press))

! full mixing of spill and generation waters
	c(link,point) = (c(link,point)*qspill + upstream_c*qgen)/(qspill+qgen)
!-----hydro inflow end

		END SELECT

	ELSE ! internal link
		sum = 0.0
		DO j=1,num_con_links(link)
			sum = sum + q(con_links(link,j),maxpoints(con_links(link,j)))*c(con_links(link,j),maxpoints(con_links(link,j)))
			c(link,point) = sum/q(link,point)
		END DO
	ENDIF

CASE(2) ! temerature species
	IF((num_con_links(link) == 0) .AND. (tempbc_table(link) /= 0))THEN
		table_type = 6
		c(link,point) = table_interp(time,table_type,tempbc_table(link),time_mult)
	ELSE ! internal link
		sum = 0.0
		DO j=1,num_con_links(link)
			sum = sum + q(con_links(link,j),maxpoints(con_links(link,j)))*c(con_links(link,j),maxpoints(con_links(link,j)))
			c(link,point) = sum/q(link,point)
		END DO
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
  corr = 0.5*(dxx(link,point+1)+dxx(link,point))/	 &
        (0.5*(dxx(link,point)+dxx(link,point-1)))
  s = corr *	&
      (c(link,point)-c(link,point-1))/f(point)
  tmp = min(2.d0,2.d0*s,0.33333333333333333d0*	 &
           (2.d0-cflx+(1.d0+cflx)*s))
  phi = max(0.0,tmp)
 else
  phi=0
 end if
   f(point)=velave*	 &
   ( c(link,point) + 0.5*(1.0-cflx)*phi*f(point) )
     
 else        !!! -ve velocity case
      f(point)=c(link,point)-c(link,point+1)
   if(abs(f(point)).gt.1.d-40)then
     corr = 0.5*(dxx(link,point+1)+dxx(link,point))/	&
            (0.5*(dxx(link,point+1)+dxx(link,point+2)))	  
     s=corr*						 &
       (c(link,point+1)-c(link,point+2))/f(point)	 
     phi=max(0.0d0,min(2.d0,2.d0*s,  &
         0.33333333333333333d0*		  &
        (2.d0-cflx+(1.d0+cflx)*s)))
   else
     phi=0
   end if
     f(point)=velave*( c(link,point+1) +	 &
           0.5*(1.0-cflx)*phi*f(point) )
   end if
                 
END DO

!update concentration values
DO point=2,maxpoints(link)-1
   !area = q(link,point)/vel(link,point)           
   dtdx = delta_t/(dxx(link,point))
              
   c(link,point)=c(link,point)*area_old(link,point)/area(link,point) - dtdx*(f(point) - f(point-1))/area(link,point)
   c_old(link,point) = c(link,point)                    
END DO

c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
c_old(link,maxpoints(link)) = c_old(link,maxpoints(link)-1)
!c(link,maxpoints(link)) = 0.0


!---------------------------------------------------------------------------
! Diffusion Step
!	explicit finite-volume solution
!
diffusion = .FALSE.
IF( (species_num == 1) .AND. gas_diffusion ) diffusion = .TRUE.
IF( (species_num == 2) .AND. temp_diffusion ) diffusion = .TRUE.
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
!
IF((species_num == 1) .AND. (gas_exchange) )THEN
	DO point=2,maxpoints(link)-1
		c(link,point) = c(link,point) + k_surf(link,point)*(100.0 - c(link,point))*delta_t
		IF(c(link,point) < 100.0) c(link,point) = 100.0
	END DO
	c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
ENDIF !degass if

!-------------------------------------------------------------------------
IF((species_num == 2) .AND. (temp_exchange) )THEN
	DO point=2,maxpoints(link)-1
		CALL update_met_data(time, met_zone(link))
		t_water = c(link,point)
		depth = y(link,point) - thalweg(link,point)
		energy_source = net_heat_flux(net_solar, t_water, t_air, t_dew, windspeed) &
										/(1000.0*4186.0/3.2808) ! rho*specifc heat*depth in feet

		c(link,point) = c(link,point) + energy_source*delta_t/depth
		IF(c(link,point) <   0.0) c(link,point) =   0.0 ! frozen - should add warning printout
		IF(c(link,point) > 100.0) c(link,point) = 100.0 ! boiling - should add warning printout
	END DO
	c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
ENDIF

ENDIF !fluvial links if

END DO  links_forward

!-------------------------------------------------------------------------------
! 

!IF(time==time_end)THEN
!	DEALLOCATE(b,m)
!ENDIF

END SUBROUTINE tvd_transport

END MODULE scalars
