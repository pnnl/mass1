
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	modules
!
! VERSION and DATE: MASS1 v0.75 3/25/1998
!
! PURPOSE: contains all module-level variable declarations
!
! RETURNS: void
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY:
!	changed maxlevels to 1500 from 1000; mcr 10/16/1997
!   added variables for bed shear, friction slope, froude number,
!         top width - enhanced output; mcr 11/21/1997
!   added variables for uniform lateral inflows; mcr 3/25/98
!
!
!***************************************************************
! CVS ID: $Id$
! Last Change: Wed Nov  8 20:16:48 2000 by William A. Perkins <perk@localhost>
!----------------------------------------------------------
MODULE general_vars



	DOUBLE PRECISION, SAVE :: time,time_begin,time_end,delta_t,time_mult
	INTEGER, SAVE :: units, channel_length_units
	INTEGER, SAVE :: time_units,mass_check,debug_print
	INTEGER, SAVE :: lateral_inflow_flag, sediment_flag, contaminant_flag
    INTEGER, SAVE :: diffusion_flag,degass_flag,infiltration_flag
	INTEGER, SAVE :: print_output_flag, plot_output_flag
	INTEGER, SAVE :: maxlinks,maxpoint,maxtable,maxtimes,scalar_steps
	INTEGER, SAVE :: dsbc_type
	REAL, SAVE :: res_coeff,grav, delta_x
    REAL, SAVE :: unit_weight_h2o,density_h2o
    CHARACTER (LEN =120) :: title,version,config_version

	INTEGER, SAVE :: print_freq

END MODULE general_vars
!------------------------------------------------------------------
MODULE date_vars

INTEGER, SAVE :: time_option
CHARACTER (LEN=10) :: date_string, date_run_begins, date_run_ends
CHARACTER (LEN=8) :: time_string, time_run_begins, time_run_ends


END MODULE date_vars

!----------------------------------------------------------
MODULE logicals

LOGICAL, SAVE :: do_flow,do_gas,do_temp,do_printout,do_gageout,do_profileout
LOGICAL, SAVE :: do_restart,do_hotstart
LOGICAL, SAVE :: temp_diffusion, temp_exchange
LOGICAL, SAVE :: gas_diffusion, gas_exchange
LOGICAL, SAVE :: print_sections,write_sections,read_sections
LOGICAL, SAVE :: do_latflow
LOGICAL, SAVE :: file_exist
LOGICAL, SAVE :: do_accumulate

END MODULE logicals

!----------------------------------------------------------
MODULE file_vars

	CHARACTER (LEN = 100), SAVE :: filename(20)
	INTEGER, SAVE :: ii,fileunit(20) = (/(ii,ii=20,39)/)

END MODULE file_vars

!-----------------------------------------------------

MODULE linkbc_vars
	
  DOUBLE PRECISION, ALLOCATABLE,SAVE :: linkbc_time(:,:),transbc_time(:,:),tempbc_time(:,:),latflowbc_time(:,:)
  REAL, ALLOCATABLE,SAVE :: linkbc(:,:),transbc(:,:),tempbc(:,:),latflowbc(:,:)
  INTEGER, ALLOCATABLE, SAVE :: linkbc_start(:), transbc_start(:), tempbc_start(:), latflowbc_start(:)
  DOUBLE PRECISION, ALLOCATABLE, SAVE :: gen_time(:,:),spill_time(:,:)
  REAL, ALLOCATABLE, SAVE :: gen_flow(:,:),spill_flow(:,:)
  INTEGER, ALLOCATABLE, SAVE :: gen_start(:), spill_start(:)
  
  CHARACTER(LEN=100), ALLOCATABLE :: linkbc_header(:),transbc_header(:),tempbc_header(:),latflowbc_header(:)
  CHARACTER(LEN=100), ALLOCATABLE :: hydrobc_header(:)

  !REAL, ALLOCATABLE,SAVE :: inflow_time(maxtimes),inflow(maxtimes,maxtables)
  !REAL, SAVE :: contam_inflow(maxtimes,maxtables),contam_inflow_time(maxtimes)
  !REAL, SAVE :: sedi_inflow_time(maxtimes),sedi_inflow(maxtimes,maxtables,2)

END MODULE linkbc_vars

!----------------------------------------------------------
MODULE link_vars

	
INTEGER, DIMENSION(:),ALLOCATABLE, SAVE :: maxpoints,linkname,linkorder,comporder,linktype,input_option
INTEGER, DIMENSION(:),ALLOCATABLE, SAVE :: linkbc_table,num_con_links,ds_conlink,&
     & dsbc_table,transbc_table,tempbc_table,latflowbc_table
INTEGER, DIMENSION(:),ALLOCATABLE, SAVE :: met_zone
INTEGER, DIMENSION(:,:),ALLOCATABLE, SAVE :: con_links
	!INTEGER, SAVE ::  maxpoints(lmax),linkorder(lmax)
	!INTEGER, SAVE :: linkbc_table(lmax),num_con_links(lmax),con_links(lmax,5),ds_conlink(lmax)
    !INTEGER, SAVE :: comporder(lmax)

END MODULE link_vars

!----------------------------------------------------------
MODULE point_vars

REAL, DIMENSION(:,:),ALLOCATABLE, SAVE :: x, q,thalweg,y,manning,vel,kstrick
REAL, DIMENSION(:,:),ALLOCATABLE, SAVE :: area, area_old, q_old,y_old,k_diff
REAL, DIMENSION(:,:),ALLOCATABLE, SAVE :: top_width, hyd_radius, froude_num, friction_slope, bed_shear
REAL, DIMENSION(:,:),ALLOCATABLE, SAVE :: lateral_inflow, lateral_inflow_old
!INTEGER, DIMENSION(:,:),ALLOCATABLE, SAVE ::	

	!REAL, SAVE :: x(lmax,pmax), q(lmax,pmax),qold(lmax,pmax)
    !REAL, SAVE :: thalweg(lmax,pmax),y(lmax,pmax),manning(lmax,pmax), v(lmax,pmax)
    !REAL, SAVE :: kstrick(lmax,pmax)
	!INTEGER, SAVE :: lat_q_table(lmax,pmax)

END MODULE point_vars

!----------------------------------------------------------
! global data module for cross section variables
!
MODULE section_vars

INTEGER, PARAMETER :: maxpairs=1000,maxlevels=3500
INTEGER, SAVE :: total_sections
INTEGER, DIMENSION(:),ALLOCATABLE, SAVE :: section_id,section_type
INTEGER, DIMENSION(:,:),ALLOCATABLE, SAVE :: section_number
REAL, DIMENSION(:),ALLOCATABLE, SAVE :: bottom_width,bottom_width_flood,depth_main
REAL, DIMENSION(:),ALLOCATABLE, SAVE :: delta_y,sect_levels


REAL, DIMENSION(:,:),ALLOCATABLE, SAVE :: sect_area,sect_hydradius,sect_depth,sect_width,sect_convey,sect_perm
	
    !INTEGER, SAVE :: section_id(maxsections),section_type(maxsections)
	!INTEGER, SAVE :: section_number(lmax,pmax), total_sections
 
    !REAL, SAVE :: bottom_width(maxsections),bottom_width_flood(maxsections)
    !REAL, SAVE :: depth_main(maxsections)


END MODULE section_vars

!----------------------------------------------------------
MODULE flow_coeffs

REAL, DIMENSION(:,:),ALLOCATABLE, SAVE :: e,f,l,m,n

END MODULE flow_coeffs


!----------------------------------------------------------
MODULE fluvial_coeffs

! REAL, SAVE :: alpha=1.0,beta=0.5,theta=1.0,q1,q2,a1,a2,b1,b2,k1,k2	&
!               ,ky1,ky2,y2,y1
REAL, SAVE :: alpha=1.0,beta=0.5,theta=1.0,q1,q2,a1,a2,b1,b2,k1,k2,ky1,ky2,y2,y1

END MODULE fluvial_coeffs

!---------------------------------------------------------
MODULE transport_vars

REAL, DIMENSION(:,:), ALLOCATABLE, SAVE :: c,k_surf
REAL, DIMENSION(:,:), ALLOCATABLE, SAVE :: dxx
REAL, DIMENSION(:,:), ALLOCATABLE, SAVE :: temp

END MODULE transport_vars
