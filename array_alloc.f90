
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	array_alloc
!
! VERSION and DATE: MASS1 v0.75 3/25/98
!
! PURPOSE: dynamic allocation of arrays
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
! MOD HISTORY: added variables for uniform lateral inflow; mcr 3/25/98
!
!
!***************************************************************
! CVS ID: $Id$
! Last Change: Tue May  1 13:15:18 2001 by William A. Perkins <perk@gehenna.pnl.gov>
! 

SUBROUTINE array_alloc

! $DEBUG

USE flow_coeffs
USE linkbc_vars
USE link_vars
USE point_vars
USE section_vars
USE transport_vars
USE general_vars, ONLY : maxlinks,maxpoint,maxtable,maxtimes
USE hydro_output_module

IMPLICIT NONE

!INTEGER :: maxsections,maxlinks,maxpoint,maxtable,maxtimes
INTEGER :: maxsections

maxsections = total_sections

!----------------------------------------------------------
!flow coeff module

ALLOCATE(e(maxlinks,maxpoint),f(maxlinks,maxpoint),l(maxlinks,maxpoint),&
     & m(maxlinks,maxpoint),n(maxlinks,maxpoint))

!----------------------------------------------------------
!MODULE linkbc_vars
	
ALLOCATE(linkbc_time(maxtimes,maxtable),linkbc(maxtimes,maxtable))
ALLOCATE(transbc_time(maxtimes,maxtable),transbc(maxtimes,maxtable))
ALLOCATE(gen_time(maxtimes,maxtable),gen_flow(maxtimes,maxtable))
ALLOCATE(spill_time(maxtimes,maxtable),spill_flow(maxtimes,maxtable))
ALLOCATE(latflowbc_time(maxtimes,maxtable),latflowbc(maxtimes,maxtable))
ALLOCATE(tempbc_time(maxtimes,maxtable),tempbc(maxtimes,maxtable))
ALLOCATE(linkbc_header(maxtable),transbc_header(maxtable),tempbc_header(maxtable))
ALLOCATE(hydrobc_header(maxtable),latflowbc_header(maxtable))

                                ! allocate and initialize search start index

ALLOCATE(linkbc_start(maxtable), transbc_start(maxtable), tempbc_start(maxtable),&
     & latflowbc_start(maxtable), gen_start(maxtable), spill_start(maxtable))

linkbc_start = 0
transbc_start = 0
tempbc_start = 0
latflowbc_start = 0
gen_start = 0
spill_start = 0

!----------------------------------------------------------
!module link_vars

ALLOCATE(maxpoints(maxlinks),linkname(maxlinks),linkorder(maxlinks),linktype(maxlinks),input_option(maxlinks))
ALLOCATE(linkbc_table(maxlinks),num_con_links(maxlinks),con_links(maxlinks,5),ds_conlink(maxlinks))
ALLOCATE(comporder(maxlinks),dsbc_table(maxlinks),transbc_table(maxlinks),tempbc_table(maxlinks))
ALLOCATE(latflowbc_table(maxlinks), met_zone(maxlinks))
ALLOCATE(crest(maxlinks))

crest = -999.0

!-----------------------------------------------------------
!module point_vars

ALLOCATE(x(maxlinks,maxpoint), q(maxlinks,maxpoint), q_old(maxlinks,maxpoint))
ALLOCATE(thalweg(maxlinks,maxpoint),y(maxlinks,maxpoint),manning(maxlinks,maxpoint),vel(maxlinks,maxpoint))
ALLOCATE(kstrick(maxlinks,maxpoint), area(maxlinks,maxpoint), area_old(maxlinks,maxpoint))
ALLOCATE(k_diff(maxlinks,maxpoint))
ALLOCATE(top_width(maxlinks,maxpoint),hyd_radius(maxlinks,maxpoint),&
     & froude_num(maxlinks,maxpoint),friction_slope(maxlinks,maxpoint),&
     & bed_shear(maxlinks,maxpoint))
ALLOCATE(lateral_inflow(maxlinks,maxpoint), lateral_inflow_old(maxlinks,maxpoint))
ALLOCATE(y_old(maxlinks,maxpoint))
!-----------------------------------------------------------
!module sections_vars

ALLOCATE(section_id(maxsections),section_type(maxsections),delta_y(maxsections),sect_levels(maxsections))
ALLOCATE(section_number(maxlinks,maxpoint))
 
ALLOCATE(bottom_width(maxsections),bottom_width_flood(maxsections))
ALLOCATE(depth_main(maxsections))
ALLOCATE(sect_area(maxsections,maxlevels),sect_hydradius(maxsections,maxlevels),sect_depth(maxsections,maxlevels))
ALLOCATE(sect_width(maxsections,maxlevels),sect_convey(maxsections,maxlevels),sect_perm(maxsections,maxlevels))

!----------------------------------------------------------
!MODULE transport_vars

ALLOCATE(c(maxlinks,0:maxpoint+2))
ALLOCATE(dxx(maxlinks,0:maxpoint+2))
ALLOCATE(k_surf(maxlinks,maxpoint))
ALLOCATE(temp(maxlinks,0:maxpoint+2))

!----------------------------------------------------------
!MODULE hydro_output_module

ALLOCATE(hydro_spill(maxlinks), hydro_gen(maxlinks), hydro_disch(maxlinks),&
     &hydro_conc(maxlinks), hydro_sat(maxlinks), hydro_temp(maxlinks), hydro_baro(maxlinks))

!----------------------------------------------------------
END SUBROUTINE array_alloc
