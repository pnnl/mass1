!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be found
! in the LICENSE file in the top level directory of this distribution.
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
! Last Change: Mon Feb 21 11:54:50 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
! 

SUBROUTINE array_alloc

! $DEBUG

USE mass1_config
USE link_vars
USE point_vars
USE transport_vars
USE hydro_output_module
USE confluence_module

IMPLICIT NONE

INTEGER :: maxlinks, maxpoint, i
maxlinks = config%maxlinks
maxpoint = config%maxpoint

!----------------------------------------------------------
!flow coeff module

ALLOCATE(e(maxlinks,maxpoint),f(maxlinks,maxpoint),l(maxlinks,maxpoint),&
     & m(maxlinks,maxpoint),n(maxlinks,maxpoint))

!----------------------------------------------------------
!module link_vars

ALLOCATE(maxpoints(maxlinks),linkname(maxlinks),linkorder(maxlinks),linktype(maxlinks),input_option(maxlinks))
ALLOCATE(linkbc_table(maxlinks),ds_conlink(maxlinks))
ALLOCATE(comporder(maxlinks),dsbc_table(maxlinks),transbc_table(maxlinks),tempbc_table(maxlinks))
ALLOCATE(usbc(maxlinks), dsbc(maxlinks), latbc(maxlinks))
DO i = 1, maxlinks
   NULLIFY(usbc(i)%p)
   NULLIFY(dsbc(i)%p)
   NULLIFY(latbc(i)%p)
END DO
ALLOCATE(latflowbc_table(maxlinks), metzone(maxlinks))
latflowbc_table = 0
ALLOCATE(lattransbc_table(maxlinks), lattempbc_table(maxlinks))
lattransbc_table = 0
lattempbc_table = 0

ALLOCATE(crest(maxlinks))

crest = -999.0

ALLOCATE(lpiexp(maxlinks))
lpiexp = 0.0

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
ALLOCATE(courant_num(maxlinks,maxpoint), diffuse_num(maxlinks,maxpoint))
ALLOCATE(section_number(maxlinks,maxpoint))
ALLOCATE(ptsection(maxlinks, maxpoint))
ALLOCATE(y_old(maxlinks,maxpoint))
ALLOCATE(ucon(maxlinks), dcon(maxlinks))

DO i = 1, maxlinks
   NULLIFY(ucon(i)%p)
   NULLIFY(dcon(i)%p)
END DO

!MODULE transport_vars

ALLOCATE(c(maxlinks,0:maxpoint+2))
ALLOCATE(dxx(maxlinks,0:maxpoint+2))
ALLOCATE(temp(maxlinks,0:maxpoint+2))

!----------------------------------------------------------
!MODULE hydro_output_module

ALLOCATE(hydro_spill(maxlinks), hydro_gen(maxlinks), hydro_disch(maxlinks),&
     &hydro_conc(maxlinks), hydro_sat(maxlinks), hydro_temp(maxlinks), hydro_baro(maxlinks))

!----------------------------------------------------------
END SUBROUTINE array_alloc
