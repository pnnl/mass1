
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
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
! Last Change: Mon Feb 21 11:54:55 2011 by William A. Perkins <d3g096@PE10900.pnl.gov>
!----------------------------------------------------------
MODULE general_vars

  DOUBLE PRECISION, SAVE :: time
  ! INTEGER, SAVE :: print_freq
  DOUBLE PRECISION, SAVE :: depth_threshold, depth_minimum

END MODULE general_vars
!----------------------------------------------------------
MODULE link_vars

  USE met_zone
  USE bc_module

  INTEGER, DIMENSION(:),ALLOCATABLE, SAVE :: maxpoints,linkname,linkorder,comporder,linktype,input_option
  INTEGER, DIMENSION(:),ALLOCATABLE, SAVE :: linkbc_table,ds_conlink,&
       & dsbc_table,transbc_table,tempbc_table,latflowbc_table,&
       &lattransbc_table,lattempbc_table
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: crest

  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: lpiexp

  TYPE (bc_ptr), DIMENSION(:), ALLOCATABLE, SAVE :: usbc, dsbc, latbc
  
  TYPE (met_zone_ptr), DIMENSION(:), ALLOCATABLE, SAVE :: metzone

END MODULE link_vars

!----------------------------------------------------------
MODULE point_vars

  USE cross_section

  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE :: x, q,thalweg,y,manning,vel,kstrick
  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE :: area, area_old, q_old,y_old,k_diff
  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE :: top_width, hyd_radius, froude_num, friction_slope, bed_shear
  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE :: lateral_inflow, lateral_inflow_old
  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE :: courant_num, diffuse_num
  INTEGER, DIMENSION(:,:),ALLOCATABLE, SAVE :: section_number

  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE :: e,f,l,m,n

  TYPE (xsection_ptr), DIMENSION(:,:), ALLOCATABLE, SAVE :: ptsection

END MODULE point_vars

!---------------------------------------------------------
MODULE transport_vars

  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: c
  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: dxx
  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: temp

END MODULE transport_vars
