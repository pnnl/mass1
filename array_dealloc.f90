
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	array_dealloc
!
! VERSION and DATE: MASS1 v0.75 3/25/98
!
! PURPOSE: deallocation of arrays after completion of run
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
! MOD HISTORY: dealloc for top_width,hyd_radius, etc; mcr 11/21/1997
!			   lateral inflows; mcr 3/25/98
!
!***************************************************************
!

SUBROUTINE array_dealloc

USE flow_coeffs
USE linkbc_vars
USE link_vars
USE point_vars
USE section_vars
USE transport_vars

IMPLICIT NONE

!----------------------------------------------------------
!flow coeff module

DEALLOCATE(e,f,l,m,n)

!----------------------------------------------------------
!MODULE linkbc_vars
	
DEALLOCATE(linkbc_time,linkbc)
DEALLOCATE(transbc_time,transbc)
DEALLOCATE(gen_time,gen_flow)
DEALLOCATE(spill_time,spill_flow)
DEALLOCATE(latflowbc_time,latflowbc)
DEALLOCATE(tempbc_time,tempbc)

!----------------------------------------------------------
!module link_vars

DEALLOCATE(maxpoints,linkname,linkorder,linktype,input_option)
DEALLOCATE(linkbc_table,num_con_links,con_links,ds_conlink)
DEALLOCATE(comporder,dsbc_table,transbc_table,tempbc_table)
DEALLOCATE(latflowbc_table, met_zone)

!-----------------------------------------------------------
!module point_vars

DEALLOCATE(x, q, q_old)
DEALLOCATE(thalweg,y,manning,vel)
DEALLOCATE(kstrick, area, area_old)
DEALLOCATE(k_diff)
DEALLOCATE(lateral_inflow)
!DEALLOCATE(top_width,hyd_radius,froude_num,friction_slope,bed_shear)

!-----------------------------------------------------------
!module sections_vars

DEALLOCATE(section_id,section_type,delta_y,sect_levels)
DEALLOCATE(section_number)
 
DEALLOCATE(bottom_width,bottom_width_flood)
DEALLOCATE(depth_main)

DEALLOCATE(sect_area,sect_hydradius,sect_depth)
DEALLOCATE(sect_width,sect_convey,sect_perm)

!----------------------------------------------------------
!MODULE transport_vars

DEALLOCATE(c)
DEALLOCATE(dxx)
DEALLOCATE(k_surf)
DEALLOCATE(temp)



END SUBROUTINE array_dealloc