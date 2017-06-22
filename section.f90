
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME:	section
!
! VERSION and DATE: MASS1 v0.61 11/21/1997
!
! PURPOSE: compute or table look-up for section geometry data
!       
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
! MOD HISTORY: changed return to include hydraulic radius; mcr 11/21/1997
!
!
!***************************************************************
!

!SUBROUTINE section(link,point,depth,area_temp,width,conveyance,dkdy)

SUBROUTINE section(link,point,depth,area_temp,width,conveyance,dkdy,hydrad)

!
  USE utility
  USE section_vars
  USE general_vars
  USE point_vars

!
        IMPLICIT NONE

        INTEGER :: i,j,link,point
        DOUBLE PRECISION :: area_temp,depth,width,perm,hydrad,dpdy,dkdy,conveyance, factor
        CHARACTER (LEN=1024) :: msg
!NOTE: y is DEPTH here NOT STAGE as in flow sim
! match section number to the right section info given
! the link and point we are at
!
        DO i=1,total_sections
          IF(section_id(i) == section_number(link,point)) EXIT
        END DO

        IF (i .GT. total_sections) THEN
           WRITE(msg,*) 'Cannot find section id ', section_number(link, point),&
                &' for link ', link, ', point ', point
           CALL error_message(msg, fatal=.TRUE.)
        END IF
           
                  
        IF (depth .LE. 0.0) THEN
           WRITE(msg, '("section: non-positive depth (", G12.6, "): link ", I3, ", point ", I3)')&
                &depth, link, point
           CALL error_message(msg, fatal=.TRUE.)
           STOP
        END IF

        SELECT CASE(section_type(i))

        CASE(1) ! rectangular channel

          area_temp = depth*bottom_width(i)
          perm = 2*depth + bottom_width(i)
          width = bottom_width(i)
          hydrad = area_temp/perm
          dpdy = 2.0
		  conveyance = res_coeff*kstrick(link,point)*(area_temp**(5./3.))/(perm**(2./3.))
		  dkdy =  conveyance*(5.0*width/area_temp - 4.0/perm)/3.0

        CASE(2) ! Compound rectangular section for main channel and floodplain

          IF(depth <= depth_main(i) )THEN

            area_temp = depth*bottom_width(i)
            perm = 2*depth+bottom_width(i)
            width = bottom_width(i)
            hydrad = area_temp/perm
            dpdy = 2.0

          ELSE

            area_temp = depth_main(i)*bottom_width(i) + (depth - depth_main(i))*bottom_width_flood(i)
            perm = 2*depth+bottom_width_flood(i)
            width = bottom_width_flood(i)
            hydrad = area_temp/perm
            dpdy = 2.0

          ENDIF

		CASE(3) ! Trapeziodal Section

		CASE(4) ! Circular Section

		CASE(50) ! Tabular Defined - Natural Channel - Table set-up in kick_off routine

		! tabel look up and linear interp in the table

		! figure out where to interpolate in the table
		! interpolate past first and last levels in the table

		j = AINT(depth/delta_y(i)) + 1

		IF(j < 1) j=1
		IF(j >= sect_levels(i)) j=sect_levels(i) - 1

		factor = (depth - delta_y(i)*(j-1))/delta_y(i)

		area_temp = factor*(sect_area(i,j+1)-sect_area(i,j))+sect_area(i,j) 
                perm = 	factor*(sect_perm(i,j+1)-sect_perm(i,j))+sect_perm(i,j)
                width = factor*(sect_width(i,j+1)-sect_width(i,j))+sect_width(i,j)
                hydrad = factor*(sect_hydradius(i,j+1)-sect_hydradius(i,j))+sect_hydradius(i,j) 
        !dpdy = 2.0
		conveyance = res_coeff*kstrick(link,point)*(factor*(sect_convey(i,j+1)-sect_convey(i,j))+sect_convey(i,j))
		dkdy = res_coeff*kstrick(link,point)*(sect_convey(i,j+1) - sect_convey(i,j))/delta_y(i)

        END SELECT

        area_temp = MAX(area_temp, 0.0D00)
        width = MAX(width, 0.0D00)
        conveyance = MAX(conveyance, 0.0D00)
        hydrad = MAX(hydrad, 0.0D00)
END SUBROUTINE section
