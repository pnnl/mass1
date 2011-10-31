
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	section_table
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: compute table of geometric info for natural sections
!          defined as x-y pairs
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
! MOD HISTORY:
!
!
!***************************************************************
!

SUBROUTINE section_table(sec_id,num_pairs,del_y,xy)

! routine to compute table of geometric information for
! each cross section of type=5

! actual table look-up during calculations is done from section.f90

! USE  NUMERICAL_LIBRARIES	! use the ms imsl single prec. library

USE section_vars
USE logicals

IMPLICIT NONE

INTEGER :: i,j,count
INTEGER :: sec_id,num_pairs,num_levels
INTEGER :: iperm(num_pairs)
DOUBLE PRECISION :: linear_interp
DOUBLE PRECISION :: del_y,xy(2*maxpairs),x(num_pairs),y(num_pairs),y_new(num_pairs)
DOUBLE PRECISION :: width(num_pairs),width_new(num_pairs),level,x_inter,max_elv,min_elv
LOGICAL :: last_point_wet, unique, nonmono

! divide xy vector into x,y pairs
count = 0
DO i=1,2*num_pairs-1,2
	count = count + 1
	x(count) = xy(i)
	y(count) = xy(i+1)
END DO

! transform x,y pairs to level-width pairs, can jump over this
! if the input data were specified in level-width format

DO i=1,num_pairs  ! pick the level to work on

level = y(i)
width(i) = 0.0

! wet if point is at or below level
! dry if point is above level

last_point_wet = .FALSE.

j = 1

IF(y(j) <= level) last_point_wet = .TRUE.

DO j = 2,num_pairs	! loop through rest of levels and add up width

IF(last_point_wet)THEN
	IF(y(j) <= level)THEN
		width(i) = width(i) + ABS(x(j) - x(j-1))
		last_point_wet = .TRUE.
	ELSE
		x_inter = linear_interp(x(j-1),y(j-1),x(j),y(j),level)
		width(i) = width(i) + ABS(x(j-1) - x_inter)
		last_point_wet = .FALSE.
	ENDIF

ELSE  ! last point was dry
	IF(y(j) <= level)THEN
		x_inter = linear_interp(x(j),y(j),x(j-1),y(j-1),level)
		width(i) = width(i) + ABS(x(j) - x_inter)
		last_point_wet = .TRUE.
	ELSE
		width(i) = width(i)
		last_point_wet = .FALSE.
	ENDIF

ENDIF

END DO

END DO

! first sort from bottom to top and then eliminate duplicate levels
iperm = (/(i,i=1,num_pairs)/) ! must initialize for IMSL to use

CALL SVRGP(num_pairs,y,y_new,iperm)	! IMSL routine to sort

width_new = width(iperm)

! cull out non-unique values; vector MUST be sorted first or this method will fail
count = 2
DO i=2,num_pairs
IF(y_new(i) /= y_new(i-1))THEN
y_new(count) = y_new(i)
width_new(count) = width_new(i)
count = count + 1
ENDIF
END DO
count = count - 1  ! reset to the new number of unique levels


! figure out number of levels needed to cover elevation range at
! the specified delta_y interval for this section
max_elv = 0.0
min_elv = 1.0e6

DO i =1,count
IF(y_new(i) < min_elv) min_elv = y_new(i)
IF(y_new(i) >= max_elv) max_elv = y_new(i)
END DO

num_levels = AINT((max_elv - min_elv)/del_y) + 1

sect_levels(sec_id) = num_levels

! compute depth, width, area, hydraulic radius, geo-conveyance
! at each depth level

sect_depth(sec_id,1) = 0.0
sect_width(sec_id,1) = 0.0
sect_area(sec_id,1) = 0.0
sect_perm(sec_id,1) = 0.0
sect_hydradius(sec_id,1) = 0.0
DO i = 2, num_levels
   sect_depth(sec_id,i) = sect_depth(sec_id,i-1) + del_y
   level = min_elv + sect_depth(sec_id,i)
   
   ! figure out where the level is in the level-width table
   DO j = 1,count-1
      IF((level >= y_new(j)) .AND. (level <= y_new(j+1))) EXIT
   END DO
   sect_width(sec_id,i) = linear_interp(width_new(j),y_new(j),width_new(j+1),y_new(j+1),level)
   
   IF(i == 1)THEN
      sect_area(sec_id,i) = del_y*0.5*(width_new(1)+sect_width(sec_id,i))
      sect_perm(sec_id,i) = width_new(1) + SQRT(4*del_y**2 + (sect_width(sec_id,i) - width_new(1))**2)
      
   ELSE
      sect_area(sec_id,i) = del_y*0.5*(sect_width(sec_id,i)+sect_width(sec_id,i-1)) &
           + sect_area(sec_id,i-1)
      sect_perm(sec_id,i) = sect_perm(sec_id,i-1) &
           + SQRT(4*del_y**2 + (sect_width(sec_id,i) - sect_width(sec_id,i-1))**2)
   ENDIF
   sect_hydradius(sec_id,i) = sect_area(sec_id,i)/sect_perm(sec_id,i)
   
   ! geo-conveyance
   
   sect_convey(sec_id,i) = sect_area(sec_id,i)*sect_hydradius(sec_id,i)**(2.0/3.0)

END DO

! adjust the geo-conveyance to be sure that it
! monotonically increases with depth
! linear extrapolation from previous 2 conveyance points

DO i = 2,num_levels

IF(sect_convey(sec_id,i) < sect_convey(sec_id,i-1))THEN
nonmono = .true.
level = sect_depth(sec_id,i)
IF(i == 2)THEN
sect_convey(sec_id,i) = linear_interp(0.0,0.0,sect_depth(sec_id,i-1),sect_convey(sec_id,i-1),level)

ELSE

sect_convey(sec_id,i) = linear_interp(sect_convey(sec_id,i-2),sect_depth(sec_id,i-2),&
     & sect_convey(sec_id,i-1),sect_depth(sec_id,i-1),level)

ENDIF

END IF

END DO


!-----------------------------------------------------------------
! optionally print-out section information
IF(print_sections)THEN

OPEN(90,file='section-geometry.out')
WRITE(90,*)'/////////////////////////////////////////////////////////////////////////////'
WRITE(90,*)''

WRITE(90,*)'section id number - ',section_id(sec_id)
WRITE(90,*)'number of pairs - ',num_pairs
WRITE(90,*)'maximum elevation - ',max_elv
WRITE(90,*)'minimum elevation - ',min_elv
WRITE(90,*)'depth interval to build table - ',del_y
WRITE(90,*)'number of levels - ',num_levels
IF(nonmono) WRITE(90,*)'!!!! geo-conveyance adjusted to be monotonically increasing'
WRITE(90,*)''
WRITE(90,*)'*** distance-level pairs specified ***'
DO i=1,num_pairs
	WRITE(90,1000)x(i),y(i)
END DO

WRITE(90,*)''
WRITE(90,*)'*** level-width pairs computed ***'
DO i=1,count
	WRITE(90,1000)y_new(i),width_new(i)
END DO

WRITE(90,*)''
WRITE(90,*)'------------------------------------------------------'
WRITE(90,*)'           Computed Geometric Properties'
WRITE(90,1015)
DO i=1,num_levels
WRITE(90,1010)i,sect_depth(sec_id,i),sect_width(sec_id,i),sect_area(sec_id,i),&
     & sect_perm(sec_id,i),sect_hydradius(sec_id,i),sect_convey(sec_id,i)
END DO
WRITE(90,*)''

1000 FORMAT(2x,2(f10.2,2x),5x,2(f10.2))
1010 FORMAT(2x,i4,6(f12.3,2x))
1015 FORMAT(2x,'Level',6x,'Depth',6x,'Width',9x,'Area',8x,'Wetted-Perim',4x,'Hydr Radius',2x,'Geo-Conveyance')

ENDIF

! optionally save binary version of tables for a read-in 



END SUBROUTINE section_table
