
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	profile_output
!
! VERSION and DATE: MASS1 v0.61 11/21/1997
!
! PURPOSE: prints profile files for each specified profile
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
! MOD HISTORY: added hyd_radius, friction_slope, etc. ; mcr 11/21/1997
!
!
!***************************************************************
!

SUBROUTINE profile_output

! Routine to write out the files for variables along a given
! profile through the system.
! Example: Plot water level vs. river mile
! Can define multiple paths through the model
! writes to a separate file for each profile

! $DEBUG

USE link_vars
USE general_vars
USE point_vars
USE file_vars
USE transport_vars
USE date_vars

USE scalars
USE gas_functions
USE met_data_module

IMPLICIT NONE

INTEGER, PARAMETER :: maxpro=10

REAL :: depth,x_pro_start(maxpro)
REAL :: tdg_press, tdg_sat
DOUBLE PRECISION :: salinity = 0.0
REAL, ALLOCATABLE, SAVE, DIMENSION(:,:) :: x_profile
INTEGER,ALLOCATABLE,SAVE, DIMENSION(:,:) :: profile_link,profile_point

INTEGER :: i,j,link,point,profile(maxpro,100)
INTEGER, SAVE :: num_profiles
INTEGER :: profile_max=0,count=0
INTEGER, SAVE :: profile_max_points(maxpro)
INTEGER :: profile_num_links(maxpro)

INTEGER :: len1,len2,spot1,spot2
CHARACTER*20 fname,string1,string2,input_str
CHARACTER*4 profile_x_units(maxpro)
        
IF(time == time_begin )THEN
        
	count = 0
	OPEN(fileunit(15),file=filename(15))
	DO WHILE(.TRUE.)
	count=count+1	
	READ(fileunit(15),*,END=100)profile_num_links(count),profile_x_units(count),x_pro_start(count)
	
	READ(fileunit(15),*)profile(count,1:profile_num_links(count))
	
	END DO
100	CLOSE(fileunit(15))
        IF (count .gt. 0) count = count - 1
	num_profiles=count

! open the files for each profile

	DO i=1,num_profiles
	count = 80 + i
        fname = ''
	fname(1:7) = 'profile'
	WRITE(string1,*)i
	string1 = ADJUSTL(string1)
	len1 = LEN_TRIM(string1)
	spot2 =8 + len1 - 1
	fname(8:spot2) = string1(1:len1)
	spot1 = spot2 + 1
	spot2 = spot1 + LEN_TRIM('.out')
	fname(spot1:spot2) = '.out'
	OPEN(count,file=fname)
	END DO

! compute the relative x distance from start to end on each
! profile. 

	DO i=1,num_profiles
	profile_max_points(i) = 0
	DO j=1,profile_num_links(i)
	link = profile(i,j)
	profile_max_points(i) =  profile_max_points(i) + maxpoints(link)

	END DO

	profile_max = MAX(profile_max_points(i),profile_max)

	END DO

	ALLOCATE(x_profile(maxpro,profile_max),profile_link(maxpro,profile_max),profile_point(maxpro,profile_max))

! figure out how the relative x distance along the profile
! figure out correspondence with profile point to link,point

	

	DO i=1,num_profiles
	count = 0

	IF(profile_x_units(i) == 'RM')THEN
		x_pro_start(i) = x_pro_start(i)*5280.0
	ENDIF

	DO j=1,profile_num_links(i)
	link = profile(i,j)
	DO point=maxpoints(link),1,-1
	count = count + 1
	profile_link(i,count) = link
	profile_point(i,count) = point
	IF(j == 1)THEN
		x_profile(i,count) = x_pro_start(i)
	ENDIF
	IF((j > 1) .AND. (point == maxpoints(link)))THEN
		x_profile(i,count) = x_profile(i,count-1)
	ENDIF
	IF(point < maxpoints(link))THEN
		x_profile(i,count) = x_profile(i,count-1) + &
		                     ABS(x(link,point)-x(link,point+1))
	ENDIF
	END DO
	END DO

	IF(profile_x_units(i) == 'RM')THEN 
	DO j=1,profile_max_points(i)
	x_profile(i,j) = x_profile(i,j)/5280.0
	END DO
	END IF

	END DO

ENDIF

DO i=1,num_profiles

count = 80 + i
WRITE(count,1110)
1110 FORMAT('#',160('-'))
CALL decimal_to_date
WRITE(count,1010)i,date_string,time_string,profile_max_points(i)
1010 FORMAT('#Profile Number - ',i3,'   for Date: ',a10,'  Time: ',a8,'  Max number of points on profile = ',i6/)
!WRITE(count,1020)time/time_mult,profile_max_points(i)
!1020 FORMAT(f14.8,2x,i6)
!WRITE(count,1005)'link','point','distance','water el','discharge','velocity','depth', &
!     'conc ','temp ','thalweg el ','area ','top width ','hyd radius ','Froude Num ','frict slope ', &
!	 'bed shear '
!1005 FORMAT(2a7,20a12)
WRITE(count,1005)
1005 FORMAT('link',8x,'point',2x,'distance',2x,'water elev',3x,'discharge',5x,'vel',2x,'depth', &
     6x,'conc',6x,'temp' ,2x,'%Sat',3x,'TDG P', &
		 5x,'thalweg el',2x,'area ',2x,'top width',2x,'hyd rad',2x,'Fr #',2x,'frict slope', &
	 2x,'bed shear')
WRITE(count,1110)

DO j=1,profile_max_points(i)

link = profile_link(i,j)
point = profile_point(i,j)
depth = y(link,point) - thalweg(link,point)
tdg_sat =   TDGasSaturation( DBLE(species(1)%conc(link,point)), DBLE(species(2)%conc(link,point)), salinity, baro_press)
tdg_press = TDGasPress( DBLE(species(1)%conc(link,point)), DBLE(species(2)%conc(link,point)), salinity)


WRITE(count,1000)link,point,j,x_profile(i,j),y(link,point),q(link,point),vel(link,point),depth, &
    species(1)%conc(link,point),species(2)%conc(link,point),tdg_sat,tdg_press, &
		thalweg(link,point),area(link,point),top_width(link,point),hyd_radius(link,point), &
	 froude_num(link,point),friction_slope(link,point),bed_shear(link,point)

!old WRITE(count,1000)link,point,j,x_profile(i,j),y(link,point),q(link,point),vel(link,point),depth, &
!     c(link,point),temp(link,point),thalweg(link,point),area(link,point)

1000 FORMAT(i5,1x,i5,1x,i5,1x,f8.2,2x,f8.2,2x,f12.2,2x,f6.2,2x,f7.2,2x,f10.2,2x,f6.2,2x,f6.2,2x,f6.1,2x, &
			f8.2,2x,es10.2,2x, &
			f8.2,2x,f6.2,f6.2,es10.2,2x,es10.2)


END DO

END DO


IF(time >= time_end)THEN

	DO i=1,num_profiles
	count = 80 + i
	CLOSE(count)
	END DO

	DEALLOCATE(x_profile,profile_link,profile_point)

ENDIF


END SUBROUTINE profile_output
