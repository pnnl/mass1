
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME: print_output
!
! VERSION and DATE: MASS1 v0.70 12/10/1997
!
! PURPOSE: prints a general output file
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:     needs alot more stuff
!
!
! MOD HISTORY: 0.61 version adds more output; 0.70 enhanced output
!
!
!***************************************************************
!

SUBROUTINE print_output(option)

USE mass1_config
USE link_vars
USE general_vars
USE point_vars
USE file_vars
USE transport_vars

USE scalars
USE gas_functions
USE met_data_module
USE date_time
USE section_handler_module

IMPLICIT NONE


DOUBLE PRECISION :: depth
DOUBLE PRECISION :: tdg_press, tdg_sat
DOUBLE PRECISION :: salinity = 0.0
INTEGER :: link,point, iounit1
CHARACTER (LEN=6) :: option
CHARACTER (LEN=10) :: date_string, time_string

iounit1 = fileunit(7)

SELECT CASE(option)

CASE("HEADER")        
  
        OPEN(fileunit(7),file=config%output_file)
    WRITE(99,*)'general output file ', config%output_file,' opened'

         WRITE(iounit1,1026)
1026 FORMAT(5x,'Modular Aquatic Simulation System 1D (MASS1)'/)

         WRITE(iounit1,1025)
1025 FORMAT(5x,'One-Dimensional River Simulation Model'//)
1030 FORMAT(5x,a120/)
         WRITE(iounit1,1040)
1040 FORMAT(5x,'Pacific Northwest National Laboratory')
         WRITE(iounit1,1050)
1050 FORMAT(5x,'Hydrology Group')
         WRITE(iounit1,1060)
1060 FORMAT(5x,'Richland, WA 99352'/)
         WRITE(iounit1,1070)
1070 FORMAT(5x,'Contact:'/)
         WRITE(iounit1,1080)
1080 FORMAT(5x,'Dr. Marshall C. Richmond')
     WRITE(iounit1,1090)
1090 FORMAT(5x,'509-372-6241')
     WRITE(iounit1,1100)
1100 FORMAT(5x,'marshall.richmond@pnl.gov'///)


CASE("CONFIG")

         WRITE(iounit1,1120) config%time%date_run_begins,config%time%time_run_begins
1120 FORMAT('Simulation Starts on Date: ',a10,'  Time: ',a8/)
         WRITE(iounit1,1130) config%time%date_run_ends,config%time%time_run_ends
1130 FORMAT('Simulation Ends on Date: ',a10,'  Time: ',a8/)


WRITE(iounit1,'("time step (hours) - ",f6.3)') config%time%delta_t
WRITE(iounit1,'("printout frequency in time step multiples - ",i4)') config%print_freq

WRITE(iounit1,'("units - ",i4)') config%units
WRITE(iounit1,'("time_option - ",i4)') config%time%option
WRITE(iounit1,'("time units - ",i4)') config%time%units
WRITE(iounit1,'("channel length units - ",i4)') config%channel_length_units
WRITE(iounit1,'("downstream boundary condition type - ",i4)') config%dsbc_type
WRITE(iounit1,'("maximum number of links - ",i5)') config%maxlinks
WRITE(iounit1,'("maximum number of points on a link - ",i5)') config%maxpoint
WRITE(iounit1,'("total number of cross sections - ",i5)') sections%size()

WRITE(iounit1,'("link data input file - ",a80)') config%link_file
WRITE(iounit1,'("point data input file - ",a80)') config%point_file
WRITE(iounit1,'("cross section input file -",a80)') config%section_file
WRITE(iounit1,'("link boundary condition files - ",a80)') config%linkbc_file
WRITE(iounit1,'("lateral inflow boundary files - ",a80)') config%lateral_file
WRITE(iounit1,'("coldstart initial link condition file - ",a80)') config%initial_file
WRITE(iounit1,'("general output (this file) - ",a80)') config%output_file
WRITE(iounit1,'("gas transport boundary condition files - ",a80)') config%tempbc_file
WRITE(iounit1,'("temperature boundary condition files - ",a80)') config%transbc_file
WRITE(iounit1,'("weather/meteorlogical conditions files - ",a80)') config%weather_file
WRITE(iounit1,'("hydro-project link boundary conditions files - ",a80)') config%hydrobc_file
WRITE(iounit1,'("total dissolved gas equation definitions - ",a80)') config%tdg_coeff_file
WRITE(iounit1,'("file to read hotstart data from on a restart - ",a80)') config%restart_load_file
WRITE(iounit1,'("file to write the hotstart data to on end of run - ",a80)') config%restart_save_file
WRITE(iounit1,'("gage control file - ",a80)') config%gage_file
WRITE(iounit1,'("profile control file - ",a80)') config%profile_file


WRITE(fileunit(7),'(//,"end of input specifications",//)')

CASE("LINKS ")
   WRITE(fileunit(7),'(//,"--- Link Data ----------------",/)')

CASE("POINTS")
   WRITE(fileunit(7),'(//,"--- Point Data ---------------",/)')

CASE("SECTIO")
   WRITE(fileunit(7),'(//,"--- Section Data -------------",/)')

CASE("RESULT")
!-----------------------------------------------------------------------------
! dumps all links and points at the simulation start and
! thereafter at the printout frequency
!-----------------------------------------------------------------------------

CALL decimal_to_date(time, date_string, time_string)

WRITE(fileunit(7),*)
WRITE(fileunit(7),1110)
1110 FORMAT(160('-'))
WRITE(fileunit(7),1020)date_string,time_string
1020 FORMAT('Date: ',a10,'  Time: ',a8/)
WRITE(fileunit(7),1010)
1010 FORMAT('link',2x,'point',2x,'distance',2x,'water elev',3x,'discharge',5x,'vel',5x,'depth', &
     7x,'conc',6x,'temp',2x,'%Sat',3x,'TDG P', &
                 2x,'thalweg el',2x,'area ',2x,'top width',2x,'hyd rad',2x,'Fr #',2x,'frict slope', &
         2x,'bed shear')

WRITE(fileunit(7),1110)

!WRITE(fileunit(7),*)time/time_mult  ! temporary output for debugging



DO link=1,config%maxlinks
   DO point=1,maxpoints(link)
      depth = y(link,point) - thalweg(link,point)
      tdg_sat = TDGasSaturation(species(1)%conc(link,point), species(2)%conc(link,point), salinity, baro_press)
      tdg_press = TDGasPress(species(1)%conc(link,point), species(2)%conc(link,point), salinity)


WRITE(fileunit(7),1000)link,point,x(link,point)/5280.0,y(link,point),q(link,point),vel(link,point),depth, &
     species(1)%conc(link,point),species(2)%conc(link,point),tdg_sat,tdg_press, &
                 thalweg(link,point),area(link,point),top_width(link,point),hyd_radius(link,point), &
         froude_num(link,point),friction_slope(link,point),bed_shear(link,point)


END DO
END DO
!1000   FORMAT(i5,i5,6(f10.2,2x),11(f12.6,1x))
1000 FORMAT(i5,2x,i5,2x,f8.2,2x,f8.2,2x,f12.2,2x,f6.2,2x,f7.2,2x,f10.2,2x,f6.2,2x,f6.2,2x,f6.1, &
                2x,f8.2,2x,es10.2,2x, &
   f8.2,2x,f6.2,f6.2,es10.2,2x,es10.2)

IF(time >= config%time%end)THEN
        CLOSE(fileunit(7))
ENDIF

END SELECT


END SUBROUTINE print_output
