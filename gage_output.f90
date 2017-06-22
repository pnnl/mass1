
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: gage_output
!
! VERSION and DATE: MASS1 v0.61 11/21/1997
!
! PURPOSE: prints a time-series file for gage location
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
! MOD HISTORY: added hyd_radius, friction_slope, etc.; mcr 11/21/1997
!
!
!***************************************************************
!

! ----------------------------------------------------------------
! MODULE gage_output_module
! ----------------------------------------------------------------
MODULE gage_output_module

  USE utility
  USE mass1_config
  IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC :: gage_t
     CHARACTER (LEN=256) :: fname
     CHARACTER (LEN=256) :: gname
     INTEGER :: link, point
   CONTAINS
     PROCEDURE :: output => gage_name
  END type gage_t

  INTERFACE gage_t
     MODULE PROCEDURE new_gage_t
  END INTERFACE gage_t

  INTEGER, PUBLIC :: num_gages
  TYPE (gage_t), PUBLIC, DIMENSION(:), ALLOCATABLE :: gages

  PUBLIC gage_output_read

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_gage_t
  ! ----------------------------------------------------------------
  FUNCTION new_gage_t()

    IMPLICIT NONE
    TYPE (gage_t) :: new_gage_t
    new_gage_t%fname = ""
    new_gage_t%gname = ""
    new_gage_t%link = 0
    new_gage_t%point = 0
  END FUNCTION new_gage_t

  ! ----------------------------------------------------------------
  !  FUNCTION gage_name
  ! ----------------------------------------------------------------
  FUNCTION gage_name(this) RESULT (oname)

    IMPLICIT NONE
    CHARACTER (LEN=path_length) :: oname
    CLASS (gage_t), INTENT(INOUT) :: this
    CHARACTER (LEN=80) :: string1, string2

    IF (LEN(TRIM(this%fname)) .LE. 0) THEN
       this%fname = ''
       WRITE(string1,*) this%link
       WRITE(string2,*) this%point
       string1 = ADJUSTL(string1)
       string2 = ADJUSTL(string2)

       this%fname = 'ts' // TRIM(string1) // TRIM(string2) // '.out'
    END IF
    oname = this%fname
  END FUNCTION gage_name

  
  ! ----------------------------------------------------------------
  ! SUBROUTINE gage_output_read
  ! ----------------------------------------------------------------
  SUBROUTINE gage_output_read()

    IMPLICIT NONE
    INTEGER, PARAMETER :: gcunit = 33, gunit = 51
    INTEGER :: count, link, point
    INTEGER :: i

    count=0
    CALL open_existing(config%gage_file, gcunit, fatal=.TRUE.)

    DO WHILE(.TRUE.)
       READ(gcunit,*, END=100) link, point
       count=count+1   
    END DO
100 CONTINUE
    REWIND(gcunit)

    num_gages=count
    ALLOCATE(gages(num_gages))

    DO i=1,num_gages
       gages(i) = gage_t()
       READ(gcunit,*, END=100) link, point
       gages(i)%link = link
       gages(i)%point = point
       gages(i)%fname = ""

       OPEN(gunit, file=gages(i)%output())
       WRITE(gunit,1005)
1005   FORMAT('#',1x,'date',8x,'time',5x,'water elev',2x,'discharge',5x,'vel',4x,'depth', &
            7x,'conc',4x,'temp' ,2x,'%Sat',3x,'TDG P', &
            4x,'thalweg el',2x,'area ',2x,'top width',2x,'hyd rad',2x,'Fr #',2x,'frict slope', &
            2x,'bed shear')

       CLOSE(gunit)
    END DO
    CLOSE(gcunit)
  END SUBROUTINE gage_output_read

END MODULE gage_output_module


SUBROUTINE gage_output

  USE general_vars
  USE mass1_config
  USE gage_output_module
  USE link_vars
  USE point_vars
  USE transport_vars
  USE scalars
  USE gas_functions
  USE hydro_output_module
  USE date_time

  IMPLICIT NONE

  INTEGER, PARAMETER :: gunit = 51

  DOUBLE PRECISION :: depth, tdg_sat, tdg_press
  DOUBLE PRECISION :: salinity = 0.0
  INTEGER :: i,link,point
  CHARACTER (LEN=10) :: date_string, time_string
  DOUBLE PRECISION :: baro_press

  IF(time == config%time%begin )THEN
     CALL gage_output_read()
     CALL hydro_output_setup()
  ENDIF

  DO i=1,num_gages
     link = gages(i)%link
     point = gages(i)%point
     depth = y(link,point) - thalweg(link,point)
     IF (config%do_gas .AND. config%met_required) THEN
        baro_press = metzone(link)%p%current%bp
     ELSE 
        baro_press = 760.0
     END IF
     tdg_sat = TDGasSaturation(species(1)%conc(link,point), &
          &species(2)%conc(link,point), &
          &salinity, baro_press)
     tdg_press = TDGasPress(species(1)%conc(link,point), &
          &species(2)%conc(link,point), salinity)

     CALL decimal_to_date(time, date_string, time_string)

     OPEN(gunit, FILE=gages(i)%output(), ACTION="WRITE", POSITION="APPEND")
     WRITE(gunit,1010)date_string,time_string,&
          &y(link,point),q(link,point),&
          &vel(link,point),depth, &
          &species(1)%conc(link,point),species(2)%conc(link,point), &
          &tdg_sat, tdg_press, &
          &thalweg(link,point),area(link,point),&
          &top_width(link,point),&
          &hyd_radius(link,point),&
          &froude_num(link,point),&
          &friction_slope(link,point),&
          &bed_shear(link,point)

1010 FORMAT(a10,2x,a8,2x,f8.2,2x,f12.2,2x,f6.2,2x,f7.2,2x,f10.2,2x,f6.2,2x,f6.2,2x,f6.1,2x, &
          f8.2,2x,es10.2,2x, &
          f8.2,2x,f6.2,f6.2,es10.2,2x,es10.2)
     CLOSE(gunit)
  END DO

  IF (time > config%time%begin) CALL hydro_output(date_string,time_string)

  IF(time >= config%time%end)THEN
     CALL hydro_output_done()
     DEALLOCATE(gages)
  ENDIF

END SUBROUTINE gage_output
