! ----------------------------------------------------------------
! file: energy_flux_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Last Change: 2020-04-17 07:38:24 d3g096
! ----------------------------------------------------------------

MODULE energy_flux

  IMPLICIT NONE

  PRIVATE :: net_solar_rad, back_radiation
  PRIVATE :: evaporation, conduction, windspeed, rel_humid, sat_vapor_press
  PUBLIC :: net_longwave, equilibrium_temperature

  DOUBLE PRECISION, PUBLIC, PARAMETER :: &
       & stephan_boltz = 5.67e-8  ! stephan-boltzmann constant in W/m2-K4

  ! density of water, kg/m3
  DOUBLE PRECISION, PARAMETER, PRIVATE :: rho = 1000.0 

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION equilibrium_temperature
  !
  ! Computes equilibrium temperature according to
  !
  ! Edinger, John E., David W. Duttweiler, and John C. Geyer.  The
  ! Response of Water Temperatures to Meteorological Conditions.
  ! Water Resources Research 4, no. 5 (1968): 1137
  ! 43. https://doi.org/10.1029/WR004i005p01137.

  ! ----------------------------------------------------------------
  FUNCTION equilibrium_temperature(coeff, &
       &net_solar, t_water, t_air, t_dew, wind_speed, lwrad) RESULT(te)

    IMPLICIT NONE
    DOUBLE PRECISION :: te
    DOUBLE PRECISION, INTENT(IN) :: coeff(*)
    DOUBLE PRECISION, INTENT(IN) :: net_solar,t_water, t_air, t_dew, wind_speed, lwrad

    DOUBLE PRECISION, PARAMETER :: Delta3 = 273.15*273.15*273.15
    DOUBLE PRECISION, PARAMETER :: Delta4 = Delta3*273.15

    DOUBLE PRECISION :: beta, ccond, K, rh, es, L, emiss

    ccond = coeff(3)
    emiss = 0.97
    rh = rel_humid(t_air, t_dew)
    es = sat_vapor_press(t_dew)

    IF (t_air .EQ. t_dew) THEN
       beta = 0.0
    ELSE 
       beta = ((1.0 - rh)*es)/(t_air - t_dew)
    END IF

    L = latent_heat(t_water)

    K = 4.0*emiss*stephan_boltz*Delta3 + L*rho*(ccond + beta)*windspeed(coeff, wind_speed)

    te = (net_solar + lwrad - emiss*stephan_boltz*Delta3)/K + &
         & (K - 4*emiss*stephan_boltz*Delta4)/(K*(ccond + beta))*&
         & (ccond*t_air + beta*t_dew)
    
  END FUNCTION equilibrium_temperature

  !######################################################################
  DOUBLE PRECISION FUNCTION net_heat_flux(coeff, &
       &net_solar, t_water, t_air, t_dew, wind_speed, lwrad)
    !
    ! Hnet - (watts/m^2)
    !
    ! computes the net heat flux at the water surface
    ! equations in the report
    !
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: coeff(*)
    DOUBLE PRECISION, INTENT(IN) :: net_solar,t_water, t_air, t_dew, wind_speed
    DOUBLE PRECISION, INTENT(IN), OPTIONAL :: lwrad

    DOUBLE PRECISION :: tmplw

    IF (PRESENT(lwrad)) THEN
       tmplw = net_longwave(coeff, t_air, t_dew, lwrad)
    ELSE
       tmplw = net_longwave(coeff, t_air, t_dew)
    END IF
    net_heat_flux = net_solar + &
         &tmplw + &
         &back_radiation(t_water) + &
         &evaporation(coeff, t_water, t_dew, wind_speed) + &
         &conduction(coeff, t_water, t_air, wind_speed) 

  END FUNCTION net_heat_flux
  !######################################################################
  DOUBLE PRECISION FUNCTION net_solar_rad()
    IMPLICIT NONE
    ! Hsn
    !
    ! computes the net incoming short-wave solar radiation using the 
    ! equations in the report
    !
    ! currently not implemented as it is an input in the weather file
    net_solar_rad = 0.0

  END FUNCTION net_solar_rad

  ! ----------------------------------------------------------------
  !  FUNCTION atm_longwave
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION atm_longwave(coeff, t_air, t_dew)

    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(IN) :: coeff(*)
    DOUBLE PRECISION, INTENT(IN) :: t_air, t_dew
    DOUBLE PRECISION :: brunt_coeff

    ! Formula 2.1.1 in Edinger, Brady, Geyer (1974)

    ! Black body radiation
    ! FIXME: Why is this not the Stefan-Boltzmann constant (5.67E-08)
    atm_longwave = 4.4e-8*(t_air + 273.15)**4

    ! Atmospheric emissivity using the Brunt formula
    brunt_coeff  = coeff(4)
    atm_longwave = atm_longwave * &
         ( brunt_coeff + 0.031*SQRT(sat_vapor_press(t_dew)) )

  END FUNCTION atm_longwave


  !######################################################################
  DOUBLE PRECISION FUNCTION net_longwave(coeff, t_air, t_dew, lwrad)
    !
    ! Han - (Watts/meter^2)
    !
    ! net longwave atmospheric radiation ( W/m2 )
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: coeff(*)
    DOUBLE PRECISION, INTENT(IN) :: t_air, t_dew
    DOUBLE PRECISION, INTENT(IN), OPTIONAL :: lwrad
    DOUBLE PRECISION, PARAMETER :: reflect = 0.03 ! relflectance assumed to be 0.03 

    IF (.NOT. PRESENT(lwrad)) THEN
       net_longwave = atm_longwave(coeff, t_air, t_dew)
    ELSE
       ! Externally computed LW is assumed to have been adjusted with emissivity
       net_longwave = lwrad
    END IF

    net_longwave = net_longwave*(1.0 - reflect)

  END FUNCTION net_longwave

  !#########################################################################
  DOUBLE PRECISION FUNCTION back_radiation(t_water)
    !
    ! Hb - (Watts/meter^2)
    !
    ! longwave back radiation (heat flux OUT) (black body radiation)
    ! formula 2.1.4 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: t_water                         ! water surface temperature in degrees C
    DOUBLE PRECISION :: emiss = 0.97    ! emissivity of water

    back_radiation = -emiss*stephan_boltz*(t_water + 273.15)**4

  END FUNCTION back_radiation

  !#########################################################################
  DOUBLE PRECISION FUNCTION evaporation(coeff, t_water, t_dew, wind_speed)
    !
    ! He - (Watts/m^2)
    !
    ! heat flux OUT due to water evaporation
    ! formula 2.1.5 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: t_water ! water surface temperature in degrees C
    DOUBLE PRECISION :: t_dew   ! air temperature in degrees C
    DOUBLE PRECISION :: wind_speed      ! wind speed in m/s at a height 7 m above water surface

    evaporation = -windspeed(coeff, wind_speed)*( sat_vapor_press(t_water) - sat_vapor_press(t_dew) )

  END FUNCTION evaporation

  !#########################################################################
  DOUBLE PRECISION FUNCTION conduction(coeff, t_water, t_air, wind_speed)
    !
    ! Hc - (Watts/m^2)
    !
    ! heat flux OUT due to conduction
    ! formula 2.1.11 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: t_water ! water surface temperature in degrees C
    DOUBLE PRECISION :: t_air           ! air temperature in degrees C
    DOUBLE PRECISION :: wind_speed      ! wind speed in m/s at a height 7 m above water surface

    !conduction = -0.47*windspeed(coeff, wind_speed)*(t_water - t_air)
    conduction = -coeff(3)*windspeed(coeff, wind_speed)*(t_water - t_air)
    !conduction = -0.018*windspeed(t_water,wind_speed)*(t_water - t_air)

  END FUNCTION conduction

  !#########################################################################
  DOUBLE PRECISION FUNCTION windspeed(coeff, wind_speed)
    !
    ! wind speed function required by conduction and evaporative heat fluxes
    ! has units of Watts/(m^2 mmHg)
    !
    ! formula 2.4.6 of in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: wind_speed      ! wind speed in m/s at a height 7 m above water surface

    ! formula 2.4.6 of in Edinger, Brady, Geyer (1974)
    ! windspeed = 9.2 + 0.46*wind_speed**2
    ! windspeed = 0.5*windspeed

    ! from the QUAL2E formulation (with unit conversions)

!!$windspeed = 1000.0              ! density: kg/m3 
!!$windspeed = windspeed*(595 - 0.5*temp)*4186.8 ! latent heat of vaporization J/kg
!!$windspeed = windspeed*(2.3D-09 + 2.0D-09*wind_speed)
    windspeed = coeff(2) + coeff(1)*wind_speed**2

  END FUNCTION windspeed


  !#########################################################################
  DOUBLE PRECISION FUNCTION rel_humid(t_air,t_dew)
    !
    ! returns relative humidity (fraction) as a function of 
    ! air temp and dew point temp
    ! eqn. 2-9 in Hydrology for Engineers
    !
    IMPLICIT NONE
    DOUBLE PRECISION :: t_air, t_dew ! air temp and dew-point temp in degrees C

    rel_humid = ( (112 - 0.1*t_air + t_dew)/(112 + 0.9*t_air) )**8

  END FUNCTION rel_humid

  !########################################################################
  DOUBLE PRECISION FUNCTION sat_vapor_press(t_air)
    !
    ! returns saturation vapor pressure of air in mmHg
    ! given an air temp in degrees C
    !
    ! Raudkivi (1979) formula
    !
    ! Note that the air vapor pressure is the air saturation vapor pressure
    !                   evaluated at the dew point temperature
    !
    IMPLICIT NONE
    DOUBLE PRECISION :: t_air

    sat_vapor_press = 4.596*EXP( (17.27*t_air)/(237.3+ t_air) )

  END FUNCTION sat_vapor_press

  !#######################################################################
  !DOUBLE PRECISION FUNCTION vapor_press
  !
  ! not needed as dew point temp is an input to model
  !
  !IMPLICIT NONE
  !
  !vapor_press = rel_humid(t_air,t_dew)*sat_vapor_press(t_air)
  !END FUNCTION vapor_press

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION dew_point
  !
  ! Computes dew point (C) from atmospheric temperature (C) and
  ! relative humidity (as a fraction).  Equation 2-7 in Hydrology for
  ! Engineers.
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION dew_point(t_air, relh) 

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_air, relh

    DOUBLE PRECISION :: T, X

    X = 1.00 - relh
    T  = (14.55 + 0.114*t_air)*X + ((2.5 + 0.007*t_air)*X)**3 &
         & + (15.9 + 0.117*t_air)*X**14

    dew_point  = t_air - T
  END FUNCTION dew_point

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION latent_heat
  !
  ! Computes the latent heat of vaporization for water (kJ/kg) given the
  ! temperature (degree C).  
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION latent_heat(T)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: T

    ! Got this from Wikipedia (I know, I know), which referenced
    !
    !   Quartic fit to Table 2.1,p.16, Textbook: R.R.Rogers & M.K. Yau, A
    !   Short Course in Cloud Physics, 2e,(1989), Pergamon press

    ! latent_heat = - 0.0000614342*T**3 + 0.00158927*T**2 - 2.36418*T + 2500.79

    ! This is a little more defendable and very close to the above, from
    !
    !   Sinokrot B and H Stefan. 1994. “Stream Water-Temperature
    !   Sensitivity to Weather and Bed Pa- rameters.”  Journal of
    !   Hydraulic Engineering 120(6):722–736.

    latent_heat = (597.31 - 0.5631*T)*4.186

  END FUNCTION latent_heat




END MODULE energy_flux
