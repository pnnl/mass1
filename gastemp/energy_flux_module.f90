
MODULE energy_flux

  IMPLICIT NONE

  PRIVATE :: net_solar_rad, net_longwave, back_radiation
  PRIVATE :: evaporation, conduction, windspeed, rel_humid, sat_vapor_press

  DOUBLE PRECISION :: stephan_boltz = 5.67e-8 !stephan-boltzmann constant in W/m2-K4


CONTAINS
  !######################################################################
  DOUBLE PRECISION FUNCTION net_heat_flux(coeff, net_solar, t_water, t_air, t_dew, wind_speed)
    !
    ! Hnet - (watts/m^2)
    !
    ! computes the net heat flux at the water surface
    ! equations in the report
    !
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: net_solar,t_water, t_air, t_dew, wind_speed


    net_heat_flux = net_solar + &
         &net_longwave(coeff, t_air, t_dew) + &
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

  !######################################################################
  DOUBLE PRECISION FUNCTION net_longwave(coeff, t_air, t_dew)
    !
    ! Han - (Watts/meter^2)
    !
    ! net longwave atmospheric radiation ( W/m2 )
    ! Formula 2.1.1 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: coeff(*)
    DOUBLE PRECISION :: t_air, t_dew
    DOUBLE PRECISION :: reflect = 0.03			! relflectance assumed to be 0.03 
    DOUBLE PRECISION :: brunt_coeff ! = 0.65	! ave. value

    brunt_coeff  = coeff(4)
    net_longwave = 4.4e-8*(t_air + 273.15)**4 * &
         ( brunt_coeff + 0.031*SQRT(sat_vapor_press(t_dew)) )*(1.0 - reflect)

  END FUNCTION net_longwave

  !#########################################################################
  DOUBLE PRECISION FUNCTION back_radiation(t_water)
    !
    ! Hb - (Watts/meter^2)
    !
    ! longwave back radiation (heat flux OUT) (black body radiation)
    ! formula 2.1.4 in Edinger, Brady, Geyer (1974)
    IMPLICIT NONE
    DOUBLE PRECISION :: t_water				! water surface temperature in degrees C
    DOUBLE PRECISION :: emiss = 0.97	! emissivity of water

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
    DOUBLE PRECISION :: t_water	! water surface temperature in degrees C
    DOUBLE PRECISION :: t_dew 	! air temperature in degrees C
    DOUBLE PRECISION :: wind_speed	! wind speed in m/s at a height 7 m above water surface

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
    DOUBLE PRECISION :: t_water	! water surface temperature in degrees C
    DOUBLE PRECISION :: t_air		! air temperature in degrees C
    DOUBLE PRECISION :: wind_speed	! wind speed in m/s at a height 7 m above water surface

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
    DOUBLE PRECISION :: wind_speed	! wind speed in m/s at a height 7 m above water surface

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
    !			evaluated at the dew point temperature
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



END MODULE energy_flux
