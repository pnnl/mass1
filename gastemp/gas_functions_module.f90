! ----------------------------------------------------------------
! file: gas_functions_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Last Change: 2017-06-22 09:18:03 d3g096
! ----------------------------------------------------------------

MODULE gas_functions
!   basic dissolved gas relationships

!               converted from C (bill perkins version) to F90 by mcr

!   Primary reference:

!   Colt, John, 1984.  Computation of Dissolved Gas Concentrations in
!   Water as Functions of Temperature, Salinity and Pressure.  American
!   Fisheries Society Special Publication 14, Bethesda, Maryland.  ISBN
!   0-913235-02-4

USE gas_coeffs

IMPLICIT NONE

INTEGER :: GASFRACTIONS = 4
PRIVATE :: prop, beta, cstar, GASFRACTIONS

CONTAINS

!/* -------------------------------------------------------------
!   static beta_formula
!
!   used to compute Bunsen's coefficient, equation A-1, temperature (T)
!   is in C, salinity (S) is in ppt.
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION beta_formula(T, S, c)
IMPLICIT NONE
DOUBLE PRECISION :: T, S, K, value, K100
TYPE(GasCoeff) :: c

  K = T + 273.15
  
  K100 = K/100.0
  
  value = c%A1 + c%A2*(100.0/K) + c%A3*Dlog(K100)
  IF(S > 0.0)THEN
    value = value + S*(c%B1 + c%B2*(K100) + c%B3*(K100)*(K100))
  END IF
  value = Dexp(value)
  
        beta_formula = value

END FUNCTION beta_formula

!/* -------------------------------------------------------------
!   GasBeta
!
!   given the gas, temperature (C), and salinity (ppt) this routine
!   computes the Bunsen coefficent (beta, in L/L-atm)
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION GasBeta(gas,  T,  S)
IMPLICIT NONE
DOUBLE PRECISION :: value, T, S
INTEGER :: gas


  value = beta_formula( T, S, beta(gas) )
  
!/* for carbon dioxide, use equation A-4 */

  SELECT CASE(gas)
  CASE (4)
    value = value*22.263;
 
  END SELECT

        GasBeta = value

END FUNCTION

!/* -------------------------------------------------------------
!   static cstar_formula
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION cstar_formula( T,  S,  c)
IMPLICIT NONE
  DOUBLE PRECISION :: value, T, S, K, K100
        TYPE(GasCoeff) :: c

  K = T + 273.15
  K100 = K/100.0

  value = c%A1 + c%A2*(100.0/K) + c%A3*Dlog(K100) + c%A4*K100
  value = value + S*(c%B1 + c%B2*K100 + c%B3*K100*K100)
  value = Dexp(value)
  
        cstar_formula = value

END FUNCTION

!/* -------------------------------------------------------------
!   GasCStar
!
!   given the gas, temperature (C), salinity (ppt) this routine
!   computes the air-solubility (mg/L) using equation A-8 for carbon
!   dioxide and equations A-2 and A-7 for the others
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION GasCStar(gas,  T,  S) 
IMPLICIT NONE
DOUBLE PRECISION ::  b, value, vp, T, S
INTEGER :: gas
  
  SELECT CASE(gas)
  CASE(4)
    vp = GasH2OPress(T, S)
    b = GasBeta(gas, T, S)
    value = 1000*prop(gas)%K*b*prop(gas)%X*(760.0 - vp)/760
    
  CASE DEFAULT
    value = cstar_formula(T, S, cstar(gas))
    value = value*prop(gas)%K;       !/* Equation A-6 */
    
  END SELECT
  
        GasCStar = value

END FUNCTION


!/* -------------------------------------------------------------
!   GasCStarBP
!
!   This routine computes air-solubility (mg/L) as in GasCStar, but
!   corrects for barometric pressure (P, mm Hg)
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION GasCStarBP(gas,  T,  S, BP) 
IMPLICIT NONE
DOUBLE PRECISION ::vp, value, T, S, BP
INTEGER :: gas

  vp = GasH2OPress(T, S)
  
  value = GasCStar(gas, T, S)
  value = value*(BP - vp)/(760.0 - vp)

        GasCStarBP = value

END FUNCTION


!/* -------------------------------------------------------------
!   GasH2OPress
!
!   This computes the vapor pressure (in mm Hg) for water given the
!   temperature (T) in C and the salinity (S) in ppt.  Colt's equation
!   A-12 is used here.  It is simpler.
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION GasH2OPress( T, S) 
IMPLICIT NONE
DOUBLE PRECISION :: K, T, S, value

  K = T + 273.15d0

  value = 24.4543d0 - 67.4509d0*(100.0d0/K) - 4.8489d0*Dlog(K/100.0d0) - 0.000544d0*S
  value = Dexp(value)          !/* in atmospheres */
  value = value*760.0d0        !    /* in mm Hg */

        GasH2OPress = value

END FUNCTION


!/* -------------------------------------------------------------
!   GasH2ORho
!
!   This routine computes weight density (mass density * gravity) in mm
!   Hg/m, given the Temperature (T, C) and salinity (S, ppt).  Uses
!   Equation A-13 of Colt, 1984.

!               5/28/98 : fixed some typos and the conversion from Pa to mmHg/m; mcr
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION GasH2ORhoG( T,  S) 
IMPLICIT NONE
DOUBLE PRECISION :: rho, T, S, A, B, C
  
DOUBLE PRECISION :: g = 9.80665
  
  rho = 999.842594                              &
    + 6.793952e-02*T                    &
    - 9.095290e-03*T*T          &
    + 1.001685e-04*T*T*T        &       
    - 1.120083e-06*T*T*T*T      &
    + 6.536336e-09*T*T*T*T*T

  IF(S > 0.0)THEN

    A = 8.34493e-01                             &
      - 4.0899e-03*T                    &
      + 7.6438e-05*T*T          &
      - 8.2467e-07*T*T*T        &
      + 5.3875e-09*T*T*T*T
  
    B = - 5.72466e-03 &
      + 1.0227e-04*T  &
      - 1.6546e-06*T*T

    C = 4.8314e-04

    rho = rho + A*S + B*S**1.5 + C*S

  END IF
    
  rho = rho*g*7.50062/1000.0

        GasH2ORhoG = rho

END FUNCTION


!/* -------------------------------------------------------------
!   TDGasBeta
!
!   This routine computes an "apparent" air-solubility (mL/L) and the
!   "A" value for total gas.  This is done using something like
!   Equations 41 and 42.
!
!   ------------------------------------------------------------- */

SUBROUTINE TDGasBeta(T,  S, B, A)
IMPLICIT NONE
DOUBLE PRECISION :: T, S, A, B, partbeta
INTEGER ::  i
DOUBLE PRECISION ::   Anum = 0.0, Adenom = 0.0
DOUBLE PRECISION ::   Bnum = 0.0, Bdenom = 0.0

Anum = 0.0
Adenom = 0.0
Bnum = 0.0
Bdenom = 0.0

  DO i = 1, GASFRACTIONS
    partbeta = GasBeta(i, T, S)
    Bnum = Bnum + partbeta*prop(i)%X
    Bdenom = Bdenom + prop(i)%X
    Anum = Anum + partbeta*prop(i)%X
    Adenom = Adenom + prop(i)%K*partbeta*prop(i)%X
  END DO

  A = 0.760*Anum/Adenom
  B = Bnum/Bdenom

END SUBROUTINE TDGasBeta

!/* -------------------------------------------------------------
!   TDGasConc
!
!   This routine computes a total gas concentration (mg/L) given the
!   total gas pressure (TGP, mm Hg), water temperature (T, C), salinity
!   (S, ppt), and barometric pressure (BP, mm Hg).  The total gas
!   pressure is assumed not to include water vapor.  Uses Equation 24
!   of Colt, 1984.
! 
!   ------------------------------------------------------------- */
DOUBLE PRECISION FUNCTION TDGasConc( TGP,  T,  S) 
IMPLICIT NONE
DOUBLE PRECISION :: A, B, TGP, T, S, PH2O
  
  PH2O = GasH2OPress(T, S)
  CALL TDGasBeta(T, S, B, A)
  TDGasConc = (TGP - PH2O)/A*B

END FUNCTION

!/* -------------------------------------------------------------
!   TDGasPress
!
!   This routine computes the total gas pressure, including water vapor
!   pressure (mm Hg), given a total gas concentration (mg/L),
!   temperature (C), and salinity (ppt).  Total gas pressure is defined
!   by Colt's equation 31.  This routine uses Equation 24 of Colt, 1984.
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION TDGasPress(conc,  T,  S)
IMPLICIT NONE
DOUBLE PRECISION :: conc, T, S, A, B, TGP, PH2O

  CALL TDGasBeta(T, S, B, A)
  TGP = conc/B*A
  PH2O = GasH2OPress(T, S)
  TDGasPress = TGP + PH2O

END FUNCTION


!/* -------------------------------------------------------------
!   TDGasDP
!
!   This routine computes the total gas pressure difference (mm Hg),
!   given a total gas concentration (conc, mg/L), temperature (T, C),
!   salinity (S, ppt), and barometric pressure (BP, mm Hg).  Uses
!  Equation 32 of Colt, 1984.
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION TDGasDP(conc, T,  S,  BP) 
IMPLICIT NONE
DOUBLE PRECISION :: T, S, TGP, conc, BP

   TGP = TDGasPress(conc, T, S)
   TDGasDP = TGP - BP
  

END FUNCTION


!/* -------------------------------------------------------------
!   TDGasSaturation
!
!   This routine computes the total gas saturation (%) given a total
!   gas concentration (conc, mg/L), temperature (T, C), salinity (S,
!   ppt), and barometric pressure (BP, mm Hg).  Uses Equation 30 of
!   Colt, 1984.
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION TDGasSaturation( conc, T,  S, BP)
IMPLICIT NONE
DOUBLE PRECISION :: conc, T, S, BP, DP
  DP = TDGasDP(conc, T, S, BP)
  TDGasSaturation = 100.0*(BP + DP)/BP


END FUNCTION


!/* -------------------------------------------------------------
!   TDGasCompDP
!
!   This routine computes depth-compensated total gas pressure
!   difference (mm Hg), given a total gas concentration (conc, mg/L),
!   temperature (T, C), salinity (S, ppt), barometric pressure (BP,
!   mm Hg), and depth (d, m).  Uses Equation 54 of Colt, 1984.
!
!   ------------------------------------------------------------- */

DOUBLE PRECISION FUNCTION TDGasCompDP( conc, T,  S, BP,  d)
IMPLICIT NONE
DOUBLE PRECISION :: conc, T, S, BP, d, TGP, dP

  TGP = TDGasPress(conc, T, S)
  dP = TGP - BP
  TDGasCompDP = dP - GasH2ORhoG(T, S)*d

END FUNCTION

!/* -------------------------------------------------------------
!   TDGasConcfromSat
!
!   This routine computes a total gas concentration (mg/L) given the
!   % total gas saturation.total gas pressure (TGP, mm Hg), water temperature (T, C), salinity
!   (S, ppt), and barometric pressure (BP, mm Hg).  The total gas
!   pressure is assumed not to include water vapor.  Uses Equation 24
!   of Colt, 1984.
! 
!   ------------------------------------------------------------- */
DOUBLE PRECISION FUNCTION TDGasConcfromSat( saturation,  T,  S, BP) 
IMPLICIT NONE
DOUBLE PRECISION :: saturation, BP, TGP, T, S
  
        TGP = saturation*BP/100.0
        TDGasConcfromSat = TDGasConc( TGP,  T,  S)
 

END FUNCTION


END MODULE gas_functions
