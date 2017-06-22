! ----------------------------------------------------------------
! file: gas_coeffs_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Last Change: 2017-06-22 09:17:36 d3g096
! ----------------------------------------------------------------

MODULE gas_coeffs
!   declarations of dissolved gas equation coefficients

!		converted from C (bill perkins version) to F90 by mcr

!   Primary reference:

!  Colt, John, 1984.  Computation of Dissolved Gas Concentrations in
!   Water as Functions of Temperature, Salinity and Pressure.  American
!   Fisheries Society Special Publication 14, Bethesda, Maryland.  ISBN
!   0-913235-02-4

IMPLICIT NONE

!/* general gas properties */

TYPE GasProperty
  DOUBLE PRECISION :: X                     !/* fraction in air */
  DOUBLE PRECISION :: aweight               !/* atomic weight, g/mole */
  DOUBLE PRECISION :: avolume               !/* atomic volume, L/mole */
  DOUBLE PRECISION :: K                     !/* conversion: mL/L * K = mg/L */
  DOUBLE PRECISION :: A                     !/* partial pressure: A = 760/1000*K */
END TYPE GasProperty

!/* -------------------------------------------------------------
!   Gas Properties
!   Source: Colt, 1984: "Properties of Gases" table (back cover)
!   ------------------------------------------------------------- */
TYPE(GasProperty) :: prop(4) = (/ &
     !/* GASFUNC_N2 */
     GasProperty( 0.78084d0, 28.0134d0, 22.403d0, 1.25043d0, 0.6078d0 ), &
     !/* GASFUNC_O2 */
     GasProperty( 0.20946d0, 31.9988d0, 22.392d0, 1.42903d0, 0.5318d0 ), &
     !/* GASFUNC_AR */
     GasProperty( 0.00934d0,  39.948d0, 22.390d0, 1.78419d0, 0.4260d0 ), &
     !/* GASFUNC_CO2 */
     GasProperty( 0.00032d0, 44.0098d0, 22.263d0, 1.97681d0, 0.3845d0 ) /)



!/* coefficients used to compute Bunsen
!  coefficient (beta) and
!  air-soluability (CStar) */

TYPE GasCoeff
  DOUBLE PRECISION :: A1, A2, A3, A4
  DOUBLE PRECISION :: B1, B2, B3
END TYPE GasCoeff

!/* -------------------------------------------------------------
!   Gas Properties
!   Source: Colt, 1984: "Properties of Gases" table (back cover)
!   ------------------------------------------------------------- */
! DATA prop(1) / GasProperty( 0.78084d0, 28.0134d0, 22.403d0, 1.25043d0, 0.6078d0 ) / ! /* GASFUNC_N2 */  
! DATA prop(2) / GasProperty( 0.20946d0, 31.9988d0, 22.392d0, 1.42903d0, 0.5318d0 ) / ! /* GASFUNC_O2 */
! DATA prop(3) / GasProperty( 0.00934d0,  39.948d0, 22.390d0, 1.78419d0, 0.4260d0 ) / ! /* GASFUNC_AR */
! DATA prop(4) / GasProperty( 0.00032d0, 44.0098d0, 22.263d0, 1.97681d0, 0.3845d0 ) / ! /* GASFUNC_CO2 */

!/* -------------------------------------------------------------
!   Bunsen Coefficient (Beta) equation coefficients
!   Source: Colt, 1984: Equation A-1 and A-3, Table A-1
!   ------------------------------------------------------------- */
TYPE(GasCoeff) :: beta(4) = (/&
     ! GASFUNC_N2
     & GasCoeff(-59.6274d0, 85.7661d0, 24.3696d0, -9999.0d0, &
     & -0.051580d0, 0.026329d0, -0.0037252d0 ),&
     !  GASFUNC_O2 
     & GasCoeff( -58.3877d0, 85.8079d0, 23.8439d0, -9999.0d0, &
     & -0.034892d0, 0.015568d0, -0.0019387d0 ),&
     ! GASFUNC_AR
     & GasCoeff( -55.6578d0, 82.0262d0, 22.5929d0, -9999.0d0, &
     & -0.036267d0, 0.016241d0, -0.0020114d0 ),&
     ! GASFUNC_CO2 (for Ko)
     & GasCoeff( -58.0931d0, 90.5069d0, 22.2940d0, -9999.0d0, &
     & 0.027766d0, -0.025888d0, 0.0050578d0 ) /)

!                                /* GASFUNC_N2 */  
! DATA beta(1) / GasCoeff(-59.6274d0, 85.7661d0, 24.3696d0, -9999.0d0, &
!      -0.051580d0, 0.026329d0, -0.0037252d0 ) /


!                                /* GASFUNC_O2 */
! DATA beta(2) /  GasCoeff( -58.3877d0, 85.8079d0, 23.8439d0, -9999.0d0, &
!      -0.034892d0, 0.015568d0, -0.0019387d0 ) /

!                                /* GASFUNC_AR */
! 
! DATA beta(3) /  GasCoeff( -55.6578d0, 82.0262d0, 22.5929d0, -9999.0d0, &
!      -0.036267d0, 0.016241d0, -0.0020114d0 ) /

!                                /* GASFUNC_CO2 (for Ko) */
! DATA beta(4) /  GasCoeff( -58.0931d0, 90.5069d0, 22.2940d0, -9999.0d0, &
!     0.027766d0, -0.025888d0, 0.0050578d0 ) /

!/* -------------------------------------------------------------
!   Air-Solubility (CStar) equation coefficients
!   Source: Colt, 1984: Equation A-2, Table A-2
!  ------------------------------------------------------------- */
TYPE(GasCoeff) :: cstar(4) = (/&
     ! GASFUNC_N2 
     & GasCoeff( -172.4965d0, 248.4262d0, 143.0738d0, -21.7120d0, &
     & -0.049781d0, 0.025018d0, -0.0034861d0 ),&
     ! GASFUNC_O2
     & GasCoeff( -173.4292d0, 249.6339d0, 143.3483d0, -21.8492d0, &
     & -0.033096d0, 0.014259d0, -0.0017000d0 ),&
     ! GASFUNC_AR
     & GasCoeff( -173.5146d0, 245.4510d0, 141.8222d0, -21.8020d0, &
     & -0.034474d0, 0.014934d0, -0.0017729d0 ),&
     ! GASFUNC_CO2 (not available)
     & GasCoeff( -9999.0d0, -9999.0d0, -9999.0d0, -9999.0d0, &
     & -9999.0d0, -9999.0d0, -9999.0d0 ) /)

!                                /* GASFUNC_N2 */  
!DATA cstar(1) / GasCoeff( -172.4965d0, 248.4262d0, 143.0738d0, -21.7120d0, &
!     -0.049781d0, 0.025018d0, -0.0034861d0 ) /
!                                /* GASFUNC_O2 */
!DATA cstar(2) / GasCoeff( -173.4292d0, 249.6339d0, 143.3483d0, -21.8492d0, &
!     -0.033096d0, 0.014259d0, -0.0017000d0 ) /
!                                /* GASFUNC_AR */
!DATA cstar(3)  /  GasCoeff( -173.5146d0, 245.4510d0, 141.8222d0, -21.8020d0, &
!    -0.034474d0, 0.014934d0, -0.0017729d0 ) /
!                                /* GASFUNC_CO2 (not available) */
!DATA cstar(4) / GasCoeff( -9999.0d0, -9999.0d0, -9999.0d0, -9999.0d0, &
!     -9999.0d0, -9999.0d0, -9999.0d0 ) /

END MODULE gas_coeffs
