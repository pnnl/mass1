
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: tdg_coeff_read
!
! VERSION and DATE: MASS1 v0.81 11/10/98
!
! PURPOSE: read in the total dissolved gas production coeff
!          for each project. %TDG saturation is a function of Qspill (KCFS)
!                                       also allows for a specified fraction of the generation flow
!                                       to be 'gassed' accounting for entraiment into the tailrace
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:     quadratic function (gas_eqn_type = 1) %tdg = a + b*Qs + c*Qs**2
!                                               exponential function (gas_eqn_type = 2) %tdg = a + b*exp(c*Qs)
!                                               Qs = effective spill flow = Qspill + qgen_frac*Qgeneration
!
! MOD HISTORY: multi-functional forms added; mcr 11-10-98
!
!
!***************************************************************
!
MODULE tdg_equation_coeff



IMPLICIT NONE

INTEGER, ALLOCATABLE, DIMENSION(:) :: gas_eqn_type
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)    :: a_gas, b_gas, c_gas, qgen_frac

CONTAINS

!#####################################################################
SUBROUTINE allocate_tdg_coeff(maxlinks,status_iounit, error_iounit)

        IMPLICIT NONE
        INTEGER :: maxlinks
        INTEGER :: alloc_stat,error_iounit,status_iounit

        ALLOCATE(gas_eqn_type(maxlinks),a_gas(maxlinks),b_gas(maxlinks) &
                & ,c_gas(maxlinks), qgen_frac(maxlinks), STAT = alloc_stat)
        IF(alloc_stat /= 0)THEN
                WRITE(error_iounit,*)'allocation failed for the gas equation - maxlinks=', maxlinks
                CALL EXIT(1)
        ELSE
                WRITE(status_iounit,*)'allocation successful for gas equation - maxlinks=', maxlinks
        ENDIF

        a_gas = 0.0
        b_gas = 0.0
        c_gas = 0.0
        gas_eqn_type = 0
        qgen_frac = 0.0

END SUBROUTINE allocate_tdg_coeff

!#####################################################################
SUBROUTINE tdg_coeff_read(status_iounit, error_iounit)

USE utility
USE mass1_config

IMPLICIT NONE

INTEGER :: link,junk,status_iounit, error_iounit
INTEGER, PARAMETER :: iunit = 30

! read in general link-related boundary condition table
CALL open_existing(config%tdg_coeff_file, iunit, fatal=.TRUE.)

DO WHILE(.TRUE.)
        READ(iunit,*,END=100)link
        BACKSPACE(iunit)
        READ(iunit,*)junk,gas_eqn_type(link),qgen_frac(link),a_gas(link),b_gas(link),c_gas(link)
END DO

100 CLOSE(iunit)
WRITE(status_iounit,*)'read in tdg equation coefficients successfully'

END SUBROUTINE tdg_coeff_read


END MODULE tdg_equation_coeff
