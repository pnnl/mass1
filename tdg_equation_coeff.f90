
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	tdg_coeff_read
!
! VERSION and DATE: MASS1 v0.81 11/10/98
!
! PURPOSE: read in the total dissolved gas production coeff
!          for each project. %TDG saturation is a function of Qspill (KCFS)
!					also allows for a specified fraction of the generation flow
!					to be 'gassed' accounting for entraiment into the tailrace
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:	quadratic function (gas_eqn_type = 1) %tdg = a + b*Qs + c*Qs**2
!						exponential function (gas_eqn_type = 2) %tdg = a + b*exp(c*Qs)
!						Qs = effective spill flow = Qspill + qgen_frac*Qgeneration
!
! MOD HISTORY: multi-functional forms added; mcr 11-10-98
!
!
!***************************************************************
!
MODULE tdg_equation_coeff



IMPLICIT NONE

INTEGER, ALLOCATABLE, DIMENSION(:) :: gas_eqn_type
REAL, ALLOCATABLE, DIMENSION(:)    :: a_gas, b_gas, c_gas, qgen_frac

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
		CALL EXIT
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

USE file_vars
USE logicals, ONLY : file_exist

IMPLICIT NONE

INTEGER :: i,link,junk,status_iounit, error_iounit

! read in general link-related boundary condition table
INQUIRE(FILE=filename(11),EXIST=file_exist)
IF(file_exist)THEN
   OPEN(fileunit(11),file=filename(11))
   WRITE(99,*)'TDG coeff file opened: ',filename(11)
ELSE
   WRITE(*,*)'TDG coeff file does not exist - ABORT: ',filename(11)
   WRITE(99,*)'TDG coeff file does not exist - ABORT: ',filename(11)
   CALL EXIT
ENDIF

DO WHILE(.TRUE.)
	READ(fileunit(11),*,END=100)link
	BACKSPACE(fileunit(11))
	READ(fileunit(11),*)junk,gas_eqn_type(link),qgen_frac(link),a_gas(link),b_gas(link),c_gas(link)
END DO

100 CLOSE(fileunit(11))
WRITE(status_iounit,*)'read in tdg equation coefficients successfully'

END SUBROUTINE tdg_coeff_read


END MODULE tdg_equation_coeff
