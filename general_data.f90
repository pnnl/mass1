
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	general_data
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: in a non-GUI version this reads general control data
!          from a file.
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

SUBROUTINE general_data

        USE general_vars
		USE logicals
        USE section_vars , ONLY : total_sections
        USE file_vars

        IMPLICIT NONE


        OPEN(fileunit(1),file=filename(1))


        !READ(fileunit(1),*)time_units,units,channel_length_units,time_begin,time_end,delta_t
        READ(fileunit(1),*)time_units,units,channel_length_units
		READ(fileunit(1),*)debug_print         ! 1=point, 2=link-based
        READ(fileunit(1),*)maxlinks,total_sections,maxpoint,maxtable,maxtimes
		!READ(fileunit(1),*)do_flow,do_gas,do_temp,do_printout,do_gageout,do_profileout
		READ(fileunit(1),*)dsbc_type
		
		READ(fileunit(1),*)gas_diffusion, gas_exchange
		READ(fileunit(1),*)temp_diffusion, temp_exchange
		READ(fileunit(1),*)do_hotstart,do_restart

        CLOSE(fileunit(1))
	


END SUBROUTINE general_data