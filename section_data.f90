
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	section_data
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE: read in section data
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

SUBROUTINE section_data

! Routine to read in data to define each cross section
! also calls section_table to compute geometric data tables
! for section type=5 (x-y pair definition)

! $DEBUG
        USE section_vars
        USE file_vars
		USE logicals
		USE general_vars, ONLY: units

        IMPLICIT NONE

        INTEGER :: i,j,num_pairs
		REAL :: xy(2*maxpairs)
        
        INQUIRE(FILE=filename(4),EXIST=file_exist)
        IF(file_exist)THEN
           OPEN(fileunit(4),file=filename(4))
           WRITE(99,*)'cross section data file opened: ',filename(4)
        ELSE
           WRITE(*,*)'cross section file does not exist - ABORT: ',filename(4)
           WRITE(99,*)'cross section file does not exist - ABORT: ',filename(4)
           CALL EXIT
        ENDIF

        DO i=1,total_sections

        READ(fileunit(4),*)section_id(i),section_type(i)

        SELECT CASE(section_type(i))

          CASE(1)    !rectangular section
            READ(fileunit(4),*)bottom_width(i)

			SELECT CASE(units)
			CASE(2)
			  bottom_width(i) = bottom_width(i)*3.2808
			END SELECT

          CASE(2)  !rectangular with a flood plain
            READ(fileunit(4),*)depth_main(i),bottom_width(i),bottom_width_flood(i)

			SELECT CASE(units)
			CASE(2)
			  depth_main(i) = depth_main(i)*3.2808
			  bottom_width(i) = bottom_width(i)*3.2808
			  bottom_width_flood(i) = bottom_width_flood(i)*3.2808
			END SELECT

		  CASE(3)  ! Trapeziodal Section

		  CASE(4)  ! Circular Section

		  CASE(50) ! section defined by x,y pairs

		  ! read in the section data
		  ! num_pairs = number of x,y paris for this section
		  ! delta_y = depth increment to build table levels in, eg. 1 ft.

			READ(fileunit(4),*)delta_y(i),num_pairs 
			READ(fileunit(4),*)xy(1:2*num_pairs)

		  ! go to routine to compute section data on a grid
		  ! later use table look up

			CALL section_table(i,num_pairs,delta_y(i),xy)

        END SELECT

        END DO

        CLOSE(fileunit(4))
		IF(print_sections) CLOSE(90)


END SUBROUTINE section_data
