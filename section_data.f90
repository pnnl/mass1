
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
  USE utility
        USE section_vars
        USE file_vars
		USE logicals
		USE general_vars, ONLY: units

        IMPLICIT NONE

        INTEGER :: i,j,num_pairs,io_unit
		REAL :: xy(2*maxpairs)
        
        CALL open_existing(filename(4), fileunit(4), fatal=.TRUE.)

        io_unit = fileunit(7)
        CALL print_output("SECTIO")
        WRITE(io_unit,*)'Total number of section to be read = ',total_sections
        WRITE(io_unit,'("Count",2x,"Section ID",2x,"Section Type",2x,"Delta Y",2x,"Num of (x,y) Pairs")')

        DO i=1,total_sections

        READ(fileunit(4),*)section_id(i),section_type(i)
        WRITE(io_unit,'(i5,3x,i6,5x,i5)',ADVANCE='NO')i,section_id(i),section_type(i)

        SELECT CASE(section_type(i))

          CASE(1)    !rectangular section
            READ(fileunit(4),*)bottom_width(i)
            WRITE(io_unit,'(5x,f8.2)',ADVANCE='YES')bottom_width(i)
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
 
            WRITE(io_unit,'(8x,f6.2,5x,i5)',ADVANCE='YES')delta_y(i),num_pairs

			READ(fileunit(4),*)xy(1:2*num_pairs)

		  ! go to routine to compute section data on a grid
		  ! later use table look up

			CALL section_table(i,num_pairs,delta_y(i),xy)

        END SELECT

        END DO

        CLOSE(fileunit(4))
		IF(print_sections) CLOSE(90)


END SUBROUTINE section_data
