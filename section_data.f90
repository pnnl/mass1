
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
  DOUBLE PRECISION :: xy(2*maxpairs)
  CHARACTER (LEN=1024) :: msg
  LOGICAL :: just_count

  INTEGER :: tmp_id, tmp_type
  DOUBLE PRECISION :: tmp_bwidth, tmp_bwidth_flood, tmp_depth_main
  DOUBLE PRECISION :: tmp_delta_y, tmp_levels

  just_count = .FALSE.
  CALL status_message(TRIM(filename(4)) // ': Reading sections ...')
  GOTO 10

  ENTRY section_data_count

  just_count = .TRUE.
  CALL status_message(TRIM(filename(4)) // ': Counting sections ...')

10 CONTINUE

  CALL open_existing(filename(4), fileunit(4), fatal=.TRUE.)

  io_unit = fileunit(7)
  CALL print_output("SECTIO")
  WRITE(io_unit,*)'Total number of section to be read = ',total_sections
  WRITE(io_unit,'("Count",2x,"Section ID",2x,"Section Type",2x,"Delta Y",2x,"Num of (x,y) Pairs")')

  i = 0

  DO WHILE (.TRUE.)

     READ(fileunit(4),*,END=100, ERR=200) tmp_id, tmp_type

     i = i + 1

     IF (.NOT. just_count) THEN
        section_id(i) = tmp_id
        section_type(i) = tmp_type
        WRITE(io_unit,'(i5,3x,i6,5x,i5)',ADVANCE='NO')i,section_id(i),section_type(i)
     END IF

     SELECT CASE(tmp_type)

     CASE(1)    !rectangular section
        READ(fileunit(4),*,END=200, ERR=200) tmp_bwidth
        IF (.NOT. just_count) THEN
           bottom_width(i) = tmp_bwidth
           WRITE(io_unit,'(5x,f8.2)',ADVANCE='YES')bottom_width(i)
           SELECT CASE(units)
           CASE(2)
              bottom_width(i) = bottom_width(i)*3.2808
           END SELECT
        END IF

     CASE(2)  !rectangular with a flood plain
        READ(fileunit(4),*,END=200, ERR=200) tmp_depth_main, tmp_bwidth, tmp_bwidth_flood

        IF (.NOT. just_count) THEN
           depth_main(i) = tmp_depth_main
           bottom_width(i) = tmp_bwidth
           bottom_width_flood(i) = tmp_bwidth_flood
           SELECT CASE(units)
           CASE(2)
              depth_main(i) = depth_main(i)*3.2808
              bottom_width(i) = bottom_width(i)*3.2808
              bottom_width_flood(i) = bottom_width_flood(i)*3.2808
           END SELECT
        END IF

     CASE(3)  ! Trapeziodal Section

     CASE(4)  ! Circular Section

     CASE(50) ! section defined by x,y pairs

        ! read in the section data
        ! num_pairs = number of x,y paris for this section
        ! delta_y = depth increment to build table levels in, eg. 1 ft.

        READ(fileunit(4),*,END=200, ERR=200) tmp_delta_y, num_pairs
        READ(fileunit(4),*,END=200, ERR=200)xy(1:2*num_pairs)

        IF (.NOT. just_count) THEN 
           delta_y(i) = tmp_delta_y
           WRITE(io_unit,'(8x,f6.2,5x,i5)',ADVANCE='YES')delta_y(i),num_pairs

           ! go to routine to compute section data on a grid
           ! later use table look up

           CALL section_table(i,num_pairs,delta_y(i),xy)
        END IF

     END SELECT

  END DO

100 CONTINUE

  CLOSE(fileunit(4))
  IF(print_sections) CLOSE(90)

  ! Go here if end of file is reached with the first read, which is OK

  IF (just_count) THEN
     total_sections = i
     WRITE (msg, *) TRIM(filename(4)) // ': counted ', total_sections, ' sections'
     CALL status_message(msg)
     CALL allocate_section_vars()
  ELSE 
     WRITE (msg, *) TRIM(filename(4)) // ': read ', total_sections, ' sections'
     CALL status_message(msg)
     IF (i .LT. total_sections) GOTO 200
  END IF
  RETURN

200 CONTINUE

  ! Go here if there is any kind of error

  WRITE (msg, *) 'Error reading "', TRIM(filename(4)), ' somewhere in section ', i
  CALL error_message(msg, fatal=.TRUE.)

END SUBROUTINE section_data
