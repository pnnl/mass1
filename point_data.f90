
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: point_data
!
! VERSION and DATE: MASS1 v0.6 10/8/97
!
! PURPOSE:reads in data for points on each link for input_option=1
!      OR assigns uniform point data for input_option=2
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

! ----------------------------------------------------------------
! SUBROUTINE point_data_scan
!
! The purpose of this routine is to count the number of links and
! points in the configuration
!
! NOTE: DO NOT USE THIS. IT IS NOT CORRECT.  IT DOES NOT WORK IF LINKS
! WITH OPTION 2 HAVE MORE POINTS THAN WITH OPTION 1.  The link file
! should probably be the definitive description of the network.
! Something like this might be used to verify that the correct number
! of points have been supplied.
! ----------------------------------------------------------------
SUBROUTINE point_data_scan()
  
  USE utility
  USE file_vars
  USE general_vars

  IMPLICIT NONE

  INTEGER :: iunit
  INTEGER :: link, point
  INTEGER :: lmax
  INTEGER, ALLOCATABLE :: numpt(:)
  CHARACTER (LEN=1024) :: msg

  iunit = fileunit(3)

  CALL open_existing(filename(3), iunit, fatal=.TRUE.)
  
  ! First, scan the file to find the maximum link id

  lmax = 0
  DO WHILE(.TRUE.)
     READ(iunit,*,END=100)link, point
     IF (link .GT. lmax) lmax = link
  END DO
100 CONTINUE
  IF (lmax .LE. 0) THEN
     CALL error_message('No links identified in point file "' // &
          &TRIM(filename(3)) // '"', fatal = .TRUE.)
  END IF
  ALLOCATE(numpt(lmax))


  ! Then, count the number of points for each link id

  REWIND(iunit)
  numpt = 0
  DO WHILE(.TRUE.)
     READ(iunit,*,END=200) link, point
     numpt(link) = numpt(link) + 1
  END DO
200 CONTINUE

  CLOSE(iunit)

  ! set global limits accordingly

  maxlinks = lmax
  maxpoint = MAXVAL(numpt) + 1

  DEALLOCATE(numpt)

  IF (maxpoint .LE. 0) THEN
     CALL error_message('Unable to scan point file "' //&
          &TRIM(filename(3)) // '"', fatal = .TRUE.)
  END IF

  WRITE(msg, *) TRIM(filename(3)) // ": found ", maxlinks, &
       &" links with a maximum points of ", maxpoint
  CALL status_message(msg)


END SUBROUTINE point_data_scan


SUBROUTINE point_data

  ! read in point-related input data

  ! input_option == 1 is point-style; properties set for each point
  ! input_option == 2 is link-style; quick, uniform properties on each link

  USE utility
  USE point_vars
  USE link_vars
  USE file_vars
  USE general_vars, ONLY : units,channel_length_units
  USE transport_vars, ONLY : k_surf
  
  IMPLICIT NONE
  
  INTEGER :: i,link, junk, point,sec_num,io_unit
  DOUBLE PRECISION :: delta_x, slope, start_el,end_el,manning_n,length,diffusion
  DOUBLE PRECISION :: surface_mass_trans
  
  CALL open_existing(filename(3), fileunit(3), fatal=.TRUE.)
  
  io_unit = fileunit(7)
  CALL print_output("POINTS")
  
  
  DO WHILE(.TRUE.)
     
     READ(fileunit(3),*,END=100)link
     BACKSPACE(fileunit(3))
     
     WRITE(io_unit,'("Link = ",i10," input option = ",i10)')link,input_option(link)
     
     SELECT CASE(input_option(link))
        
        !point based input
     CASE(1)
        
        DO i=1,maxpoints(link)
           
           READ(fileunit(3),*)junk,point,x(link,i),section_number(link,i),thalweg(link,i), &
                manning(link,i),k_diff(link,i),k_surf(link,i)
           
           WRITE(io_unit,*)link,point,x(link,i),section_number(link,i),thalweg(link,i), &
                manning(link,i),k_diff(link,i),k_surf(link,i)
           
           kstrick(link,i) = 1.0/manning(link,i)
           
           SELECT CASE(channel_length_units)
           CASE(1) ! length is in feet
              x(link,i) = x(link,i)
           CASE(2) ! length is in meters
              x(link,i) = x(link,i)*3.2808
           CASE(3) ! length is in miles
              x(link,i) = x(link,i)*5280.0
           CASE(4) ! length in kilometers
              x(link,i) = x(link,i)*0.6211*5280.0
           END SELECT
           
        END DO
        
        !--------------------------------------------------------
        !link based input
     CASE(2)
        
        
        READ(fileunit(3),*)junk,length,start_el,end_el,sec_num,manning_n,diffusion,surface_mass_trans
        
        WRITE(io_unit,*)link,length,start_el,end_el,sec_num,manning_n,diffusion,surface_mass_trans
        
        SELECT CASE(channel_length_units)
        CASE(1) ! length is in feet
           length = length
        CASE(2) ! length is in meters
           length = length*3.2808
        CASE(3) ! length is in miles
           length = length*5280.0
        CASE(4) ! length in kilometers
           length = length*0.6211*5280.0
        END SELECT
        
        SELECT CASE(units)
        CASE(2)
           start_el = start_el*3.2808
           end_el = end_el*3.2808
        END SELECT
        
        delta_x = length/(maxpoints(link) - 1)
        slope = (start_el - end_el)/length
        
        DO i=1,maxpoints(link)
           
           IF(i == 1)THEN
              x(link,i) = 0.0
              thalweg(link,i) = start_el
              
           ELSE
              x(link,i) = x(link,i-1) + delta_x
              thalweg(link,i) = thalweg(link,i-1) - slope*delta_x
              
           ENDIF
           
           section_number(link,i) = sec_num
           manning(link,i) = manning_n
           kstrick(link,i) = 1.0/manning_n
           k_diff(link,i) = diffusion
           k_surf(link,i) = surface_mass_trans
           
        END DO
        
     END SELECT
  END DO
100 CLOSE(fileunit(3))

END SUBROUTINE point_data
