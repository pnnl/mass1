
!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:	table_interp
!
! VERSION and DATE: MASS1 v0.75 3/25/98
!
! PURPOSE: linear interpolation to get BC values at the current
!          time step for each sort of table.
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
! MOD HISTORY: added table_type=5 for laterla inflow; mcr 3/25/98
!              added table_type=6 for temperature inflows; mcr 11/8/98
!
!
!***************************************************************
!

REAL FUNCTION table_interp(time,table_type,column, time_mult)
        
  USE linkbc_vars
  USE general_vars, ONLY : maxtimes,debug_print

  IMPLICIT NONE

  DOUBLE PRECISION :: time, time_mult
  INTEGER :: table_type, column
  INTEGER :: i, istart

  SELECT CASE(table_type)

  CASE(1) ! water inflow

     istart = MAX(linkbc_start(column), 1)
     DO i=istart,maxtimes-1
        IF((time >= linkbc_time(i,column)*time_mult) .AND.&
             & (time <= linkbc_time(i+1,column)*time_mult)) EXIT
     END DO
     linkbc_start(column) = MAX(MIN(i - 1, maxtimes - 2), 0)

     IF(debug_print == 1)&
          & WRITE(11,*)"link inflows",time,column,linkbc_time(i,column),&
          & linkbc_time(i+1,column),linkbc(i,column),linkbc(i+1,column),&
          & linkbc_start(column)
        
     table_interp = linkbc(i,column) +&
          & (time - linkbc_time(i,column)*time_mult)*(linkbc(i+1,column) -&
          & linkbc(i,column))/(linkbc_time(i+1,column)*time_mult -&
          & linkbc_time(i,column)*time_mult)

     !------------------------------------------------------------------------------

  CASE(2)                       ! gas transport varibles

     istart = MAX(transbc_start(column), 1)
     DO i=istart,maxtimes-1
        IF((time >= transbc_time(i,column)*time_mult) .AND.&
             & (time <= transbc_time(i+1,column)*time_mult)) EXIT
     END DO
     transbc_start(column) = MAX(MIN(i - 1, maxtimes - 2), 0)

     IF(debug_print == 1)&
          & WRITE(11,*)"gas inflow conc ",time,column,transbc_time(i,column),&
          & transbc_time(i+1,column),transbc(i,column),transbc(i+1,column),&
          & transbc_start(column)

     table_interp = transbc(i,column) +&
          & (time - transbc_time(i,column)*time_mult)*(transbc(i+1,column) -&
          & transbc(i,column))/(transbc_time(i+1,column)*time_mult -&
          & transbc_time(i,column)*time_mult)

  CASE(3)                       ! generation flow

     istart = MAX(gen_start(column), 1)
     DO i=istart,maxtimes-1
        IF((time >= gen_time(i,column)*time_mult) .AND.&
             & (time <= gen_time(i+1,column)*time_mult)) EXIT
     END DO
     gen_start(column) = MAX(MIN(i - 1, maxtimes - 2), 0)
 
     IF(debug_print == 1)&
          & WRITE(11,*)"generation flows ",time,column,gen_time(i,column),&
          & gen_time(i+1,column),gen_flow(i,column),gen_flow(i+1,column),&
          & gen_start(column)

     table_interp = gen_flow(i,column) +&
          & (time - gen_time(i,column)*time_mult)*(gen_flow(i+1,column) -&
          & gen_flow(i,column))/(gen_time(i+1,column)*time_mult -&
          & gen_time(i,column)*time_mult)
     
  CASE(4)                       ! spill flow

     istart = MAX(spill_start(column), 1)
     DO i=istart,maxtimes-1
        IF((time >= spill_time(i,column)*time_mult) .AND.&
             & (time <= spill_time(i+1,column)*time_mult)) EXIT
     END DO
     spill_start(column) = MAX(MIN(i - 1, maxtimes - 2), 0)
 
     IF(debug_print == 1)&
          & WRITE(11,*)"spill flows ",time,column,spill_time(i,column),&
          & spill_time(i+1,column),spill_flow(i,column),spill_flow(i+1,column),&
          & spill_start(column)

     table_interp = spill_flow(i,column) +&
          & (time - spill_time(i,column)*time_mult)*(spill_flow(i+1,column) -&
          & spill_flow(i,column))/(spill_time(i+1,column)*time_mult -&
          & spill_time(i,column)*time_mult)

     !-------------------------------------------------------------------------------
  CASE(5)                       ! lateral inflow to the channel

     istart = MAX(latflowbc_start(column), 1)
     DO i=istart,maxtimes-1
        IF((time >= latflowbc_time(i,column)*time_mult) .AND.&
             & (time <= latflowbc_time(i+1,column)*time_mult)) EXIT
     END DO
     latflowbc_start(column) = MAX(MIN(i - 1, maxtimes - 2), 0)
     
     IF(debug_print == 1)&
          & WRITE(11,*)"lateral inflow",time,column,latflowbc_time(i,column),&
          & latflowbc_time(i+1,column),latflowbc(i,column),latflowbc(i+1,column),&
          & latflowbc_start(column)
        
     table_interp = latflowbc(i,column) +&
          & (time - latflowbc_time(i,column)*time_mult)*(latflowbc(i+1,column) -&
          & latflowbc(i,column))/(latflowbc_time(i+1,column)*time_mult -&
          & latflowbc_time(i,column)*time_mult)

!-------------------------------------------------------------------------------
  CASE(6)                       ! temp transport varibles
	
     istart = MAX(tempbc_start(column), 1)
     DO i=istart,maxtimes-1
        IF((time >= tempbc_time(i,column)*time_mult) .AND.&
             & (time <= tempbc_time(i+1,column)*time_mult)) EXIT
     END DO
     tempbc_start(column) = MAX(MIN(i - 1, maxtimes - 2), 0)

     IF (debug_print == 1)&
          & WRITE(11,*)"temperature:",time,column,&
          & tempbc_time(i,column), tempbc_time(i+1,column),&
          & tempbc(i,column),tempbc(i+1,column), tempbc_start(column)
        
     table_interp = tempbc(i,column) +&
          & (time - tempbc_time(i,column)*time_mult)*(tempbc(i+1,column) -&
          & tempbc(i,column))/(tempbc_time(i+1,column)*time_mult -&
          & tempbc_time(i,column)*time_mult)
        
  END SELECT

END FUNCTION table_interp
