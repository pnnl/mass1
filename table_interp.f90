
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
    INTEGER :: i 


SELECT CASE(table_type)

   CASE(1) ! water inflow

   DO i=1,maxtimes-1

     IF((time >= linkbc_time(i,column)*time_mult) .AND. (time <= linkbc_time(i+1,column)*time_mult)) EXIT

   END DO

   IF(debug_print == 1)&
        & WRITE(11,*)"link inflows",time,column,linkbc_time(i,column),&
        & linkbc_time(i+1,column),linkbc(i,column),linkbc(i+1,column)
        
   table_interp = linkbc(i,column) +&
        & (time - linkbc_time(i,column)*time_mult)*(linkbc(i+1,column) -&
        & linkbc(i,column))/(linkbc_time(i+1,column)*time_mult -&
        & linkbc_time(i,column)*time_mult)

!------------------------------------------------------------------------------
		CASE(2)	 ! gas transport varibles

		 
		DO i=1,maxtimes-1

                   IF((time >= transbc_time(i,column)*time_mult) .AND. (time <= transbc_time(i+1,column)*time_mult)) EXIT

        END DO

		!WRITE(*,*)time,column,inflow_time(i),inflow_time(i+1),inflow(i,column),inflow(i+1,column)
        
        table_interp = transbc(i,column) +&
             & (time - transbc_time(i,column)*time_mult)*(transbc(i+1,column) -&
             & transbc(i,column))/(transbc_time(i+1,column)*time_mult -&
             & transbc_time(i,column)*time_mult)

		CASE(3) ! generation flow

		DO i=1,maxtimes-1

        IF((time >= gen_time(i,column)*time_mult) .AND. (time <= gen_time(i+1,column)*time_mult)) EXIT

        END DO
 
        table_interp = gen_flow(i,column) +&
             & (time - gen_time(i,column)*time_mult)*(gen_flow(i+1,column) -&
             & gen_flow(i,column))/(gen_time(i+1,column)*time_mult -&
             & gen_time(i,column)*time_mult)



		CASE(4) ! spill flow

		DO i=1,maxtimes-1

        IF((time >= spill_time(i,column)*time_mult) .AND. (time <= spill_time(i+1,column)*time_mult)) EXIT

        END DO
 
        table_interp = spill_flow(i,column) +&
             & (time - spill_time(i,column)*time_mult)*(spill_flow(i+1,column) -&
             & spill_flow(i,column))/(spill_time(i+1,column)*time_mult -&
             & spill_time(i,column)*time_mult)

!-------------------------------------------------------------------------------
		CASE(5) ! lateral inflow to the channel

		DO i=1,maxtimes-1

        IF((time >= latflowbc_time(i,column)*time_mult) .AND. (time <= latflowbc_time(i+1,column)*time_mult)) EXIT

        END DO

        IF(debug_print == 1)&
             & WRITE(11,*)"lateral inflow",time,column,latflowbc_time(i,column),&
             & latflowbc_time(i+1,column),latflowbc(i,column),latflowbc(i+1,column)
        
		table_interp = latflowbc(i,column) +&
                     & (time - latflowbc_time(i,column)*time_mult)*(latflowbc(i+1,column) -&
                     & latflowbc(i,column))/(latflowbc_time(i+1,column)*time_mult -&
                     & latflowbc_time(i,column)*time_mult)

!-------------------------------------------------------------------------------
		CASE(6)	 ! temp transport varibles

		 
                   DO i=1,maxtimes-1

!                      IF (debug_print == 1)&
!                           & WRITE(11,*)"temperature:",time,column,&
!                           & tempbc_time(i,column), tempbc_time(i+1,column),&
!                           & tempbc(i,column),tempbc(i+1,column)
                      IF((time >= tempbc_time(i,column)*time_mult) .AND.&
                           & (time <= tempbc_time(i+1,column)*time_mult)) EXIT

                   END DO
    
                   IF (debug_print == 1)&
                        & WRITE(11,*)"temperature:",time,column,&
                        & tempbc_time(i,column), tempbc_time(i+1,column),&
                        & tempbc(i,column),tempbc(i+1,column)
        
    table_interp = tempbc(i,column) +&
         & (time - tempbc_time(i,column)*time_mult)*(tempbc(i+1,column) -&
         & tempbc(i,column))/(tempbc_time(i+1,column)*time_mult -&
         & tempbc_time(i,column)*time_mult)


        
END SELECT

END FUNCTION table_interp
