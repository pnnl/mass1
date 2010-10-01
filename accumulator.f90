! ----------------------------------------------------------------
! file: accumulator.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created November  6, 2000 by William A. Perkins
! Last Change: Wed Sep 29 14:43:06 2010 by William A. Perkins <d3g096@bearflag.pnl.gov>
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE accumulator
! ----------------------------------------------------------------
MODULE accumulator

  USE general_vars
  USE link_vars
  USE point_vars
  USE scalars

  IMPLICIT NONE
  
  CHARACTER(LEN=80), SAVE, PRIVATE :: RCS_ID = "$Id$"
  DOUBLE PRECISION, PARAMETER, PRIVATE :: realbig = 1.0e+10, realsmall = 1.0e-10

  TYPE accum_var_rec
     DOUBLE PRECISION, POINTER :: max(:,:)
     DOUBLE PRECISION, POINTER :: min(:,:)
     DOUBLE PRECISION, POINTER :: sum(:,:)
  END TYPE accum_var_rec
  
  TYPE accum_tdg_rec
     TYPE (accum_var_rec) :: press, deltap, sat
  END TYPE accum_tdg_rec

  TYPE accum_rec
     TYPE (accum_var_rec) :: y, q, vel, area, top_width, hyd_radius
     TYPE (accum_var_rec) :: froude_num, friction_slope, bed_shear
     TYPE (accum_var_rec), POINTER :: conc(:)
     TYPE (accum_tdg_rec) :: tdg
  END TYPE accum_rec

  TYPE (accum_rec), PUBLIC :: accum_var
  INTEGER, PRIVATE :: accum_count
  DOUBLE PRECISION, PUBLIC :: accum_time
CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_init_var
  ! ----------------------------------------------------------------
  SUBROUTINE accum_init_var(nlnk, npnt, rec)
    
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nlnk, npnt
    TYPE (accum_var_rec) :: rec

    ALLOCATE(rec%max(nlnk, npnt))
    ALLOCATE(rec%min(nlnk, npnt))
    ALLOCATE(rec%sum(nlnk, npnt))

  END SUBROUTINE accum_init_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_init_tdg
  ! ----------------------------------------------------------------
  SUBROUTINE accum_init_tdg(nlnk, npnt, tdg)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nlnk, npnt
    TYPE (accum_tdg_rec) :: tdg

     CALL accum_init_var(nlnk, npnt, tdg%press)
     CALL accum_init_var(nlnk, npnt, tdg%deltap)
     CALL accum_init_var(nlnk, npnt, tdg%sat)

  END SUBROUTINE accum_init_tdg



  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE accum_initialize()

    IMPLICIT NONE
    INTEGER :: ispecies

    CALL accum_init_var(maxlinks, maxpoint, accum_var%y)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%q)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%vel)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%area)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%top_width)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%hyd_radius)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%froude_num)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%friction_slope)
    CALL accum_init_var(maxlinks, maxpoint, accum_var%bed_shear)

    ALLOCATE(accum_var%conc(max_species))
    
    DO ispecies = 1, max_species
       CALL accum_init_var(maxlinks, maxpoint, accum_var%conc(ispecies))
    END DO

    CALL accum_init_tdg(maxlinks, maxpoint, accum_var%tdg)

  END SUBROUTINE accum_initialize


  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_reset_var
  ! ----------------------------------------------------------------
  SUBROUTINE accum_reset_var(rec)

    IMPLICIT NONE
    TYPE (accum_var_rec) :: rec

    rec%sum = 0.0
    rec%max = realsmall
    rec%min = realbig

  END SUBROUTINE accum_reset_var


  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_reset_tdg
  ! ----------------------------------------------------------------
  SUBROUTINE accum_reset_tdg(tdg)

    IMPLICIT NONE
    TYPE (accum_tdg_rec) :: tdg

    CALL accum_reset_var(tdg%press)
    CALL accum_reset_var(tdg%deltap)
    CALL accum_reset_var(tdg%sat)

  END SUBROUTINE accum_reset_tdg

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_reset
  ! ----------------------------------------------------------------
  SUBROUTINE accum_reset(model_time)

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: model_time
    INTEGER :: ispec

    CALL accum_reset_var(accum_var%y)
    CALL accum_reset_var(accum_var%q)
    CALL accum_reset_var(accum_var%vel)
    CALL accum_reset_var(accum_var%area)
    CALL accum_reset_var(accum_var%top_width)
    CALL accum_reset_var(accum_var%hyd_radius)
    CALL accum_reset_var(accum_var%froude_num)
    CALL accum_reset_var(accum_var%friction_slope)
    CALL accum_reset_var(accum_var%bed_shear)

    DO ispec = 1, max_species
       CALL accum_reset_var(accum_var%conc(ispec))
    END DO

    CALL accum_reset_tdg(accum_var%tdg)

    accum_count = 0
    accum_time = model_time

  END SUBROUTINE accum_reset

  ! ----------------------------------------------------------------
  ! SUBROUTINE accumulate_var
  ! ----------------------------------------------------------------
  SUBROUTINE accumulate_var(val, rec)

    IMPLICIT NONE 

    DOUBLE PRECISION, INTENT(IN) :: val(:,:)
    TYPE (accum_var_rec) :: rec

    WHERE (val .GT. rec%max) rec%max = val
    WHERE (val .LT. rec%min) rec%min = val
    rec%sum = rec%sum + val

  END SUBROUTINE accumulate_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE accumulate_tdg
  ! ----------------------------------------------------------------
  SUBROUTINE accumulate_tdg(tdg)

    USE met_data_module
    USE logicals
    USE gas_functions

    IMPLICIT NONE

    TYPE (accum_tdg_rec) :: tdg
    INTEGER :: link, point
    DOUBLE PRECISION :: tdg_sat, tdg_press, deltap
    DOUBLE PRECISION :: salinity = 0.0
    
    DO link = 1, maxlinks
       IF ((do_temp .AND. temp_exchange) .OR. (do_gas .AND. gas_exchange) ) &
               &CALL update_met_data(time, met_zone(link))
      DO point = 1, maxpoints(link)
         tdg_press = TDGasPress(species(1)%conc(link,point), species(2)%conc(link,point), salinity)
         IF (tdg_press .GT. tdg%press%max(link, point)) &
              &tdg%press%max(link, point) = tdg_press
         IF (tdg_press .LT. tdg%press%min(link, point)) &
              &tdg%press%min(link, point) = tdg_press
         tdg%press%sum(link, point) = tdg%press%sum(link, point) + tdg_press

         tdg_sat = TDGasSaturation(species(1)%conc(link,point), species(2)%conc(link,point), &
              &salinity, baro_press)
         IF (tdg_sat .GT. tdg%sat%max(link, point)) &
              &tdg%sat%max(link, point) = tdg_sat
         IF (tdg_sat .LT. tdg%sat%min(link, point)) &
              &tdg%sat%min(link, point) = tdg_sat
         tdg%sat%sum(link, point) = tdg%sat%sum(link, point) + tdg_sat

         deltap = tdg_press - baro_press
         IF (deltap .GT. tdg%deltap%max(link, point)) &
              &tdg%deltap%max(link, point) = deltap
         IF (deltap .LT. tdg%deltap%min(link, point)) &
              &tdg%deltap%min(link, point) = deltap
         tdg%deltap%sum(link, point) = tdg%deltap%sum(link, point) + deltap
         
      END DO
   END DO
  END SUBROUTINE accumulate_tdg


  ! ----------------------------------------------------------------
  ! SUBROUTINE accumulate
  ! ----------------------------------------------------------------
  SUBROUTINE accumulate()

    IMPLICIT NONE

    INTEGER :: ispec

    CALL accumulate_var(y, accum_var%y)
    CALL accumulate_var(q, accum_var%q)
    CALL accumulate_var(vel, accum_var%vel)
    CALL accumulate_var(area, accum_var%area)
    CALL accumulate_var(top_width, accum_var%top_width)
    CALL accumulate_var(hyd_radius, accum_var%hyd_radius)
    CALL accumulate_var(froude_num, accum_var%froude_num)
    CALL accumulate_var(friction_slope, accum_var%friction_slope)
    CALL accumulate_var(bed_shear, accum_var%bed_shear)

    DO ispec = 1, max_species
       call accumulate_var(species(ispec)%conc(:,1:maxpoint), accum_var%conc(ispec))
    END DO

    CALL accumulate_tdg(accum_var%tdg)
    
    accum_count = accum_count + 1

  END SUBROUTINE accumulate

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_calc_var
  ! ----------------------------------------------------------------
  SUBROUTINE accum_calc_var(rec)

    IMPLICIT NONE

    TYPE (accum_var_rec) :: rec

    rec%sum = rec%sum/DBLE(accum_count)

  END SUBROUTINE accum_calc_var

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_calc_tdg
  ! ----------------------------------------------------------------
  SUBROUTINE accum_calc_tdg(tdg)

    IMPLICIT NONE

    TYPE (accum_tdg_rec) :: tdg

    CALL accum_calc_var(tdg%press)
    CALL accum_calc_var(tdg%sat)
    CALL accum_calc_var(tdg%deltap)

  END SUBROUTINE accum_calc_tdg

  ! ----------------------------------------------------------------
  ! SUBROUTINE accum_calc
  ! ----------------------------------------------------------------
  SUBROUTINE accum_calc(model_time)
    
    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(IN) :: model_time
    INTEGER :: ispec

    IF (accum_count .LE. 0) accum_count = 1

    CALL accum_calc_var(accum_var%y)
    CALL accum_calc_var(accum_var%q)
    CALL accum_calc_var(accum_var%vel)
    CALL accum_calc_var(accum_var%area)
    CALL accum_calc_var(accum_var%top_width)
    CALL accum_calc_var(accum_var%hyd_radius)
    CALL accum_calc_var(accum_var%froude_num)
    CALL accum_calc_var(accum_var%friction_slope)
    CALL accum_calc_var(accum_var%bed_shear)

    DO ispec = 1, max_species
       CALL accum_calc_var(accum_var%conc(ispec))
    END DO

    CALL accum_calc_tdg(accum_var%tdg)

    accum_time = (accum_time + model_time)/2.0
    
  END SUBROUTINE accum_calc


END MODULE accumulator
