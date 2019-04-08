! ----------------------------------------------------------------
! file: mass1_dhsvm_api.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February  4, 2019 by William A. Perkins
! Last Change: 2019-04-08 13:06:00 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
!  FUNCTION mass1_create
! ----------------------------------------------------------------
FUNCTION mass1_create(c_cfgdir, c_outdir, start, end, pid, dotemp) RESULT(net) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module
  IMPLICIT NONE
  TYPE (c_ptr) :: net
  TYPE (c_ptr), VALUE :: c_cfgdir, c_outdir
  TYPE (DHSVM_date), INTENT(INOUT) :: start, end
  INTEGER(KIND=C_INT), VALUE :: pid, dotemp

  TYPE (DHSVM_network), POINTER :: f_net
  CHARACTER (LEN=1024) :: cfgdir, outdir, spath, epath, buf
  LOGICAL :: f_dotemp

  CALL c2fstring(c_cfgdir, cfgdir)
  CALL c2fstring(c_outdir, outdir)

  ALLOCATE(f_net)
  ALLOCATE(f_net%net)
  f_net%net = network()
  f_dotemp = (dotemp .NE. 0)

  utility_error_iounit = 11
  utility_status_iounit = 99

  CALL date_time_flags()
  CALL time_series_module_init()

  ! open the status file - this file records progress through the
  ! input stream and errors
  !
  WRITE(buf, '(I0.4)') pid

  spath = "mass1-status.out" // "." // TRIM(buf)
  epath = "mass1-error.out" // "." // TRIM(buf)
  IF (LEN_TRIM(outdir) .GT. 0) THEN
     spath = TRIM(outdir) // "/" // spath
     epath = TRIM(outdir) // "/" // epath
  END IF
  CALL open_new(spath, utility_status_iounit)
  CALL open_new(epath, utility_error_iounit)

  IF (pid .EQ. 0) THEN
     CALL banner()
  END IF

  CALL mass1_initialize(f_net, cfgdir, outdir, start, end, .TRUE., f_dotemp)

  net = C_LOC(f_net)

END FUNCTION mass1_create

! ----------------------------------------------------------------
! SUBROUTINE mass1_route
! ----------------------------------------------------------------
SUBROUTINE mass1_route(cnet, ddate) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module


  IMPLICIT NONE

  TYPE (C_PTR), VALUE :: cnet
  TYPE (DHSVM_date) :: ddate
  TYPE (DHSVM_network), POINTER :: dnet
  DOUBLE PRECISION :: time

  CALL C_F_POINTER(cnet, dnet)

  time = dhsvm_to_decimal(ddate)

  CALL dnet%net%run_to(time)

END SUBROUTINE mass1_route



! ----------------------------------------------------------------
! SUBROUTINE mass1_update_latq
! ----------------------------------------------------------------
SUBROUTINE mass1_update_latq(cnet, linkid, latq, ddate) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module

  IMPLICIT NONE
  TYPE (C_PTR), VALUE :: cnet
  INTEGER(KIND=C_INT), VALUE :: linkid
  REAL(KIND=C_FLOAT), VALUE :: latq !volume rate
  TYPE (DHSVM_date) :: ddate

  DOUBLE PRECISION :: lq(1), llen, time
  TYPE (DHSVM_network), POINTER :: dnet
  CLASS (link_t), POINTER :: link
  TYPE (time_series_rec), POINTER :: ts

  CALL C_F_POINTER(cnet, dnet)

  link => dnet%link_lookup(linkid)%p
  llen = link%length()

  ! assume everything is in correct units
  lq = latq/llen

  time = dhsvm_to_decimal(ddate)
  ts => link%latbc%table()
  CALL time_series_push(ts, time, lq)

END SUBROUTINE mass1_update_latq

! ----------------------------------------------------------------
! SUBROUTINE mass1_update_latt
! ----------------------------------------------------------------
SUBROUTINE mass1_update_latt(cnet, linkid, latt, ddate) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module
  IMPLICIT NONE
  TYPE (C_PTR), VALUE :: cnet
  INTEGER(KIND=C_INT), VALUE :: linkid
  REAL(KIND=C_FLOAT), VALUE :: latt 
  TYPE (DHSVM_date) :: ddate

  DOUBLE PRECISION :: lt(1), time
  TYPE (DHSVM_network), POINTER :: dnet
  CLASS (link_t), POINTER :: link
  TYPE (time_series_rec), POINTER :: ts
  INTEGER :: tidx
  CHARACTER (LEN=1024) :: msg

  CALL C_F_POINTER(cnet, dnet)

  tidx = dnet%net%scalars%temp_index
  link => dnet%link_lookup(linkid)%p

  IF (tidx .GT. 0) THEN
     time = dhsvm_to_decimal(ddate)
     lt = latt
     ts => link%species(tidx)%latbc%table()
     CALL time_series_push(ts, time, lt)
  ELSE 
     WRITE(msg, *) 'mass1_update_latt: link ', link%id,&
          &': ', ' temperature index not set'
     CALL error_message(msg)
  END IF


END SUBROUTINE mass1_update_latt


! ----------------------------------------------------------------
! SUBROUTINE mass1_update_met
! ----------------------------------------------------------------
SUBROUTINE mass1_update_met(cnet, linkid, &
     &airtemp, dewtemp, windsp, swrad, ddate) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module
  IMPLICIT NONE
  TYPE (C_PTR), VALUE :: cnet
  INTEGER(KIND=C_INT), VALUE :: linkid
  REAL(KIND=C_FLOAT), VALUE :: airtemp, dewtemp, windsp, swrad
  TYPE (DHSVM_date) :: ddate

  DOUBLE PRECISION :: time
  TYPE (DHSVM_network), POINTER :: dnet
  CLASS (link_t), POINTER :: link
  TYPE (time_series_rec), POINTER :: ts
  INTEGER :: tidx
  DOUBLE PRECISION :: metvalues(5)
  CHARACTER (LEN=1024) :: msg

  CALL C_F_POINTER(cnet, dnet)

  tidx = dnet%net%scalars%temp_index
  link => dnet%link_lookup(linkid)%p

  IF (tidx .GT. 0) THEN
     IF (ASSOCIATED(link%species(tidx)%met)) THEN
        time = dhsvm_to_decimal(ddate)
        metvalues(1) = airtemp
        metvalues(2) = dewtemp
        metvalues(3) = windsp
        metvalues(4) = 760.0
        metvalues(5) = swrad
        ts => link%species(tidx)%met%met%ts
        CALL time_series_push(ts, time, metvalues)
     ELSE
        WRITE(msg, *) 'mass1_update_met: link ', link%id,&
          &': ', ' met zone not set'
        CALL error_message(msg)
     END IF
  ELSE 
     WRITE(msg, *) 'mass1_update_met: link ', link%id,&
          &': ', ' temperature index not set'
     CALL error_message(msg)
  END IF
  

END SUBROUTINE mass1_update_met

! ----------------------------------------------------------------
!  FUNCTION mass1_link_outflow
! ----------------------------------------------------------------
FUNCTION mass1_link_outflow(cnet, linkid) RESULT (q) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module

  IMPLICIT NONE
  REAL(KIND=C_DOUBLE) :: q
  TYPE (C_PTR), VALUE :: cnet
  INTEGER(KIND=C_INT), VALUE :: linkid
  TYPE (DHSVM_network), POINTER :: dnet
  CLASS (link_t), POINTER :: link

  CALL C_F_POINTER(cnet, dnet)

  link => dnet%link_lookup(linkid)%p

  q = link%q_down()

END FUNCTION mass1_link_outflow

! ----------------------------------------------------------------
!  FUNCTION mass1_link_inflow
! ----------------------------------------------------------------
FUNCTION mass1_link_inflow(cnet, linkid) RESULT (q) BIND(c)

  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module

  IMPLICIT NONE
  REAL(KIND=C_DOUBLE) :: q
  TYPE (C_PTR), VALUE :: cnet
  INTEGER(KIND=C_INT), VALUE :: linkid
  TYPE (DHSVM_network), POINTER :: dnet
  CLASS (link_t), POINTER :: link

  CALL C_F_POINTER(cnet, dnet)

  link => dnet%link_lookup(linkid)%p

  q = link%q_up()

END FUNCTION mass1_link_inflow

! ----------------------------------------------------------------
!  FUNCTION mass1_link_temp
! ----------------------------------------------------------------
FUNCTION mass1_link_temp(cnet, linkid) RESULT (t) BIND(c)

  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module

  IMPLICIT NONE
  REAL(KIND=C_DOUBLE) :: t
  TYPE (C_PTR), VALUE :: cnet
  INTEGER(KIND=C_INT), VALUE :: linkid
  TYPE (DHSVM_network), POINTER :: dnet
  CLASS (link_t), POINTER :: link
  INTEGER :: tidx
  CHARACTER (LEN=1024) :: msg

  CALL C_F_POINTER(cnet, dnet)

  link => dnet%link_lookup(linkid)%p
  tidx = dnet%net%scalars%temp_index

  t = 0.0
  IF (tidx .GT. 0) THEN
     t = link%c_down(tidx)
  ELSE 
     WRITE(msg, *) 'mass1_link_temp: link ', link%id,&
          &': ', ' temperature index not set'
     CALL error_message(msg)
  END IF

END FUNCTION mass1_link_temp

! ----------------------------------------------------------------
! SUBROUTINE mass1_write_hotstart
! ----------------------------------------------------------------
SUBROUTINE mass1_write_hotstart(cnet, cname) BIND(C)

  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module
  IMPLICIT NONE
  TYPE (C_PTR), VALUE :: cnet
  TYPE (C_PTR), VALUE :: cname
  CHARACTER (LEN=1024) :: fname
  TYPE (DHSVM_network), POINTER :: dnet

  CALL C_F_POINTER(cnet, dnet)
  CALL c2fstring(cname, fname)

  dnet%net%config%restart_save_file = fname
  CALL dnet%net%write_restart()
END SUBROUTINE mass1_write_hotstart


! ----------------------------------------------------------------
! SUBROUTINE mass1_read_hotstart
! ----------------------------------------------------------------
SUBROUTINE mass1_read_hotstart(cnet, cname) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module
  IMPLICIT NONE
  TYPE (C_PTR), VALUE :: cnet
  TYPE (C_PTR), VALUE :: cname
  CHARACTER (LEN=1024) :: fname
  TYPE (DHSVM_network), POINTER :: dnet

  CALL C_F_POINTER(cnet, dnet)
  CALL c2fstring(cname, fname)

  dnet%net%config%restart_load_file = fname
  CALL dnet%net%read_restart()

END SUBROUTINE mass1_read_hotstart

  
! ----------------------------------------------------------------
! SUBROUTINE mass1_destroy
! ----------------------------------------------------------------
SUBROUTINE mass1_destroy(cnet) BIND(c)
  USE, INTRINSIC :: iso_c_binding
  USE mass1_dhsvm_module
  IMPLICIT NONE
  TYPE (C_PTR), VALUE :: cnet
  TYPE (DHSVM_network), POINTER :: dnet

  CALL C_F_POINTER(cnet, dnet)
  DEALLOCATE(dnet%link_lookup)
  DEALLOCATE(dnet%net)
  DEALLOCATE(dnet)

END SUBROUTINE mass1_destroy
