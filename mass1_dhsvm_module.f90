! ----------------------------------------------------------------
! MODULE mass1_dhsvm_module
! ----------------------------------------------------------------
MODULE mass1_dhsvm_module

  USE iso_c_binding
  USE utility
  USE julian
  USE date_time
  USE time_series
  USE met_time_series
  USE network_module
  USE link_module
  USE bc_module
  USE scalar_module

  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! TYPE DHSVM_date
  !
  ! Corresponds to the DATE struct in DHSVM
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, BIND(c) :: DHSVM_date
     INTEGER(KIND=C_INT) :: year
     INTEGER(KIND=C_INT) :: month
     INTEGER(KIND=C_INT) :: day
     INTEGER(KIND=C_INT) :: hour
     INTEGER(KIND=C_INT) :: min
     INTEGER(KIND=C_INT) :: sec
     INTEGER(KIND=C_INT) :: jday     ! day of year
     REAL(KIND=C_DOUBLE) :: julian   ! julian day?
  END type DHSVM_date

  TYPE, PUBLIC :: DHSVM_network
     TYPE (link_ptr), ALLOCATABLE :: link_lookup(:)
     TYPE (network), POINTER :: net
  END type DHSVM_network

CONTAINS

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION dhsvm_to_decimal
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION dhsvm_to_decimal(ddate) 

    IMPLICIT NONE
    TYPE (DHSVM_date), INTENT(IN) :: ddate
    DOUBLE PRECISION :: ss

    ss = DBLE(ddate%sec)

    dhsvm_to_decimal = juldays(&
         &ddate%month, ddate%day, ddate%year, &
         &ddate%hour, ddate%min, ss)
  END FUNCTION dhsvm_to_decimal

  ! ----------------------------------------------------------------
  ! SUBROUTINE f2cstring
  ! ----------------------------------------------------------------
  SUBROUTINE f2cstring(fstr, cstr)
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: fstr
    CHARACTER(KIND=c_char), INTENT(OUT) :: cstr(*)
    INTEGER :: i, len
    len = len_trim(fstr)
    DO i = 1, len
       cstr(i) = fstr(i:i)
    END DO
    cstr(len+1) = C_NULL_CHAR
  END SUBROUTINE f2cstring

  ! ----------------------------------------------------------------
  ! SUBROUTINE c2fstring
  ! This assumes some dangerous things about cstr. 
  ! ----------------------------------------------------------------
  SUBROUTINE c2fstring(cstr, fstr)
    IMPLICIT NONE
    TYPE (c_ptr), VALUE :: cstr
    CHARACTER(LEN=1, KIND=c_char), DIMENSION(:), POINTER :: p_chars
    CHARACTER (LEN=*), INTENT(OUT) :: fstr
    INTEGER :: i

    IF (.NOT. C_ASSOCIATED(cstr)) THEN
       FSTR = " "
    ELSE
       CALL C_F_POINTER(cstr, p_chars, [HUGE(0)])
       DO i = 1, LEN(fstr)
          IF (p_chars(i) .EQ. C_NULL_CHAR) EXIT
          fstr(i:i) = p_chars(i)
       END DO
       IF (i .LE. LEN(fstr)) fstr(i:) = " "
    END IF
       
  END SUBROUTINE c2fstring


  ! ----------------------------------------------------------------
  ! SUBROUTINE mass1_initialize
  ! ----------------------------------------------------------------
  SUBROUTINE mass1_initialize(dnet, cfgdir, outdir, start, end, quiet, &
       &dotemp, dolwrad, dobed)

    IMPLICIT NONE
    TYPE (DHSVM_network), INTENT(INOUT) :: dnet
    CHARACTER (LEN=*), INTENT(IN) :: cfgdir, outdir
    TYPE (DHSVM_date), INTENT(INOUT) :: start, end
    LOGICAL, INTENT(IN) :: quiet, dotemp, dolwrad, dobed

    INTEGER :: id, n

    CLASS (link_t), POINTER :: link
    DOUBLE PRECISION, PARAMETER :: zero(5) = 0.0, tempin(5) = 12.0
    INTEGER :: tidx

    ! When using MASS1 from DHSVM, keep temperature between 0 and 100 C
    IF (dotemp) THEN
       temperature_limits = .TRUE.
    END IF

    CALL dnet%net%read(cfgdir, dotemp, dobed)

    ASSOCIATE (cfg => dnet%net%config)
      cfg%time%begin = dhsvm_to_decimal(start)
      cfg%time%end = dhsvm_to_decimal(end)
      cfg%time%time = cfg%time%begin

      cfg%quiet = quiet
    END ASSOCIATE

    IF (dotemp) THEN
       tidx = dnet%net%scalars%temp_index
       IF (tidx .LE. 0) THEN
          CALL error_message("Temperature requested by DHSVM, but not enabled in MASS1",&
               &fatal=.TRUE.)
       END IF
       ! if true DHSVM lw rad is used
       dnet%net%config%do_met_lwrad = dolwrad

       dnet%net%config%do_temp_bed = dobed

       IF (dnet%net%config%do_met_lwrad) THEN
          CALL status_message("MASS1 will use longwave radiation from DHSVM")
       ELSE
          CALL status_message("MASS1 will compute longwave radiation internally")
       END IF

       IF (dnet%net%config%do_temp_bed) THEN
          CALL status_message("MASS1 will simulate stream bed temperature")
       END IF

    END IF

    ! assume link id's are generally contiguous, or at least not too
    ! big, make a lookup table so we can easily find links later;
    ! also, initialize a lateral inflow table for each link, if
    ! necessary
    n = dnet%net%links%maxid()
    ALLOCATE(dnet%link_lookup(n))
    ASSOCIATE (links => dnet%net%links%links, bcs => dnet%net%bcs%bcs)
      CALL links%begin();
      link =>links%current();

      DO WHILE (ASSOCIATED(link))
         id = link%id
         dnet%link_lookup(id)%p => link
         CALL links%next();
         link =>links%current();
      END DO
    END ASSOCIATE

    CALL dnet%net%initialize()

  END SUBROUTINE mass1_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE mass1_prepare_link
  !
  ! This routine is called for every link in the DHSVM mask.  This way
  ! the MASS1 configuration can have links external to the DHSVM domain
  ! that are still part of the MASS1 network.  
  ! ----------------------------------------------------------------
  SUBROUTINE mass1_prepare_link(dnet, linkid)

    IMPLICIT NONE
    TYPE (DHSVM_network), INTENT(INOUT) :: dnet
    INTEGER, INTENT(IN) :: linkid

    CLASS (link_t), POINTER :: link
    CLASS (simple_bc_t), POINTER :: latbc
    CLASS (bc_t), POINTER :: stupid
    TYPE (met_zone_t), POINTER :: zone
    DOUBLE PRECISION :: coeff(4)
    INTEGER :: tidx

    DOUBLE PRECISION, PARAMETER :: zero(5) = 0.0, tempin(5) = 12.0

    link => dnet%link_lookup(linkid)%p

    ! Create a lateral inflow BC

    ALLOCATE(latbc)
    latbc%tbl => time_series_alloc(linkid, 1, 1)
    latbc%tbl%limit_mode = TS_LIMIT_FLAT
    CALL time_series_push(latbc%tbl, dnet%net%config%time%begin, zero)
    stupid => latbc
    CALL dnet%net%bcs%bcs%push(LATFLOW_BC_TYPE, stupid)
    link%latbc => latbc
    NULLIFY(latbc)

    IF (dnet%net%config%do_temp) THEN

       tidx = dnet%net%scalars%temp_index

       ! create a temperature BC for lateral inflow

       ALLOCATE(latbc)
       latbc%tbl => time_series_alloc(linkid, 1, 1)
       latbc%tbl%limit_mode = TS_LIMIT_FLAT
       CALL time_series_push(latbc%tbl, dnet%net%config%time%begin, tempin)
       stupid => latbc
       CALL dnet%net%bcs%bcs%push(TEMP_BC_TYPE, stupid)
       link%species(tidx)%latbc => latbc

       IF (dnet%net%scalars%species(tidx)%p%needmet) THEN

          ! Add a met zone for the link

          coeff(1) = 0.46 ! wind function multiplier
          coeff(2) = 9.2  ! wind function offset
          coeff(3) = 0.47 ! conduction coefficient
          coeff(4) = 0.65 ! "brunt" coefficient for lw atm radiation
          
          ALLOCATE(zone)
          zone%id = link%id
          zone%coeff = coeff
          zone%havelw = dnet%net%config%do_met_lwrad
          
          zone%met => met_time_series_alloc(link%id, 1, zone%havelw)
          zone%met%ts%limit_mode = TS_LIMIT_FLAT
          
          CALL dnet%net%met%zonelist%push(zone)
          link%species(tidx)%met => zone
       END IF
    END IF
  END SUBROUTINE mass1_prepare_link

  
END MODULE mass1_dhsvm_module
