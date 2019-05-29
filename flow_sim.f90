
!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME: flow_sim
!
! VERSION and DATE: MASS1 v0.75 3/25/98
!
! PURPOSE: hydraulics solver for St. Venant Equations
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS: based on an old homework problem from Holly.
!
!
! MOD HISTORY: added hydrad to CALL section and added
!              calculation of froude_num, friction_slope, etc. ; mcr 11/21/1997
!                          added lateral inflow; mcr 3/25/98
!
!***************************************************************
!

! ----------------------------------------------------------------
! SUBROUTINE depth_check
! ----------------------------------------------------------------
SUBROUTINE depth_check(thalweg, y, q)
  USE general_vars, ONLY: depth_minimum
  IMPLICIT NONE
  DOUBLE PRECISION, INTENT(IN) :: thalweg
  DOUBLE PRECISION, INTENT(INOUT) :: y, q
  DOUBLE PRECISION :: depth

  depth = y - thalweg
  IF (depth .LT. depth_minimum) THEN
     y = thalweg + depth_minimum
     ! q = 0.0
  END if
END SUBROUTINE depth_check


SUBROUTINE flow_sim

  ! $DEBUG

  USE mass1_config
  USE general_vars
  USE cross_section
  USE link_vars
  USE point_vars
  USE fluvial_coeffs
  USE confluence_module

  IMPLICIT NONE

  DOUBLE PRECISION :: denom
  DOUBLE PRECISION :: latq_old,latq_new
  DOUBLE PRECISION :: depth,area_temp,width,perim,conveyance,dkdy,hydrad
  DOUBLE PRECISION :: bcval,dy,dq,y_new_time, q_new_time
  DOUBLE PRECISION :: delta_x
  TYPE (fluvial_state) :: s1, s2
  TYPE (coeff) :: cf
  TYPE (xsection_prop) :: props


  INTEGER :: i,point_num,link,point

  LOGICAL :: fluvial

  CHARACTER (LEN=1024) :: msg

  ! run through links top down according to computational order


  links_forward: DO i=1,config%maxlinks

     link = comporder(i)

     SELECT CASE(linktype(link))
     CASE(1,20,21)
        fluvial = .TRUE.
     CASE(2,3,4,5,6,7,12,13)
        fluvial = .FALSE.
     END SELECT

     ! set upstream bc q(t) or junction condition for first point

     point = 1

     IF (.NOT. ASSOCIATED(ucon(link)%p)) THEN  ! must be an upstream most link
        bcval = usbc(link)%p%current_value
        s1%q = q(link,point)
        e(link,point) = 0.0
        f(link,point) = bcval - s1%q

     ELSE
        e(link,point) = ucon(link)%p%coeff_e()
        f(link,point) = ucon(link)%p%coeff_f()
     END IF


     ! do internal links
     points: DO point=1,maxpoints(link)-1

        IF( fluvial )THEN
           ! fluvial links
           ! set geometric data for points i, i+1
           point_num = point
           depth = y(link,point) - thalweg(link,point) !remember y is ELEVATION
           depth = MAX(depth, depth_minimum)
           
           CALL ptsection(link, point_num)%p%props(depth, props)
           area_temp = props%area
           hydrad = props%hydrad
           width = props%topwidth
           perim = props%wetperim
           conveyance = kstrick(link,point_num)*props%conveyance
           dkdy = kstrick(link,point_num)*props%dkdy

           s1%d = depth
           s1%y = y(link,point)
           s1%q = q(link,point)
           s1%a = area_temp
           s1%b = width
           s1%k = conveyance
           s1%ky = dkdy
           s1%fr = froude_num(link,point)

           IF (point .EQ. 1) THEN
              IF (s1%a .GT. 0.0D00) THEN
                 vel(link,point_num) = s1%q/s1%a
                 area_old(link,point_num) = s1%a
              ELSE 
                 vel(link,point_num) = 0.0
                 area_old(link,point_num) = 0.0
              END IF
              q_old(link,point_num) = s1%q
              y_old(link,point_num) = s1%y
           END IF
           
           point_num = point + 1
           depth = y(link,point+1) - thalweg(link,point+1)
           depth = MAX(depth, depth_minimum)

           CALL ptsection(link, point_num)%p%props(depth, props)
           area_temp = props%area
           hydrad = props%hydrad
           width = props%topwidth
           perim = props%wetperim
           conveyance = kstrick(link,point_num)*props%conveyance
           dkdy = kstrick(link,point_num)*props%dkdy

           s2%d = depth
           s2%y = y(link,point+1)
           s2%q = q(link,point+1)
           s2%a = area_temp
           s2%b = width
           s2%k = conveyance
           s2%ky = dkdy
           s2%fr = froude_num(link,point+1)

           IF (s2%a .GT. 0.0) THEN
              vel(link,point_num) = s2%q/s2%a
              area_old(link,point_num) = s2%a
           ELSE 
              vel(link,point_num) = 0.0
              area_old(link,point_num) = 0.0
           END IF
           q_old(link,point_num) = s2%q
           y_old(link,point_num) = s2%y

           delta_x = ABS(x(link,point+1) - x(link,point))

           ! uniform lateral inflow per unit length
           IF(config%do_latflow)THEN
              latq_old = lateral_inflow(link,point) 
              lateral_inflow_old(link,point) = latq_old
              IF (ASSOCIATED(latbc(link)%p)) THEN
                 lateral_inflow(link,point) = &
                      &latbc(link)%p%current_value
                 latq_new = lateral_inflow(link,point)
              ELSE
                 latq_old = 0.0
                 latq_new = 0.0
              ENDIF
           ELSE
              latq_old = 0.0
              latq_new = 0.0
           ENDIF

           CALL fluvial_coeff(s1, s2, cf, delta_x,&
                &config%time%delta_t,config%grav,&
                &latq_old,latq_new,lpiexp(link), depth_threshold)


           ! nonfluvial internal links ----------------------------
        ELSE

           ! nonfluvial links also need q_old for
           ! transport

           ! FIXME: test usbc association before using
           bcval = usbc(link)%p%current_value

           CALL nonfluvial_coeff(link,point,bcval,cf)

           q_old(link, :) = q(link, :)

        END IF

        denom = (cf%c*cf%dp - cf%cp*cf%d)
        l(link,point) = (cf%a*cf%dp - cf%ap*cf%d)/denom
        m(link,point) = (cf%b*cf%dp - cf%bp*cf%d)/denom
        n(link,point) = (cf%d*cf%gp - cf%dp*cf%g)/denom

        denom = cf%b - m(link,point)*(cf%c + cf%d*e(link,point))
        e(link,point+1) = (l(link,point)*(cf%c + cf%d*e(link,point)) - cf%a)/denom
        f(link,point+1) = (n(link,point)*(cf%c + cf%d*e(link,point)) + &
             &cf%d*f(link,point) + cf%g)/denom

     END DO points


  END DO links_forward

  !------------------------------------------------------------------------------
  ! run through links bottom to top

  links_backward: DO i=config%maxlinks,1,-1

     link = comporder(i)

     point = maxpoints(link)

     ! set downstream bc y(t) or Q(t)  OR junction conditions

     IF (.NOT. ASSOCIATED(dcon(link)%p))THEN

        bcval = dsbc(link)%p%current_value
        SELECT CASE(config%dsbc_type)
        CASE(1)
           ! given downstream stage y(t)

           s1%y = y(link,point)
           y_new_time = bcval

           dy = y_new_time - s1%y
           dq = e(link,point)*dy + f(link,point)
        CASE(2)
           ! given Q(t)
           s1%q = q(link,point)
           q_new_time = bcval
           dq = q_new_time - s1%q
           dy = (dq - f(link,point))/e(link,point)
        END SELECT

        !update stage and discharge at last point on the link
        y(link,point) = y(link,point) + dy
        q(link,point) = q(link,point) + dq

     ELSE
        dy = dcon(link)%p%elev() - y(link,point)
        dq = e(link,point)*dy + f(link,point)
        y(link,point) = y(link,point) + dy
        q(link,point) = q(link,point) + dq

     END IF

     DO point=maxpoints(link)-1,1,-1

        IF(linktype(link) == 2)THEN
           bcval = usbc(link)%p%current_value
           dq = bcval - q(link,point)
           IF (e(link,point) .GT. 0.0) THEN
              dy = (dq - f(link,point))/e(link,point)
           ELSE 
              dy = 0.0          ! not sure if this ever happens
           END IF

        ELSEIF(linktype(link) == 6)THEN

           bcval = usbc(link)%p%current_value

           dq = bcval - q(link,point)
           dy = (dq - f(link,point))/e(link,point)

        ELSE

           dy = l(link,point)*dy + m(link,point)*dq + n(link,point)
           dq = e(link,point)*dy + f(link,point)

        ENDIF
        y(link,point) = y(link,point) + dy
        q(link,point) = q(link,point) + dq

     END DO


  END DO links_backward


  ! Initialize these so unused parts of the array do not affect later
  ! calls to max/min
  froude_num = 10.0
  courant_num = 0.0
  diffuse_num = 0.0

  !------------------------------------------------------------------------------
  ! computes additional data after hydraulics have been
  ! updated for this time 
  !-------------------------------------------------------------------------------
  DO link = 1,config%maxlinks
     SELECT CASE(linktype(link)) 
     CASE(1,20,21)
        DO point = 1,maxpoints(link)

           CALL depth_check(thalweg(link, point), y(link,point), q(link,point))
           depth = y(link,point) - thalweg(link,point)

           CALL ptsection(link, point)%p%props(depth, props)
           area_temp = props%area
           hydrad = props%hydrad
           width = props%topwidth
           perim = props%wetperim
           conveyance = kstrick(link,point)*props%conveyance
           dkdy = kstrick(link,point)*props%dkdy


           IF (point .GE. maxpoints(link)) THEN
              delta_x = ABS(x(link,point-1) - x(link,point))
           ELSE
              delta_x = ABS(x(link,point+1) - x(link,point))
           END IF

           top_width(link,point) = width
           hyd_radius(link,point) = hydrad
           IF (area_temp .GT. 0.0) THEN
              area(link,point) = area_temp
              froude_num(link,point) = &
                   &SQRT((q(link,point)**2*width)/(config%grav*area_temp**3))
              friction_slope(link,point) =&
                   & ((q(link,point))/&
                   & (kstrick(link,point)*area_temp*(hydrad**2.0)**0.3333333))**2.0
              courant_num(link, point) = &
                   &ABS(q(link,point))/area_temp*config%time%delta_t/delta_x
           ELSE 
              area(link,point) = 0.0
              froude_num(link,point) = 0.0
              friction_slope(link,point) = 0.0
              courant_num(link, point) = 0.0
           END IF

           bed_shear(link,point) = &
                &config%unit_weight_h2o*hydrad*friction_slope(link,point)

           diffuse_num(link, point) = &
                &2.0*k_diff(link,point)*config%time%delta_t/delta_x/delta_x

           IF (froude_num(link, point) .GE. 1.0) THEN
              WRITE (msg, '("warning: supercritial (Fr=", F5.1, ") indicated at link ", I3, ", point ", I3)')&
                   &froude_num(link, point), link, point
              CALL status_message(msg)
           END IF

        END DO
     END SELECT
  END DO



END SUBROUTINE flow_sim
