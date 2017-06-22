
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
  USE link_vars
  USE point_vars
  USE fluvial_coeffs
  USE flow_coeffs
  USE confluence_module

  IMPLICIT NONE

  DOUBLE PRECISION :: a,b,c,d,g,ap,bp,cp,dp,gp,denom
  DOUBLE PRECISION :: latq_old,latq_new
  DOUBLE PRECISION :: depth,area_temp,width,conveyance,dkdy,hydrad
  DOUBLE PRECISION :: bcval,dy,dq,y_new_time, q_new_time
  DOUBLE PRECISION :: temp
  DOUBLE PRECISION :: delta_x

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
        q1 = q(link,point)
        e(link,point) = 0.0
        f(link,point) = bcval - q1

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
           depth = MAX(depth, 0.0d00)
           
           CALL ptsection(link, point_num)%p%props(depth, &
                &area_temp, hydrad, width, conveyance, dkdy)
           conveyance = config%res_coeff*kstrick(link,point_num)*conveyance
           dkdy = config%res_coeff*kstrick(link,point_num)*dkdy

           d1 = depth
           fr1 = froude_num(link,point)
           y1 = y(link,point)
           q1 = q(link,point)
           a1 = area_temp
           b1 = width
           k1 = conveyance
           ky1 = dkdy

           vel(link,point_num) = q1/a1
           area_old(link,point_num) = a1
           q_old(link,point_num) = q1
           y_old(link,point_num) = y1

           point_num = point + 1
           depth = y(link,point+1) - thalweg(link,point+1)
           depth = MAX(depth, 0.0D00)

           CALL ptsection(link, point_num)%p%props(depth, &
                &area_temp, hydrad, width, conveyance, dkdy)
           conveyance = config%res_coeff*kstrick(link,point_num)*conveyance
           dkdy = config%res_coeff*kstrick(link,point_num)*dkdy

           d2 = depth
           fr2 = froude_num(link,point+1)
           y2 = y(link,point+1)
           q2 = q(link,point+1)
           a2 = area_temp
           b2 = width
           k2 = conveyance
           ky2 = dkdy

           vel(link,point_num) = q2/a2
           area_old(link,point_num) = a2
           q_old(link,point_num) = q2
           y_old(link,point_num) = y2

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

           CALL fluvial_coeff(a,b,c,d,g,ap,bp,cp,dp,gp,delta_x,&
                &config%time%delta_t,config%grav,&
                &latq_old,latq_new,lpiexp(link))


           ! nonfluvial internal links ----------------------------
        ELSE

           ! nonfluvial links also need q_old for
           ! transport

           ! FIXME: test usbc association before using
           bcval = usbc(link)%p%current_value

           CALL nonfluvial_coeff(link,point,bcval,a,b,c,d,g,ap,bp,cp,dp,gp)

           q_old(link, :) = q(link, :)

        END IF

        denom = (c*dp - cp*d)
        l(link,point) = (a*dp - ap*d)/denom
        m(link,point) = (b*dp - bp*d)/denom
        n(link,point) = (d*gp - dp*g)/denom

        denom = b - m(link,point)*(c + d*e(link,point))
        e(link,point+1) = (l(link,point)*(c + d*e(link,point)) - a)/denom
        f(link,point+1) = (n(link,point)*(c + d*e(link,point)) +d*f(link,point) + g)/denom

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

           y1 = y(link,point)
           y_new_time = bcval

           dy = y_new_time - y1
           dq = e(link,point)*dy + f(link,point)
        CASE(2)
           ! given Q(t)
           q1 = q(link,point)
           q_new_time = bcval
           dq = q_new_time - q1
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
           dy = (dq - f(link,point))/e(link,point)

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

           CALL ptsection(link, point)%p%props(depth, &
                &area_temp, hydrad, width, conveyance, dkdy)
           conveyance = config%res_coeff*kstrick(link,point)*conveyance
           dkdy = config%res_coeff*kstrick(link,point)*dkdy

           area(link,point) = area_temp
           top_width(link,point) = width
           hyd_radius(link,point) = hydrad
           froude_num(link,point) = &
                &SQRT((q(link,point)**2*width)/(config%grav*area_temp**3))

           IF (froude_num(link, point) .GE. 1.0) THEN
              WRITE (msg, '("warning: supercritial (Fr=", F5.1, ") indicated at link ", I3, ", point ", I3)')&
                   &froude_num(link, point), link, point
              CALL status_message(msg)
           END IF

           friction_slope(link,point) =&
                & ((q(link,point)*manning(link,point))/&
                & (config%res_coeff*area_temp*(hydrad**2.0)**0.3333333))**2.0
           bed_shear(link,point) = &
                &config%unit_weight_h2o*hydrad*friction_slope(link,point)

           IF (point .GE. maxpoints(link)) THEN
              delta_x = ABS(x(link,point-1) - x(link,point))
           ELSE
              delta_x = ABS(x(link,point+1) - x(link,point))
           END IF
           courant_num(link, point) = &
                &ABS(q(link,point))/area_temp*config%time%delta_t/delta_x
           diffuse_num(link, point) = &
                &2.0*k_diff(link,point)*config%time%delta_t/delta_x/delta_x

        END DO
     END SELECT
  END DO



END SUBROUTINE flow_sim
