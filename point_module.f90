! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 12, 2017 by William A. Perkins
! Last Change: 2019-06-20 06:41:25 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE point_module
! ----------------------------------------------------------------
MODULE point_module

  USE cross_section
  USE general_vars, ONLY: depth_minimum

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE point_hydro_state
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: point_hydro_state
     DOUBLE PRECISION :: y, q, v
     DOUBLE PRECISION :: lateral_inflow
     DOUBLE PRECISION :: froude_num
     DOUBLE PRECISION :: friction_slope, bed_shear
     DOUBLE PRECISION :: courant_num, diffuse_num
  END type point_hydro_state

  ! ----------------------------------------------------------------
  ! TYPE point_transport_state
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: point_transport_state
     TYPE (point_hydro_state) :: hnow, hold
     TYPE (xsection_prop) :: xsprop, xspropold
     DOUBLE PRECISION :: twater
     DOUBLE PRECISION, POINTER, DIMENSION(:) :: cnow, cold
  END type point_transport_state

  ! ----------------------------------------------------------------
  ! TYPE point_sweep_coeff
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: point_sweep_coeff
     DOUBLE PRECISION :: e,f,l,m,n
  END type point_sweep_coeff

  ! ----------------------------------------------------------------
  ! TYPE point_t
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: point_t
     DOUBLE PRECISION :: x, thalweg
     DOUBLE PRECISION :: manning, k_diff, kstrick
     TYPE (xsection_ptr) :: xsection
     TYPE (xsection_prop) :: xsprop, xspropold
     TYPE (point_hydro_state) :: hnow, hold
     TYPE (point_sweep_coeff) :: sweep
     TYPE (point_transport_state) :: trans
   CONTAINS
     PROCEDURE :: section_update => point_section_update
     PROCEDURE :: depth_check => point_depth_check
     PROCEDURE :: hydro_update => point_hydro_update
     PROCEDURE :: assign => point_assign
     PROCEDURE :: transport_interp => point_transport_interp
  END type point_t

  ! ----------------------------------------------------------------
  ! TYPE point_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: point_ptr
     TYPE (point_t), POINTER :: p
  END type point_ptr

  PUBLIC hydro_average

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_average
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_average(h0, h1, h)

    IMPLICIT NONE
    TYPE (point_hydro_state), INTENT(IN) :: h0, h1
    TYPE (point_hydro_state), INTENT(INOUT) :: h

    h%y = 0.5*(h0%y + h1%y)
    h%q = 0.5*(h0%q + h1%q)
    h%v = 0.5*(h0%v + h1%v)
    h%lateral_inflow = 0.5*(h0%lateral_inflow + h1%lateral_inflow)
    h%friction_slope = 0.5*(h0%friction_slope + h1%friction_slope)
    h%bed_shear = 0.5*(h0%bed_shear + h1%bed_shear)
    h%courant_num = 0.5*(h0%courant_num + h1%courant_num)
    h%diffuse_num = 0.5*(h0%diffuse_num + h1%diffuse_num)

  END SUBROUTINE hydro_average


  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_interp
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_interp(t, told, tnew, hold, hnew, h)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t, told, tnew
    TYPE (point_hydro_state), INTENT(IN) :: hold, hnew
    TYPE (point_hydro_state), INTENT(INOUT) :: h
    DOUBLE PRECISION :: dlinear_interp

    h%y = dlinear_interp(hold%y, told, hnew%y, tnew, t)
    h%q = dlinear_interp(hold%q, told, hnew%q, tnew, t)
    h%v = dlinear_interp(hold%v, told, hnew%v, tnew, t)
    h%lateral_inflow = &
         &dlinear_interp(hold%lateral_inflow, told, hnew%lateral_inflow, tnew, t)
    h%friction_slope = &
         &dlinear_interp(hold%friction_slope, told, hnew%friction_slope, tnew, t)
    h%bed_shear = dlinear_interp(hold%bed_shear, told, hnew%bed_shear, tnew, t)
    h%courant_num = dlinear_interp(hold%courant_num, told, hnew%courant_num, tnew, t)
    h%diffuse_num = dlinear_interp(hold%diffuse_num, told, hnew%diffuse_num, tnew, t)

    
  END SUBROUTINE hydro_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE point_section_update
  ! ----------------------------------------------------------------
  SUBROUTINE point_section_update(this)

    IMPLICIT NONE
    CLASS (point_t), INTENT(INOUT) :: this
    DOUBLE PRECISION :: depth

    ASSOCIATE ( xs => this%xsection%p )

      depth = this%hnow%y - this%thalweg
      CALL xs%props(depth, this%xsprop)
      this%xsprop%conveyance = &
           &this%kstrick*this%xsprop%conveyance
      this%xsprop%dkdy = &
           &this%kstrick*this%xsprop%dkdy
      IF (this%xsprop%area .GT. 0.0D00) THEN
         this%hnow%v = this%hnow%q/this%xsprop%area
      ELSE 
         this%hnow%v = 0.0
         this%xsprop%area = 0.0
      END IF
      !this%trans%xsprop = this%xsprop
    END ASSOCIATE

  END SUBROUTINE point_section_update

  ! ----------------------------------------------------------------
  ! SUBROUTINE point_assign
  ! 
  ! point_section_update() must be called before this
  ! ----------------------------------------------------------------
  SUBROUTINE point_assign(this, y, d, q, a, b, k, ky, fr)

    IMPLICIT NONE
    CLASS (point_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(OUT) :: y, d, q, a, b, k, ky, fr

    y = this%hnow%y
    d = y - this%thalweg
    q = this%hnow%q
    a = this%xsprop%area
    b = this%xsprop%topwidth
    k = this%xsprop%conveyance
    ky = this%xsprop%dkdy
    fr = this%hnow%froude_num

  END SUBROUTINE point_assign

  ! ----------------------------------------------------------------
  ! SUBROUTINE point_hydro_update
  ! ----------------------------------------------------------------
  SUBROUTINE point_hydro_update(this, grav, unitwt, deltat, deltax)

    IMPLICIT NONE
    CLASS (point_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: grav, unitwt, deltat, deltax

    DOUBLE PRECISION :: depth

    ASSOCIATE (h => this%hnow, xs => this%xsprop)

      depth = h%y - this%thalweg
      IF (depth .LT. depth_minimum) THEN
         h%y = this%thalweg + depth_minimum
      END IF

      CALL this%section_update()

      IF (xs%area .GT. 0.0) THEN
         h%froude_num = &
              &SQRT((h%q*h%q*xs%topwidth)/(grav*xs%area*xs%area*xs%area))
         h%friction_slope = &
              &((h%q)/(this%kstrick*xs%area*(xs%hydrad*xs%hydrad)**0.3333333))**2
         IF (deltax .GT. 0.0) THEN
            h%courant_num = &
                 &ABS(h%q)/xs%area*deltat/deltax
         ELSE
            h%courant_num = 0.0
         END IF
      ELSE 
         h%froude_num = 0.0
         h%friction_slope = 0.0
         h%courant_num = 0.0
      END IF

      h%bed_shear = unitwt*xs%hydrad*h%friction_slope
      h%diffuse_num = 2.0*this%k_diff*deltat/deltax/deltax

    END ASSOCIATE

  END SUBROUTINE point_hydro_update

  ! ----------------------------------------------------------------
  ! SUBROUTINE point_transport_interp
  ! ----------------------------------------------------------------
  SUBROUTINE point_transport_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (point_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1
    DOUBLE PRECISION :: depth

    this%trans%hold = this%trans%hnow
    this%trans%xspropold = this%trans%xsprop
    
    CALL hydro_interp(tnow, htime0, htime1, &
         &this%hold, this%hnow, this%trans%hnow)

    depth = this%trans%hnow%y - this%thalweg
    CALL this%xsection%p%props(depth, this%trans%xsprop)
    ! conveyance not needed
    IF (this%trans%xsprop%area .GT. 0.0D00) THEN
       this%trans%hnow%v = this%trans%hnow%q/this%trans%xsprop%area
    ELSE 
       this%trans%hnow%v = 0.0
       this%trans%xsprop%area = 0.0
    END IF

  END SUBROUTINE point_transport_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE point_depth_check
  !
  ! Should be called *after* hydrodynamics, but before
  ! point_section_update
  ! ----------------------------------------------------------------
  SUBROUTINE point_depth_check(this)
    IMPLICIT NONE
    CLASS (point_t), INTENT(INOUT) :: this
    DOUBLE PRECISION :: depth

    depth = this%hnow%y - this%thalweg
    IF (depth .LT. depth_minimum) THEN
       this%hnow%y = this%thalweg + depth_minimum
       ! q = 0.0
    END IF
  END SUBROUTINE point_depth_check



END MODULE point_module
