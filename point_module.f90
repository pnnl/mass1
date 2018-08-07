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
! Last Change: 2018-08-07 08:54:45 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE point_module
! ----------------------------------------------------------------
MODULE point_module

  USE mass1_config
  USE cross_section
  USE general_vars

  IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC :: point_hydro_state
     DOUBLE PRECISION :: y, q, v
     DOUBLE PRECISION :: lateral_inflow
     DOUBLE PRECISION :: froude_num
     DOUBLE PRECISION :: friction_slope, bed_shear
     DOUBLE PRECISION :: courant_num, diffuse_num
  END type point_hydro_state

  TYPE, PUBLIC :: point_sweep_coeff
     DOUBLE PRECISION :: e,f,l,m,n
  END type point_sweep_coeff

  TYPE, PUBLIC :: point_t
     DOUBLE PRECISION :: x, thalweg
     DOUBLE PRECISION :: manning, k_diff, kstrick
     TYPE (xsection_ptr) :: xsection
     TYPE (xsection_prop) :: xsprop
     TYPE (point_hydro_state) :: hnow, hold
     TYPE (point_sweep_coeff) :: sweep
   CONTAINS
     PROCEDURE :: section_update => point_section_update
     PROCEDURE :: depth_check => point_depth_check
     PROCEDURE :: hydro_update => point_hydro_update
     PROCEDURE :: assign => point_assign
  END type point_t

  ! ----------------------------------------------------------------
  ! TYPE point_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: point_ptr
     TYPE (point_t), POINTER :: p
  END type point_ptr

CONTAINS

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
  SUBROUTINE point_hydro_update(this, grav, deltat, deltax)
    USE general_vars, ONLY: depth_minimum

    IMPLICIT NONE
    CLASS (point_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: grav, deltat, deltax

    DOUBLE PRECISION :: depth

    this%hold = this%hnow

    ASSOCIATE (h => this%hnow, xs => this%xsprop)

      depth = h%y - this%thalweg
      IF (depth .LT. depth_minimum) THEN
         h%y = this%thalweg + depth_minimum
      END IF

      CALL this%section_update()

      IF (xs%area .GT. 0.0) THEN
         h%froude_num = &
              &SQRT((h%q*h%q*xs%topwidth)/(grav*xs%area**3))
         h%friction_slope = &
              &((h%q)/(this%kstrick*xs%area*(xs%hydrad**2)**0.3333333))**2.0
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
    END ASSOCIATE

  END SUBROUTINE point_hydro_update


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
