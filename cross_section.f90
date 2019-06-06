! ----------------------------------------------------------------
! file: cross_section.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  3, 2017 by William A. Perkins
! Last Change: 2019-06-06 09:53:46 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE cross_section
! ----------------------------------------------------------------
MODULE cross_section

  USE utility

  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! TYPE xsection_prop
  ! ----------------------------------------------------------------
  TYPE, PUBLIC ::  xsection_prop
     DOUBLE PRECISION :: depth
     DOUBLE PRECISION :: area
     DOUBLE PRECISION :: topwidth
     DOUBLE PRECISION :: hydrad
     DOUBLE PRECISION :: wetperim
     DOUBLE PRECISION :: conveyance
     DOUBLE PRECISION :: dkdy
  END type xsection_prop

  ! ----------------------------------------------------------------
  ! TYPE xsection_t
  ! ----------------------------------------------------------------
  TYPE, ABSTRACT, PUBLIC :: xsection_t
     INTEGER :: ID
   CONTAINS
     PROCEDURE (read_proc), DEFERRED :: read
     PROCEDURE :: area => xsection_area
     PROCEDURE :: invarea => xsection_inverse_area
     PROCEDURE (props_proc), DEFERRED :: props
     PROCEDURE (print_proc), DEFERRED :: print
     PROCEDURE (destroy_proc), DEFERRED :: destroy
     PROCEDURE :: normal_depth_iter => xsection_normal_iterate
     PROCEDURE :: normal_depth => xsection_normal_depth
  END TYPE xsection_t

  ABSTRACT INTERFACE
     SUBROUTINE read_proc(this, iounit, ioerr)
       IMPORT :: xsection_t
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(INOUT) :: this
       INTEGER, INTENT(IN) :: iounit
       INTEGER, INTENT(OUT) :: ioerr
     END SUBROUTINE read_proc

     SUBROUTINE props_proc(this, depth, props)
       IMPORT :: xsection_t, xsection_prop
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(IN) :: this
       DOUBLE PRECISION, INTENT(IN) :: depth 
       TYPE (xsection_prop), INTENT(OUT) :: props
     END SUBROUTINE props_proc

     SUBROUTINE print_proc(this, iounit, ioerr)
       IMPORT :: xsection_t
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(IN) :: this
       INTEGER, INTENT(IN) :: iounit
       INTEGER, INTENT(OUT) :: ioerr
     END SUBROUTINE print_proc

     SUBROUTINE destroy_proc(this)
       IMPORT :: xsection_t
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(INOUT) :: this
     END SUBROUTINE destroy_proc
  END INTERFACE

  ! ----------------------------------------------------------------
  ! TYPE xsection_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: xsection_ptr
     CLASS (xsection_t), POINTER :: p
  END type xsection_ptr

  ! ----------------------------------------------------------------
  ! TYPE prismatic_section
  ! Provides null methods not use by prismatic sections (print, destroy)
  ! ----------------------------------------------------------------
  TYPE, PRIVATE, EXTENDS(xsection_t) :: prismatic_section_t
   CONTAINS
     PROCEDURE :: read => prismatic_read
     PROCEDURE :: props => prismatic_props
     PROCEDURE :: print => prismatic_print
     PROCEDURE :: destroy => prismatic_destroy
  END type prismatic_section_t

  ! ----------------------------------------------------------------
  ! TYPE rectangular_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(prismatic_section_t) :: rectangular_section
     DOUBLE PRECISION :: bottom_width
   CONTAINS
     PROCEDURE :: read => rectangular_read
     PROCEDURE :: props => rectangular_props
     PROCEDURE :: normal_depth_iter => rectangular_normal_iterate
  END type rectangular_section

  ! ----------------------------------------------------------------
  ! TYPE trapezoidal_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(rectangular_section) :: trapezoidal_section
     DOUBLE PRECISION :: sidez
   CONTAINS
     PROCEDURE :: read => trapezoidal_read
     PROCEDURE :: props => trapezoidal_props
     PROCEDURE :: normal_depth_iter => trapezoidal_normal_iterate
  END type trapezoidal_section

  ! ----------------------------------------------------------------
  ! TYPE triangular_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(prismatic_section_t) :: triangular_section
     DOUBLE PRECISION :: sidez
   CONTAINS
     PROCEDURE :: read => triangular_read
     PROCEDURE :: props => triangular_props
     PROCEDURE :: normal_depth_iter => triangular_normal_iterate
  END type triangular_section

  ! ----------------------------------------------------------------
  ! TYPE triangular_rounded_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(triangular_section) :: triangular_rounded_section
     DOUBLE PRECISION :: bottom_radius
   CONTAINS
     PROCEDURE :: read => triangular_rounded_read
     PROCEDURE :: props => triangular_rounded_props
     PROCEDURE :: normal_depth_iter => triangular_rounded_normal_iterate
  END type triangular_rounded_section

  ! ----------------------------------------------------------------
  ! TYPE parabolic_section
  !
  ! The bottom elevation, y = k*x^2, where x is the distance from the
  ! cross section center, where y = 0.  
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(prismatic_section_t) :: parabolic_section
     DOUBLE PRECISION :: k
   CONTAINS
     PROCEDURE :: read => parabolic_read
     PROCEDURE :: props => parabolic_props
  END type parabolic_section

  ! ----------------------------------------------------------------
  ! TYPE compound_section
  ! ----------------------------------------------------------------
  TYPE, PRIVATE, EXTENDS(prismatic_section_t) :: compound_section_t
     INTEGER :: nsect
     TYPE (xsection_ptr), ALLOCATABLE :: sect(:)
     DOUBLE PRECISION, ALLOCATABLE :: selev(:)
  CONTAINS  
     PROCEDURE :: props => compound_props
     PROCEDURE :: destroy => compound_destroy
  END type compound_section_t

  ! ----------------------------------------------------------------
  ! TYPE rectangular_flood_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(compound_section_t) :: rectangular_flood_section
   CONTAINS
     PROCEDURE :: read => rectangular_flood_read
  END type rectangular_flood_section

  ! ----------------------------------------------------------------
  ! TYPE general_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(xsection_t) :: general_section
     INTEGER :: npoint          ! number of original distarce/elevation pairs
     DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: x_orig, y_orig

     INTEGER :: nlevel          ! number of section prop levels
     DOUBLE PRECISION :: delta_y
     LOGICAL :: nonmono
     TYPE(xsection_prop), ALLOCATABLE, DIMENSION(:) :: prop

     INTEGER :: nwidth          ! number of unique depth/width pairs
     DOUBLE PRECISION :: ymin, ymax
     DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: ylevel, width
   CONTAINS
     PROCEDURE, PRIVATE :: general_build
     GENERIC :: build => general_build
     PROCEDURE, PRIVATE :: general_interp
     GENERIC :: interp => general_interp
     PROCEDURE :: read => general_read
     PROCEDURE :: props => general_props
     PROCEDURE :: print => general_print
     PROCEDURE :: destroy => general_destroy
  END type general_section

CONTAINS

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION xsection_area
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION xsection_area(this, depth)
    IMPLICIT NONE
    CLASS(xsection_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop) :: props
    
    ! slow, but correct
    CALL this%props(depth, props);
    xsection_area = props%area
  END FUNCTION xsection_area

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION xsection_inverse_area
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION xsection_inverse_area(this, area)
    IMPLICIT NONE
    CLASS(xsection_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: area
    DOUBLE PRECISION, PARAMETER :: scale = 2.0, eps = 1.0E-03
    DOUBLE PRECISION :: a0, y, ymin, ymax
    LOGICAL :: done

    xsection_inverse_area = 0.0
    IF (area .LE. 0.0) RETURN

    done = .FALSE.

    y = SQRT(area)
    ymin = y
    ymax = y
    DO WHILE (.NOT. done) 
       a0 = this%area(y)
       IF (ABS(a0 - area) .LE. eps) THEN
          xsection_inverse_area = y
          done = .TRUE.
       ELSE IF (a0 .GT. area) THEN
          ymax = y
          IF (ymin .LT. y) THEN
             y = (ymin + y)/2.0
          ELSE 
             y = y/2.0
             ymin = y
          END IF
       ELSE
          ymin = y
          IF (ymax .GT. y) THEN
             y = (ymax + y)/2.0
          ELSE 
             y = y*2.0
             ymax = y
          END IF
       END IF
    END DO

    xsection_inverse_area = y

  END FUNCTION xsection_inverse_area


  ! ----------------------------------------------------------------
  ! SUBROUTINE prismatic_read
  ! ----------------------------------------------------------------
  SUBROUTINE prismatic_read(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(prismatic_section_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    
    ioerr = 0
    CALL error_message("prismatic_read: this should not happen", fatal=.TRUE.)

  END SUBROUTINE prismatic_read


  ! ----------------------------------------------------------------
  ! SUBROUTINE prismatic_props
  ! ----------------------------------------------------------------
  SUBROUTINE prismatic_props(this, depth, props)
    IMPLICIT NONE
    CLASS(prismatic_section_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(OUT) :: props

    props%depth = depth
    props%area = this%area(depth)

    CALL error_message("prismatic_props: this should not happen", fatal=.TRUE.)

  END SUBROUTINE prismatic_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE prismatic_print
  ! ----------------------------------------------------------------
  SUBROUTINE prismatic_print(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(prismatic_section_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    ioerr = iounit
    ioerr = 0
    ! do nothing
    RETURN
  END SUBROUTINE prismatic_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE prismatic_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE prismatic_destroy(this)
    IMPLICIT NONE
    CLASS(prismatic_section_t), INTENT(INOUT) :: this

    ! do nothing

  END SUBROUTINE prismatic_destroy

  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_read
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_read(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(rectangular_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    
    READ(iounit,*,IOSTAT=ioerr) this%bottom_width
  END SUBROUTINE rectangular_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_props
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_props(this, depth, props)
    IMPLICIT NONE
    CLASS(rectangular_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(OUT) :: props

    props%depth = depth
    props%wetperim = this%bottom_width + 2.0*depth
    props%area = depth*this%bottom_width
    props%topwidth = this%bottom_width
    IF (props%area .GT. 0.0) THEN
       props%hydrad = props%area/props%wetperim
       ! props%conveyance = (props%area**(5./3.))/(props%wetperim**(2./3.))
       props%conveyance = (props%area**5/props%wetperim**2)**(1.0/3.0)
       props%dkdy =  props%conveyance*(5.0*this%bottom_width/props%area - 4.0/props%wetperim)/3.0
    ELSE 
       props%hydrad = 0.0
       props%conveyance = 0.0
       props%dkdy = 0.0
    END IF
  END SUBROUTINE rectangular_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE trapezoidal_read
  ! ----------------------------------------------------------------
  SUBROUTINE trapezoidal_read(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS (trapezoidal_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    ioerr = 0

    READ(iounit,*,IOSTAT=ioerr) this%bottom_width, this%sidez
    
  END SUBROUTINE trapezoidal_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE trapezoidal_props
  !
  ! Taken from Table 2-1 in Chow
  ! ----------------------------------------------------------------
  SUBROUTINE trapezoidal_props(this, depth, props)
    IMPLICIT NONE
    CLASS (trapezoidal_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth 
    TYPE (xsection_prop), INTENT(OUT) :: props

    ! to make thing a little more readable
    DOUBLE PRECISION :: b, z, dAdy, dPdy
    b = this%bottom_width
    z = this%sidez
    
    props%depth = depth
    props%area = (b + z*depth)*depth
    props%wetperim = b + 2.0*depth*SQRT(1 + z*z)
    props%hydrad = props%area/props%wetperim
    props%topwidth = b + 2.0*z*depth
    props%conveyance = props%area*props%hydrad**(2.0/3.0)
    ! or conveyance = area**(5.0/3.0)/wetperim
    dAdy = b + 2*z*depth
    dPdy = 2*SQRT(1 + z*z)
    props%dkdy = (5.0/3.0)*dAdy*props%hydrad**(2.0/3.0) - &
         &(2.0/3.0)*props%hydrad**(5.0/3.0)*dPdy

  END SUBROUTINE trapezoidal_props


  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_flood_read
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_flood_read(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(rectangular_flood_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    DOUBLE PRECISION :: depth_main, bottom_width, bottom_width_flood
    CLASS (rectangular_section), POINTER :: s1, s2
    
    
    READ(iounit,*,IOSTAT=ioerr) &
         &depth_main, bottom_width, bottom_width_flood

    this%nsect = 2
    ALLOCATE(this%sect(this%nsect), this%selev(this%nsect))
    this%selev(1) = 0
    this%selev(2) = depth_main

    ALLOCATE(rectangular_section :: s1)
    s1%bottom_width = bottom_width
    this%sect(1)%p => s1
    ALLOCATE(rectangular_section :: s2)
    s2%bottom_width = bottom_width_flood
    this%sect(2)%p => s2
    
  END SUBROUTINE rectangular_flood_read


  ! ----------------------------------------------------------------
  ! SUBROUTINE triangular_read
  ! ----------------------------------------------------------------
  SUBROUTINE triangular_read(this, iounit, ioerr)

    IMPLICIT NONE
    CLASS (triangular_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr

    READ(iounit,*,IOSTAT=ioerr) this%sidez

  END SUBROUTINE triangular_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE triangular_props
  ! ----------------------------------------------------------------
  SUBROUTINE triangular_props(this, depth, props)
    IMPLICIT NONE
    CLASS(triangular_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(OUT) :: props
    DOUBLE PRECISION :: z, dAdy, dPdy

    z = this%sidez

    props%depth = depth
    props%wetperim = 2.0*depth*SQRT(1 + z*z)
    props%area = z*depth*depth
    props%topwidth = 0.5*z*depth
    IF (props%area .GT. 0.0) THEN
       props%hydrad = props%area/props%wetperim
       props%conveyance = props%area*props%hydrad**(2.0/3.0)
       ! or conveyance = area**(5.0/3.0)/wetperim
       dAdy = 2.0*z*depth
       dPdy = 2.0*SQRT(1 + z*z)
       props%dkdy = (5.0/3.0)*dAdy*props%hydrad**(2.0/3.0) - &
            &(2.0/3.0)*props%hydrad**(5.0/3.0)*dPdy
    ELSE 
       props%hydrad = 0.0
       props%conveyance = 0.0
       props%dkdy = 0.0
    END IF
  END SUBROUTINE triangular_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE triangular_rounded_read
  ! ----------------------------------------------------------------
  SUBROUTINE triangular_rounded_read(this, iounit, ioerr)

    IMPLICIT NONE
    CLASS (triangular_rounded_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr

    READ(iounit,*,IOSTAT=ioerr) this%sidez, this%bottom_radius

  END SUBROUTINE triangular_rounded_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE triangular_rounded_props
  ! ----------------------------------------------------------------
  SUBROUTINE triangular_rounded_props(this, depth, props)
    IMPLICIT NONE
    CLASS(triangular_rounded_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(OUT) :: props
    DOUBLE PRECISION :: z, r, y, T, dAdy, dPdy, pi, acotz

    y = depth
    z = this%sidez
    r = this%bottom_radius
    pi=4.D0*DATAN(1.D0)
    acotz = pi/2.0 - DATAN(z)

    props%depth = y
    IF (y .GT. 0.0) THEN
       T = 2.0*(z*(y-r)+r*SQRT(1.0+z*z))
       props%area = T*T/4.0/z - r*r/z*(1.0 - z*acotz)
       props%wetperim = T/z*SQRT(1 + z*z) - 2.0*r/z*(1 - z*acotz)
       props%topwidth = T
       props%hydrad = props%area/props%wetperim
       props%conveyance = props%area*props%hydrad**(2.0/3.0)
       ! or conveyance = area**(5.0/3.0)/wetperim
       dAdy = 2.0*z*depth
       dPdy = 2.0*SQRT(1 + z*z)
       props%dkdy = (5.0/3.0)*dAdy*props%hydrad**(2.0/3.0) - &
            &(2.0/3.0)*props%hydrad**(5.0/3.0)*dPdy
    ELSE 
       props%hydrad = 0.0
       props%conveyance = 0.0
       props%dkdy = 0.0
    END IF
  END SUBROUTINE triangular_rounded_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE parabolic_read
  ! ----------------------------------------------------------------
  SUBROUTINE parabolic_read(this, iounit, ioerr)

    IMPLICIT NONE
    CLASS (parabolic_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr

    READ(iounit,*,IOSTAT=ioerr) this%k

  END SUBROUTINE parabolic_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE parabolic_props
  ! ----------------------------------------------------------------
  SUBROUTINE parabolic_props(this, depth, props)
    IMPLICIT NONE
    CLASS(parabolic_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(OUT) :: props
    DOUBLE PRECISION :: y, z, T, dTdy, dAdy, dPdy

    y = depth


    IF (y .GT. 0.0) THEN
       props%depth = depth
       T = 2*SQRT(y/this%k)
       z = 4.0*y/T
       dTdY = SQRT(this%k/y)
       props%topwidth = T
       ! approximate: props%wetperim = T + 8.0/3.0*y*y/T
       props%wetperim = 0.5*T*(SQRT(1 + z*z) + 1/z*LOG(z + SQRT(1 + z*Z)))
       props%area = 2.0/3.0*T*y
       props%hydrad = props%area/props%wetperim
       props%conveyance = props%area*props%hydrad**(2.0/3.0)
       ! or conveyance = area**(5.0/3.0)/wetperim
       dAdy = 2.0/3.0*(dTdy*y + T)
       ! approximate: dPdy = dTdy + 16.0/3.0*y/T - 8.0/3.0*y*y/T/T*dTdy
       dPdy = 0.5*(y/this%k + 4*y*y)**(-0.5)*(1.0/this%k + 8.0*y) + &
            &0.5/this%k*(SQRT(this%k/y) + 2.0*this%k/SQRT(1.0 + 4.0*y*this%k))/&
            &(2.0*SQRT(y*this%k) + SQRT(1.0 + 4.0*y*this%k))
       props%dkdy = (5.0/3.0)*dAdy*props%hydrad**(2.0/3.0) - &
            &(2.0/3.0)*props%hydrad**(5.0/3.0)*dPdy
       ! IF (4.0*y/T .GT. 1.0) THEN
       !    WRITE (msg, *) 'WARNING: parabolic section ', this%id, &
       !         &': conditions for wetted perimeter approximation exceeded'
       !    CALL status_message(msg)
       ! END IF
    ELSE 
       props%hydrad = 0.0
       props%conveyance = 0.0
       props%dkdy = 0.0
    END IF
  END SUBROUTINE parabolic_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE compound_props
  ! ----------------------------------------------------------------
  SUBROUTINE compound_props(this, depth, props)
    IMPLICIT NONE
    CLASS(compound_section_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(OUT) :: props
    DOUBLE PRECISION :: y, d
    INTEGER :: i
    TYPE (xsection_prop) :: p0

    props%depth = depth
    props%area = 0.0
    props%conveyance = 0.0
    props%wetperim = 0.0
    props%topwidth = 0.0

    y = depth

    DO i = 1, this%nsect
       IF (y .LE. this%selev(i)) EXIT
       d = y - this%selev(i)
       IF (i .LT. this%nsect) THEN
          IF (y .GT. this%selev(i+1)) THEN
             d = this%selev(i+1) - this%selev(i)
          END IF
       END IF
       CALL this%sect(i)%p%props(d, p0)
       props%wetperim = props%wetperim + p0%wetperim - props%topwidth
       props%area = props%area + p0%area
       props%topwidth = p0%topwidth
       props%conveyance = props%conveyance + p0%conveyance
       props%dkdy = p0%dkdy
       y = y - this%selev(i)
    END DO

    IF (props%wetperim .GT. 0.0) THEN
       props%hydrad = props%area/props%wetperim
    END IF

  END SUBROUTINE compound_props

    ! ----------------------------------------------------------------
  ! SUBROUTINE compound_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE compound_destroy(this)
    IMPLICIT NONE
    CLASS(compound_section_t), INTENT(INOUT) :: this
    INTEGER :: istatus 
    CHARACTER (LEN=1024) :: msg

    DEALLOCATE(this%sect, this%selev, STAT=istatus)
    IF (istatus .NE. 0) THEN
       WRITE(msg, *) 'compound_destroy: error deallocating compound section' , &
            &this%id 
       CALL error_message(msg)
    END IF

  END SUBROUTINE compound_destroy




  ! ----------------------------------------------------------------
  ! SUBROUTINE general_build
  ! ----------------------------------------------------------------
  SUBROUTINE general_build(this, delta_y, num_pairs, xy)
    IMPLICIT NONE
    CLASS(general_section), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: delta_y
    INTEGER, INTENT(IN) :: num_pairs
    DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: xy(1:2*num_pairs)

    DOUBLE PRECISION :: linear_interp !external

    INTEGER :: i, j, count
    INTEGER :: iperm(num_pairs)
    DOUBLE PRECISION :: x(num_pairs), y(num_pairs)
    DOUBLE PRECISION :: y_new(num_pairs)
    DOUBLE PRECISION :: width(num_pairs), width_new(num_pairs)
    DOUBLE PRECISION :: level, x_inter, max_elv, min_elv
    LOGICAL :: last_point_wet

    this%delta_y = delta_y
    this%nonmono = .FALSE.

    ! divide xy vector into x,y pairs
    count = 0
    DO i=1,2*num_pairs-1,2
       count = count + 1
       x(count) = xy(i)
       y(count) = xy(i+1)
    END DO

    ! save to print elsewhere, maybe
    this%npoint = count
    ALLOCATE(this%x_orig(count), this%y_orig(count))
    this%x_orig(1:count) = x(1:count)
    this%y_orig(1:count) = y(1:count)
    this%delta_y = delta_y

    ! transform x,y pairs to level-width pairs, can jump over this
    ! if the input data were specified in level-width format

    DO i=1,num_pairs  ! pick the level to work on

       level = y(i)
       width(i) = 0.0

       ! wet if point is at or below level
       ! dry if point is above level

       last_point_wet = .FALSE.

       j = 1

       IF(y(j) <= level) last_point_wet = .TRUE.

       DO j = 2,num_pairs       ! loop through rest of levels and add up width

          IF(last_point_wet)THEN
             IF(y(j) <= level)THEN
                width(i) = width(i) + ABS(x(j) - x(j-1))
                last_point_wet = .TRUE.
             ELSE
                x_inter = linear_interp(x(j-1),y(j-1),x(j),y(j),level)
                width(i) = width(i) + ABS(x(j-1) - x_inter)
                last_point_wet = .FALSE.
             ENDIF

          ELSE  ! last point was dry
             IF(y(j) <= level)THEN
                x_inter = linear_interp(x(j),y(j),x(j-1),y(j-1),level)
                width(i) = width(i) + ABS(x(j) - x_inter)
                last_point_wet = .TRUE.
             ELSE
                width(i) = width(i)
                last_point_wet = .FALSE.
             ENDIF

          ENDIF

       END DO

    END DO

    ! first sort from bottom to top and then eliminate duplicate levels
    iperm = (/(i,i=1,num_pairs)/) ! must initialize for IMSL to use

    CALL SVRGP(num_pairs,y,y_new,iperm) ! IMSL routine to sort

    width_new = width(iperm)

    ! cull out non-unique values; vector MUST be sorted first or this method will fail
    count = 2
    DO i=2,num_pairs
       IF(y_new(i) /= y_new(i-1))THEN
          y_new(count) = y_new(i)
          width_new(count) = width_new(i)
          count = count + 1
       ENDIF
    END DO
    count = count - 1  ! reset to the new number of unique levels

    ! save for printing later, maybe
    this%nwidth = count
    ALLOCATE(this%ylevel(count), this%width(count))
    this%ylevel(1:count) = y_new(1:count)
    this%width(1:count) = width_new(1:count)

    ! figure out number of levels needed to cover elevation range at
    ! the specified delta_y interval for this section
    max_elv = MAXVAL(y_new)
    min_elv = MINVAL(y_new)
    this%ymin = min_elv
    this%ymax = max_elv

    this%nlevel = AINT((max_elv - min_elv)/this%delta_y) + 1
    ALLOCATE(this%prop(this%nlevel))

    ! compute depth, width, area, hydraulic radius, geo-conveyance
    ! at each depth level
    
    i = 1
    this%prop(i)%depth = 0.0
    this%prop(i)%area = 0.0
    this%prop(i)%topwidth = 0.0
    this%prop(i)%hydrad = 0.0
    this%prop(i)%wetperim = 0.0
    this%prop(i)%conveyance = 0.0
    this%prop(i)%dkdy = 0.0
    
    DO i = 2, this%nlevel
       this%prop(i)%depth = this%prop(i-1)%depth + this%delta_y
       level = min_elv + this%prop(i)%depth 

       ! figure out where the level is in the level-width table
       DO j = 1,count-1
          IF((level >= y_new(j)) .AND. (level <= y_new(j+1))) EXIT
       END DO
       this%prop(i)%topwidth = &
            &linear_interp(width_new(j),y_new(j),width_new(j+1),y_new(j+1),level)

       this%prop(i)%area = &
            &this%delta_y*0.5*(this%prop(i)%topwidth + this%prop(i-1)%topwidth) + &
            &this%prop(i-1)%area
       this%prop(i)%wetperim = &
            &this%prop(i-1)%wetperim + &
            &SQRT(4*this%delta_y**2 + (this%prop(i)%topwidth - this%prop(i-1)%topwidth)**2)
       this%prop(i)%hydrad = this%prop(i)%area/this%prop(i)%wetperim
       this%prop(i)%conveyance = &
            &this%prop(i)%area*this%prop(i)%hydrad**(2.0/3.0)
    END DO

    ! adjust the geo-conveyance to be sure that it
    ! monotonically increases with depth
    ! linear extrapolation from previous 2 conveyance points

    DO i = 2, this%nlevel
       IF (this%prop(i)%conveyance .LT. this%prop(i-1)%conveyance) THEN
          this%nonmono = .true.
          ! WRITE (*,*) 'Section ', this%id, ", level", i, ": conveyance at ", &
          !      &this%prop(i)%depth, " = ", this%prop(i)%conveyance, " < ", &
          !      &this%prop(i-1)%conveyance
          level = this%prop(i)%depth
          IF(i == 2)THEN
             this%prop(i)%conveyance = 0.5*&
                  &(this%prop(i-1)%conveyance + this%prop(i+1)%conveyance)
          ELSE
             this%prop(i)%conveyance = &
                  &linear_interp(this%prop(i-2)%conveyance, this%prop(i-2)%depth, &
                  &this%prop(i-1)%conveyance, this%prop(i-1)%depth, level)
          ENDIF
       END IF
    END DO
  END SUBROUTINE general_build


  ! ----------------------------------------------------------------
  ! SUBROUTINE general_interp
  ! ----------------------------------------------------------------
  SUBROUTINE general_interp(this, depth, p)
    IMPLICIT NONE
    CLASS(general_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    TYPE (xsection_prop), INTENT(INOUT) :: p

    INTEGER :: j
    DOUBLE PRECISION :: factor

    j = AINT(depth/this%delta_y) + 1

    IF(j < 1) j=1
    IF(j >= this%nlevel) j = this%nlevel - 1

    factor = (depth - this%delta_y*(j-1))/this%delta_y

    p%depth = depth
    p%area = factor*(this%prop(j+1)%area - this%prop(j)%area) + this%prop(j)%area
    p%topwidth = factor*(this%prop(j+1)%topwidth - this%prop(j)%topwidth) + this%prop(j)%topwidth
    p%wetperim = factor*(this%prop(j+1)%wetperim - this%prop(j)%wetperim) + this%prop(j)%wetperim
    ! p%hydrad = factor*(this%prop(j+1)%hydrad - this%prop(j)%hydrad) + this%prop(j)%hydrad
    IF (p%wetperim .GT. 0.0) THEN 
       p%hydrad = p%area/p%wetperim
    ELSE 
       p%hydrad = 0.0
    END IF
    p%conveyance = factor*(this%prop(j+1)%conveyance - this%prop(j)%conveyance) + this%prop(j)%conveyance
    p%dkdy = (this%prop(j+1)%conveyance - this%prop(j)%conveyance)/this%delta_y
  END SUBROUTINE general_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE general_read
  ! ----------------------------------------------------------------
  SUBROUTINE general_read(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(general_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    DOUBLE PRECISION :: tmp_delta_y
    INTEGER :: num_pairs
    DOUBLE PRECISION, ALLOCATABLE :: xy(:)

    READ(iounit,*,IOSTAT=ioerr) tmp_delta_y, num_pairs
    ALLOCATE(xy(2*(num_pairs+1))) !allocate too many to make sure all are read
    xy = -9999.0
    READ(iounit,*,IOSTAT=ioerr) xy(1:2*(num_pairs+1))
    
    CALL this%build(tmp_delta_y, num_pairs, xy)
    DEALLOCATE(xy)

  END SUBROUTINE general_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE general_props
  ! ----------------------------------------------------------------
  SUBROUTINE general_props(this, depth, props)
    IMPLICIT NONE
    CLASS(general_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth 
    TYPE (xsection_prop), INTENT(OUT) :: props

    CALL this%interp(depth, props)

  END SUBROUTINE general_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE general_print
  ! ----------------------------------------------------------------
  SUBROUTINE general_print(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(general_section), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    INTEGER :: i

    WRITE(iounit, *, IOSTAT=ioerr)'/////////////////////////////////////////////////////////////////////////////'
    WRITE(iounit, *, IOSTAT=ioerr)''

    WRITE(iounit, *, IOSTAT=ioerr)'section id number - ', this%id
    WRITE(iounit, *, IOSTAT=ioerr)'number of pairs - ', this%npoint
    WRITE(iounit, *, IOSTAT=ioerr)'maximum elevation - ', this%ymax
    WRITE(iounit, *, IOSTAT=ioerr)'minimum elevation - ', this%ymin
    WRITE(iounit, *, IOSTAT=ioerr)'depth interval to build table - ', this%delta_y
    WRITE(iounit, *, IOSTAT=ioerr)'number of levels - ', this%nlevel
    IF (this%nonmono) THEN 
       WRITE(iounit, *, IOSTAT=ioerr)'!!!! geo-conveyance adjusted to be monotonically increasing'
    END IF
    WRITE(iounit, *, IOSTAT=ioerr)''
    WRITE(iounit, *, IOSTAT=ioerr)'*** distance-level pairs specified ***'
    DO i= 1, this%npoint
       WRITE(iounit, 1000, IOSTAT=ioerr) this%x_orig(i),this%y_orig(i)
    END DO

    WRITE(iounit, *, IOSTAT=ioerr)''
    WRITE(iounit, *, IOSTAT=ioerr)'*** level-width pairs computed ***'
    DO i=1,this%nwidth
       WRITE(iounit,1000) this%ylevel(i), this%width(i)
    END DO

    WRITE(iounit, *, IOSTAT=ioerr)''
    WRITE(iounit, *, IOSTAT=ioerr)'------------------------------------------------------'
    WRITE(iounit, *, IOSTAT=ioerr)'           Computed Geometric Properties'
    WRITE(iounit,1015)
    DO i= 1, this%nlevel
       WRITE(iounit, 1010, IOSTAT=ioerr) i, &
            &this%prop(i)%depth, this%prop(i)%topwidth, this%prop(i)%area, &
            &this%prop(i)%wetperim, this%prop(i)%hydrad, this%prop(i)%conveyance
    END DO
    WRITE(iounit, *, IOSTAT=ioerr)''

1000 FORMAT(2x,2(f10.2,2x),5x,2(f10.2))
1010 FORMAT(2x,i4,6(f12.3,2x))
1015 FORMAT(2x,'Level',6x,'Depth',6x,'Width',9x,'Area',8x,'Wetted-Perim',4x,'Hydr Radius',2x,'Geo-Conveyance')

  END SUBROUTINE general_print

  ! ----------------------------------------------------------------
  ! SUBROUTINE general_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE general_destroy(this)

    IMPLICIT NONE
    CLASS(general_section), INTENT(INOUT) :: this
    
    DEALLOCATE(this%x_orig, this%y_orig)
    DEALLOCATE(this%prop)
    DEALLOCATE(this%ylevel, this%width)
  END SUBROUTINE general_destroy



  ! ----------------------------------------------------------------
  ! FUNCTION read_cross_section
  ! ----------------------------------------------------------------
  FUNCTION read_cross_section(iounit, ierr) RESULT(xsect)
    IMPLICIT NONE
    CLASS(xsection_t), POINTER :: xsect
    INTEGER, INTENT(in) :: iounit
    INTEGER, INTENT(out) :: ierr

    INTEGER :: id
    INTEGER :: section_type
    INTEGER :: ioerr
    CHARACTER(LEN=80) :: msg

    ierr = 0
    id = 0
    NULLIFY(xsect)

    
    
    READ(iounit, *, END=100, ERR=200) id, section_type

    SELECT CASE (section_type)
    CASE (1)
       ALLOCATE(rectangular_section :: xsect)
    CASE (2)
       ALLOCATE(rectangular_flood_section :: xsect)
    CASE (3)
       ALLOCATE(trapezoidal_section :: xsect)
    CASE (4)
       ALLOCATE(triangular_section :: xsect)
    CASE (6)
       ALLOCATE(parabolic_section :: xsect)
    CASE (14)
       ALLOCATE(triangular_rounded_section :: xsect)
    CASE (50)
       ALLOCATE(general_section :: xsect)
    CASE DEFAULT
       WRITE(msg, *) 'Cross section ', id, ': unknown type: ', section_type
       CALL error_message(msg)
    END SELECT

    xsect%id = id
    CALL xsect%read(iounit, ioerr)

    IF (ioerr .NE. 0) THEN
       WRITE (msg, *) 'Cross section ', id, ', type ', section_type, ": error reading"
       CALL error_message(msg)
       CALL xsect%destroy()
       DEALLOCATE(xsect)
       NULLIFY(xsect)
       ierr = ierr + 1
       RETURN
    END IF

    RETURN
100 CONTINUE
    RETURN
200 CONTINUE
    WRITE(msg, *) 'Unknown error reading cross section, last ID = ', id
    CALL error_message(msg)
    ierr = ierr + 1
    RETURN
  END FUNCTION read_cross_section

  ! ----------------------------------------------------------------
  ! SUBROUTINE cross_section_table
  ! ----------------------------------------------------------------
  SUBROUTINE cross_section_table(iounit, xsect, ymax, dy)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: iounit
    CLASS(xsection_t), INTENT(IN) :: xsect
    DOUBLE PRECISION, INTENT(IN) :: ymax, dy
    
    DOUBLE PRECISION :: y
    INTEGER :: i, steps
    TYPE (xsection_prop) :: props
    

    WRITE(iounit, *)
    WRITE(iounit, *) 'Section Number ', xsect%id
    WRITE(iounit, 10)
    WRITE(iounit, 12) 'y', 'Width', 'Area', 'W Perim', 'H Radius', 'Convey', 'dkdy'
    WRITE(iounit, 10)

    steps = AINT(ymax/dy + 0.5) + 1
    y = 0
    DO i = 0, steps
       y = REAL(i)*dy
       CALL xsect%props(y, props)
       WRITE(iounit, 15) y, props%topwidth, props%area, props%wetperim, &
            &props%hydrad, props%conveyance, props%dkdy
    END DO
    WRITE(iounit, 10)
    WRITE(iounit, *)
  
10 FORMAT(7('-------------'))
12 FORMAT(7(1X, A12))
15 FORMAT(7(1X, F12.2))

    

  END SUBROUTINE cross_section_table

  ! ----------------------------------------------------------------
  !  FUNCTION xsection_normal_iterate
  ! ----------------------------------------------------------------
  FUNCTION xsection_normal_iterate(this, &
       &discharge, slope, kstrick, dguess) RESULT (f)

    IMPLICIT NONE

    DOUBLE PRECISION :: f
    CLASS(xsection_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: discharge, slope, kstrick, dguess
    DOUBLE PRECISION, PARAMETER :: alpha = 5.0/3.0, beta = 2.0/3.0

    TYPE (xsection_prop) :: props
    DOUBLE PRECISION :: K

    K = SQRT(slope)*kstrick
    K = (discharge/K)**(1.0/alpha)
    CALL this%props(dguess, props)
    f = K*(props%wetperim**(beta/alpha))
    f = this%invarea(f)

  END FUNCTION xsection_normal_iterate


  ! ----------------------------------------------------------------
  !  FUNCTION xsection_normal_depth
  !
  ! This is an implementation of the algorithm to compute normal depth
  ! from
  !
  ! Shirley Edward D., and Lopes Vicente L. 1991. “Normal‐Depth
  ! Calculations in Complex Channel Sections.” Journal of Irrigation
  ! and Drainage Engineering 117 (2): 220–32. 
  ! https://doi.org/10.1061/(ASCE)0733-9437(1991)117:2(220).
  ! ----------------------------------------------------------------
  FUNCTION xsection_normal_depth(this, &
       &discharge, slope, kstrick, dguess) RESULT (dnorm)

    IMPLICIT NONE
    DOUBLE PRECISION :: dnorm
    CLASS(xsection_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: discharge, slope, kstrick, dguess
    DOUBLE PRECISION :: d, dprev
    LOGICAL :: done
    INTEGER :: iter

    DOUBLE PRECISION, PARAMETER :: lim = 1.0E-03
    INTEGER, PARAMETER :: maxiter = 10

    CHARACTER (LEN=256) :: msg

    IF (discharge .LE. 0.0) THEN
       dnorm = 0.0
       RETURN
    END IF

    IF (slope .LE. 0.0) THEN 
       CALL error_message("xsection_normal_depth: undefined for zero slope")
       dnorm = dguess
       RETURN
    END IF
    
    d = dguess
    done = .FALSE.
    iter = 0
    DO WHILE (.NOT. done) 
       dprev = d
       d = this%normal_depth_iter(discharge, slope, kstrick, d)
       done = ABS(d - dprev) .LT. lim
       iter = iter + 1
       IF (iter .GT. maxiter) THEN
          WRITE(msg, *) 'xsection_normal_depth: maximum iterations (', maxiter, &
               &') , exceeded at q = ', discharge, ", d = ", d
          CALL error_message(msg)
          done = .TRUE.
       END IF
    END DO
    dnorm = d
    RETURN
  END FUNCTION xsection_normal_depth

  ! ----------------------------------------------------------------
  !  FUNCTION triangular_normal_iterate
  ! ----------------------------------------------------------------
  FUNCTION triangular_normal_iterate(this, &
       &discharge, slope, kstrick, dguess) RESULT (dnorm)
    IMPLICIT NONE
    DOUBLE PRECISION :: dnorm
    CLASS(triangular_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: discharge, slope, kstrick, dguess
    
    DOUBLE PRECISION :: c1, c2

    c1 = this%sidez
    c2 = 2.0*SQRT(1 + c1*c1)

    dnorm = discharge/(kstrick)/c2/sqrt(slope)
    dnorm = dnorm**(3.0/5.0)
    dnorm = dnorm*c2/c1
    dnorm = dnorm**(5.0/8.0)
  END FUNCTION triangular_normal_iterate

  ! ----------------------------------------------------------------
  !  FUNCTION triangular_rounded_normal_iterate
  ! ----------------------------------------------------------------
  FUNCTION triangular_rounded_normal_iterate(this, &
       &discharge, slope, kstrick, dguess) RESULT (f)
    IMPLICIT NONE
    DOUBLE PRECISION :: f
    CLASS(triangular_rounded_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: discharge, slope, kstrick, dguess
    DOUBLE PRECISION, PARAMETER :: alpha = 5.0/3.0, beta = 2.0/3.0

    TYPE (xsection_prop) :: props
    DOUBLE PRECISION :: K

    ! copied from xsection_normal_iterate() because we can't do this:
    ! f = this%xsection_t%normal_depth_iter(discharge, slope, kstrick, dguess)
    
    K = SQRT(slope)*kstrick
    K = (discharge/K)**(1.0/alpha)
    CALL this%props(dguess, props)
    f = K*(props%wetperim**(beta/alpha))
    f = this%invarea(f)

  END FUNCTION triangular_rounded_normal_iterate



  ! ----------------------------------------------------------------
  ! FUNCTION rectangular_normal_iterate
  ! Using Equation 18a
  ! ----------------------------------------------------------------
  FUNCTION rectangular_normal_iterate(this, &
       &discharge, slope, kstrick, dguess) RESULT (dnorm)
    IMPLICIT NONE
    DOUBLE PRECISION :: dnorm
    CLASS(rectangular_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: discharge, slope, kstrick, dguess
    
    DOUBLE PRECISION :: b, a1, a2

    b = this%bottom_width
    a1 = b/2.0
    a2 = (discharge/sqrt(slope)/2/(kstrick))**(3.0/5.0) * (2.0/b)
    dnorm = a2*(a1 + dguess)**(2.0/5.0)
  
  END FUNCTION rectangular_normal_iterate


  ! ----------------------------------------------------------------
  ! FUNCTION trapezoidal_normal_iterate
  ! Using Equation 20a of Shirley and Lopes.
  ! ----------------------------------------------------------------
  FUNCTION trapezoidal_normal_iterate(this, &
       &discharge, slope, kstrick, dguess) RESULT (dnorm)

    IMPLICIT NONE

    DOUBLE PRECISION :: dnorm
    CLASS(trapezoidal_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: discharge, slope, kstrick, dguess
    
    DOUBLE PRECISION :: b, c1, c2, a1, a2, a3, a4, w

    c1 = this%sidez
    c2 = 2*sqrt(1+this%sidez*this%sidez)
    b = this%bottom_width

    a1 = b/2.0/c1
    a2 = a1*a1
    a3 = b/c2
    a4 = (discharge/(kstrick)/c1/sqrt(slope))**1.5
    a4 = a4*c2/c1
    w = ((a3 + dguess)*a4)**(0.4)

    dnorm = w/(a1 + sqrt(a2 + w))

  END FUNCTION trapezoidal_normal_iterate

END MODULE cross_section
