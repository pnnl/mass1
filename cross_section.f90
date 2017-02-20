! ----------------------------------------------------------------
! file: cross_section.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  3, 2017 by William A. Perkins
! Last Change: 2017-01-06 09:35:03 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE cross_section
! ----------------------------------------------------------------
MODULE cross_section

  USE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  ! ----------------------------------------------------------------
  ! TYPE xsection_t
  ! ----------------------------------------------------------------
  TYPE, ABSTRACT, PUBLIC :: xsection_t
     INTEGER :: ID
   CONTAINS
     PROCEDURE (read_proc), DEFERRED :: read
     PROCEDURE (props_proc), DEFERRED :: props
     PROCEDURE (print_proc), DEFERRED :: print
     PROCEDURE (destroy_proc), DEFERRED :: destroy
  END TYPE xsection_t

  ABSTRACT INTERFACE
     SUBROUTINE read_proc(this, iounit, ioerr)
       IMPORT :: xsection_t
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(INOUT) :: this
       INTEGER, INTENT(IN) :: iounit
       INTEGER, INTENT(OUT) :: ioerr
     END SUBROUTINE read_proc
     
     SUBROUTINE props_proc(this, depth, area, hydrad, topwidth, conveyance, dkdy)
       IMPORT :: xsection_t
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(IN) :: this
       DOUBLE PRECISION, INTENT(IN) :: depth 
       DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy
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
  ! TYPE rectangular_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(xsection_t) :: rectangular_section
     DOUBLE PRECISION :: bottom_width
   CONTAINS
     PROCEDURE :: read => rectangular_read
     PROCEDURE :: props => rectangular_props
     PROCEDURE :: print => rectangular_print
     PROCEDURE :: destroy => rectangular_destroy
  END type rectangular_section

  ! ----------------------------------------------------------------
  ! TYPE trapezoidal_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(rectangular_section) :: trapezoidal_section
     DOUBLE PRECISION :: sidez
   CONTAINS
     PROCEDURE :: read => trapezoidal_read
     PROCEDURE :: props => trapezoidal_props
  END type trapezoidal_section
  ! ----------------------------------------------------------------
  ! TYPE rectangular_flood_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(rectangular_section) :: rectangular_flood_section
     DOUBLE PRECISION :: depth_main
     DOUBLE PRECISION :: bottom_width_flood
   CONTAINS
     PROCEDURE :: read => rectangular_flood_read
     PROCEDURE :: props => rectangular_flood_props
  END type rectangular_flood_section

  ! ----------------------------------------------------------------
  ! TYPE general_section_prop
  ! ----------------------------------------------------------------
  TYPE, PRIVATE :: general_section_prop
     DOUBLE PRECISION :: depth
     DOUBLE PRECISION :: area
     DOUBLE PRECISION :: topwidth
     DOUBLE PRECISION :: hydradius
     DOUBLE PRECISION :: wetperim
     DOUBLE PRECISION :: conveyance
     DOUBLE PRECISION :: dkdy
  END type general_section_prop

  ! ----------------------------------------------------------------
  ! TYPE general_section
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(xsection_t) :: general_section
     INTEGER :: npoint          ! number of original distarce/elevation pairs
     DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: x_orig, y_orig

     INTEGER :: nlevel          ! number of section prop levels
     DOUBLE PRECISION :: delta_y
     LOGICAL :: nonmono
     TYPE(general_section_prop), ALLOCATABLE, DIMENSION(:) :: prop

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
  ! SUBROUTINE rectangular_print
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_print(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(rectangular_section), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr
    ioerr = 0
    ! do nothing
    RETURN
  END SUBROUTINE rectangular_print


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
  SUBROUTINE rectangular_props(this, depth, area, hydrad, &
       &topwidth, conveyance, dkdy)
    IMPLICIT NONE
    CLASS(rectangular_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy
    DOUBLE PRECISION :: perm

    perm = this%bottom_width + 2.0*depth
    area = depth*this%bottom_width
    topwidth = this%bottom_width
    IF (area .GT. 0.0) THEN
       hydrad = area/perm
       conveyance = (area**(5./3.))/(perm**(2./3.))
       dkdy =  conveyance*(5.0*this%bottom_width/area - 4.0/perm)/3.0
    ELSE 
       hydrad = 0.0
       conveyance = 0.0
       dkdy = 0.0
    END IF
  END SUBROUTINE rectangular_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_destroy(this)
    IMPLICIT NONE
    CLASS(rectangular_section), INTENT(INOUT) :: this

    ! do nothing

  END SUBROUTINE rectangular_destroy

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
  SUBROUTINE trapezoidal_props(this, depth, area, hydrad, topwidth, conveyance, dkdy)
    IMPLICIT NONE
    CLASS (trapezoidal_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth 
    DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy

    ! to make thing a little more readable
    DOUBLE PRECISION :: b, z, P, dAdy, dPdy
    b = this%bottom_width
    z = this%sidez
    
    area = (b + z*depth)*depth
    P = b + 2.0*depth*SQRT(1 + z*z)
    hydrad = area/P
    topwidth = b + 2.0*z*depth
    conveyance = area*hydrad**(2.0/3.0)
    ! or conveyance = area**(5.0/3.0)/P
    dAdy = b + 2*z*depth
    dPdy = 2*SQRT(1 + z*z)
    dkdy = (5.0/3.0)*dAdy*hydrad**(2.0/3.0) - (2.0/3.0)*hydrad**(5.0/3.0)*dPdy

  END SUBROUTINE trapezoidal_props


  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_flood_read
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_flood_read(this, iounit, ioerr)
    IMPLICIT NONE
    CLASS(rectangular_flood_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    INTEGER, INTENT(OUT) :: ioerr

    READ(iounit,*,IOSTAT=ioerr) &
         &this%depth_main, this%bottom_width, this%bottom_width_flood

  END SUBROUTINE rectangular_flood_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_flood_props
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_flood_props(this, depth, area, hydrad, &
       &topwidth, conveyance, dkdy)

    IMPLICIT NONE
    CLASS(rectangular_flood_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth
    DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy
    DOUBLE PRECISION :: perm
    
    IF (depth .LE. this%depth_main) THEN
       CALL rectangular_props(this, depth, area, hydrad, &
            &topwidth, conveyance, dkdy)
    ELSE
       area = this%depth_main*this%bottom_width + &
            &(depth - this%depth_main)*this%bottom_width_flood
       perm = 2*depth + this%bottom_width_flood
       topwidth = this%bottom_width_flood
       hydrad = area/perm
       conveyance = (area**(5./3.))/(perm**(2./3.))
       dkdy = conveyance*(5.0*topwidth/area - 4.0/perm)/3.0
    ENDIF
  END SUBROUTINE rectangular_flood_props

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
    INTEGER :: num_levels
    INTEGER :: iperm(num_pairs)
    DOUBLE PRECISION :: x(num_pairs), y(num_pairs)
    DOUBLE PRECISION :: y_new(num_pairs)
    DOUBLE PRECISION :: width(num_pairs), width_new(num_pairs)
    DOUBLE PRECISION :: level, x_inter, max_elv, min_elv
    LOGICAL :: last_point_wet, unique

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

       DO j = 2,num_pairs	! loop through rest of levels and add up width

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

    CALL SVRGP(num_pairs,y,y_new,iperm)	! IMSL routine to sort

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
    this%prop(i)%hydradius = 0.0
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
       this%prop(i)%hydradius = this%prop(i)%area/this%prop(i)%wetperim
       this%prop(i)%conveyance = &
            &this%prop(i)%area*this%prop(i)%hydradius**(2.0/3.0)
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
    TYPE (general_section_prop), INTENT(INOUT) :: p

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
    p%hydradius = factor*(this%prop(j+1)%hydradius - this%prop(j)%hydradius) + this%prop(j)%hydradius
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
    ALLOCATE(xy(2*num_pairs))
    READ(iounit,*,IOSTAT=ioerr) xy(1:2*num_pairs)
    
    CALL this%build(tmp_delta_y, num_pairs, xy)

  END SUBROUTINE general_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE general_props
  ! ----------------------------------------------------------------
  SUBROUTINE general_props(this, depth, area, hydrad, &
       &topwidth, conveyance, dkdy)
    IMPLICIT NONE
    CLASS(general_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth 
    DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy

    TYPE (general_section_prop) :: prop
    CALL this%interp(depth, prop)
    area = prop%area
    hydrad = prop%hydradius
    topwidth = prop%topwidth
    conveyance = prop%conveyance
    dkdy = prop%dkdy

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
            &this%prop(i)%wetperim, this%prop(i)%hydradius, this%prop(i)%conveyance
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
    DOUBLE PRECISION :: area, hydrad, topwidth, conveyance, dkdy
    INTEGER :: id, i, steps
    

    WRITE(iounit, *)
    WRITE(iounit, *) 'Section Number ', xsect%id
    WRITE(iounit, 10)
    WRITE(iounit, 12) 'y', 'Width', 'Area', 'H Radius', 'Convey', 'dkdy'
    WRITE(iounit, 10)

    steps = AINT(ymax/dy + 0.5) + 1
    y = 0
    DO i = 0, steps
       y = REAL(i)*dy
       CALL xsect%props(y, area, hydrad, topwidth, conveyance, dkdy)
       WRITE(iounit, 15) y, area, hydrad, topwidth, conveyance, dkdy
    END DO
    WRITE(iounit, 10)
    WRITE(iounit, *)
  
10 FORMAT(6('-------------'))
12 FORMAT(6(1X, A12))
15 FORMAT(6(1X, F12.2))

    

  END SUBROUTINE cross_section_table


END MODULE cross_section
