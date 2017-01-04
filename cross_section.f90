! ----------------------------------------------------------------
! file: cross_section.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January  3, 2017 by William A. Perkins
! Last Change: 2017-01-03 14:46:50 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE cross_section
! ----------------------------------------------------------------
MODULE cross_section

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  TYPE, ABSTRACT, PUBLIC :: xsection_t
     INTEGER :: ID
   CONTAINS
     PROCEDURE (read_proc), DEFERRED :: read
     PROCEDURE (props_proc), DEFERRED :: props
  END TYPE xsection_t

  ABSTRACT INTERFACE
     SUBROUTINE read_proc(this, iounit)
       IMPORT :: xsection_t
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(INOUT) :: this
       INTEGER, INTENT(IN) :: iounit
     END SUBROUTINE read_proc
     
     SUBROUTINE props_proc(this, depth, kstrick, area, hydrad, topwidth, conveyance, dkdy)
       IMPORT :: xsection_t
       IMPLICIT NONE
       CLASS(xsection_t), INTENT(IN) :: this
       DOUBLE PRECISION, INTENT(IN) :: depth, kstrick
       DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy
     END SUBROUTINE props_proc
  END INTERFACE

  TYPE, PUBLIC, EXTENDS(xsection_t) :: rectangular_section
     DOUBLE PRECISION :: bottom_width
   CONTAINS
     PROCEDURE :: read => rectangular_read
     PROCEDURE :: props => rectangular_props
  END type rectangular_section

  TYPE, PUBLIC, EXTENDS(rectangular_section) :: rectangular_flood_section
     DOUBLE PRECISION :: depth_main
     DOUBLE PRECISION :: bottom_width_flood
   CONTAINS
     PROCEDURE :: read => rectangular_flood_read
     PROCEDURE :: props => rectangular_flood_props
  END type rectangular_flood_section

  DOUBLE PRECISION, PARAMETER, PRIVATE :: res_coeff = 1.49

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_read
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_read(this, iounit)
    IMPLICIT NONE
    CLASS(rectangular_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    
    READ(iounit,*) this%bottom_width

  END SUBROUTINE rectangular_read


  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_props
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_props(this, depth, kstrick, area, hydrad, &
       &topwidth, conveyance, dkdy)
    IMPLICIT NONE
    CLASS(rectangular_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth, kstrick
    DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy
    DOUBLE PRECISION :: perm

    perm = this%bottom_width + 2.0*depth
    area = depth*this%bottom_width
    IF (perm > 0.0) THEN
       hydrad = area/perm
    ELSE 
       hydrad = 0.0
    END IF
    topwidth = this%bottom_width
    conveyance = res_coeff*kstrick*(area**(5./3.))/(perm**(2./3.))
    dkdy =  conveyance*(5.0*this%bottom_width/area - 4.0/perm)/3.0
  END SUBROUTINE rectangular_props

  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_flood_read
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_flood_read(this, iounit)
    IMPLICIT NONE
    CLASS(rectangular_flood_section), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iounit
    
    READ(iounit,*) this%depth_main, this%bottom_width, this%bottom_width_flood
  END SUBROUTINE rectangular_flood_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE rectangular_flood_props
  ! ----------------------------------------------------------------
  SUBROUTINE rectangular_flood_props(this, depth, kstrick, area, hydrad, &
       &topwidth, conveyance, dkdy)

    IMPLICIT NONE
    CLASS(rectangular_flood_section), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: depth, kstrick
    DOUBLE PRECISION, INTENT(OUT) :: area, hydrad, topwidth, conveyance, dkdy
    DOUBLE PRECISION :: perm
    
    IF (depth .LE. this%depth_main) THEN
       CALL rectangular_props(this, depth, kstrick, area, hydrad, &
            &topwidth, conveyance, dkdy)
    ELSE
       area = this%depth_main*this%bottom_width + &
            &(depth - this%depth_main)*this%bottom_width_flood
       perm = 2*depth + this%bottom_width_flood
       topwidth = this%bottom_width_flood
       hydrad = area/perm
       conveyance = res_coeff*kstrick*(area**(5./3.))/(perm**(2./3.))
       dkdy = conveyance*(5.0*topwidth/area - 4.0/perm)/3.0
    ENDIF
  END SUBROUTINE rectangular_flood_props

END MODULE cross_section
