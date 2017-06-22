! ----------------------------------------------------------------
! file: hydro_output.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created November  2, 1999 by William A. Perkins
! Last Change: 2017-06-22 15:08:23 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE hydro_output
! Variables and functions used to produce hydro power link water
! quality data.
! ----------------------------------------------------------------
MODULE hydro_output_module

  INTEGER, PARAMETER, PRIVATE :: iobase = 110
  INTEGER, PRIVATE :: hydro_links

  TYPE hydro_specs_struct
     INTEGER :: link
     CHARACTER (LEN=80) :: filename
  END TYPE hydro_specs_struct

  TYPE(hydro_specs_struct), ALLOCATABLE :: hydro_specs(:)

  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: hydro_spill, hydro_gen, hydro_disch, &
       &hydro_conc, hydro_sat, hydro_temp, hydro_baro

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_output_filename
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_output_filename(rec)
    
    IMPLICIT NONE
    TYPE(hydro_specs_struct) :: rec
    CHARACTER (LEN=80) buffer

    buffer = ""

    WRITE(buffer, 100) rec%link

    rec%filename = buffer

100 FORMAT("hts", I2.2, ".out")

  END SUBROUTINE hydro_output_filename


  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_output_setup
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_output_setup()

    USE mass1_config
    USE link_vars, ONLY : linktype

    IMPLICIT NONE

    INTEGER :: link, i

    hydro_links = 0

    IF (.NOT. config%do_gas) RETURN

                                ! count up the number of hydro links



    DO link = 1, config%maxlinks
       SELECT CASE (linktype(link))
       CASE (6,21)
          hydro_links = hydro_links + 1
       END SELECT
    END DO

    IF (hydro_links .LE. 0) RETURN

                                ! allocate arrays for the necessary
                                ! information

    ALLOCATE(hydro_specs(hydro_links))

                                ! build a file name, open that file
                                ! and put a header in it

    i = 0
    DO link = 1, config%maxlinks
       hydro_disch(link) = -1.0
       hydro_spill(link) = -1.0
       hydro_gen(link) = -1.0
       hydro_conc(link) = -1.0
       hydro_sat(link) = -1.0
       hydro_temp(link) = -1.0
       SELECT CASE (linktype(link))
       CASE (6,21)
          i = i + 1
          hydro_specs(i)%link = link
          CALL hydro_output_filename(hydro_specs(i))
          OPEN(iobase + i, file=hydro_specs(i)%filename)
          WRITE(iobase + i, 100) 
       END SELECT
    END DO
100 FORMAT("#Date      Time       Discharge      Spill   ",&
         &"Nonspill       Temp       Conc        Sat      Press    Delta P")
  END SUBROUTINE hydro_output_setup

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_output
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_output(date, time)

    USE mass1_config
    USE gas_functions

    IMPLICIT NONE

    CHARACTER(*) :: date, time
    INTEGER :: i, link
    DOUBLE PRECISION :: press, deltap

    IF (hydro_links .LE. 0 .OR. .NOT. config%do_gas) RETURN

    DO i = 1, hydro_links
       link = hydro_specs(i)%link

       press = TDGasPress(hydro_conc(link), hydro_temp(link), DBLE(0.0));
       deltap = press - hydro_baro(link)
       WRITE(iobase + i, 100) date, time, hydro_disch(link), hydro_spill(link),&
            &hydro_gen(link), hydro_temp(link), hydro_conc(link), hydro_sat(link), &
            &press, deltap
    END DO

100 FORMAT(A10,1X,A8,1X,8(1X, F10.1))
  END SUBROUTINE hydro_output

  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_output_done
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_output_done()

    INTEGER :: i

    DO i = 1, hydro_links
       CLOSE(iobase + i)
    END DO

  END SUBROUTINE hydro_output_done

END MODULE hydro_output_module
