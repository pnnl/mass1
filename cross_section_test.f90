  ! ----------------------------------------------------------------
  ! file: cross_section_test.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created January  3, 2017 by William A. Perkins
  ! Last Change: 2017-01-05 10:59:12 d3g096
  ! ----------------------------------------------------------------
PROGRAM cross_section_test
  USE utility
  USE cross_section
  USE section_handler_module
  IMPLICIT NONE

  CLASS (xsection_t), POINTER :: x
  TYPE (section_list) :: xslist
  TYPE (section_list_node), POINTER :: xsnode
  INTEGER :: iounit, ioerr 

  iounit = 52

  CALL open_existing("cross_section_test.dat", iounit, fatal=.TRUE.)

  DO WHILE (.TRUE.)
     x => read_cross_section(iounit, ioerr)
     IF (.NOT. ASSOCIATED(x)) EXIT
     WRITE (6, *) "Read cross section ", x%id
     CALL xslist%push(x)
  END DO

  CLOSE(iounit)

  WRITE (6, *) "A total of ", xslist%size(), " sections were read"

  x => xslist%find(10000)
  
  IF (ASSOCIATED(x)) THEN
     WRITE (6, *) 'Found section ', x%id
  END IF

  xsnode => xslist%head
  DO WHILE (ASSOCIATED(xsnode))
     x => xsnode%section
     CALL x%print(6, ioerr)
     xsnode => xsnode%next
  END DO

  CALL xslist%clear()

END PROGRAM cross_section_test
