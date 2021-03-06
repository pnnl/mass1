! ----------------------------------------------------------------
! file: banner.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created February 17, 2017 by William A. Perkins
! Last Change: 2017-06-21 15:04:12 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! SUBROUTINE banner
! ----------------------------------------------------------------
SUBROUTINE banner()

  IMPLICIT NONE
  WRITE(*, 10)
  WRITE(*, 11)
  WRITE (*, 15)
10 FORMAT(&
     &T4, " ____    ____       _       ______    ______    __     ", /,&
     &T4, "|_   \  /   _|     / \    .' ____ \ .' ____ \  /  |    ", /,&
     &T4, "  |   \/   |      / _ \   | (___ \_|| (___ \_| `| |    ", /,&
     &T4, "  | |\  /| |     / ___ \   _.____`.  _.____`.   | |    ", /,&
     &T4, " _| |_\/_| |_  _/ /   \ \_| \____) || \____) | _| |_   ", /,&
     &T4, "|_____||_____||____| |____|\______.' \______.'|_____|  ", /)
11 FORMAT(&
        &T4, "Copyright (c) 2017 Battelle Memorial Institute", /,&
        &T4, "Licensed under modified BSD License. A copy of this license can be", /,&
        &T4, "found in the LICENSE file in the top level directory of the source", /,&
        &T4, "distribution.", /,&
        &T4, "https://github.com/pnnl/mass1", /)

15 FORMAT(&
        &T4, 'Modular Aquatic Simulation System 1D (MASS1)', /,&
        &T4, 'Pacific Northwest National Laboratory', /,&
        &T4, 'Hydrology Group', /,&
        &T4, 'Contact:', /,&
        &T8, 'Dr. Marshall C. Richmond (marshall.richmond@pnnl.gov)', /,&
        &T8, 'William A. Perkins (william.perkins@pnnl.gov)', /)

END SUBROUTINE banner
