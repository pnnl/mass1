# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 29, 2016 by William A. Perkins
# Last Change: 2017-06-30 09:11:19 d3g096
# -------------------------------------------------------------

add_library(gastemp STATIC
  gas_coeffs_module.f90
  gas_functions_module.f90
  energy_flux_module.f90
)
