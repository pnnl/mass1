# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 29, 2016 by William A. Perkins
# Last Change: 2016-12-29 11:30:33 d3g096
# -------------------------------------------------------------

add_library(gastemp STATIC
  gas_coeffs_module.f90
  gas_functions_module.f90
  energy_flux_module.f90
)
