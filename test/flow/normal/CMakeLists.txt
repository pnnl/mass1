# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2017-01-19 09:52:40 d3g096
# -------------------------------------------------------------

set(SRC "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1.cfg"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/link-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/runit.sh.in"
  "${CMAKE_CURRENT_BINARY_DIR}/runit.sh"
  @ONLY
)

set(testfiles 
  "${CMAKE_CURRENT_BINARY_DIR}/mass1.cfg"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/gage_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/tailwater-bc.dat"
)

add_custom_target(run_normal_test
  COMMAND sh ./runit.sh >& make.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/normal"
  DEPENDS mass1 ${testfiles}
)
add_dependencies(runtests run_normal_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  status.out
  plot.ps
  plot.eps
  plot-elev.ps
  plot-elev.eps
  mass1.log
)

