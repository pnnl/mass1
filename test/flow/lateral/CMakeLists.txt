# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2018-08-16 07:02:49 d3g096
# -------------------------------------------------------------

set(SRC "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1-inflow.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-inflow.cfg"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1-outflow.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-outflow.cfg"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/link-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/lat-inflow-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/lat-inflow-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/lat-outflow-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/lat-outflow-files.dat"
)

set(testfiles 
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/tailwater-bc.dat"
)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/profile1.inflow.out"
  COMMAND ${CMAKE_COMMAND} -E copy mass1-inflow.cfg mass1.cfg
  COMMAND ${mass1exe} > mass1.inflow.log
  COMMAND ${CMAKE_COMMAND} -E rename profile1.out profile1.inflow.out
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/lateral (inflow)"
  DEPENDS 
  mass1 ${testfiles}
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-inflow.cfg"
  "${CMAKE_CURRENT_BINARY_DIR}/lat-inflow-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/lateral-inflow.dat"
)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/profile1.outflow.out"
  COMMAND ${CMAKE_COMMAND} -E copy mass1-outflow.cfg mass1.cfg
  COMMAND ${mass1exe} > mass1.outflow.log
  COMMAND ${CMAKE_COMMAND} -E rename profile1.out profile1.outflow.out
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/lateral (outflow)"
  DEPENDS 
  mass1 ${testfiles}
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-outflow.cfg"
  "${CMAKE_CURRENT_BINARY_DIR}/lat-outflow-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/lateral-outflow.dat"
)
set(rundeps 
  "${CMAKE_CURRENT_BINARY_DIR}/profile1.inflow.out"
  "${CMAKE_CURRENT_BINARY_DIR}/profile1.outflow.out"
)

if (GNUPLOT_EXECUTABLE) 
  add_custom_command(
    OUTPUT "plot.eps"
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp" > plot.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting test flow/lateral"
    DEPENDS 
    ${rundeps}
    "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp"
  )
  set(rundeps "plot.eps")
endif (GNUPLOT_EXECUTABLE)

add_custom_target(run_lateral_test
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_lateral_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  profile1.inflow.out
  profile1.outflow.out
  status.out
  plot-depth.eps
  plot-elev.eps
  mass1.log
)

