# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2019-04-02 10:32:02 d3g096
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

set(testfiles 
  "${CMAKE_CURRENT_BINARY_DIR}/mass1.cfg"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/gage_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/tailwater-bc.dat"
)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
  COMMAND ${mass1newexe} > mass1.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/drain"
  DEPENDS mass1_new ${testfiles}
)
set(rundeps "${CMAKE_CURRENT_BINARY_DIR}/profile1.out")

if (GNUPLOT_EXECUTABLE) 
  add_custom_command(
    OUTPUT 
    "plot.eps" 
    "plot-elev.eps" 
    "plot-disch.eps"
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp" > plot.eps
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot-elev.gp" > plot-elev.eps
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot-disch.gp" > plot-disch.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting tests flow/drain"
    DEPENDS 
    "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot-elev.gp"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot-disch.gp"
  )
  set(rundeps 
    "plot.eps" 
    "plot-elev.eps" 
    "plot-disch.eps"
    )
endif (GNUPLOT_EXECUTABLE)

add_custom_target(run_drain_test
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_drain_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  status.out
  error-warning.out
  plot.eps
  plot-elev.eps
  plot-disch.eps
  error-warning.out
  mass1.log
)

