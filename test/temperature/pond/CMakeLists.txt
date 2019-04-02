# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 28, 2017 by William A. Perkins
# Last Change: 2019-03-29 14:07:44 d3g096
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
  "${CMAKE_CURRENT_SOURCE_DIR}/temp-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/temp-bc-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/weather-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/weather-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/temp.gp.in"
  "${CMAKE_CURRENT_BINARY_DIR}/temp.gp"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/temp.1992.gp.in"
  "${CMAKE_CURRENT_BINARY_DIR}/temp.1992.gp"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/temp.summer.1992.gp.in"
  "${CMAKE_CURRENT_BINARY_DIR}/temp.summer.1992.gp"
)


set(testfiles 
  "${CMAKE_CURRENT_BINARY_DIR}/mass1.cfg"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/temp-bc-files.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/weather-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/gage_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/inflow-temp.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/tailwater-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/met.dat"
)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/ts1500.out"
  COMMAND ${mass1newexe} > mass1.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test temperature/pond"
  DEPENDS mass1_new ${testfiles}
)
set(rundeps "${CMAKE_CURRENT_BINARY_DIR}/ts1500.out")

if (GNUPLOT_EXECUTABLE) 
  add_custom_command(
    OUTPUT 
    "temp.eps" "temp.1992.eps" "temp.summer.1992.eps"
    COMMAND "${GNUPLOT_EXECUTABLE}" "temp.gp" > "temp.eps"
    COMMAND "${GNUPLOT_EXECUTABLE}" "temp.1992.gp" > "temp.1992.eps"
    COMMAND "${GNUPLOT_EXECUTABLE}" "temp.summer.1992.gp" > "temp.summer.1992.eps"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting tests temperature/pond"
    DEPENDS 
    "${CMAKE_CURRENT_BINARY_DIR}/ts1500.out"
    "${CMAKE_CURRENT_BINARY_DIR}/temp.gp"
    "${CMAKE_CURRENT_BINARY_DIR}/temp.1992.gp"
    "${CMAKE_CURRENT_BINARY_DIR}/temp.summer.1992.gp"
  )
  set(rundeps 
    "temp.eps" 
    "temp.1992.eps"
    "temp.summer.1992.eps"
    )
endif (GNUPLOT_EXECUTABLE)

add_custom_target(run_pond_test
  DEPENDS ${rundeps}
)

# This test takes too long for this
# add_dependencies(runtests run_pond_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  status.out
  temp.eps 
  temp.1992.eps 
  temp.summer.1992.eps
  mass1.log
  error-warning.out
  ts11.out
  ts1250.out
  ts1500.out
)

