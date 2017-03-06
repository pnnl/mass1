# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2017-03-06 10:59:53 d3g096
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
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/upstream-q.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/downstream-y.dat"
)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
  COMMAND ${mass1exe} > mass1.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/dam-break"
  DEPENDS mass1 ${testfiles}
)
set(rundeps "${CMAKE_CURRENT_BINARY_DIR}/profile1.out")

if (GNUPLOT_EXECUTABLE) 
  add_custom_command(
    OUTPUT 
    "${CMAKE_CURRENT_BINARY_DIR}/plot.eps" 
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp" > plot.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting tests flow/dam-break"
    DEPENDS 
    "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp"
  )
  set(rundeps 
    "${CMAKE_CURRENT_BINARY_DIR}/plot.eps" 
    )
endif (GNUPLOT_EXECUTABLE)

add_custom_target(run_dam_break_test
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_dam_break_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  status.out
  plot.eps
  mass1.log
)

