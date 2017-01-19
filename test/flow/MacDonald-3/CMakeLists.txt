# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2017-01-19 11:36:04 d3g096
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
  "${CMAKE_CURRENT_SOURCE_DIR}/flow.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/stage.dat"
)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
  COMMAND ${mass1exe} > mass1.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/MacDonald-3"
  DEPENDS mass1 ${testfiles}
)
set(rundeps "${CMAKE_CURRENT_BINARY_DIR}/profile1.out")

if (GNUPLOT_EXECUTABLE) 
  add_custom_command(
    OUTPUT plot-depth.eps plot-elev.eps
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot-depth.gp" > plot-depth.eps
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot-elev.gp" > plot-elev.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting test flow/MacDonald-3"
    DEPENDS 
    "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot-depth.gp"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot-elev.gp"
  )
  set(rundeps plot-depth.eps plot-elev.eps)
endif (GNUPLOT_EXECUTABLE)

add_custom_target(run_macdonald_3_test
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_macdonald_3_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  status.out
  plot-depth.eps
  plot-elev.eps
  mass1.log
)

