# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2019-05-23 13:35:12 d3g096
# -------------------------------------------------------------

set(SRC "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1-warmup.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-warmup.cfg"
  )

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1-run.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-run.cfg"
  )

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/link-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/temp-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/temp-bc-files.dat"
)

set(testfiles 
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/temp-bc-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/gage_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/tailwater-bc.dat"
)

set(rundeps)

# Warm up phase

add_custom_command(
  OUTPUT
  "${CMAKE_CURRENT_BINARY_DIR}/hotstart-warmup.dat"
  COMMAND ${CMAKE_COMMAND} -E copy mass1-warmup.cfg mass1.cfg
  COMMAND ${mass1newexe} > mass1.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test transport/pulse (warmup)"
  DEPENDS mass1 "${CMAKE_CURRENT_BINARY_DIR}/mass1-warmup.cfg" ${testfiles}
  )

# Run phase

add_custom_command(
  OUTPUT
  "${CMAKE_CURRENT_BINARY_DIR}/ts11.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts12.out"
  COMMAND ${CMAKE_COMMAND} -E copy mass1-run.cfg mass1.cfg
  COMMAND ${mass1newexe} > mass1.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test transport/hlink-pulse (run)"
  DEPENDS mass1 "${CMAKE_CURRENT_BINARY_DIR}/mass1-run.cfg" ${testfiles}
  "${CMAKE_CURRENT_BINARY_DIR}/hotstart-warmup.dat"
  )

if (GNUPLOT_EXECUTABLE)
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/plot.gp
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMAND ${CMAKE_COMMAND} -E echo "set terminal postscript eps color solid \\\"Helvetica\\\" 14" > plot.gp
    COMMAND ${CMAKE_COMMAND} -E echo "load \\\"${CMAKE_CURRENT_SOURCE_DIR}/plot.gp\\\"" >> plot.gp
    DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp"
    )
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/plot.eps
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_BINARY_DIR}/plot.gp" > plot.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting test transport/hlink-pulse (conc)"
    DEPENDS 
    "${CMAKE_CURRENT_BINARY_DIR}/ts11.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts12.out"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp"
    "${CMAKE_CURRENT_BINARY_DIR}/plot.gp"
    )
  list(APPEND rundeps ${CMAKE_CURRENT_BINARY_DIR}/plot.eps)
  
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mass-plot.gp
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMAND ${CMAKE_COMMAND} -E echo "set terminal postscript eps color solid \\\"Helvetica\\\" 14" > mass-plot.gp
    COMMAND ${CMAKE_COMMAND} -E echo "load \\\"${CMAKE_CURRENT_SOURCE_DIR}/mass-plot.gp\\\"" >> mass-plot.gp
    DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/mass-plot.gp"
    )
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/mass-plot.eps
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_BINARY_DIR}/mass-plot.gp" > mass-plot.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting test transport/hlink-pulse (mass)"
    DEPENDS 
    "${CMAKE_CURRENT_BINARY_DIR}/ts11.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts12.out"
    "${CMAKE_CURRENT_SOURCE_DIR}/mass-plot.gp"
    "${CMAKE_CURRENT_BINARY_DIR}/mass-plot.gp"
    )
  list(APPEND rundeps ${CMAKE_CURRENT_BINARY_DIR}/mass-plot.eps)
else()
  list(APPEND rundeps "${CMAKE_CURRENT_BINARY_DIR}/ts11.out")
endif (GNUPLOT_EXECUTABLE)

add_custom_target(run_hlink_pulse
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_hlink_pulse)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  profile1.warmup.out
  status.out
  error-warning.out
  ts11.out ts12.out
  mass1.cfg
  mass1.log
)

