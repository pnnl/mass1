# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2019-04-02 10:35:22 d3g096
# -------------------------------------------------------------

set(SRC "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1-warmup.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-warmup.cfg"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1-flowbc.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-flowbc.cfg"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1-lateral.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-lateral.cfg"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/link-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/link-flowbc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/link-flowbc-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/lateral-inflow-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/lateral-inflow-files.dat"
)

set(testfiles 
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
)

add_custom_command(
  OUTPUT 
  "${CMAKE_CURRENT_BINARY_DIR}/hotstart-warmup.dat"
  COMMAND ${CMAKE_COMMAND} -E copy mass1-warmup.cfg mass1.cfg
  COMMAND ${mass1newexe} > mass1.warmup.log
  COMMAND ${CMAKE_COMMAND} -E rename profile1.out profile1.warmup.out
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/storage (warmup)"
  DEPENDS 
  mass1 ${testfiles}
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-warmup.cfg"
  "${CMAKE_CURRENT_SOURCE_DIR}/inflow-bc.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/link-flowbc-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/tailwater-bc.dat"
)
set(rundeps "${CMAKE_CURRENT_BINARY_DIR}/hotstart-warmup.dat")

add_custom_command(
  OUTPUT 
  "${CMAKE_CURRENT_BINARY_DIR}/profile1.flowbc.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts11.flowbc.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts1149.flowbc.out"
  COMMAND ${CMAKE_COMMAND} -E copy mass1-flowbc.cfg mass1.cfg
  COMMAND ${mass1newexe} > mass1.flowbc.log
  COMMAND ${CMAKE_COMMAND} -E rename profile1.out profile1.flowbc.out
  COMMAND ${CMAKE_COMMAND} -E rename ts11.out ts11.flowbc.out
  COMMAND ${CMAKE_COMMAND} -E rename ts1149.out ts1149.flowbc.out
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/storage (flowbc)"
  DEPENDS 
  mass1 ${testfiles}
  "${CMAKE_CURRENT_BINARY_DIR}/hotstart-warmup.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-flowbc.cfg"
  "${CMAKE_CURRENT_SOURCE_DIR}/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/outflow-bc.dat"
)

add_custom_command(
  OUTPUT 
  "${CMAKE_CURRENT_BINARY_DIR}/profile1.lateral.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts11.lateral.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts1149.lateral.out"
  COMMAND ${CMAKE_COMMAND} -E copy mass1-lateral.cfg mass1.cfg
  COMMAND ${mass1newexe} > mass1.lateral.log
  COMMAND ${CMAKE_COMMAND} -E rename profile1.out profile1.lateral.out
  COMMAND ${CMAKE_COMMAND} -E rename ts11.out ts11.lateral.out
  COMMAND ${CMAKE_COMMAND} -E rename ts1149.out ts1149.lateral.out
  
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test flow/storage (lateral)"
  DEPENDS 
  mass1 ${testfiles}
  "${CMAKE_CURRENT_BINARY_DIR}/hotstart-warmup.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1-lateral.cfg"
  "${CMAKE_CURRENT_SOURCE_DIR}/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/outflow-bc.dat"
)
set(rundeps 
  "${CMAKE_CURRENT_BINARY_DIR}/profile1.flowbc.out"
  "${CMAKE_CURRENT_BINARY_DIR}/profile1.lateral.out"
)

if (GNUPLOT_EXECUTABLE AND AWK_EXECUTABLE) 
  add_custom_command(
    OUTPUT 
    "${CMAKE_CURRENT_BINARY_DIR}/storage.flowbc.out"
    "${CMAKE_CURRENT_BINARY_DIR}/storage.lateral.out"
    COMMAND ${AWK_EXECUTABLE} -f "${CMAKE_CURRENT_SOURCE_DIR}/profile-storage.gawk" 
        profile1.flowbc.out > storage.flowbc.out
    COMMAND ${AWK_EXECUTABLE} -f "${CMAKE_CURRENT_SOURCE_DIR}/profile-storage.gawk" 
        profile1.lateral.out > storage.lateral.out
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/profile-storage.gawk"
    "${CMAKE_CURRENT_BINARY_DIR}/profile1.flowbc.out"
    "${CMAKE_CURRENT_BINARY_DIR}/profile1.lateral.out"
    )

  add_custom_command(
    OUTPUT 
    "${CMAKE_CURRENT_BINARY_DIR}/plot-flowbc.eps"
    "${CMAKE_CURRENT_BINARY_DIR}/plot-lateral.eps"
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot-flowbc.gp" > plot-flowbc.eps
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot-lateral.gp" > plot-lateral.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting test flow/lateral"
    DEPENDS 
    ${rundeps}
    "${CMAKE_CURRENT_BINARY_DIR}/ts11.flowbc.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts1149.flowbc.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts11.lateral.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts1149.lateral.out"
    "${CMAKE_CURRENT_BINARY_DIR}/storage.flowbc.out"
    "${CMAKE_CURRENT_BINARY_DIR}/storage.lateral.out"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot-flowbc.gp"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot-lateral.gp"
  )
  set(rundeps 
    "${CMAKE_CURRENT_BINARY_DIR}/plot-flowbc.eps"
    "${CMAKE_CURRENT_BINARY_DIR}/plot-lateral.eps"
    )
endif ()

add_custom_target(run_storage_test
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_storage_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  status.out
  error-warning.out
  hotstart-warmup.dat
  profile1.flowbc.out
  profile1.lateral.out
  profile1.warmup.out
  ts11.flowbc.out
  ts11.lateral.out
  ts1149.flowbc.out
  ts1149.lateral.out
  mass1.cfg
  mass1.flowbc.log
  mass1.lateral.log
  mass1.warmup.log
)

