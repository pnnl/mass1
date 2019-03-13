# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2019-03-13 08:13:26 d3g096
# -------------------------------------------------------------

set(SRC "${CMAKE_CURRENT_SOURCE_DIR}")

set(CASES "warmup" "Cn=0.1" "Cn=1.0")

foreach(c ${CASES})
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/mass1-${c}.cfg.in"
    "${CMAKE_CURRENT_BINARY_DIR}/mass1-${c}.cfg"
    )
endforeach()

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
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/tailwater-bc.dat"
)

set(rundeps)

foreach(c ${CASES})

  if(${c} STREQUAL "warmup")
    set(out "hotstart-warmup.dat")
    set(exdep)
  else()
    set(out "profile1.${c}.out")
    set(exdep "${CMAKE_CURRENT_BINARY_DIR}/hotstart-warmup.dat")
  endif()
  
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${out}"
    COMMAND ${CMAKE_COMMAND} -E copy mass1-${c}.cfg mass1.cfg
    COMMAND ${mass1exe} > mass1.log
    COMMAND ${CMAKE_COMMAND} -E copy profile1.out profile1.${c}.out 
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Running test transport/advection-1-segment (${c})"
    DEPENDS mass1 "${CMAKE_CURRENT_BINARY_DIR}/mass1-${c}.cfg" ${testfiles} ${exdep}
    )
  if(NOT ${c} STREQUAL "warmup")
    list(APPEND rundeps "${CMAKE_CURRENT_BINARY_DIR}/profile1.${c}.out")
  endif()

# if (GNUPLOT_EXECUTABLE) 
#   add_custom_command(
#     OUTPUT plot-depth.eps plot-elev.eps
#     COMMAND "${GNUPLOT_EXECUTABLE}" "plot-depth.gp" > plot-depth.eps
#     COMMAND "${GNUPLOT_EXECUTABLE}" "plot-elev.gp" > plot-elev.eps
#     WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
#     COMMENT "Plotting test flow/MacDonald-1"
#     DEPENDS 
#     "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
#     "${CMAKE_CURRENT_BINARY_DIR}/plot-depth.gp"
#     "${CMAKE_CURRENT_BINARY_DIR}/plot-elev.gp"
#   )
#   set(rundeps plot-depth.eps plot-elev.eps)
# endif (GNUPLOT_EXECUTABLE)
endforeach()

add_custom_target(run_advection_1_test
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_advection_1_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  profile1.warmup.out
  status.out
  error-warning.out
  ts11.out ts1149.out
  plot-depth.eps
  plot-elev.eps
  mass1.log
)

