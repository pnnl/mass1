# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 29, 2016 by William A. Perkins
# Last Change: 2017-07-03 11:33:04 d3g096
# -------------------------------------------------------------

# -------------------------------------------------------------
# General set up
# -------------------------------------------------------------
cmake_minimum_required (VERSION 2.8.12)

project(MASS1 Fortran)

# -------------------------------------------------------------
# Project set up
# -------------------------------------------------------------

# where to find CMake modules
list (APPEND CMAKE_MODULE_PATH "${MASS1_SOURCE_DIR}/cmake")

# -------------------------------------------------------------
# Compiler options
# -------------------------------------------------------------

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # trap floating point exceptions
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -ffpe-trap=invalid,zero,overflow")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  # w/ Intel, build static binaries that trap floating point exceptions
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpe0")
endif()

# -------------------------------------------------------------
# Perl - for scripts
# -------------------------------------------------------------
find_package(Perl)

find_package(PerlModules COMPONENTS Date::Manip)

# -------------------------------------------------------------
# Gnuplot - for tests
# -------------------------------------------------------------
find_program(GNUPLOT_EXECUTABLE gnuplot)
# find_package(gnuplot)
# should check version

find_program(AWK_EXECUTABLE gawk)
if (NOT AWK_EXECUTABLE) 
  find_program(AWK_EXECUTABLE awk)
endif ()

# -------------------------------------------------------------
# GIT - for banner revision
# -------------------------------------------------------------
find_package(Git)

if (GIT_EXECUTABLE) 

  execute_process(
    COMMAND git rev-parse --abbrev-ref HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE MASS1_BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  execute_process(
    COMMAND ${GIT_EXECUTABLE} describe --always --abbrev=6 --dirty
    WORKING_DIRECTORY "${MASS1_SOURCE_DIR}"
    OUTPUT_VARIABLE MASS1_REVISION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} log -1 --format=%cd --date=local
    WORKING_DIRECTORY "${MASS1_SOURCE_DIR}"
    OUTPUT_VARIABLE MASS1_REVISION_DATE
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
else(GIT_EXECUTABLE)
  set(MASS1_BRANCH "Unknown")
  set(MASS1_REVISION "Unknown")
  set(MASS1_REVISION_DATE "Unknown")
endif(GIT_EXECUTABLE)
configure_file(banner.f90.in banner.f90 @ONLY)

# -------------------------------------------------------------
# subdirectories
# -------------------------------------------------------------

add_subdirectory(time_series)
add_subdirectory(gastemp)
add_subdirectory(scripts)

include_directories(BEFORE 
  ${CMAKE_CURRENT_BINARY_DIR}/gastemp
  ${CMAKE_CURRENT_BINARY_DIR}/time_series
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
)

# -------------------------------------------------------------
# mass1
# -------------------------------------------------------------
set(mass1src
  array_alloc.f90
  array_dealloc.f90
  "${CMAKE_CURRENT_BINARY_DIR}/banner.f90"
  bc_module.f90
  coeff.f90
  cross_section.f90
  confluence_module.f90
  dlist_module.f90
  flow_sim.f90
  gage_output.f90
  hydro_output.f90
  initial_cond.f90
  kick_off.f90
  linear_interp.f90
  link_data.f90
  mass1.f90
  mass1_config.f90
  met_zone.f90
  modules.f90
  nonfluvial_coeff.f90
  pidlink.f90
  point_data.f90
  print_output.f90
  profile_output.f90
  read_hotstart.f90
  scalars_module.f90
  section_handler.f90
  svgrp.f90
  tdg_equation_coeff.f90
  write_restart.f90
)

add_executable(mass1 ${mass1src})
target_link_libraries(mass1 gastemp ts)
install(TARGETS mass1 DESTINATION bin)

# save for running tests
set(mass1exe $<TARGET_FILE:mass1>)
#this is deprecated
#get_property(mass1exe TARGET mass1 PROPERTY LOCATION)

# -------------------------------------------------------------
# cross_section_test
# -------------------------------------------------------------
add_executable(cross_section_test 
  cross_section_test.f90
  section_handler.f90
  cross_section.f90
  dlist_module.f90
  linear_interp.f90
  svgrp.f90
)
target_link_libraries(cross_section_test ts)

# -------------------------------------------------------------
# trapezoid_test
# -------------------------------------------------------------
add_executable(cross_section_table
  cross_section_table.f90
  section_handler.f90
  cross_section.f90
  dlist_module.f90
  linear_interp.f90
  svgrp.f90
)
target_link_libraries(cross_section_table ts)

# -------------------------------------------------------------
# svgrptest
# -------------------------------------------------------------
add_executable(svgrptest svgrp-test.f90 svgrp.f90)

# -------------------------------------------------------------
# link_test
# -------------------------------------------------------------
add_executable(link_test 
  link_test.f90 
  link_module.f90
  linear_link_module.f90
  dlist_module.f90
  bc_module.f90
)
target_link_libraries(link_test ts)

# -------------------------------------------------------------
# custom test target
# -------------------------------------------------------------
add_custom_target(runtests
  COMMENT "MASS1 tests completed"
)

# target runtests must exist before doing this
add_subdirectory(test)
