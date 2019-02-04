# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest National Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 29, 2016 by William A. Perkins
# Last Change: 2018-05-02 14:18:55 d3g096
# -------------------------------------------------------------

# NOTE: The contents of this directory are expected to be a
# subdirectory in a larger project.  That project should have the
# Fortran language enabled.


# -------------------------------------------------------------
# ts - time series library
# -------------------------------------------------------------
set(tsources
  time_series.f90
  cumulative_time_series.f90
  met_time_series.f90
  const_series.f90
  julian.f90
  date_time_module.f90
  utility.f90
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  list(APPEND tsources fptrap-null.f90)
elseif()
  list(APPEND tsources fptrap-intel.f90)
else()
  list(APPEND tsources fptrap-null.f90)
endif()

add_library(ts STATIC
  ${tsources}
)



# -------------------------------------------------------------
# test programs w/ input files
# -------------------------------------------------------------
set(tstests 	
  datetest1 
  datetest2
  tstest1
  tstest2
  tstest3
  tstest4
  cumtstest1
  mettest1
  fptraptest
)

foreach(t ${tstests})
  add_executable(${t} ${t}.f90)
  target_link_libraries(${t} ts)
endforeach(t)

add_custom_target(cumtstest1_input
  COMMAND ${CMAKE_COMMAND} -E copy 
  ${CMAKE_CURRENT_SOURCE_DIR}/cumtstest1.dat
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/cumtstest1.dat
)
add_dependencies(cumtstest1 cumtstest1_input)

add_custom_target(datetest1_input
  COMMAND ${CMAKE_COMMAND} -E copy 
  ${CMAKE_CURRENT_SOURCE_DIR}/datetest1.dat
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/datetest1.dat
)
add_dependencies(datetest1 datetest1_input)

add_custom_target(mettest1_input
  COMMAND ${CMAKE_COMMAND} -E copy 
  ${CMAKE_CURRENT_SOURCE_DIR}/mettest1.dat
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/mettest1.dat
)
add_dependencies(mettest1 mettest1_input)

add_custom_target(tstest1_input
  COMMAND ${CMAKE_COMMAND} -E copy 
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest1.dat
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest1.dat
)
add_dependencies(tstest1 tstest1_input)

add_custom_target(tstest2_input
  COMMAND ${CMAKE_COMMAND} -E copy 
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest2.dat
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest2.dat
)
add_dependencies(tstest2 tstest2_input)

add_custom_target(tstest3_input
  COMMAND ${CMAKE_COMMAND} -E copy 
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest3a.dat
  ${CMAKE_CURRENT_BINARY_DIR}

  COMMAND ${CMAKE_COMMAND} -E copy 
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest3b.dat
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest3a.dat
  ${CMAKE_CURRENT_SOURCE_DIR}/tstest3b.dat
)
add_dependencies(tstest3 tstest3_input)
