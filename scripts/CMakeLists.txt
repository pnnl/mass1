# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 30, 2016 by William A. Perkins
# Last Change: 2016-12-30 13:37:36 d3g096
# -------------------------------------------------------------

if (PERL_FOUND) 
  set(PACKAGE "${PROJECT_NAME}")
  set(VERSION "0.80")
  set(PERL "${PERL_EXECUTABLE}")
  foreach(s mass1bc mass1profile)
    configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/${s}.in"    
      "${CMAKE_CURRENT_BINARY_DIR}/${s}"
      @ONLY
    )
    install(PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/${s}" DESTINATION bin)
  endforeach(s)
  unset(PACKAGE)
  unset(VERSION)
  unset(PERL)
endif (PERL_FOUND)

install(FILES 
  profile2cgns.py
  profile_extract.py
  DESTINATION bin
)
