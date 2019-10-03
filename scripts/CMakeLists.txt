# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 30, 2016 by William A. Perkins
# Last Change: 2019-10-03 11:36:01 d3g096
# -------------------------------------------------------------

if (PERLMODULES_FOUND) 
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
else(PERLMODULES_FOUND)
  message(WARNING "Perl scripts will not be installed")
endif (PERLMODULES_FOUND)

install(FILES 
  bcsteps.py
  profile2cgns.py
  profile_extract.py
  profile_tecplot.py
  read.R
  timeshift.py
  xy-looper-png.mcr
  xy-looper.mcr
  README.md
  DESTINATION share/doc/mass1
  )
install(FILES
  cross-section/CHARIMAarea.pl
  cross-section/CHARIMAextract.pl
  cross-section/CHARIMAfill.pl
  cross-section/CHARIMAinsert.pl
  cross-section/CHARIMAnotch.pl
  cross-section/CHARIMArenum.pl
  cross-section/CHARIMAthalweg.pl
  cross-section/CHARIMAview.pl
  cross-section/HEC2CHARIMA.pl
  cross-section/HEC2area.pl
  cross-section/HECRASCHARIMA.pl
  cross-section/README.md
  cross-section/XSECTprops.pl
  cross-section/XSection.pm
  cross-section/section-fix.awk
  cross-section/section-unfix.awk
  DESTINATION share/doc/mass1/cross-section
)

