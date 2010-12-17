# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 10, 2010 by William A. Perkins
# Last Change: Fri Dec 10 08:38:55 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
# -------------------------------------------------------------

SUBDIRS =

AM_FCFLAGS = 

lib_LIBRARIES = libgastemp.a

libgastemp_a_SOURCES = \
	gas_coeffs_module.f90 \
	gas_functions_module.f90 \
	energy_flux_module.f90

gas_functions_module.$(OBJEXT): gas_coeffs_module.$(OBJEXT) 
