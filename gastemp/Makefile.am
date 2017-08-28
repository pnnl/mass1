# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 10, 2010 by William A. Perkins
# Last Change: 2017-08-24 09:40:11 d3g096
# -------------------------------------------------------------

SUBDIRS =

AM_FCFLAGS = 

noinst_LIBRARIES = libgastemp.a

libgastemp_a_SOURCES = \
	gas_coeffs_module.f90 \
	gas_functions_module.f90 \
	energy_flux_module.f90

gas_functions_module.$(OBJEXT): gas_coeffs_module.$(OBJEXT) 
