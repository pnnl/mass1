# -*- makefile -*----------------------------------------------
# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 23, 2005 by William A. Perkins
# Last Change: 2017-06-22 09:19:37 d3g096
# -------------------------------------------------------------
# RCS ID: $Id$

AM_FCFLAGS = ${FC_MODINC}time_series ${FC_MODINC}gastemp

SUBDIRS = @SUBDIRS@

bin_PROGRAMS = mass1
check_PROGRAMS = svgrptest

mass1_SOURCES =                                 \
    accumulator.f90                             \
    array_alloc.f90                             \
    array_dealloc.f90                           \
    banner.f90                                  \
    bctable.f90                                 \
    coeff.f90                                   \
    flow_sim.f90                                \
    gage_output.f90                             \
    hydro_output.f90                            \
    initial_cond.f90                            \
    kick_off.f90                                \
    latflow_bc.f90                              \
    linear_interp.f90                           \
    link_bc.f90                                 \
    link_data.f90                               \
    mass1.f90                                   \
    met_data_module.f90                         \
    modules.f90                                 \
    nonfluvial_coeff.f90                        \
    pidlink.f90                                 \
    point_data.f90                              \
    print_output.f90                            \
    profile_output.f90                          \
    read_config.f90                             \
    read_hotstart.f90                           \
    scalars_module.f90                          \
    section.f90                                 \
    section_data.f90                            \
    section_table.f90                           \
    svgrp.f90                                   \
    tdg_equation_coeff.f90                      \
    trans_bc.f90                                \
    write_restart.f90

mass1_LDADD =  gastemp/libgastemp.a time_series/libts.a


svgrptest_SOURCES = svgrp-test.f90 svgrp.f90
svgrptest_LDADD =

# dependancies for individual object files

accumulator.$(OBJEXT): modules.$(OBJEXT) scalars_module.$(OBJEXT)
array_alloc.$(OBJEXT):  modules.$(OBJEXT) hydro_output.$(OBJEXT)
array_dealloc.$(OBJEXT): modules.$(OBJEXT) hydro_output.$(OBJEXT)
coeff.$(OBJEXT): modules.$(OBJEXT) 
file_manager.$(OBJEXT): modules.$(OBJEXT) 
flow_sim.$(OBJEXT): modules.$(OBJEXT)  bctable.$(OBJEXT)
gage_output.$(OBJEXT): 	modules.$(OBJEXT) scalars_module.$(OBJEXT) met_data_module.$(OBJEXT) accumulator.$(OBJEXT) hydro_output.$(OBJEXT)

hydro_output.$(OBJEXT): modules.$(OBJEXT) 
initial_cond.$(OBJEXT): modules.$(OBJEXT) scalars_module.$(OBJEXT)
kick_off.$(OBJEXT): modules.$(OBJEXT) pidlink.$(OBJEXT)
latflow_bc.$(OBJEXT): modules.$(OBJEXT) 
link_bc.$(OBJEXT): modules.$(OBJEXT) bctable.$(OBJEXT)
link_data.$(OBJEXT): modules.$(OBJEXT) 
mass1.$(OBJEXT): modules.$(OBJEXT) scalars_module.$(OBJEXT)  met_data_module.$(OBJEXT) accumulator.$(OBJEXT) profile_output.$(OBJEXT)
met_data_module.$(OBJEXT): modules.$(OBJEXT) 
nonfluvial_coeff.$(OBJEXT): modules.$(OBJEXT) pidlink.$(OBJEXT) 
pidlink.$(OBJEXT): modules.$(OBJEXT) bctable.$(OBJEXT)
point_data.$(OBJEXT): modules.$(OBJEXT) 
print_output.$(OBJEXT): modules.$(OBJEXT) scalars_module.$(OBJEXT)  met_data_module.$(OBJEXT) 
profile_output.$(OBJEXT): modules.$(OBJEXT)  scalars_module.$(OBJEXT)  met_data_module.$(OBJEXT) accumulator.$(OBJEXT) 
read_config.$(OBJEXT): modules.$(OBJEXT)
read_hotstart.$(OBJEXT):  modules.$(OBJEXT) scalars_module.$(OBJEXT)
scalars_module.$(OBJEXT): modules.$(OBJEXT) bctable.$(OBJEXT) met_data_module.$(OBJEXT) tdg_equation_coeff.$(OBJEXT)  hydro_output.$(OBJEXT) 
section.$(OBJEXT): modules.$(OBJEXT)
section_data.$(OBJEXT): modules.$(OBJEXT)
temp_sim.$(OBJEXT):  modules.$(OBJEXT)
trans_bc.$(OBJEXT):  modules.$(OBJEXT) bctable.$(OBJEXT)
write_restart.$(OBJEXT): modules.$(OBJEXT) scalars_module.$(OBJEXT)


clean-local:
	rm -f *.$(FC_MODEXT)
	rm -f *.il work.*
	rm -f *~

EXTRA_DIST = \
	TODO \
	makefile.old \
	makefile.gnu \
	makefile.ibm \
	makefile.lahey \
	makefile.linux \
	makefile.sun \
	mass1_v082.dsp \
	mass1_v082.dsw \
	mass1_v082.mak

