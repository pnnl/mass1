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
# Created June 22, 2017 by William A. Perkins
# Last Change: 2019-02-22 14:47:28 d3g096
# -------------------------------------------------------------

AM_FFLAGS = $(FCFLAGS)

EXTRA_DIST = \
	README \
	Makefile.noauto \
	fptrap-intel.f90 \
	fptraptest.f90 \
	fptrap-null.f90  \
	fptraptest.dsp \
	fptrap-win32.f90


noinst_LIBRARIES = libts.a

libts_a_SOURCES = \
	time_series.f90 \
	cumulative_time_series.f90 \
	met_time_series.f90 \
	const_series.f90 \
	julian.f90 \
	date_time_module.f90 \
	utility.f90

# libts_a_LIBADD = @F90TRAPOBJ@
# libts_a_DEPENDENCIES = @F90TRAPOBJ@

check_PROGRAMS = \
	datetest1 \
	datetest2 \
	tstest1 \
	tstest2 \
	tstest3 \
	tstest4 \
	cumtstest1 \
	mettest1

datetest1_SOURCES = datetest1.f90
datetest1_LDADD = libts.a 
datetest2_SOURCES = datetest2.f90
datetest2_LDADD = libts.a 
tstest1_SOURCES = tstest1.f90
tstest1_LDADD = libts.a
tstest2_SOURCES = tstest2.f90
tstest2_LDADD = libts.a
tstest3_SOURCES = tstest3.f90
tstest3_LDADD = libts.a
tstest4_SOURCES = tstest4.f90
tstest4_LDADD = libts.a
cumtstest1_SOURCES = cumtstest1.f90
cumtstest1_LDADD = libts.a
mettest1_SOURCES = mettest1.f90
mettest1_LDADD = libts.a
# fptraptest_SOURCES = fptraptest.f90
# fptraptest_LDADD = libts.a

# dependancies for individual object files

tstest1.$(OBJEXT): tstest1.f90 time_series.$(OBJEXT)
tstest2.$(OBJEXT): tstest2.f90 time_series.$(OBJEXT)
tstest3.$(OBJEXT): tstest3.f90 time_series.$(OBJEXT)
datetime1.$(OBJEXT): datetime1.f90 date_time_module.$(OBJEXT) utility.$(OBJEXT)
time_series.$(OBJEXT): time_series.f90 date_time_module.$(OBJEXT) utility.$(OBJEXT)
cumulative_time_series.$(OBJEXT): cumulative_time_series.f90 time_series.$(OBJEXT)
cumtstest1.$(OBJEXT): cumtstest1.f90 cumulative_time_series.$(OBJEXT)
met_time_series.$(OBJEXT): met_time_series.f90 time_series.$(OBJEXT)
const_series.$(OBJEXT): const_series.f90 time_series.$(OBJEXT)
mettest1.$(OBJECT): mettest1.f90 met_time_series.$(OBJEXT)
date_time_module.$(OBJEXT): date_time_module.f90 julian.$(OBJEXT)
julian.$(OBJEXT): julian.f90
utility.$(OBJEXT): utility.f90
fptrap-null.$(OBJEXT): fptrap-null.f90 utility.$(OBJEXT)
fptrap-intel.$(OBJEXT): fptrap-intel.f90 utility.$(OBJEXT)

clean-local:
	rm -f *.$(FC_MODEXT)
	rm -f fort.*
	rm -f *~
