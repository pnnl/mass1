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
# Created June 22, 2005 by William A. Perkins
# Last Change: 2017-08-24 09:37:27 d3g096
# -------------------------------------------------------------
# $Id$

SUBDIRS = 

if CONFIG_PERL

do_subst = sed -e 's,[@]PERL[@],$(PERL_SHEBANG),g' \
            -e 's,[@]PACKAGE[@],$(PACKAGE),g' \
            -e 's,[@]VERSION[@],$(VERSION),g'

if CONFIG_PERL_DATEMANIP

perl_DateManip_scripts = mass1bc mass1profile
perl_DateManip_mans = mass1bc.1 mass1profile.1

mass1bc: mass1bc.in Makefile
	$(do_subst) < $(srcdir)/mass1bc.in > $@
	chmod +x $@

mass1bc.1: mass1bc
	$(POD2MAN) $< $@

mass1profile: mass1profile.in Makefile
	$(do_subst) < $(srcdir)/mass1profile.in > $@
	chmod +x $@

mass1profile.1: mass1profile
	$(POD2MAN) $< $@

endif

perl_scripts = \
	$(perl_DateManip_scripts) 
perl_mans = \
	$(perl_DateManip_mans)

endif

bin_SCRIPTS = \
	$(perl_scripts)

man_MANS = \
	$(perl_mans)

scriptdir = $(datadir)/$(PACKAGE)/scripts
script_DATA = 	\
	README.md \
	profile2cgns.py \
	profile_extract.py \
	profile_tecplot.py \
	xy-looper.mcr \
	xy-looper-png.mcr \
	timeshift.py \
	read.R \
	bcsteps.py

xsscriptdir = $(scriptdir)/cross-section
xsscript_DATA =					\
	cross-section/CHARIMAarea.pl		\
	cross-section/CHARIMAextract.pl		\
	cross-section/CHARIMAfill.pl		\
	cross-section/CHARIMAinsert.pl		\
	cross-section/CHARIMAnotch.pl		\
	cross-section/CHARIMArenum.pl		\
	cross-section/CHARIMAthalweg.pl		\
	cross-section/CHARIMAview.pl		\
	cross-section/HEC2area.pl		\
	cross-section/HEC2CHARIMA.pl		\
	cross-section/HECRASCHARIMA.pl		\
	cross-section/README.md			\
	cross-section/section-fix.awk		\
	cross-section/section-unfix.awk		\
	cross-section/XSection.pm		\
	cross-section/XSECTprops.pl



CLEANFILES = $(perl_scripts) $(man_MANS)
EXTRA_DIST = $(man_MANS)
