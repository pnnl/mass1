# -------------------------------------------------------------
# file: Makefile.am
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June 22, 2005 by William A. Perkins
# Last Change: Thu Jun 23 07:54:19 2005 by William A. Perkins <perk@McPerk.local>
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

CLEANFILES = $(bin_SCRIPTS) $(man_MANS)
EXTRA_DIST = $(man_MANS)
