#! /bin/sh
# -------------------------------------------------------------
# file: bootstrap.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 26, 2003 by William A. Perkins
# Last Change: Thu Jun  9 13:41:29 2005 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------
# $Id$

aclocal -I time_series && automake --foreign --add-missing --copy && autoconf

(cd time_series; sh bootstrap.sh)