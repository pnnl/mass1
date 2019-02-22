#! /bin/sh
# -------------------------------------------------------------
# file: bootstrap.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 26, 2003 by William A. Perkins
# Last Change: 2019-02-22 14:29:22 d3g096
# -------------------------------------------------------------

aclocal -I . && automake --foreign --add-missing --copy && autoconf
