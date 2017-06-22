# -------------------------------------------------------------
# file: section-unfix.awk

# Converts a CHARIMA file from the "new" format, used by MASS1, back
# to the original CHARIMA format

# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October  1, 1999 by William A. Perkins
# Last Change: 2017-06-22 11:42:06 d3g096
# -------------------------------------------------------------

/\/ *$/ { 
    sub(/\//, "");
    print;
    print " -1.0"; 
    next;
}
{ 
    print; 
}
