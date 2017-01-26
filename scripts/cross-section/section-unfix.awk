# -------------------------------------------------------------
# file: section-unfix.awk

# Converts a CHARIMA file from the "new" format, used by MASS1, back
# to the original CHARIMA format

# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October  1, 1999 by William A. Perkins
# Last Change: Fri Oct  1 13:20:57 1999 by William A. Perkins <perk@erebus.pnl.gov>
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
