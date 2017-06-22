# -------------------------------------------------------------
# file: section-fix.awk
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 16, 1997 by William A. Perkins
# Last Change: 2017-06-22 11:41:57 d3g096
# -------------------------------------------------------------

BEGIN {
  first = 1;
}

NF >= 4 && $0 ~ /[A-z][A-z]*/, $1 + 0 == -1 {
  if ($1 + 0 == -1) {
    printf(" /\n");
    first = 1;
  } else {
    if (!first) printf("\n");
    first = 0;
    printf("%s", $0);
  }
}
  
