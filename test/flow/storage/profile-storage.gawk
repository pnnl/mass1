# -------------------------------------------------------------
# file: profile-storage.gawk
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 13, 1999 by William A. Perkins
# Last Change: Tue Jul 13 09:02:26 1999 by William A. Perkins <perk@hazel.pnl.gov>
# -------------------------------------------------------------


BEGIN { 
  storage = 0.0;
  first = 1;
  width = 100.0
}

                                # print out the total storage before
                                # we get a new date

/Date:/ && !first {
  printf("%s %.1f\n", date, storage);
}

                                # extract the date

/Date:/ {
  date = $7 " " $9;
  first = 0;
  storage = 0;
  lastx = 0;
  lasty = 0;
  next;
}

$1 ~ /^1$/ {
  x = $4;
  y = $5;
  storage += (x - lastx)*width*(y + lasty)/2.0;
}

{ next; }

END {
  printf("%s %.1f\n", date, storage);
}
