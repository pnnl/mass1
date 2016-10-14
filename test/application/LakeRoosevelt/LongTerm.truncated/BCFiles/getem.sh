#!/bin/sh

set -xue

start="01/01/1976"
end="01/01/2017"
massbc="perl /home/d3g096/do9/database/massbc.pl"

# Shift elevation datum for Grand Coulee forebay stage
$massbc -F -0 -o GCL-Elev-NGVD29.dat GCL "$start" "$end"
awk -f - GCL-Elev-NGVD29.dat > GCL-Elev.dat <<EOF
NR == 1 { print; next; }
{ printf("%s %s %.3f / %.3f\n", \$1, \$2, \$3 + 2.502, \$3 + 0.0); }
EOF

$massbc -Q -O "-30 minute" -o GCL-Discharge.dat GCL "$start" "$end"
$massbc -g -h -Q -0 -o Border-Discharge.dat 12399500 "$start" "$end"
$massbc -g -Q -O "-12 hour" -o Border-Discharge-daily.dat 12399500 "$start" "$end"
$massbc -g -Q -O "-12 hour" -o Kettle-Discharge-daily.dat 12404500 "$start" "$end"
$massbc -g -Q -O "-12 hour" -o Colville-Discharge-daily.dat 12409000 "$start" "$end"
$massbc -g -Q -O "-12 hour" -o Sanpoil-Discharge.dat 12434590 "$start" "$end"
$massbc -g -Q -O "-12 hour" -o Spokane-Discharge.dat 12433000 "$start" "$end"

# Provisional data not available 
# This is actually the reverse: negative should be withdrawl via pump to Banks Lake.  
$massbc -g -Q -O "-12 hour" -o 12435500 -x -1.0 -o BanksCanal-Discharge.dat 12435500 "$start" "$end"

