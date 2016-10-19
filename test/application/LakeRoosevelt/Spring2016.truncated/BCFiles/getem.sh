#!/bin/sh

set -xue

start="01/01/2016"
end="01/01/2017"
massbc="perl /home/d3g096/do9/database/massbc.pl"

$massbc -Q -O "-30 minute" -o GCL-Discharge.dat GCL "$start" "$end"
$massbc -g -h -Q -0 -o Border-Discharge.dat 12399500 "$start" "$end"
$massbc -g -h -Q -0 -o Kettle-Discharge.dat 12404900 "$start" "$end"
$massbc -g -Q -O "12 hour" -o Kettle-Discharge-daily.dat 12404500 "$start" "$end"
$massbc -g -h -Q -0 -o Colville-Discharge.dat 12409000 "$start" "$end"
$massbc -g -Q -O "12 hour" -o Colville-Discharge-daily.dat 12409000 "$start" "$end"
$massbc -g -h -Q -0 -o Sanpoil-Discharge.dat 12434590 "$start" "$end"
$massbc -g -Q -O "12 hour" -o Spokane-Discharge.dat 12433000 "$start" "$end"

# Provisional data not available 
# This is actually the reverse: negative should be withdrawl via pump to Banks Lake.  
# $massbc -g -Q -O "12 hour" -o 12435500 BanksCanal-Discharge.dat 12435500 "$start" "$end"

# Shift elevation datum for Grand Coulee forebay stage
# $massbc -F -0 -o GCL-Elev-NGVD29.dat GCL "$start" "$end"
awk -f - GCL-Elev-NGVD29.dat > GCL-Elev.dat <<EOF
NR == 1 { print; next; }
{ printf("%s %s %.3f / %.3f\n", \$1, \$2, \$3 + 2.502, \$3 + 0.0); }
EOF

$massbc -q -0 -o CIBW-Temperature.dat CIBW "$start" "$end"


python2.7 gen-temp.py --output Kettle-Temperature.dat  \
    --title "Generated Kettle River Temperature" \
    10.4 11.3 169 8.5 171 305

python2.7 gen-temp.py --output Colville-Temperature.dat  \
    --title "Generated Colville River Temperature" \
    10.3  9.8 169.5 3.4 145 304

python2.7 gen-temp.py --output Spokane-Temperature.dat  \
    --title "Generated Spokane River Temperature" \
    11.2  8.3  151.9  2  138  311

python2.7 gen-temp.py --output Sanpoil-Temperature.dat  \
    --title "Generated Sanpoil River Temperature" \
    9.5 9.4 172.3 4.6 145 306

python2.7 gen-temp.py --output BanksCanal-Temperature.dat  \
    --title "Generated Banks Lake Canal Temperature" \
    10.9 8.3 130 2.2 217 342
