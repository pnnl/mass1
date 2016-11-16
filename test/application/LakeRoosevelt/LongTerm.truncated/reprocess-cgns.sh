#!/bin/sh
# -------------------------------------------------------------
# file: reprocess-cgns.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 16, 2016 by William A. Perkins
# Last Change: 2016-11-16 07:52:24 d3g096
# -------------------------------------------------------------
set -uex

startyr=1976
endyr=2015
modeldir="${MASS1-/net/flophouse/files0/perksoft/linux64/mass1}"
profile2cgns="${modeldir}/bin/profile2cgns.py"
python="${PYTHON-python}"

y=$startyr
lasty=`expr $y - 1`

while [ $y -le $endyr ]; do

    if [ -d "$y" ]; then
        prof="$y/profile1.out.$y"
        gunzip "${prof}.gz"
        $python $profile2cgns \
            --base "01-01-$y 00:00:00" \
            --end 9999 \
            "$prof" "$y/profile1.$y.cgns" "$y/profile1.$y.lst"
        gzip "${prof}"
    fi
    lasty="$y"
    y=`expr $y + 1`
done
