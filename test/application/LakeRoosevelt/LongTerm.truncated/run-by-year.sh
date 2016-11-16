#! /bin/sh
# -------------------------------------------------------------
# file: run-by-year.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 30, 2001 by William A. Perkins
# Last Change: 2016-11-16 06:55:24 d3g096
# -------------------------------------------------------------

set -ex

startyr=1976
endyr=2015
modeldir="${MASS1-/net/flophouse/files0/perksoft/linux64/mass1}"
model="${MODEL-${modeldir}/bin/mass1}"
profile2cgns="${modeldir}/bin/profile2cgns.py"
python="${PYTHON-python}"

y=$startyr
lasty=`expr $y - 1`

while [ $y -le $endyr ]; do

    sed -e "s/@YEAR@/$y/g" \
        -e "s/@LASTYEAR@/$lasty/g" mass1-base.cfg > mass1.cfg

                                #  run the model for the year and tag
                                #  the output files

    echo Running $y ...
    time $model > /dev/null

    if [ -d "$y" ]; then
        if [ -d "${y}.old" ]; then
            rm -rf "${y}.old"
        fi
        mv "$y" "${y}.old"
    fi
    mkdir "$y"
    $python $profile2cgns \
        --base "01-01-$y 00:00:00" \
        --end 9999 \
        "profile1.out" "$y/profile1.$y.cgns" "$y/profile1.$y.lst"
    for i in *.out; do
        mv $i $y/${i}.$y
        gzip -f $y/${i}.$y
    done

    lasty="$y"
    y=`expr $y + 1`
done
