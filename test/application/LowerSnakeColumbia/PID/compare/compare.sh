#! /bin/sh
# -------------------------------------------------------------
# file: compare.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 25, 2001 by William A. Perkins
# Last Change: Fri Jul 15 13:54:30 2005 by William A. Perkins <perk@McPerk.pnl.gov>
# -------------------------------------------------------------

set -x

# -------------------------------------------------------------
# damname
# -------------------------------------------------------------
damname() {
    case $1 in
        GCL) name="Grand Coulee" ;;
        CHJ) name="Chief Joseph" ;;
        WEL) name="Wells" ;;
        RRH) name="Rocky Reach" ;;
        RIS) name="Rock Island" ;;
        WAN) name="Wanapum" ;;
        PRD) name="Priest Rapids" ;;
        LWG) name="Lower Granite" ;;
        LGS) name="Little Goose" ;;
        LMN) name="Lower Monumental" ;;
        IHR) name="Ice Harbor" ;;
        MCN) name="McNary" ;;
        JDA) name="John Day" ;;
        TDA) name="The Dalles" ;;
        BON) name="Bonneville" ;;
        100-B) name="100-B Area" ;;
        100-N) name="100-N Area" ;;
        100-D) name="100-D Area" ;;
        100-H) name="100-H Area" ;;
        100-F) name="100-F Area" ;;
        300) name="300 Area" ;;
        *)   name="Unknown"
    esac
    echo $name
}

# -------------------------------------------------------------
# fbloc
# return the time series number for the forebay of the specified
# project
# -------------------------------------------------------------
fbloc() {
    case $1 in
        LWG) loc="865" ;;
        LGS) loc="1085" ;;
        LMN) loc="1637" ;;
        IHR) loc="1875" ;;
        MCN) loc="2345" ;;
        JDA) loc="298" ;;
        TDA) loc="3328" ;;
        BON) loc="4317" ;;
        *)   loc="0000"
    esac
    echo $loc
}

# -------------------------------------------------------------
# twloc
# return the time series number for the tailwater of the specified
# project or location code
# -------------------------------------------------------------
twloc() {
    case $1 in
        PRD) loc="331" ;;
        LWG) loc="101" ;;
        LGS) loc="121" ;;
        LMN) loc="181" ;;
        IHR) loc="201" ;;
        MCN) loc="251" ;;
        JDA) loc="311" ;;
        TDA) loc="351" ;;
        BON) loc="451" ;;
        # 100-B) loc="3351" ;;
        # 100-N) loc="3363" ;;
        # 100-D) loc="3367" ;;
        # 100-H) loc="3382" ;;
        # 100-F) loc="3393" ;;
        # 300) loc="33150" ;;
        *)   loc="0000"
    esac
    echo $loc
}

# -------------------------------------------------------------
# fmsloc
# -------------------------------------------------------------
fmsloc() {
    case $1 in
        PRD) loc="3129" ;;
        PRXW) loc="3318" ;;
        PAQW) loc="3513" ;;
        LWG) loc="784";;
        GCGW) loc="924" ;;
        CHJ) loc="9117" ;;
        CHQW) loc="116" ;;
        WEL) loc="1517" ;;
        WELW) loc="171" ;;
        RRH) loc="2121" ;;
        RRDW) loc="231" ;;
        RIS) loc="2531" ;;
        RIGW) loc="271" ;;
        WAN) loc="2782" ;;
        WANW) loc="2911" ;;
        *)   loc="0000"
    esac
    echo $loc
}

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------

fblist="LWG LGS LMN IHR MCN JDA TDA BON"

R="R --no-save --slave"
# R="R --no-save"

bcdir="../../BCFiles"
obsdir="../../Observed"

dotbl=""

                                # need this for R so it wont use
                                # daylight savings time
TZ="UTC"
export TZ

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------
statlist=""

dir=".."
statfile="$dir/statistics.dat"

if [ -f $statfile ]; then
    mv -f $statfile ${statfile}.old
fi
cp /dev/null $statfile

statlist="$statlist $statfile"

for code in $fblist; do
    loc=`fbloc $code`
    name=""
    mname=`damname $code`
    
    sed -e "s;@SIMFILE@;$dir/ts${loc}.out;g" \
        -e "s;@OBSFILE@;$bcdir/Stage/${code}-FBE.dat;g" \
        -e "s;@VAR@;wselev;g" \
        -e "s;@STATSFILE@;$statfile;g" \
        -e "s;@OUTPS@;$dir/$code-scatter-FBE.eps;g" \
        -e "s;@DATA@;\"Forebay Stage, feet\";g" \
        -e "s;@TAG@;$code FBE;g" \
        -e "s;@NAME@;$name;g" \
        -e "s;@FACTOR@;1.0;g" compare.R  > tmp.R 
    $R < tmp.R
    
    sed -e "s;@SIMFILE@;$dir/ts${loc}.out;g" \
        -e "s;@OBSFILE@;$bcdir/Flow/${code}-Qtotal.dat;g" \
        -e "s;@VAR@;discharge;g" \
        -e "s;@STATSFILE@;$statfile;g" \
        -e "s;@OUTPS@;$dir/$code-scatter-QTL.eps;g" \
        -e "s;@DATA@;\"Discharge, cfs\";g" \
        -e "s;@TAG@;$code QTL;g" \
        -e "s;@NAME@;$name;g" \
        -e "s;offset=0;offset=0.5/24;g"  \
        -e "s;@FACTOR@;1.0;g" compare.R  > tmp.R 
    $R < tmp.R
    
done

#perl fbtable.pl $statlist > fbstats.tex
