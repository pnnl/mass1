#! /bin/sh
# -------------------------------------------------------------
# file: getbc.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November 19, 1999 by William A. Perkins
# Last Change: Fri Nov 19 11:00:32 1999 by William A. Perkins <perk@erebus.pnl.gov>
# -------------------------------------------------------------

start="01/01/1997"
end="01/01/1999"

script="/home/perk/do9/database/massbc.pl"

                                # dam operations

$script -F -o BON-FBE.dat BON $start $end
$script -Q -o BON-Qtotal.dat BON $start $end
$script -Q -o TDA-Qtotal.dat TDA $start $end

                                # trib flows

$script -g -Q -o Klickitat-Flow.dat 14113000 $start $end
$script -g -Q -o Hood-Flow.dat 14120000 $start $end
$script -g -Q -o WhiteSalmon-Flow.dat 14123500 $start $end
