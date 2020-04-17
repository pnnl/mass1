
set xdata time
set timefmt "%m-%d-%Y %H:%M:%S

cmd = "< awk 'BEGIN {sum = 0.0}{print $1, $2, sum; sum = sum + $9}' %s"

plot sprintf(cmd, "ts11.out") using 1:3 title "Inlet" w lines, \
     sprintf(cmd, "ts12.out") using 1:3 title "Outlet" w lines

