quad = quad + 1
quadrm = real(system(sprintf("awk -F'|' '$1 + 0 == %d { print $4 } {next}' quadrm.txt", quad)))
pcmd = sprintf("<awk -F',' '$2 == %d {print $1, $4, $5, $6}' %s", quad, thefile)
thetitle = sprintf("Quandrant %d, RM %.2f", quad, quadrm)

set title thetitle

set output sprintf("q%03d.png", quad)
set ylabel "Discharge, kcfs"
unset y2label
unset y2tics
set ytics mirror
set format y "%.1f"
plot pcmd using 1:($4/1000.0) title thetitle with linesp lt 1, \
     'ts11.out' using 1:($4/1000) title 'Priest Rapids Dam' with lines lt 3
     

set output sprintf("tw%03d.png", quad)
set ylabel "Top Width, m"
unset y2label
unset y2tics
set ytics mirror
set format y "%.1f"
plot pcmd using 1:($5*0.3048) title thetitle with linesp lt 1
     

set y2label "Priest Rapids Elevation, m"
set output sprintf("e%03d.png", quad)
set ylabel "Downstream Elevation, m"
set ytics nomirror
set y2tics nomirror
set format y "%.2f"
set format y2 "%.2f"
plot pcmd using 1:($3*0.3048) axes x1y1 title thetitle with linesp lt 1, \
     'ts11.out' using 1:($3*0.3048) axes x1y2 title 'Priest Rapids Dam' with lines lt 3

set output

if (quad < 360) reread
