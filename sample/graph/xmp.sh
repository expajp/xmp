#!/bin/sh

rm -f xmp.eps

gnuplot<<EOF
set title "XcalableMP"
set xlabel "Parallel"
set ylabel "Time(s)"
set xrange[0:17]
set yrange[0:1]
set xtics 1
set ytics 0.1
set terminal postscript enhanced color
set output "xmp.eps"
plot "xmpplot.dat" w lp t "XcalableMP"
exit
EOF

echo "\"xmp.eps\" was output successfully."
display xmp.eps