#!/bin/sh

rm -f calc.eps

gnuplot<<EOF
set xlabel "Parallel"
set ylabel "Time[s]"
set logscale x
set xrange[1.6:18]
set yrange[0:0.011]
set xtics 2, 2, 16
set ytics 0.001
set terminal postscript enhanced color
set output "calc.eps"
plot "calc.dat" using 1:2 w lp t "MPI" lc rgb "brown", "calc.dat" using 1:3 w lp t "XMP" lc rgb "navy"
exit
EOF

echo "\"calc.eps\" was output successfully."
display calc.eps