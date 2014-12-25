#!/bin/sh

rm -f mpi.eps

gnuplot<<EOF
set xlabel "Parallel"
set ylabel "Time[s]"
set logscale x
set xrange[1.6:18]
set yrange[0:0.013]
set xtics 2, 2, 16
set ytics 0.001
set terminal postscript enhanced color
set output "mpi.eps"
plot "mpi.dat" using 1:4 w lp t "calc" lc rgb "blue", "mpi.dat" using 1:3 w lp t "comm" lc rgb "green"
exit
EOF

echo "\"mpi.eps\" was output successfully."
display mpi.eps