#!/bin/sh

rm -f mpi.eps

gnuplot<<EOF
set title "MPI"
set xlabel "Parallel"
set ylabel "Time(s)"
set xrange[0:17]
set yrange[0:1]
set xtics 1
set ytics 0.1
set terminal postscript enhanced color
set output "mpi.eps"
plot "mpiplot.dat" w lp t "MPI"
exit
EOF

echo "\"mpi.eps\" was output successfully."
display mpi.eps