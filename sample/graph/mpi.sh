#!/bin/sh

rm -f mpi.eps

gnuplot<<EOF
set title "Machine time for Parallel Computing by MPI"
set xlabel "Parallel"
set ylabel "Time(s)"
set xrange[0:17]
set yrange[0:0.6]
set xtics 1
set ytics 0.05
set terminal postscript enhanced color
set output "mpi.eps"
plot "mpi.dat" w lp t "MPI"
exit
EOF

echo "\"mpi.eps\" was output successfully."
display mpi.eps