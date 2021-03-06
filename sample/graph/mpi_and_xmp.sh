#!/bin/sh

rm -f mpi_and_xmp.eps

gnuplot<<EOF
set title ""
set xlabel "Parallel"
set ylabel "Time(s)"
set xrange[0:17]
set yrange[0:2.1]
set xtics 1
set ytics 0.1
set terminal postscript enhanced color
set output "mpi_and_xmp.eps"
plot "mpi.dat" w lp t "MPI" lt 1 lc rgb "red", "xmp.dat" w lp t "XcalableMP" lt 1 lc rgb "blue"
exit
EOF

echo "\"mpi_and_xmp.eps\" was output successfully."
display mpi_and_xmp.eps