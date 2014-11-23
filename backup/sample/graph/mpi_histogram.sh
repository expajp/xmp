#!/bin/sh

rm -f mpi_histo.eps

gnuplot<<EOF
set xlabel "Parallel"
set ylabel "Time(s)"
set style fill solid
set border lc rgb "black"
set boxwidth 0.8 relative
set xtics rotate by -90
set terminal postscript enhanced color
set output "mpi_histo.eps"
plot "mpi_histo.dat" using 0:($$2+$$3+$$4)   with boxes lw 2 lc rgb "light-pink"  title "Initialization",\
     "mpi_histo.dat" using 0:($$2+$$3)      with boxes lw 2 lc rgb "light-green" title "Communication",\
     "mpi_histo.dat" using 0:($$2):xtic(1) with boxes lw 2 lc rgb "light-cyan"  title "Calculation"
exit
EOF

echo "\"mpi_histo.eps\" was output successfully."
display mpi_histo.eps