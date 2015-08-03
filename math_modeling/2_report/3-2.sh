#!/bin/sh

for i in 0 25 50 100
do
t=`echo "scale=2; i/100" | bc`
echo ${t}

gnuplot<<EOF
set xl "x";set yl "y";set zl "u"

set terminal postscript enhanced color
set output "3-2-${i}.eps"
splot "3-2-${i}.out" using 2:3:4 title "t=${t}" with line

set terminal png
set output "3-2-${i}.png"
replot

exit
EOF
done

echo "done."