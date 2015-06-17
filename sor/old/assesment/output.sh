#!/bin/sh

# $1:元データ
# $2:出力ファイル
# $3:グラフの線の方のタイトル

rm -f $2.eps $2.png
LINES=`wc -l <$1`
RANGE=`expr \( ${LINES} / 100 + 1 \) \* 100`
echo "LINES = ${LINES}, RANGE = ${RANGE}"

gnuplot<<EOF
set xlabel "Number of occurrence"
set ylabel "Residual error"
set xrange[0:${RANGE}]
set yrange[5.0e-09:1]
set logscale y
set terminal postscript enhanced color
set output "$2.eps"
plot "$1" w l t "$3"
set terminal png
set output "$2.png"
replot
exit
EOF

display $2.eps