#!/bin/sh

#$1で指定されたファイルをmpisxでコンパイルして、data領域に移す
rm -f /S/data00/G5055/y0405/a.out 2>/dev/null
xmpf90 -C hopt -I /SX/usr/include/f03/ $1
cp a.out /S/data00/G5055/y0405/
