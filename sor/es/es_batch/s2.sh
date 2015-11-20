#!/bin/sh
#PBS -q S
#PBS -b 2
#PBS -l elapstim_req=00:10:00
#PBS -v MPIPROGINF=ALL_DETAIL

cd /S/data00/G5055/y0405
mpirun -nnp 1 -nn 2 -np 2 ./a.out
