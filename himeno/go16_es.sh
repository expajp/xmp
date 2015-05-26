#!/bin/sh
#PBS -q S
#PBS -b 4
#PBS -l memsz_job=4gb
#PBS -l elapstim_req=00:10:00

#PBS -v MPIPROGINF=ALL_DETAIL

cd /S/data00/G5055/y0405
mpirun -nnp 4 -nn 4 -np 16 ./a.out