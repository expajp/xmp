#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=large"
#PJM -L "node=16"
#PJM -L "elapse=10:00"
#PJM -j

mpirun -np 16 ./miflow