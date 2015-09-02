#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=small"
#PJM -L "node=4"
#PJM -L "elapse=10:00"
#PJM -j

mpirun -np 4 ./miflow