#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=small"
#PJM -L "node=2"
#PJM -L "elapse=10:00"
#PJM -j

mpiexec -np 2 ./miflow
