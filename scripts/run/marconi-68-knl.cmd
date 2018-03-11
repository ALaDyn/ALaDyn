#!/bin/bash
#SBATCH --account=IscrB_account    # put the name of your project
#SBATCH --time=10:00               # 10 minutes
#SBATCH -N1 -n68                   # 1 node, 68 tasks
#SBATCH --error  job.err
#SBATCH --output job.out
#SBATCH --partition=knl_usr_prod

module purge

module load env-knl
module load intel
module load intelmpi
module load boost
module load mkl

mpirun ./ALaDyn >> opic.txt 2>> epic.txt
