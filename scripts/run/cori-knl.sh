#!/bin/bash

#SBATCH --qos=debug
#SBATCH --time=0:05:00
#SBATCH -N1 -n64
#SBATCH --constraint=knl
#SBATCH -J test_job
#SBATCH --error job.err
#SBATCH --output job.out

module load PrgEnv-intel
module swap craype-${CRAY_CPU_TARGET} craype-mic-knl
module load boost

srun -n 64 /global/homes/t/terzani/Pic/forked_ALaDyn/bin/ALaDyn >> opic.txt 2>> epic.txt

