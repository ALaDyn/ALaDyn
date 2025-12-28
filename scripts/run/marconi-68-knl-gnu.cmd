#!/bin/bash
#SBATCH --account=IscrB_account    # put the name of your project
#SBATCH --time=10:00               # 10 minutes
#SBATCH -N1 -n68                   # 1 node, 68 tasks
#SBATCH --error  job.err
#SBATCH --output job.out
#SBATCH --partition=knl_usr_prod

module purge

module load env-knl
module load profile/global
module load gnu/6.1.0
module load openmpi/1-10.3--gnu--6.1.0
module load fftw/3.3.4--openmpi--1-10.3--gnu--6.1.0
module load boost/1.61.0--gnu--6.1.0
module load cmake

mpirun ./ALaDyn >> opic.txt 2>> epic.txt
