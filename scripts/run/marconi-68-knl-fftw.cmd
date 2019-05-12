#!/bin/bash
#SBATCH --account=IscrB_account    # put the name of your project
#SBATCH --time=10:00               # 10 minutes
#SBATCH -N1 -n68                   # 1 node, 68 tasks
#SBATCH --error  job.err
#SBATCH --output job.out
#SBATCH --partition=knl_usr_prod

module purge

module load profile/base
module load profile/knl
module load env-knl
module load intel/pe-xe-2018--binary
module load intelmpi/2018--binary
module load boost/1.66.0--intelmpi--2018--binary
module load fftw/3.3.7_knl--intelmpi--2018--binary

mpirun ./ALaDyn >> opic.txt 2>> epic.txt
