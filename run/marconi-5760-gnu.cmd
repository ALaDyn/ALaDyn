#!/bin/bash
#PBS -A my_cineca_computing_account
#PBS -l walltime=24:0:00
#PBS -l select=160:ncpus=36:mpiprocs=36:mem=120GB
#PBS -j eo

##PBS -o opic.txt   #commented to have output in real-time via redirection and not all at the end of the job
##PBS -e epic.txt


cd ${PBS_O_WORKDIR}

module purge

module load profile/advanced
module load gnu/6.1.0
module load openmpi/1-10.3--gnu--6.1.0
module load boost/1.61.0--gnu--6.1.0
module load fftw/3.3.4--openmpi--1-10.3--gnu--6.1.0


## export FOR_PRINT="./opic.txt"
## mpirun ./ALaDyn

mpirun ./ALaDyn >> opic.txt 2>> epic.txt


