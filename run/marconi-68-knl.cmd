#!/bin/bash
#PBS -A my_cineca_computing_account
#PBS -l walltime=0:10:00
#PBS -l select=1:ncpus=68:mpiprocs=68:mem=108GB:mcdram=flat:numa=snc2
#PBS -j eo

##PBS -o opic.txt   #commented to have output in real-time via redirection and not all at the end of the job
##PBS -e epic.txt


cd ${PBS_O_WORKDIR}

module purge

module load env-knl
module load intel
module load intelmpi
module load boost
#module load gnu

mpirun ./ALaDyn >> opic.txt 2>> epic.txt
