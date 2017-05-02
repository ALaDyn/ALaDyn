module purge

module load profile/advanced
module load gnu/4.9.2
module load openmpi/1.8.4--gnu--4.9.2
module load fftw/3.3.4--openmpi--1.8.4--gnu--4.9.2
module load boost/1.58.0--openmpi--1.8.4--gnu--4.9.2

make galileo_gnu_debug
