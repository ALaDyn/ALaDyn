#!/bin/bash

module purge

module load profile/advanced
module load gnu/6.1.0
module load openmpi/1-10.3--gnu--6.1.0
module load fftw/3.3.4--openmpi--1-10.3--gnu--6.1.0
module load boost/1.61.0--gnu--6.1.0
module load cmake

mkdir -p build ; cd build
cmake .. -DBoost_NO_BOOST_CMAKE=ON
cmake --build . --target install
cd ..
