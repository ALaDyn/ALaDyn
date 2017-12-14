#!/bin/bash 

module purge

module load gcc/6.3.0
module load openmpi/2.0.3
module load boost/1.63
module load fftw

export CC=/opt/gcc/6.3.0/bin/gcc
export CXX=/opt/gcc/6.3.0/bin/g++


mkdir -p build_gcc ; cd build_gcc ;

cmake .. -DBoost_NO_BOOST_CMAKE=ON 
cmake --build . --target install

cd ..

