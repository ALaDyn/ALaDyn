#!/bin/bash

module swap PrgEnv-${PE_ENV,,} PrgEnv-gnu
module swap craype-${CRAY_CPU_TARGET} craype-haswell
module load boost
module load cray-fftw
module load cmake

export FC=ftn
export CLINKER=$CXX

mkdir -p build ; cd build
cmake .. -DCORI_HASWELL:BOOL=TRUE -DCMAKE_LINKER=$CLINKER -DBoost_NO_BOOST_CMAKE=ON -DCMAKE_Fortran_COMPILER=$FC
cmake --build . --target install -- -j8
cd ..
