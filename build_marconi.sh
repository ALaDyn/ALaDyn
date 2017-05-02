module purge

module load intel
module load intelmpi
module load boost
module load fftw
#module load mkl
module load cmake

#using fftw
mkdir build_ifort ; cd build_ifort ; cmake .. -DOTHER_INCLUDE_DIR=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/mkl/include/fftw/ -DCMAKE_C_COMPILER=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/bin/icc -DCMAKE_CXX_COMPILER=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/bin/icpc -DCMAKE_Fortran_COMPILER=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/bin/ifort ; cmake --build . --target install ; cd ..

#using mkl
#mkdir build_ifort ; cd build_ifort ; cmake .. -DCMAKE_LINKER=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/bin/icpc -DOTHER_INCLUDE_DIR=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/mkl/include/fftw/ -DBoost_NO_BOOST_CMAKE=ON -DCMAKE_C_COMPILER=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/bin/icc -DCMAKE_CXX_COMPILER=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/bin/icpc -DCMAKE_Fortran_COMPILER=/cineca/prod/opt/compilers/intel/pe-xe-2017/binary/bin/ifort ; cmake --build . --target install ; cd ..

