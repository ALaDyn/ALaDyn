module purge

module load compilers/gcc-4.9.0
module load compilers/intel-parallel-studio-2017
module load boost_1_56_0_gcc4_9_0

mkdir build_ifort ; cd build_ifort ; cmake .. -DCMAKE_LINKER=/shared/software/compilers/intel_2017/compilers_and_libraries_2017.0.098/linux/bin/intel64/icpc -DOTHER_INCLUDE_DIR=/shared/software/compilers/intel_2017/compilers_and_libraries/linux/mkl/include/fftw/ -DBoost_NO_BOOST_CMAKE=ON -DCMAKE_C_COMPILER=/shared/software/compilers/intel_2017/compilers_and_libraries_2017.0.098/linux/bin/intel64/icc -DCMAKE_CXX_COMPILER=/shared/software/compilers/intel_2017/compilers_and_libraries_2017.0.098/linux/bin/intel64/icpc -DCMAKE_Fortran_COMPILER=/shared/software/compilers/intel_2017/compilers_and_libraries_2017.0.098/linux/bin/intel64/ifort ; cmake --build . --target install ; cd ..

