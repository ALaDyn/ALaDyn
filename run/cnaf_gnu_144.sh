#!/bin/bash


TOTAL_NUMBER_OF_CORES=144

EXECUTABLE="./ALaDyn"
stderr_file=epic.txt
stdout_file=opic.txt

job=job_test_gcc.cmd
job_name=ompi
queue=hpc_inf

###########################
rm -f $job
touch $job
chmod 755 $job

touch ${stderr_file}
touch ${stdout_file}

echo "#BSUB -J ${job_name}" > $job # Job name
echo "#BSUB -o %J.out" >> $job # Job standard output
echo "#BSUB -e %J.err" >> $job # Job standard error
echo "#BSUB -q ${queue}" >> $job
echo "#BSUB -a openmpi" >> $job
echo "#BSUB -n ${TOTAL_NUMBER_OF_CORES}" >> $job
echo "module purge" >> $job
echo "module load compilers/gcc-4.9.0" >> $job
echo "module load compilers/openmpi-1.8.1_gcc-4.9.0_with_cuda6.5" >> $job
echo "module load boost_1_56_0_gcc4_9_0" >> $job
echo "/usr/share/lsf/9.1/linux2.6-glibc2.3-x86_64/bin/mpirun.lsf env PSM_SHAREDCONTEXTS_MAX=8 ${EXECUTABLE} >> ${stdout_file} 2>> ${stderr_file}" >> $job


echo "Please submit the job with the following command:"
echo "bsub < $job"


