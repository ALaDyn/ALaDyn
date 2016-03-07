FC          = mpif90
CC          = mpicxx
OPTFC       = -fdefault-real-8 -O3
OPTCC       = -O3
SRC_FOLDER  = src
OBJ_FOLDER  = .
EXE_FOLDER  = bin
EXE         = ALaDyn
FFTW        = -lfftw3
MATH_LIB    = -lm
FFTW_LIB    = /usr/lib/
FFTW_INC    = /usr/include/
FFTW_DEF    = -DUSE_OLD_FFTW_INTERFACE
BOOST_LIB   = /usr/lib/
BOOST_INC   = /usr/include/
BOOST_FS    = -lboost_filesystem
BOOST_S     = -lboost_system
OTHER_LIB   = .
STDCPP_LINK = -lstdc++

FILES       = ALaDyn.F90 \
              all_param.f90 \
              code_util.f90 \
              control_bunch_input.f90 \
              cpp_folder_tree.cpp \
              der_lib.f90 \
              fft_lib.f90 \
              fftg_param.f90 \
              fstruct_data.f90 \
              grid_and_particles.f90 \
              grid_fields.f90 \
              grid_param.f90 \
              ionize.f90 \
              mpi_var.f90 \
              parallel.F90 \
              particles.f90 \
              pdf_moments.f90 \
              phys_param.f90 \
              pic_dump.f90 \
              pic_evolve_in_time.f90 \
              pic_in.f90 \
              pic_out.f90 \
              pic_rutil.f90 \
              precision_def.F90 \
              pstruct_data.f90 \
              pwfa_bunch_field_calculation.f90 \
              pwfa_output_addons.f90 \
              read_input.f90 \
              struct_def.f90 \
              system_utilities.f90 \
              util.f90


SOURCES     = $(addprefix $(SRC_FOLDER), $(FILES))
OBJECTS     = $(addsuffix .o, $(addprefix $(OBJ_FOLDER)/, $(basename $(FILES))))
MODULES     = $(addsuffix .mod, $(addprefix $(OBJ_FOLDER)/, $(basename $(FILES))))
EXECUTABLE  = $(addprefix $(EXE_FOLDER)/, $(EXE))

all: $(OBJECTS) $(MODULES)
	$(FC) $(OPTFC) -L$(FFTW_LIB) -L$(BOOST_LIB) -L$(OTHER_LIB) $(OBJECTS) -o $(EXECUTABLE) $(OTHER_LINKS) $(STDCPP_LINK) $(FFTW) $(BOOST_FS) $(BOOST_S) $(MATH_LIB) $(REDIRECT)

brew: FFTW_LIB = /usr/local/Cellar/fftw/3.3.4_1/lib
brew: FFTW_INC = /usr/local/Cellar/fftw/3.3.4_1/include
brew: BOOST_LIB = /usr/local/Cellar/boost/1.59.0/lib
brew: BOOST_INC = /usr/local/Cellar/boost/1.59.0/include
brew: FFTW_DEF =
brew: all

perf: OPTFC += -march=native
perf: OPTCC += -march=native
perf: all

debug: OPTFC = -fdefault-real-8 -Og -g -Wall -Wextra -fbacktrace -fbounds-check
debug: OPTCC = -Og -g -Wall -Wextra -fbounds-check
debug: all

intel: FC = mpiifort
intel: CC = mpiicpc
intel: OPTFC = -real-size 64 -O3
intel: FFTW =
intel: MATH_LIB = -limf
intel: OTHER_LINKS = -mkl
intel: all

cnaf_intel: intel
cnaf_intel: OPTFC += -vec-report4
cnaf_intel: BOOST_LIB = /shared/software/BOOST/boost_1_56_0/lib
cnaf_intel: BOOST_INC = /shared/software/BOOST/boost_1_56_0/include
cnaf_intel: FFTW_LIB = /shared/software/compilers/intel/compilers_and_libraries/linux/mkl/lib/intel64
cnaf_intel: FFTW_INC = /shared/software/compilers/intel/compilers_and_libraries/linux/mkl/include/fftw
cnaf_intel: REDIRECT = 2>> opt_report.txt
cnaf_intel: all

#cnaf_gnu: OPTFC += -march=core-avx-i
#cnaf_gnu: OPTCC += -march=core-avx-i
cnaf_gnu: BOOST_LIB = /shared/software/BOOST/boost_1_56_0/lib
cnaf_gnu: BOOST_INC = /shared/software/BOOST/boost_1_56_0/include
cnaf_gnu: FFTW_LIB = /shared/software/project/aladyn/fftw/lib/
cnaf_gnu: FFTW_INC = /shared/software/project/aladyn/fftw/include/
cnaf_gnu: all

cnaf_intel_perf: cnaf_intel
cnaf_intel_perf: OPTFC += -axcore-avx-i,SSE4.2
cnaf_intel_perf: OPTFC += -ipo
cnaf_intel_perf: all

cnaf_intel_debug: cnaf_intel
cnaf_intel_debug: EXEC = ALaDyn.debug
cnaf_intel_debug: OPTFC = -real-size 64 -g -check all -fpe0 -warn -traceback -debug extended
cnaf_intel_debug: OPTCC = -g
cnaf_intel_debug: all

cnaf_gnu_perf: cnaf_gnu
cnaf_gnu_perf: all

cnaf_gnu_debug: cnaf_gnu
cnaf_gnu_debug: EXEC = ALaDyn.debug
cnaf_gnu_debug: OPTFC = -fdefault-real-8 -O0 -g -Wall -Wextra -fbacktrace -fbounds-check
cnaf_gnu_debug: OPTCC = -O0 -g
cnaf_gnu_debug: all

cnaf_scal: cnaf_gnu
cnaf_scal: FC = scalasca -instrument mpif90
cnaf_scal: CC = scalasca -instrument mpic++
cnaf_scal: EXEC = ALaDyn.scal
cnaf_scal: all

fermi: FC = mpixlf90
fermi: CC = mpixlcxx
fermi: FFTW_DEF = -WF,-DUSE_OLD_FFTW_INTERFACE
fermi: FFTW_LIB = /cineca/prod/libraries/fftw/3.3.2/bgq-xl--1.0/lib/
fermi: FFTW_INC = /cineca/prod/libraries/fftw/3.3.2/bgq-xl--1.0/include/
fermi: BOOST_LIB = /cineca/prod/libraries/boost/1.51.0/bgq-xl--1.0/lib/
fermi: BOOST_INC = /cineca/prod/libraries/boost/1.51.0/bgq-xl--1.0/include/
fermi: STDCPP_LINK += -libmc++
fermi: OTHER_LIB = /opt/ibmcmp/vacpp/bg/12.1/bglib64
fermi: all

fermi_gnu: FFTW_LIB = /cineca/prod/libraries/fftw/3.3.2/bgq-gnu--4.4.6/lib/
fermi_gnu: FFTW_INC = /cineca/prod/libraries/fftw/3.3.2/bgq-gnu--4.4.6/include/
fermi_gnu: BOOST_LIB = /cineca/prod/libraries/boost/1.51.0/bgq-xl--1.0/lib/
fermi_gnu: BOOST_INC = /cineca/prod/libraries/boost/1.51.0/bgq-xl--1.0/include/
fermi_gnu: all

fermi_debug: fermi
fermi_debug: EXEC = ALaDyn.debug
fermi_debug: OPTFC = -qrealsize=8 -qipa=partition=large -g -qcheck -qflttrap -qfullpath -qarch=qp -qtune=qp -qmaxmem=-1 -qinitauto=FF
fermi_debug: OPTCC = -g
fermi_debug: all

fermi_debug_slow: fermi
fermi_debug_slow: EXEC = ALaDyn.debug
fermi_debug_slow: OPTFC = -g -qrealsize=8 -qcheck -qflttrap -qfullpath -qmaxmem=-1 -qinitauto=FF
fermi_debug_slow: OPTCC = -g
fermi_debug_slow: all

fermi_debug_gnu: fermi_gnu
fermi_debug_gnu: EXEC = ALaDyn.debug
fermi_debug_gnu: OPTFC = -fdefault-real-8 -O0 -g -Wall -Wextra -fbacktrace -fbounds-check
fermi_debug_gnu: OPTCC = -O0 -g
fermi_debug_gnu: all

fermi_perf: OPTFC = -qrealsize=8 -qipa=partition=large -qarch=qp -qtune=qp -qmaxmem=-1
fermi_perf: fermi
fermi_perf: all

fermi_scal: fermi
fermi_scal: FC = scalasca -instrument mpixlf90
fermi_scal: CC = scalasca -instrument mpixlcxx
fermi_scal: EXEC = ALaDyn.scal
fermi_scal: OPTFC = -qrealsize=8 -qipa=partition=large -qarch=qp -qtune=qp -qmaxmem=-1
fermi_scal: all

galileo_gnu: FFTW_LIB = /cineca/prod/libraries/fftw/3.3.4/openmpi--1.8.4--gnu--4.9.2/lib/
galileo_gnu: FFTW_INC = /cineca/prod/libraries/fftw/3.3.4/openmpi--1.8.4--gnu--4.9.2/include/
galileo_gnu: all

galileo_intel: intel
galileo_intel: FFTW_LIB = /cineca/prod/compilers/intel/pe-xe-2016/binary/compilers_and_libraries/linux/mkl/lib/intel64/
galileo_intel: FFTW_INC = /cineca/prod/compilers/intel/pe-xe-2016/binary/compilers_and_libraries/linux/mkl/include/
galileo_intel: all

galileo_debug_gnu: galileo_gnu
galileo_debug_gnu: EXEC = ALaDyn.debug
galileo_debug_gnu: OPTFC = -D_GALILEO -fdefault-real-8 -O0 -g -Wall -Wextra -fbacktrace -fbounds-check
galileo_debug_gnu: OPTCC = -O0 -g
galileo_debug_gnu: all

galileo_perf: galileo_intel
galileo_perf: all



#shared_variables_and_params start
$(OBJ_FOLDER)/precision_def.o: $(SRC_FOLDER)/precision_def.F90
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/precision_def.mod: $(SRC_FOLDER)/precision_def.F90 $(OBJ_FOLDER)/precision_def.o
	@true

$(OBJ_FOLDER)/mpi_var.o: $(SRC_FOLDER)/mpi_var.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/mpi_var.mod: $(SRC_FOLDER)/mpi_var.f90 $(OBJ_FOLDER)/mpi_var.o
	@true

$(OBJ_FOLDER)/phys_param.o: $(SRC_FOLDER)/phys_param.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/phys_param.mod: $(SRC_FOLDER)/phys_param.f90 $(OBJ_FOLDER)/phys_param.o
	@true

$(OBJ_FOLDER)/grid_and_particles.o: $(SRC_FOLDER)/grid_and_particles.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/grid_and_particles.mod: $(SRC_FOLDER)/grid_and_particles.f90 $(OBJ_FOLDER)/grid_and_particles.o
	@true

$(OBJ_FOLDER)/code_util.o: $(SRC_FOLDER)/code_util.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/code_util.mod: $(SRC_FOLDER)/code_util.f90 $(OBJ_FOLDER)/code_util.o
	@true
#shared_variables_and_params end

$(OBJ_FOLDER)/cpp_folder_tree.o: $(SRC_FOLDER)/cpp_folder_tree.cpp 
	$(CC) $(OPTCC) -I$(BOOST_INC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/cpp_folder_tree.mod: $(SRC_FOLDER)/cpp_folder_tree.cpp $(OBJ_FOLDER)/cpp_folder_tree.o
	@true

$(OBJ_FOLDER)/system_utilities.o: $(SRC_FOLDER)/system_utilities.f90 $(OBJ_FOLDER)/mpi_var.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/system_utilities.mod: $(SRC_FOLDER)/system_utilities.f90 $(OBJ_FOLDER)/system_utilities.o
	@true

$(OBJ_FOLDER)/util.o: $(SRC_FOLDER)/util.f90 $(OBJ_FOLDER)/code_util.mod $(OBJ_FOLDER)/grid_and_particles.mod \
                      $(OBJ_FOLDER)/phys_param.mod $(OBJ_FOLDER)/mpi_var.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $(SRC_FOLDER)/util.f90 $(REDIRECT)
$(OBJ_FOLDER)/util.mod: $(SRC_FOLDER)/util.f90 $(OBJ_FOLDER)/util.o
	@true

$(OBJ_FOLDER)/fft_lib.o: $(SRC_FOLDER)/fft_lib.F90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) $(FFTW_DEF) -I$(FFTW_INC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/fft_lib.mod: $(SRC_FOLDER)/fft_lib.F90 $(OBJ_FOLDER)/fft_lib.o
	@true

#pic_mod start
$(OBJ_FOLDER)/struct_def.o: $(SRC_FOLDER)/struct_def.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/struct_def.mod: $(SRC_FOLDER)/struct_def.f90 $(OBJ_FOLDER)/struct_def.o
	@true

$(OBJ_FOLDER)/fftg_param.o: $(SRC_FOLDER)/fftg_param.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/fftg_param.mod: $(SRC_FOLDER)/fftg_param.f90 $(OBJ_FOLDER)/fftg_param.o
	@true

$(OBJ_FOLDER)/grid_param.o: $(SRC_FOLDER)/grid_param.f90 $(OBJ_FOLDER)/struct_def.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/grid_param.mod: $(SRC_FOLDER)/grid_param.f90 $(OBJ_FOLDER)/grid_param.o
	@true

$(OBJ_FOLDER)/control_bunch_input.o: $(SRC_FOLDER)/control_bunch_input.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/control_bunch_input.mod: $(SRC_FOLDER)/control_bunch_input.f90 $(OBJ_FOLDER)/control_bunch_input.o
	@true

$(OBJ_FOLDER)/ionize.o: $(SRC_FOLDER)/ionize.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/ionize.mod: $(SRC_FOLDER)/ionize.f90 $(OBJ_FOLDER)/ionize.o
	@true

#common_param_and_fields start
$(OBJ_FOLDER)/pstruct_data.o: $(SRC_FOLDER)/pstruct_data.f90 $(OBJ_FOLDER)/struct_def.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pstruct_data.mod: $(SRC_FOLDER)/pstruct_data.f90 $(OBJ_FOLDER)/pstruct_data.o
	@true

$(OBJ_FOLDER)/fstruct_data.o: $(SRC_FOLDER)/fstruct_data.f90 $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/fstruct_data.mod: $(SRC_FOLDER)/fstruct_data.f90 $(OBJ_FOLDER)/fstruct_data.o
	@true

$(OBJ_FOLDER)/all_param.o: $(SRC_FOLDER)/all_param.f90 $(OBJ_FOLDER)/fftg_param.mod \
                           $(OBJ_FOLDER)/control_bunch_input.mod $(OBJ_FOLDER)/precision_def.mod \
                           $(OBJ_FOLDER)/ionize.mod $(OBJ_FOLDER)/mpi_var.mod \
                           $(OBJ_FOLDER)/phys_param.mod $(OBJ_FOLDER)/grid_and_particles.mod \
                           $(OBJ_FOLDER)/code_util.mod $(OBJ_FOLDER)/grid_param.mod 
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/all_param.mod: $(SRC_FOLDER)/all_param.f90 $(OBJ_FOLDER)/all_param.o
	@true
#common_param_and_fields end
#pic_mod end

$(OBJ_FOLDER)/particles.o: $(SRC_FOLDER)/particles.f90 $(OBJ_FOLDER)/all_param.mod $(OBJ_FOLDER)/precision_def.mod \
                           $(OBJ_FOLDER)/pstruct_data.mod $(OBJ_FOLDER)/fstruct_data.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/particles.mod: $(SRC_FOLDER)/particles.f90 $(OBJ_FOLDER)/particles.o
	@true

$(OBJ_FOLDER)/parallel.o: $(SRC_FOLDER)/parallel.F90 $(OBJ_FOLDER)/all_param.mod $(OBJ_FOLDER)/pstruct_data.mod \
                          $(OBJ_FOLDER)/fstruct_data.mod $(OBJ_FOLDER)/fft_lib.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/parallel.mod: $(SRC_FOLDER)/parallel.F90 $(OBJ_FOLDER)/parallel.o
	@true

$(OBJ_FOLDER)/pic_rutil.o: $(SRC_FOLDER)/pic_rutil.f90 $(OBJ_FOLDER)/pstruct_data.mod $(OBJ_FOLDER)/util.mod \
                           $(OBJ_FOLDER)/fstruct_data.mod $(OBJ_FOLDER)/all_param.mod $(OBJ_FOLDER)/parallel.mod \
                           $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pic_rutil.mod: $(SRC_FOLDER)/pic_rutil.f90 $(OBJ_FOLDER)/pic_rutil.o
	@true

$(OBJ_FOLDER)/der_lib.o: $(SRC_FOLDER)/der_lib.f90 $(OBJ_FOLDER)/util.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/der_lib.mod: $(SRC_FOLDER)/der_lib.f90 $(OBJ_FOLDER)/der_lib.o
	@true

$(OBJ_FOLDER)/grid_fields.o: $(SRC_FOLDER)/grid_fields.f90 $(OBJ_FOLDER)/util.mod $(OBJ_FOLDER)/der_lib.mod \
                             $(OBJ_FOLDER)/all_param.mod $(OBJ_FOLDER)/fft_lib.mod $(OBJ_FOLDER)/precision_def.mod \
                             $(OBJ_FOLDER)/parallel.mod $(OBJ_FOLDER)/fstruct_data.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/grid_fields.mod: $(SRC_FOLDER)/grid_fields.f90 $(OBJ_FOLDER)/grid_fields.o
	@true

$(OBJ_FOLDER)/pdf_moments.o: $(SRC_FOLDER)/pdf_moments.f90 $(OBJ_FOLDER)/all_param.mod \
                             $(OBJ_FOLDER)/fstruct_data.mod $(OBJ_FOLDER)/pic_rutil.mod \
                             $(OBJ_FOLDER)/grid_fields.mod $(OBJ_FOLDER)/fstruct_data.mod \
                             $(OBJ_FOLDER)/pstruct_data.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pdf_moments.mod: $(SRC_FOLDER)/pdf_moments.f90 $(OBJ_FOLDER)/pdf_moments.o
	@true

$(OBJ_FOLDER)/pic_in.o: $(SRC_FOLDER)/pic_in.f90 $(OBJ_FOLDER)/particles.mod $(OBJ_FOLDER)/pic_rutil.mod \
                        $(OBJ_FOLDER)/fft_lib.mod $(OBJ_FOLDER)/grid_fields.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pic_in.mod: $(SRC_FOLDER)/pic_in.f90 $(OBJ_FOLDER)/pic_in.o
	@true

$(OBJ_FOLDER)/pic_out.o: $(SRC_FOLDER)/pic_out.f90 $(OBJ_FOLDER)/all_param.mod $(OBJ_FOLDER)/pstruct_data.mod \
                         $(OBJ_FOLDER)/fstruct_data.mod $(OBJ_FOLDER)/parallel.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pic_out.mod: $(SRC_FOLDER)/pic_out.f90 $(OBJ_FOLDER)/pic_out.o
	@true

$(OBJ_FOLDER)/pic_dump.o: $(SRC_FOLDER)/pic_dump.f90 $(OBJ_FOLDER)/all_param.mod $(OBJ_FOLDER)/pstruct_data.mod \
                          $(OBJ_FOLDER)/fstruct_data.mod $(OBJ_FOLDER)/parallel.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pic_dump.mod: $(SRC_FOLDER)/pic_dump.f90 $(OBJ_FOLDER)/pic_dump.o
	@true

$(OBJ_FOLDER)/pic_evolve_in_time.o: $(SRC_FOLDER)/pic_evolve_in_time.f90 $(OBJ_FOLDER)/pic_rutil.mod \
                                    $(OBJ_FOLDER)/particles.mod $(OBJ_FOLDER)/grid_fields.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pic_evolve_in_time.mod: $(SRC_FOLDER)/pic_evolve_in_time.f90 $(OBJ_FOLDER)/pic_evolve_in_time.o
	@true

$(OBJ_FOLDER)/pwfa_bunch_field_calculation.o: $(SRC_FOLDER)/pwfa_bunch_field_calculation.f90 \
                                              $(OBJ_FOLDER)/pic_in.mod \
                                              $(OBJ_FOLDER)/pic_evolve_in_time.mod \
                                              $(OBJ_FOLDER)/particles.mod \
                                              $(OBJ_FOLDER)/pic_rutil.mod \
                                              $(OBJ_FOLDER)/fft_lib.mod \
                                              $(OBJ_FOLDER)/grid_fields.mod \
                                              $(OBJ_FOLDER)/all_param.mod \
                                              $(OBJ_FOLDER)/pstruct_data.mod \
                                              $(OBJ_FOLDER)/fstruct_data.mod \
                                              $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pwfa_bunch_field_calculation.mod: $(SRC_FOLDER)/pwfa_bunch_field_calculation.f90 \
                                              $(OBJ_FOLDER)/pwfa_bunch_field_calculation.o
	@true

$(OBJ_FOLDER)/read_input.o: $(SRC_FOLDER)/read_input.f90 $(OBJ_FOLDER)/control_bunch_input.mod \
                            $(OBJ_FOLDER)/grid_and_particles.mod $(OBJ_FOLDER)/phys_param.mod \
                            $(OBJ_FOLDER)/code_util.mod $(OBJ_FOLDER)/mpi_var.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/read_input.mod: $(SRC_FOLDER)/read_input.f90 $(OBJ_FOLDER)/read_input.o
	@true

$(OBJ_FOLDER)/pwfa_output_addons.o: $(SRC_FOLDER)/pwfa_output_addons.f90 $(OBJ_FOLDER)/pic_in.mod \
                                    $(OBJ_FOLDER)/pic_evolve_in_time.mod $(OBJ_FOLDER)/read_input.mod \
                                    $(OBJ_FOLDER)/pdf_moments.mod $(OBJ_FOLDER)/system_utilities.mod \
                                    $(OBJ_FOLDER)/pic_out.mod $(OBJ_FOLDER)/pic_dump.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/pwfa_output_addons.mod: $(SRC_FOLDER)/pwfa_output_addons.f90 $(OBJ_FOLDER)/pwfa_output_addons.o
	@true

$(OBJ_FOLDER)/ALaDyn.o: $(SRC_FOLDER)/ALaDyn.F90 $(OBJ_FOLDER)/pic_in.mod $(OBJ_FOLDER)/pic_out.mod $(OBJ_FOLDER)/pic_dump.mod \
                        $(OBJ_FOLDER)/read_input.mod $(OBJ_FOLDER)/pdf_moments.mod $(OBJ_FOLDER)/pwfa_output_addons.mod \
                        $(OBJ_FOLDER)/pic_evolve_in_time.mod $(OBJ_FOLDER)/system_utilities.mod $(OBJ_FOLDER)/precision_def.mod
	$(FC) $(OPTFC) -c -o $@ $< $(REDIRECT)
$(OBJ_FOLDER)/ALaDyn.mod: $(SRC_FOLDER)/ALaDyn.F90 $(OBJ_FOLDER)/ALaDyn.o
	@true

clean:
	rm -f $(OBJECTS) $(MODULES)

cleanall:
	rm -f $(OBJECTS) $(MODULES) $(EXECUTABLE) opt_report.txt *.exe *~ .*~ log.txt *.optrpt bin/*.optrpt

