 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
 !                                                Alberto Marocchino                                   !
 !*****************************************************************************************************!

 !*****************************************************************************************************!
 !  This file is part of ALaDyn.                                                                       !
 !                                                                                                     !
 !  ALaDyn is free software: you can redistribute it and/or modify                                     !
 !  it under the terms of the GNU General Public License as published by                               !
 !  the Free Software Foundation, either version 3 of the License, or                                  !
 !  (at your option) any later version.                                                                !
 !                                                                                                     !
 !  ALaDyn is distributed in the hope that it will be useful,                                          !
 !  but WITHOUT ANY WARRANTY; without even the implied warranty of                                     !
 !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                      !
 !  GNU General Public License for more details.                                                       !
 !                                                                                                     !
 !  You should have received a copy of the GNU General Public License                                  !
 !  along with ALaDyn.  If not, see <http://www.gnu.org/licenses/>.                                    !
 !*****************************************************************************************************!

 module code_util

 use precision_def

 implicit none

 integer,parameter :: major_version = 5
 integer,parameter :: minor_version = 12
 character(6) :: sw_name='ALaDyn'
 character(9) :: input_namelist_filename='input.nml'
 character(10) :: input_data_filename='input.data'
 integer,parameter :: MAXV=1,SUMV=0,MINV=-1
 integer,parameter :: LEFT=-1,RIGHT=1
 integer,parameter :: FIELD=0,CURR=1
 integer,parameter :: sh_ix=3
 integer :: mem_size,mem_psize
 integer :: time2dump(1)=0
 real(dp) :: mem_psize_max,dump_t0,dump_t1
 real(dp) :: unix_time_begin, unix_time_now
 real(dp) :: time_interval_dumps, unix_time_last_dump
 real(dp) :: gamma_cut_min,weights_cut_min,weights_cut_max
 logical :: Part,part_dcmp,cmp,test,Stretch,Hybrid
 logical :: Lp_active,Lp_inject,Plane_wave,Lin_lp,Circ_lp,Relativistic,Envelope,Ions,Beam,Pbeam,Two_color
 logical :: Ionization,Wake,Solid_target,Charge_cons,G_prof,High_gamma
 logical :: Impact_ioniz,Comoving,P_tracking
 logical :: L_intdiagnostics_pwfa,L_intdiagnostics_classic
 logical :: L_force_singlefile_output
 logical :: L_print_J_on_grid
 logical :: L_first_output_on_restart
 logical :: L_use_unique_dumps
 logical :: L_disable_rng_seed
 logical :: L_intdiagnostics_background
 logical :: L_env_modulus

 end module code_util
