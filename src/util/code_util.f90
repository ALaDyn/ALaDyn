!*****************************************************************************************************!
!                            Copyright 2008-2020  The ALaDyn Collaboration                            !
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

  integer, parameter :: major_version = 8
  integer, parameter :: minor_version = 2
  character (6), parameter :: sw_name = 'ALaDyn'
  character(:), allocatable :: input_namelist_filename
  character(:), allocatable :: input_json_filename
  character(10) :: input_data_filename = 'input.data'
  integer, parameter :: maxv = 1, sumv = 0, minv = -1
  integer, parameter :: left = -1, right = 1
  integer, parameter :: field = 0, curr = 1
  integer, parameter :: sh_ix = 3, sh_iy = 3, sh_iz = 3
  integer :: time2dump(1) = 0
  integer :: mem_size, mem_psize
  integer :: last_iter, iter_max, write_every
  integer :: t_ind, inject_ind, tk_ind
  integer :: ienout, iout, iter, ier
  real(dp) :: mem_psize_max, dump_t0, dump_t1
  real(dp) :: unix_time_begin, unix_time_now
  real(dp) :: time_interval_dumps, unix_time_last_dump
  real(dp) :: gamma_cut_min, weights_cut_min, weights_cut_max
  real(dp) :: tdia, dtdia, tout, dtout, tstart, mem_max_addr

  logical :: diag, tpart
  logical :: l_intdiagnostics_pwfa, l_intdiagnostics_classic
  logical :: l_force_singlefile_output
  logical :: l_print_j_on_grid
  logical :: l_first_output_on_restart
  logical :: l_use_unique_dumps
  logical :: l_disable_rng_seed
  logical :: l_intdiagnostics_background
  logical :: l_env_modulus

 end module
