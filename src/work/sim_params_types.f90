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

module sim_params_types

 use precision_def

 implicit none

 type grid_parameters_t
 !! Container with the grid parameters
  integer  :: nx      = 1
  integer  :: ny      = 1
  integer  :: nz      = 1
  integer  :: ny_targ = 1
  real(dp) :: k0      = one_dp
  real(dp) :: yx_rat  = -one_dp
  real(dp) :: zx_rat  = -one_dp
 end type

 type simulation_parameters_t
 !! Container with the simulation parameters
  integer :: lpf_ord         = 2
  integer :: der_ord         = 2
  integer :: str_flag        = 0
  integer :: iform           = 0
  integer :: model_id        = 1
  integer :: dmodel_id       = 1
  integer :: ibx             = 0
  integer :: iby             = 0
  integer :: ibz             = 0
  integer :: ibeam           = 1
  logical :: density_limiter = .false.
  integer :: pusher          = 1
  integer :: n_substeps      = 1
 end type

 type targ_description_parameters_t
 !! Container with the target parameters
  integer                             :: nsp                = 0
  integer                             :: nsb                = 0
  integer                             :: ionz_lev           = 0
  integer                             :: ionz_model         = 1
  integer                             :: transverse_dist     = 0
  integer,  allocatable, dimension(:) :: ion_min
  integer,  allocatable, dimension(:) :: ion_max
  integer,  allocatable, dimension(:) :: atomic_number
  real(dp), allocatable, dimension(:) :: mass_number
  real(dp), allocatable, dimension(:) :: t0_pl
  integer,  allocatable, dimension(:) :: np_per_xc
  integer,  allocatable, dimension(:) :: np_per_yc
  integer,  allocatable, dimension(:) :: np_per_zc
  real(dp), allocatable, dimension(:) :: concentration
  real(dp), allocatable, dimension(:) :: lpx
  real(dp), allocatable, dimension(:) :: lpy
  real(dp)                            :: n0_ref             = one_dp
  real(dp)                            :: np1                = zero_dp
  real(dp)                            :: np2                = zero_dp
  real(dp)                            :: r_c                = zero_dp
  logical                             :: l_disable_rng_seed = .false.
 end type

 type laser_parameters_t
 !! Container with the laser parameters
  logical                             :: g_prof = .true.
  integer                             :: nb_laser = 1
  real(dp)                            :: t0_lp = zero_dp
  real(dp)                            :: xc_lp = zero_dp
  real(dp)                            :: tau_fwhm = one_dp
  real(dp)                            :: w0_y = one_dp
  real(dp)                            :: a0 = zero_dp
  real(dp)                            :: lam0 = one_dp
  real(dp), allocatable, dimension(:) :: lp_delay
  real(dp)                            :: lp_offset = zero_dp
  real(dp)                            :: t1_lp = zero_dp
  real(dp)                            :: tau1_fwhm = zero_dp
  real(dp)                            :: w1_y = zero_dp
  real(dp)                            :: a1 = zero_dp
  real(dp)                            :: lam1 = one_dp
  logical                             :: symmetrization_pulse = .false.
  real(dp)                            :: a_symm_rat = one_dp
  logical,  allocatable, dimension(:) :: enable_ionization
  real(dp), allocatable, dimension(:) :: y0_cent
  real(dp), allocatable, dimension(:) :: z0_cent
  real(dp)                            :: y1_cent = zero_dp
  real(dp)                            :: z1_cent = zero_dp
  real(dp)                            :: incid_angle = zero_dp
  logical                             :: improved_envelope = .false.
 end type

 type beam_parameters_t
 !! Container with the beam parameters
  integer  :: nb_1 = 0
  real(dp) :: xc_1 = one_dp
  real(dp) :: gam_1 = one_dp
  real(dp) :: sxb_1 = one_dp
  real(dp) :: syb_1 = one_dp
  real(dp) :: epsy_1 = one_dp
  real(dp) :: epsz_1 = one_dp
  real(dp) :: dg_1 = one_dp
  real(dp) :: charge_1 = one_dp
  real(dp) :: ap1_twiss = one_dp
  real(dp) :: bt1_twiss = one_dp
  real(dp) :: t_inject = zero_dp
 end type

 type window_parameters_t
 !! Container with the moving window parameters
  integer  :: w_sh    = 10
  real(dp) :: wi_time = zero_dp
  real(dp) :: wf_time = zero_dp
  real(dp) :: w_speed = zero_dp
 end type

 type output_parameters_t
 !! Container with the output parameters
  integer  :: nouts                     = 1
  integer  :: iene                      = 10
  integer  :: nvout                     = 2
  integer  :: nden                      = 1
  integer  :: ncurr                     = 0
  integer  :: npout                     = 0
  integer  :: jump                      = 1
  integer  :: pjump                     = 1
  real(dp) :: gamma_min                 = one_dp
  real(dp) :: xp0_out                   = zero_dp
  real(dp) :: xp1_out                   = 100*one_dp
  real(dp) :: yp_out                    = 10*one_dp
  real(dp) :: tmax                      = zero_dp
  real(dp) :: cfl                       = 0.8*one_dp
  integer  :: new_sim                   = 0
  integer  :: id_new                    = 0
  integer  :: dump                      = 0
  logical  :: l_force_single_output     = .true.
  logical  :: l_print_j_on_grid         = .true.
  logical  :: l_first_output_on_restart = .false.
  logical  :: l_env_modulus             = .true.
  real(dp) :: time_interval_dumps        = -one_dp
 end type

 type tracking_parameters_t
 !! Container with the tracking parameters
  logical  :: p_tracking = .false.
  integer  :: every_track = 1
  integer  :: nkjump = 1
  real(dp) :: txmin  = zero_dp
  real(dp) :: txmax  = zero_dp
  real(dp) :: tymin  = zero_dp
  real(dp) :: tymax  = zero_dp
  real(dp) :: tzmin  = zero_dp
  real(dp) :: tzmax  = zero_dp
  real(dp) :: t_in   = zero_dp
  real(dp) :: t_out  = one_dp
  logical  :: a_on_particles = .false.
 end type

 type mpi_parameters_t
 !! Container with the MPI parameters
  integer :: nprocx = -1
  integer :: nprocy = -1
  integer :: nprocz = -1
 end type

 type parameters_t
 !! Container with the all parameters
 type(grid_parameters_t), allocatable             :: grid_params
 logical                                          :: exists_grid = .false.
 type(simulation_parameters_t), allocatable       :: sim_params
 logical                                          :: exists_simulation = .false.
 type(targ_description_parameters_t), allocatable :: targ_params
 logical                                          :: exists_target = .false.
 type(laser_parameters_t), allocatable            :: laser_params
 logical                                          :: exists_laser = .false.
 type(beam_parameters_t), allocatable             :: beam_params
 logical                                          :: exists_beam = .false.
 type(window_parameters_t), allocatable           :: window_params
 logical                                          :: exists_window = .false.
 type(tracking_parameters_t), allocatable         :: track_params
 logical                                          :: exists_tracking = .false.
 type(output_parameters_t), allocatable           :: output_params
 logical                                          :: exists_output = .false.
 type(mpi_parameters_t), allocatable              :: mpi_params
 logical                                          :: exists_mpi = .false.
 end type
end module
