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
!Common parameters defined in
!   1) input.nml list (=> START/read_input.f90)
!   2) initial conditions (=> START/set_init_param.f90, init)
!   3) during time evolution
!================================

 module common_param
  use precision_def
  use sim_params_types
  
  implicit none

  integer, parameter :: ref_nlayer = 6, ref_nlas = 8, &
                        ref_nspec = 8
  ! Parameters type
  type(parameters_t) :: parameters
  !namelist input parameters
  integer :: nx, ny, nz, ny_targ
  integer :: n1ft, n2ft, n3ft
  integer :: n1ft_loc, n2ft_loc, n3ft_loc
  real(dp) :: k0, yx_rat, zx_rat
  integer :: ibx, iby, ibz, ibeam
  integer :: lpf_ord, der_ord, str_flag, iform, model_id, dmodel_id
  integer :: pusher, n_substeps
  integer :: nsp, nsb, ionz_lev, ionz_model, ion_min(ref_nlayer), &
             ion_max(ref_nlayer), transverse_dist
  integer :: atomic_number(ref_nlayer), n_mol_atoms(ref_nlayer)
  integer :: nb_laser, nb_1, np_per_xc(ref_nlayer), &
             np_per_yc(ref_nlayer)
  real (dp) :: mass_number(ref_nlayer), t0_pl(ref_nlayer)
  real (dp) :: lpx(7), lpy(2), n_over_nc, np1, np2, r_c
  real (dp) :: t0_lp, xc_lp, tau_fwhm, w0_y, a0, lam0, &
    lp_delay(ref_nlas)
  real (dp) :: lp_offset, t1_lp, tau1_fwhm, w1_y, a1, lam1, a_symm_rat
  real (dp) :: xc_1, gam_1, sxb_1, syb_1, epsy_1, epsz_1, dg_1, &
               charge_1, ap1_twiss,bt1_twiss, t_inject
  integer :: nouts, iene, nvout, nden, npout, nbout, jump, pjump, ncurr
  integer :: new_sim, id_new, dump
  real(dp) :: gam_min, xp0_out, xp1_out, yp_out
  !====================
  real(dp) :: w_speed, wi_time, wf_time
  real(dp) :: tnow, tmax, tscale, dt_loc, dt, cfl
  logical :: initial_time
  !====================
  ! TRACKING
  !====================
  integer :: every_track(ref_nspec), nkjump(ref_nspec), track_tot_nstep
  real (dp) :: txmin(ref_nspec), txmax(ref_nspec)
  real (dp) :: tymin(ref_nspec), tymax(ref_nspec)
  real (dp) :: tzmin(ref_nspec), tzmax(ref_nspec)
  real (dp) :: t_in(ref_nspec), t_out(ref_nspec)
  logical ::  p_tracking(ref_nspec), a_on_particles(ref_nspec)
  !====================
  ! END TRACKING
  !====================
  integer :: nprocx, nprocy, nprocz
  logical :: g_prof, comoving
  logical :: beam, hybrid, wake, envelope, solid_target
  logical :: ionization, ions
  logical :: part, stretch, channel, inject_beam
  logical :: lp_active, lp_inject, plane_wave, p_polar, s_polar, &
    lin_lp, circ_lp, relativistic, Two_color, improved_envelope
  logical :: enable_ionization(2), symmetrization_pulse
  logical :: charge_cons, high_gamma, test
  logical :: density_limiter, decreasing_transverse

  integer :: nx_loc, ny_loc, nz_loc, npty, nptz, nptx_max, ncmp_max, &
             nx_alloc
  integer :: loc_npty(ref_nspec), loc_nptz(ref_nspec), nptx(ref_nspec), &
             loc_nptx(ref_nspec), sptx_max(ref_nspec), nxf, npt_buffer(ref_nspec)
  integer :: sh_targ
  integer :: mp_per_cell(ref_nlayer), nref, np_per_zc(ref_nlayer), &
             ppc(ref_nlayer)
  integer :: loc_nyc_max, loc_nzc_max, loc_nxc_max, ndim_max
  real(dp) :: djc(3), ratio_mpc(ref_nlayer), pavg_npart(4), wgh_ion, &
              concentration(ref_nlayer)
  real(dp) :: mass(4), mass_rat(4), charge_to_mass(4), unit_charge(4), &
              lorentz_fact(4)
  real(dp) :: n0_ref, pmass, ompe, vbeam, curr_max(3), j0_norm, &
              ratio_mpfluid, chann_fact, n_plasma
  real(dp) :: gam0, bet0, u0_b, nb_over_np, b_charge

  real(dp) :: oml, e0, lp_pow, zr, lp_intensity, lp_xsize, p_c
  real(dp) :: w0_x, lp_amp, xf, lp_max, eb_max, lp_energy, lp_rad
  real(dp) :: xc1_lp, xf1, zr1, lp1_rad, lp1_amp, om1, w1_x

  real(dp) :: t0_b, el_lp, el_d, lambda_p, omega_p, lpvol
  real(dp) :: nc0, ncrit, n1_over_n, n2_over_n
  real(dp) :: np_per_cell, np_per_nmacro, nmacro
  real(dp) :: targ_in, targ_end, lx_fwhm
  real(dp) :: lp_in(ref_nlas), lp_end(ref_nlas), lp_ionz_in, &
              lp_ionz_end, xf_loc(ref_nlas), xc_loc(ref_nlas)
  real(dp) :: y0_cent(ref_nlas), z0_cent(ref_nlas), y1_cent, z1_cent, &
              incid_angle
  real(dp) :: ymin_t, ymax_t, zmin_t, zmax_t, rmin_t, rmax_t
  ! tracking param
  integer :: track_tot_part

  integer :: pot_ndim, nb_max, pe_nbmax, nb_min, pe_nbmin
  integer :: tsc_ord, t_ord, spl_ord
  integer :: nsp_run, nsp_ionz
  integer :: ndim, curr_ndim, nj_dim, nd2, nfield, nbfield, nfcomp, &
             mod_ord, w_sh
  real(dp) :: macro_charge
  
  real(dp) :: energy_in_targ
  integer(kind=8) :: nptot_global
  !==================
  ! PARTICLE PUSHER
  !==================
  integer, parameter :: HIGUERA = 1
  integer, parameter :: BORIS = 2

 end module

