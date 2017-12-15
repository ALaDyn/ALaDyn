 !*****************************************************************************************************!
 !                            Copyright 2008-2018  The ALaDyn Collaboration                            !
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

 module grid_and_particles
 use precision_def

 implicit none

 integer,parameter :: Ref_nlayer=6,Ref_nlas=8
 integer(kind=8) :: nptot_global
 integer :: nx,ny,nz,nx_loc,nx1_loc,ny_loc,nz_loc,nx_alloc
 integer :: nxcell,nycell,nzcell,ny_targ,npty,nptz,nptx_max
 integer :: loc_npty(8),loc_nptz(8),nptx(8),loc_nptx(8),sptx_max(4),nxf,npt_buffer(4)
 integer :: nx_stretch,ny_stretch,nz_stretch
 integer :: ibx,iby,ibz
 real(dp) :: k0,yx_rat,zx_rat,djc(3),lxb,lyb

 integer :: mp_per_cell(6),nref,nb_laser
 integer :: np_per_xc(Ref_nlayer),np_per_yc(Ref_nlayer),np_per_zc(Ref_nlayer),ppc(Ref_nlayer)
 integer :: ion_min(3),ion_max(3),atomic_number(3),ionz_lev,ionz_model
 integer :: loc_nyc_max,loc_nzc_max,loc_nxc_max,ndim_max
 real(dp) :: ratio_mpc(6),pavg_npart(4),wgh_ion
 real(dp) :: mass(4),mass_rat(4),charge_to_mass(4),unit_charge(4),Lorentz_fact(4)
 real(dp) :: mass_number(3)
 real(dp) :: n0_ref,pmass,ompe,vbeam,curr_max(3),t0_pl(4),j0_norm,ratio_mpfluid
 real(dp) :: gam_min,gam0,bet0,u0_b, nb_over_np,b_charge

 real(dp) :: t0_lp,xc_lp,xf,w0_x,w0_y,lam0,a0
 real(dp) :: lp_offset,t1_lp,xc1_lp,xf1,w1_x,w1_y,lam1,a1,lp1_rad,ZR1,tau1_fwhm
 real(dp) :: lp1_amp,om1
 real(dp) :: oml,ZR,E0,lp_pow,lp_intensity,lp_xsize,lp_delay,P_c
 real(dp) :: lp_amp,lp_max,eb_max,lp_energy

 real(dp) :: t0_b,lp_rad,el_lp,el_D,lambda_p,omega_p,lpvol
 real(dp) :: nc0,ncrit,n_over_nc,n1_over_n,n2_over_n,np1,np2,np_per_cell,nmacro
 real(dp) :: lpx(7),lpy(3),targ_in,targ_end,lx_fwhm,tau_fwhm
 real(dp) :: lp_in(Ref_nlas),lp_end(Ref_nlas),lp_ionz_in,lp_ionz_end,xf_loc(Ref_nlas),xc_loc(Ref_nlas)
 real(dp) :: ymin_t,ymax_t,zmin_t,zmax_t,rmin_t,rmax_t
 ! tracking param
 real(dp) :: txmin,txmax,tymin,tymax,tzmin,tzmax,t_in,t_out
 integer  :: tkjump,nkjump,track_tot_nstep,track_tot_part

 integer :: pot_ndim,nb_max,pe_nbmax,nb_min,pe_nbmin
 integer :: RK_ord,LPf_ord,Tsc_ord,der_ord,t_ord,spl_ord
 integer :: ifilt,iform,rk_form(5),lpf_form(4)
 integer :: nouts,iene,iout,dcmp_count
 integer :: ienout,iter,ier
 integer :: nsp,nsp_run,nsb,nsp_ionz
 integer :: ibeam,ndim,curr_ndim,nj_dim,nd2,nfield,nbfield,nfcomp,model_id,dmodel_id,mod_ord
 integer :: new_sim,id_new
 integer :: dump,jump,pjump
 integer :: nden,nvout,npout,nbout
 integer :: str_flag,w_sh
 real(dp) :: xp0_out,xp1_out,yp_out
 real(dp) :: w_speed,wi_time,wf_time,macro_charge
 real(dp) :: tnow,tscale,tmax,dt,cfl
 real(dp) :: a_rk(6),b_rk(6),c_rk(0:6)
 real(dp) :: a_lpf(4),b_lpf(4),c_lpf(4)
 real(dp) :: energy_in_targ

 end module grid_and_particles

