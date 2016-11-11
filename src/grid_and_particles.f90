 !*****************************************************************************************************!
 !             Copyright 2008-2016 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
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

 integer,parameter :: Ref_nlayer=6
 integer(kind=8) :: nptot_global
 integer,allocatable :: el_ionz_count(:)
 integer :: nx,ny,nz,nx_loc,nx1_loc,ny_loc,nz_loc,npt_buffer,nx_alloc
 integer :: nxcell,nycell,nzcell,ny_targ,npty,nptz,nptx_max
 integer :: ibx,iby,ibz
 integer :: loc_npty(8),loc_nptz(8),nptx(8),loc_nptx(8)
 integer :: nx_stretch,ny_stretch,nz_stretch
 integer :: mp_per_cell(6),nref
 integer :: np_per_xc(Ref_nlayer),np_per_yc(Ref_nlayer),np_per_zc(Ref_nlayer),ppc(Ref_nlayer)
 integer :: ion_min(3),ion_max(3),atomic_number(3),ionz_lev,ionz_model
 integer :: loc_nyc_max,loc_nzc_max,loc_nxc_max,ndim_max
 real(dp) :: k0,yx_rat,zx_rat,djc(3)
 real(dp) :: ratio_mpc(6),pavg_npart(4),mem_psize_max
 real(dp) :: mass(4),mass_rat(4),charge_to_mass(4),unit_charge(4),Lorentz_fact(4)
 real(dp) :: mass_number(3)
 real(dp) :: pmass,ompe,vbeam,curr_max(3),t0_pl(4),j0_norm
 real(dp) :: gam0,bet0,u0_b, nb_over_np,b_charge
 real(dp) :: t0_lp,xc_lp,xf,w0_x,w0_y
 real(dp) :: t0_b,lp_rad,el_lp,el_D,lambda_p,omega_p,chann_rad,lpvol
 real(dp) :: nc0,nc,n_over_nc,n1_over_n,n2_over_n,shock_over_n,np_per_cell,nmacro
 real(dp) :: lam0,oml,ZR,E0,lp_pow,lp_intensity,lp_xsize
 real(dp) :: lp_amp,lp_max,eb_max,lp_energy
 real(dp) :: lpx(7),lpy(3),targ_in,targ_end,alp_HM,lx_FWHM,tau_FWHM,a0
 real(dp) :: lp_in,lp_end,ymin_t,ymax_t,zmin_t,zmax_t,rmin_t,rmax_t
 end module grid_and_particles
