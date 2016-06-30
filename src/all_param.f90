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

 module all_param

 use precision_def
 use mpi_var
 use phys_param
 use grid_and_particles
 use code_util
 use grid_param
 use control_bunch_input
 use ionize

 implicit none



 contains


 subroutine lpf_sch_coeff

 a_lpf(1:4)=1.
 b_lpf(1:4)=1.
 lpf_form(1:4)=iform
 if(LPf_ord==3)then
  lpf_form(1:3)=iform
  lpf_form(4)=0
  b_lpf(1)=1./(2.-2**(1./3.))  !alpha
  b_lpf(2)=1.-2.*b_lpf(1)      !beta=1.-2*alpha
  b_lpf(3)=b_lpf(1)            !alpha
  b_lpf(4)=0.0
  a_lpf(1)=0.5*b_lpf(1)        !alpha/2
  a_lpf(2)=0.5*(1.-b_lpf(1))   !1/2(1-alpha)
  a_lpf(3)=a_lpf(2)
  a_lpf(4)=a_lpf(1)
  c_lpf(1)=0.0
  c_lpf(2)=a_lpf(1)
  c_lpf(3)=0.5*b_lpf(2)
  c_lpf(4)=c_lpf(2)
 endif

 end subroutine lpf_sch_coeff

 !--------------------------

 subroutine rk_tsch_coeff(rk)
 integer,intent(in) :: rk
 integer :: ip

 select case(rk)
 case(3)
  b_rk(1)=1.
  b_rk(2)=0.25
  b_rk(3)=2./3.
  a_rk(1)=0.0
  a_rk(2)=1.-b_rk(2)
  a_rk(3)=1.-b_rk(3)

 case(4)
  rk_form(1:4)=iform
  if(iform==2)rk_form(4)=0
  a_rk(1)=1./6.
  a_rk(2)=1./3.
  a_rk(3)=1./3.
  a_rk(4)=a_rk(1)
  c_rk(0)=-4./3.
  c_rk(1)=1./3.
  c_rk(2)=2./3.
  c_rk(3)=c_rk(1)
  c_rk(4)=1.
  b_rk(1)=0.5
  b_rk(2)=0.5
  b_rk(3)=1.0
  b_rk(4)=1.0/6.0

 case(5)      !4th order five-stage optimized
  rk_form(1:5)=iform
  a_rk(1)=0.4
  a_rk(2)=0.0581597561
  a_rk(3)=1.0746983979
  a_rk(4)=-0.5338616876
  a_rk(5)=0.0

  b_rk(1)=-0.01
  b_rk(2)=0.4818402439
  b_rk(3)=-0.8615395999
  b_rk(4)=1.057293281
  b_rk(5)=0.3324060746

  c_rk(1)=0.0
  c_rk(2)=0.39
  c_rk(3)=0.53
  c_rk(4)=0.6349990419
  c_rk(5)=0.1337322374

  do ip=2,5
   c_rk(ip)=b_rk(ip-1)+c_rk(ip-1)
  end do

  do ip=2,5
   c_rk(ip)=a_rk(ip-1)+c_rk(ip)
  end do

 case(6)          !4th-order six-stage optimized
  b_rk(1)=0.10893125722541
  b_rk(2)=0.13201701492152
  b_rk(3)=0.38911623225517
  b_rk(4)=-0.59203884581148
  b_rk(5)=0.47385028714844
  b_rk(6)=0.48812405426094
  a_rk(1)=0.17985400977138
  a_rk(2)=0.14081893152111
  a_rk(3)=0.08255631629428
  a_rk(4)=0.65804425034331
  a_rk(5)=0.31862993413251
  a_rk(6)=0.
  c_rk(1)=0.0
  do ip=2,6
   c_rk(ip)=b_rk(ip-1)+c_rk(ip-1)
  end do
  do ip=2,6
   c_rk(ip)=a_rk(ip-1)+c_rk(ip)
  end do

 end select

 end subroutine rk_tsch_coeff

 !--------------------------

 subroutine param
 ! sets general parameters and grid depending on initial conditions
 integer :: i, sh_t,pml_size
 real(dp) :: b_charge_den,gvol,gvol_inv,a_ch,k_ch,nm_fact

 a_lpf(1:4)=1.
 b_lpf(1:4)=1.
 if(LPf_ord >2)call rk_tsch_coeff(LPf_ord)

 nxcell=nx
 nycell=ny
 nzcell=nz

 ndim=1
 if(ny>1)ndim=2
 if(nz>1)ndim=3

 nx_loc=nx/nprocx
 nx1_loc=nx/nprocy
 ny_loc=ny/nprocy
 nz_loc=nz/nprocz
 sh_t=ny/2-ny_targ/2
 Lp_inject=.false.
 Cyl_coord=.false.
 Ionization=.false.
 Impact_ioniz=.false.
 !===============================
 ! Multispecies target with max 3 ionic species
 !=================
 Charge_cons=.true.
 if(iform>1)Charge_cons=.false.
 !==========================
 nsp_run=nsp
 !===============================
 do i=1, nsp-1                  !ion species
  if(ion_min(i)< ion_max(i))nsp_ionz=i+1
 enddo
 nsp_ionz=min(nsp_ionz,nsp)
 do i=1,3
  if(mass_number(i)< 1.)call set_atomic_weight(atomic_number(i),mass_number(i))
 end do
 if(ionz_lev >0)Ionization=.true.
 if(Ionization) call set_ionization_coeff(atomic_number,nsp_ionz)
 !==========================
 Wake=.false.
 Solid_target=.false.
 if(n_over_nc < 0.5)Wake=.true.
 if(n_over_nc >1.)Solid_target=.true.
 !=================================
 Stretch=.false.
 pml_size=0
 ny_stretch=0
 nx_stretch=0
 if(str_flag==1)then
  Stretch=.true.
  ny_stretch=nint(real(ny,dp)*size_of_stretch_along_y)
 endif
 if(str_flag==2)then
  Stretch=.true.
  ny_stretch=nint(real(ny,dp)*size_of_stretch_along_y)
  nx_stretch=nint(real(nx,dp)*size_of_stretch_along_x)
 endif
 call grid_alloc(nx,nx_loc,ny,ny_loc,nz,nz_loc,ny_targ,&
                               pml_size,nprocy,nprocz,nprocx)
 loc_nyc_max=loc_ygr_max
 loc_nzc_max=loc_zgr_max
 loc_nxc_max=loc_xgr_max

 pi2=2.0*pi
 test=.false.
 Part=.false.

 if(np_per_xc(1) > 0)Part=.true.

 np_per_cell=1
 cmp=.false.
 Lin_lp=.false.
 Relativistic=.false.
 Ions=.false.
 Beam=.false.
 Pbeam=.false.
 Envelope=.false.
 Circ_lp=.false.
 Plane_wave=.false.
 if(iby==2)Plane_wave=.true.

 ifilt=0
 if(der_ord >3)cmp=.true.
 t_ord=LPf_ord
 RK_ord=LPf_ord
 Lp_active=.false.

 ! mass-charge parameters four species: three ion species+ electrons
 ! Ions charges defined by initial conditions
 unit_charge(1)=electron_charge_norm
 unit_charge(2)=ion_min(1)*proton_charge_norm
 unit_charge(3)=ion_min(2)*proton_charge_norm
 unit_charge(4)=ion_min(3)*proton_charge_norm
 !=================================
 mass(1)=electron_mass_norm
 mass(2)=mass_number(1)*proton_mass_norm
 mass(3)=mass_number(2)*proton_mass_norm
 mass(4)=mass_number(3)*proton_mass_norm
 if(dmodel_id==0)then
  mass(2)=mass(1)
 endif
 do i=1,4
  mass_rat(i)=1./mass(i)
  charge_to_mass(i)=unit_charge(i)*mass_rat(i)
 end do
 !======================================
 Lorentz_fact(1:4)=mass_rat(1:4)  
!to be multiplied by the particle charge in the equation of motion
 !==========================================
 nm_fact=1.
 gvol_inv=1.
 gvol=1.
 j0_norm=1.
 jb_norm=1.
 E0=electron_mass !E=in unit mc^2/(e*l0)=[TV/m]/E0; E0*E in [TV/m]=[MV/mu] unit
 
 !do not remove following initialization, not required outside of PWFA but needed to avoid NaN ops
 nb_over_np=1.
 b_charge=1.
 gam0=1.
 

 do i=1,6
  mp_per_cell(i)=np_per_xc(i)*np_per_yc(i)
  if(ndim==3) mp_per_cell(i)=np_per_xc(i)*np_per_yc(i)*np_per_zc(i)
 end do
 ratio_mpc=1.
 nref=mp_per_cell(1)
 if(mp_per_cell(1) >0)then
  j0_norm=1./real(nref,dp)
  do i=1,6
   ratio_mpc(i)=real(mp_per_cell(1),dp)/real(mp_per_cell(i),dp)
  enddo
 endif
 if(w_speed < 0.0)then
  vbeam=-w_speed
  Comoving=.true.
 else
  Comoving=.false.
  vbeam=0.0
 endif

 if(model_id < 5)then
  mod_ord=1
  ifilt=0
  Relativistic=.true.
  part_dcmp=.false.
  nfield=3
  curr_ndim=2
  if(ndim > 2)then
   nfield=6
   curr_ndim=ndim
  endif
  dx=1./k0
  call set_grid(nx,ny,nz,ibx,nx_stretch,ny_stretch,k0,yx_rat,zx_rat)
  dt=cfl*dx
  select case(ndim)
  case(1)
   dt=cfl/sqrt(dx_inv*dx_inv)
   gvol_inv=dx_inv*dx_inv*dx_inv
  case(2)
   dt=cfl/sqrt(dx_inv*dx_inv+dy_inv*dy_inv)
   gvol_inv=dx_inv*dy_inv*dy_inv
  case(3)
   dt=cfl/sqrt(dx_inv*dx_inv+dy_inv*dy_inv+dz_inv*dz_inv)
   gvol_inv=dx_inv*dy_inv*dz_inv
  end select
  gvol=1./gvol_inv
  Lp_active=.true.

  if(model_id < 3) Lin_lp = .true.
  if(model_id == 3) Circ_lp = .true.
  if(model_id ==4) then
   mod_ord=2
   Envelope=.true.
  endif
  !=======================
  ! Code Units for laser fields
  nc=pi/(rc0*lam0*lam0)    !critical density in units n0=10^21/cm^3=10^9/mu^3
  nm_fact=nc*(1.e+9)       ! critical density (1/mu^3)
  oml=pi2/lam0             !laser frequency in unit c/l0
  lp_amp=a0*oml
  lp_max=lp_amp
  nc0=oml*oml              !nc0=(2*pi/lam0)** 2
  ompe=nc0*n_over_nc       !squared adimensional plasma frequency :

  lp_intensity=1.37*(a0/lam0)*(a0/lam0)  !in units 10^18 W/cm^2
  if(Plane_wave)then        !plane LP wave
   lp_pow=0.01*lp_intensity*Ly_box*Lz_box  !in TW = 10^{-3}J/fs
   ZR=0.0
  else
   lp_rad=w0_y*sqrt(2.*log(2.))           !FWHM focal spot
   ZR=pi*w0_y*w0_y/lam0
   lp_pow=0.5*pi*lp_intensity*w0_y*w0_y   !in units 10^10 W
   if(ndim==2)lp_pow=0.5*dy*lp_intensity*w0_y   !in units 10^10 W
   lp_pow=0.01*lp_pow                     !in TW= 1.e-03[J/fs]  units
  endif
  lx_FWHM=2.*w0_x*acos(sqrt(0.5*sqrt(2.)))/pi !FWHM llength in microns
  tau_FWHM=lx_FWHM/speed_of_light
  lp_energy=1.e-03*tau_FWHM*lp_pow
  energy_in_targ=0.0
  el_lp=lam0
  chann_rad=0.0
  if(n_over_nc > 0.0)then
   el_lp=lam0/(pi2*sqrt(n_over_nc))
   el_D=t0_pl(1)*el_lp         !Debye length t0_pl(1)= V_T/c at t=0
   omega_p=1./el_lp
   k_ch=8.*13.5*1000.*n_over_nc/(17.*pi*pi)
   a_ch=(k_ch*lp_pow)**(1./3.)
   if(n_over_nc >1.)chann_rad=pi*lpx(3)*n_over_nc/(a0*lam0)
  endif
  bet0=0.0
  lambda_p=pi2*el_lp
  lpvol=el_lp*el_lp*el_lp

 endif
!=============================
 nx_alloc=nint(dx_inv*sum(lpx(1:5)))
 nx_alloc=min(nx_loc,nx_alloc)
 npt_buffer=nx_alloc*ny_loc*nz_loc*nref
 npt_buffer=npt_buffer+npt_buffer/8
 if(Ionization)allocate(el_ionz_count(npt_buffer))
!===================================


 if(model_id> 4)then !  e-Beams section
  select case(model_id)
   !========================================
   ! uses l0=1mu
   ! for fields E_u=GV/m = 1.e-03*mc^2/(l0*e*E0)
   ! n0= 10^18/cc =10^6/mu^3
   !========================================
   ! the electron radius r_c=rc0*10^{-8}mu
   !the squared adimensional plasma frequency on n0 density
   !   r_c*l0*l0*n0= 10^{-3}*rc0
   !omp^2=   4*pi*l0*l0*rc*n0=4*pi*rc0*1.e-03*E0/1.e-03
   !=============================
  case(5)
   nm_fact=1.e+06      !electron density[mu^{-3}] in the n0=10^18/cm^3 plasma
   nc=1.0
   Wake=.true.
   Solid_target=.false.
   !======================
   eb_max=0.0
   nc0=2.*E0*pi2*rc0
   !======================
   ompe=nc0*n_over_nc
   Lorentz_fact(1:4)=1.e-03*Lorentz_fact(1:4)/E0
   omega_p=0.02*sqrt(10.*pi*rc0*n_over_nc)
   el_lp=1./omega_p
   nb_over_np=rhob(1)
   lam0=1.             !Is the unit of length
   gam0=gam(1)
   u0_b=sqrt(gam0*gam0-1.)   !the beam x-momentum
   bet0=u0_b/gam0            !the beam velocity
   Lorentz_bfact(1:5)=Lorentz_fact(1)
   lambda_p=pi2*el_lp
   lpvol=el_lp*el_lp*el_lp
   Beam=.true.
   nfield=3
   nbfield=4
   if(ndim ==3)then
    nfield=6
    nbfield=nfield
   endif
   curr_ndim=ndim
   do i=1,5
    if(bunch_type(i)==3)Lorentz_bfact(i)=Lorentz_fact(1)/proton_mass_norm
   end do
   mod_ord=3
   dx=1./k0
   jb_norm=1.
   bvol=1.
   call set_grid(nx,ny,nz,ibx,nx_stretch,ny_stretch,k0,yx_rat,zx_rat)
   dt=cfl/sqrt(dx_inv*dx_inv+dy_inv*dy_inv+dz_inv*dz_inv)
   do i=1,nsb
    bvol(i)=pi2*sqrt(pi2)*sxb(i)*syb(i)*syb(i)    !the bunch volume (mu^3)
    b_charge_den=rhob(i)*n_over_nc*nm_fact*e_charge  !pC/mu^3
    bcharge(i)=b_charge_den*bvol(i)  !bunch charge in [pC]
    Qbch(i)=rhob(i)*bvol(i)/lpvol
   end do
   gvol_inv=dx_inv*dy_inv*dz_inv
   gvol=1./gvol_inv
   if(L_particles)then
    do i=1,nsb
     nb_tot(i) = nint(gvol_inv* bvol(i)/j0_norm)
    end do
   endif
   do i=1,nsb
    jb_norm(i)=rhob(i)*gvol_inv*bvol(i)/(nb_tot(i)*j0_norm)
   end do
   b_charge=bcharge(1)

  case(6) !  Proton one-beam section
   ! uses l0=1mm
   ! for fields E_u=GV/m = mc^2/(l0*e*E0)
   ! n0= 10^11/cc =10^8/mm^3
   !==============================
   !  n_over_nc = initial plasma density/n0= ne/n0
   !=======================
   lam0=1.             !n [mm] is the unit of length
   nm_fact=1.e+08      !electron density[mm^{-3}]
   nc=1.0              !the plasma ref density is n0=10^11/cm^3=10^8/mm^3
   nc0=2.*pi2*rc0*1.e-03
   Wake=.true.
   Solid_target=.false.
   !=============================
   !the squared adimensional plasma frequency on n0 density
   ! the electron radius r_c=rc0*10^{-11}mm
   !   r_c*l0*l0*n0= 10^{-3}*rc0
   !omp^2=   4*pi*l0*l0*rc*n0=4*pi*rc0*10^{-3}
   !=======================
   omega_p=n_over_nc*nc0
   ompe=E0*omega_p        ! the source term in Maxw.eqs: E0*omega_p^2
   omega_p=sqrt(omega_p)
   !======================
   !============================
   el_lp=1./omega_p    !skin depth in l0[mm] unit
   lambda_p=pi2*el_lp  ! the plasma wavelength in l0 unit
   lpvol=el_lp*el_lp*el_lp
   gam0=gam(1)
   u0_b=sqrt(gam0*gam0-1.)   !the beam x-momentum
   bet0=u0_b/gam0            !the beam v_b/c norm velocity
   w_speed=bet0
   vbeam=0.0
   Beam=.true.
   Pbeam=.true.
   Lorentz_fact(1)=Lorentz_fact(1)/E0
   Lorentz_bfact(1)=Lorentz_fact(1)/proton_mass_norm
   !==================================
   nfield=6
   nbfield=nfield
   curr_ndim=ndim
   mod_ord=4
   dx=1./k0
   jb_norm=1.
   call set_grid(nx,ny,nz,ibx,nx_stretch,ny_stretch,k0,yx_rat,zx_rat)
   dt=cfl/sqrt(dx_inv*dx_inv+dy_inv*dy_inv+dz_inv*dz_inv)
   !====================
   ! i=1 only one beam: enters rhob= bunch number density/np
   i=1
   bvol(i)=pi2*syb(i)*syb(i)    !the volume (mm^3)
   b_charge_den=rhob(i)*n_over_nc*nm_fact*e_charge  !pC/mm^3
   bcharge(i)=b_charge_den*bvol(i)  !unit length bunch charge in [pC]
   Qbch(i)=rhob(i)*bvol(i)/lpvol
   gvol_inv=dx_inv*dy_inv*dz_inv
   gvol=1./gvol_inv
   nb_over_np=rhob(1)
   !========== Normalization
   jb_norm(i)=rhob(i)*gvol_inv*bvol(i)*sxb(i)/real(nb_tot(i),dp)
   !====bvol*gvol_inv/nb_tot = the beam cell number/beam macroparticles number
   ! jb_norm = weight= 1/beam macroparticles per cell
   ! j0_norm = 1/plasma macroparticles per cell
   b_charge=b_charge_den
   !==================
  end select
 endif
 if(Wake)nsp_run=1  !only electrons running
 !============================
 !==============  in 2D dz=dy mp_per_cell =mp_x*mp_y,  mp_z=1
 if(mp_per_cell(1) >0)then
  nmacro=nref*gvol_inv
  np_per_cell=nm_fact*n_over_nc*gvol !here the real particle number per cell
  macro_charge=np_per_cell*e_charge  !real charge [in pC units]/cell
  !===========================================
  !all parameters to be multiplied by the macro weight= j0_norm=1/mp_per_cell
  !==========================
  ! j0_norm*np_percell = number of real particles for each
  ! macroparticle
  !========================
  nmacro=nref*real(nx*ny*nz,dp)
  ! here the total initial nmacro particles
 endif
 !================================
 !  SET PARAM all cases

 !===============================================================
 !charge per macroparticle: np_per_nmacro=nm_fact*n_over_nc/nmacro
 !Qbch=bvol/lpvol

 !np_per_nmacro= electron density over el_macro density
 !multiplied by nb_over_np gives the bunch elecron density over bunch macro
 !under the condition :np_per_cell is the same for plasma and bunch macro
 !-----------------------------
 !========================= driving beams parameters

 pot_ndim=0
 nd2=2*curr_ndim
 nj_dim=curr_ndim
 if(Envelope)nj_dim=curr_ndim+1
 if(ibeam==2)then
  nj_dim=curr_ndim+1
  pot_ndim=nj_dim+1
 endif


 djc(1)=dx
 djc(2:3)=0.0
 if(ndim==2)then
  djc(1)=0.5*dx
  djc(2)=0.5*dy
 endif
 if(ndim==3)then
  djc(1)=dx/3.
  djc(2)=dy/3.
  djc(3)=dz/3.
 endif
 ymin_t=y(1)
 ymax_t=y(ny+1)
 zmin_t=z(1)
 zmax_t=z(nz+1)
 if(ndim >1)then
  ymin_t=y(sh_t+1)
  ymax_t=y(ny+1-sh_t)
 endif
 if(ndim >2 )then
  zmin_t=z(1+sh_t)
  zmax_t=z(nz+1-sh_t)
 endif
 end subroutine param
 !--------------------------
 end module all_param


 !--------------------------
