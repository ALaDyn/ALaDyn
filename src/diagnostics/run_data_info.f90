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

 module run_data_info
 
 use array_wspace
 use code_util
 use common_param
 use grid_param
 use ionz_data
 use parallel

 implicit none

 
 contains

 subroutine timing

 integer,parameter :: write_every=100
 
 if (mod(iter,write_every)==0) then
  mem_psize_max=0.0
  if (Part) then
   if (prl) then
    call part_numbers
    call max_pmemory_check()
   endif
  endif

  if (pe0) then
   write(6,'(a10,i6,a10,e11.4,a10,e11.4)') 'iter = ',iter,' t = ',tnow,' dt = ',dt_loc
   call tot_num_part(nptot_global)
   call cpu_time(unix_time_now)
   write(6,'(a16,f12.3,a10,i15)')' Time elapsed = ',unix_time_now-unix_time_begin,', nptot = ', nptot_global
   if(prl)then
    if(Part)then
     write(6,'(a21,i10,a1,i10)')' part min/max distr. ',np_min,' ',np_max
     write(6,'(a18,2i8)')' where pmin/pmax  ',pe_npmin,pe_npmax
     if (prl) then
      write(6,'(a24,e12.5)')' max part memory in MB= ',mem_psize_max
      write(6,'(a20,e12.5)')' Max part  address= ',mem_max_addr
     endif
    endif
   endif
   write(6,'(a13,2E11.4)')' xmin/xmax   ',xmin,xmax
   write(6,*)'========================'
  end if   !end Pe0 write
 end if

 if(tnow <tmax)then
  tnow=tnow+dt_loc
  iter=iter+1
 endif
 end subroutine timing

 !---------------------------

 subroutine error_message
 if(pe0)then
  if(ier >0)write(6,*)'error occurred: '
  if (ier==20) write(6,*)'error: negative density: ', ier
  if (ier==1) write(6,*)'error: fields values too big: ', ier
 endif
 end subroutine error_message
!======================================
 subroutine part_numbers
 integer :: ip,iz,ix,pp,ic,np_new,nploc(npe)

 do ic=1,nsp
  nploc=0
  np_new=loc_npart(imody,imodz,imodx,ic)
  call intvec_distribute(np_new,nploc,npe)
  pp=0
  do ix=0,npe_xloc-1
   do iz=0,npe_zloc-1
    do ip=0,npe_yloc-1
     pp=pp+1
     loc_npart(ip,iz,ix,ic)=nploc(pp)
    end do
   end do
  end do
 end do
 nploc=0
 do ic=1,nsp
  pp=0
  do ix=0,npe_xloc-1
   do iz=0,npe_zloc-1
    do ip=0,npe_yloc-1
     pp=pp+1
     nploc(pp)=nploc(pp)+loc_npart(ip,iz,ix,ic)
    end do
   end do
  end do
 end do
 np_max=maxval(nploc(1:npe))
 np_min=minval(nploc(1:npe))


 do ip=0,npe-1
  if(nploc(ip+1)==np_min)pe_npmin=ip
  if(nploc(ip+1)==np_max)pe_npmax=ip
 end do
 if(Beam)then
  do ic=1,nsb
   nploc=0
   np_new=loc_nbpart(imody,imodz,imodx,ic)
   call intvec_distribute(np_new,nploc,npe)
   pp=0
   do ix=0,npe_xloc-1
    do iz=0,npe_zloc-1
     do ip=0,npe_yloc-1
      pp=pp+1
      loc_nbpart(ip,iz,ix,ic)=nploc(pp)
     end do
    end do
   end do
  end do
  nploc=0
  do ic=1,nsb
   pp=0
   do ix=0,npe_xloc-1
    do iz=0,npe_zloc-1
     do ip=0,npe_yloc-1
      pp=pp+1
      nploc(pp)=nploc(pp)+loc_nbpart(ip,iz,ix,ic)
     end do
    end do
   end do
  end do
  nploc=0
  do ic=1,nsb
   pp=0
   do ix=0,npe_xloc-1
    do iz=0,npe_zloc-1
     do ip=0,npe_yloc-1
      pp=pp+1
      nploc(pp)=nploc(pp)+loc_nbpart(ip,iz,ix,ic)
     end do
    end do
   end do
  end do
  nb_max=maxval(nploc(1:npe))
  nb_min=minval(nploc(1:npe))
  do ip=0,npe-1
   if(nploc(ip+1)==nb_min)pe_nbmin=ip
   if(nploc(ip+1)==nb_max)pe_nbmax=ip
  end do
 endif
 end subroutine part_numbers

 subroutine tot_num_part(nptot_global)

 integer(dp),intent(out) :: nptot_global
 integer(dp) :: nptot_local
 integer :: iterator_x, iterator_y, iterator_z
 integer :: iterator_species
 nptot_global = 0

 !! WARNING: allreduce_big_int is unsupported on many architectures: MPI_SUM not available for MPI_LONG_INT datatype
 !do iterator_species=1,nsp_run
 ! nptot_local = loc_npart(imody, imodz, imodx, nsp_run)
 !end do
 !call allreduce_big_int(nptot_local, nptot_global)

 if (pe0) then
  do iterator_y=0,npe_yloc-1
   do iterator_z=0,npe_zloc-1
    do iterator_x=0,npe_xloc-1
     do iterator_species=1,nsp_run  !nsp_run is the real number of species running!
      nptot_local=int(loc_npart(iterator_y, iterator_z, iterator_x, iterator_species),dp_int)
      nptot_global=nptot_global+nptot_local
     end do
    end do
   end do
  end do
 endif
 end subroutine tot_num_part
 !---------------------------

 subroutine initial_run_info(nw)
 integer,intent(in) :: nw
 integer :: i
 character(len=21) :: output_data_in
 integer(hp_int) :: chsize
 real(sp) :: wgsize

 write(output_data_in,'(a15,i2.2,a4)')'init_data_info_',id_new,'.dat'

 write(6,*)'***********************************************'
 write(6,*)'Start: new = ',nw
 if(nw==0)then
  write(6,'(a18,3i8)')'  total grid size ',nx,ny,nz
  write(6,'(a18,3i8)')'  local grid size ',nx_loc,ny_loc,nz_loc
  write(6,'(a27,i3)')'  Cartesian grid dimension ',ndim
 endif
 if(nw==1)then
  write(6,'(a13,e11.4)')' restart time',tstart
  write(6,*)' diag ienout',ienout
 endif
 !write(6,*)' kind of dp and int data',kind(tstart),kind(nx)
!======================================

 open(60,file=output_data_in)
 write(60,*)' data bsize'
 write(60,*) huge(chsize),huge(wgsize)
!============================================
 if(nw==0)then
  write(60,*)'********** INITIAL DATA INFO************* '
 else
  write(60,*)'*********  RESTART DATA INFO*************'
 endif
  write(60,'(a12,f6.2)')'  Start time=',tstart
  write(60,'(a20,i4,a4)')'  ALaDyn running on ',npe,' cpu'
  write(60,'(a30,3i4)')'  (x-y-z) MPI decomposition : ',npe_xloc,npe_yloc,npe_zloc
  write(60,'(a26,i4)')'  Data output initial id= ',id_new

 write(60,*)'*************IMPLEMENTATION TOOLS********************'
  write(60,*)'  Field collocation on the Yee-module staggered grid'
  write(60,*)'  B-spline shapes of alternating first-second order '
  if(LPf_ord >0)then
   if(LPf_ord==2)write(60,*)'  One-step leap-frog time integration '
   if(der_ord==2)write(60,*)'  Explicit second order space derivative '
   if(der_ord==3)write(60,*)'  Optmal Explicit second order space derivative'
   if(der_ord==4)write(60,*)'  Fourth-order Maxwell solver only for (E,B) fields'
   if(LPf_ord>2)then
    write(60,*)'  RK multi-step fourth order time scheme '
    write(60,*)'  Explicit fourth order Space Derivative'
   endif
  endif
  if(Charge_cons)then
   if(iform <2)then
    write(60,*)'  Continuity equation enforced by Esirkepov scheme'
   else
    write(60,*)' Continuity equation enforced on x-grid using non-conservative schemes'
   endif
  endif
  write(60,*)'***************GRID**********************'
  write(60,'(a18,3i8)')'  total grid size ',nx,ny,nz
  write(60,'(a18,3i8)')'  local grid size ',nx_loc,ny_loc,nz_loc
  write(60,'(a27,i3)')'  Cartesian grid dimension ',ndim
  if(curr_ndim==2)then
   write(60,*)' Current components: [Jx,Jy] '
   write(60,*)' Field components: [Ex,Ey,Bz] '
  else
   write(60,*)' Current components: [Jx,Jy,Jz] '
   write(60,*)' Field components: [Ex,Ey,Ez,Bx,By,Bz] '
 endif
  write(60,*)'   Box sizes'
  write(60,*)'     xmin,      xmax     '
  write(60,'(a1,2e11.4)')' ',xmin,xmax
 if (ndim > 1) then
   write(60,*)'    ymin       ymax     '
   write(60,'(a1,2e11.4)')' ',ymin,ymax
  if (ndim > 2) then
    write(60,*)'    zmin       zmax     '
    write(60,'(a1,2e11.4)')' ',zmin,zmax
  endif
 endif
  write(60,*)'   Cell sizes'
  if(ndim <3)then
   write(60,'(a6,e11.4,a6,e11.4)')'  Dx =',dx,'  Dy =',dy
  else
   write(60,'(a6,e11.4,a6,e11.4,a6,e11.4)')'  Dx =',dx,'  Dy =',dy,'  Dz =',dz
  endif
 if(Stretch)then
  write(60,*)'  y=tang(a*xi) stretched layers on the transverse coordinates'
  write(60,'(a20,i6,2e11.4)')'  stretched grid size=',ny_stretch,y(ny_stretch+1)
 endif
 write(60,*)'***************PHYSICAL MODEL**********************'
 if(model_id <4)then
  write(60,*)'  Laser field injected '
  if(model_id==1)write(60,*)'  P-polarization on y-axis'
  if(model_id==2)write(60,*)'  S-polarization on z-axis'
  if(model_id==3)write(60,*)'  Circular polarization on y-z plane'
  write(60,*)'***************** Laser pulse structure***************'
  if(model_id==0)then
   write(60,*)'  A plane wave model'
  else
   write(60,*)'  Gaussian profile exp(-[r/w0_y]^2) on radial coordinate'
  endif
  if(G_prof)then
   write(60,*)'  Gaussian profile exp(-[(x-t)/w0_x]^2) on longitudinal (x-t) coordinate'
  else
   write(60,*)'  Cos^2[Pi(x-t)/w0_x] profile on longitudinal (x-t) coordinate'
  endif
 else
  select case(model_id)
  case(4)
  write(60,*)'  LP(P-pol) laser field injected using ENVELOPE integration model'
  case(5)
  write(60,*)'  Electron bunch injected'
  end select
 endif
 write(60,*)'***********FIELD BOUNDARY CONDITIONS*************************'
 if(ibx==0)write(60,*)' Open boundaries on x axis'
 if(ibx==1)write(60,*)' Reflecting boundary on right x '
 if(ibx==2)write(60,*)' Periodic boundaries on x axis'
 if(ibx >2)write(60,*)' Invalid x-boundary flag'
 if(ndim >1)then
  if(iby==0)write(60,*)' Open boundaries on y axis'
  if(iby==1)write(60,*)' Reflecting boundaries on y axis'
  if(iby==2)write(60,*)' Periodic boundaries on y axis'
  if(iby >2)write(60,*)' Invalid y-boundary flag'
 endif
 if(ndim >2)then
  if(ibz==0)write(60,*)' Open boundaries on z axis'
  if(ibz==1)write(60,*)' Reflecting boundaries on z axis'
  if(ibz==2)write(60,*)' Periodic boundaries on z axis'
  if(ibz >2)write(60,*)' Invalid z-boundary flag'
 endif
 if (w_speed > 0.0) then
  write(60,'(a23,f5.2)')'  Moving window speed= ',w_speed
 endif
 if (w_speed < 0.0) then
  write(60,'(a35,f5.2)')'  Comoving x-coordinate system V_b=',vbeam
 endif
 if(model_id < 5)then
  write(60,*)'******LASER PHYSICAL PARAMETERS *****************'
  write(60,*)'     Main pulse parameters '
  write(60,*)' Transverse scales'
  write(60,'(a8,f6.2,a18,f6.2)')'  w0_y= ',w0_y,'   focal spot =   ',lp_rad
  write(60,*)' Longitudinal scales'
  write(60,'(a8,f6.2,a18,f6.2)')'  w0_x= ',w0_x,'   tau_fwhm(fs) = ',tau_FWHM
  write(60,'(a13,f5.2,a13,f5.2)')'  wavelength=',lam0,'   frequency=',oml
  write(60,'(a17,e13.3,a17,f6.2)')'  Initial focus =',xf,'   Pulse center= ',xc_lp
  write(60,'(a29,e11.4)')'  Diffraction length Z_Rayl= ',ZR
  write(60,'(a25,f5.2)')'  Strength parameter a0= ',a0
  write(60,'(a33,f5.2,a6)')'  Max transverse field at focus= ',E0*a0*oml,'(TV/m)'
  write(60,'(a13,e13.3,a10)')'  Intensity= ',lp_intensity,'(e18W/cm2)'
  write(60,'(a9,e13.3,a4)')'  Power = ',lp_pow,'(TW)'
  write(60,'(a10,e13.3,a3)')'  Energy= ',lp_energy,'(J)'
  write(60,'(a30,i4)')'  Number of main laser pulses= ',nb_laser
  if(.not.Enable_ionization(1))write(60,*)' WARNING: Ionization disabled for the main pulse'
  if(nb_laser >1)then
   do i=2,nb_laser
    write(60,'(a23,i2,a5,i2,a10,f5.2)')'  Distance between the ',i-1,' and ',i,'&
    & centers= ',lp_delay(i-1)
   end do
  endif
  write(60,*)'-----------------------------------'
  if(Two_color)then
   write(60,*)'     Injected pulse parameters '
   write(60,'(a20,f5.2)')'  Offset distance = ',lp_offset
   write(60,*)' Transverse scales'
   write(60,'(a8,f5.2,a18,f5.2)')'  w1_y= ',w1_y,'   focal spot =   ',lp1_rad
   write(60,*)' Longitudinal scales'
   write(60,'(a8,f5.2,a18,f5.2)')'  w0_x= ',w1_x,'   tau_fwhm(fs) = ',tau1_FWHM
   write(60,'(a13,f5.2,a13,f5.2)')'  wavelength=',lam1,'   frequency=',om1
   write(60,'(a17,f6.2,a17,f6.2)')'  Initial focus =',xf1,'   Pulse center= ',xc1_lp
   write(60,'(a29,e11.4)')'  Diffraction length Z_Rayl= ',ZR1
   write(60,'(a25,f5.2)')'  Strength parameter a0= ',a1
   write(60,'(a33,f5.2,a6)')'  Max transverse field at focus= ',E0*a1*om1,'(TV/m)'
   if(.not.Enable_ionization(2))write(60,*)' WARNING: Ionization disabled for the injection pulse'
  endif
 endif
 if(Hybrid)then
  write(60,*)'************** FLUID DATA *****************'
  write(60,*)'Fluid density-momenta components',nfcomp
 endif
 write(60,*)'**************PARTICLE DATA *****************'
 write(60,'(a18,i4)')'  Species number =',nsp
 write(60,'(a32,i4)')'  Id number of running species: ',nsp_run
 write(60,*)'============================='
 write(60,*)' Species name:',species_name(0)
 write(60,'(a29,i4)')'  Macropart number per cell =',mp_per_cell(1)
 write(60,'(a34,e11.4)')'  Reference macroparticle weight =',j0_norm
 write(60,*)' Initial electron thermal speed V_T/c'
 write(60,'(a2,1E12.4)')'  ',t0_pl(1)
 write(60,*)'============================='
 if(nsp > 1)then
  do i=2,nsp
   write(60,*)' Ion species name:',species_name(atomic_number(i-1))
   write(60,'(a23,i4)')'  Ion number per cell =',mp_per_cell(i)
   write(60,'(a34,e11.4)')'  Reference macroparticle weight= ',wgh_ion
   write(60,*)' Initial charge, atomic number , mass number'
   write(60,'(a4,i8,a4,i8,a6,e11.4)')'    ',ion_min(i-1),'    ',&
         atomic_number(i-1),'      ',mass_number(i-1)
   write(60,*)' Initial ion thermal speed V_T/c'
   write(60,'(a2,1E12.4)')'  ',t0_pl(i)
  end do
   endif
 write(60,*)'-----------------------------------'
   if(Ionization)then
  write(60,*)' Field ionization model:'
  if(ionz_model==1)write(60,*)' W_DC ADK  '
  if(ionz_model==2)write(60,*)' W_AC= <W_DC>  ADK '
  if(ionz_model==4)write(60,*)' W_AC ADK +BSI '
  if(Beam)write(60,'(a24,e11.4,a6)')'  Reference max E_field ',eb_max,'(TV/m)'
  if(Lp_active)write(60,'(a24,e11.4,a6)')'  Reference max E_field ',lp_max,'(TV/m)'
    do i=1,nsp_ionz-1
   write(60,*)' Ionization active on ion species:',species_name(atomic_number(i))
    end do
    if(Symmetrization_pulse .and. (curr_ndim>2))then
     write(60,*)' A symmetrization pulse is implied when ionizing'
     write(60,*)' The sym. formula is sin(2*pi*u)*\Delta*a_1*a_symm_rat'
     if(a_symm_rat>zero_dp)then
      write(60,'(a32,e11.4)')' Symmetrization amplitude a_symm_rat',a_symm_rat
     else
      write(60,'(a32,e11.4)')' Symmetrization amplitude a_symm_rat',sqrt(2.)
     endif
    endif
   end if
 write(60,*)'**********TARGET PLASMA PARAMETERS***********'
  if(Part.or.Hybrid)then
   write(60,'(a26,e11.4,a10)')'  Electron number density ',n0_ref,'[10^18/cc]'
   write(60,'(a21,e11.4)')'  Plasma wavelength= ',lambda_p
   write(60,'(a20,e11.4)')' Chanelling fact  = ',chann_fact
   if(model_id < 5)then
    write(60,'(a20,f5.2,a10)')'  Critical density= ',ncrit,'[10^21/cc]'
    write(60,'(a18,e11.4,a4)')'  Critical power= ',P_c,'(TW)'
   endif
   write(60,*)'     Target sizes '
   write(60,*)'  xmin_t        xmax_t'
   write(60,'(a2,2e11.4)')'  ',targ_in,targ_end
   write(60,*)' ymin_t       ymax_t     '
   write(60,'(a2,2e11.4)')'  ',ymin_t,ymax_t
   if (ndim > 2) then
    write(60,*)' zmin_t       zmax_t     '
    write(60,'(a2,2e11.4)')'  ',zmin_t,zmax_t
   endif
   write(60,*)'********** TARGET CONFIGURATION***********'
   if(Wake)then
    select case(dmodel_id)
    case(1)
     write(60,*)' Multispecies five-layer x-profile with one central plateau '
    case(2)
     write(60,*)' Multispecie five-layer x-profile with one central lpx(3) downrump'
     write(6,*) '        connecting two lpx(2) lpx(4) plateau '
    case(3)
     write(60,*)' Five-layer x-profile with two lpx(2) lpx(4) plateau and'
     write(60,*)' a ionizing dopant added in lpx(3) layer'
    end select
   endif
   if(Solid_target)then
    select case(dmodel_id)
    case(3)
     write(60,*)' Target preplasma-enabled '
     if(lpx(1)>0.0)write(60,'(a17,e11.4)')'  Preplasma size ',lpx(1)
     if(lpx(2)>0.0)write(60,'(a12,e11.4)')'  Ramp size ',lpx(2)
     if(lpx(3)>0.0)write(60,'(a21,e11.4)')'  Central layer size ',lpx(3)
     if(lpx(5)>0.0)write(60,'(a33,e11.4)')'  Post-layer H contaminants size ',lpx(5)
    case(4)
     write(60,*)' Target H-foam-enabled '
     if(lpx(1)>0.0)write(60,'(a12,e11.4)')'  Foam size ',lpx(1)
     if(lpx(2)>0.0)write(60,'(a12,e11.4)')'  Ramp size ',lpx(2)
     if(lpx(5)>0.0)write(60,'(a33,e11.4)')'  Post-layer H contaminants size ',lpx(5)
    case(5)
     write(60,*)' Three species [El,Z1,Z3] target +[El,Z2] contaminants '
     write(60,*)'x-layer sizes'
     write(60,'(3e11.4)')lpx(1),lpx(3),lpx(5)
     write(60,*)'Layer density'
     write(60,'(3e11.4)')n1_over_n,n_over_nc,n2_over_n
    case(6)
     write(60,*)' Three species [El,Z]nanowires+ bulk '
     write(60,*)'wire size,interwire distance filling fact'
     if(ndim <3)then
      write(60,'(3e11.4)')lpy(1),lpy(2),(lpy(1)/(lpy(1)+lpy(2)))
     else
      write(60,'(3e11.4)')lpy(1),lpy(2),(lpy(1)/(lpy(1)+lpy(2)))**2
     endif
     write(60,*)'Boundaries',ibx,iby
    case(7)
     write(60,*)' One layer [El,Z] nano-tubes'
     write(60,*)' 2R_ext    2dr   '
     write(60,'(2e11.4)')lpy(1),lpy(2)
     write(60,*)'Boundaries',ibx,iby
    end select
   endif
  write(60,*)'Fully kinetic PIC schemes'
 if(model_id >4)then
   write(60,*)'******Beam+plasma data *****************'
   write(60,*)' nbfield components',nbfield
   if(ibeam>0)then
    write(60,*)' Bunch fields described by two arrays: ebf_bunch,ebf1_bunch'
   endif
   write(60,*)' Beam parameters: '
   write(60,*)' unit length is 1mu, unit density is n0=10^18/cc'
   write(60,*)'  Lambda , Omega_p,  n_over_n0 '
   write(60,'(3e11.4)')lambda_p,omega_p,n_over_nc
   write(60,*)'  gamma ,  sigma_x ,     sigma_y,   eps_y       eps_z '
   do i=1,nsb
    write(60,'(5e11.4)')gam(i),sxb(i),syb(i),epsy(i),epsz(i)
   end do
   write(60,*)'  nb_over_np  b_charge   Qcharge '
   do i=1,nsb
    write(60,'(3e11.4)')rhob(i),bunch_charge(i),reduced_charge(i)
   end do
  else
   write(60,*)' unit length is 1mm, unit density is nc'
   write(60,*)'  Lambda , Omega_p    n_over_nc  '
   write(60,'(3e11.4)')lambda_p,omega_p,n_over_nc
   write(60,*)'  gamma   bet0        Lx       sigma_y,   eps_y       eps_z '
   i=1
   write(60,'(6e11.4)')gam(i),bet0,sxb(i),syb(i),epsy(i),epsz(i)
   write(60,*)' jb_norm     nb_o_np    b_charge_den  '
   write(60,'(3e11.4)')jb_norm(i),rhob(i),b_charge
  endif
  write(60,*)' target in  target_end'
  write(60,'(2e11.4)')targ_in,targ_end
  write(60,*)' ymin_t       ymax_t     '
  write(60,'(2e11.4)')ymin_t,ymax_t
  write(60,*)' Electron number per cell '
  write(60,'(i4)')nref
  write(60,*)' Particle density normalization  '
  write(60,'(e11.4)')j0_norm
 endif
 write(60,*)'*******  END INITIAL DATA INFO***********'
 close(60)
 write(6,*)'********** TARGET *********************'
  write(6,*)' target in  target_end'
  write(6,'(2e11.4)')targ_in,targ_end
  write(6,*)' ymin_t       ymax_t     '
  write(6,'(2e11.4)')ymin_t,ymax_t
  write(6,*)' Electron number per cell '
  write(6,'(i4)')nref
  write(6,*)' Particle density normalization  '
  write(6,'(e11.4)')j0_norm
 write(6,*)'********** ALLOCATED MEMORY (MB) *********************'
 write(6,'(a28,e12.5)')' Pe0 allocated grid memory= ',1.e-06*real(mem_size,dp)*kind(electron_charge_norm)
 write(6,'(a28,e12.5)')' Pe0 allocated part memory= ',1.e-06*real(mem_psize,dp)*kind(electron_charge_norm)
 if (prl) then
  write(6,'(a24,e12.5)')' Max part memory (MB) = ',mem_psize_max
  !write(6,'(a20,e12.5)')' Max part  address= ',mem_max_addr
 endif
 write(6,*)' Particle min/max distr. '
 write(6,'(i10,a1,i10)')np_min,' ',np_max
 write(6,'(a18,2i8)')' where pmin/pmax  ',pe_npmin,pe_npmax
 write(6,*)'******************************************************'
 end  subroutine initial_run_info
!================================
 subroutine Ioniz_data(Ef_max,z0,An,zlev,zmod)

  real(dp),intent(in) :: Ef_max
  integer,intent(in) :: z0(:),An(:),zlev,zmod
  integer :: i,ic,k,zmax,zm_loc
  if(zlev==1)open(10,file='diag_one_level_ionz.dat')
  if(zlev==2)open(10,file='diag_two_level_ionz.dat')
  write(10,*)'nsp_ionz-1,zlev,zmod,N_ge '
  write(10,'(4i8)')nsp_ionz-1,zlev,zmod,N_ge
  write(10,*)'  Max Ef       dt      Omega_au  '
  write(10,'(3E11.4)')Ef_max,dt_fs,omega_a
  if(Two_color)then
  write(10,*)' a0        lam0,      om0,      a1,      lam1,         om1'
  write(10,'(6E11.4)')a0,lam0,oml,a1,lam1,om1
  else
  write(10,*)' a0        lam0,      om0'
  write(10,'(6E11.4)')a0,lam0,oml
  endif
  do ic=1,nsp_ionz-1
   write(10,*)' z0,     zmax'
   write(10,'(2i6)')z0(ic),An(ic)
   write(10,*)' E_c       E_b           V_norm(a.u.)  '
   zmax=An(ic)
   do i=1,zmax
    write(10,'(3E12.4)')E_c(i,ic),E_b(i,ic),V_norm(i,ic)
   end do
   zm_loc=zmax-z0(ic)
   write(10,*)'ionization rate :Wi(Ef,1:zmax-z0,ic) in fs^{-1}'
   do i=1,zm_loc
    write(10,*)i
    write(10,'(6e12.4)')wi(1:N_ge,i,ic)
   end do
!==!========================= cumulative distribution wsp
   if(zlev==1)then
    write(10,*)'ionization one level probability  wsp(Ne_g,z0:zmax)'
    do i=0,zm_loc-1
     write(10,*)i
     write(10,'(6e12.4)')W_one_lev(1:N_ge,i+z0(ic),ic)
    end do
   else               !for multi-level ionization
    write(10,*)'ionization multi level probability  wsp(Ne_g,z0:zmax,z0+i:zmax)'
    do i=0,zm_loc-1
     write(10,*)i
     do k=0,zm_loc-i
      write(10,*)k
      write(10,'(6e12.4)')wsp(1:N_ge,k,i+z0(ic),ic)
     end do
    end do
   endif
  end do
  close(10)
 end subroutine Ioniz_data
!====================================
 subroutine final_run_info
 if (pe0)then
  write (6,'(a14,i6,a5,e11.4,a11,e11.4)') ' final iter = ',iter,' t = ',tnow,' last dt = ',dt_loc
  !call tot_num_part(nptot_global)
  call cpu_time(unix_time_now)
  write(6,'(a22,f12.3,a10,i15)') ' Total time elapsed = ',unix_time_now-unix_time_begin
  write(6,*) ' END OF RUN'
 end if
 end subroutine final_run_info

 !---------------------------
 subroutine submem(rmem)
 real(dp),intent(out) :: rmem
 integer(kind=8) :: addr
 real(dp), allocatable :: am(:)

 allocate( am(100) )
 call memaddr( am, addr )
 deallocate(am)
 rmem=addr
 end subroutine submem
 !---------------------------

 subroutine max_pmemory_check()

 integer :: ndv1,ndv2
 integer :: ic
 real(dp) :: mem_loc(1),max_mem(1)
 !real(dp) :: adr

 mem_loc=0.
 max_mem=0.
 do ic=1,nsp
  if(allocated(spec(ic)%part))then
   ndv1=size(spec(ic)%part,1)
   ndv2=size(spec(ic)%part,2)
   mem_loc(1)=mem_loc(1)+real(ndv1*ndv2,dp)
  endif
 end do
 if(allocated(ebfp))then
  ndv1=size(ebfp,1)
  ndv2=size(ebfp,2)
  mem_loc(1)=mem_loc(1)+real(ndv1*ndv2,dp)
 endif
 if(Beam)then
  do ic=1,nsb
   if(allocated(bunch(ic)%part))then
    ndv1=size(spec(ic)%part,1)
    ndv2=size(spec(ic)%part,2)
    mem_loc(1)=mem_loc(1)+real(ndv1*ndv2,dp)
   endif
  end do
  if(allocated(ebfb))then
   ndv1=size(ebfb,1)
   ndv2=size(ebfb,2)
   mem_loc(1)=mem_loc(1)+real(ndv1*ndv2,dp)
  endif
 endif
 if(allocated(ebfp0))then
  ndv1=size(ebfp0,1)
  ndv2=size(ebfp0,2)
  mem_loc(1)=mem_loc(1)+real(ndv1*ndv2,dp)
 endif
 if(allocated(ebfp1))then
  ndv1=size(ebfp1,1)
  ndv2=size(ebfp1,2)
  mem_loc(1)=mem_loc(1)+real(ndv1*ndv2,dp)
 endif
 call allreduce_dpreal(MAXV,mem_loc,max_mem,1)
 mem_psize_max=kind(electron_charge_norm)*1.e-06*max_mem(1)

 !call submem(adr)
 !mem_loc(1)=adr
 !call allreduce_dpreal(MAXV,mem_loc,max_mem,1)
 !mem_max_addr=1.e-06*max_mem(1)

 end subroutine max_pmemory_check
 !---------------------------
 end module run_data_info