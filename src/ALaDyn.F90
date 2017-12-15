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

 program ALaDyn

 use precision_def
 use pic_in
 use pic_out
 use pic_dump
 use pic_evolve_in_time
 use pic_out_util
 use read_input
 use pdf_moments
 use pwfa_output_addons
 use system_utilities
 use code_util

 implicit none

 integer :: last_iter,ngout
 logical :: Diag
 real(dp) :: tdia,dtdia,tout,dtout,tstart,mem_max_addr
 real(dp) :: dt_loc
 integer :: t_ind,ic,tk_ind

 mem_psize_max=0.0
 mem_max_addr=0.0


 call start

 call cpu_time(unix_time_now)
 unix_time_begin=unix_time_now
 unix_time_last_dump=unix_time_begin

 tnow=tstart
 ! iter=last_iter
 iter=0
 call diag_part_dist

 select case(mod_ord)
 case(1)
  call LP_cycle
 case(2)
  call ENV_cycle
 case(3)
  call BUNCH_cycle
 end select

 call timing
 call mpi_barrier(comm,error)
 call final_run_info
 call end_parallel


 !--------------------------

 contains

 !--------------------------

 subroutine LP_cycle

 call data_out(jump)
 dt_loc=dt
 t_ind=0
 tk_ind=0
 if(Ionization)then
  lp_max=2.*lp_max
  do ic=2,nsp_ionz
   call set_field_ioniz_wfunction(ion_min(ic-1),atomic_number(ic-1),ic,ionz_lev,ionz_model,lp_max,dt)
  end do
  if(Pe0) call Ioniz_data(lp_max,ion_min,atomic_number,ionz_lev,ionz_model)
  if(Impact_ioniz)then
   call set_impact_ioniz_wfunction(atomic_number(nsp_ionz-1),2)
   if(Pe0) call Impact_ioniz_data(atomic_number(nsp_ionz-1),z1_coll)
  endif
 endif
 if(P_tracking)then
  call initial_tparticles_select(spec(1),dt,txmin,txmax,tymin,tymax,tzmin,tzmax)
  tk_ind=tk_ind+1
  call t_particles_collect(spec(1),tk_ind)
 endif
 do while (tnow < tmax)

  call LP_run(tnow,dt_loc,iter,LPf_ord)

  call timing
  call data_out(jump)

  if (ier /= 0) then
   call error_message
   exit
  endif
  if (tnow+dt_loc >= tmax) dt_loc=tmax-tnow
 end do
 if (dump>0) call dump_data(iter,tnow)
 end subroutine LP_cycle

 !--------------------------

 subroutine ENV_cycle

 if(P_tracking)then
  tk_ind=tk_ind+1
  call initial_tparticles_select(spec(1),dt,txmin,txmax,tymin,tymax,tzmin,tzmax)
  call t_particles_collect(spec(1),tk_ind)
 endif
 call data_out(jump)
 dt_loc=dt
 tk_ind=0
 if(Ionization)then
  !lp_max=1.2*oml*a0
  do ic=2,nsp_ionz
   call set_field_ioniz_wfunction(ion_min(ic-1),atomic_number(ic-1),ic,ionz_lev,ionz_model,lp_max,dt)
  end do
  if(Pe0) call Ioniz_data(lp_max,ion_min,atomic_number,ionz_lev,ionz_model)
 endif
 do while (tnow < tmax)
  call ENV_run(tnow,dt_loc,iter,LPf_ord)

  if(P_tracking)then
   if(mod(iter,tkjump)==0)then
    tk_ind=tk_ind+1
    call t_particles_collect(spec(1),tk_ind)
   endif
  endif

  call timing
  call data_out(jump)

  if (ier /= 0) then
   call error_message
   exit
  endif
  if (tnow+dt_loc >= tmax) dt_loc=tmax-tnow
 end do
 if (dump>0) call dump_data(iter,tnow)
 end subroutine ENV_cycle

 !--------------------------

 subroutine BUNCH_cycle

 if(L_intdiagnostics_pwfa) then
  call bunch_output_struct(tdia,dtdia,tout,dtout)
 endif
 call bdata_out(jump)
 dt_loc=dt
 t_ind=0
 if(Ionization)then
  eb_max=0.8   !max beam field in atomic unit 0.514[TV/m] => here Ef_{max}=410[GV/m]
  do ic=2,nsp_ionz
   call set_field_ioniz_wfunction(ion_min(ic-1),atomic_number(ic-1),ic,ionz_lev,ionz_model,eb_max,dt)
  end do
  if(Pe0) call Ioniz_data(eb_max,ion_min,atomic_number,ionz_lev,ionz_model)
 endif

 do while (tnow < tmax)
  call BUNCH_run(tnow,dt_loc,iter)

  call timing
  if(L_intdiagnostics_pwfa) then
   call bunch_output_struct(tdia,dtdia,tout,dtout)
  endif
  call bdata_out(jump)

  if (ier /= 0) then
   call error_message
   exit
  endif
  if (tnow+dt_loc >= tmax) dt_loc=tmax-tnow
 end do
 if (dump>0) call dump_data(iter,tnow)

 end subroutine BUNCH_cycle
 !======================
 subroutine data_out(jump)
 integer,intent(in) :: jump
 integer :: i,ic,idata


 idata=iout
 if(Diag)then
  if (tnow>=tdia) then
   ienout=ienout+1
   call envar(ienout,tnow)
   tdia=tdia+dtdia
   if (pe0) write(6,'(a9,i3,a10,e11.4)')'rms data ',ienout,' at time =',tnow
  endif
 endif

 if (tnow>=tout) then
  call create_timestep_folder(iout)
  tout=tout+dtout
  if(Diag)then
   if(pe0)call en_data(ienout,iter,idata)
  endif
  if(P_tracking)then
   if(tk_ind >1)then
   if(tk_ind <= track_tot_nstep)call track_part_pdata_out(tnow,tk_ind,1)
   endif
  endif
!==================
  if(nvout>0)then
   if (mod_ord==2) then
    if(L_env_modulus)then
     i=0
     call env_fields_out(env,tnow,i,jump)
     if(Two_color)call env_fields_out(env1,tnow,-1,jump)
    else
     if(Two_color)then
      do i=1,2
       call env_two_fields_out(env,env1,tnow,i,jump)
      end do
     else
      do i=1,2
       call env_fields_out(env,tnow,i,jump)
      end do
     endif
    endif
   endif
   do i=1,nvout
    if(L_force_singlefile_output) then
     call fields_out(ebf,tnow,i,i,jump)      !i to label field name
    else
     call fields_out_new(ebf,tnow,i,i,jump)
    endif
   end do
  endif
  if (nden>0) then
   do i=1,nsp
    call prl_den_energy_interp(i)
    do ic=1,min(2,nden)
     call den_energy_out(tnow,i,ic,ic,jump)
    end do
   enddo
   if(nden > 2)then
    call set_wake_potential
    call den_energy_out(tnow,0,nden,1,jump)  !data on jc(1) for wake potential 
   endif
  endif
  if(Hybrid)then
   do i=1,nfcomp
    call fluid_den_mom_out(up,tnow,i,nfcomp,jump)
   end do
  endif
  if(Ionization)call part_ionz_out(tnow)
  if(gam_min >1.)call part_high_gamma_out(gam_min,tnow)
  if (npout>0) then
   if (npout<=nsp) then
    call part_pdata_out(tnow,xp0_out,xp1_out,yp_out,npout,pjump)
   else
    do i=1,nsp
     call part_pdata_out(tnow,xp0_out,xp1_out,yp_out,i,pjump)
    end do
   endif
  endif
  if (dump>0 .and. time_interval_dumps < 0.0) then
   if (iter>0) call dump_data(iter,tnow)
  endif
  iout=iout+1
 endif

 call cpu_time(unix_time_now)

 if((unix_time_now - unix_time_last_dump) > time_interval_dumps .and. time_interval_dumps > 0.0) then
  call dump_data(iter,tnow)
 endif

 end subroutine data_out

 !--------------------------
 subroutine bdata_out(jump)

 integer,intent(in) :: jump
 integer :: i,ic,idata

 idata=iout

 if(Diag)then
  if (tnow>=tdia) then
   ienout=ienout+1
   !uncomment for extracting lineout in enbvar
   !call collect_bunch_and_plasma_density(nsb,1) !data is now stored on jc(1)
   !---gather data for lineout---!
   !---> lineout total density
   if(L_intdiagnostics_pwfa) then
    call collect_bunch_and_plasma_density(0,1)
   endif
   !---!
   call enbvar(ienout,tnow)
   !---!
   call envar(ienout,tnow)
   tdia=tdia+dtdia
   if (pe0) then
    write(6,'(a9,i3,a10,e11.4)')'rms data ',ienout,' at time =',tnow
   endif
  endif
 endif

 if (tnow>=tout) then
  tout=tout+dtout

  call create_timestep_folder(iout)

  if(Diag)then
   if(pe0)then
    call en_data(ienout,iter,idata)
    call en_bdata(ienout,iter,idata)
   endif
  endif

  if(nvout>0)then
   do i=1,min(nvout,nfield)
    if(L_force_singlefile_output) then
     call fields_out(ebf,tnow,i,i,jump)    ! second index to label field
    else
     call fields_out_new(ebf,tnow,i,i,jump)
    endif
   end do

   if      (L_print_J_on_grid .AND. L_force_singlefile_output) then
     call fields_out(jc,tnow,1,0,jump)     ! 0 for Jx current
   else if  (L_print_J_on_grid .AND. L_force_singlefile_output) then
     call fields_out_new(jc,tnow,1,0,jump) ! 0  to label Jx field
   endif

   do i=1,nbfield
    if(L_force_singlefile_output) then
     call bfields_out(ebf_bunch,ebf1_bunch,tnow,i,jump)
    else
     call fields_out_new(ebf_bunch,tnow,i,i+6,jump)
    endif
   end do
  endif
  if(nden>0)then
   ic=0
   call prl_bden_energy_interp(ic)
   call bden_energy_out(tnow,1,jump)
   !============= bunch density
   do i=1,nsp
    call prl_den_energy_interp(i)
    do ic=1,min(2,nden)
     call den_energy_out(tnow,i,ic,ic,jump)
    end do
   enddo
   if(nden>2)then
    call set_wake_potential
    call den_energy_out(tnow,0,nden,1,jump)  !data on jc(1) for wake potential 
   endif
  endif
  if(nbout> 0)then
   do i=1,nsb
    call part_bdata_out(tnow,i,pjump)
   end do
  endif
  if(Part)then
   if(Ionization)call part_ionz_out(tnow)
   if(gam_min >1.)call part_high_gamma_out(gam_min,tnow)
   if(npout> 0)then
    if(npout<=nsp)then
     call part_pdata_out(tnow,xp0_out,xp1_out,yp_out,npout,pjump)
    else
     do i=1,nsp
      call part_pdata_out(tnow,xp0_out,xp1_out,yp_out,i,pjump)
     end do
    endif
   endif
  endif
  if(dump>0 .and. time_interval_dumps < 0.)then
   if(iter>0)call dump_data(iter,tnow)
  endif
  iout=iout+1
 endif

 call cpu_time(unix_time_now)

 if((unix_time_now - unix_time_last_dump) > time_interval_dumps .and. time_interval_dumps > 0.0) then
  if(pe0) write(6,*) '3D dump data being written'
  call dump_data(iter,tnow)
 endif
 end subroutine bdata_out
 !--------------------------

 subroutine diag_part_dist
 integer :: j, jj, jjj, lun
 if (pe0) then
  lun=999
  open(lun,file='part.dist.dat')
  write(lun,*)' Local node grid points in y-z-x coord'
  write(lun,*)'loc_ymin'
  write(lun,'(4e13.6)')loc_ygrid(0:npe_yloc-1)%gmin
  write(lun,*)'loc_ymax'
  write(lun,'(4e13.6)')loc_ygrid(0:npe_yloc-1)%gmax
  write(lun,*)'loc_zmin'
  write(lun,'(4e13.6)')loc_zgrid(0:npe_zloc-1)%gmin
  write(lun,*)'loc_zmax'
  write(lun,'(4e13.6)')loc_zgrid(0:npe_zloc-1)%gmax
  write(lun,*)'loc_xmin'
  write(lun,'(4e13.6)')loc_xgrid(0:npe_xloc-1)%gmin
  write(lun,*)'loc_xmax'
  write(lun,'(4e13.6)')loc_xgrid(0:npe_xloc-1)%gmax
  write(lun,*)'Node P-distribution'
  if (Part) then
   write(lun,'(a9,i6,a5,i4)')'at iter =',iter,'dcmp=',dcmp_count
   do j=0,npe_zloc-1
    write(lun,*)'npe_zloc=',j
    do jj=0,npe_xloc-1
     write(lun,*)'npe_xloc=',jj
     do jjj=1,nsp_run
      write(lun,*)'nsp=',jjj
      write(lun,'(4i8)')loc_npart(0:npe_yloc-1,j,jj,jjj)
     end do
    end do
   end do
  end if
  if (Beam) then
   write(lun,'(a24,i6,a5,i4)')'beam particles at iter =',iter,'dcmp=',dcmp_count
   do j=0,npe_zloc-1
    write(lun,*)'npe_zloc=',j
    do jj=0,npe_xloc-1
     write(lun,*)'npe_xloc=',jj
     do jjj=1,nsp_run
      write(lun,*)'nsp=',jjj
      write(lun,'(4i8)')loc_nbpart(0:npe_yloc-1,j,jj,jjj)
     end do
    end do
   end do
  endif
  close(lun)
 end if
 end subroutine diag_part_dist


 subroutine timing
 integer,parameter :: write_every=100
 if (mod(iter,write_every)==0) then
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

 !---------------------------

 subroutine start

 integer :: nxp,nyp,nzp,ns_ioniz
 real(dp), parameter :: opt_der=1.0

 !enable loop to attach with gdb only if really needed
 !WARNING if enabled without needed, the program sleeps at start without doing anything!
#ifdef ENABLE_GDB_ATTACH
 call gdbattach
#endif

 !Read parameters from input.nml file
 call read_main_input
 call read_nml_integrated_background_diagnostic
 if (model_id > 4) then
  call read_bunch_namelist
  call read_bunch_TWISS_namelist
  call read_poloidal_field_namelist
  nsb=n_bunches
 endif
 ns_ioniz=0
 spl_ord=2
 RK_ord=0
 xf=xc_lp+t0_lp
 xf1=xc1_lp+t1_lp
 ngout=max(1,nz/jump)*nx*ny/(jump*jump)
 part_dcmp=.false.
 call check_grid_size
 !===========================
 if(model_id==6)then
  nsb=1
  call start_parallel(nd2,nsp,nsb+1,model_id)
 else
  call start_parallel(nd2,nsp,nsb,model_id)
 endif
 if(mpi_err >0)then
  if(pe0)write(6,*)' ERROR in mpi domain decomposition'
  call end_parallel
  stop
 endif
 if(pe0)then
  write(6,*)                    '========================================'
  write(6,'(a4,a6,a4,i1,a1,i2,a23)') ' =  ',sw_name,' v. ',major_version,'.',minor_version,'                      ='
  write(6,*)                    '========================================'
 endif
 if (pe0) call create_initial_folders
 if (pe0) call write_read_nml
 call param
 !=========================
 if(ndim==1) Stretch=.false.
 call set_fyzxgrid(npe_yloc,npe_zloc,npe_xloc,sh_ix)
 if (Stretch) call set_str_ind(npe_yloc,npe_zloc,ndim)
 if(Impact_ioniz)ns_ioniz=nsp              !only for collisional ionization
 !=====================
 ! Allocates basic arrays, defines grid parameters, boundary index etc
 call w_alloc(opt_der)      !local arrays and matrix for space derivatives
 mem_size=0
 mem_psize=0
 if (nvout>nfield) nvout=nfield
 if (.not.Part) then
  if (model_id<5) then
   nden=0
   npout=0
  endif
 endif
 !============
 !                              The local grid right boundary
 nyp=loc_ygrid(imody)%p_ind(2)  !Ny_loc+2
 nzp=loc_zgrid(imodz)%p_ind(2)  !Nz_loc+2
 nxp=loc_xgrid(imodx)%p_ind(2)  !Nx_loc+2
 !====== Fields and current arrays allocated on [1: N_loc+5]
 !==========================
 call v_alloc(nxp,nyp,nzp,nfield,nj_dim,&
             ndim,ns_ioniz,ibeam,LPf_ord,der_ord,Envelope,Two_color,Comoving,mem_size)
 if(Hybrid)call fluid_alloc(nxp,nyp,nzp,nfcomp,ndim,LPF_ord,mem_size)
 if (Beam) then
  call bv_alloc(nxp,nyp,nzp,nbfield,ndim,ibeam,mem_size)
 endif
 if (Stretch) then
  if (pe0) call str_grid_data
 endif
 if (iform==1) iform=0    !only esirkepov schemes used
 call mpi_valloc(loc_nxc_max,loc_nyc_max,loc_nzc_max,nfield,iform,mem_size)
 call set_output_grid(jump)   !allocates wdata(); counts nhx,nhy,nhz local grid points
 if(pe0)then
  write(6,*)'START OF RUN'
  write(6,*)'Running on ',npe,' cpu'
  write(6,'(a32,3i4)')' MPI decomposition along x-y-z: ',npe_xloc,npe_yloc,npe_zloc
 endif
 !====To activate/disactivate diagnostics in envar() en_data routines=========
 Diag=.true.
 if(iene==0)then
  Diag = .false.
  iene=1
 endif


 select case (new_sim)

 case (0) ! Set initial conditions, allocate fields and particles
  iout=id_new
  ienout=0
  call init
  tstart=0.0
  last_iter=0
  tdia=tstart
  tout=tstart
  ! to count outputs in energy-data (iene+1 times)
  ! in general data (nouts+1 times)
  dtout=(tmax-tstart)/nouts
  dtdia=(tmax-tstart)/iene

 case (1) ! reads from dump evolved data
  if (.not.L_first_output_on_restart) then
   iout=id_new
   ienout=0
  else
   iout=id_new+1
   ienout=0
  endif
  call restart(last_iter,tstart)
  call mpi_barrier(comm,error)
  if(pe0)then
   write(6,*)' Dump data read completed'
  endif
  call set_fxgrid(npe_xloc,sh_ix)
  tmax=tmax+tstart
  dtout=(tmax-tstart)/nouts
  dtdia=(tmax-tstart)/iene
  if(.not.L_first_output_on_restart) then
   tdia=tstart+dtdia
   tout=tstart+dtout
  else
   tdia=tstart
   tout=tstart
  endif

 end select

 if(Part)then
  if(prl)then
   call part_numbers
   call max_pmemory_check()
  endif
 endif

 if (pe0)then
  call initial_run_info(new_sim)
 endif
 end subroutine start

 !---------------------------
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

 !===================
 ! subroutine write_ionization_probability_alternative(Ef_max,z0,zmax,ionz_sch,An)
 ! real(dp),intent(in) :: Ef_max
 ! integer,intent(in) :: z0,zmax,ionz_sch,An
 ! integer :: i,k,zm_loc
 !
 ! open(10,file='ionization_summary.dat')
 !  write(10,*)'Ef gr, Zmax, z0   ionz_sch    An    '
 !  write(10,'(5i6)')N_ge,zmax,z0,ionz_sch,An
 !  write(10,*)'  Max Ef       dt      Omega_au  '
 !  write(10,'(3E11.4)')Ef_max,dt,omega_a
 !  write(10,*)' E_c       E_b           I_pot(a.u.)  '
 !   do i=1,zmax
 !    write(10,'(4E12.4)')E_c(i),E_b(i),wec(i)
 !   end do
 ! close(10)
 !
 ! open(10,file='ionization_rate.dat')
 ! write(10,2001) ('Z_',i,i=z0,zmax) !header
 ! DO i=1,N_ge !add headers
 !  write(10,'(100e14.5)') wi(1:zmax-z0,i)
 ! ENDDO
 ! close(10)
 !
 ! if(ionz_sch==1) then
 !  open(10,file='ionization_probability.dat')
 !   write(10,2001) ('Z_',i,i=z0,zmax) !header
 !    DO i=1,N_ge
 !     write(10,'(6e12.4)') wspec(i,0,z0:zmax-1)
 !    ENDDO
 !  close(10)
 ! else if(ionz_sch==2) then
 !  zm_loc=zmax-z0
 !  open(10,file='ionization_probability_multilevel.dat')
 !   do i=0,zm_loc-1
 !    write(10,*)i
 !     do k=0,zm_loc-i
 !      write(10,*)k
 !      write(10,'(6e12.4)')wspec(1:N_ge,k,i+z0)
 !     end do
 !   end do
 !  close(10)
 ! endif
 !
 ! 2001 format ( 100(A,I2.2,'  ')) !format header ionization
 ! end subroutine write_ionization_probability_alternative
 !===================

 subroutine Impact_ioniz_data(An,z0_coll)
 integer,intent(in) :: An,z0_coll
 integer :: j,k,kk

 open(10,file='diag_impact.dat')
 write(10,*)'Ne  z0    An    '
 write(10,'(3i6)')Nec,z0_coll,An
 write(10,*)nl_indx(z0_coll:An-1)
 do j=z0_coll,An-1
  kk=nl_indx(j)
  write(10,*)'potentials for ion'
  do k=1,kk
   write(10,'(e12.4)')P_nl(k,j)
  end do
  !write(10,*)'energy'
  !write(10,'(5e12.4)')E_coll(1:Nec,j)
  do k=1,kk
   write(10,*)'Cross section'
   write(10,'(5e12.4)')sigma_coll(1:Nec,k,j)
  end do
 end do
 close(10)
 end subroutine Impact_ioniz_data

 subroutine str_grid_data
 real(dp),allocatable :: dern(:),der_ex(:)
 integer :: i,j,k
 real(dp) :: yy,y1,y2
 allocate(dern(ny+1),der_ex(ny+1))
 do i=1,ny+1
  yy=y(i)/w0_y
  dern(i)=exp(-yy*yy)
  yy=yy/w0_y
  der_ex(i)=-2.*yy*dern(i)
 end do
 do i=2,ny+1
  y2=yh(i)/w0_y
  y2=exp(-y2*y2)
  y1=yh(i-1)/w0_y
  y1=exp(-y1*y1)
  dern(i)=dy1(i)*dy_inv*(y2-y1)
 end do
 dern(1)=dern(2)
 open(20,file='sgrid_map.data')
 write(20,*)'test'
 do i=1,ny
  write(20,'(2E11.4)')dern(i),der_ex(i)
 end do
 write(20,*)'max ncell',loc_nyc_max,loc_nzc_max
 write(20,'(5e11.4)')y(1:ny+1)
 write(20,'(a11,2e11.4)')'y smin,smax',str_ygrid%smin,str_ygrid%smax
 write(20,'(a11,2e11.4)')'z smin,smax',str_zgrid%smin,str_zgrid%smax
 write(20,*)'y-ng '
 write(20,'(4i4)')loc_ygrid(0:npe_yloc-1)%ng
 do j=0,npe_zloc-1
  do i=0,npe_yloc-1
   do k=0,8
    if(str_indx(i,j)==k)then
     write(20,'(a5,3i8)')'sind ',k,i,j
     write(20,'(2E11.4)')loc_ygrid(i)%gmin,loc_zgrid(j)%gmin
    endif
   enddo
  end do
 end do
 do i=0,npe_yloc-1
  write(20,*)'imody j1  ,j2  '
  write(20,'(3i4)')i,loc_ygrid(i)%p_ind(1:2)
  j=loc_ygrid(i)%ng
  write(20,'(a12,i4)')'loc y-grid  ',j
  write(20,'(4e11.4)')loc_yg(1:j,1,i)
 end do
 do j=0,npe_zloc-1
  write(20,*)'imodz,k1   ,k2  '
  write(20,'(3i4)')j,loc_zgrid(j)%p_ind(1:2)
  i=loc_zgrid(j)%ng
  write(20,'(a12,i4)')'loc z-grid  ',i
  write(20,'(4e11.4)')loc_zg(1:i,1,j)
 end do
 close(20)
 deallocate(dern,der_ex)
 end subroutine str_grid_data

 !---------------------------
 !---------------------------

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
   if(der_ord==4)write(60,*)'  Forth-order Mawvell solver only for (E,B) fields'
   if(LPf_ord>2)then
    write(60,*)'  RK multi-step fourth order time scheme '
    write(60,*)'  Explicit fourth order Space Derivative'
   endif
  endif
  if(Charge_cons)then
   write(60,*)'  Continuity equation enforced by Esirkepov scheme'
  else
   write(60,*)'  No charge preserving scheme activated'
  endif
  write(60,*)'***************GRID**********************'
  write(60,'(a18,3i8)')'  total grid size ',nx,ny,nz
  write(60,'(a18,3i8)')'  local grid size ',nx_loc,ny_loc,nz_loc
  write(60,'(a27,i3)')'  Cartesian grid dimension ',ndim
  if(curr_ndim==2)then
   write(60,*)' Current components: [Jx,Jy] '
   write(60,*)' Field components: [Ex,Ey,Bz] '
  else
   write(60,*)' Current components: [Jx,Jyi,Jz] '
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
  write(60,'(a8,f5.2,a18,f5.2)')'  w0_y= ',w0_y,'   focal spot =   ',lp_rad
  write(60,*)' Longitudinal scales'
  write(60,'(a8,f5.2,a18,f5.2)')'  w0_x= ',w0_x,'   tau_fwhm(fs) = ',tau_FWHM
  write(60,'(a13,f5.2,a13,f5.2)')'  wavelength=',lam0,'   frequency=',oml
  write(60,'(a17,f6.2,a17,f6.2)')'  Initial focus =',xf,'   Pulse center= ',xc_lp
  write(60,'(a29,e11.4)')'  Diffraction length Z_Rayl= ',ZR
  write(60,'(a25,f5.2)')'  Strength parameter a0= ',a0
  write(60,'(a33,f5.2,a6)')'  Max transverse field at focus= ',E0*a0*oml,'(TV/m)'
  write(60,'(a13,e13.3,a10)')'  Intensity= ',lp_intensity,'(e18W/cm2)'
  write(60,'(a9,e13.3,a4)')'  Power = ',lp_pow,'(TW)'
  write(60,'(a10,e13.3,a3)')'  Energy= ',lp_energy,'(J)'
  write(60,'(a30,i4)')'  Number of main laser pulses= ',nb_laser
  if(nb_laser >1)write(60,'(a29,f5.2)')'  Distance among lp centers= ',lp_delay
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
   end if
 write(60,*)'**********TARGET PLASMA PARAMETERS***********'
  if(Part)then
   write(60,'(a26,e11.4,a10)')'  Electron number density ',n0_ref,'[10^18/cc]'
   write(60,'(a21,f5.2)')'  Plasma wavelength= ',lambda_p
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
     write(60,*)' 2R_ext    2dr       filling fact '
     write(60,'(3e11.4)')lpy(1)+lpy(2),lpy(1),lpy(3)
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
   write(60,*)' unit length is 1mm, unit density is n0=10^14/cc'
   write(60,*)'  Lambda , Omega_p    n_over_n0  '
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
  write(6,'(a20,e12.5)')' Max part  address= ',mem_max_addr
 endif
 write(6,*)' Particle min/max distr. '
 write(6,'(i10,a1,i10)')np_min,' ',np_max
 write(6,'(a18,2i8)')' where pmin/pmax  ',pe_npmin,pe_npmax
 write(6,*)'******************************************************'
 end  subroutine initial_run_info

 !---------------------------

 subroutine final_run_info
 if (pe0)then
  write (6,'(a7,i6,a5,e11.4,a11,e11.4)') 'iter = ',iter,' t = ',tnow,' last dt = ',dt_loc
  call tot_num_part(nptot_global)
  call cpu_time(unix_time_now)
  write(6,'(a15,f12.3,a10,i15)') 'Time elapsed = ',unix_time_now-unix_time_begin,', nptot = ', nptot_global
  write(6,*) 'END OF RUN'
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
 !integer :: np
 real(dp) :: mem_loc(1),max_mem(1)
 real(dp) :: adr

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

 call submem(adr)
 mem_loc(1)=adr
 call allreduce_dpreal(MAXV,mem_loc,max_mem,1)
 mem_max_addr=1.e-06*max_mem(1)

 end subroutine max_pmemory_check


 !---------------------------

 end program ALaDyn
