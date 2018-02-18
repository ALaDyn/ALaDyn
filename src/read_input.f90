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

 module read_input

 use control_bunch_input
 use mpi_var
 use phys_param
 use grid_and_particles
 use code_util

 implicit none
 !--- ---!
 integer, public :: nml_iounit,nml_ierr
 character(100), public :: nml_error_message
 data nml_iounit,nml_ierr,nml_error_message /1,0,''/
 !--- ---!
 contains


 subroutine read_main_input
 logical exist_nml
 logical exist_data

 inquire(file=input_namelist_filename, exist=exist_nml)
 inquire(file=input_data_filename, exist=exist_data)

 if (exist_nml) then
  call read_input_nml
 else if (exist_data) then
  call read_input_data
 else
  write(6,*) 'No usable input file (.nml or .data) has been found'
  stop 5
 endif
 END SUBROUTINE


 subroutine read_input_nml
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads the input namelist
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 NAMELIST/GRID/nx,ny,nz,ny_targ,k0,yx_rat,zx_rat
 NAMELIST/SIMULATION/LPf_ord,der_ord,str_flag,iform,model_id,&
  dmodel_id,ibx,iby,ibz,ibeam,ch_opt,fl_opt
 NAMELIST/TARGET_DESCRIPTION/nsp,nsb,ionz_lev,ionz_model,ion_min,ion_max,atomic_number,&
  mass_number,t0_pl,ppc,np_per_xc,np_per_yc,np_per_zc,lpx,lpy,&
  n_over_nc,np1,np2,L_disable_rng_seed
 NAMELIST/LASER/G_prof,nb_laser,t0_lp,xc_lp,tau_fwhm,w0_y,a0,lam0,lp_delay,&
 lp_offset,t1_lp,tau1_fwhm,w1_y,a1,lam1
 NAMELIST/MOVING_WINDOW/w_sh,wi_time,wf_time,w_speed
 NAMELIST/OUTPUT/nouts,iene,nvout,nden,npout,nbout,jump,pjump,gam_min,xp0_out,xp1_out,yp_out,tmax,cfl, &
  new_sim,id_new,dump,P_tracking,L_force_singlefile_output,time_interval_dumps,L_print_J_on_grid, &
  L_first_output_on_restart,L_env_modulus
 NAMELIST/TRACKING/tkjump,nkjump,txmin,txmax,tymin,tymax,tzmin,tzmax,t_in,t_out
 NAMELIST/MPIPARAMS/nprocx,nprocy,nprocz

 !--- reading grid parameters ---!
 yx_rat=-1.
 zx_rat=-1.
 open (nml_iounit,file=input_namelist_filename, status='old')
 read (nml_iounit,GRID,iostat=nml_ierr)
 nml_error_message='GRID'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error
 call consistency_check_grid

 !--- reading sim parameters ---!
 ch_opt=1.
 fl_opt=0.5
 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,SIMULATION,iostat=nml_ierr)
 nml_error_message='SIMULATION'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error
 !--- reading target parameters ---!
 mass_number(1:3) = 1.0
 ppc=-1
 np_per_xc=-1
 np_per_yc=-1
 np_per_zc=-1
 L_disable_rng_seed = .false.
 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,TARGET_DESCRIPTION,iostat=nml_ierr)
 nml_error_message='TARGET_DESCRIPTION'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error
 !call consistency_check_number_of_particles
 call consistency_check_number_of_particles_comp

 !--- reading laser parameters ---!
 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,LASER,iostat=nml_ierr)
 nml_error_message='LASER'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error


 !--- reading moving window parameters ---!
 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,MOVING_WINDOW,iostat=nml_ierr)
 nml_error_message='MOVING_WINDOW'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error


 !--- reading output parameters ---!
 time_interval_dumps = -1. !if -1 use classical output
 L_force_singlefile_output = .true.
 L_first_output_on_restart = .false.
 L_print_J_on_grid = .true.
 L_env_modulus = .true.
 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,OUTPUT,iostat=nml_ierr)
 nml_error_message='OUTPUT'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error

 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,TRACKING,iostat=nml_ierr)
 nml_error_message='TRACKING'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error

 !--- reading mpi decomposition ---!
 nprocx=-1
 nprocy=-1
 nprocz=-1
 npe_yz=-1
 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,MPIPARAMS,iostat=nml_ierr)
 nml_error_message='MPIPARAMS'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error


 END SUBROUTINE


 SUBROUTINE read_bunch_namelist
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads bunch namelist for PWFA
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 !--- *** namelist *** ---!
 NAMELIST/NUMBER_BUNCHES/ n_bunches, L_particles, L_intdiagnostics_pwfa, &
  L_intdiagnostics_classic,L_EMBunchEvolution,number_of_slices
 NAMELIST/BUNCHES/nb_tot,bunch_type,bunch_shape,rhob,xc_bunch,yc_bunch,zc_bunch, &
  gam,sxb,syb,epsy,epsz,dg,Charge_right,Charge_left,sigma_cut_bunch, &
  ppc_x_bunch,ppc_y_bunch,ppc_z_bunch


 !--- reading number of bunches ---!
 open(nml_iounit,file=input_namelist_filename, status='old')
 L_particles = .false.
 L_intdiagnostics_pwfa=.false.
 L_intdiagnostics_classic=.true.
 L_EMBunchEvolution=.true.
 number_of_slices = (/10,0,0,0/)
 read(nml_iounit,NUMBER_BUNCHES,iostat=nml_ierr)
 nml_error_message='NUMBER_BUNCHES'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error


 !--> initialization
 yc_bunch = 0.0
 zc_bunch = 0.0
 bunch_type = 1 !electron bunch
 bunch_shape= 1 !shape 1: bi-giassian
 !shape 2: trapezoidal (linear in Z, uniform with cutoff in R)
 !shape 3: trapezoidal-gaussian (linear in Z, gaussian in R)
 !shape 4: cylinder
 rhob=1.0 !relative density n_bunch/n_plasmabackground
 Charge_right=-1.0
 Charge_left =-1.0
 ppc_x_bunch=-1 !number of particle per cell 'x' direction :: this implies weighted option
 ppc_y_bunch=-1 !number of particle per cell 'x' direction :: this implies weighted option
 ppc_z_bunch=-1 !number of particle per cell 'x' direction :: this implies weighted option
 nb_tot=-1 !total number of bunch particles :: implies particle with same weight
 sigma_cut_bunch=3. !standard cut at 3-rms
 !-->
 open(nml_iounit,file=input_namelist_filename, status='old')
 read(nml_iounit,BUNCHES,iostat=nml_ierr)
 nml_error_message='BUNCHES'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error

 call select_number_of_bunch_particles()

 ! IF(1.le.n_bunches) call select_number_of_bunch_particles(ppc_x_bunch(1),ppc_y_bunch(1),ppc_z_bunch(1),ppc_bunch(1,1:3),nb_tot(1))
 ! IF(2.le.n_bunches) call select_number_of_bunch_particles(ppc_x_bunch(2),ppc_y_bunch(2),ppc_z_bunch(2),ppc_bunch(2,1:3),nb_tot(2))
 ! IF(3.le.n_bunches) call select_number_of_bunch_particles(ppc_x_bunch(3),ppc_y_bunch(3),ppc_z_bunch(3),ppc_bunch(3,1:3),nb_tot(3))
 ! IF(4.le.n_bunches) call select_number_of_bunch_particles(ppc_x_bunch(4),ppc_y_bunch(4),ppc_z_bunch(4),ppc_bunch(4,1:3),nb_tot(4))
 ! IF(5.le.n_bunches) call select_number_of_bunch_particles(ppc_x_bunch(5),ppc_y_bunch(5),ppc_z_bunch(5),ppc_bunch(5,1:3),nb_tot(5))

 end subroutine read_bunch_namelist



 SUBROUTINE read_bunch_TWISS_namelist
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads TWISS parameter for PWFA
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 NAMELIST/TWISS/L_TWISS,alpha_twiss,beta_twiss

 open(nml_iounit,file=input_namelist_filename, status='old')
 L_TWISS = .false. !bunch at waist
 read(nml_iounit,TWISS,iostat=nml_ierr)
 nml_error_message='TWISS'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error
 end subroutine read_bunch_TWISS_namelist


 SUBROUTINE read_poloidal_field_namelist
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads Poloidal Field parameters for PWFA
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 NAMELIST/BPOLOIDAL/L_Bpoloidal,B_ex_poloidal,radius_poloidal

 open(nml_iounit,file=input_namelist_filename, status='old')
 L_Bpoloidal = .false.
 B_ex_poloidal=0.0
 radius_poloidal=1.0
 read(nml_iounit,BPOLOIDAL,iostat=nml_ierr)
 nml_error_message='BPOLOIDAL'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error
 end subroutine read_poloidal_field_namelist



 SUBROUTINE read_nml_integrated_background_diagnostic
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads nml for BACKGROUND particle online DIAGNOSTIC
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 !--- *** namelist *** ---!
 NAMELIST/integrated_background_diagnostic/L_intdiagnostics_background, &
                                       gamma_cut_min,weights_cut_min,weights_cut_max

 !--- reading nml ---!
 open(nml_iounit,file=input_namelist_filename, status='old')
      L_intdiagnostics_background =.false.
      gamma_cut_min=0.0
      weights_cut_min=0.0
      weights_cut_max=1.0
 read(nml_iounit,integrated_background_diagnostic,iostat=nml_ierr)
 nml_error_message='integrated_background_diagnostic'
 close(nml_iounit)
 if(nml_ierr>0) call print_at_screen_nml_error

end subroutine read_nml_integrated_background_diagnostic



 subroutine write_read_nml
 character(len=12) :: output_filename
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C write namelist on a file 'input_  .nml'
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 NAMELIST/GRID/nx,ny,nz,ny_targ,k0,yx_rat,zx_rat
 NAMELIST/SIMULATION/LPf_ord,der_ord,str_flag,iform,model_id,&
  dmodel_id,ibx,iby,ibz,ibeam
 NAMELIST/TARGET_DESCRIPTION/nsp,nsb,ionz_lev,ionz_model,ion_min,ion_max,atomic_number,&
  mass_number,t0_pl,ppc,np_per_xc,np_per_yc,np_per_zc,lpx,lpy,&
  n_over_nc,np1,np2
 NAMELIST/LASER/G_prof,nb_laser,t0_lp,xc_lp,tau_fwhm,w0_y,a0,lam0,lp_delay,&
  lp_offset,t1_lp,tau1_fwhm,w1_y,a1,lam1
 NAMELIST/MOVING_WINDOW/w_sh,wi_time,wf_time,w_speed
 NAMELIST/OUTPUT/nouts,iene,nvout,nden,npout,nbout,jump,pjump,gam_min,xp0_out,xp1_out,yp_out,tmax,cfl, &
  new_sim,id_new,dump,P_tracking,L_force_singlefile_output,time_interval_dumps,L_print_J_on_grid, &
  L_first_output_on_restart,L_env_modulus
 NAMELIST/TRACKING/tkjump,nkjump,txmin,txmax,tymin,tymax,tzmin,tzmax,t_in,t_out
 NAMELIST/MPIPARAMS/nprocx,nprocy,nprocz
 NAMELIST/NUMBER_BUNCHES/ n_bunches, L_particles, L_intdiagnostics_pwfa, &
  L_intdiagnostics_classic,L_EMBunchEvolution,number_of_slices
 NAMELIST/BUNCHES/nb_tot,bunch_type,bunch_shape,rhob,xc_bunch,yc_bunch,zc_bunch, &
  gam,sxb,syb,epsy,epsz,dg,Charge_right,Charge_left,sigma_cut_bunch, &
  ppc_x_bunch,ppc_y_bunch,ppc_z_bunch
 NAMELIST/TWISS/L_TWISS,alpha_twiss,beta_twiss
 NAMELIST/BPOLOIDAL/L_Bpoloidal,B_ex_poloidal,radius_poloidal

 write(output_filename,29)'input_',id_new,'.nml'
29 format(a6,i2.2,a4)
 open(nml_iounit,file=output_filename)
 write(nml_iounit,nml=GRID,ERR=30)
 write(nml_iounit,nml=SIMULATION,ERR=30)
 write(nml_iounit,nml=TARGET_DESCRIPTION,ERR=30)
 write(nml_iounit,nml=LASER,ERR=30)
 write(nml_iounit,nml=MOVING_WINDOW,ERR=30)
 write(nml_iounit,nml=OUTPUT,ERR=30)
 if(P_tracking)write(nml_iounit,nml=TRACKING,ERR=30)
 write(nml_iounit,nml=MPIPARAMS,ERR=30)
 write(nml_iounit,nml=NUMBER_BUNCHES,ERR=30)
 write(nml_iounit,nml=BUNCHES,ERR=30)
 write(nml_iounit,nml=TWISS,ERR=30)
 write(nml_iounit,nml=BPOLOIDAL,ERR=30)
30 continue
 close(nml_iounit)
 END SUBROUTINE



 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C old namelist format
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 subroutine read_input_data
 !Default initialization like in input.nml
 yx_rat=-1.
 zx_rat=-1.
 mass_number(1:3) = 1.0
 ppc=-1
 np_per_xc=-1
 np_per_yc=-1
 np_per_zc=-1
 L_disable_rng_seed = .false.
 time_interval_dumps = -1. !if -1 use classical output
 L_force_singlefile_output = .true.
 L_first_output_on_restart = .false.
 L_print_J_on_grid = .true.
 nprocx=-1
 nprocy=-1
 nprocz=-1
 npe_yz=-1

 open (10,file=input_data_filename)
 read (10,*)nx,ny,nz,ny_targ
 read (10,*)
 read (10,*)k0, yx_rat
 read (10,*)
 read (10,*)LPf_ord,der_ord,str_flag,iform
 read (10,*)
 read (10,*)model_id,dmodel_id,ibeam
 read (10,*)
 read (10,*)ibx,iby,ibz,nsp,nsb
 read (10,*)
 read (10,*)ion_min(1),ion_max(1),atomic_number(1),mass_number(1)
 read (10,*)
 read (10,*)ion_min(2),ion_max(2),atomic_number(2),mass_number(2)
 read (10,*)
 read (10,*)ionz_lev,ionz_model
 read (10,*)
 read (10,*)t0_pl(1),t0_pl(2),t0_pl(3),t0_pl(4)
 read (10,*)
 read (10,*)np_per_xc(1:6)
 read (10,*)
 read (10,*)np_per_yc(1:6)
 read (10,*)
 read (10,*)t0_lp,xc_lp,w0_x,w0_y,a0,lam0
 read (10,*)
 read (10,*)lpx(1),lpx(2),lpx(3),lpx(4),lpx(5),lpx(6),lpx(7)
 read (10,*)
 read (10,*)lpy(1),lpy(2)
 read (10,*)
 read (10,*)n_over_nc,n1_over_n,n2_over_n
 read (10,*)
 read (10,*)w_sh,wi_time,wf_time,w_speed
 read (10,*)
 read (10,*)nouts,iene,nvout,nden,npout,nbout
 read (10,*)
 read (10,*)jump,pjump
 read (10,*)
 read (10,*)xp0_out,xp1_out,yp_out
 read (10,*)
 read (10,*)tmax,cfl
 read (10,*)
 read (10,*)new_sim,id_new,dump,npe_yz
 read (10,*)
 close(10)
 !the following parameters were not used in original input.data and, for compatibility reasons,
 !have not been added since input.data is deprecated in favour of the input.nml
 zx_rat=yx_rat
 call consistency_check_grid
 call consistency_check_number_of_particles_comp

 end subroutine read_input_data


 subroutine consistency_check_number_of_particles_comp
 if(all(ppc>=1)) then
  call from_ppc_to_npx_npy_npz
 elseif(all(np_per_zc==-1)) then
   np_per_zc=np_per_yc
 endif
 end subroutine consistency_check_number_of_particles_comp


 subroutine consistency_check_grid
 if( zx_rat < 0. .and. yx_rat > 0. ) then
  zx_rat = yx_rat
  !write(6,'(A)') "force zx_rat equal to yx_rat"
 else if ( zx_rat > 0. .and. yx_rat < 0. ) then
  yx_rat = zx_rat
  !write(6,'(A)') "force yx_rat equal to zx_rat"
 else if ( zx_rat < 0. .and. yx_rat < 0. ) then
  yx_rat = 1.
  zx_rat = 1.
  !write(6,'(A)') "force yx_rat=1 and zx_rat=1"
 endif
 end subroutine consistency_check_grid





 !------------------------------------------------------!
 subroutine consistency_check_number_of_particles !excluded temporarily because it doesn't deal with few cases, most of all np_per_xc=0

 !--->case 0: ppc is the only defined (new nml)
 if(all(ppc>=1) .and. all(np_per_xc==-1) .and. all(np_per_yc==-1) .and. all(np_per_zc==-1 ) ) then
  call from_ppc_to_npx_npy_npz

  !--->case 1: np_per_zc not defined: copy from np_per_yc
 elseif( all(ppc==-1) .and. all(np_per_xc>=0) .and. all(np_per_yc>=0) .and. all(np_per_zc==-1) ) then
  np_per_zc=np_per_yc

  !--->case 3: new and old methods both defined
 elseif( all(ppc>=1) .and. ( all(np_per_xc>=1) .or. all(np_per_yc>=1) .or. all(np_per_zc>=1) ) ) then
  np_per_xc=-1
  np_per_yc=-1
  np_per_zc=-1
  call from_ppc_to_npx_npy_npz

  !--->case default
 else
  ppc=8
  call from_ppc_to_npx_npy_npz
 endif

 end subroutine consistency_check_number_of_particles

 !------> Particle organisation
 subroutine from_ppc_to_npx_npy_npz
 !--->Subdivide ppc into np_per_xc,np_per_yc and theoretically into np_per_zc
 !logical isprime
 integer i,number_of_factors
 integer, allocatable, dimension(:) :: factors

 !verify input 'ppc' are not prime numbers
 do i=1,6
  do while(ISPRIME(ppc(i)))
   !if(pe0) write(6,'(A,I1,A,I3)')'The input parameter ppc(',i,') is prime - corrected to >',ppc(i)+1
   ppc(i)=ppc(i)+1
  enddo
 enddo

 !subdivide ppc into np_per_xc,yc,zc
 do i=1,6
  ALLOCATE(factors(ppc(i)/2))
  CALL PRIMEFACTORS(ppc(i),factors,number_of_factors)
  if(ndim==2) then
   np_per_xc(i)  = factors(1)
   np_per_yc(i)  = PRODUCT(factors(2:number_of_factors))
   !if(pe0) write(6,'(A,I2,A,I3,A,I3,A)') 'layer:',i,' > ',np_per_xc(i),'*',np_per_yc(i),' particles'
  elseif(ndim==3) then
   if(number_of_factors>2) then
    np_per_xc(i)  = factors(1)
    np_per_yc(i)  = factors(2)
    np_per_zc(i)  = PRODUCT(factors(3:number_of_factors))
   else
    np_per_xc(i)  = 1
    np_per_yc(i)  = factors(1)
    np_per_zc(i)  = factors(2)
   endif
   !if(pe0) write(6,'(A,I2,A,I3,A,I3,A,I3,A)') 'layer:',i,' > ',np_per_xc(i),'*',np_per_yc(i),'*',np_per_zc(i),' particles'
  endif
  deallocate(factors)
 enddo
 end subroutine from_ppc_to_npx_npy_npz

 FUNCTION ISPRIME(num)
 INTEGER, INTENT(IN) :: num  !input number
 INTEGER :: i
 LOGICAL :: ISPRIME

 ISPRIME=.TRUE.

 Do i=2,num-1
  IF( MOD(num,i) == 0 ) then
   ISPRIME=.FALSE.
   EXIT
  ENDIF
 EndDo
 END FUNCTION ISPRIME

 SUBROUTINE PRIMEFACTORS(num, factors, number_factors)
 INTEGER, INTENT(IN) :: num  !input number
 INTEGER,INTENT(OUT), DIMENSION((num/2))::factors !Array to store factors
 INTEGER, INTENT(INOUT) :: number_factors
 INTEGER :: i, n
 i = 2  !Eligible factor
 number_factors = 1  !Number of factors
 n = num !store input number into a temporary variable
 DO
  IF (MOD(n,i) == 0) THEN !If i divides 2, it is a factor
   factors(number_factors) = i
   number_factors = number_factors+1
   n = n/i
  ELSE
   i = i+1     !Not a factor. Move to next number
  END IF
  IF (n == 1) THEN
   !Since f is incremented after a factor is found
   number_factors = number_factors-1  !its value will be one more than the number of factors
   !Hence the value of number_factors is decremented
   EXIT
  END IF
 END DO
 END SUBROUTINE PRIMEFACTORS

 !--- *** *** *** ---!
 subroutine print_at_screen_nml_error
 !character(100) :: line
 !backspace(nml_iounit)
 !read(nml_iounit,fmt='(A)') line
 write(*,'(A)')    '*** *** *** *** *** *** *** *** *** *** *** ***'
 write(*,'(A)')    'Error in namelist:      '//trim(nml_error_message)
 !write(*,'(A)')    'Invalid namelist entry: '//trim(line)
 write(*,'(A,I5)') 'iostat type of error:   ',nml_ierr
 write(*,'(A)')    '*** *** *** *** *** *** *** *** *** *** *** ***'
 !stop
 end subroutine print_at_screen_nml_error

 !--- *** *** *** ---!
 subroutine select_number_of_bunch_particles()
   integer :: i

   Do i = 1,n_bunches

     if(ppc_x_bunch(i)==-1 .and. ppc_y_bunch(i)==-1 .and. ppc_z_bunch(i)==-1 .and. nb_tot(i)==-1) then
       ppc_bunch(i,:)=1
       nb_tot(i)=-1
     elseif(ppc_x_bunch(i)>=1 .and. ppc_y_bunch(i)>=1 .and. ppc_z_bunch(i)>=1 .and. nb_tot(i)>=1) then
       ppc_bunch(i,1)=ppc_x_bunch(i)
       ppc_bunch(i,2)=ppc_y_bunch(i)
       ppc_bunch(i,3)=ppc_z_bunch(i)
       nb_tot(i)=-1
     else
       ppc_bunch(i,1)=ppc_x_bunch(i)
       ppc_bunch(i,2)=ppc_y_bunch(i)
       ppc_bunch(i,3)=ppc_z_bunch(i)
       nb_tot(i)=-1
     endif

   EndDo
 end subroutine select_number_of_bunch_particles

 end module read_input
