 !*****************************************************************************************************!
 !             Copyright 2008-2016 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
 !                                 Alberto Marocchino                                                  !
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
 contains


 subroutine read_main_input
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads the input namelist
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 NAMELIST/OLD_INPUT/L_read_input_data
 NAMELIST/GRID/nx,ny,nz,ny_targ,k0,yx_rat,zx_rat
 NAMELIST/SIMULATION/LPf_ord,der_ord,str_flag,iform,model_id,&
  dmodel_id,ibx,iby,ibz,ibeam
 NAMELIST/TARGET_DESCRIPTION/nsp,nsb,ion_min,ion_max,atomic_number,&
  mass_number,ionz_lev,ionz_model,t0_pl,ppc,np_per_xc,np_per_yc,np_per_zc,lpx,lpy,&
  n_over_nc,n1_over_n,n2_over_n,L_disable_rng_seed
 NAMELIST/LASER/t0_lp,xc_lp,w0_x,w0_y,a0,lam0
 NAMELIST/MOVING_WINDOW/w_sh,wi_time,wf_time,w_speed
 NAMELIST/OUTPUT/nouts,iene,nvout,nden,npout,nbout,jump,pjump,xp0_out,xp1_out,yp_out,tmax,cfl, &
  new_sim,id_new,dump,L_force_singlefile_output,time_interval_dumps,L_print_J_on_grid, &
  L_first_output_on_restart
 NAMELIST/MPIPARAMS/nprocx,nprocy,nprocz

 !--- reading OLD_INPUT ---!
 L_read_input_data = .false.
 open(1,file='input.nml', status='old')
 read(1,OLD_INPUT,ERR=16,END=16)
16 continue
 close(1)

 !--- reading grid parameters ---!
 yx_rat=-1.
 zx_rat=-1.
 open(1,file='input.nml', status='old')
 read(1,GRID,ERR=17,END=17)
17 continue
 close(1)
 call nml_consistency_check_grid

 !--- reading sim parameters ---!
 open(1,file='input.nml', status='old')
 read(1,SIMULATION,ERR=18,END=18)
18 continue
 close(1)

 !--- reading target parameters ---!
 mass_number(1:3) = 1.0
 ppc=-1
 np_per_xc=-1
 np_per_yc=-1
 np_per_zc=-1
 L_disable_rng_seed = .false.
 open(1,file='input.nml', status='old')
 read(1,TARGET_DESCRIPTION,ERR=19,END=19)
19 continue
 close(1)
 !call nml_consistency_check_number_of_particles
 call nml_consistency_check_number_of_particles_comp

 !--- reading laser parameters ---!
 open(1,file='input.nml', status='old')
 read(1,LASER,ERR=20,END=20)
20 continue
 close(1)

 !--- reading moving window parameters ---!
 open(1,file='input.nml', status='old')
 read(1,MOVING_WINDOW,ERR=21,END=21)
21 continue
 close(1)

 !--- reading output parameters ---!
 time_interval_dumps = -1. !if -1 use classical output
 L_force_singlefile_output = .true.
 L_first_output_on_restart = .false.
 L_print_J_on_grid = .true.
 open(1,file='input.nml', status='old')
 read(1,OUTPUT,ERR=22,END=22)
22 continue
 close(1)


 !--- reading mpi decomposition ---!
 nprocx=-1
 nprocy=-1
 nprocz=-1
 npe_yz=-1
 open(1,file='input.nml', status='old')
 read(1,MPIPARAMS,ERR=23,END=23)
23 continue
 close(1)

 END SUBROUTINE


 SUBROUTINE read_bunch_namelist
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads bunch namelist for PWFA
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 !--- *** namelist *** ---!
 NAMELIST/NUMBER_BUNCHES/ n_bunches, L_particles, L_intdiagnostics_pwfa, &
  L_intdiagnostics_classic,number_of_slices
 NAMELIST/BUNCH1/rho_b_1,gamma_1,xb_1,yb_1,zb_1,sx_1,sy_1,epsy_1,epsz_1,dg_1,np_1,bunch_type_1
 NAMELIST/BUNCH2/rho_b_2,gamma_2,xb_2,yb_2,zb_2,sx_2,sy_2,epsy_2,epsz_2,dg_2,np_2,bunch_type_2
 NAMELIST/BUNCH3/rho_b_3,gamma_3,xb_3,yb_3,zb_3,sx_3,sy_3,epsy_3,epsz_3,dg_3,np_3,bunch_type_3
 NAMELIST/BUNCH4/rho_b_4,gamma_4,xb_4,yb_4,zb_4,sx_4,sy_4,epsy_4,epsz_4,dg_4,np_4,bunch_type_4
 NAMELIST/BUNCH5/rho_b_5,gamma_5,xb_5,yb_5,zb_5,sx_5,sy_5,epsy_5,epsz_5,dg_5,np_5,bunch_type_5

 !--- reading number of bunches ---!
 open(1,file='input.nml', status='old')
 L_particles = .false.
 L_intdiagnostics_pwfa=.false.
 L_intdiagnostics_classic=.true.
 number_of_slices = (/10,0,0,0/)
 read(1,NUMBER_BUNCHES,ERR=10,END=10)
10 continue
 close(1)

 !--- reading BUNCH1 ---!
 !--> initialization
 yb_1 = 0.0
 zb_1 = 0.0
 !-->
 IF( 1 .le. n_bunches) then
  open(1,file='input.nml', status='old')
  read(1,BUNCH1,ERR=11,END=11)
11 continue
  close(1)
  !passing values to ALaDyn's parameter
  nb_tot(1)     = np_1
  bunch_type(1) = bunch_type_1
  rhob(1)       = rho_b_1
  xc_bunch(1)   = xb_1
  yc_bunch(1)   = yb_1
  zc_bunch(1)   = zb_1
  gam(1)        = gamma_1
  sxb(1)        = sx_1
  syb(1)        = sy_1
  epsy(1)       = epsy_1
  epsz(1)       = epsz_1
  dg(1)         = dg_1
 END IF



 !--- reading BUNCH2 ---!
 !--> initialization
 yb_2 = 0.0
 zb_2 = 0.0
 !-->
 IF( 2 .le. n_bunches) then
  open(1,file='input.nml', status='old')
  read(1,BUNCH2,ERR=12,END=12)
12 continue
  close(1)
  !passing values to ALaDyn's parameter
  nb_tot(2)     = np_2
  bunch_type(2) = bunch_type_2
  rhob(2)       = rho_b_2
  xc_bunch(2)   = xb_2
  yc_bunch(2)   = yb_2
  zc_bunch(2)   = zb_2
  gam(2)        = gamma_2
  sxb(2)        = sx_2
  syb(2)        = sy_2
  epsy(2)       = epsy_2
  epsz(2)       = epsz_2
  dg(2)         =dg_2
 END IF



 !--- reading BUNCH3 ---!
 !--> initialization
 yb_3 = 0.0
 zb_3 = 0.0
 !-->
 IF( 3 .le. n_bunches) then
  open(1,file='input.nml', status='old')
  read(1,BUNCH3,ERR=13,END=13)
13 continue
  close(1)
  !passing values to ALaDyn's parameter
  nb_tot(3)     = np_3
  bunch_type(3) = bunch_type_3
  rhob(3)       = rho_b_3
  xc_bunch(3)   = xb_3
  yc_bunch(3)   = yb_3
  zc_bunch(3)   = zb_3
  gam(3)        = gamma_3
  sxb(3)        = sx_3
  syb(3)        = sy_3
  epsy(3)       = epsy_3
  epsz(3)       = epsz_3
  dg(3)         = dg_3
 END IF



 !--- reading BUNCH4 ---!
 !--> initialization
 yb_4 = 0.0
 zb_4 = 0.0
 !-->
 IF( 4 .le. n_bunches) then
  open(1,file='input.nml', status='old')
  read(1,BUNCH4,ERR=14,END=14)
14 continue
  close(1)
  !passing values to ALaDyn's parameter
  nb_tot(4)     = np_4
  bunch_type(4) = bunch_type_4
  rhob(4)       = rho_b_4
  xc_bunch(4)   = xb_4
  yc_bunch(4)   = yb_4
  zc_bunch(4)   = zb_4
  gam(4)        = gamma_4
  sxb(4)        = sx_4
  syb(4)        = sy_4
  epsy(4)       = epsy_4
  epsz(4)       = epsz_4
  dg(4)         = dg_4
 END IF



 !--- reading BUNCH5 ---!
 !--> initialization
 yb_5 = 0.0
 zb_5 = 0.0
 !-->
 IF( 5 .le. n_bunches) then
  open(1,file='input.nml', status='old')
  read(1,BUNCH5,ERR=15,END=15)
15 continue
  close(1)
  !passing values to ALaDyn's parameter
  nb_tot(5)     = np_5
  bunch_type(5) = bunch_type_5
  rhob(5)       = rho_b_5
  xc_bunch(5)   = xb_5
  yc_bunch(5)   = yb_5
  zc_bunch(5)   = zb_5
  gam(5)        = gamma_5
  sxb(5)        = sx_5
  syb(5)        = sy_5
  epsy(5)       = epsy_5
  epsz(5)       = epsz_5
  dg(5)         = dg_5
 END IF

 end subroutine read_bunch_namelist



 SUBROUTINE read_bunch_TWISS_namelist
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads TWISS parameter for PWFA
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 NAMELIST/TWISS/L_TWISS,alpha_twiss,beta_twiss

 open(1,file='input.nml', status='old')
 L_TWISS = .false. !bunch at waist
 read(1,TWISS,ERR=16,END=16)
16 continue
 close(1)
 end subroutine read_bunch_TWISS_namelist


 SUBROUTINE read_poloidal_field_namelist
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C Reads Poloidal Field parameters for PWFA
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 NAMELIST/BPOLOIDAL/L_Bpoloidal,B_ex_poloidal,radius_poloidal

 open(1,file='input.nml', status='old')
 L_Bpoloidal = .false.
 B_ex_poloidal=0.0
 radius_poloidal=1.0
 read(1,BPOLOIDAL,ERR=17,END=17)
17 continue
 close(1)
 end subroutine read_poloidal_field_namelist



 subroutine write_read_nml
 character(len=12) :: output_filename
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C write namelist on a file 'input_  .nml'
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 NAMELIST/OLD_INPUT/L_read_input_data
 NAMELIST/GRID/nx,ny,nz,ny_targ,k0,yx_rat,zx_rat
 NAMELIST/SIMULATION/LPf_ord,der_ord,str_flag,iform,model_id,&
  dmodel_id,ibx,iby,ibz,ibeam
 NAMELIST/TARGET_DESCRIPTION/nsp,nsb,ion_min,ion_max,atomic_number,&
  mass_number,ionz_lev,ionz_model,t0_pl,ppc,np_per_xc,np_per_yc,np_per_zc,lpx,lpy,&
  n_over_nc,n1_over_n,n2_over_n
 NAMELIST/LASER/t0_lp,xc_lp,w0_x,w0_y,a0,lam0
 NAMELIST/MOVING_WINDOW/w_sh,wi_time,wf_time,w_speed
 NAMELIST/OUTPUT/nouts,iene,nvout,nden,npout,nbout,jump,pjump,xp0_out,xp1_out,yp_out,tmax,cfl, &
  new_sim,id_new,dump,L_force_singlefile_output,time_interval_dumps,L_print_J_on_grid, &
  L_first_output_on_restart
 NAMELIST/MPIPARAMS/nprocx,nprocy,nprocz
 NAMELIST/NUMBER_BUNCHES/ n_bunches, L_particles, L_intdiagnostics_pwfa, &
  L_intdiagnostics_classic,number_of_slices
 NAMELIST/BUNCH1/rho_b_1,gamma_1,xb_1,yb_1,zb_1,sx_1,sy_1,epsy_1,epsz_1,dg_1,np_1,bunch_type_1
 NAMELIST/BUNCH2/rho_b_2,gamma_2,xb_2,yb_2,zb_2,sx_2,sy_2,epsy_2,epsz_2,dg_2,np_2,bunch_type_2
 NAMELIST/BUNCH3/rho_b_3,gamma_3,xb_3,yb_3,zb_3,sx_3,sy_3,epsy_3,epsz_3,dg_3,np_3,bunch_type_3
 NAMELIST/BUNCH4/rho_b_4,gamma_4,xb_4,yb_4,zb_4,sx_4,sy_4,epsy_4,epsz_4,dg_4,np_4,bunch_type_4
 NAMELIST/BUNCH5/rho_b_5,gamma_5,xb_5,yb_5,zb_5,sx_5,sy_5,epsy_5,epsz_5,dg_5,np_5,bunch_type_5
 NAMELIST/TWISS/L_TWISS,alpha_twiss,beta_twiss
 NAMELIST/BPOLOIDAL/L_Bpoloidal,B_ex_poloidal,radius_poloidal

 write(output_filename,29)'input_',id_new,'.nml'
29 format(a6,i2.2,a4)
 !todo: modify input_read.nml using id_new
 open(1,file=output_filename)
 write(1,nml=OLD_INPUT,ERR=30)
 write(1,nml=GRID,ERR=30)
 write(1,nml=SIMULATION,ERR=30)
 write(1,nml=TARGET_DESCRIPTION,ERR=30)
 write(1,nml=LASER,ERR=30)
 write(1,nml=MOVING_WINDOW,ERR=30)
 write(1,nml=OUTPUT,ERR=30)
 write(1,nml=MPIPARAMS,ERR=30)
 write(1,nml=NUMBER_BUNCHES,ERR=30)
 write(1,nml=BUNCH1,ERR=30)
 write(1,nml=BUNCH2,ERR=30)
 write(1,nml=BUNCH3,ERR=30)
 write(1,nml=BUNCH4,ERR=30)
 write(1,nml=BUNCH5,ERR=30)
 write(1,nml=TWISS,ERR=30)
 write(1,nml=BPOLOIDAL,ERR=30)
30 continue
 close(1)
 END SUBROUTINE



 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C old namelist format
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 subroutine read_input_data
 open (10,file='input.data')
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
 !have not been added since input.data is deprecated in favour of the input namelist
 !Default initialization
 L_force_singlefile_output=.true.
 L_first_output_on_restart=.false.
 time_interval_dumps=-1
 nprocx=-1
 nprocy=-1
 nprocz=-1
 end subroutine read_input_data


 subroutine nml_consistency_check_number_of_particles_comp
 if(all(ppc>=1)) then
  call from_ppc_to_npx_npy_npz
 else
  np_per_zc=np_per_yc
 endif
 end subroutine nml_consistency_check_number_of_particles_comp


 subroutine nml_consistency_check_grid
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
 end subroutine nml_consistency_check_grid





 !------------------------------------------------------!
 subroutine nml_consistency_check_number_of_particles !excluded temporarily because it doesn't deal with few cases, most of all np_per_xc=0

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

 end subroutine nml_consistency_check_number_of_particles

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


 end module read_input
