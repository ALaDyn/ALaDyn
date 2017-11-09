 !*****************************************************************************************************!
 !             Copyright 2008-2016 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni       !
 !                                                Alberto Marocchino                                                              !
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

 module pic_out

 use precision_def
 use pstruct_data
 use fstruct_data
 use all_param
 use parallel

 implicit none

 real(sp),allocatable :: wdata(:),gwdata(:)

 real(dp) :: tloc(1000),tsp(1:1001),eavg(10,1001),eavg1(10,1001),&
  pavg(15,1001,4),favg(30,1001)
 integer,parameter :: par_dim=20,ne=100
 real(dp) :: nde0(ne),nde1(ne),nde2(ne)
 real(dp) :: nde(ne,500,4),eksp_max(500,4),nde_sm(ne,500,4),nde_sp(ne,500,4)
 real(sp) :: real_par(par_dim),part_real_par(20)
 integer :: int_par(par_dim),part_int_par(20),ionz_number(500)
 real(dp) :: ionz_bavg(500,16),bavg(1000,16,8),tb(1000),tionz(500)

 character(13),dimension(20),parameter :: rpar=(/&
 ' time =      ',' xmin =      ',' xmax =      ',' ymin =      ',' ymax =      ',&
 ' zmin =      ',' zmax =      ',' w0_x =      ',' w0_y =      ',' a0 =        ',&
 ' lam0 =      ',' mc2(MeV) =  ',' n0(e18/cc) =',' np/cell =   ',' weight =    ',&
 ' mass =      ',' xmin_out =  ',' xmax_out =  ',' ymax_out =  ',' gam_min =   '/)
 character(12),dimension(20),parameter :: ipar=(/&
 ' npe =      ',' nx =       ',' ny =       ',' nz =       ',' model =    ',&
 ' dmodel =   ',' nsp =      ',' curr_ndim =',' mp/cell =  ',' ion_ch =   ',&
 ' tsch_ord = ',' der_ord =  ',' iform =    ',' ph_sp_nc = ',' f_version =',&
 ' i_end =    ',' nx_loc =   ',' ny_loc =   ',' nz_loc =   ',' null  =    '/)
 !--------------------------

 contains

 !--------------------------

 subroutine endian(iend)
 implicit none
 integer,intent(out) :: iend
 integer, parameter :: ik1 = selected_int_kind(2)
 integer, parameter :: ik4 = selected_int_kind(9)

 iend=0
 if(btest(transfer(int((/1,0,0,0/),ik1),1_ik4),0))then
  iend=1
 else
  iend=2
 end if
 end subroutine endian

 subroutine fluid_den_mom_out(fvar,tnow,cmp,flcomp,jump)
 real(dp), intent(in) :: fvar(:,:,:,:)
 real(dp),intent(in) :: tnow
 integer,intent(in) :: cmp,flcomp,jump
 character(9) :: fname='         '
 character(7),dimension(4),parameter :: flvar=&
  (/'Fdenout','Flpxout','Flpyout','Flpzout'/)

 integer :: ix,iy,iz,iq,ipe
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: gr_dim(3),i_end,cmp_name
 integer :: lun,i1,j1,k1,nxp,nyp,nzp
 logical :: sd
 character(4) :: folderName
 integer,parameter :: file_version = 2
!========================
! ns_index select ion species
! cmp select components (density, energy,..)
! cmp_loc is the index of output data:  jc(cmp_loc)

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=0
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 do iz=k1,nzp,jump
  do iy=j1,nyp,jump
   do ix=i1,nxp,jump
    kk=kk+1
    wdata(kk)=real(fvar(ix,iy,iz,cmp),sp)
   end do
  end do
 end do
 if(cmp==flcomp)then
   cmp_name=1
 else
  cmp_name=cmp+1
 endif

 if(pe0)then
  call endian(i_end)
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,&
   nx1,ny1,loc_nyc_max,nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,i_end/)

  write (fname,'(a7,i2.2)')flvar(cmp_name),iout
  open (10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i14)')int_par
  write(10,*)' Real parameters'
  write (10,'(4e14.5)')real_par
  close(10)
  write(6,*)'Field data written on file: '//foldername//'/'//fname//'.dat'

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10
  open(10,file=foldername//'/'//fname//'.bin',form='unformatted')
  write(10)par_dim
  write(10)int_par
  write(10)real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(mype >0)then
  gr_dim(1)=nxh(imodx+1)
  gr_dim(2)=nyh(imody+1)
  gr_dim(3)=nzh(imodz+1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  sd=.true.
  call exchange_pdata(sd,wdata,lenw,pe_min,mype+100)
 else
  sd=.false.
  do ix=0,npe_xloc-1
   gr_dim(1)=nxh(ix+1)
   do iz=0,npe_zloc-1
    gr_dim(3)=nzh(iz+1)
    do iy=0,npe_yloc-1
     gr_dim(2)=nyh(iy+1)
     ipe=iy+npe_yloc*(iz+npe_zloc*ix)
     if(ipe >0)then
      lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
      call exchange_pdata(sd,wdata,lenw,ipe,ipe+100)
      write(lun)gr_dim
      write(lun)wdata(1:lenw)
     endif
    end do
   end do
  end do
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(r(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Fluid density-momenta written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine fluid_den_mom_out
 !--------------------------
 subroutine den_energy_out(tnow,ns_ind,cmp,cmp_loc,jump)
 real(dp),intent(in) :: tnow
 integer,intent(in) :: ns_ind,cmp,cmp_loc,jump
 character(9) :: fname='         '
 character(7),dimension(1),parameter :: epot=&
  (/'Wakepot'/)
 character(7),dimension(2),parameter :: el1=&
  (/'Edenout','Elenout'/)
 character(7),dimension(2),parameter :: pr1=&
  (/'Pdenout','Prenout'/)
 character(7),dimension(2),parameter :: io1=&
  (/'H1dnout','H1enout'/)
 character(7),dimension(2),parameter :: io2=&
  (/'H2dnout','H2enout'/)

 integer :: ix,iy,iz,iq,ipe
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: gr_dim(3),i_end
 integer :: lun,i1,j1,k1,nxp,nyp,nzp
 logical :: sd
 character(4) :: folderName
 integer,parameter :: file_version = 2
!========================
! ns_index select ion species
! cmp select components (density, energy,..)
! cmp_loc is the index of output data:  jc(cmp_loc)

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=0
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 do iz=k1,nzp,jump
  do iy=j1,nyp,jump
   do ix=i1,nxp,jump
    kk=kk+1
    wdata(kk)=real(jc(ix,iy,iz,cmp_loc),sp)
   end do
  end do
 end do

 if(pe0)then
  call endian(i_end)
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,&
   nx1,ny1,loc_nyc_max,nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,i_end/)

  select case(ns_ind)
  case(0)
   write (fname,'(a7,i2.2)')epot,iout
  case(1)
   write (fname,'(a7,i2.2)')el1(cmp),iout
  case(2)
   if(atomic_number(1)==1)then
    write (fname,'(a7,i2.2)')pr1(cmp),iout
   else
    write (fname,'(a7,i2.2)')io1(cmp),iout
   endif
  case(3)
   if(atomic_number(2)==1)then
    write (fname,'(a7,i2.2)')pr1(cmp),iout
   else
    write (fname,'(a7,i2.2)')io2(cmp),iout
   endif
  case(4)
   write (fname,'(a7,i2.2)')io2(cmp),iout
  end select
  open (10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i14)')int_par
  write(10,*)' Real parameters'
  write (10,'(4e14.5)')real_par
  close(10)
  write(6,*)'Field data written on file: '//foldername//'/'//fname//'.dat'

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10
  open(10,file=foldername//'/'//fname//'.bin',form='unformatted')
  write(10)par_dim
  write(10)int_par
  write(10)real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(mype >0)then
  gr_dim(1)=nxh(imodx+1)
  gr_dim(2)=nyh(imody+1)
  gr_dim(3)=nzh(imodz+1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  sd=.true.
  call exchange_pdata(sd,wdata,lenw,pe_min,mype+100)
 else
  sd=.false.
  do ix=0,npe_xloc-1
   gr_dim(1)=nxh(ix+1)
   do iz=0,npe_zloc-1
    gr_dim(3)=nzh(iz+1)
    do iy=0,npe_yloc-1
     gr_dim(2)=nyh(iy+1)
     ipe=iy+npe_yloc*(iz+npe_zloc*ix)
     if(ipe >0)then
      lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
      call exchange_pdata(sd,wdata,lenw,ipe,ipe+100)
      write(lun)gr_dim
      write(lun)wdata(1:lenw)
     endif
    end do
   end do
  end do
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(r(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Den_Energy_Momenta written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine den_energy_out

 !--------------------------
 subroutine bden_energy_out(tnow,cmp_loc,jump)

 real(dp),intent(in) :: tnow
 integer,intent(in) :: cmp_loc,jump
 character(9) :: fname='         '

 integer :: ix,iy,iz,ip,iq,ipe
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: i_end,i1,j1,k1,nxp,nyp,nzp
 integer :: lun,gr_dim(3)
 character(4) :: folderName
 integer,parameter :: file_version = 2
 logical :: sd

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=10
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 do iz=k1,nzp,jump
  do iy=j1,nyp,jump
   do ix=i1,nxp,jump
    kk=kk+1
    wdata(kk)=real(jc(ix,iy,iz,cmp_loc),sp)
   end do
  end do
 end do

 if(pe0)then
  call endian(i_end)
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,&
   nx1,ny1,loc_nyc_max,nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,i_end/)

  write(fname,'(a7,i2.2)')'Bdenout',iout
  open (20,file=foldername//'/'//fname//'.dat',form='formatted')
  write(20,*)' Integer parameters'
  write(20,'(4i14)')int_par
  write(20,*)' Real parameters'
  write (20,'(4e14.5)')real_par
  close(20)
  write(6,*)'Field data written on file: '//foldername//'/'//fname//'.dat'

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10

  open (10,file=foldername//'/'//fname//'.bin',form='unformatted') !qui
  write(10)par_dim
  write(10)int_par
  write (10)real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(prl) then
  if(mype >0)then
   gr_dim(1)=nxh(imodx+1)
   gr_dim(2)=nyh(imody+1)
   gr_dim(3)=nzh(imodz+1)
   lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
   sd=.true.
   call exchange_pdata(sd,wdata,lenw,pe_min,mype+100)
  else
   sd=.false.
   do ix=0,npe_xloc-1
    gr_dim(1)=nxh(ix+1)
    do ip=0,npe_zloc-1
     gr_dim(3)=nzh(ip+1)
     do iq=0,npe_yloc-1
      gr_dim(2)=nyh(iq+1)
      ipe=iq+npe_yloc*(ip+npe_zloc*ix)
      if(ipe >0)then
       lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
       call exchange_pdata(sd,wdata,lenw,ipe,ipe+100)
       write(lun)gr_dim
       write(lun)wdata(1:lenw)
      endif
     end do
    end do
   end do
  endif
 endif

 if(pe0)then
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(r(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Den_Energy_Momenta written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine bden_energy_out
 !--------------------------
 subroutine ext_bfield_out(ef,tnow,f_ind,jump)
 real(dp),intent(in) :: ef(:,:,:,:)
 real(dp),intent(in) :: tnow
 character(8) :: fname='        '
 integer,intent(in) :: f_ind,jump
 integer :: ix,iy,iz,iq,ipe
 integer :: lun,lenw,kk,nx1,ny1,nz1
 integer :: i1,j1,k1,nxp,nyp,nzp,i_end
 integer :: gr_dim(3)
 logical :: sd
 character(4) :: folderName
 integer,parameter :: file_version = 2

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=0

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 do iz=k1,nzp,jump
  do iy=j1,nyp,jump
   do ix=i1,nxp,jump
    kk=kk+1
    wdata(kk)=real(ef(ix,iy,iz,f_ind),sp)
   end do
  end do
 end do

 if(pe0)then
  call endian(i_end)
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,nx1,ny1,loc_nyc_max,&
   nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,i_end/)

  select case(f_ind)
  case(1)
   write (fname,'(a6,i2.2)') 'Bx0out' ,iout
  case(2)
   write (fname,'(a6,i2.2)') 'By0out' ,iout
  case(3)
   write (fname,'(a6,i2.2)') 'Bz0out' ,iout
  end select

  open(10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i10)')int_par
  write(10,*)' Real parameters'
  write(10,'(4e14.5)')real_par
  close(10)
  write(6,*)'Field data written on file: '//foldername//'/'//fname//'.dat'

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10
  open(10,file=foldername//'/'//fname//'.bin',form='unformatted')
  write(10)par_dim
  write(10)int_par
  write(10)real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(mype >0)then
  gr_dim(1)=nxh(imodx+1)
  gr_dim(2)=nyh(imody+1)
  gr_dim(3)=nzh(imodz+1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  sd=.true.
  call exchange_pdata(sd,wdata,lenw,pe_min,mype+100)
 else
  sd=.false.
  do ix=0,npe_xloc-1
   gr_dim(1)=nxh(ix+1)
   do iz=0,npe_zloc-1
    gr_dim(3)=nzh(iz+1)
    do iy=0,npe_yloc-1
     gr_dim(2)=nyh(iy+1)
     ipe=iy+npe_yloc*(iz+npe_zloc*ix)
     if(ipe >0)then
      lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
      call exchange_pdata(sd,wdata,lenw,ipe,ipe+100)
      write(lun)gr_dim
      write(lun)wdata(1:lenw)
     endif
    end do
   end do
  end do
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(r(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Fields written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine ext_bfield_out

 subroutine fields_out(ef,tnow,f_ind,f_var,jump)
 real(dp),intent(in) :: ef(:,:,:,:)
 real(dp),intent(in) :: tnow
 character(8) :: fname='        '
 integer,intent(in) :: f_ind,f_var,jump
 integer :: ix,iy,iz,iq,ipe
 integer :: lun,lenw,kk,nx1,ny1,nz1
 integer :: i1,j1,k1,nxp,nyp,nzp,i_end
 integer :: gr_dim(3)
 logical :: sd
 character(4) :: folderName
 integer,parameter :: file_version = 2

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=0

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 do iz=k1,nzp,jump
  do iy=j1,nyp,jump
   do ix=i1,nxp,jump
    kk=kk+1
    wdata(kk)=real(ef(ix,iy,iz,f_ind),sp)
   end do
  end do
 end do

 if(pe0)then
  call endian(i_end)
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,nx1,ny1,loc_nyc_max,&
   nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,i_end/)

  select case(f_var)
  case(0)
   write (fname,'(a6,i2.2)') 'Jxfout' ,iout
  case(1)
   write (fname,'(a6,i2.2)') 'Exfout' ,iout
  case(2)
   write (fname,'(a6,i2.2)') 'Eyfout' ,iout
  case(3)
   if(nfield==3)then
    write (fname,'(a6,i2.2)') 'Bzfout' ,iout
   else
    write (fname,'(a6,i2.2)') 'Ezfout' ,iout
   endif
  case(4)
   write (fname,'(a6,i2.2)') 'Bxfout' ,iout
  case(5)
   write (fname,'(a6,i2.2)') 'Byfout' ,iout
  case(6)
   write (fname,'(a6,i2.2)') 'Bzfout' ,iout
  end select

  open(10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i14)')int_par
  write(10,*)' Real parameters'
  write(10,'(4e14.5)')real_par
  close(10)
  write(6,*)'Field data written on file: '//foldername//'/'//fname//'.dat'

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10
  open(10,file=foldername//'/'//fname//'.bin',form='unformatted')
  write(10)par_dim
  write(10)int_par
  write(10)real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(mype >0)then
  gr_dim(1)=nxh(imodx+1)
  gr_dim(2)=nyh(imody+1)
  gr_dim(3)=nzh(imodz+1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  sd=.true.
  call exchange_pdata(sd,wdata,lenw,pe_min,mype+100)
 else
  sd=.false.
  do ix=0,npe_xloc-1
   gr_dim(1)=nxh(ix+1)
   do iz=0,npe_zloc-1
    gr_dim(3)=nzh(iz+1)
    do iy=0,npe_yloc-1
     gr_dim(2)=nyh(iy+1)
     ipe=iy+npe_yloc*(iz+npe_zloc*ix)
     if(ipe >0)then
      lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
      call exchange_pdata(sd,wdata,lenw,ipe,ipe+100)
      write(lun)gr_dim
      write(lun)wdata(1:lenw)
     endif
    end do
   end do
  end do
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(r(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Fields written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine fields_out

 !--------------------------

 subroutine fields_out_new(ef,tnow,f_ind,var_ind,jump)
 real(dp),intent(in) :: ef(:,:,:,:)
 real(dp),intent(in) :: tnow
 character(8) :: fname='        '
 character(12) :: fnamel='            '
 character(17) :: fname_out='                 '
 character(21) :: fname_outl='                     '
 integer,intent(in) :: f_ind,var_ind,jump
 integer :: ix,iy,iz,iq
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: i1,j1,k1,nxp,nyp,nzp,i_end
 integer(offset_kind) :: disp,disp_col
 integer :: num_header_int,gr_dim(3),header(3)
 real(dp),allocatable :: ascii_grid(:)
 integer :: gridsize_x,gridsize_y,gridsize_z
 character(4) :: folderName
 integer,parameter :: file_version = 4

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 gr_dim=0

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0

 do iz=k1,nzp,jump
  do iy=j1,nyp,jump
   do ix=i1,nxp,jump
    kk=kk+1
    wdata(kk)=real(ef(ix,iy,iz,f_ind),sp)
   end do
  end do
 end do
 !================================
 call endian(i_end)
 ny1=sum(nyh(1:npe_yloc))
 nz1=sum(nzh(1:npe_zloc))
 nx1=sum(nxh(1:npe_xloc))

 real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
  real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
  real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
  real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
  real(b_charge,sp),real(vbeam,sp)/)

 int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,nx1,ny1,nz1,&
  loc_nyc_max,jump,ibx,iby,iform,&
  model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
  LPf_ord,der_ord,file_version,i_end/)

 select case(var_ind)
 case(0)
  write (fname,'(a6,i2.2)') 'dvEout',iout
 case(1)
  write (fname,'(a6,i2.2)') 'Exfout' ,iout
 case(2)
  write (fname,'(a6,i2.2)') 'Eyfout' ,iout
 case(3)
  if(nfield==3)then
   write (fname,'(a6,i2.2)') 'Bzfout' ,iout
  else
   write (fname,'(a6,i2.2)') 'Ezfout' ,iout
  endif
 case(4)
  write (fname,'(a6,i2.2)') 'Bxfout' ,iout
 case(5)
  write (fname,'(a6,i2.2)') 'Byfout' ,iout
 case(6)
  write (fname,'(a6,i2.2)') 'Bzfout' ,iout
 case(7)
  write (fname,'(a6,i2.2)') 'Exbout' ,iout
 case(8)
  write (fname,'(a6,i2.2)') 'Eybout' ,iout
 case(9)
  if(nfield==3)then
   write (fname,'(a6,i2.2)') 'Bzbout' ,iout
  else
   write (fname,'(a6,i2.2)') 'Ezbout' ,iout
  endif
 case(10)
  write (fname,'(a6,i2.2)') 'Jxbout' ,iout
 case(11)
  write (fname,'(a6,i2.2)') 'Bybout' ,iout
 case(12)
  write (fname,'(a6,i2.2)') 'Bzbout' ,iout
 end select

 if(pe0)then
  open (10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i14)')int_par
  write(10,*)' Real parameters'
  write(10,'(4e14.5)')real_par
  write(10,*)' Coordinates'

  gridsize_x=int(nx/jump)
  gridsize_y=int(ny/jump)
  gridsize_z=int(nz/jump)
  allocate(ascii_grid(gridsize_x+1))
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   ascii_grid(kk)=x(iq)
  end do
  do iq=1,kk
   write(10,'(es14.5)',advance='no')ascii_grid(iq)
   if (MOD(iq,8).eq.0) write(10,*) ''
  end do
  deallocate(ascii_grid)
  allocate(ascii_grid(gridsize_y+1))
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   ascii_grid(kk)=r(iq)
  end do
  do iq=1,kk
   write(10,'(es14.5)',advance='no')ascii_grid(iq)
   if (MOD(iq,8).eq.0) write(10,*) ''
  end do
  deallocate(ascii_grid)
  allocate(ascii_grid(gridsize_z+1))
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   ascii_grid(kk)=z(iq)
  end do
  do iq=1,kk
   write(10,'(es14.5)',advance='no')ascii_grid(iq)
   if (MOD(iq,8).eq.0) write(10,*) ''
  end do
  close(10)
  write(6,*)'Fields parameters written on file: '//foldername//'/'//fname//'.dat'
 endif

 gr_dim(1)=nxh(imodx+1)
 gr_dim(2)=nyh(imody+1)
 gr_dim(3)=nzh(imodz+1)
 lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)

 write (fnamel,'(a8,a1,i3.3)')fname,'_',imodz
 fname_out=foldername//'/'//fname//'.bin'
 fname_outl=foldername//'/'//fnamel//'.bin'
 num_header_int=3
 header(1:3)=gr_dim(1:3)
 disp=4*mype*(num_header_int+lenw)  ! da usare con mpi_write !assuming that all procs have the same grid size
 disp_col=4*imody*(num_header_int+lenw)  ! con mpi_write_col !assuming that all procs have the same grid size

 if((ndim<3).or.(L_force_singlefile_output))then
  call mpi_write_field(wdata,lenw,header,num_header_int,disp,17,fname_out)
 else
  call mpi_write_field_col(wdata,lenw,header,num_header_int,disp_col,21,fname_outl)
 endif
 if(pe0)then
  write(6,*)'Fields written on file: '//foldername//'/'//fname//'.bin'
  write(6,*)' Output logical flag ',L_force_singlefile_output
 endif

 end subroutine fields_out_new

 !--------------------------

 subroutine bfields_out(ef,ef1,tnow,f_ind,jump)
 real(dp),intent(in) :: ef(:,:,:,:),ef1(:,:,:,:)
 real(dp),intent(in) :: tnow
 character(8) :: fname='        '
 integer,intent(in) :: f_ind,jump
 integer :: ix,iy,iz,iq,ipe
 integer :: lun,lenw,kk,nx1,ny1,nz1
 integer :: i1,j1,k1,nxp,nyp,nzp,i_end
 integer :: gr_dim(3)
 logical :: sd
 character(4) :: folderName
 integer,parameter :: file_version = 2

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=0

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 select case(ibeam)
 case(0)
  do iz=k1,nzp,jump
   do iy=j1,nyp,jump
    do ix=i1,nxp,jump
     kk=kk+1
     wdata(kk)=real(ef(ix,iy,iz,f_ind),sp)
    end do
   end do
  end do
 case(1)
  do iz=k1,nzp,jump
   do iy=j1,nyp,jump
    do ix=i1,nxp,jump
     kk=kk+1
     wdata(kk)=real(ef(ix,iy,iz,f_ind)+ef1(ix,iy,iz,f_ind),sp)
    end do
   end do
  end do
 end select

 if(pe0)then
  call endian(i_end)
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,nx1,ny1,loc_nyc_max,&
   nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,i_end/)

  select case(f_ind)
  case(1)
   write (fname,'(a6,i2.2)') 'Exbout' ,iout
  case(2)
   write (fname,'(a6,i2.2)') 'Eybout' ,iout
  case(3)
   if(nfield==3)then
    write (fname,'(a6,i2.2)') 'Bzbout' ,iout
   else
    write (fname,'(a6,i2.2)') 'Ezbout' ,iout
   endif
  case(4)
   write (fname,'(a6,i2.2)') 'Jxbout' ,iout
  case(5)
   write (fname,'(a6,i2.2)') 'Bybout' ,iout
  case(6)
   write (fname,'(a6,i2.2)') 'Bzbout' ,iout
  end select

  open (10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i14)')int_par
  write(10,*)' Real parameters'
  write (10,'(4e14.5)')real_par
  close(10)
  write(6,*)'Field data written on file: '//foldername//'/'//fname//'.dat'

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10
  open (10,file=foldername//'/'//fname//'.bin',form='unformatted')
  write(10)par_dim
  write(10)int_par
  write (10) real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(mype >0)then
  gr_dim(1)=nxh(imodx+1)
  gr_dim(2)=nyh(imody+1)
  gr_dim(3)=nzh(imodz+1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  sd=.true.
  call exchange_pdata(sd,wdata,lenw,pe_min,mype+200)
 else
  sd=.false.
  do ix=0,npe_xloc-1
   gr_dim(1)=nxh(ix+1)
   do iz=0,npe_zloc-1
    gr_dim(3)=nzh(iz+1)
    do iy=0,npe_yloc-1
     gr_dim(2)=nyh(iy+1)
     ipe=iy+npe_yloc*(iz+npe_zloc*ix)
     if(ipe >0)then
      lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
      call exchange_pdata(sd,wdata,lenw,ipe,ipe+200)
      write(lun)gr_dim
      write(lun)wdata(1:lenw)
     endif
    end do
   end do
  end do
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(r(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Fields written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine bfields_out

 !--------------------------
 subroutine env_two_fields_out(ef,ef1,tnow,f_ind,jump)
 real(dp),intent(in) :: ef(:,:,:,:),ef1(:,:,:,:)
 real(dp),intent(in) :: tnow
 character(9) :: fname='         '
 integer,intent(in) :: f_ind,jump
 integer :: ix,iy,iz,iq,ipe
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: gr_dim(3)
 integer :: i1,j1,k1,nxp,nyp,nzp,lun
 logical :: sd
 real(dp) :: a2,avec
 character(4) :: folderName
 integer,parameter :: file_version = 2

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=0

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 if(f_ind==0)then
  do iz=k1,nzp,jump
   do iy=j1,nyp,jump
    do ix=i1,nxp,jump
     kk=kk+1
     a2=ef(ix,iy,iz,1)*ef(ix,iy,iz,1)+ef(ix,iy,iz,2)*ef(ix,iy,iz,2)
     avec=sqrt(a2)
     a2=ef1(ix,iy,iz,1)*ef1(ix,iy,iz,1)+ef1(ix,iy,iz,2)*ef1(ix,iy,iz,2)
     avec=avec+sqrt(a2)
     wdata(kk)=real(avec,sp)
    end do
   end do
  end do
 else
  do iz=k1,nzp,jump
   do iy=j1,nyp,jump
    do ix=i1,nxp,jump
     kk=kk+1
     wdata(kk)=real(ef(ix,iy,iz,f_ind),sp)
     wdata(kk)=wdata(kk)+real(ef1(ix,iy,iz,f_ind),sp)
    end do
   end do
  end do
 endif

 if(pe0)then
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,&
   nx1,ny1,loc_nyc_max,nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,ibeam/)

  select case(f_ind)
  case(0)
   write (fname,'(a7,i2.2)') 'Aenvout' ,iout
  case(1)
   write (fname,'(a7,i2.2)') 'Renvout' ,iout
  case(2)
   write (fname,'(a7,i2.2)') 'Ienvout' ,iout
  end select

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10
  open (10,file=foldername//'/'//fname//'.bin',form='unformatted')
  write(10)par_dim
  write(10)int_par
  write (10) real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(mype >0)then
  gr_dim(1)=nxh(imodx+1)
  gr_dim(2)=nyh(imody+1)
  gr_dim(3)=nzh(imodz+1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  sd=.true.
  call exchange_pdata(sd,wdata,lenw,pe_min,mype+100)
 else
  sd=.false.
  do ix=0,npe_xloc-1
   gr_dim(1)=nxh(ix+1)
   do iz=0,npe_zloc-1
    gr_dim(3)=nzh(iz+1)
    do iy=0,npe_yloc-1
     gr_dim(2)=nyh(iy+1)
     ipe=iy+npe_yloc*(iz+npe_zloc*ix)
     if(ipe >0)then
      lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
      call exchange_pdata(sd,wdata,lenw,ipe,ipe+100)
      write(lun)gr_dim
      write(lun)wdata(1:lenw)
     endif
    end do
   end do
  end do
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(y(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Fields written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine env_two_fields_out

 subroutine env_fields_out(ef,tnow,f_ind,jump)
 real(dp),intent(in) :: ef(:,:,:,:)
 real(dp),intent(in) :: tnow
 character(9) :: fname='         '
 integer,intent(in) :: f_ind,jump
 integer :: ix,iy,iz,iq,ipe
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: gr_dim(3)
 integer :: i1,j1,k1,nxp,nyp,nzp,lun
 logical :: sd
 character(4) :: folderName
 integer,parameter :: file_version = 2
 real(dp) :: a2,avec

 write (folderName,'(i4.4)') iout

 int_par=0
 real_par=0.0
 lun=0

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 nxp=loc_xgrid(imodx)%p_ind(2)

 kk=0
 if(f_ind< 1)then
  do iz=k1,nzp,jump
   do iy=j1,nyp,jump
    do ix=i1,nxp,jump
     kk=kk+1
     a2=ef(ix,iy,iz,1)*ef(ix,iy,iz,1)+ef(ix,iy,iz,2)*ef(ix,iy,iz,2)
     avec=sqrt(a2)
     wdata(kk)=real(avec,sp)
    end do
   end do
  end do
 else
 do iz=k1,nzp,jump
  do iy=j1,nyp,jump
   do ix=i1,nxp,jump
    kk=kk+1
    wdata(kk)=real(ef(ix,iy,iz,f_ind),sp)
   end do
  end do
 end do
 endif

 if(pe0)then
  nx1=sum(nxh(1:npe_xloc))
  ny1=sum(nyh(1:npe_yloc))
  nz1=sum(nzh(1:npe_zloc))

  real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
   real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
   real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
   real(targ_in,sp),real(targ_end,sp),real(gam0,sp),real(nb_over_np,sp),&
   real(b_charge,sp),real(vbeam,sp)/)

  int_par(1:20) = (/npe_yloc,npe_zloc,npe_xloc,&
   nx1,ny1,loc_nyc_max,nz1,loc_nzc_max,jump,iby,iform,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,file_version,ibeam/)

  select case(f_ind)
  case(-1)
   write (fname,'(a7,i2.2)') 'aenvout' ,iout
  case(0)
   write (fname,'(a7,i2.2)') 'Aenvout' ,iout
  case(1)
   write (fname,'(a7,i2.2)') 'Renvout' ,iout
  case(2)
   write (fname,'(a7,i2.2)') 'Ienvout' ,iout
  end select

  gr_dim(1)=nxh(1)
  gr_dim(2)=nyh(1)
  gr_dim(3)=nzh(1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  lun=10
  open (10,file=foldername//'/'//fname//'.bin',form='unformatted')
  write(10)par_dim
  write(10)int_par
  write (10) real_par
  write(10)gr_dim
  write(10)wdata(1:lenw)
 endif

 if(mype >0)then
  gr_dim(1)=nxh(imodx+1)
  gr_dim(2)=nyh(imody+1)
  gr_dim(3)=nzh(imodz+1)
  lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
  sd=.true.
  call exchange_pdata(sd,wdata,lenw,pe_min,mype+100)
 else
  sd=.false.
  do ix=0,npe_xloc-1
   gr_dim(1)=nxh(ix+1)
   do iz=0,npe_zloc-1
    gr_dim(3)=nzh(iz+1)
    do iy=0,npe_yloc-1
     gr_dim(2)=nyh(iy+1)
     ipe=iy+npe_yloc*(iz+npe_zloc*ix)
     if(ipe >0)then
      lenw=gr_dim(1)*gr_dim(2)*gr_dim(3)
      call exchange_pdata(sd,wdata,lenw,ipe,ipe+100)
      write(lun)gr_dim
      write(lun)wdata(1:lenw)
     endif
    end do
   end do
  end do
  kk=0
  do iq=1,nx,jump
   kk=kk+1
   gwdata(kk)=real(x(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,ny,jump
   kk=kk+1
   gwdata(kk)=real(y(iq),sp)
  end do
  write(10)gwdata(1:kk)
  kk=0
  do iq=1,nz,jump
   kk=kk+1
   gwdata(kk)=real(z(iq),sp)
  end do
  write(10)gwdata(1:kk)
  close(10)
  write(6,*)'Fields written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine env_fields_out

 !--------------------------
 subroutine part_high_gamma_out(gam_in,tnow)

 character(8),dimension(1),parameter :: part=(/'E_hg_out'/)
 character(10) :: fname
 character(19) :: fname_out
 real(dp),intent(in) :: gam_in,tnow
 real(sp),allocatable :: pdata(:)
 integer(dp) :: nptot_global_reduced
 integer :: id_ch,ik,p,q,ip,ip_max,nptot
 integer :: jmp,ne,lenp,ip_loc(npe),ndv,i_end
 integer(offset_kind) :: disp
 real(dp) :: wgh,gam,pp(3)
 real(sp) :: ch(2)
 equivalence(wgh,ch)
 character(4) :: folderName
 integer,parameter :: file_version = 4

 write (folderName,'(i4.4)') iout
 jmp=1
 id_ch=nd2+1
 ndv=nd2+2
 ne=loc_npart(imody,imodz,imodx,1)
 select case(nd2)
 case(4)
  ip=0
  if(ne > 0)then
   do p=1,ne
    pp(1:2)=spec(1)%part(p,3:4)
    gam=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2))
    if(gam > gam_in)then
     ip=ip+1
     do q=1,nd2+1
      ebfp(ip,q)=spec(1)%part(p,q)
     end do
    endif
   end do
  endif
 case(6)
  ip=0
  if(ne > 0)then
   do p=1,ne
    pp(1:3)=spec(1)%part(p,4:6)
    gam=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3))
    if(gam > gam_in)then
     ip=ip+1
     do q=1,nd2+1
      ebfp(ip,q)=spec(1)%part(p,q)
     end do
    endif
   end do
  endif
 end select
 ip_loc(mype+1)=ip

 ip=ip_loc(mype+1)
 call intvec_distribute(ip,ip_loc,npe)
 nptot_global_reduced=0
 !nptot_global_reduced=sum(ip_loc(1:npe))
 do ik=1,npe
  nptot_global_reduced=nptot_global_reduced+ip_loc(ik)
 end do
 if (nptot_global < 1E9) then
  nptot=int(nptot_global_reduced)
 else
  nptot=-1
 endif
 ip_max=ip
 if(pe0)ip_max=maxval(ip_loc(1:npe))
 lenp=ndv*ip
 ik=max(1,lenp)
 allocate(pdata(lenp))
 ik=0
 do p=1,ip
  do q=1,nd2
   ik=ik+1
   pdata(ik)=real(ebfp(p,q),sp)
  end do
  wgh=ebfp(p,nd2+1)
  ik=ik+1
  pdata(ik)=ch(1)
  ik=ik+1
  pdata(ik)=ch(2)
 end do
 if(ik /= lenp)write(6,'(a16,3i8)')'wrong pdata size',mype,lenp,ik

 int_par=0
 call endian(i_end)

  part_real_par(1:20)=&
  (/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
  real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
  real(a0,sp),real(lam0,sp),real(E0,sp),real(n0_ref,sp),&
  real(np_per_cell,sp),real(wgh_ion,sp),real(mass(1),sp),&
  real(xp0_out,sp),real(xp1_out,sp),real(yp_out,sp),real(gam_in,sp)/)

  part_int_par(1:20) = (/npe,nx,ny,nz,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   LPf_ord,der_ord,iform,ndv,file_version,i_end,&
   nx_loc,ny_loc,nz_loc,0,0/)

 write(fname,'(a8,i2.2)')part(1),iout      !serve sempre
 fname_out=foldername//'/'//fname//'.bin'
 disp=0
 if(pe0)then
  open(10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Real parameters'
  do q=1,20
   write(10,'(a13,e11.4)')rpar(q),part_real_par(q)
  enddo
  write(10,*)' Integer parameters'
  do p=1,20
   write(10,'(a12,i8)')ipar(p),part_int_par(p)
  end do
  write(10,*)' Number of particles in the output box'
  write(10,'(4i20)')nptot_global_reduced
  close(10)
  write(6,*)'Particles param written on file: '//foldername//'/'//fname//'.dat'
 else
  disp=mype+ndv*sum(ip_loc(1:mype))  ! da usare con mpi_write_part
 endif

 disp=disp*4  ! sia gli int che i float sono di 4 bytes
 call mpi_write_part(pdata,lenp,ip,disp,19,fname_out)
 if(allocated(pdata))deallocate(pdata)
 if(pe0)then
  write(6,*)'Particles data written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine part_high_gamma_out
!==============================================
 subroutine part_ionz_out(tnow)

 character(8),dimension(1),parameter :: part=(/'Eionzout'/)
 character(10) :: fname
 character(19) :: fname_out
 real(dp),intent(in) :: tnow
 real(sp),allocatable :: pdata(:)
 integer(dp) :: nptot_global_reduced
 integer :: id_ch,ik,p,q,ip,ip_max,nptot
 integer :: jmp,ne,lenp,ip_loc(npe),ndv,i_end
 integer(offset_kind) :: disp
 real(dp) :: wgh
 real(sp) :: ch(2),ch_ion
 equivalence(wgh,ch)
 character(4) :: folderName
 integer,parameter :: file_version = 4

 write (folderName,'(i4.4)') iout
 jmp=1
 id_ch=nd2+1
 ndv=nd2+2
 ch_ion=real(wgh_ion,sp)
 ne=loc_npart(imody,imodz,imodx,1)
 ip=0
 if(ne > 0)then
  do p=1,ne
   wgh=spec(1)%part(p,id_ch)
   if(abs(ch(1)-ch_ion)< 1.e-05)then
    ip=ip+1
    do q=1,nd2+1
     ebfp(ip,q)=spec(1)%part(p,q)
    end do
   endif
  end do
 endif
 ip_loc(mype+1)=ip

 ip=ip_loc(mype+1)
 call intvec_distribute(ip,ip_loc,npe)
 nptot_global_reduced=0
 !nptot_global_reduced=sum(ip_loc(1:npe))
 do ik=1,npe
  nptot_global_reduced=nptot_global_reduced+ip_loc(ik)
 end do
 if (nptot_global < 1E9) then
  nptot=int(nptot_global_reduced)
 else
  nptot=-1
     endif
 ip_max=ip
 if(pe0)ip_max=maxval(ip_loc(1:npe))
 lenp=ndv*ip
 ik=max(1,lenp)
 allocate(pdata(lenp))
 ik=0
 do p=1,ip
  do q=1,nd2
   ik=ik+1
   pdata(ik)=real(ebfp(p,q),sp)
    end do
  wgh=ebfp(p,nd2+1)
  ik=ik+1
  pdata(ik)=ch(1)
  ik=ik+1
  pdata(ik)=ch(2)
   end do
 if(ik /= lenp)write(6,'(a16,3i8)')'wrong pdata size',mype,lenp,ik

 int_par=0
 call endian(i_end)

  part_real_par(1:20)=&
  (/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
  real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
  real(a0,sp),real(lam0,sp),real(E0,sp),real(n0_ref,sp),&
  real(np_per_cell,sp),real(wgh_ion,sp),real(mass(1),sp),&
  real(xp0_out,sp),real(xp1_out,sp),real(yp_out,sp),real(gam_min,sp)/)


  part_int_par(1:20) = (/npe,nx,ny,nz,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
   ion_min(1),LPf_ord,der_ord,iform,ndv,file_version,i_end,&
   nx_loc,ny_loc,nz_loc,0/)

 write(fname,'(a8,i2.2)')part(1),iout      !serve sempre
 fname_out=foldername//'/'//fname//'.bin'
 disp=0
 if(pe0)then
  open(10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Real parameters'
  do q=1,20
   write(10,'(a13,e11.4)')rpar(q),part_real_par(q)
  enddo
  write(10,*)' Integer parameters'
  do p=1,20
   write(10,'(a12,i8)')ipar(p),part_int_par(p)
  end do
  write(10,*)' Number of particles in the output box'
  write(10,'(4i20)')nptot_global_reduced
  close(10)
  write(6,*)'Particles param written on file: '//foldername//'/'//fname//'.dat'
 else
  disp=mype+ndv*sum(ip_loc(1:mype))  ! da usare con mpi_write_part
 endif

 disp=disp*4  ! sia gli int che i float sono di 4 bytes
 call mpi_write_part(pdata,lenp,ip,disp,19,fname_out)
 if(allocated(pdata))deallocate(pdata)
 if(pe0)then
  write(6,*)'Particles data written on file: '//foldername//'/'//fname//'.bin'
 endif
 end subroutine part_ionz_out
!================================
 subroutine part_pdata_out(tnow,xmin_out,xmax_out,ymax_out,pid,jmp)

 character(6),dimension(4),parameter :: part=&
  (/'Elpout','H1pout','Prpout','H2pout'/)
 character(8) :: fname
 character(17) :: fname_out
 character(12) :: fnamel
 character(21) :: fname_outl
 real(dp),intent(in) :: tnow,xmin_out,xmax_out,ymax_out
 integer,intent(in) :: pid,jmp
 real(sp),allocatable :: pdata(:)
 integer(dp) :: nptot_global_reduced
 integer :: ik,p,q,np,ip,ip_max,nptot
 integer :: lenp,ip_loc(npe),ndv,i_end
 integer(offset_kind) :: disp,disp_col
 real(dp) :: xx,yy,zz
 real(dp) :: wgh
 real(sp) :: ch(2)
 character(4) :: folderName
 integer,parameter :: file_version = 4

 equivalence(wgh,ch)

 write (folderName,'(i4.4)') iout


 ndv=nd2+2
 np=loc_npart(imody,imodz,imodx,pid)
 ip=0
 if(np >0)then
 if(ndim >2)then
  do p=1,np,jmp
   yy=spec(pid)%part(p,2)
   zz=spec(pid)%part(p,3)
   if(abs(yy)<=ymax_out.and.abs(zz)<=ymax_out)then
    xx=spec(pid)%part(p,1)
    if(xx>=xmin_out.and.xx <=xmax_out)then
     ip=ip+1
     do q=1,nd2+1
      ebfp(ip,q)=spec(pid)%part(p,q)
     end do
    endif
   endif
  end do
 else
  zz=1.
  do p=1,np,jmp
   yy=spec(pid)%part(p,2)
   if(abs(yy)<=ymax_out)then
    xx=spec(pid)%part(p,1)
    if(xx>=xmin_out.and.xx<=xmax_out)then
     ip=ip+1
     do q=1,nd2+1
      ebfp(ip,q)=spec(pid)%part(p,q)
     end do
    endif
   endif
  end do
 endif
 endif
 ip_loc(mype+1)=ip

 ip=ip_loc(mype+1)
 call intvec_distribute(ip,ip_loc,npe)


 ! this differs from nptot_global since it represents just the reduced number of particles
 ! that will be present in the output (should be equal to nptot_global for p_jump=1)!
 nptot_global_reduced=0
 !nptot_global_reduced=sum(ip_loc(1:npe))
 do ik=1,npe
  nptot_global_reduced=nptot_global_reduced+ip_loc(ik)
 end do
 if (nptot_global < 1E9) then
  nptot=int(nptot_global_reduced)
 else
  nptot=-1
 endif



 ip_max=ip
 if(pe0)ip_max=maxval(ip_loc(1:npe))
 lenp=ndv*ip
 allocate(pdata(lenp))
 ik=0
 do p=1,ip
  do q=1,nd2
   ik=ik+1
   pdata(ik)=real(ebfp(p,q),sp)
  end do
  wgh=ebfp(p,nd2+1)
  ik=ik+1
  pdata(ik)=ch(1)
  ik=ik+1
  pdata(ik)=ch(2)
 end do
 if(ik /= lenp)write(6,'(a16,3i8)')'wrong pdata size',mype,lenp,ik

 call endian(i_end)
  part_real_par(1:20)=&
  (/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
  real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
  real(a0,sp),real(lam0,sp),real(E0,sp),real(n0_ref,sp),&
  real(np_per_cell,sp),real(j0_norm,sp),real(mass(pid),sp),&
  real(xmin_out,sp),real(xmax_out,sp),real(ymax_out,sp),real(gam_min,sp)/)

  part_int_par(1:20) = (/npe,nx,ny,nz,&
   model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(pid),&
   LPf_ord,der_ord,iform,ndv,file_version,i_end,&
   nx_loc,ny_loc,nz_loc,0,0/)

 write(fname,'(a6,i2.2)')part(pid),iout      !serve sempre
 write(fnamel,'(a6,i2.2,a1,i3.3)')part(pid),iout,'_',imodz !usare con mpi_write_part_col
 fname_out=foldername//'/'//fname//'.bin'
 fname_outl=foldername//'/'//fnamel//'.bin'
 disp=0
 disp_col=0
 if(pe0)then
  open(10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Real parameters'
  do q=1,20
   write(10,'(a13,e11.4)')rpar(q),part_real_par(q)
  enddo
  write(10,*)' Integer parameters'
  do p=1,20
   write(10,'(a12,i8)')ipar(p),part_int_par(p)
  end do
  write(10,*)' Number of particles in the output box'
  write(10,'(4i20)')nptot_global_reduced
  close(10)
  write(6,*)'Particles param written on file: '//foldername//'/'//fname//'.dat'
 else
  disp=mype+ndv*sum(ip_loc(1:mype))  ! da usare con mpi_write_part
 endif

 if(MOD(mype,npe_yloc) > 0) disp_col=ndv*sum(ip_loc(imodz*npe_yloc+1:mype)) ! da usare con mpi_write_part_col

 disp=disp*4  ! sia gli int che i float sono di 4 bytes
 disp_col=disp_col*4

 if((ndim<3).or.(L_force_singlefile_output))then
  call mpi_write_part(pdata,lenp,ip,disp,17,fname_out)
 else
  call mpi_write_part_col(pdata,lenp,disp_col,21,fname_outl)
 endif

 if(allocated(pdata))deallocate(pdata)
 if(pe0)then
  write(6,*)'Particles data written on file: '//foldername//'/'//fname//'.bin'
  write(6,*)' Output logical flag ',L_force_singlefile_output
 endif
 end subroutine part_pdata_out

 !--------------------------

 subroutine part_bdata_out(tnow,pid,jmp)

 character(9),dimension(5),parameter :: part=&
  (/'PSBunch1_','PSBunch2_','PSBunch3_','PSBunch4_','PSBunch5_'/)
 character(2) :: num2str
 character(3) :: num3str
 character(20) :: fname_out
 real(dp),intent(in) :: tnow
 integer,intent(in) :: pid,jmp
 real(sp),allocatable :: pdata(:)
 integer :: ik,p,q,np,ip,ip_max,nptot
 integer :: lenp,ip_loc(npe),ndv,i_end
 real(dp) :: wgh
 integer(offset_kind) :: disp
 real(sp) :: ch(2)
 character(4) :: folderName
 integer,parameter :: file_version = 4
 equivalence(wgh,ch)

 write (folderName,'(i4.4)') iout

 ndv=nd2+2
 np=loc_nbpart(imody,imodz,imodx,pid)
 ip=0
 do p=1,np,jmp
  ip=ip+1
  do q=1,nd2+1
   ebfb(ip,q)=bunch(pid)%part(p,q)
  end do
 end do
 ip_loc(mype+1)=ip

 ip=ip_loc(mype+1)
 call intvec_distribute(ip,ip_loc,npe)
 nptot=sum(ip_loc(1:npe))
 ip_max=ip
 if(pe0)ip_max=maxval(ip_loc(1:npe))
 lenp=ndv*ip
 allocate(pdata(lenp))
 ik=0
 do p=1,ip
  do q=1,nd2
   ik=ik+1
   pdata(ik)=real(ebfb(p,q),sp)
  end do
  wgh=ebfb(p,nd2+1)
  ik=ik+1
  pdata(ik)=ch(1)
  ik=ik+1
  pdata(ik)=ch(2)
 end do

 int_par=0
 call endian(i_end)
 real_par=0.0

 real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
  real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
  real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
  real(np_per_cell,sp),real(targ_in,sp),real(targ_end,sp),real(unit_charge(1),sp),&
  real(mass(1),sp),0.0_sp/)

 int_par(1:20) = &
  (/npe,nx,ny_loc,nz_loc,jmp,iby,iform,&
  model_id,dmodel_id,nsb,curr_ndim,mp_per_cell(1),&
  LPf_ord,der_ord,iform,pid,nptot,ndv,file_version,i_end/)

 write(num2str,'(i2.2)') iout
 write(num3str,'(i3.3)') imodz
 fname_out = foldername//'/'//part(pid)//num2str//'.bin'

 disp=0
 if(pe0)then
  open(10,file=foldername//'/'//part(pid)//num2str//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i10)')int_par
  write(10,*)' Real parameters'
  write (10,'(4e14.5)')real_par
  close(10)
  write(6,*)'Particles param written on file: '//foldername//'/'//part(pid)//num2str//'.dat'
 else
  disp=mype+ndv*sum(ip_loc(1:mype))  ! da usare con mpi_write
 endif

 disp=disp*4  ! sia gli int che i float sono di 4 bytes

 call mpi_write_part(pdata,lenp,ip,disp,20,fname_out)

 if(allocated(pdata))deallocate(pdata)
 if(pe0)then
  write(6,*)'Particles data written on file: '//foldername//'/'//part(pid)//num2str//'.bin'
  write(6,*)' Output logical flag ',L_force_singlefile_output
 endif
 end subroutine part_bdata_out

 !--------------------------
 subroutine energy_spect(np,ekem,gfield)
 integer,intent(in) :: np
 real(dp),intent(in) :: ekem
 real(dp),intent(in) :: gfield(:,:)
 integer :: p,ix,ne
 real(dp) :: xx,de,wgh
 ! activated only for np>0

 ne=size(nde0)
 de=ekem/real(ne,dp)
 if(ekem < 1.e-06)return
 do p=1,np
  xx=gfield(p,1)/de           !0.5*mc^2*(gamma-1) energy in MeV
  wgh=gfield(p,2)          !weight >0 to be multiplied by np_per_cell
  ix=nint(xx)
  ix=min(ix+1,ne)
  nde0(ix)=nde0(ix)+wgh
 end do
 end subroutine energy_spect

 subroutine select_energy_spect(np,ekem,xl,xr,gfield)
 integer,intent(in) :: np
 real(dp),intent(in) :: ekem,xl,xr
 real(dp),intent(in) :: gfield(:,:)
 integer :: p,ix,ne
 real(dp) :: xx,de,wgh
 ! activated only for np>0

 ne=size(nde0)
 de=ekem/real(ne,dp)
 if(ekem < 1.e-06)return
 do p=1,np
  xx=gfield(p,1)/de           !0.5*mc^2*(gamma-1) energy in MeV
  wgh=gfield(p,2)          !weight >0
  ix=nint(xx)
  ix=min(ix+1,ne)
  if(gfield(p,4)< xl)then
   nde0(ix)=nde0(ix)+wgh
  endif
  if(gfield(p,4)> xr)then
   nde1(ix)=nde1(ix)+wgh
  endif
 end do

 end subroutine select_energy_spect

 !--------------------------

 subroutine energy_momenta(sp_loc,gfield,np,pmass,ek,ekmax)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: gfield(:,:)
 integer,intent(in) :: np
 real(dp),intent(in) :: pmass
 real(dp),intent(out) :: ek(:),ekmax
 integer :: ip,ik
 real(dp) :: xp(3),vp(3),wgh,gam,gam1
 real(sp) :: charge(2)
 equivalence (wgh,charge)

 ek=0.0
 ekmax=0.0
 if(curr_ndim <3)then
  do ip=1,np
   vp(1:2)=sp_loc%part(ip,3:4)
   gam=sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2))
   gfield(ip,1)=pmass*(gam-1.)
   wgh=sp_loc%part(ip,5)
   gfield(ip,2)=charge(1)
   gfield(ip,3)=vp(1)
   gfield(ip,4)=sp_loc%part(ip,1)
   gam1=gam-1.
   do ik=1,curr_ndim
    ek(ik)=ek(ik)+charge(1)*vp(ik)
   end do
   ek(6)=ek(6)+charge(1)*charge(2)
   ek(7)=ek(7)+charge(1)*gam1
   ekmax=max(ekmax,gam1)
  end do
 else
  do ip=1,np
   xp(1:3)=sp_loc%part(ip,1:3)
   vp(1:3)=sp_loc%part(ip,4:6)
   gam=sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3))
   gfield(ip,1)=pmass*(gam-1.)
   wgh=sp_loc%part(ip,7)
   gfield(ip,2)=charge(1)
   gfield(ip,3)=vp(1)
   gfield(ip,4)=sp_loc%part(ip,1)
   gam1=gam-1.
   do ik=1,curr_ndim
    ek(ik)=ek(ik)+vp(ik)       !momenta
   end do
   ek(4)=ek(4)+charge(1)*(xp(2)*vp(3)-xp(3)*vp(2))
   ek(6)=ek(6)+charge(2)*charge(1)
   ek(7)=ek(7)+charge(1)*gam1
   ekmax=max(ekmax,gam1)
  end do
 endif
 end subroutine energy_momenta

 !--------------------------
 subroutine laser_struct_data(nst)

 integer,intent(in) :: nst
 integer :: i1,j1,k1,i2,nyp,nzp,ic,nb_tot
 integer :: ik,ix,iy,iz
 real(dp) :: xm,a2,ekt(10),xcm(10),eks(10),xcms(10)

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 xm=loc_xgrid(imodx)%gmin


 ekt=0.0
 xcm=0.0
  ! !data on driver laser field
  ! CComputes the COM x- coordinates of nb_laser Ey fields (=> group velocity)
  !===================
  ik=3
  if(ndim <3)ik=2
  nb_tot=nb_laser
  if(Two_color)nb_tot=nb_laser+1
  eavg(1:nb_tot,nst)=0.0    
!================ field component selection
  do ic=1,nb_laser
   if(lp_in(ic) > xm)then
    do iz=k1,nzp
     do iy=j1,nyp
      do ix=i1,i2
       if(x(ix)>=lp_in(ic).and.x(ix) <=lp_end(ic))then
        a2=ebf(ix,iy,iz,ik)*ebf(ix,iy,iz,ik)
        xcm(ic)=xcm(ic)+x(ix)*a2
        ekt(ic)=ekt(ic)+a2
       endif
      end do
     end do
    end do
   endif
  enddo
  if(Two_color)then
   if(lp_ionz_in > xm)then
    ic=nb_laser+1
    do iz=k1,nzp
     do iy=j1,nyp
      do ix=i1,i2
       if(x(ix)>= lp_ionz_in.and.x(ix) <=lp_ionz_end)then
        a2=ebf(ix,iy,iz,ik)*ebf(ix,iy,iz,ik)
        xcm(ic)=xcm(ic)+x(ix)*a2
        ekt(ic)=ekt(ic)+a2
       endif
      end do
     end do
    end do
   endif
  endif
  eks(1:nb_tot)=ekt(1:nb_tot)
  xcms(1:nb_tot)=xcm(1:nb_tot)
  call allreduce_dpreal(SUMV,ekt,eks,nb_tot)
  call allreduce_dpreal(SUMV,xcm,xcms,nb_tot)
  do ic=1,nb_tot
   if(eks(ic) >0.0)eavg(ic,nst)=xcms(ic)/eks(ic)          !Sum(xE^2)/sum(E^2) 
  end do
  !=================
 end subroutine laser_struct_data

 subroutine envelope_struct_data(nst)

 integer,intent(in) :: nst
 integer :: i1,j1,k1,i2,nyp,nzp,kk
 integer :: ik,ix,iy,iz,i01,i02,i0_lp
 real(dp) :: ekt(7),ekm(7)
 real(dp) :: dvol,dgvol
 real(dp) :: dar,dai,a2,aph1,aph2
 real(dp),parameter :: field_energy=1.156e-06

 dgvol=dx*dy*dz
 if(ndim==2)dgvol=dx*dy*dy
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)


 ekt=0.0
 i0_lp=3+nint(dx_inv*(lp_in(1)-xmin))

  ! env(1)=Re[A], env(2)=Im[A] A in adimensional form
  aph1=0.5*dx_inv
  aph2=0.0
  i01=i1+1
  i02=i2-1
  if(der_ord==4)then
   aph1=4.*dx_inv/3.
   aph2=-dx_inv/6.
   i01=i01+1
   i02=i02-1
  endif
  kk=0
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i01,i02
     ik=ix-2
     dar=aph1*(env(ix+1,iy,iz,1)-env(ix-1,iy,iz,1))+aph2*(&
      env(ix+2,iy,iz,1)-env(ix-2,iy,iz,1))
     dai=aph1*(env(ix+1,iy,iz,2)-env(ix-1,iy,iz,2))+aph2*(&
      env(ix+2,iy,iz,2)-env(ix-2,iy,iz,2))
     a2=env(ix,iy,iz,1)*env(ix,iy,iz,1)+&
      env(ix,iy,iz,2)*env(ix,iy,iz,2)

     ekt(1)=ekt(1)+x(ik)*a2         ! Centroid
     ekt(2)=ekt(2)+a2               ! !A|^2
     ekt(6)=dai*env(ix,iy,iz,1)-dar*env(ix,iy,iz,2)
     ekt(3)=ekt(3)+oml*oml*a2+ 2.*oml*ekt(6)+dar*dar+dai*dai
     !|Z|^2=(Ey^2+Bz^2)/2= field energy
     ekt(4)=ekt(4)+oml*a2+ekt(6)    ! Action
     ekt(5)=max(ekt(5),sqrt(a2))    ! Max |A|
     kk=kk+1
    end do
   end do
  end do
  dvol=1./real(kk,dp)
  call allreduce_dpreal(SUMV,ekt,ekm,4)
  if(ekm(2)> 0.0)eavg(2,nst)=ekm(1)/ekm(2)  !Centroid
  eavg(3,nst)=field_energy*dgvol*ekm(3)   !Energy
  eavg(4,nst)=dvol*ekm(4)    !Action
  ekt(1)=ekt(5)
  if(ekt(1) > giant_field)then
   write(6,*)' WARNING: Env field too big ',ekt(1)
   write(6,'(a23,3i4)')' At the mpi_task=',imodx,imody,imodz
  endif
  ekm(1)=ekt(1)
  if(prl)call allreduce_dpreal(MAXV,ekt,ekm,1)
  eavg(1,nst)=ekm(1)
  if(Two_color)then
  kk=0
   ekt=0.0
  do iz=k1,nzp
   do iy=j1,nyp
     do ix=i1,i2
      ik=ix-2
      dar=aph1*(env1(ix+1,iy,iz,1)-env1(ix-1,iy,iz,1))+aph2*(&
      env1(ix+2,iy,iz,1)-env1(ix-2,iy,iz,1))
      dai=aph1*(env1(ix+1,iy,iz,2)-env1(ix-1,iy,iz,2))+aph2*(&
      env1(ix+2,iy,iz,2)-env1(ix-2,iy,iz,2))
      a2=env1(ix,iy,iz,1)*env1(ix,iy,iz,1)+&
      env1(ix,iy,iz,2)*env1(ix,iy,iz,2)

      ekt(1)=ekt(1)+x(ik)*a2         ! Centroid
      ekt(2)=ekt(2)+a2               ! !A|^2
      ekt(6)=dai*env1(ix,iy,iz,1)-dar*env1(ix,iy,iz,2)
      ekt(3)=ekt(3)+om1*om1*a2+ 2.*om1*ekt(6)+dar*dar+dai*dai
      !|Z|^2=(Ey^2+Bz^2)/2= field energy
      ekt(4)=ekt(4)+om1*a2+ekt(6)    ! Action
      ekt(5)=max(ekt(5),sqrt(a2))    ! Max |A|
     kk=kk+1
    end do
   end do
  end do
  dvol=1./real(kk,dp)
  call allreduce_dpreal(SUMV,ekt,ekm,4)
   if(ekm(2)> 0.0)eavg1(2,nst)=ekm(1)/ekm(2)  !Centroid
   eavg1(3,nst)=field_energy*dgvol*ekm(3)   !Energy
   eavg1(4,nst)=dvol*ekm(4)    !Action
  ekt(1)=ekt(5)
   if(ekt(1) > giant_field)then
    write(6,*)' WARNING: Env field too big ',ekt(1)
    write(6,'(a23,3i4)')' At the mpi_task=',imodx,imody,imodz
   endif
  ekm(1)=ekt(1)
  if(prl)call allreduce_dpreal(MAXV,ekt,ekm,1)
   eavg1(1,nst)=ekm(1)
 endif
 end subroutine envelope_struct_data
 !===========================
 subroutine fields_on_target(nst)

 integer,intent(in) :: nst
 integer :: i1,j1,k1,i2,nyp,nzp,ii
 integer :: ik,ix,iy,iz
 real(dp) :: ekt(7),ekm(7)
 real(dp) :: dgvol
 real(dp),parameter :: field_energy=1.156e-06
 dgvol=dx*dy*dz
 if(ndim==2)dgvol=dx*dy*dy
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 ekt=0.0
 do ix=i1,i2
  ii=ix-2
  if(x(ii)>= targ_in)then
   do ik=1,nfield
    do iz=k1,nzp
     do iy=j1,nyp
      ekt(ik)=ekt(ik)+ebf(ix,iy,iz,ik)*ebf(ix,iy,iz,ik)
     end do
    end do
   end do
  endif
 enddo
 ekt(1:nfield)=dgvol*ekt(1:nfield)
 call allreduce_dpreal(SUMV,ekt,ekm,nfield)
 eavg(1:nfield,nst)=field_energy*ekm(1:nfield)
 !=======================
 end subroutine fields_on_target

 subroutine envar(nst,tnow)

 integer,intent(in) :: nst
 real(dp),intent(in) :: tnow

 integer :: np,ik,ix,iy,iz,ic,i1,i2,i2b,ndv
 integer :: j1,k1,nyp,nzp,ii,jj,kk,j,k,l
 real(dp) :: ek_max(1),ekt(7),ekm(7),ekmax(1)
 real(dp) :: pmass,dvol,dgvol,sgz,sg,ef2
 real(dp) :: np_norm,p_energy_norm
 real(dp),parameter :: mev_to_joule=1.602e-13
 real(dp),parameter :: field_energy=1.156e-06
 integer,parameter :: zg_ind(6)=(/3,3,4,4,4,3/)
 integer,parameter :: yg_ind(6)=(/3,4,3,4,3,4/)
 integer,parameter :: xg_ind(6)=(/4,3,3,3,4,4/)
 !================================================
 ! field_energy transforms the energy density u=E^2/2 in adimensional
 ! form to energy density in Joule/mu^3
 ! field_energy =epsilon_0*(E_0^2)/2 in SI or
 !              =(E_0^2)/8pi         in cgs (Gaussian) units
 !==================================================

 dgvol=dx*dy*dz
 if(ndim==2)dgvol=dx*dy*dy
 ndv=nd2+1
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)

 tloc(nst)=tnow
 tsp(nst)=tnow
 if(nst==1)then
  pavg =0.0
  favg=0.0
  eavg=0.0
  nde_sp=0.0
  nde_sm=0.0
  nde=0.0
 endif
 ekt(1)=real(nx*ny*nz,dp)
 dvol=1./ekt(1)

 if(Part)then
  p_energy_norm=np_per_cell*mev_to_joule
  do ic=1,nsp
   ekm=0.0
   ekt=0.0
   ekmax=0.0
   ek_max=0.0
   np=loc_npart(imody,imodz,imodx,ic)
   ekt(1)=real(np,dp)
   call allreduce_dpreal(SUMV,ekt,ekm,1)
   np_norm=1.
   if(ekm(1)>0.0)np_norm=1.0/ekm(1)
   pmass=electron_mass*mass(ic)    !In MeV
   ekt(1)=0.0
   ekm(1)=0.0
   if(np>0)then
    call energy_momenta(spec(ic),ebfp,np,pmass,ekt,ekmax(1))
    !  WARNING: total variables multipied by a weight = 1/nmacro_per cell
    !  Momenta ekt(1:3) are averaged by the macroparticle total number
    !==================================
    !
    ekt(4)=pmass*ekt(4)           !Total angular momentum (Mev/c^2)
    ekt(7)=pmass*ekt(7)           !the ic-species TOTAL energy (MeV)
    ekmax(1)=pmass*ekmax(1)

   endif
   call allreduce_dpreal(MAXV,ekmax,ek_max,1)
   !============= spectra section
   nde0(1:ne)=0.0
   if(np>0)call energy_spect(np,ek_max(1),ebfp)
   nde1(1:ne)=nde0(1:ne)
   call allreduce_dpreal(SUMV,nde0,nde1,ne)
   nde(1:ne,nst,ic)=nde1(1:ne)
   if(Solid_target)then
    nde0(1:ne)=0.0
    nde1(1:ne)=0.0
    if(np>0)call select_energy_spect(np,ek_max(1),&
     targ_in,targ_end,ebfp)
    nde2(1:ne)=nde0(1:ne)
    call allreduce_dpreal(SUMV,nde0,nde2,ne)
    nde_sm(1:ne,nst,ic)=nde2(1:ne)
    nde2(1:ne)=nde1(1:ne)
    call allreduce_dpreal(SUMV,nde1,nde2,ne)
    nde_sp(1:ne,nst,ic)=nde2(1:ne)
   endif
   !======================= end spectra section
   call allreduce_dpreal(SUMV,ekt,ekm,7)
   do ik=1,curr_ndim
    ekm(ik)=ekm(ik)*np_norm          !Average Momenta
   end do
   !======================= phase space integrated data for each species
   eksp_max(nst,ic)=ek_max(1)
   pavg(1,nst,ic)=p_energy_norm*ekm(7) !Total energy of ic species (Joule)
   pavg(2,nst,ic)=ek_max(1)           ! Max energy (MeV)
   pavg(3,nst,ic)=mev_to_joule*ekm(4)    !total angular Momenta
   pavg(4:6,nst,ic)=pmass*ekm(1:3)       !averaged linear Momenta (MeV/c)
   pavg(10,nst,ic)=np_norm*ekm(6)        !Mean charge
   pavg(11,nst,ic)=dvol*ekm(6)           !Charge per cell
   ekt(1:3)=0.0
   if(np >0)then
    do ik=1,curr_ndim
     kk=ik+curr_ndim
     do ix=1,np
      sg=spec(ic)%part(ix,kk)-ekm(ik)
      ekt(ik)=ekt(ik)+sg*sg              !<[p -<p>]^2>, p=gamma*v/c
     end do
    end do
   endif
   call allreduce_dpreal(SUMV,ekt,ekm,3)
   ekm(1:3)=np_norm*ekm(1:3)
   do ik=1,curr_ndim
    pavg(6+ik,nst,ic)=1.e+03*pmass*ekm(ik)!
    !sigma^2 of particle momenta (in KeV)
   end do
  end do
 endif  ! End of particles section
 if(Ionization)call enb_ionz(nst,tnow)
 !======================== Field  section
 ekt=0.0
 ekm=0.0
 if(Stretch)then
  if(ndim==3)then
   do ik=1,nfield
    k=zg_ind(ik)           !staggering of stretched grid cell
    j=yg_ind(ik)
    l=xg_ind(ik)
    do iz=k1,nzp
     kk=iz-2
     sgz=1./loc_zg(kk,k,imodz)
     do iy=j1,nyp
      jj=iy-2
      sg=sgz/loc_yg(jj,j,imody)
      do ix=i1,i2
       ii=ix-2
       dvol=sg/loc_xg(ii,l,imodx)
       ekt(ik)=ekt(ik)+ &
        dvol*ebf(ix,iy,iz,ik)*ebf(ix,iy,iz,ik)
      end do
     end do
    end do
   enddo
  else
   do ik=1,nfield
    j=yg_ind(ik)
    l=xg_ind(ik)
    do iz=k1,nzp
     sgz=1.
     do iy=j1,nyp
      jj=iy-2
      sg=sgz/loc_yg(jj,j,imody)
      do ix=i1,i2
       ii=ix-2
       dvol=sg/loc_xg(ii,l,imodx)
       ekt(ik)=ekt(ik)+dvol*ebf(ix,iy,iz,ik)*ebf(ix,iy,iz,ik)
      end do
     end do
    end do
   enddo
  endif
 else
  do ik=1,nfield
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,i2
      ekt(ik)=ekt(ik)+ &
       ebf(ix,iy,iz,ik)*ebf(ix,iy,iz,ik)
     end do
    end do
   end do
  enddo
 endif
 ekt(1:nfield)=dgvol*ekt(1:nfield)
 call allreduce_dpreal(SUMV,ekt,ekm,nfield)
 favg(1:3,nst)=field_energy*ekm(1:3)        !field itotal energy (in Joule)
 favg(7:9,nst)=field_energy*ekm(4:6)

 ekt=0.0
 do iz=k1,nzp
  do iy=j1,nyp
   do ix=i1,i2
    ef2=dot_product(ebf(ix,iy,iz,1:curr_ndim),ebf(ix,iy,iz,1:curr_ndim))
    ekt(7)=max(ekt(7),ef2)
   end do
  end do
 end do
 do ik=1,nfield
  if(ekm(ik) >0.0)ekt(ik)=maxval(abs(ebf(i1:i2,j1:nyp,k1:nzp,ik)))
  if(ekt(ik) > giant_field)then
   write(6,*)' WARNING: Ebf field too big at component=',ik
   write(6,*)'max fields',mype,ekt(ik)
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,i2
      if(abs(ebf(ix,iy,iz,ik)-ekt(ik)) < epsilon)then
       ii=ix
       jj=iy
       kk=iz
      endif
     end do
    end do
   end do
   write(6,'(a23,3i4)')' At the mpi_task=',imodx,imody,imodz
   write(6,'(a19,3i6)')' At the local grid=',ii,jj,kk
  endif
 end do
 ekm=0.0  ! Max values
 call allreduce_dpreal(MAXV,ekt,ekm,7)
 favg(4:6,nst)=E0*ekm(1:3)      !Max fields in TV/m
 favg(10:12,nst)=E0*ekm(4:6)
 !=====================================
 if(Beam)then
  i2b=nx+2
  ekt=0.0
  select case(ibeam)
  case(0)
   do ik=1,nfield
    kk=0
    do iz=k1,nzp
     do iy=j1,nyp
      do ix=i1,i2b
       ekt(ik)=ekt(ik)+ &
        ebf_bunch(ix,iy,iz,ik)*ebf_bunch(ix,iy,iz,ik)
       kk=kk+1
      end do
     end do
    end do
    if(kk>0)ekt(ik)=ekt(ik)/real(kk,dp)
   enddo
   ekt(1:nfield)=0.5*ekt(1:nfield)               ! rms variables
   ekm(1:nfield)=ekt(1:nfield)
   if(prl)call allreduce_dpreal(SUMV,ekt,ekm,nfield)
   favg(13:15,nst)=ekm(1:3)/real(npe,dp)
   favg(19:21,nst)=ekm(4:6)/real(npe,dp)
   ekm=0.0
   ekt=0.0
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,i2
      ef2=dot_product(ebf(ix,iy,iz,1:curr_ndim),ebf(ix,iy,iz,1:curr_ndim))
      ekt(7)=max(ekt(7),ef2)
     end do
    end do
   end do
   do ik=1,nfield
    ekt(ik)=maxval(abs(ebf_bunch(i1:i2b,j1:nyp,k1:nzp,ik)))
   end do
   if(maxval(ekt(1:nfield)) > giant_field)then
    write(6,*)' WARNING: Bunch field too big'
    write(6,'(a23,3i4)')' At the mpi_task=',imodx,imody,imodz
   endif
   ekm(1:nfield)=ekt(1:nfield)
   if(prl)call allreduce_dpreal(MAXV,ekt,ekm,7)
   favg(16:18,nst)=ekm(1:3)
   favg(22:24,nst)=ekm(4:6)
   eb_max=0.0
   if(ekm(7)>0.0)eb_max=sqrt(ekm(7))
   !============================
  case(1)
   do ik=1,nfield
    kk=0
    do iz=k1,nzp
     do iy=j1,nyp
      do ix=i1,i2b
       ekt(ik)=ekt(ik)+ &
        (ebf_bunch(ix,iy,iz,ik)+ebf1_bunch(ix,iy,iz,ik))*(&
        ebf_bunch(ix,iy,iz,ik)+ebf1_bunch(ix,iy,iz,ik))
       kk=kk+1
      end do
     end do
    end do
    if(kk>0)ekt(ik)=ekt(ik)/real(kk,dp)
   enddo
   ekt(1:nfield)=0.5*ekt(1:nfield)               ! rms variables
   ekm(1:nfield)=ekt(1:nfield)
   if(prl)call allreduce_dpreal(SUMV,ekt,ekm,nfield)
   favg(13:15,nst)=ekm(1:3)/real(npe,dp)
   favg(19:21,nst)=ekm(4:6)/real(npe,dp)
   ekm=0.0
   ekt=0.0
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,i2
      ef2=0.0
      do ik=1,curr_ndim
       ef2=ef2+(ebf_bunch(ix,iy,iz,ik)+ebf1_bunch(ix,iy,iz,ik))*(&
        ebf_bunch(ix,iy,iz,ik)+ebf1_bunch(ix,iy,iz,ik))
      end do
      ekt(7)=max(ekt(7),ef2)
     end do
    end do
   end do
   do ik=1,nfield
    ef2=0.0
    do iz=k1,nzp
     do iy=j1,nyp
      do ix=i1,i2
       ef2=max(ef2,abs((ebf_bunch(ix,iy,iz,ik)+ebf1_bunch(ix,iy,iz,ik))))
      end do
     end do
    end do
    ekt(ik)=ef2
   end do
   if(maxval(ekt(1:nfield)) > giant_field)then
    write(6,*)' WARNING: Bunch field too big'
    write(6,'(a23,3i4)')' At the mpi_task=',imodx,imody,imodz
   endif
   ekm(1:nfield)=ekt(1:nfield)
   if(prl)call allreduce_dpreal(MAXV,ekt,ekm,7)
   favg(16:18,nst)=ekm(1:3)
   favg(22:24,nst)=ekm(4:6)
   eb_max=0.0
   if(ekm(7)>0.0)eb_max=sqrt(ekm(7))
  end select
 endif
 if(Wake)then
  if(Envelope)then
   call envelope_struct_data(nst)
  !else
  ! call laser_struct_data(nst)
  endif
 endif
 if(Solid_target)call fields_on_target(nst)

 end subroutine envar
 !--------------------------

 subroutine en_data(nst,itr,idata)

 integer,intent(in) :: nst,itr,idata
 character(6) :: fname='      '
 character(14),dimension(4),parameter:: sp_type=(/&
  '   Electrons  ','  A1-Z1 Ions  ','  A2-Z2 Ions  ','  A3-Z3 Ions  '/)

 character(14),dimension(11), parameter:: pe=(/&
  ' Tot Ek[J]    ',' Ek_max[Mev]  ',' Jz ang-moment', &
  '<px>-momentum ','<py>-momentum ','<pz>-momentum ', &
  'sigma_px[KeV] ','sigma_py[KeV] ','sigma_pz[KeV] ', &
  'Mean Charge   ','Charge percell'/)
 character(14),dimension(16), parameter:: fe=(/&
  'Ex2(J)        ','Ey2(J)        ','Ez2(J)        ',&
  'Ex_max(TV/m)  ','Ey_max(TV/m)  ','Ez_max(TV/m)  ',&
  'Bx2(J)        ','By2(J)        ','Bz2(J)        ',&
  'Bx_max(TV/m)  ','By_max(TV/m)  ','Bz_max(TV/m)  ',&
  '  E2(x<X_t)   ','  B2(x<X_t)   ',&
  '  E2(x>X_t)   ','  B2(x>X_t)   '/)
 character(14),dimension(6), parameter:: fe2=(/&
  'Ex2(J)        ','Ey2(J)        ','Bz2(J)        ',&
  'Ex_max(TV/m)  ','Ey_max(TV/m)  ','Bz_max(TV/m)  '/)
 character(14),dimension(6), parameter:: feb2=(/&
  'Ex2(J)        ','Ey2(J)        ','Bz2(J)        ',&
  'Ex_max(GV/m)  ','Ey_max(GV/m)  ','Bz_max(GV/m)  '/)
 character(14),dimension(16), parameter:: feb=(/&
  'Ex2(J)        ','Ey2(J)        ','Ez2(J)        ',&
  'Ex_max(GV/m)  ','Ey_max(GV/m)  ','Ez_max(GV/m)  ',&
  'Bx2(J)        ','By2(J)        ','Bz2(J)        ',&
  'Bx_max(GV/m)  ','By_max(GV/m)  ','Bz_max(GV/m)  ',&
  '  E2(x<X_t)   ','  B2(x<X_t)   ',&
  '  E2(x>X_t)   ','  B2(x>X_t)   '/)
 character(14),dimension(6), parameter:: lfenv=(/&
  '  COM(1)      ','   COM(2)     ',' COM(3)       ','  COM(4)      ',&
  '  COM(5)      ','   COM(6)     '/)
 character(14),dimension(4), parameter:: fenv=(/&
  '  Env_max     ','   Centroid   ',' Env_energy   ','  Env_action  '/)
 !character(14),dimension(5), parameter:: flaser=(/&
 ! '  Int_max     ',' Las_energy(J)','  < X_c >     ','   <W_y>      ',&
 ! '   < W_z >    '/)
 character(14),dimension(6), parameter:: flt=(/&
  'Ex2(J)        ','Ey2(J)        ','Ez2(J)        ',&
  'Bx2(J)        ','By2(J)        ','Bz2(J)        '/)

 character(12),dimension(4), parameter:: enspect=(/&
  'Electron NdE',' A1-ion NdE ',' A2-ion NdE ',' A3-ion NdE '/)

 integer :: ik,ic,nfv,npv,t_ord,nt,ne,color
 integer,parameter :: lun=10
 nfv=6
 if(curr_ndim==3)nfv=12
 npv=9
 t_ord=max(RK_ord,LPf_ord)
 color=0
 if(Two_color)color=1

 ! if (iout<100) write (fname,'(a4,i2)') 'diag' ,idata
 ! if (iout< 10) write (fname,'(a5,i1)') 'diag0',idata
 if(Ionization)call en_ionz_data(nst,itr,idata)
 write (fname,'(a4,i2.2)') 'diag',idata

 open (lun,file='diagnostics/'//fname//'.dat',form='formatted')
 write(lun,*)'mod_id,dmodel_id LP_ord,der_ord, ibeam, color'
 write(lun,'(6i8)')model_id,dmodel_id,LPf_ord,der_ord,ibeam,color
 write(lun,*)'Z1_i,  A1_i,   Z2_i,   A2_i,   iform,    str'
 write(lun,'(6i6)')ion_min(1),atomic_number(1),ion_min(2),atomic_number(2),iform,str_flag
 write(lun,*)' xmax       xmin       ymax      ymin      '
 write(lun,'(4e12.4)')xmax,xmin,ymax,ymin
 if(model_id <= 4)then
  write(lun,*)' lam0       w0x       w0y        energy'
  write(lun,'(4e11.4)')lam0,w0_x,w0_y,lp_energy
  write(lun,*)' a0        lp_int     lp_pow    energy_on_targ'
  write(lun,'(4e12.4)')a0,lp_intensity,lp_pow, energy_in_targ
  write(lun,*)' targ_x1  targ_x2     n/nc       el_lp        '
  write(lun,'(4e12.4)')targ_in,targ_end,n_over_nc,el_lp
  if(dmodel_id > 5)then
   write(lun,*)'  lx2        lx3        lx4         dw         lw '
   write(lun,'(5e12.4)')lpx(2:4),lpy(1:2)
  else
   write(lun,*)' lx1        lx2        lx3        lx4        lx5 '
   write(lun,'(5e12.4)')lpx(1:5)
  endif
  write(lun,*)' ompe2       nmacro       np_per_cell    '
  write(lun,'(3e12.4)')ompe,nmacro,np_per_cell
  write(lun,*)'    Nx      Ny      Nz    n_cell   Nsp  Nb_las'
  write(lun,'(6i8)')nx,ny,nz,mp_per_cell(1), nsp,  nb_laser
  write(lun,*)' iter, nst, nvar npvar'
  write(lun,'(4i6)')itr,nst,nfv,npv
 endif
 if(model_id > 4)then
  write(lun,*)' Fields are in units GV/m'
  write(lun,*)' The driving bunches'
  write(lun,*)' plvol      lambda_p   '
  write(lun,'(2e12.3)')lpvol,lambda_p
  do ik=1,nsb
   write(lun,*)' Qcharge    b_charge     sigmx      sigmy'
   write(lun,'(4e12.3)')reduced_charge(ik),bunch_charge(ik),sxb(ik),syb(ik)
   write(lun,*)' eps_y       eps_z       gamma      dg/g'
   write(lun,'(4e12.3)')epsy(ik),epsz(ik),gam(ik),dg(ik)
  end do
  write(lun,*)'np/nc[10^18/cm3]           n_b/n_p'
  write(lun,'(e12.4,a8,e11.4)')n_over_nc,'        ',nb_over_np
  write(lun,*)' targ_x1  targ_x2     n/nc       el_lp        '
  write(lun,'(4e12.4)')targ_in,targ_end,n_over_nc,el_lp
  write(lun,*)' lx1        lx2        lx3        lx4        lx5 '
  write(lun,'(5e12.4)')lpx(1:5)
  write(lun,*)' ompe2       nmacro       np_per_cell    '
  write(lun,'(3e12.4)')ompe,nmacro,np_per_cell
  write(lun,*)'    Nx      Ny      Nz    n_cell   Nsp  Nsb'
  write(lun,'(6i8)')nx,ny,nz,mp_per_cell(1),nsp,nsb
  write(lun,*)' iter, nst, nvar npvar'
  write(lun,'(4i6)')itr,nst,nfv,npv
 endif
 write(lun,*)'====================================='
 write(lun,*)'time'
 write(lun,'(5e11.4)')tloc(1:nst)
 if(Part)then
  write(lun,*)'========== Particle section======='
  do ic=1,nsp
   write(lun,'(a14)')sp_type(ic)
   write(lun,'(6a14)')pe(1:6)
   do ik=1,nst
    write(lun,'(6e13.5)')pavg(1:6,ik,ic)
   end do
   write(lun,'(5a14)')pe(7:11)
   do ik=1,nst
    write(lun,'(5e13.5)')pavg(7:11,ik,ic)
   end do
  end do
 endif         !END particle section
 write(lun,*)'========== Fields section======='
 if(nfield < 6)then
  if(Beam)then
   write(lun,'(6a14)')feb2(1:6)
  else
   write(lun,'(6a14)')fe2(1:6)
  endif
  do ik=1,nst
   write(lun,'(6e13.5)')favg(1:6,ik)
  end do
 else
  if(Beam)then
   write(lun,'(6a14)')feb(1:6)
  else
   write(lun,'(6a14)')fe(1:6)
  endif
  do ik=1,nst
   write(lun,'(6e13.5)')favg(1:6,ik)
  end do
  if(Beam)then
   write(lun,'(6a14)')feb(7:12)
  else
   write(lun,'(6a14)')fe(7:12)
  endif
  do ik=1,nst
   write(lun,'(6e13.5)')favg(7:12,ik)
  end do
 endif
 if(Beam)then
  write(lun,*)'========== BUNCH fields section======='
  if(nfield < 6)then
   write(lun,'(6a14)')feb2(1:6)
   do ik=1,nst
    write(lun,'(6e13.5)')favg(13:18,ik)
   end do
  else
  write(lun,'(6a14)')feb(1:6)
  do ik=1,nst
   write(lun,'(6e13.5)')favg(13:18,ik)
  end do
  write(lun,'(6a14)')feb(7:12)
  do ik=1,nst
   write(lun,'(6e13.5)')favg(19:24,ik)
  end do
 endif
 endif
 if(Wake)then
 if(Envelope)then
   write(lun,*)'====  the leading pulse integrated variables'
  write(lun,'(4a14)')fenv(1:4)
  do ik=1,nst
   write(lun,'(4e13.5)')eavg(1:4,ik)
  end do
   if(Two_color)then
    write(lun,*)'====  the injection pulse integrated variables'
    write(lun,'(4a14)')fenv(1:4)
    do ik=1,nst
     write(lun,'(4e13.5)')eavg1(1:4,ik)
    end do
   endif
  endif
 endif
 if(Solid_target)then
  write(lun,*)'====  Field energy on solid targets'
  if(nfield==6)then
   write(lun,'(6a14)')flt(1:6)
   do ik=1,nst
    write(lun,'(6e13.5)')eavg(1:6,ik)
   end do
  else
   write(lun,'(3a14)')flt(1:3)
   do ik=1,nst
    write(lun,'(3e13.5)')eavg(1:3,ik)
   end do
  endif
 endif
 close(lun)

 if(nst >0)then
  ! if (iout<100) write (fname,'(a4,i2)') 'spec' ,idata
  ! if (iout< 10) write (fname,'(a5,i1)') 'spec0',idata
  write (fname,'(a4,i2.2)') 'spec',idata
  open (lun,file='diagnostics/'//fname//'.dat',form='formatted')
  write(lun,*)'mod_id,dmodel_id LP_ord,der_ord'
  write(lun,'(4i8)')model_id,dmodel_id,LPf_ord,der_ord
  write(lun,*)'Z1_i,A1_i,Z2_i,A2_i,iform, str'
  write(lun,'(6i4)')ion_min(1),atomic_number(1),ion_min(2),atomic_number(2),iform,str_flag
  write(lun,*)' xmax       xmin       ymax      ymin      '
  write(lun,'(4e12.4)')xmax,xmin,ymax,ymin
  if(model_id <= 4)then
   write(lun,*)' lam0       w0x       w0y        tau'
   write(lun,'(4e12.4)')lam0,w0_x,w0_y,tau_fwhm
   write(lun,*)' a0        lp_int     lp_pow'
   write(lun,'(3e12.4)')a0,lp_intensity,lp_pow
   write(lun,*)' targ_x1  targ_x2     n/nc       el_lp        '
   write(lun,'(4e12.4)')targ_in,targ_end,n_over_nc,el_lp
   write(lun,*)' lx1        lx2          lx3          lx4        lx5 '
   write(lun,'(5e12.4)')lpx(1:5)
   write(lun,*)' ompe2       nmacro       np_per_cell    '
   write(lun,'(3e12.4)')ompe,nmacro,np_per_cell
  endif
  write(lun,*)'    Nx      Ny      Nz    n_cell   Nsp  Nsb'
  write(lun,'(6i8)')nx,ny,nz,mp_per_cell(1),nsp,nsb
  write(lun,*)' iter, nst, nvar npvar'
  write(lun,'(4i6)')itr,nst,nfv,npv
  write(lun,*)'             ENERGY SPECTRA            '
  ne=size(nde,1)
  write(lun,'(i4)')ne
  do ik=1,nsp
   write(lun,*)enspect(ik)
   do nt=1,nst
    write(lun,*)' time   emax'
    write(lun,'(2e13.5)')tsp(nt),eksp_max(nt,ik)
    write(lun,*)'Global  spectral  data  '
    write(lun,'(6e13.5)')nde(1:ne,nt,ik)
    if(Solid_target)then
     write(lun,*)'Front side  selected spectral  data  '
     write(lun,'(6e13.5)')nde_sm(1:ne,nt,ik)
     write(lun,*)'Rear side   selected spectral  data  '
     write(lun,'(6e13.5)')nde_sp(1:ne,nt,ik)
    endif
   end do
  end do
  close(lun)
 endif
 end subroutine en_data
 !--------------------------
  subroutine bunch_corr(bch,np_loc,np_norm,bcorr)
  real(dp),intent(in) :: bch(:,:)
  integer,intent(in) :: np_loc
  real(dp),intent(out) :: bcorr(16)
  real(dp),intent(in) :: np_norm

  integer :: ik,ic,kk,ndv
  real(dp) :: gmb,pp(3),mu(7),ekt(9),ekm(9)
  real(dp) :: corr2(8),emy,emz,dgam
 !=====================
  bcorr=0.0
  mu=0.0
  corr2=0.0
  ndv=2*curr_ndim
  ekt=0.0
  if(np_loc>0)then
   do ik=1,ndv
    ekt(ik)=sum(bch(1:np_loc,ik)) !averages six phase space coordinates
   enddo
   if(curr_ndim==2)then
    do kk=1,np_loc
     pp(1:2)=bch(kk,3:4)
     gmb=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2))
     ekt(ndv+1)=ekt(ndv+1)+gmb
    end do
    ekt(7)=ekt(ndv+1)  !<gam>
    ekt(6)=0.0         !<Pz>
    ekt(5)=ekt(4)      !<Py>
    ekt(4)=ekt(3)      !<Px>
    ekt(3)=0.0          !<z>
   else    
    do kk=1,np_loc
     pp(1:3)=bch(kk,4:6)
     gmb=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3))
     ekt(7)=ekt(7)+gmb
    end do
   endif
  endif
  call allreduce_dpreal(SUMV,ekt,ekm,7)
  mu(1:7)=np_norm*ekm(1:7) !Averages <(x,y,z,Px,Py,Pz,gamma)>
  !=========== 2th moments
  ekm=0.0
  ekt=0.0
  if(np_loc>0)then
   do ik=1,ndv
    do kk=1,np_loc
     ekt(ik)=ekt(ik)+bch(kk,ik)*bch(kk,ik)
    end do
   enddo
   if(curr_ndim==2)then
    ekt(6)=0.0         !<Pz*Pz>
    ekt(5)=ekt(4)      !<Py*Py>
    ekt(4)=ekt(3)      !<Px+Px>
    ekt(3)=0.0         !<z*z>
   !================mixed corr
    do kk=1,np_loc
     ekt(7)=ekt(7)+bch(kk,4)*bch(kk,2)  !<y*py>
     ekt(8)=0.0
     pp(1:2)=bch(kk,3:4)
     gmb=1.+pp(1)*pp(1)+pp(2)*pp(2)
     ekt(9)=ekt(9)+gmb       !<(gam**2>
    end do
   else
    do kk=1,np_loc
     ekt(7)=ekt(7)+bch(kk,5)*bch(kk,2)  !<yPy>
     ekt(8)=ekt(8)+bch(kk,6)*bch(kk,3)  !<zPz>
     pp(1:3)=bch(kk,4:6)
     gmb=1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)
     ekt(9)=ekt(9)+gmb                            !<(gam**2>
    end do
   endif
  endif
  call allreduce_dpreal(SUMV,ekt,ekm,9)
  ekm(1:9)=np_norm*ekm(1:9)
  do ik=1,6
   corr2(ik)=ekm(ik)-mu(ik)*mu(ik)
  end do
  !    emy^2= corr2_y*corr2_py -mixed
  !<yy><p_yp_y>-(<yp_y>-<y><p_y>)^2
  emy=corr2(2)*corr2(5)-(corr2(7)-mu(2)*mu(5))**2
  emz=corr2(3)*corr2(6)-(corr2(8)-mu(3)*mu(6))**2
  if(emy>0.0)emy=sqrt(emy)
  if(emz>0.0)emz=sqrt(emz)
  gmb=mu(7)*mu(7)
  dgam=0.0
  if(gmb >0.0)dgam=ekm(9)/gmb -1.0
  if(dgam >0.0)dgam=sqrt(dgam)   !Dgamm/gamma
  bcorr(1:6)=mu(1:6)
  bcorr(7:12)=corr2(1:6)
  bcorr(13)=emy
  bcorr(14)=emz
  bcorr(15)=mu(7)
  bcorr(16)=dgam
 !==========================
 end subroutine bunch_corr

 subroutine enbvar(nst,tnow)
 integer,intent(in) :: nst
 real(dp),intent(in) :: tnow

 integer :: ic,np,ndv,p
 real(dp) :: np_norm,bcorr(16),ekt(1),ekm(1)
 !=====================
 if(nst==0)bavg=0.0
 tb(nst)=tnow
 ndv=size(ebfb,2)
 do ic=1,nsb
  np=loc_nbpart(imody,imodz,imodx,ic)
  do p=1,np
   ebfb(p,1:ndv)=bunch(ic)%part(p,1:ndv)
  end do
  ekt(1)=real(np,dp)
  call allreduce_dpreal(SUMV,ekt,ekm,1)
  np_norm=1.
  if(ekm(1) >0.0)np_norm=1./ekm(1)

  call  bunch_corr(ebfb,np,np_norm,bcorr)

  bavg(nst,1:16,ic)=bcorr(1:16)
 end do
 !==========================
 end subroutine enbvar

 !=====================================

 subroutine enb_ionz(nst,tloc)
 integer,intent(in) :: nst
 real(dp),intent(in) :: tloc

 integer :: ik,np,p,q,id_ch
 real(dp) :: np_norm,bcorr(16),ekt(1),ekm(1)
 real(dp) :: wgh
 real(sp) :: ch(2),ch_ion
 equivalence(wgh,ch)

 ik=0
 ch_ion=real(wgh_ion,sp)
 np=loc_npart(imody,imodz,imodx,1)
 id_ch=size(ebfp,2)
 if(np > 0)then
  do p=1,np
   wgh=spec(1)%part(p,id_ch)
   if(abs(ch(1)-ch_ion)< 1.e-05)then
    ik=ik+1
    do q=1,nd2+1
     ebfp(ik,q)=spec(1)%part(p,q)
    end do
   endif
  end do
 endif
 if(nst==0)ionz_bavg=0.0
 tionz(nst)=tloc
!================================
  ekt(1)=real(ik,dp)
  call allreduce_dpreal(SUMV,ekt,ekm,1)
  ionz_number(nst)=nint(ekm(1))
  np_norm=1.
  if(ekm(1) >0.0)np_norm=1./ekm(1)
  call  bunch_corr(ebfp,ik,np_norm,bcorr)
  ionz_bavg(nst,1:16)=bcorr(1:16)
 !==========================
 end subroutine enb_ionz
 !===============================
 subroutine en_bdata(nst,it,idata)

 integer,intent(in) :: nst,it,idata
 character(7) :: bfname='       '
 character(14),dimension(16), parameter:: fb=(/&
  '     <X>      ','     <Y>      ','     <Z>      ',&
  '     <Px>     ','     <Py>     ','     <Pz>     ',&
  '   <msqX>     ','   <msqY>     ','   <msqZ>     ',&
  '  <msqPx>     ','  <msqPy>     ','  <msqPz>     ',&
  '   <Emy>      ','   <Emz>      ','   <Gam>      ','   DGam/Gam   '/)


 integer :: ib,nbvar,ik
 integer,parameter :: lun=10
 nbvar=16
  write (bfname,'(a5,i2.2)') 'bdiag',idata
  open (lun,file='diagnostics/'//bfname//'.dat',form='formatted')
  write(lun,'(6a14)')fb(1:6)
  write(lun,'(6a14)')fb(7:12)
  write(lun,'(4a14)')fb(13:16)
  write(lun,*)' nbunch,   nstep,   nbvar'
  write(lun,*) nst,nsb,nbvar,it
  write(lun,*)'time'
  do ik=1,nst
   write(lun,'(e13.4)')tb(ik)
   do ib=1,nsb
    write(lun,'(6e13.4)')bavg(ik,1:6,ib)
    write(lun,'(6e13.4)')bavg(ik,7:12,ib)
    write(lun,'(4e13.4)')bavg(ik,13:16,ib)
  end do
  end do
  close(lun)
 end subroutine en_bdata

  subroutine en_ionz_data(nst,itrz,data_id)

 integer,intent(in) :: nst,itrz,data_id
 character(12) :: fname='            '
 character(14),dimension(16), parameter:: fb=(/&
  '     <X>      ','     <Y>      ','     <Z>      ',&
  '     <Px>     ','     <Py>     ','     <Pz>     ',&
  '   <msqX>     ','   <msqY>     ','   <msqZ>     ',&
  '  <msqPx>     ','  <msqPy>     ','  <msqPz>     ',&
  '   <Emy>      ','   <Emz>      ','   <Gam>      ','   DGam/Gam   '/)


 integer :: ik,color,npv
 integer,parameter :: lun=10
 color=0
 if(Two_color)color=1
 npv=16
 write (fname,'(a10,i2.2)') 'ionz_emitt',data_id

  open (lun,file='diagnostics/'//fname//'.dat',form='formatted')
  write(lun,*)'mod_id,dmodel_id LP_ord,der_ord, ibeam,  color'
  write(lun,'(6i8)')model_id,dmodel_id,LPf_ord,der_ord,ibeam,color
  write(lun,*)'Z1_i,  A1_i,   Z2_i,   A2_i,   iform,    str'
  write(lun,'(6i6)')ion_min(1),atomic_number(1),ion_min(2),atomic_number(2),iform,str_flag
  write(lun,*)' xmax       xmin       ymax      ymin      '
  write(lun,'(4e12.4)')xmax,xmin,ymax,ymin
  if(model_id <= 4)then
   write(lun,*)' lam0       w0x       w0y        energy'
   write(lun,'(4e11.4)')lam0,w0_x,w0_y,lp_energy
   write(lun,*)' a0        lp_int     lp_pow    energy_on_targ'
   write(lun,'(4e12.4)')a0,lp_intensity,lp_pow, energy_in_targ
   write(lun,*)' targ_x1  targ_x2     n/nc       el_lp        '
   write(lun,'(4e12.4)')targ_in,targ_end,n_over_nc,el_lp
  endif
  write(lun,*)' ompe2       nmacro       np_per_cell    '
  write(lun,'(3e12.4)')ompe,nmacro,np_per_cell
  write(lun,*)'    Nx      Ny      Nz    n_cell   Nsp  Nb_las'
  write(lun,'(6i8)')nx,ny,nz,mp_per_cell(1), nsp,  nb_laser
  write(lun,*)' iter, nst, npvar'
  write(lun,'(3i6)')itrz,nst,npv
  write(lun,*)'====================================='
  write(lun,*)'time'
  write(lun,'(5e11.4)')tionz(1:nst)
  write(lun,'(5i8)')ionz_number(1:nst)
   write(lun,'(6a14)')fb(1:6)
  do ik=1,nst
   write(lun,'(6e13.4)')ionz_bavg(ik,1:6)
  end do
   write(lun,'(6a14)')fb(7:12)
  do ik=1,nst
   write(lun,'(6e13.4)')ionz_bavg(ik,7:12)
  end do
   write(lun,'(4a14)')fb(13:16)
  do ik=1,nst
   write(lun,'(4e13.4)')ionz_bavg(ik,13:16)
  end do
  close(lun)
 end subroutine en_ionz_data




 subroutine set_output_grid(jmp)

 integer,intent(in) :: jmp
 integer :: i1,j1,k1,i2,j2,k2
 integer :: ix,ix1,iy1,iz1,ngyzx(3),ngout

 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)

 ix1=0
 iz1=0
 iy1=0
 do ix=i1,i2,jmp
  ix1=ix1+1
 enddo
 do ix=k1,k2,jmp
  iz1=iz1+1
 enddo
 do ix=j1,j2,jmp
  iy1=iy1+1
 enddo
 nxh(imodx+1)=ix1
 nyh(imody+1)=iy1
 nzh(imodz+1)=iz1

 ngyzx(1)=iy1
 ngyzx(2)=iz1
 ngyzx(3)=ix1
 call intvec_row_distribute(iy1,iz1,ix1)
 if(pe0)then
  ngout=max(nx,ny)
  ngout=max(ngout,nz)
  allocate(gwdata(ngout))

  ngyzx(1)=maxval(nyh(1:npe_yloc))
  ngyzx(2)=maxval(nzh(1:npe_zloc))
  ngyzx(3)=maxval(nxh(1:npe_xloc))
  ngout=ngyzx(1)*ngyzx(2)*ngyzx(3)
 else
  ngout=ix1*iy1*iz1
 endif
 allocate(wdata(ngout))

 end subroutine set_output_grid

 !--------------------------

 end module pic_out
