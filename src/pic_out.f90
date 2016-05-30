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

 module pic_out

 use precision_def
 use pstruct_data
 use fstruct_data
 use all_param
 use parallel

 implicit none

 real(sp),allocatable :: wdata(:),gwdata(:)

 real(dp) :: tloc(1000),tsp(1:1001),eavg(10,1001),&
  pavg(15,1001,4),favg(30,1001),bavg(50,1001)
 integer,parameter :: par_dim=20,ne=100
 real(dp) :: nde0(ne),nde1(ne),nde2(ne)
 real(dp) :: nde(ne,500,4),eksp_max(500,4),nde_sm(ne,500,4),nde_sp(ne,500,4)
 real(sp) :: real_par(par_dim)
 integer :: int_par(par_dim)

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

 !--------------------------

 subroutine den_ene_mom_out(tnow,ns_ind,cmp,cmp_loc,jump)
 real(dp),intent(in) :: tnow
 integer,intent(in) :: ns_ind,cmp,cmp_loc,jump
 character(9) :: fname='         '
 character(7),dimension(6),parameter :: el1=&
  (/'Edenout','Elenout',&
  'Elvxout','Elvyout','Elvzout','Bdenout'/)
 character(7),dimension(5),parameter :: pr1=&
  (/'Pdenout','Prenout',&
  'Prvxout','Prvyout','Prvzout'/)
 character(7),dimension(5),parameter :: io1=&
  (/'H1dnout','H1enout',&
  'H1vxout','H1vyout','H1vzout'/)
 character(7),dimension(5),parameter :: io2=&
  (/'H2dnout','H2enout',&
  'H2vxout','H2vyout','H2vzout'/)

 integer :: ix,iy,iz,iq,ipe
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: gr_dim(3),i_end
 integer :: lun,i1,j1,k1,nxp,nyp,nzp
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
 end subroutine den_ene_mom_out

 !--------------------------

 subroutine bden_ene_mom_out(tnow,cmp_loc,jump)

 real(dp),intent(in) :: tnow
 integer,intent(in) :: cmp_loc,jump
 character(9) :: fname='         '

 integer :: ix,iy,iz,iq
 integer :: lenw,kk,nx1,ny1,nz1
 integer :: i_end,i1,j1,k1,nxp,nyp,nzp
 integer :: lun,gr_dim(3)
 character(4) :: folderName
 integer,parameter :: file_version = 2

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

 if(prl)call serial_fwrite(wdata,lun)
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
 end subroutine bden_ene_mom_out

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
   if(nbfield==3)then
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

 subroutine part_pdata_out(tnow,x0,x1,ym,pid,jmp)

 character(6),dimension(4),parameter :: part=&
  (/'Elpout','H1pout','Prpout','H2pout'/)
 character(8) :: fname='        '
 character(17) :: fname_out='                 '
 character(12) :: fnamel='            '
 character(21) :: fname_outl='                     '
 real(dp),intent(in) :: tnow,x0,x1,ym
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
 if(ndim >2)then
  do p=1,np,jmp
   yy=spec(pid)%part(p,2)
   zz=spec(pid)%part(p,3)
   if(abs(yy)<=ym.and.abs(zz)<=ym)then
    xx=spec(pid)%part(p,1)
    if(xx>=x0.and.xx <=x1)then
     ip=ip+1
     do q=1,nd2+1
      ebfp(ip,q)=spec(pid)%part(p,q)
     end do
    endif
   endif
  end do
 else
  do p=1,np,jmp
   yy=spec(pid)%part(p,2)
   if(abs(yy)<=ym)then
    xx=spec(pid)%part(p,1)
    if(xx>=x0.and.xx<=x1)then
     ip=ip+1
     do q=1,nd2+1
      ebfp(ip,q)=spec(pid)%part(p,q)
     end do
    endif
   endif
  end do
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

 int_par=0
 call endian(i_end)
 real_par=0.0

 real_par(1:20) =(/real(tnow,sp),real(xmin,sp),real(xmax,sp),real(ymin,sp),&
  real(ymax,sp),real(zmin,sp),real(zmax,sp),real(w0_x,sp),real(w0_y,sp),&
  real(n_over_nc,sp),real(a0,sp),real(lam0,sp),real(E0,sp),real(ompe,sp),&
  real(np_per_cell,sp),real(targ_in,sp),real(targ_end,sp),real(unit_charge(pid),sp),&
  real(mass(pid),sp),0.0_sp/)

 int_par(1:20) = (/npe,nx,ny_loc,nz_loc,jmp,iby,iform,&
  model_id,dmodel_id,nsp,curr_ndim,mp_per_cell(1),&
  LPf_ord,der_ord,iform,pid,nptot,ndv,file_version,i_end/)

 write(fname,'(a6,i2.2)')part(pid),iout      !serve sempre
 write(fnamel,'(a6,i2.2,a1,i3.3)')part(pid),iout,'_',imodz !usare con mpi_write_part_col
 fname_out=foldername//'/'//fname//'.bin'
 fname_outl=foldername//'/'//fnamel//'.bin'
 disp=0
 disp_col=0
 if(pe0)then
  open(10,file=foldername//'/'//fname//'.dat',form='formatted')
  write(10,*)' Integer parameters'
  write(10,'(4i14)')int_par
  write(10,*)' Real parameters'
  write(10,'(4e14.5)')real_par
  write(10,*)' Number of particles'
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
 subroutine envelope_struct_data(nst)

 integer,intent(in) :: nst
 integer :: i1,j1,k1,i2,nyp,nzp,jj,kk
 integer :: k,ik,ix,iy,iz,i01,i02,i0_lp
 real(dp) :: ekt(7),ekm(7),ef(6)
 real(dp) :: dvol
 real(dp) :: dgvol,sgz,sg
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
 i0_lp=3+nint(dx_inv*(lp_in-xmin))

 if(Envelope)then
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
 else
  ! !data on driver laser field
  kk=0
  !===================
  kk=0
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i0_lp-1,i2
     ef(1:curr_ndim)=ebf(ix,iy,iz,1:curr_ndim)
     a2=dot_product(ef(1:curr_ndim),ef(1:curr_ndim))
     dar=ef(2)*ef(2)               !E_y^2
     ekt(1)=ekt(1)+ a2
     ekt(2)=ekt(2)+x(ix-2)*dar         ! Centroid of E_y^2
     ekt(5)=max(ekt(5),sqrt(a2))    ! Max |E|
     kk=kk+1
    end do
   end do
  end do
  dvol=1./real(kk,dp)
  call allreduce_dpreal(SUMV,ekt,ekm,4)
  eavg(2,nst)=field_energy*dvol*ekm(1)/real(npe,dp)    !Energy
  if(ekm(1)> 0.0)eavg(3,nst)=ekm(2)/ekm(1)  !x-Centroid
  ekt(1)=ekt(5)
  ekm(1)=ekt(1)
  if(prl)call allreduce_dpreal(MAXV,ekt,ekm,1)
  eavg(1,nst)=ekm(1)           !Max |E|
  !=================
  kk=0
  ekt=0.0
  if(ndim <3)then
   iz=1
   do iy=j1,nyp
    jj=iy-2
    sg=loc_yg(jj,2,imody)
    do ix=i0_lp,i2-1
     ef(2)=ebf(ix,iy,iz,2)
     a2=ef(2)*ef(2)               !E_y^2
     ekt(1)=ekt(1)+sg*a2
     ekt(2)=ekt(2)+sg*sg*a2
     ekt(3)=ekt(3)+a2
     kk=kk+1
    end do
   end do
   if(kk >0)dvol=1./real(kk,dp)
   call allreduce_dpreal(SUMV,ekt,ekm,3)
   if(ekm(3) >0.0)ekm(1:2)=ekm(1:2)/ekm(3)
   eavg(4,nst)=ekm(2)-ekm(1)*ekm(1)
  else
   do iz=k1,nzp
    k=iz-2
    sgz=loc_zg(k,1,imodz)
    do iy=j1,nyp
     jj=iy-2
     sg=loc_yg(jj,2,imody)
     do ix=i0_lp,i2-1
      ef(2)=ebf(ix,iy,iz,2)
      a2=ef(2)*ef(2)               !E_y^2
      ekt(1)=ekt(1)+sg*a2
      ekt(2)=ekt(2)+sgz*a2
      ekt(3)=ekt(3)+sg*sg*a2
      ekt(4)=ekt(4)+sgz*sgz*a2
      ekt(5)=ekt(5)+a2
      kk=kk+1
     end do
    end do
   end do
   if(kk >0)dvol=1./real(kk,dp)
   call allreduce_dpreal(SUMV,ekt,ekm,5)
   if(ekm(5) >0.0)ekm(1:4)=ekm(1:4)/ekm(5)
   eavg(4,nst)=ekm(3)-ekm(1)*ekm(1)
   eavg(5,nst)=ekm(4)-ekm(2)*ekm(2)
  endif
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
 favg(4:6,nst)=ekm(1:3)
 favg(10:12,nst)=ekm(4:6)
 lp_max=0.0
 if(ekm(7)>0.0)lp_max=sqrt(ekm(7))
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
 if(Solid_target)then
  call fields_on_target(nst)
 else
  if (model_id < 5) call envelope_struct_data(nst)
 endif

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

 integer :: ik,ic,nfv,npv,t_ord,nt,ne
 integer,parameter :: lun=10
 nfv=6
 if(curr_ndim==3)nfv=12
 npv=9
 t_ord=max(RK_ord,LPf_ord)

 ! if (iout<100) write (fname,'(a4,i2)') 'diag' ,idata
 ! if (iout< 10) write (fname,'(a5,i1)') 'diag0',idata
 write (fname,'(a4,i2.2)') 'diag',idata

 open (lun,file='diagnostics/'//fname//'.dat',form='formatted')
 write(lun,*)'mod_id,dmodel_id LP_ord,der_ord'
 write(lun,'(4i8)')model_id,dmodel_id,LPf_ord,der_ord
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
  if(dmodel_id > 4)then
   write(lun,*)'  lx2        lx3        lx4         dw         lw '
   write(lun,'(5e12.4)')lpx(2:4),lpy(1:2)
  else
   write(lun,*)' lx1        lx2        lx3        lx4        lx5 '
   write(lun,'(5e12.4)')lpx(1:5)
  endif
  write(lun,*)' ompe2       nmacro       np_per_cell    '
  write(lun,'(3e12.4)')ompe,nmacro,np_per_cell
  write(lun,*)'    Nx      Ny      Nz    n_cell   Nsp  Nsb'
  write(lun,'(6i8)')nx,ny,nz,mp_per_cell(1),nsp,nsb
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
   write(lun,'(4e12.3)')Qbch(ik),bcharge(ik),sxb(ik),syb(ik)
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
  write(lun,'(6a14)')feb(1:6)
  do ik=1,nst
   write(lun,'(6e13.5)')favg(13:18,ik)
  end do
  write(lun,'(6a14)')feb(7:12)
  do ik=1,nst
   write(lun,'(6e13.5)')favg(19:24,ik)
  end do
 endif
 if(Envelope)then
  write(lun,*)'====  the envelope integrated variables'
  write(lun,'(4a14)')fenv(1:4)
  do ik=1,nst
   write(lun,'(4e13.5)')eavg(1:4,ik)
  end do
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
   write(lun,*)' lam0       w0x       w0y        chann_rad'
   write(lun,'(4e12.4)')lam0,w0_x,w0_y,chann_rad
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
 subroutine beam_selection(nst,tnow)

 integer,intent(in) :: nst
 real(dp),intent(in) :: tnow

 integer :: ik,ic,kk,np
 real(dp) :: np_norm,gmb,pp(3),mu(7,5),ekt(9),ekm(9)
 real(dp) :: corr2(8,5),emy(5),emz(5),dgam(5)
 !=====================
 mu=0.0
 corr2=0.0
 do ic=1,nsb
  np=loc_nbpart(imody,imodz,imodx,ic)
  ekt(1)=real(np,dp)
  call allreduce_dpreal(SUMV,ekt,ekm,1)
  np_norm=1.
  if(ekm(1) >0.0)np_norm=1./ekm(1)
  ekm=0.0
  ekt=0.0
  !USES real to sum big integers
  if(np>0)then
   do ik=1,6
    ekt(ik)=sum(bunch(ic)%part(1:np,ik))
   enddo
   do kk=1,np
    pp(1:3)=bunch(ic)%part(kk,4:6)
    gmb=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3))
    ekt(7)=ekt(7)+gmb
   end do
  endif
  call allreduce_dpreal(SUMV,ekt,ekm,7)
  mu(1:7,ic)=np_norm*ekm(1:7) !Averages <(x,y,z,Px,Py,Pz,gamma)>
  !=========== 2th moments
  ekm=0.0
  ekt=0.0
  if(np>0)then
   do ik=1,6
    do kk=1,np
     ekt(ik)=ekt(ik)+(bunch(ic)%part(kk,ik)-mu(ik,ic))**2
    end do
   enddo
   !================mixed corr
   do kk=1,np
    ekt(7)=ekt(7)+bunch(ic)%part(kk,5)*bunch(ic)%part(kk,2)
    ekt(8)=ekt(8)+bunch(ic)%part(kk,6)*bunch(ic)%part(kk,3)
    pp(1:3)=bunch(ic)%part(kk,4:6)
    gmb=1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)
    ekt(9)=ekt(9)+gmb       !<(gam**2>
   end do
  endif
  call allreduce_dpreal(SUMV,ekt,ekm,9)
  corr2(1:8,ic)=np_norm*ekm(1:8)
  ekm(9)=np_norm*ekm(9)
  !    emy^2= corr2_y*corr2_py -mixed
  !<yy><p_yp_y>-(<yp_y>-<y><p_y>)^2
  emy(ic)=corr2(2,ic)*corr2(5,ic)-(corr2(7,ic)-mu(2,ic)*mu(5,ic))**2
  emz(ic)=corr2(3,ic)*corr2(6,ic)-(corr2(8,ic)-mu(3,ic)*mu(6,ic))**2
  if(emy(ic)>0.0)emy(ic)=sqrt(emy(ic))
  if(emz(ic)>0.0)emz(ic)=sqrt(emz(ic))


  gmb=mu(7,ic)*mu(7,ic)
  dgam(ic)=ekm(9)/gmb -1.0
  if(dgam(ic) >0.0)dgam(ic)=sqrt(dgam(ic))   !Dgamm/gamma
 end do
 if(pe0)call enbdata(nst,nsb,mu,corr2,emy,emz,dgam,tnow)
 !==========================
 end subroutine beam_selection
!=====================================
 subroutine enbvar(nst,tnow)

 integer,intent(in) :: nst
 real(dp),intent(in) :: tnow

 integer :: ik,ic,kk,np
 real(dp) :: np_norm,gmb,pp(3),mu(7,5),ekt(9),ekm(9)
 real(dp) :: corr2(8,5),emy(5),emz(5),dgam(5)
 !=====================
 mu=0.0
 corr2=0.0
 do ic=1,nsb
  np=loc_nbpart(imody,imodz,imodx,ic)
  ekt(1)=real(np,dp)
  call allreduce_dpreal(SUMV,ekt,ekm,1)
  np_norm=1.
  if(ekm(1) >0.0)np_norm=1./ekm(1)
  ekm=0.0
  ekt=0.0
  !USES real to sum big integers
  if(np>0)then
   do ik=1,6
    ekt(ik)=sum(bunch(ic)%part(1:np,ik))
   enddo
   do kk=1,np
    pp(1:3)=bunch(ic)%part(kk,4:6)
    gmb=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3))
    ekt(7)=ekt(7)+gmb
   end do
  endif
  call allreduce_dpreal(SUMV,ekt,ekm,7)
  mu(1:7,ic)=np_norm*ekm(1:7) !Averages <(x,y,z,Px,Py,Pz,gamma)>
  !=========== 2th moments
  ekm=0.0
  ekt=0.0
  if(np>0)then
   do ik=1,6
    do kk=1,np
     ekt(ik)=ekt(ik)+(bunch(ic)%part(kk,ik)-mu(ik,ic))**2
    end do
   enddo
   !================mixed corr
   do kk=1,np
    ekt(7)=ekt(7)+bunch(ic)%part(kk,5)*bunch(ic)%part(kk,2)
    ekt(8)=ekt(8)+bunch(ic)%part(kk,6)*bunch(ic)%part(kk,3)
    pp(1:3)=bunch(ic)%part(kk,4:6)
    gmb=1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)
    ekt(9)=ekt(9)+gmb       !<(gam**2>
   end do
  endif
  call allreduce_dpreal(SUMV,ekt,ekm,9)
  corr2(1:8,ic)=np_norm*ekm(1:8)
  ekm(9)=np_norm*ekm(9)
  !    emy^2= corr2_y*corr2_py -mixed
  !<yy><p_yp_y>-(<yp_y>-<y><p_y>)^2
  !    emz^2= corr2_z*corr2_pz -mixed
  !<zz><p_zp_z>-(<zp_z>-<z><p_z>)^2
  emy(ic)=corr2(2,ic)*corr2(5,ic)-(corr2(7,ic)-mu(2,ic)*mu(5,ic))**2
  emz(ic)=corr2(3,ic)*corr2(6,ic)-(corr2(8,ic)-mu(3,ic)*mu(6,ic))**2
  if(emy(ic)>0.0)emy(ic)=sqrt(emy(ic))
  if(emz(ic)>0.0)emz(ic)=sqrt(emz(ic))


  gmb=mu(7,ic)*mu(7,ic)
  dgam(ic)=ekm(9)/gmb -1.0
  if(dgam(ic) >0.0)dgam(ic)=sqrt(dgam(ic))   !Dgamm/gamma
 end do
 if(pe0)call enbdata(nst,nsb,mu,corr2,emy,emz,dgam,tnow)
 !==========================
 end subroutine enbvar
 !========================
 subroutine enbdata(nst,nb,avg,corr2,emy,emz,dgm,t_loc)

 integer,intent(in) :: nst,nb
 real(dp),intent(in) :: avg(:,:),corr2(:,:),emy(:),emz(:),dgm(:)
 real(dp),intent(in) :: t_loc
 character(5) :: bfname='     '
 character(14),dimension(16), parameter:: fb=(/&
  '     <X>      ','     <Y>      ','     <Z>      ',&
  '     <Px>     ','     <Py>     ','     <Pz>     ',&
  '   <rmsX>     ','   <rmsY>     ','   <rmsZ>     ',&
  '  <rmsPx>     ','  <rmsPy>     ','  <rmsPz>     ',&
  '   <Emy>      ','   <Emz>      ','   <Gam>      ','   DGam/Gam   '/)


 integer :: ib,nbvar
 integer,parameter :: lun=10
 nbvar=16
 if(nst==1)then
  write (bfname,'(a5)') 'bdiag'
  open (lun,file='diagnostics/'//bfname//'.dat',form='formatted')
  write(lun,'(6a14)')fb(1:6)
  write(lun,'(6a14)')fb(7:12)
  write(lun,'(4a14)')fb(13:16)
  write(lun,*)' nbunch      nbvar'
  write(lun,*) nb, nbvar
  write(lun,*)'time'
  write(lun,'(e13.4)')t_loc
  do ib=1,nb
   write(lun,'(6e13.4)')avg(1:6,ib)
   write(lun,'(6e13.4)')sqrt(corr2(1:6,ib))
   write(lun,'(4e13.4)')emy(ib),emz(ib),avg(7,ib),dgm(ib)
  end do
  close(lun)
 else
  open (lun,file='diagnostics/'//bfname//'.dat',form='formatted',position='append')
  write(lun,*)'time'
  write(lun,'(e13.4)')t_loc
  do ib=1,nb
   write(lun,'(6e13.4)')avg(1:6,ib)
   write(lun,'(6e13.4)')sqrt(corr2(1:6,ib))
   write(lun,'(4e13.4)')emy(ib),emz(ib),avg(7,ib),dgm(ib)
  end do
  close(lun)
 endif
 end subroutine enbdata



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
 if(Cyl_coord)then
  j1=loc_rgrid(imody)%p_ind(1)
  j2=loc_rgrid(imody)%p_ind(2)
 endif

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
