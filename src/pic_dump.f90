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

 module pic_dump

 use precision_def
 use pstruct_data
 use fstruct_data
 use all_param
 use parallel

 implicit none
 contains

 subroutine dump_data(it_loc,tloc)
 integer,intent(in) :: it_loc
 real(dp),intent(in) :: tloc
 character(13) :: fname='             '
 integer :: np,ic,lun,i,j
 integer :: nxf_loc,nyf_loc,nzf_loc,nf
 integer :: nxfl,i2b,j2b,k2b,nbf,env_cp
 real(dp) :: rdata(10)
 integer :: ndata(10),nps_loc(4),nbs_loc(5)
 !==============
 write (fname,'(a7,i6.6)') 'dumpout',mype

 nxf_loc=size(ebf,1)
 nyf_loc=size(ebf,2)
 nzf_loc=size(ebf,3)
 nf=size(ebf,4)
 if(Beam)then
  i2b=size(ebf_bunch,1)
  j2b=size(ebf_bunch,2)
  k2b=size(ebf_bunch,3)
  nbf=size(ebf_bunch,4)
 endif
 !=========================
 ndata=0
 rdata=0.0
 rdata(1)=tloc
 rdata(2)=j0_norm
 rdata(3)=ompe
 rdata(4)=targ_in
 rdata(5)=targ_end
 rdata(6)=lp_in(1)
 rdata(7)=xp0_out
 rdata(8)=xp1_out

 ndata(1)=it_loc
 ndata(2)=nxf_loc
 ndata(3)=nyf_loc
 ndata(4)=nzf_loc
 ndata(5)=nf
 ndata(6)=nptx_max
 ndata(7)=npty
 ndata(8)=npt_buffer(1)
 ndata(9)=size(x)
 ndata(10)=nxfl
 !==============
 lun=10
 open (lun,file='dumpRestart/'//fname//'.bin',form='unformatted',status='unknown')
 write(lun)rdata(1:10)
 write(lun)ndata(1:10)
 write(lun)nptx(1:nsp)
 write(lun)sptx_max(1:nsp)
 i=ndata(9)
 write(lun)x(1:i)
 !-----------------------------
 if(targ_end > xmax)then
  if(Hybrid)then
   write(lun)fluid_x_profile(1:nxfl)
  endif
  do i=1,nsp
   write(lun)loc_npty(i),loc_nptz(i)
  end do
  do i=1,nsp
   do j=1,nptx_max
    write(lun)xpt(j,i),wghpt(j,i)
   end do
   do j=1,loc_npty(1)
    write(lun)loc_ypt(j,i),loc_wghy(j,i)
   end do
  end do
  if(ndim==3)then
   do i=1,nsp
    do j=1,loc_nptz(1)
     write(lun)loc_zpt(j,i),loc_wghz(j,i)
    end do
   end do
  endif
 endif
 !-----------------------
 !========================
 write(lun)ebf(:,:,:,:)
 if(Envelope)then
  env_cp=size(env,4)
  write(lun)env(:,:,:,:)
  if(Two_color)write(lun)env1(:,:,:,:)
 endif
 if(Hybrid)then
  write(lun)up(:,:,:,:)
  write(lun)up0(:,:,:,:)
 endif
  
 if(Beam)then
  write(lun)ebf_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(ibeam==1)write(lun)ebf1_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(Pbeam)write(lun)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
  if(L_Bpoloidal)write(lun)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
 endif
 !========================================Particle section
 if(Part)then
  do i=1,nsp
   nps_loc(i)=size(spec(i)%part,1)
  end do
  write(lun)nps_loc(1:nsp)
  write(lun)loc_npart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsp)
  do ic=1,nsp
   np=loc_npart(imody,imodz,imodx,ic)
   if(np >0)then
    ebfp(1:np,1:nd2+1)=spec(ic)%part(1:np,1:nd2+1)
    write(lun)ebfp(1:np,1:nd2+1)
   endif
  end do
 endif
 if(Beam)then
  do i=1,nsb
   nbs_loc(i)=size(bunch(i)%part,1)
  end do
  write(lun)nbs_loc(1:nsb)
  write(lun)loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsb)
  do ic=1,nsb
   np=loc_nbpart(imody,imodz,imodx,ic)
   if(np >0)then
    ebfb(1:np,1:nd2+1)=bunch(ic)%part(1:np,1:nd2+1)
    write(lun)ebfb(1:np,1:nd2+1)
   endif
  end do
 endif
 close(lun)
 unix_time_last_dump = unix_time_now
 end subroutine dump_data
 !==============================================================
 !==============================================================
 subroutine restart(it_loc,tloc)
 integer,intent(out) :: it_loc
 real(dp),intent(out) :: tloc
 character(13) :: fname='             '
 integer :: np,nps_loc(4),nbs_loc(5),np_max
 integer :: n1_old,lun,i,j,ic
 integer :: nxf_loc,nyf_loc,nzf_loc,nf,npt_max
 integer :: i2b,j2b,k2b,nbf,env_cp,nxfl
 integer :: n1_loc,n2_loc,n3_loc,nf_loc
 real(dp) :: rdata(10)
 integer :: ndata(10)
 real(dp),allocatable :: xx(:)

 !==============
 write (fname,'(a7,i6.6)') 'dumpout',mype
 !==============
 lun=10
 open (lun,file='dumpRestart/'//fname//'.bin',form='unformatted',status='unknown')

 read(lun)rdata(1:10)
 read(lun)ndata(1:10)
 read(lun)nptx(1:nsp)
 read(lun)sptx_max(1:nsp)
 !=============================
 tloc=rdata(1)
 j0_norm=rdata(2)
 ompe=rdata(3)
 targ_in=rdata(4)
 targ_end=rdata(5)
 lp_in(1)=rdata(6)

 it_loc=ndata(1)
 n1_loc=ndata(2)
 n2_loc=ndata(3)
 n3_loc=ndata(4)
 nf_loc=ndata(5)
 nptx_max=ndata(6)
 npty=ndata(7)
 nptz=npty
 npt_max=ndata(8)
 n1_old=ndata(9)
 nxfl=ndata(10)
 !=======================================
 !=======================================
 allocate(xx(n1_old))
 i=ndata(9)
 read(lun)xx(1:i)
 !===================
 ! x() defined on the grid module starting from x(1)=0.0
 if(xx(1) >0.0)then
  x=x+xx(1)
  xh=xh+xx(1)
  xmin=xmin+xx(1)
  xmax=xmax+xx(1)
  loc_xgrid(imodx)%gmin=loc_xgrid(imodx)%gmin+xx(1)
  loc_xgrid(imodx)%gmax=loc_xgrid(imodx)%gmax+xx(1)
  xp0_out=xp0_out+xx(1)
  xp1_out=xp1_out+xx(1)
 endif
 !===================
 if(targ_end > xmax)then
  if(nxfl>0)then
   read(lun)fluid_x_profile(1:nxfl)
  endif
  do i=1,nsp
   read(lun)loc_npty(i),loc_nptz(i)
  end do
  allocate(xpt(nptx_max,nsp))
  allocate(wghpt(nptx_max,nsp))
  allocate(loc_ypt(loc_npty(1),nsp))
  allocate(loc_wghy(loc_npty(1),nsp))
  do i=1,nsp
   do j=1,nptx_max
    read(lun)xpt(j,i),wghpt(j,i)
   end do
   do j=1,loc_npty(1)
    read(lun)loc_ypt(j,i),loc_wghy(j,i)
   end do
  end do
  if(ndim==3)then
   allocate(loc_zpt(loc_nptz(1),nsp))
   allocate(loc_wghz(loc_nptz(1),nsp))
   do i=1,nsp
    do j=1,loc_nptz(1)
     read(lun)loc_zpt(j,i),loc_wghz(j,i)
    end do
   end do
  endif
 endif
 !=========================
 !=============== field dimensions
 nxf_loc=size(ebf,1)
 nyf_loc=size(ebf,2)
 nzf_loc=size(ebf,3)
 nf=size(ebf,4)
 if(Beam)then
  i2b=size(ebf_bunch,1)
  j2b=size(ebf_bunch,2)
  k2b=size(ebf_bunch,3)
  nbf=size(ebf_bunch,4)
 endif
 !=================
 read(lun)ebf(:,:,:,:)
 if(Envelope)then
  env_cp=size(env,4)
  read(lun)env(:,:,:,:)
  if(Two_color)read(lun)env1(:,:,:,:)
 endif
 if(Hybrid)then
  read(lun)up(:,:,:,:)
  read(lun)up0(:,:,:,:)
 endif
 if(Beam)then
  read(lun)ebf_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(ibeam==1)read(lun)ebf1_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(Pbeam)read(lun)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
  if(L_Bpoloidal)read(lun)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
 endif
 !=========== end field section
 if(Part)then
  read(lun)nps_loc(1:nsp)
  read(lun)loc_npart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsp)
  np_max=nps_loc(1)
  call p_alloc(np_max,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
  !=========================
  do ic=1,nsp
   np=loc_npart(imody,imodz,imodx,ic)
   if(np >0)then
    read(lun)ebfp(1:np,1:nd2+1)
    spec(ic)%part(1:np,1:nd2+1)=ebfp(1:np,1:nd2+1)
   endif
  end do
  if(Beam)then
   read(lun)nbs_loc(1:nsb)
   read(lun)loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsb)
   np_max=maxval(nbs_loc(1:nsb))
   if(np_max >0)call p_alloc(np_max,nd2+1,nbs_loc,nsb,LPf_ord,1,2,mem_psize)
   !========================
   do ic=1,nsb
    np=loc_nbpart(imody,imodz,imodx,ic)
    if(np >0)then
     read(lun)ebfb(1:np,1:nd2+1)
     bunch(ic)%part(1:np,1:nd2+1)=ebfb(1:np,1:nd2+1)
    endif
   end do
  endif
 endif
 !====================
 close(lun)
 !====================
 end subroutine restart
 !===========================
 end module pic_dump
 !===================================
