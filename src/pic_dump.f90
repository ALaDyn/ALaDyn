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
 integer :: np,ic
 integer :: nxf,nyf,nzf,nf
 integer :: i2b,j2b,k2b,nbf,nd_pot
 real(dp) :: rdata(10)
 integer :: ndata(10)
 !==============
 write (fname,'(a7,i6.6)') 'dumpout',mype

 nxf=size(ebf,1)
 nyf=size(ebf,2)
 nzf=size(ebf,3)
 nf=size(ebf,4)
 i2b=nxf
 j2b=nyf
 k2b=nzf
 nbf=nf
 nd_pot=0
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
 rdata(6)=lp_in

 ndata(1)=it_loc
 ndata(2)=nxf
 ndata(3)=nyf
 ndata(4)=nzf
 ndata(5)=nf
 ndata(6)=nptx_max
 ndata(7)=npty
 ndata(8)=npt_buffer
 ndata(9)=size(x)
 !==============
 open (10,file='dumpRestart/'//fname//'.bin',form='unformatted')
 write(10)rdata
 write(10)ndata
 write(10)x(1:ndata(9))
 write(10)nptx(1:nsp)
 !-----------------------------
 if(targ_end > xmax)then
  write(10)loc_npty(1:nsp)
  write(10)loc_nptz(1:nsp)
  write(10)xpt(1:nptx_max,1:nsp)
  write(10)wghpt(1:nptx_max,1:nsp)
  write(10)loc_ypt(1:loc_npty(1),1:nsp)
  write(10)loc_wghy(1:loc_npty(1),1:nsp)
  if(ndim==3)then
   write(10)loc_zpt(1:loc_nptz(1),1:nsp)
   write(10)loc_wghz(1:loc_nptz(1),1:nsp)
  endif
 endif
 !-----------------------
 write(10)ebf(1:nxf,1:nyf,1:nzf,1:nf)
 if(Envelope)then
  write(10)env(1:nxf,1:nyf,1:nzf,1:2)
  write(10)env0(1:nxf,1:nyf,1:nzf,1:2)
 endif
 if(Beam)then
  write(10)ebf_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(ibeam==1)write(10)ebf1_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(Pbeam)write(10)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
  if(L_Bpoloidal)write(10)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
 endif
 !========================================Particle section
 if(Part)then
  write(10)loc_npart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsp)
  do ic=1,nsp
   np=loc_npart(imody,imodz,imodx,ic)
   if(np >0)then
    write(10)spec(ic)%part(1:np,1:nd2+1)
   endif
  end do
 endif
 if(Beam)then
  write(10)loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsb)
  do ic=1,nsb
   np=loc_nbpart(imody,imodz,imodx,ic)
   if(np >0)then
    write(10)bunch(ic)%part(1:np,1:nd2+1)
   endif
  end do
 endif
 close(10)
 unix_time_last_dump = unix_time_now
 end subroutine dump_data
 !==============================================================
 !==============================================================
 subroutine restart(it_loc,tloc)
 integer,intent(out) :: it_loc
 real(dp),intent(out) :: tloc
 character(13) :: fname='             '
 integer :: np,nps_loc(4),np_max,ic
 integer :: n1_old
 integer :: nxf,nyf,nzf,nf,npt_max
 integer :: i2b,j2b,k2b,nbf
 integer :: n1_loc,n2_loc,n3_loc,nf_loc
 real(dp) :: rdata(10)
 integer :: ndata(10)
 real(dp),allocatable :: xx(:)

 !==============
 write (fname,'(a7,i6.6)') 'dumpout',mype
 !==============
 open (10,file='dumpRestart/'//fname//'.bin',form='unformatted')

 read(10)rdata
 read(10)ndata
 !=============================
 tloc=rdata(1)
 j0_norm=rdata(2)
 ompe=rdata(3)
 targ_in=rdata(4)
 targ_end=rdata(5)
 lp_in=rdata(6)

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
 !=======================================
 !=======================================
 allocate(xx(n1_old))
 read(10)xx(1:n1_old) !xx is the x grid modified my the moving window
 read(10)nptx(1:nsp)
 !===================
 ! x() defined on the grid module starting from x(1)=0.0
 if(xx(1) >0.0)then
  x=x+xx(1)
  xh=xh+xx(1)
  xmin=xmin+xx(1)
  xmax=xmax+xx(1)
  loc_xgrid(imodx)%gmin=loc_xgrid(imodx)%gmin+xx(1)
  loc_xgrid(imodx)%gmax=loc_xgrid(imodx)%gmax+xx(1)
 endif
 !===================
 if(targ_end > xmax)then
  read(10)loc_npty(1:nsp)
  read(10)loc_nptz(1:nsp)
  allocate(xpt(nptx_max,nsp))
  allocate(wghpt(nptx_max,nsp))
  allocate(loc_ypt(loc_npty(1),nsp))
  allocate(loc_wghy(loc_npty(1),nsp))
  read(10)xpt(1:nptx_max,1:nsp)
  read(10)wghpt(1:nptx_max,1:nsp)
  read(10)loc_ypt(1:loc_npty(1),1:nsp)
  read(10)loc_wghy(1:loc_npty(1),1:nsp)
  if(ndim==3)then
   allocate(loc_zpt(loc_nptz(1),nsp))
   allocate(loc_wghz(loc_nptz(1),nsp))
   read(10)loc_zpt(1:loc_nptz(1),1:nsp)
   read(10)loc_wghz(1:loc_nptz(1),1:nsp)
  endif
 endif
 !=============== field dimensions
 nxf=size(ebf,1)
 nyf=size(ebf,2)
 nzf=size(ebf,3)
 nf=size(ebf,4)
 i2b=nxf
 j2b=nyf
 k2b=nzf
 nbf=nf
 if(Beam)then
  i2b=size(ebf_bunch,1)
  j2b=size(ebf_bunch,2)
  k2b=size(ebf_bunch,3)
  nbf=size(ebf_bunch,4)
 endif
 !=================
 read(10)ebf(1:nxf,1:nyf,1:nzf,1:nf)
 if(Envelope)then
  read(10)env(1:nxf,1:nyf,1:nzf,1:2)
  read(10)env0(1:nxf,1:nyf,1:nzf,1:2)
 endif
 if(Beam)then
  read(10)ebf_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(ibeam==1)read(10)ebf1_bunch(1:i2b,1:j2b,1:k2b,1:nbf)
  if(Pbeam)read(10)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
  if(L_Bpoloidal)read(10)ebf0_bunch(1:i2b,1:j2b,1:k2b,1:3)
 endif
 !=========== end field section
 if(Part)then
  read(10)loc_npart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsp)
  nps_loc(1:nsp)=loc_npart(imody,imodz,imodx,1:nsp)
  nps_loc(1)=max(nps_loc(1),npt_max)
  np_max=nps_loc(1)
  call p_alloc(np_max,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
  !=========================
  do ic=1,nsp
   np=loc_npart(imody,imodz,imodx,ic)
   if(np >0)then
    read(10)spec(ic)%part(1:np,1:nd2+1)
   endif
  end do
  if(Beam)then
   read(10)loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsb)
   nps_loc(1:nsb)=loc_nbpart(imody,imodz,imodx,1:nsb)
   np_max=maxval(nps_loc(1:nsb))
   if(np_max >0)call p_alloc(np_max,nd2+1,nps_loc,nsb,LPf_ord,1,2,mem_psize)
   !========================
   do ic=1,nsb
    np=loc_nbpart(imody,imodz,imodx,ic)
    if(np >0)then
     read(10)bunch(ic)%part(1:np,1:nd2+1)
    endif
   end do
  endif
 endif
 !====================
 close(10)
 !====================
 end subroutine restart
 !===========================
 end module pic_dump
 !===================================
