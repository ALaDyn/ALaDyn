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

 module pic_in
 use precision_def
 use particles
 use pic_rutil
 use fft_lib
 use grid_fields
 use control_bunch_input

 implicit none

 real(dp),allocatable :: wa(:,:,:,:),wb(:,:,:),xpb(:),ypb(:),zpb(:)
 real(dp),allocatable :: bpart(:,:)
 integer,allocatable :: loc_imax(:,:),loc_jmax(:,:),loc_kmax(:,:)

 !--------------------------

 contains

 !--------------------------
 !--------------------------
 subroutine set_pgrid_xind(npx,ic)
 integer,intent(in) :: npx,ic
 integer :: i,p,ip
 real(dp) :: xp,x1,x2

 !Defines the the number of particles on each mpi x-domain
 ! Enter the total particle number npx and the particle species ic
 ! Particle x-distribution is xpt(nptx,ic)
 ! Exit loc_imax(pex,ic) for each pex task
 !=================================================
 loc_imax(0:npe_xloc-1,ic)=1
 p=0
 ip=0
 x1=loc_xgrid(0)%gmin
 x2=loc_xgrid(0)%gmax
 do i=1,npx
  xp=xpt(i,ic)
  if(xp>=x1.and.xp<x2)ip=ip+1
 end do
 loc_imax(p,ic)=ip  !number of grid points in [loc_xmin,loc_xmax]
 if(npe_xloc >1)then
  do p=1,npe_xloc-1
   x1=loc_xgrid(p)%gmin
   x2=loc_xgrid(p)%gmax
   ip=0
   do i=1,npx
    xp=xpt(i,ic)
    if(xp>=x1.and.xp<x2)ip=ip+1
   end do
   loc_imax(p,ic)=ip
  end do
 endif
 end subroutine set_pgrid_xind

 subroutine set_pgrid_ind(npy,npz,ic)
 integer,intent(in) :: npy,npz,ic
 integer :: i,p,ip
 real(dp) :: yp,y1,y2

 ! Particles number index on each y-z mpi domain
 ! Enter the total particle number npy, npz and the particle species ic
 ! Particle y-z-distributions are ypt(npy,ic), zpt(npz,ic)
 ! Exit loc_jmax(pey,ic) loc_kmax(pez,ic)for each pey peztask
 !=======================================
 loc_jmax(0:npe_yloc-1,ic)=1
 loc_kmax(0:npe_zloc-1,ic)=1

 p=0
 ip=0
 do i=1,npy
  yp=ypt(i,ic)
  y1=ymin_t
  y2=loc_ygrid(p)%gmax
  if(yp>=y1.and.yp<y2)ip=ip+1
 end do
 loc_jmax(p,ic)=ip  !number of particle y-positions in [loc_ymin,loc_ymax]
 if(npe_yloc >2)then
  do p=1,npe_yloc-2
   ip=0
   do i=1,npy
    yp=ypt(i,ic)
    y1=loc_ygrid(p)%gmin
    y2=loc_ygrid(p)%gmax
    if(yp>=y1.and.yp<y2)ip=ip+1
   end do
   loc_jmax(p,ic)=ip
  end do
 endif
 p=npe_yloc-1
 ip=0
 do i=1,npy
  yp=ypt(i,ic)
  y1=loc_ygrid(p)%gmin
  y2=ymax_t
  if(yp>=y1.and.yp<y2)ip=ip+1
 end do
 loc_jmax(p,ic)=ip
 if(npz==1)return

 p=0
 ip=0
 do i=1,npz
  yp=zpt(i,ic)
  y1=zmin_t
  y2=loc_zgrid(p)%gmax
  if(yp>=y1.and.yp<y2)ip=ip+1
 end do
 loc_kmax(p,ic)=ip
 if(npe_zloc >2)then
  do p=1,npe_zloc-2
   ip=0
   do i=1,npz
    yp=zpt(i,ic)
    y1=loc_zgrid(p)%gmin
    y2=loc_zgrid(p)%gmax
    if(yp>=y1.and.yp<y2)ip=ip+1
   end do
   loc_kmax(p,ic)=ip
  end do
 endif
 p=npe_zloc-1
 ip=0
 do i=1,npz
  yp=zpt(i,ic)
  y1=loc_zgrid(p)%gmin
  y2=zmax_t
  if(yp>=y1.and.yp<y2)ip=ip+1
 end do
 loc_kmax(p,ic)=ip
 end subroutine set_pgrid_ind

 !--------------------------

 subroutine pspecies_distribute(loc_sp,t_x,ch,q,ic,i2,p)
 type(species),intent(inout) :: loc_sp
 real(dp),intent(in) :: t_x,ch
 integer,intent(in) :: q,ic,i2
 integer,intent(out) :: p
 integer :: i,j,k,j2,k2
 real(dp) :: u,whp,whz
 real(sp) :: charge(2)
 equivalence(charge,whp)

 call init_random_seed(mype)
 p=q
 charge(2)=real(ch,sp)
 k2=loc_nptz(ic)
 j2=loc_npty(ic)
 if(curr_ndim >2 )then
  do k=1,k2
   do j=1,j2
    do i=1,i2
     whz=loc_wghx(i,ic)*loc_wghz(k,ic)*loc_wghy(j,ic)
     charge(1)=real(whz,sp)
     p=p+1
     loc_sp%part(p,1)=loc_xpt(i,ic)
     loc_sp%part(p,2)=loc_ypt(j,ic)
     loc_sp%part(p,3)=loc_zpt(k,ic)
     call gasdev(u)
     loc_sp%part(p,4)=t_x*u
     call gasdev(u)
     loc_sp%part(p,5)=t_x*u
     call gasdev(u)
     loc_sp%part(p,6)=t_x*u
     loc_sp%part(p,7)=whp
    end do
   end do
  end do
  return
 endif

 do j=1,j2
  do i=1,i2
   whz=loc_wghx(i,ic)*loc_wghy(j,ic)
   charge(1)=real(whz,sp)
   p=p+1
   loc_sp%part(p,1)=loc_xpt(i,ic)
   loc_sp%part(p,2)=loc_ypt(j,ic)
   call gasdev(u)
   loc_sp%part(p,3)=t_x*u
   call gasdev(u)
   loc_sp%part(p,4)=t_x*u
   loc_sp%part(p,5)=whp
  end do
 end do
 end subroutine pspecies_distribute
 !==============================
 subroutine mpi_yz_part_distrib(nc,ky2,kz2,nyc,nzc,ymt,zmt,why,whz)

 integer,intent(in) :: nc,ky2(:),kz2(:),nyc(:),nzc(:)
 real(dp),intent(in) :: ymt,zmt,why(:,:),whz(:,:)
 integer :: ic,i2,k1
 real(dp) :: loc_ym

 if(ndim < 2)return
 loc_ym=loc_ygrid(imody)%gmin
 if(imody==0)loc_ym=ymt
 do ic=1,nc
  k1=0
  do i2=1,nyc(ic)
   if(ypt(i2,ic)< loc_ym)k1=i2
  end do
  do i2=1,ky2(ic)
   k1=k1+1
   loc_ypt(i2,ic)=ypt(k1,ic)
   loc_wghy(i2,ic)=why(k1,ic)
  end do
 end do
 if(ndim<3)return
 !==========================
 loc_ym=loc_zgrid(imodz)%gmin
 if(imodz==0)loc_ym=zmt
 !==========================
 do ic=1,nc
  k1=0
  do i2=1,nzc(ic)
   if(zpt(i2,ic)< loc_ym)k1=i2
  end do
  do i2=1,kz2(ic)
   k1=k1+1
   loc_zpt(i2,ic)=zpt(k1,ic)
   loc_wghz(i2,ic)=whz(k1,ic)
  end do
 end do
 end subroutine mpi_yz_part_distrib
 !--------------------------
 !============================

 subroutine multi_layer_gas_target(layer_mod,nyh,xf0)

 integer,intent(in) :: layer_mod,nyh
 real(dp),intent(in) :: xf0
 integer :: p
 integer :: i,j,i1,i2,ic
 integer :: n_peak,nyl1,nzl1
 integer :: npyc(4),npzc(4),npty_ne,nptz_ne
 integer :: npmax
 real(dp) :: uu,yy,zz,dxip,dpy,dpz,np1,np2
 real(dp) :: zp_min,zp_max,yp_min,yp_max,xp_min,xp_max
 real(dp) :: xfsh,un(4),j1_norm,j2_norm
 integer :: nxl(5)
 real(dp),allocatable :: wy(:,:),wz(:,:)
 integer :: loc_nptx(4),nps_loc(4),np_per_zcell(4),last_particle_index(4)
 !==========================
 p=0
 i=0
 j=0
 i1=0
 i2=0
 ic=0
 ! the following variables are used only in Stretched mode, initialized anyway to zero
 yy=0.0
 dxip=0.0
 !==========================
 xp_min=xmin
 xp_max=xmax
 np_per_zcell(1:nsp)=1
 !==========================
 nxl=0
 np1=n1_over_n
 np2=n2_over_n
 if(n_over_nc > 0.0)then
  np1=np1/n_over_nc
  np2=np2/n_over_nc
 endif
 nyl1=1+ny/2-nyh/2  !=1 if nyh=ny
 nzl1=1+nz/2-nyh/2
 !============================
 ! Parameters for particle distribution along the x-coordinate
 !============================
 ! Layers nxl(1:5) all contain the same species
 xtot=0.0
 do i=1,5
  nxl(i)=nint(dx_inv*lpx(i))
  lpx(i)=nxl(i)*dx
  xtot=xtot+lpx(i)
 end do
 xfsh=xf0+lpx(7)
 targ_in=xfsh
 targ_end=targ_in+xtot
 !=============================
 loc_nptx=0
 loc_nptx(1:nsp)=(nxl(1)+nxl(2)+nxl(3)+nxl(4)+nxl(5))*np_per_xc(1:nsp)

 nptx_max=maxval(loc_nptx(1:nsp))
 allocate(xpt(nptx_max,nsp))
 allocate(wghpt(nptx_max,nsp))
 allocate(loc_xpt(nptx_max,nsp))
 !=============================
 ! mulltispecies y-z distribution in a uniform target
 yp_min=ymin_t
 yp_max=ymax_t
 zp_min=zmin_t
 zp_max=zmax_t
 !----------------------
 npty=maxval(np_per_yc(1:nsp))
 npty=nyh*npty
 nptz=1
 if(ndim==3)then
  np_per_zcell(1:nsp)=np_per_zc(1:nsp)
  nptz=maxval(np_per_zc(1:nsp))
  nptz=nyh*nptz
 endif
 allocate(ypt(npty,nsp))
 allocate(zpt(nptz,nsp))
 allocate(wy(npty,nsp))
 allocate(wz(nptz,nsp))
 ypt=0.
 zpt=0.
 wy=1.
 wz=1.
 !==================
 allocate(loc_jmax(0:npe_yloc-1,nsp))
 allocate(loc_kmax(0:npe_zloc-1,nsp))
 allocate(loc_imax(0:npe_xloc-1,nsp))
 !==================
 !========================
 wghpt=1.0
 !===================================
 ! Distributes particles on y-z coordinates
 do ic=1,nsp
  npyc(ic)=nyh*np_per_yc(ic)
  npty_ne=npyc(ic)
  npzc(ic)=1
  if(ndim > 1 )then
   dpy=(yp_max-yp_min)/real(npyc(ic),dp)
   do i=1,npyc(ic)
    ypt(i,ic)=yp_min+dpy*(real(i,dp)-0.5)
   enddo
   if(Stretch)then
    yy=str_ygrid%smin
    if(yy >yp_min)then
     dpy=dyi/real(np_per_yc(ic),dp)
     i1=(str_ygrid%sind(1)-nyl1+1)*np_per_yc(ic)
     i2=npty_ne-i1
     do i=1,i1
      dxip=dpy*(real(i-i1,dp)-0.5)
      ypt(i,ic)=str_ygrid%smin+L_s*tan(dxip)
      wy(i,ic)=1./(cos(dxip)*cos(dxip))
     enddo
     dxip=dy/real(np_per_yc(ic),dp)
     do i=i1+1,i2
      ypt(i,ic)=str_ygrid%smin+dxip*(real(i-i1,dp)-0.5)
     end do
     do i=i2+1,npty_ne
      dxip=dpy*(real(i-i2,dp)-0.5)
      ypt(i,ic)=str_ygrid%smax+L_s*tan(dxip)
      wy(i,ic)=1./(cos(dxip)*cos(dxip))
     enddo
    endif
   endif
  endif
 enddo
 if(ndim > 2)then !along-z-drection
  do ic=1,nsp
   npzc(ic)=nyh*np_per_zc(ic)
   nptz_ne=npzc(ic)
   dpz=(zp_max-zp_min)/real(npzc(ic),dp)
   do i=1,npzc(ic)
    zpt(i,ic)=zp_min+dpz*(real(i,dp)-0.5)
   enddo
   if(Stretch)then
    zz=str_zgrid%smin
    if(zz>zp_min)then
     dpz=dzi/real(np_per_zc(ic),dp)
     i1=(str_zgrid%sind(1)-nzl1+1)*np_per_zc(ic)
     i2=nptz_ne-i1
     do i=1,i1
      dxip=dpz*(real(i-i1,dp)-0.5)
      zpt(i,ic)=str_zgrid%smin+L_s*tan(dxip)
      wz(i,ic)=1./(cos(dxip)*cos(dxip))
     enddo
     dxip=dz/real(np_per_zc(ic),dp)
     do i=i1+1,i2
      zpt(i,ic)=str_zgrid%smin+dxip*(real(i-i1,dp)-0.5)
     enddo
     do i=i2+1,nptz_ne
      dxip=dpz*(real(i-i2,dp)-0.5)
      zpt(i,ic)=str_zgrid%smax+L_s*tan(dxip)
      wz(i,ic)=1./(cos(dxip)*cos(dxip))
     enddo
    endif
   endif
  enddo
 endif
 ! Now the y-z particle distribution are shared among mpi-tasks
 ! on output =>> loc_jmax, loc_kmax
 !=======================================
 do ic=1,nsp
  call set_pgrid_ind(npyc(ic),npzc(ic),ic)
 enddo
 !===========================
 ! WARNING for charge distribution
 !====================================
 ! wgh(1)=1/mp_per_cell(1) for electron
 ! For nsp =2):
 ! if mp_per_cell(2)=mp_per_cell(1)/ion_min(1)  => wgh(2)=wgh(1)  same weight
 ! otherwise  if mp_per_cell(2)*ion_min(1) < mp_per_cell(1) :
 !           wgh(2)=1/(ion_min(1)*mp_per_cell(2)) > wgh(1)
 ! mp_per_cell have to be set in a way total charge is zero:
 ! For nsp > 2 mp_per_cell(2:nsp) have to be set in a way
 ! Z1*mp_per_cell(Z1)+Z2*mp_per_cell(Z2)=mp_per_cell(El)
 !====================
 ! Longitudinal distribution
 nptx=0
 un(1:nsp)=1.
 if(nsp==2)then
  if(ion_min(2)>0)un(2)=ratio_mpc(2)/real(ion_min(1),dp)   !float(mp_per_cell(1))/float(mp_per_cell(2))
 endif
 select case(layer_mod)
  !================ first uniform layer np1=================
 case(1)
  if(nxl(1)>0)then
   do ic=1,nsp
    n_peak=nxl(1)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(1)*uu
     wghpt(i1,ic)=j1_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(1)
  endif
  !================ first linear ramp np1 => 1 =================
  if(nxl(2)>0)then
   do ic=1,nsp
    n_peak=nxl(2)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(2)*uu
     wghpt(i1,ic)=(np1+uu*(1.-np1))*j0_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(2)
  endif
  !================ Central layer=================
  if(nxl(3)>0)then
   do ic=1,nsp
    n_peak=nxl(3)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(3)*uu
     wghpt(i1,ic)=j0_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(3)
  endif
  !================ second linear ramp =================
  if(nxl(4)>0)then
   do ic=1,nsp
    n_peak=nxl(4)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(4)*uu
     wghpt(i1,ic)=(1.-uu*(1.-np2))*j0_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(4)
  endif
  if(nxl(5)>0)then
   do ic=1,nsp
    n_peak=nxl(5)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(5)*uu
     wghpt(i1,ic)=np2*j0_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(5)
  endif
  !=========================================
 case(2)
  !                 two bumps  (n1/n_c, n2/n_c) of length x_1 and x_2
  !                 n_over_nc enters as average n_over nc= (n1* x_1+n2*x_2)/(x_1+x_2)
  !                 weight j0_norm =>> j0_norm*np1 in x_1       =>> j0_norm*np2 in x_2
  !                 particle per cell uniform
  !================================================
  !================ first linear ramp to first plateau n1/n_c =================
  j1_norm=np1*j0_norm
  j2_norm=np2*j0_norm
  if(nxl(1)>0)then
   do ic=1,nsp
    n_peak=nxl(1)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(1)*uu
     wghpt(i1,ic)=uu*j1_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(1)
  endif
  if(nxl(2)>0)then              !first plateau
   do ic=1,nsp
    n_peak=nxl(2)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(2)*uu
     wghpt(i1,ic)=j1_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(2)
  endif
  !================ np1 => np2 down-ramp =================
  if(nxl(3)>0)then
   do ic=1,nsp
    n_peak=nxl(3)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(3)*uu
     wghpt(i1,ic)=j1_norm+uu*(j2_norm-j1_norm)
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   enddo
   xfsh=xfsh+lpx(3)
  endif
  !================ second plateau n2/n_c < n1/n_c =================
  if(nxl(4)>0)then
   do ic=1,nsp
    n_peak=nxl(4)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(4)*uu
     wghpt(i1,ic)=j2_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(4)
  endif
  if(nxl(5)>0)then     !second down-ramp n2/n_c ==> 0
   do ic=1,nsp
    n_peak=nxl(5)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(5)*uu
     wghpt(i1,ic)=(1.-uu)*j2_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(5)
  endif
 case(3)
  !                 three layers
  !                 lpx(1)[ramp]+lpx(2)[plateau]  and lpx(4) plateu lpx(5) downramp n_ion=0 n_e=n_0
  !                 lpx(3)[plateau]  with a (A1-Z1) dopant with % density np1=n1_per_nc/n_per_nc
  !                 and electronic density n_e=n_0+Z1*n_ion  n0=n_H(+)
  !---------------
  !================================================
  un(2)=np1*ratio_mpc(2)          !float(mp_per_cell(1))/float(mp_per_cell(ic))
  if(ion_min(2)>1)un(2)=un(2)/real(ion_min(1),dp) !float(mp_per_cell(1))/Z1*float(mp_per_cell(ic))
  !Z1 electrons are accounted for by a larger electron weight
  un(1)=1.+ion_min(1)*np1
  if(nxl(1)>0)then
   do ic=1,nsp_run
    n_peak=nxl(1)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(1)*uu
     wghpt(i1,ic)=uu*j0_norm
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(1)
  endif
  if(nxl(2)>0)then              !first plateau
   do ic=1,nsp_run
    n_peak=nxl(2)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(2)*uu
     wghpt(i1,ic)=j0_norm
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(2)
  endif
  !================
  if(nxl(3)>0)then              !H(+) + dopant
   do ic=1,nsp
    n_peak=nxl(3)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(3)*uu
     wghpt(i1,ic)=j0_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(3)
  endif
  !================ second plateau only electrons =================
  if(nxl(4)>0)then
   do ic=1,nsp_run
    n_peak=nxl(4)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(4)*uu
     wghpt(i1,ic)=j0_norm
    end do
    nptx(ic)=nptx(ic)+n_peak
    xfsh=xfsh+lpx(4)
   enddo
  endif
  if(nxl(5)>0)then     !second down-ramp ==> 0
   do ic=1,nsp_run
    n_peak=nxl(5)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(5)*uu
     wghpt(i1,ic)=(1.-uu)*j0_norm
    end do
    nptx(ic)=nptx(ic)+n_peak
    xfsh=xfsh+lpx(5)
   end do
  endif
 case(5)
  !                 three layers: dopant in first layer
  !                 lpx(1)[ramp]+lpx(2)[plateau]  with a (A1-Z1) dopant with % density np1=n1_per_nc/n_per_nc
  !                 and electronic density n_e=n_0+Z1*n_ion  n0=n_H(+)
  !---------------
  !                 lpx(4)[plateau +lpx(5)[downramp] with mp_per_cell(1) electrons
  !                 of density n_e/n_0=1
  !================================================
  un(2)=np1*ratio_mpc(2)                     !float(mp_per_cell(1))/float(mp_per_cell(ic))
  if(ion_min(2)>1)un(2)=un(2)/real(ion_min(1),dp) !float(mp_per_cell(1))/Z1*float(mp_per_cell(ic))
  !Z1 electrons are accounted for by a larger electron weight
  un(1)=1.+ion_min(1)*np1
  if(nxl(1)>0)then
   do ic=1,nsp
    n_peak=nxl(1)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(1)*uu
     wghpt(i1,ic)=uu*j0_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(1)
  endif
  if(nxl(2)>0)then              !first plateau
   do ic=1,nsp
    n_peak=nxl(2)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(2)*uu
     wghpt(i1,ic)=j0_norm
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
   end do
   xfsh=xfsh+lpx(2)
  endif
  !================
  if(nxl(3)>0)then
   do ic=1, nsp_run
    n_peak=nxl(3)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(3)*uu
     wghpt(i1,ic)=j1_norm+uu*(j2_norm-j1_norm)
     wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
    end do
    nptx(ic)=nptx(ic)+n_peak
    xfsh=xfsh+lpx(3)
   enddo
  endif
  !================ second plateau only electrons =================
  if(nxl(4)>0)then
   do ic=1,nsp_run
    n_peak=nxl(4)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i,dp)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(4)*uu
     wghpt(i1,ic)=j0_norm
    end do
    nptx(ic)=nptx(ic)+n_peak
    xfsh=xfsh+lpx(4)
   enddo
  endif
  if(nxl(5)>0)then     !second down-ramp ==> 0
   do ic=1,nsp_run
    n_peak=nxl(5)*np_per_xc(ic)
    do i=1,n_peak
     uu=(real(i)-0.5)/real(n_peak,dp)
     i1=nptx(ic)+i
     xpt(i1,ic)=xfsh+lpx(5)*uu
     wghpt(i1,ic)=(1.-uu)*j0_norm
    end do
    nptx(ic)=nptx(ic)+n_peak
    xfsh=xfsh+lpx(5)
   end do
  endif

!---------------------------------------------------------------!
!------ Cos-sinusoidal background modulation  ---!
!---------------------------------------------------------------!
case(6)
 j1_norm=np1*j0_norm
 j2_norm=np2*j0_norm
 !================ Cos-sinusoidal background modulation =================
 if(nxl(4)>0)then
  do ic=1,nsp
   n_peak=nxl(4)*np_per_xc(ic)
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    i1=nptx(ic)+i
    xpt(i1,ic)=xfsh+lpx(4)*uu !lab-X position in um
    wghpt(i1,ic)=j0_norm*(1.0+n1_over_n*cos(xpt(i1,ic)*2.*pi/lpx(2)))
    wghpt(i1,ic)=wghpt(i1,ic)*un(ic)
   end do
   nptx(ic)=nptx(ic)+n_peak
  end do
  xfsh=xfsh+lpx(4)
 endif
!----------------------------!


 end select
 !================================
 ! END of section setting global coordinates
 !=================================
 ! LOCAL to mpi domains particle representation
 ! NO dependence of (y,z) coordinates on x coordinate
 !=====loc_npty,loc_nptz allocated to (6) yz-layers or components
 loc_npty(1:nsp)=loc_jmax(imody,1:nsp)
 loc_nptz(1:nsp)=loc_kmax(imodz,1:nsp)
 !===========================
 npty_ne=maxval(loc_npty(1:nsp))
 nptz_ne=maxval(loc_nptz(1:nsp))
 !==========================
 !====================
 if(npty_ne >0)then
  allocate(loc_wghy(npty_ne,nsp))
  allocate(loc_ypt(npty_ne,nsp))
  loc_ypt=0.0
  loc_wghy=1.
 endif
 !==============
 if(nptz_ne >0)then
  allocate(loc_wghz(nptz_ne,nsp))
  allocate(loc_zpt(nptz_ne,nsp))
  loc_wghz=1.
  loc_zpt=0.0
 endif
 !====================
 call mpi_yz_part_distrib(nsp,loc_npty,loc_nptz,npyc,npzc,&
  ymin_t,zmin_t,wy,wz)
 ! Restricts to the computational box
 !=================================
 do ic=1,nsp
  do j=1,nptx(ic)
   if(xpt(j,ic)>xmax)exit
  end do
  nptx(ic)=j-1
 end do
 if(pe0)then
  write(6,*)'last particle coordinate',nptx(1:nsp)
 endif
 !=========== Local x-distribution
 allocate(loc_wghx(nptx_max,nsp))
 do ic=1,nsp
  call set_pgrid_xind(nptx(ic),ic)
 end do
 if(pe0)write(6,*)'imax on each mpi task',loc_imax(0:npe_xloc-1,1)
 loc_nptx(1:nsp)=loc_imax(imodx,1:nsp)
 !==================
 do ic=1,nsp
  i1=0
  do i=1,nptx(ic)
   if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
    .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
   i1=i1+1
   loc_xpt(i1,ic)=xpt(i,ic)
   loc_wghx(i1,ic)=wghpt(i,ic)
   endif
  end do
  if(i1 /= loc_nptx(ic))write(6,*)'wrong part numbers',imodx,i1
 end do
 !=============================
 nps_loc(1:nsp)=loc_imax(imodx,1:nsp)*loc_jmax(imody,1:nsp)*loc_kmax(imodz,1:nsp)
 !===================================
 loc_npart(imody,imodz,imodx,1:nsp)=nps_loc(1:nsp)
 ! Alocation using a large buffer npt_max=mp_per_cell(1)*nx_loc*ny_loc*nz_loc

 npmax=maxval(nps_loc(1:nsp))
 npmax=max(npt_buffer,npmax)
 nps_loc(1)=npmax
 call p_alloc(npmax,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
 !===========================
 last_particle_index=0
 !============
 do ic=1,nsp
  p=0
  i2=loc_nptx(ic)
  if (i2>0) call pspecies_distribute(spec(ic),t0_pl(ic),unit_charge(ic),&
   p,ic,i2,last_particle_index(ic))
 enddo

 end subroutine multi_layer_gas_target
 !=============================
 !===============================
 subroutine preplasma_multisp(nyh,xf0)

 integer,intent(in) :: nyh
 real(dp),intent(in) :: xf0
 integer :: p,ip
 integer :: l,i,i1,i2,ic
 integer :: np_per_zcell(6),n_peak
 integer :: nptx_loc(6)
 integer :: npty_layer(6),npty_ne,nptz_ne
 integer :: npmax,nps_loc(4)
 real(dp) :: uu,yy,dxip,dpy,np1,np2,np1_loc
 real(dp) :: zp_min,zp_max,yp_min,yp_max,xp_min,xp_max
 real(dp) :: xfsh,l_inv
 integer :: nxl(6),nyl1
 integer :: ip_ion,ip_el,ip_pr,npyc(6)
 real(dp),allocatable :: wy(:,:),wz(:,:)
 !=================
 xp_min=xmin
 xp_max=xmax
 np_per_zcell(1:6)=1
 !!+++++++++++++++++++++++++++++++
 nxl=0
 np1=n1_over_n
 np2=n2_over_n
 if(n_over_nc >0.0)then
  np1=np1/n_over_nc
  np2=np2/n_over_nc
 endif
 !========= gridding the transverese target size
 nyl1=1+ny/2-nyh/2  !=1 if nyh=ny
 yp_min=ymin_t
 yp_max=ymax_t
 !=============================
 ! Multispecies
 !=============================
 npty=maxval(np_per_yc(1:6))
 npty=nyh*npty
 nptz=1
 if(ndim==3)then
  np_per_zcell(1:6)=np_per_zc(1:6)
  zp_min=zmin_t    !-Lz
  zp_max=zmax_t    !+Lz
  nptz=maxval(np_per_zc(1:6))
  nptz=nyh*nptz
 endif
 allocate(ypt(npty+1,6))
 allocate(zpt(nptz+1,6))
 allocate(wy(npty+1,6))
 allocate(wz(nptz+1,6))
 ypt=0.
 zpt=0.
 wy=1.
 wz=1.
 !==================
 allocate(loc_jmax(0:npe_yloc-1,1:6))
 allocate(loc_kmax(0:npe_zloc-1,1:6))
 allocate(loc_imax(0:npe_xloc-1,1:6))
 !====================
 ! Layers space ordering(1:6)
 ! [1:2 electon-proton foam,3:4 electron-ion target, 5:6 electron-proton coat]
 !===============
 npyc(1:2)=np_per_yc(3:4) !pre-plasma or foam layer (Z1+El) lx(1) layer
 npyc(3:4)=np_per_yc(1:2) ! central target+ exp ramp (Z1+El) lx(2)+lx(3)+lx(4)
 npyc(5:6)=np_per_yc(5:6) ! contaminants (Z2+ El)layer lx(5)
 do ic=1,6
  npty_layer(ic)=nyh*npyc(ic)
 end do
 nptz_ne=1
 do ic=1,6
  npty_ne=npty_layer(ic)
  dpy=(yp_max-yp_min)/real(npty_ne,dp)
  if(ic>4)dpy=(yp_max-yp_min)/real(npty_ne,dp)
  do i=1,npty_ne
   ypt(i,ic)=yp_min+dpy*(real(i,dp)-0.5)
  enddo
  if(Stretch)then
   yy=str_ygrid%smin
   if(yy >yp_min)then
    dpy=dyi/real(npyc(ic),dp)
    i1=(str_ygrid%sind(1)-nyl1+1)*npyc(ic)
    i2=npty_ne-i1
    do i=1,i1
     dxip=dpy*(real(i-i1,dp)-0.5)
     ypt(i,ic)=str_ygrid%smin+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
    dxip=dy/real(npyc(ic),dp)
    do i=i1+1,i2
     ypt(i,ic)=str_ygrid%smin+dxip*(real(i-i1,dp)-0.5)
    end do
    do i=i2+1,npty_ne
     dxip=dpy*(real(i-i2,dp)-0.5)
     ypt(i,ic)=str_ygrid%smax+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
   endif
  endif
  nptz_ne=1
  if(ndim==3)then
   zpt(1:npty_ne,ic)=ypt(1:npty_ne,ic)
   wz(1:npty_ne,ic)=wy(1:npty_ne,ic)
   nptz_ne=npty_ne
  endif
  call set_pgrid_ind(npty_ne,nptz_ne,ic)
 end do
 !===========================
 xtot=0.0
 do i=1,5
  nxl(i)=nint(dx_inv*lpx(i))
  lpx(i)=nxl(i)*dx
  xtot=xtot+lpx(i)
 end do
 if(nsp < 3)then
  nxl(5)=0
  if(pe0)then
   write(6,*)'Warning : for nsp=2 nxl(1) and nxl(5) forced to zero'
  endif
 endif
 xfsh=xf0+lpx(7)
 targ_in=xfsh
 targ_end=targ_in+xtot
 !               Species x-distribution
 !            lx(1)-lx(3) (Z1+ El)   lx(5) (Z2+El)
 nptx_loc(1:2)=nxl(1)*np_per_xc(3:4)
 nptx_loc(3:4)=(nxl(2)+nxl(3))*np_per_xc(1:2)
 nptx_loc(5:6)=nxl(5)*np_per_xc(5:6)
 nptx(1)=nptx_loc(1)+nptx_loc(3)+nptx_loc(5)  !electrons
 nptx(2)=nptx_loc(4)                          !Z1-A1 species
 nptx(3)=nptx_loc(2)+nptx_loc(6)              !Z2-A2  species nxl(1)+nlx(4)
 nptx_max=maxval(nptx_loc(1:6))
 !=======================
 allocate(xpt(nptx_max,6))
 allocate(wghpt(nptx_max,6))

 allocate(loc_xpt(nptx_max,6))
 allocate(loc_wghx(nptx_max,6))
 wghpt(1:nptx_max,1:6)=1.
 !================ local y-z part coordinates==========
 loc_npty(1:6)=loc_jmax(imody,1:6)
 loc_nptz(1:6)=loc_kmax(imodz,1:6)
 !=============================
 npty_ne=maxval(loc_npty(1:6))
 nptz_ne=maxval(loc_nptz(1:6))
 if(npty_ne >0)then
  allocate(loc_wghy(npty_ne,6))
  allocate(loc_ypt(npty_ne,6))
  loc_wghy=1.
  loc_ypt=0.0
 endif
 if(nptz_ne >0)then
  allocate(loc_wghz(nptz_ne,6))
  allocate(loc_zpt(nptz_ne,6))
  loc_wghz=1.
  loc_zpt=0.0
 endif
 call mpi_yz_part_distrib(6,loc_npty,loc_nptz,npty_layer,npty_layer,&
  ymin_t,zmin_t,wy,wz)
 !==================
 ! first layer: electrons and Z1 ions
 !x distribution
 loc_imax(imodx,1:6)=nptx_loc(1:6)
 nps_loc=0
 if(nxl(1) >0)then
  do ic=1,2
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    xpt(i,ic)=xfsh+lpx(1)*uu
   end do
  end do
  wghpt(1:nptx_loc(1),1)=np1*j0_norm
  wghpt(1:nptx_loc(2),2)=np1*j0_norm
  if(mp_per_cell(3) >0)then
   uu=ratio_mpc(3)                                  !float(mp_per_cell(1))/float(mp_per_cell(3))
   wghpt(1:nptx_loc(1),1)=wghpt(1:nptx_loc(1),1)*uu
   uu=ratio_mpc(4)/unit_charge(2)                   !float(mp_per_cell(1))/float(mp_per_cell(4))
   wghpt(1:nptx_loc(2),2)=wghpt(1:nptx_loc(2),2)*uu
  endif
  xfsh=xfsh+lpx(1)
  !=========== Distributes on x-MPI tasks the first layer
  do ic=1,2
   i1=0
   do i=1,nptx_loc(ic)
    if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
     .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
    i1=i1+1
    loc_xpt(i1,ic)=xpt(i,ic)
    loc_wghx(i1,ic)=wghpt(i,ic)
    endif
   end do
   loc_imax(imodx,ic)=i1
  enddo
  !========================
  !========================
  p=imodx
  l=imody
  ip=imodz
  ! Counts particles

  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,1)*loc_jmax(l,1)*loc_kmax(ip,1)
  nps_loc(2)=nps_loc(2)+&
   loc_imax(p,2)*loc_jmax(l,2)*loc_kmax(ip,2)

 endif
 !------------------------------
 ! Electrons and Z1_ions : central layer
 ! x distribution
 !====================
 do ic=3,4
  n_peak=nptx_loc(ic)
  do i=1,n_peak
   xpt(i,ic)=xfsh+(lpx(2)+lpx(3))*(real(i,dp)-0.5)/real(n_peak,dp)
   wghpt(i,ic)=j0_norm
  end do
 end do
 uu=ratio_mpc(2)/real(ion_min(1),dp)
 ic=4
 n_peak=nptx_loc(ic)
 wghpt(1:n_peak,ic)=j0_norm*uu
 !================================
 ! Electrons and Z1 ions have the same weight if uu=1
 !=================================
 xfsh=xfsh+lpx(3)+lpx(2)
 if(np1> 0.0) then
  np1_loc=np1
 else
  np1_loc=0.005
 endif
 !======== a preplasma rump
 if(nxl(2) >0)then
  do ic=3,4
   n_peak=nxl(2)*np_per_xc(ic-2)
   l_inv=log(1./np1_loc)
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    ! rampa esponenziale v1
    ! wghpt(i,ic)=wghpt(i,ic)*exp(-5.*(1.-uu))
    ! rampa esponenziale v2
    ! Same species as later 2 (electrons+Z1-ions)
    wghpt(i,ic)=wghpt(i,ic)*np1_loc*exp(uu*l_inv)
    ! rampa lineare
    ! wghpt(i,ic)=wghpt(i,ic)*uu
   end do
  end do
 endif
 !=========== Distributes on x-MPI tasks
 do ic=3,4
  i1=0
  do i=1,nptx_loc(ic)
   if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
    .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
   i1=i1+1
   loc_xpt(i1,ic)=xpt(i,ic)
   loc_wghx(i1,ic)=wghpt(i,ic)
   endif
  end do
  loc_imax(imodx,ic)=i1
 end do
 p=imodx
 l=imody
 ip=imodz

 nps_loc(1)=nps_loc(1)+&
  loc_imax(p,3)*loc_jmax(l,3)*loc_kmax(ip,3)
 nps_loc(2)=nps_loc(2)+&
  loc_imax(p,4)*loc_jmax(l,4)*loc_kmax(ip,4)
 !============================
 if(nptx_loc(5)>0.0)then
  do ic=5,6
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    xpt(i,ic)=xfsh+lpx(5)*(real(i,dp)-0.5)/real(n_peak,dp)
   end do
   wghpt(1:n_peak,ic)=np2*j0_norm
  end do
  do ic=5,6
   if(mp_per_cell(ic) >0)then
    uu=ratio_mpc(ic)       !float(mp_per_cell(1))/float(mp_per_cell(ic))
    n_peak=nptx_loc(ic)
    wghpt(1:n_peak,ic)=wghpt(1:n_peak,ic)*uu
   endif
  end do
  xfsh=xfsh+lpx(5)
  !===============================
  ! Warning : holds for Z2_ion=1
  !===============================
  do ic=5,6
   i1=0
   do i=1,nptx_loc(ic)
    if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
     .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
    i1=i1+1
    loc_xpt(i1,ic)=xpt(i,ic)
    loc_wghx(i1,ic)=wghpt(i,ic)
    endif
   end do
   loc_imax(imodx,ic)=i1
  end do
  p=imodx
  l=imody
  ip=imodz

  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,5)*loc_jmax(l,5)*loc_kmax(ip,5)
  nps_loc(3)=nps_loc(3)+&
   loc_imax(p,6)*loc_jmax(l,6)*loc_kmax(ip,6)

 endif
 !======================
 npmax=maxval(nps_loc(1:nsp))
 npmax=max(npmax,1)
 call p_alloc(npmax,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
 !===========================
 ip_el=0
 ip_pr=0
 ip_ion=0
 !============
 ! The first electron-proton(or C) foam layer
 if(nxl(1) >0)then
  p=0
  i2=loc_imax(imodx,1)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,1,i2,ip_el)
  p=0
  i2=loc_imax(imodx,2)
  call pspecies_distribute(spec(2),t0_pl(2),unit_charge(2),p,2,i2,ip_ion)
 endif
 !=========================
 ! The second electron-ion solid electron-Z1 layer
 p=ip_el
 i2=loc_imax(imodx,3)
 call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,3,i2,ip_el)

 p=ip_ion
 i2=loc_imax(imodx,4)
 call pspecies_distribute(spec(2),t0_pl(2),unit_charge(2),p,4,i2,ip_ion)
 !============
 ! The third electron-proton layer
 !=========================
 if(nxl(5) >0.0)then
  p=ip_el
  i2=loc_imax(imodx,5)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,5,i2,ip_el)
  p=0
  i2=loc_imax(imodx,6)
  call pspecies_distribute(spec(3),t0_pl(3),unit_charge(3),p,6,i2,ip_pr)
 endif

 do ic=1,nsp
  loc_npart(imody,imodz,imodx,ic)=nps_loc(ic)
 end do

 !============
 end subroutine preplasma_multisp
 !=====================
 subroutine multi_layer_multisp(nyh,xf0)

 integer,intent(in) :: nyh
 real(dp),intent(in) :: xf0
 integer :: p,ip
 integer :: l,i,i1,i2,ic
 integer :: np_per_zcell(6),n_peak
 integer :: nptx_loc(6)
 integer :: npty_layer(6),npty_ne,nptz_ne
 integer :: npmax,nps_loc(4)
 real(dp) :: uu,yy,dxip,dpy,np1,np2,np1_loc
 real(dp) :: zp_min,zp_max,yp_min,yp_max,xp_min,xp_max
 real(dp) :: xfsh,l_inv
 integer :: nxl(6),nyl1
 integer :: ip_ion,ip_el,ip_pr,npyc(6)
 real(dp),allocatable :: wy(:,:),wz(:,:)
 !=================
 xp_min=xmin
 xp_max=xmax
 np_per_zcell(1:6)=1
 !!+++++++++++++++++++++++++++++++
 nxl=0
 np1=n1_over_n
 np2=n2_over_n
 if(n_over_nc >0.0)then
  np1=np1/n_over_nc
  np2=np2/n_over_nc
 endif
 !========= gridding the transverese target size
 nyl1=1+ny/2-nyh/2  !=1 if nyh=ny
 yp_min=ymin_t
 yp_max=ymax_t
 !=============================
 ! Multispecies
 !=============================
 npty=maxval(np_per_yc(1:6))
 npty=nyh*npty
 nptz=1
 if(ndim==3)then
  np_per_zcell(1:6)=np_per_zc(1:6)
  zp_min=zmin_t    !-Lz
  zp_max=zmax_t    !+Lz
  nptz=maxval(np_per_zc(1:6))
  nptz=nyh*nptz
 endif
 allocate(ypt(npty+1,6))
 allocate(zpt(nptz+1,6))
 allocate(wy(npty+1,6))
 allocate(wz(nptz+1,6))
 ypt=0.
 zpt=0.
 wy=1.
 wz=1.
 !==================
 allocate(loc_jmax(0:npe_yloc-1,1:6))
 allocate(loc_kmax(0:npe_zloc-1,1:6))
 allocate(loc_imax(0:npe_xloc-1,1:6))
 !====================
 ! Layers space ordering(1:6)
 ! [1:2 electon-proton foam,3:4 electron-ion target, 5:6 electron-proton coat]
 !===============
 npyc(1:2)=np_per_yc(3:4)  !pre-plasma or foam layer
 npyc(3:4)=np_per_yc(1:2)  ! central target
 npyc(5:6)=np_per_yc(5:6)  ! contaminants layer
 do ic=1,6
  npty_layer(ic)=nyh*npyc(ic)
 end do
 nptz_ne=1
 do ic=1,6
  npty_ne=npty_layer(ic)
  dpy=(yp_max-yp_min)/real(npty_ne,dp)
  if(ic>4)dpy=(yp_max-yp_min)/real(npty_ne,dp)
  do i=1,npty_ne
   ypt(i,ic)=yp_min+dpy*(real(i,dp)-0.5)
  enddo
  if(Stretch)then
   yy=str_ygrid%smin
   if(yy >yp_min)then
    dpy=dyi/real(npyc(ic),dp)
    i1=(str_ygrid%sind(1)-nyl1+1)*npyc(ic)
    i2=npty_ne-i1
    do i=1,i1
     dxip=dpy*(real(i-i1,dp)-0.5)
     ypt(i,ic)=str_ygrid%smin+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
    dxip=dy/real(npyc(ic),dp)
    do i=i1+1,i2
     ypt(i,ic)=str_ygrid%smin+dxip*(real(i-i1,dp)-0.5)
    end do
    do i=i2+1,npty_ne
     dxip=dpy*(real(i-i2,dp)-0.5)
     ypt(i,ic)=str_ygrid%smax+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
   endif
  endif
  nptz_ne=1
  if(ndim==3)then
   zpt(1:npty_ne,ic)=ypt(1:npty_ne,ic)
   wz(1:npty_ne,ic)=wy(1:npty_ne,ic)
   nptz_ne=npty_ne
  endif
  call set_pgrid_ind(npty_ne,nptz_ne,ic)
 end do
 !===========================
 xtot=0.0
 do i=1,5
  nxl(i)=nint(dx_inv*lpx(i))
  lpx(i)=nxl(i)*dx
  xtot=xtot+lpx(i)
 end do
 if(nsp < 3)then
  nxl(1)=0
  nxl(5)=0
  if(pe0)then
   write(6,*)'Warning : for nsp=2 nxl(1) and nxl(5) forced to zero'
  endif
 endif
 xfsh=xf0+lpx(7)
 targ_in=xfsh
 targ_end=targ_in+xtot
 ! Species x-distribution
 !========= np_per_xc(1:2) electrons and Z1-ions in central layer +ramp
 !========= np_per_xc(3:4) electrons and Z2-ions in layer 1
 !========= np_per_xc(5:6) electrons and Z2-ions in layer 3 +ramp
 nptx_loc(1:2)=nxl(1)*np_per_xc(3:4)
 nptx_loc(3:4)=(nxl(2)+nxl(3))*np_per_xc(1:2)
 nptx_loc(5:6)=nxl(5)*np_per_xc(5:6)
 nptx(1)=nptx_loc(1)+nptx_loc(3)+nptx_loc(5)  !electrons
 nptx(2)=nptx_loc(4)                          !Z1-A1 species
 nptx(3)=nptx_loc(2)+nptx_loc(6)              !Z2-A2  species nxl(1)+nlx(4)
 nptx_max=maxval(nptx_loc(1:6))
 !=======================
 allocate(xpt(nptx_max,6))
 allocate(wghpt(nptx_max,6))

 allocate(loc_xpt(nptx_max,6))
 allocate(loc_wghx(nptx_max,6))
 wghpt(1:nptx_max,1:6)=1.
 !================ local y-z part coordinates==========
 loc_npty(1:6)=loc_jmax(imody,1:6)
 loc_nptz(1:6)=loc_kmax(imodz,1:6)
 !=============================
 npty_ne=maxval(loc_npty(1:6))
 nptz_ne=maxval(loc_nptz(1:6))
 if(npty_ne >0)then
  allocate(loc_wghy(npty_ne,6))
  allocate(loc_ypt(npty_ne,6))
  loc_wghy=1.
  loc_ypt=0.
 endif
 if(nptz_ne >0)then
  allocate(loc_wghz(nptz_ne,6))
  allocate(loc_zpt(nptz_ne,6))
  loc_wghz=1.
  loc_zpt=0.0
 endif
 call mpi_yz_part_distrib(6,loc_npty,loc_nptz,npty_layer,npty_layer,&
  ymin_t,zmin_t,wy,wz)
 !==================
 ! nsp ordering: electrons+ Z1 ions + Z2 ions +Z3 ions
 ! first layer: electrons and Z2 (protons) ions
 !x distribution
 loc_imax(imodx,1:6)=nptx_loc(1:6)
 nps_loc=0
 if(nxl(1) >0)then
  do ic=1,2
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    xpt(i,ic)=xfsh+lpx(1)*uu
   end do
  end do
  wghpt(1:nptx_loc(1),1)=np1*j0_norm
  wghpt(1:nptx_loc(2),2)=np1*j0_norm
  if(mp_per_cell(3) >0)then
   uu=ratio_mpc(3)   !float(mp_per_cell(1))/float(mp_per_cell(3))
   wghpt(1:nptx_loc(1),1)=wghpt(1:nptx_loc(1),1)*uu
   uu=ratio_mpc(4)/unit_charge(3) !float(mp_per_cell(1))/float(mp_per_cell(4))
   wghpt(1:nptx_loc(2),2)=wghpt(1:nptx_loc(2),2)*uu
  endif
  xfsh=xfsh+lpx(1)
  !=========== Distributes on x-MPI tasks
  do ic=1,2
   i1=0
   do i=1,nptx_loc(ic)
    if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
     .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
    i1=i1+1
    loc_xpt(i1,ic)=xpt(i,ic)
    loc_wghx(i1,ic)=wghpt(i,ic)
    endif
   end do
   loc_imax(imodx,ic)=i1
  enddo
  !========================
  !========================
  p=imodx
  l=imody
  ip=imodz
  ! Counts particles (e+Z2 ions)

  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,1)*loc_jmax(l,1)*loc_kmax(ip,1)
  nps_loc(3)=nps_loc(3)+&
   loc_imax(p,2)*loc_jmax(l,2)*loc_kmax(ip,2)

 endif
 !------------------------------
 ! Electrons and Z1_ions : central layer
 ! x distribution
 !====================
 do ic=3,4
  n_peak=nptx_loc(ic)
  do i=1,n_peak
   xpt(i,ic)=xfsh+(lpx(2)+lpx(3))*(real(i,dp)-0.5)/real(n_peak,dp)
   wghpt(i,ic)=j0_norm
  end do
 end do
 uu=ratio_mpc(2)/real(ion_min(1),dp)
 ic=4
 n_peak=nptx_loc(ic)
 wghpt(1:n_peak,ic)=j0_norm*uu
 !================================
 ! WARNING : electrons and Z1 ions have the same weight
 ! The Z1 ion mp_per_cell(2)=mp_per_cell(1)/Z1_ion
 !=================================
 xfsh=xfsh+lpx(3)+lpx(2)
 if(np1> 0.0) then
  np1_loc=np1
 else
  np1_loc=0.005
 endif
 !======== a preplasma rump
 if(nxl(2) >0)then
  do ic=3,4
   n_peak=nxl(2)*np_per_xc(ic-2)
   l_inv=log(1./np1_loc)
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    ! rampa esponenziale v1
    ! wghpt(i,ic)=wghpt(i,ic)*exp(-5.*(1.-uu))
    ! rampa esponenziale v2
    ! Same species as later 2 (electrons+Z1-ions)
    wghpt(i,ic)=wghpt(i,ic)*np1_loc*exp(uu*l_inv)
    ! rampa lineare
    ! wghpt(i,ic)=wghpt(i,ic)*uu
   end do
  end do
 endif
 !=========== Distributes on x-MPI tasks
 do ic=3,4
  i1=0
  do i=1,nptx_loc(ic)
   if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
    .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
   i1=i1+1
   loc_xpt(i1,ic)=xpt(i,ic)
   loc_wghx(i1,ic)=wghpt(i,ic)
   endif
  end do
  loc_imax(imodx,ic)=i1
 end do
 p=imodx
 l=imody
 ip=imodz

 nps_loc(1)=nps_loc(1)+&
  loc_imax(p,3)*loc_jmax(l,3)*loc_kmax(ip,3)
 nps_loc(2)=nps_loc(2)+&
  loc_imax(p,4)*loc_jmax(l,4)*loc_kmax(ip,4)
 !============================
 if(nptx_loc(5)>0.0)then
  do ic=5,6
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    xpt(i,ic)=xfsh+lpx(5)*(real(i,dp)-0.5)/real(n_peak,dp)
   end do
   wghpt(1:n_peak,ic)=np2*j0_norm
  end do
  do ic=5,6
   if(mp_per_cell(ic) >0)then
    uu=ratio_mpc(ic)       !float(mp_per_cell(1))/float(mp_per_cell(ic))
    n_peak=nptx_loc(ic)
    wghpt(1:n_peak,ic)=wghpt(1:n_peak,ic)*uu
   endif
  end do
  xfsh=xfsh+lpx(5)
  !===============================
  ! Warning : holds for Z2_ion=1
  !===============================
  do ic=5,6
   i1=0
   do i=1,nptx_loc(ic)
    if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
     .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
    i1=i1+1
    loc_xpt(i1,ic)=xpt(i,ic)
    loc_wghx(i1,ic)=wghpt(i,ic)
    endif
   end do
   loc_imax(imodx,ic)=i1
  end do
  p=imodx
  l=imody
  ip=imodz

  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,5)*loc_jmax(l,5)*loc_kmax(ip,5)
  nps_loc(3)=nps_loc(3)+&
   loc_imax(p,6)*loc_jmax(l,6)*loc_kmax(ip,6)

 endif
 !======================
 npmax=maxval(nps_loc(1:nsp))
 npmax=max(npmax,1)
 call p_alloc(npmax,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
 !===========================
 ip_el=0
 ip_pr=0
 ip_ion=0
 !============
 ! The first electron-proton(or C) foam layer
 if(nxl(1) >0)then
  p=0
  i2=loc_imax(imodx,1)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,1,i2,ip_el)
  p=0
  i2=loc_imax(imodx,2)
  call pspecies_distribute(spec(3),t0_pl(3),unit_charge(3),p,2,i2,ip_pr)
 endif
 !=========================
 ! The second electron-ion solid electron-Z1 layer
 p=ip_el
 i2=loc_imax(imodx,3)
 call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,3,i2,ip_el)

 p=0
 i2=loc_imax(imodx,4)
 call pspecies_distribute(spec(2),t0_pl(2),unit_charge(2),p,4,i2,ip_ion)
 !============
 ! The third electron-proton layer
 !=========================
 if(nxl(5) >0.0)then
  p=ip_el
  i2=loc_imax(imodx,5)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,5,i2,ip)
  p=ip_pr
  i2=loc_imax(imodx,6)
  call pspecies_distribute(spec(3),t0_pl(3),unit_charge(3),p,6,i2,ip)
 endif

 do ic=1,nsp
  loc_npart(imody,imodz,imodx,ic)=nps_loc(ic)
 end do

 !============
 end subroutine multi_layer_multisp
 !=======================
 subroutine multisp_target(nyh,xf0)

 integer,intent(in) :: nyh
 real(dp),intent(in) :: xf0
 integer :: p,ip
 integer :: l,i,i1,i2,ic
 integer :: np_per_zcell(6),n_peak
 integer :: nptx_loc(7)
 integer :: npty_layer(7),npty_ne,nptz_ne
 integer :: npmax,nps_loc(4)
 real(dp) :: uu,yy,dxip,dpy,np1,np2,np1_loc
 real(dp) :: zp_min,zp_max,yp_min,yp_max,xp_min,xp_max
 real(dp) :: xfsh,l_inv
 integer :: nxl(6),nyl1
 integer :: ip_ion,ip_el,ip_pr,npyc(7)
 real(dp),allocatable :: wy(:,:),wz(:,:)
 !=================
 xp_min=xmin
 xp_max=xmax
 np_per_zcell(1:6)=1
 !!+++++++++++++++++++++++++++++++
 nxl=0
 np1=n1_over_n
 np2=n2_over_n
 if(n_over_nc >0.0)then
  np1=np1/n_over_nc
  np2=np2/n_over_nc
 endif
 !========= gridding the transverese target size
 nyl1=1+ny/2-nyh/2  !=1 if nyh=ny
 yp_min=ymin_t
 yp_max=ymax_t
 !=============================
 ! Multispecies     nsp=4 required
 ! Particles distribution np_per_yc(1:3) and np_per_xc(1:3) central target
 ! (El+Z1+ Z3)
 !                        np_per_yc(5:6) and np_per_xc(5:6)
 !                        contaminants (El+Z2) in front and rear layers
 !=============================================
 !WARNING  nm_per_cell(1:3) and mp_per_cell(5:6) have to be set in a way global
 !zero charge per cell is assured
 ! In central layer:
 ! Electron number mp_per_cell(1)= Z1*mp_per_cell(2) +Z3*mp_per_cell(3)
 !=============================
 npty=maxval(np_per_yc(1:6))
 npty=nyh*npty
 nptz=1
 if(ndim==3)then
  np_per_zcell(1:6)=np_per_zc(1:6)
  zp_min=zmin_t    !-Lz
  zp_max=zmax_t    !+Lz
  nptz=maxval(np_per_zc(1:6))
  nptz=nyh*nptz
 endif
 allocate(ypt(npty+1,7))
 allocate(zpt(nptz+1,7))
 allocate(wy(npty+1,7))
 allocate(wz(nptz+1,7))
 ypt=0.
 zpt=0.
 wy=1.
 wz=1.
 !==================
 allocate(loc_jmax(0:npe_yloc-1,1:7))
 allocate(loc_kmax(0:npe_zloc-1,1:7))
 allocate(loc_imax(0:npe_xloc-1,1:7))
 !====================
 npyc(1:2)=np_per_yc(5:6)       !contaminants
 npyc(3:5)=np_per_yc(1:3)  ! central target (El + two(Z1-Z3) ion species
 npyc(6:7)=npyc(1:2)
 do ic=1,7
  npty_layer(ic)=nyh*npyc(ic)
 end do
 nptz_ne=1
 do ic=1,7
  npty_ne=npty_layer(ic)
  dpy=(yp_max-yp_min)/real(npty_ne,dp)
  do i=1,npty_ne
   ypt(i,ic)=yp_min+dpy*(real(i,dp)-0.5)
  enddo
  if(Stretch)then
   yy=str_ygrid%smin
   if(yy >yp_min)then
    dpy=dyi/real(npyc(ic),dp)
    i1=(str_ygrid%sind(1)-nyl1+1)*npyc(ic)
    i2=npty_ne-i1
    do i=1,i1
     dxip=dpy*(real(i-i1,dp)-0.5)
     ypt(i,ic)=str_ygrid%smin+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
    dxip=dy/real(npyc(ic),dp)
    do i=i1+1,i2
     ypt(i,ic)=str_ygrid%smin+dxip*(real(i-i1,dp)-0.5)
    end do
    do i=i2+1,npty_ne
     dxip=dpy*(real(i-i2,dp)-0.5)
     ypt(i,ic)=str_ygrid%smax+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
   endif
  endif
  nptz_ne=1
  if(ndim==3)then
   zpt(1:npty_ne,ic)=ypt(1:npty_ne,ic)
   wz(1:npty_ne,ic)=wy(1:npty_ne,ic)
   nptz_ne=npty_ne
  endif
  call set_pgrid_ind(npty_ne,nptz_ne,ic)
 end do
 !===========================
 xtot=0.0
 do i=1,5
  nxl(i)=nint(dx_inv*lpx(i))
  lpx(i)=nxl(i)*dx
  xtot=xtot+lpx(i)
 end do
 xfsh=xf0+lpx(7)
 targ_in=xfsh
 targ_end=targ_in+xtot
 ! Species x-distribution
 !=  np_per_xc(1:3) electrons and Z1-Z3 ions in central layer
 !=  np_per_xc(5:6) electrons and Z2-ions front and rear side contaminants (same
 !composition)
 !======================================
 nptx_loc(1:2)=nxl(1)*np_per_xc(5:6)
 nptx_loc(3:5)=(nxl(2)+nxl(3))*np_per_xc(1:3)
 nptx_loc(6:7)=nxl(5)*np_per_xc(5:6)
 !============ nptx(nsp)  distribution
 nptx(1)=nptx_loc(1)+nptx_loc(3)+nptx_loc(6)  !electrons
 nptx(2)=nptx_loc(4)                          !Z1-A1 species
 nptx(3)=nptx_loc(2)+nptx_loc(6)              !Z2-A2  species in nxl(1) and nxl(5) layer
 nptx(4)=nptx_loc(5)                          !Z3-A3 species (for nsp=4)
 nptx_max=maxval(nptx_loc(1:7))
 !=======================
 allocate(xpt(nptx_max,7))
 allocate(wghpt(nptx_max,7))

 allocate(loc_xpt(nptx_max,7))
 allocate(loc_wghx(nptx_max,7))
 wghpt(1:nptx_max,1:7)=1.
 !================ local y-z part coordinates==========
 loc_npty(1:7)=loc_jmax(imody,1:7)
 loc_nptz(1:7)=loc_kmax(imodz,1:7)
 !=============================
 npty_ne=maxval(loc_npty(1:7))
 nptz_ne=maxval(loc_nptz(1:7))
 if(npty_ne >0)then
  allocate(loc_wghy(npty_ne,7))
  allocate(loc_ypt(npty_ne,7))
  loc_wghy=1.
  loc_ypt=0.
 endif
 if(nptz_ne >0)then
  allocate(loc_wghz(nptz_ne,7))
  allocate(loc_zpt(nptz_ne,7))
  loc_wghz=1.
  loc_zpt=0.0
 endif
 call mpi_yz_part_distrib(7,loc_npty,loc_nptz,npty_layer,npty_layer,&
  ymin_t,zmin_t,wy,wz)
 !==================
 ! nsp ordering: electrons+ Z1 ions + Z2 ions
 ! first and last layers: electrons and Z3 (protons) ions
 loc_imax(imodx,1:7)=nptx_loc(1:7)
 nps_loc=0
 !==========================================
 if(nxl(1) >0)then                  !first contaminant layer
  do ic=1,2
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    xpt(i,ic)=xfsh+lpx(1)*uu
   end do
  end do
  wghpt(1:nptx_loc(1),1)=np1*j0_norm    !El
  wghpt(1:nptx_loc(2),2)=np1*j0_norm    !Z2-A2
  if(mp_per_cell(5) >0)then
   uu=ratio_mpc(5)   !float(mp_per_cell(1))/float(mp_per_cell(5))
   wghpt(1:nptx_loc(1),1)=wghpt(1:nptx_loc(1),1)*uu
   uu=ratio_mpc(6)/real(ion_min(2),dp)    !float(mp_per_cell(1))/Z2*float(mp_per_cell(6))
   wghpt(1:nptx_loc(2),2)=wghpt(1:nptx_loc(2),2)*uu
  endif
  xfsh=xfsh+lpx(1)
  !=========== Distributes on x-MPI tasks
  do ic=1,2
   i1=0
   do i=1,nptx_loc(ic)
    if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
     .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
    i1=i1+1
    loc_xpt(i1,ic)=xpt(i,ic)
    loc_wghx(i1,ic)=wghpt(i,ic)
    endif
   end do
   loc_imax(imodx,ic)=i1
  enddo
  !========================
  !========================
  p=imodx
  l=imody
  ip=imodz
  ! Counts particles (e+Z2 ions)

  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,1)*loc_jmax(l,1)*loc_kmax(ip,1)
  nps_loc(3)=nps_loc(3)+&
   loc_imax(p,2)*loc_jmax(l,2)*loc_kmax(ip,2)

 endif
 !------------------------------
 ! Electrons and (Z1+Z3)_ions : central layer with lpx(2) preplasma
 ! x distribution
 !====================
 np1_loc=0.005
 l_inv=log(1./np1_loc)
 do ic=3,5
  n_peak=nptx_loc(ic)  !central target
  do i=1,n_peak
   uu=(real(i,dp)-0.5)/real(n_peak,dp)
   xpt(i,ic)=xfsh+(lpx(2)+lpx(3))*uu
   wghpt(i,ic)=j0_norm
  end do
  if(nxl(2) >0)then
   n_peak=nxl(2)*np_per_xc(ic-2)  !preplasma
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    wghpt(i,ic)=j0_norm*np1_loc*exp(uu*l_inv)
   end do
  endif
 end do
 xfsh=xfsh+lpx(3)+lpx(2)
 !=============================
 ! Charge equilibria mp_per_cell(4)*Z1 +mp_per_cell(5)*Z3= mp_per_cell(1)
 !==========================
 !=========== Distributes on x-MPI tasks
 do ic=3,5
  i1=0
  do i=1,nptx_loc(ic)
   if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
    .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
   i1=i1+1
   loc_xpt(i1,ic)=xpt(i,ic)
   loc_wghx(i1,ic)=wghpt(i,ic)
   endif
  end do
  loc_imax(imodx,ic)=i1
 end do
 p=imodx
 l=imody
 ip=imodz

 nps_loc(1)=nps_loc(1)+&
  loc_imax(p,3)*loc_jmax(l,3)*loc_kmax(ip,3)
 nps_loc(2)=nps_loc(2)+&
  loc_imax(p,4)*loc_jmax(l,4)*loc_kmax(ip,4)
 if(nsp >3)then
  nps_loc(4)=nps_loc(4)+&
   loc_imax(p,5)*loc_jmax(l,5)*loc_kmax(ip,5)
 endif
 !============================
 if(lpx(5) >0.0)then
  do ic=6,7
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    xpt(i,ic)=xfsh+lpx(5)*(real(i,dp)-0.5)/real(n_peak,dp)
   end do
   wghpt(1:n_peak,ic)=np2*j0_norm
  end do
  if(mp_per_cell(5) >0)then
   uu=ratio_mpc(5)   !float(mp_per_cell(1))/float(mp_per_cell(5))
   wghpt(1:nptx_loc(6),6)=wghpt(1:nptx_loc(6),6)*uu
   uu=ratio_mpc(6)/real(ion_min(2),dp)         !float(mp_per_cell(1))/(Z2*float(mp_per_cell(6))
   wghpt(1:nptx_loc(7),7)=wghpt(1:nptx_loc(7),7)*uu
  endif
  xfsh=xfsh+lpx(5)
  !===============================
  do ic=6,7
   i1=0
   do i=1,nptx_loc(ic)
    if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
     .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
    i1=i1+1
    loc_xpt(i1,ic)=xpt(i,ic)
    loc_wghx(i1,ic)=wghpt(i,ic)
    endif
   end do
   loc_imax(imodx,ic)=i1
  end do
  p=imodx
  l=imody
  ip=imodz

  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,6)*loc_jmax(l,6)*loc_kmax(ip,6)
  nps_loc(3)=nps_loc(3)+&
   loc_imax(p,7)*loc_jmax(l,7)*loc_kmax(ip,7)

 endif
 !======================
 npmax=maxval(nps_loc(1:nsp))
 npmax=max(npmax,1)
 call p_alloc(npmax,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
 !===========================
 ip_el=0
 ip_pr=0
 ip_ion=0
 !============
 ! The first electron-H1 layer
 if(nxl(1) >0)then
  p=0
  i2=loc_imax(imodx,1)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,1,i2,ip_el)
  p=0
  i2=loc_imax(imodx,2)
  call pspecies_distribute(spec(3),t0_pl(3),unit_charge(3),p,2,i2,ip_pr)
 endif
 !=========================
 ! The second solid electron-Z1-Z3 layer
 p=ip_el
 i2=loc_imax(imodx,3)
 call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,3,i2,ip_el)
 p=0
 i2=loc_imax(imodx,4)
 call pspecies_distribute(spec(2),t0_pl(2),unit_charge(2),p,4,i2,ip_ion)
 if(nsp >3)then
  p=0
  i2=loc_imax(imodx,5)
  call pspecies_distribute(spec(4),t0_pl(4),unit_charge(4),p,5,i2,ip_ion)
 endif
 !============
 ! The third electron-proton layer
 !=========================
 if(nxl(5) >0.0)then
  p=ip_el
  i2=loc_imax(imodx,6)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,6,i2,ip)
  p=ip_pr
  i2=loc_imax(imodx,7)
  call pspecies_distribute(spec(3),t0_pl(3),unit_charge(3),p,7,i2,ip)
 endif

 do ic=1,nsp
  loc_npart(imody,imodz,imodx,ic)=nps_loc(ic)
 end do

 !============
 end subroutine multisp_target
 !=====================
 subroutine one_layer_nano_wires(nyh,xf0)

 integer,intent(in) :: nyh
 real(dp),intent(in) :: xf0
 integer :: p,ip
 integer :: l,i,i1,i2,ic
 integer :: np_per_zcell(6),n_peak
 integer :: nptx_loc(8)
 integer :: npty_layer(8),npyc(8),npty_ne,nptz_ne
 integer :: npmax,nps_loc(4),nps_bulk
 real(dp) :: uu,yy,dxip,dpy,np1,np2
 real(dp) :: zp_min,zp_max,yp_min,yp_max,xp_min,xp_max
 real(dp) :: xfsh,dlpy,tot_lpy,loc_ymp
 integer :: z2,nxl(6),nyl1,nlpy,nholes
 integer :: ip_ion,ip_el,ip_pr,nwires
 real(dp),allocatable :: wy(:,:),wz(:,:)
 !=================
 !++++++++++++++++ WARNING
 ! ONLY layers (3) n_over_nc, (4) and (5)
 !============================
 xp_min=xmin
 xp_max=xmax
 np_per_zcell(1:6)=1
 nps_bulk=min(3,nsp)        !
 !!+++++++++++++++++++++++++++++++
 nxl=0
 z2=ion_min(nsp-1)
 np1=n1_over_n
 np2=n2_over_n
 if(n_over_nc >0.0)then
  np1=np1/n_over_nc
  np2=np2/n_over_nc
 endif
 !========= gridding the transverese target size
 nyl1=1+ny/2-nyh/2  !=1 if nyh=ny
 yp_min=ymin_t
 yp_max=ymax_t

 dlpy=lpy(1)             !nanowire (y,z) thickness
 tot_lpy=dlpy+lpy(2)     !distance among elements (void+nanowire)`
 nwires=nint((yp_max-yp_min)/tot_lpy)    !numbers of lpy elements
 nlpy=nint(dy_inv*dlpy) ! cell numbers in dlpy
 nholes=nint(dy_inv*lpy(2))! cell number in the lpy(2) interwire region
 if(pe0)then
  write(6,'(a18,i6)')' Nanowires number ',nwires
  write(6,'(a23,i6)')' Grid-points per nanow ',nlpy
 endif
 !=============================
 ! Multispecies
 !=============================
 npty=maxval(np_per_yc(1:6))
 npty=nyh*npty
 nptz=1
 if(ndim==3)then
  np_per_zcell(1:6)=np_per_yc(1:6)
  zp_min=zmin_t    !-Lz
  zp_max=zmax_t    !+Lz
  nptz=maxval(np_per_zc(1:6))
  nptz=nyh*nptz
 endif
 allocate(ypt(npty+1,8))
 allocate(zpt(nptz+1,8))
 allocate(wy(npty+1,8))
 allocate(wz(nptz+1,8))
 ypt=0.
 zpt=0.
 wy=1.
 wz=1.
 !==================
 allocate(loc_jmax(0:npe_yloc-1,1:8))
 allocate(loc_kmax(0:npe_zloc-1,1:8))
 allocate(loc_imax(0:npe_xloc-1,1:8))
 !====================
 ! Layers space ordering(1:4)
 ! [1:2 electon-ions wires,1:2 electron-ion target, 3:4 electron-proton coat]
 !===============
 npyc(1:2)=np_per_yc(1:2)  !layer of nano_wires
 npyc(3:4)=np_per_yc(1:2)  !layer inter wire plasma
 nptz_ne=1
 if(nwires >2)then
  do ic=1,2
   npty_ne=nlpy*npyc(ic)    !number of yp points in a dlpy layer
   i2=0
   loc_ymp=yp_min+lpy(2)
   do i1=1,nwires                     !layers of lpy=dlpy(1+rat) length
    dpy=dlpy/real(npty_ne,dp)
    do i=1,npty_ne
     ypt(i+i2,ic)=loc_ymp+dpy*(real(i,dp)-0.1)
    enddo
    i2=i2+npty_ne
    loc_ymp=loc_ymp+tot_lpy
   end do
   npty_layer(ic)=i2
  end do
  do ic=3,4
   npty_ne=nholes*npyc(ic)    !number of yp points in a lpy(2) layer
   i2=0
   loc_ymp=yp_min
   do i1=1,nwires
    dpy=lpy(2)/real(npty_ne,dp)
    do i=1,npty_ne
     ypt(i+i2,ic)=loc_ymp+dpy*(real(i,dp)-0.1)
    end do
    i2=i2+npty_ne
    loc_ymp=loc_ymp+tot_lpy
   enddo
   npty_layer(ic)=i2
   !===========================
  end do
 else                  !two nanowires filled with n1_over_nc (el+Z1) plasma
  do ic=1,2
   npty_ne=nlpy*npyc(ic)    !number of yp points in a dlpy layer
   i2=0
   loc_ymp= -0.5*tot_lpy
   dpy=dlpy/real(npty_ne,dp)
   do i=1,npty_ne
    ypt(i+i2,ic)=loc_ymp+dpy*(real(i,dp)-0.1)
   enddo
   loc_ymp=loc_ymp+lpy(2)       !first layer
   i2=i2+npty_ne
   !===========================
   do i=1,npty_ne
    ypt(i+i2,ic)=loc_ymp+dpy*(real(i,dp)-0.1)
   enddo
   i2=i2+npty_ne
   !====================
   npty_layer(ic)=i2
  end do
  do ic=3,4
   npty_ne=nholes*npyc(ic)    !number of yp points in a lpy(2) layer
   loc_ymp= -0.5*lpy(2)
   dpy=lpy(2)/real(npty_ne,dp)
   do i=1,npty_ne
    ypt(i,ic)=loc_ymp+dpy*(real(i,dp)-0.1)
   enddo
   npty_layer(ic)=npty_ne
   !===========================
  end do
 endif
 !============= Uniform y-z distribution
 npyc(5:8)=np_per_yc(3:6)  ! bulk target + contaminants
 do ic=5,8
  npty_layer(ic)=nyh*npyc(ic)
  npty_ne=npty_layer(ic)
  dpy=(yp_max-yp_min)/real(npty_ne,dp)
  do i=1,npty_ne
   ypt(i,ic)=yp_min+dpy*(real(i,dp)-0.5)
  enddo
 end do
 !========= For all (y,z) coordinates
 do ic=1,8
  npty_ne=npty_layer(ic)
  if(Stretch)then
   yy=str_ygrid%smin
   if(yy >yp_min)then
    dpy=dyi/real(npyc(ic),dp)
    i1=(str_ygrid%sind(1)-nyl1+1)*npyc(ic)
    i2=npty_ne-i1
    do i=1,i1
     dxip=dpy*(real(i-i1,dp)-0.5)
     ypt(i,ic)=str_ygrid%smin+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
    dxip=dy/real(npyc(ic),dp)
    do i=i1+1,i2
     ypt(i,ic)=str_ygrid%smin+dxip*(real(i-i1,dp)-0.5)
    end do
    do i=i2+1,npty_ne
     dxip=dpy*(real(i-i2,dp)-0.5)
     ypt(i,ic)=str_ygrid%smax+L_s*tan(dxip)
     wy(i,ic)=1./(cos(dxip)*cos(dxip))
    enddo
   endif
  endif
  !============= end stretching correction
  nptz_ne=1
  if(ndim==3)then
   zpt(1:npty_ne,ic)=ypt(1:npty_ne,ic)
   wz(1:npty_ne,ic)=wy(1:npty_ne,ic)
   nptz_ne=npty_ne
  endif
  call set_pgrid_ind(npty_ne,nptz_ne,ic)
 enddo
 !===========================
 xtot=0.0
 lpx(1:2)=0.0
 do i=1,5
  nxl(i)=nint(dx_inv*lpx(i))
  lpx(i)=nxl(i)*dx
  xtot=xtot+lpx(i)
 end do
 xfsh=xf0+lpx(7)
 targ_in=xfsh
 targ_end=targ_in+xtot
 ! Input particles
 !====np_per_xc(1:2) electrons and Z1 ions in the nanowires target => lpx(3)
 !====np_per_xc(3:4) electrons and Z2 ions in bulk layer
 !=== np_per_xc(5:6) electrons and Z3=proton in contaminant layer
 !  Particles grid ordering
 !  only nxl(3) nxl(4) and nxl(5) layers activated
 nptx_loc(1:2)=nxl(3)*np_per_xc(1:2) !inter-wire  electrons+Z1-ion plasma
 nptx_loc(3:4)=nptx_loc(1:2)         !nanowires electron-Z1 ions
 nptx_loc(5:6)=nxl(4)*np_per_xc(3:4) !bulk layer electrons +Z2 ions
 nptx_loc(7:8)=nxl(5)*np_per_xc(5:6) !contaminant electrons +Z3 ions (proton)

 nptx_max=maxval(nptx_loc(1:8))
 !=======================
 allocate(xpt(nptx_max,8))
 allocate(wghpt(nptx_max,8))

 allocate(loc_xpt(nptx_max,8))
 allocate(loc_wghx(nptx_max,8))
 wghpt(1:nptx_max,1:8)=1.
 !================ local y-z part coordinates==========
 loc_npty(1:8)=loc_jmax(imody,1:8)
 loc_nptz(1:8)=loc_kmax(imodz,1:8)
 !=============================
 npty_ne=maxval(loc_npty(1:8))
 nptz_ne=maxval(loc_nptz(1:8))
 if(npty_ne >0)then
  allocate(loc_wghy(npty_ne,8))
  allocate(loc_ypt(npty_ne,8))
  loc_ypt=0.0
  loc_wghy=1.
 endif
 if(nptz_ne >0)then
  allocate(loc_wghz(nptz_ne,8))
  allocate(loc_zpt(nptz_ne,8))
  loc_wghz=1.
  loc_zpt=0.0
 endif
 call mpi_yz_part_distrib(8,loc_npty,loc_nptz,npty_layer,npty_layer,&
  ymin_t,zmin_t,wy,wz)
 !=======================
 !========================
 loc_imax(imodx,1:8)=nptx_loc(1:8)
 nps_loc(1:nsp)=0
 ! Nanowires x-layer: electrons and Z1-ions
 ! nanowires density is the reference density
 if(nxl(3) >0)then
  do ic=1,2
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    xpt(i,ic)=xfsh+lpx(3)*uu
    wghpt(i,ic)=j0_norm
   end do
   !========================= np1>0 a low density  interwire plasma
   do i=1,n_peak
    xpt(i,ic+2)=xpt(i,ic)
    wghpt(i,ic+2)=np1*j0_norm
   end do
  end do
  ic=2
  n_peak=nptx_loc(ic)
  if(mp_per_cell(ic) >0)then
   uu=ratio_mpc(ic)/real(ion_min(1),dp) !float(mp_per_cell(1))/float(mp_per_cell(ic))
   wghpt(1:n_peak,ic)=wghpt(1:n_peak,ic)*uu
   wghpt(1:n_peak,ic+2)=wghpt(1:n_peak,ic+2)*uu
  endif
  xfsh=xfsh+lpx(3)
  !============= first x-layer distributed on locx mpi tasks
  do ic=1,2
   i1=0
   do i=1,nptx_loc(ic)
    if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
     .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
    i1=i1+1
    loc_xpt(i1,ic)=xpt(i,ic)
    loc_wghx(i1,ic)=wghpt(i,ic)
    loc_xpt(i1,ic+2)=xpt(i,ic+2)
    loc_wghx(i1,ic+2)=wghpt(i,ic+2)
    endif
   end do
   loc_imax(imodx,ic)=i1
   loc_imax(imodx,ic+2)=i1
  enddo
  !========================
  !========================
  p=imodx
  l=imody
  ip=imodz
  nps_loc=0
  ! Counts particles

  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,1)*loc_jmax(l,1)*loc_kmax(ip,1)
  nps_loc(2)=nps_loc(2)+&
   loc_imax(p,2)*loc_jmax(l,2)*loc_kmax(ip,2)
  if(np1 >0.0)then
   nps_loc(1)=nps_loc(1)+&
    loc_imax(p,3)*loc_jmax(l,3)*loc_kmax(ip,3)
   nps_loc(2)=nps_loc(2)+&
    loc_imax(p,4)*loc_jmax(l,4)*loc_kmax(ip,4)
  endif
 endif
 !------------------------------
 !  Electrons and Z2_ions: bulk layer  species nps_bulk
 !     x distribution. Density given by the particle density mpc(3:4)
 !====================
 if(nxl(4) >0)then
  do ic=5,6
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    xpt(i,ic)=xfsh+lpx(4)*(real(i,dp)-0.5)/real(n_peak,dp)
    uu=j0_norm*ratio_mpc(ic-2)
    wghpt(i,ic)=uu
   end do
  end do
  ic=6
  n_peak=nptx_loc(ic)
  wghpt(1:n_peak,ic)=wghpt(1:n_peak,ic)/real(ion_min(nps_bulk-1),dp)
 endif
 xfsh=xfsh+lpx(4)
 !  Electrons and Z3_ions contaminants
 !     x distribution density given by np2
 !====================
 if(nxl(5) >0)then
  do ic=7,8
   n_peak=nptx_loc(ic)
   do i=1,n_peak
    xpt(i,ic)=xfsh+lpx(4)*(real(i,dp)-0.5)/real(n_peak,dp)
    uu=j0_norm*ratio_mpc(ic-2)
    wghpt(i,ic)=uu*np2
   end do
  end do
  ic=8
  n_peak=nptx_loc(ic)
  wghpt(1:n_peak,ic)=wghpt(1:n_peak,ic)/real(ion_min(nsp-1),dp)
 endif
 xfsh=xfsh+lpx(5)
 !===============
 do ic=5,8
  i1=0
  do i=1,nptx_loc(ic)
   if(xpt(i,ic)>=loc_xgrid(imodx)%gmin&
    .and.xpt(i,ic)<loc_xgrid(imodx)%gmax)then
   i1=i1+1
   loc_xpt(i1,ic)=xpt(i,ic)
   loc_wghx(i1,ic)=wghpt(i,ic)
   endif
  end do
  loc_imax(imodx,ic)=i1
 end do
 p=imodx
 l=imody
 ip=imodz

 nps_loc(1)=nps_loc(1)+&
  loc_imax(p,5)*loc_jmax(l,5)*loc_kmax(ip,5)
 nps_loc(nps_bulk)=nps_loc(nps_bulk)+&
  loc_imax(p,6)*loc_jmax(l,6)*loc_kmax(ip,6)
 if(nsp==4)then   !contaminants added
  nps_loc(1)=nps_loc(1)+&
   loc_imax(p,7)*loc_jmax(l,7)*loc_kmax(ip,7)
  nps_loc(nsp)=nps_loc(nsp)+&
   loc_imax(p,8)*loc_jmax(l,8)*loc_kmax(ip,8)
 endif
 !==============
 npmax=maxval(nps_loc(1:nsp))
 npmax=max(npmax,1)
 call p_alloc(npmax,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
 !===========================
 ip_el=0
 ip_pr=0
 ip_ion=0
 ! The first electron-Z1-ions nanowires layer
 if(nxl(3) >0)then
  p=0
  i2=loc_imax(imodx,1)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,1,i2,ip_el)
  p=0
  i2=loc_imax(imodx,2)
  call pspecies_distribute(spec(2),t0_pl(2),unit_charge(2),p,2,i2,ip_ion)
  if(np1 >0.0)then
   p=ip_el
   i2=loc_imax(imodx,3)
   call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,3,i2,ip_el)
   p=ip_ion
   i2=loc_imax(imodx,4)
   call pspecies_distribute(spec(2),t0_pl(2),unit_charge(2),p,4,i2,ip_ion)
  endif
 endif
 !=========================
 ! The second electron-ion solid layer with Z2 A2 ion element
 if(nxl(4) >0)then
  p=ip_el
  i2=loc_imax(imodx,5)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,5,i2,ip_el)

  p=0
  i2=loc_imax(imodx,6)
  call pspecies_distribute(spec(nps_bulk),t0_pl(nps_bulk),unit_charge(nps_bulk),p,6,i2,ip_ion)
 endif
 !============
 ! The contaminant electron-ion solid layer Z3=proton ion element
 if(nxl(5) >0)then
  p=ip_el
  i2=loc_imax(imodx,7)
  call pspecies_distribute(spec(1),t0_pl(1),unit_charge(1),p,7,i2,ip_el)

  p=0
  i2=loc_imax(imodx,8)
  call pspecies_distribute(spec(nsp),t0_pl(nsp),unit_charge(nsp),p,8,i2,ip_ion)
 endif
 do ic=1,nsp
  loc_npart(imody,imodz,imodx,ic)=nps_loc(ic)
 end do

 end subroutine one_layer_nano_wires
 !============
 subroutine one_layer_nano_tubes(nyh,xf0)

 integer,intent(in) :: nyh
 real(dp),intent(in) :: xf0
 integer :: p
 integer :: i,j,i1,i2,ic,k1,k2
 integer :: np_per_zcell(2),n_peak,ntubes
 integer :: npty_ne,nptz_ne
 integer :: npmax,nps_loc(2)
 real(dp) :: uu,dpy,dlpy,rat,whp
 real(dp) :: zp_min,zp_max,yp_min,yp_max,xp_min,xp_max
 real(dp) :: loc_ym,loc_ymx,loc_zm,loc_zmx
 real(dp) :: xfsh,r_int,r_ext
 integer :: nxl(5),loc_nptx(4),npt_nano(4)
 integer :: nlpy,loc_npty(6)
 integer :: npty_layer(2),nptz_layer(2)
 real(dp),allocatable :: yc(:),wy(:,:),wz(:,:),ypt_nano(:,:),zpt_nano(:,:)
 real(dp),allocatable :: locy_nano(:,:),locz_nano(:,:)
 real(sp) :: ch(2)
 equivalence(whp,ch)
 !=================
 xp_min=xmin
 xp_max=xmax
 np_per_zcell(1:2)=1
 !!+++++++++++++++++++++++++++++++
 nxl=0
 !========= gridding the transverese target size
 yp_min=ymin_t
 yp_max=ymax_t
 !===============================
 ! Geometry |--s/2--|======v=====|---s/2--|
 ! total size L=s+v=s*(1+v/s)=s(1+rat)
 ! Filling factor f=(1-(v/L)^2)
 !===========================
 ! Two-species Electrons + Z ions
 !=============================

 npty=maxval(np_per_yc(1:2))
 npty=nyh*npty             !particles number in 3 nlpy slabs
 nptz=1
 if(ndim==3)then
  np_per_zcell(1:2)=np_per_zc(1:2)
  zp_min=yp_min    !-Lz
  zp_max=yp_max    !+Lz
  nptz=maxval(np_per_zc(1:6))
  nptz=nyh*nptz
 endif
 allocate(ypt(npty,2))
 allocate(wy(npty,2))
 allocate(zpt(nptz,2))
 allocate(wz(nptz,2))
 wy=1.
 wz=1.
 !==================
 allocate(loc_jmax(0:npe_yloc-1,1:2))
 allocate(loc_kmax(0:npe_zloc-1,1:2))
 !====================
 ! Uniform yp grid of size npty_ne
 do ic=1,2
  npty_ne=nyh*np_per_yc(ic) !number of yp points in 2*ymax size
  dpy=(yp_max-yp_min)/real(npty_ne,dp)
  do i=1,npty_ne
   ypt(i,ic)=yp_min+dpy*(real(i,dp)-0.5)
  end do
  npty_layer(ic)=npty_ne
  nptz_layer(ic)=1
  if(ndim==3)then
   i2=npty_ne
   zpt(1:i2,ic)=ypt(1:i2,ic)
   wz(1:i2,ic)=wy(1:i2,ic)
   nptz_layer(ic)=i2
  endif
  call set_pgrid_ind(npty_layer(ic),nptz_layer(ic),ic)
 end do
 !===========================
 ! Layer lpx(3) for nanotubes lpx(4) for target
 xtot=0.0
 do i=3,4
  nxl(i)=nint(dx_inv*lpx(i))
  lpx(i)=nxl(i)*dx
  xtot=xtot+lpx(i)
 end do
 xfsh=xf0+lpx(7)
 targ_in=xfsh
 targ_end=targ_in+xtot
 !=======================
 ! Only layers 3 and 4 in x
 loc_nptx(1:2)=nxl(3)*np_per_xc(1:2)
 loc_nptx(3:4)=nxl(4)*np_per_xc(3:4)
 nptx_max=loc_nptx(1)+loc_nptx(3)

 allocate(xpt(nptx_max,nsp))
 allocate(loc_xpt(nptx_max,nsp))
 allocate(wghpt(nptx_max,nsp))
 !======================================
 ! Uses the yp,zp=yp for ic=1,2 uniform p-grid
 ! to select a three-layer array of circular nanotubes
 !===============
 dlpy=lpy(1)   !2*dr  lpy(2)=2*r_int
 nlpy=nint(dy_inv*dlpy) ! cell numbers in [dlpy layer]
 rat=lpy(2)/dlpy
 r_int=0.5*lpy(2)
 r_ext=r_int+0.5*dlpy

 uu=(yp_max-yp_min-0.5*dlpy)/(lpy(2)+1.5*dlpy)
 ntubes=nint(uu)
 allocate(yc(ntubes))
 !=========================
 yc(1)=yp_min+0.5*dlpy+r_ext
 do ic=2,ntubes
  yc(ic)=yc(ic-1)+lpy(2)+1.5*dlpy
 end do
 !========= filling factor
 lpy(3)=acos(-1.)*(r_ext*r_ext-r_int*r_int)/(lpy(2)+1.5*dlpy)**2
 !=============================
 do ic=1,2
  npty_ne=nyh*np_per_yc(ic)    !number of yp points in Ly=2*ymax size
  nptz_ne=nyh*np_per_zc(ic)    !number of zp points in Lz=2*zmax size
  npt_nano(ic)=0
  do k1=1,ntubes
   do k2=1,ntubes
    do i=1,nptz_ne
     do j=1,npty_ne
      dpy=sqrt((ypt(j,ic)-yc(k2))**2+(zpt(i,ic)-yc(k1))**2)
      if(dpy >= r_int.and.dpy < r_ext)npt_nano(ic)=npt_nano(ic)+1
     end do
    end do
   end do
  end do
 end do
 ! npt_nano(ic) nanotubes section
 !====================
 npty_ne=npt_nano(1)
 allocate(ypt_nano(npty_ne,nsp),zpt_nano(npty_ne,nsp))
 loc_ym=loc_ygrid(imody)%gmin
 loc_ymx=loc_ygrid(imody)%gmax
 loc_zm=loc_zgrid(imodz)%gmin
 loc_zmx=loc_zgrid(imodz)%gmax
 do ic=1,2
  npty_ne=nyh*np_per_yc(ic)    !number of yp points in 2*ymax size
  nptz_ne=nyh*np_per_zc(ic)    !number of yp points in 2*ymax size
  i2=0
  do k1=1,ntubes
   do k2=1,ntubes
    do i=1,nptz_ne
     do j=1,npty_ne
      dpy=sqrt((ypt(j,ic)-yc(k2))**2+(zpt(i,ic)-yc(k1))**2)
      if(dpy >= r_int.and.dpy < r_ext)then
       i2=i2+1
       ypt_nano(i2,ic)=ypt(j,ic)
       zpt_nano(i2,ic)=zpt(i,ic)
      endif
     end do
    end do
   end do
  end do
  loc_npty(ic)=0
  do i=1,i2
   uu=ypt_nano(i,ic)
   if(uu >= loc_ym.and.uu< loc_ymx)then
    uu=zpt_nano(i,ic)
    if(uu >= loc_zm.and.uu< loc_zmx)loc_npty(ic)=loc_npty(ic)+1
   endif
  end do
 end do
 npty_ne=loc_npty(1)
 nptz_ne=npty_ne
 !==========================
 if(npty_ne >0)then
  allocate(locy_nano(npty_ne,2))
  allocate(locz_nano(nptz_ne,2))
 endif
 !==========================
 !========== Nanotubes layer
 do ic=1,2
  if(loc_nptx(ic) >0)then
   k1=0
   do i=1,npt_nano(ic)
    uu=ypt_nano(i,ic)
    if(uu >= loc_ym.and.uu< loc_ymx)then
     uu=zpt_nano(i,ic)
     if(uu >= loc_zm.and.uu< loc_zmx)then
      k1=k1+1
      locy_nano(k1,ic)=ypt_nano(i,ic)
      locz_nano(k1,ic)=zpt_nano(i,ic)
     endif
    endif
   end do
   loc_npty(ic)=k1
  endif
 end do
 !============================ Flat target
 !====================================
 npty_ne=maxval(npty_layer(1:nsp))
 if(npty_ne >0)then
  allocate(loc_wghy(npty_ne,nsp))
  allocate(loc_ypt(npty_ne,nsp))
  loc_ypt=0.0
  loc_wghy=1.
  allocate(loc_wghz(nptz_ne,nsp))
  allocate(loc_zpt(nptz_ne,nsp))
  loc_zpt=0.0
  loc_wghz=1.
 endif
 !============================= Uniform target
 call mpi_yz_part_distrib(2,loc_npty,loc_nptz,npty_layer,nptz_layer,&
  ymin_t,zmin_t,wy,wz)
 !==========================
 nptx(1:nsp)=0
 !========================
 do ic=1,nsp
  i1=nptx(ic)
  n_peak=nxl(3)*np_per_xc(ic)
  do i=1,n_peak
   i1=i1+1
   uu=(real(i,dp)-0.5)/real(n_peak,dp)
   xpt(i1,ic)=xfsh+lpx(3)*uu
   wghpt(i1,ic)=j0_norm
   uu=ratio_mpc(ic)/real(ion_min(1),dp) !float(mp_per_cell(1))/float(mp_per_cell(ic))
   if(ic==2)wghpt(i1,ic)=wghpt(i1,ic)*uu
   loc_xpt(i1,ic)=xpt(i1,ic)
  end do
  nptx(ic)=i1
 end do
 xfsh=xfsh+lpx(3)
 if(nxl(4)>0)then            !a bulk
  do ic=1,nsp
   i1=nptx(ic)
   n_peak=nxl(4)*np_per_xc(ic)
   do i=1,n_peak
    i1=i1+1
    uu=(real(i,dp)-0.5)/real(n_peak,dp)
    xpt(i1,ic)=xfsh+lpx(4)*uu
    wghpt(i1,ic)=j0_norm
    loc_xpt(i,ic)=xpt(i1,ic)
   end do
   nptx(ic)=i1
  end do
  xfsh=xfsh+lpx(4)
 endif
 !=================== on index ic=2 are ions with charge Z_i=npc_e/npc_i
 !======================
 do ic=1,nsp
  j=nptx(ic)
  if(xpt(j,ic)> xmax)then
   p=0
   do i=1,nptx_max
    if(xpt(i,ic)<=xmax)p=i !inside the box xpt[1:nptx(ic)]
   enddo
   nptx(ic)=p
  endif
 end do
 !============ count partuckes of nano-tubes
 do ic=1,nsp
  nps_loc(ic)=0
  i2=loc_nptx(ic)
  do k1=1,loc_npty(ic)
   do i=1,i2
    nps_loc(ic)=nps_loc(ic)+1
   end do
  end do
  if(nptx(ic)> i2)then
   do i1=1,loc_kmax(imodz,ic)
    do k1=1,loc_jmax(imody,ic)
     do i=i2+1,nptx(ic)
      nps_loc(ic)=nps_loc(ic)+1
     end do
    end do
   end do
  endif
 enddo
 loc_npart(imody,imodz,imodx,1:nsp)=nps_loc(1:nsp)
 npmax=maxval(nps_loc(1:nsp))
 npmax=max(npmax,1)
 call p_alloc(npmax,nd2+1,nps_loc,nsp,LPf_ord,1,1,mem_psize)
 !===========================
 call init_random_seed(mype)
 !============
 do ic=1,nsp
  i2=loc_nptx(ic)
  ch(2)=real(unit_charge(ic),sp)
  p=0
  do k1=1,loc_npty(ic)
   do i=1,i2
    ch(1)=real(wghpt(i,ic),sp)
    p=p+1
    spec(ic)%part(p,1)=xpt(i,ic)
    spec(ic)%part(p,2)=locy_nano(k1,ic)
    spec(ic)%part(p,3)=locz_nano(k1,ic)
    call gasdev(uu)
    spec(ic)%part(p,4)=t0_pl(ic)*uu
    call gasdev(uu)
    spec(ic)%part(p,5)=t0_pl(ic)*uu
    call gasdev(uu)
    spec(ic)%part(p,6)=t0_pl(ic)*uu
    spec(ic)%part(p,7)=whp
   end do
  end do
  if(nptx(ic)> loc_nptx(ic))then
   i2=nptx(ic)+1-loc_nptx(ic)
   call pspecies_distribute(spec(ic),t0_pl(ic),unit_charge(ic),p,ic,i2,i1)
  endif
 end do
 !============
 end subroutine one_layer_nano_tubes
 !==============================
 !====================================
 subroutine part_distribute(id,xf0)
 integer,intent(in) :: id
 real(dp),intent(in) :: xf0
 integer :: ip,pp,l,p
 integer :: tot_nploc(npe)
 !=================
 if(Wake)then
  !nps_run =1
  !if nsp > 1 ions active only for ionization
  call multi_layer_gas_target(id,ny_targ,xf0)
  !======================
  !id=1
  !lpx(1) first plateau np1 density
  !ramp lpx(2)+ plateau lpx(3) + downramp lpx(4)] density 1
  !lpx(5) last plateau np2 density
  !all densities normalized to n_0=n_over_nc
  !====================================
  !id=2  target with two central plateau (for shocked gas-jet)
  !lpx(1) first ramp up to lpx(2) first plateau density np1
  !lpx(3) downramp to
  !lpx(4) second plateau density np2 and to final downramp lpx(5)
  !n_0=n_over_nc can be an average, or n1_over_nc or n2_over_nc
  !====================================
  !id=3 as id=2, but multispecies is allowed:
  ! The layer 1):
  ! of length [lpx(1) a ramp +lpx(2) a plateau] contain a dopant (A1,Z1) with
  ! density n_ion=w_ion*mp_per_cell(2), background (H+) electrons with density
  ! n_0=w_el*mp_per_cell(1) and Z1 electrons of ions. The total electron density
  ! is then:
  ! n_e=n_0+Z1*n_ion=w_el*(mp_per_cell(1)+Z1*mp_per_cell(2)), and normalized
  ! density:
  ! n_e/n_0=(1+Z1*mp_per_cell(2)/mp_per_cell(2))
  !----------
  ! Layer 3) of length [lpx(4) a plateau lpx(5) a downramp] has density n_e=n_0
  !------------------
  ! layer 2) of length lpx(3) is a transition between layer 1) and 3).
  !
  ! In layer 2):
  ![lpx(4) a plateau lpx(5) a downramp] contains only electrons with reference density n_e
  ! (n_e/n_0=1)
  !===================================
  !id=4 as  id=1 along the x-coordinate but (y-z profile is a matched parabolic
  !channel defined by lpy(1) and lpy(2) scale length parameters.
  !======================================

 else
  !SOLID MULTISPECIES TARGETS
  !flag id=1,2 allowed not even implemented
  select case(id)
  case(3)
   call preplasma_multisp(ny_targ,xf0)
   ! (e+Z1) preplasma and central target
   !+ (e+Z2)coating
  case(4)
   call multi_layer_multisp(ny_targ,xf0)
   !(e+Z2) foam
   !(e+Z1) central layer
   !(e+Z2)coating
   !============warning exponential ramp (in layer 2) always using (e+Z1) species
  case(5)
   call multisp_target(ny_targ,xf0)
   !(e+Z2) coating
   !(e+Z1+Z3) central layer with lpx(2) preplasma
   !+ (e+Z2)coating
  case(6)
   call one_layer_nano_wires(ny_targ,xf0)
   !e+Z1 wires, e+Z2 bulk. interwire low density (e+Z1) plasma allowed
  case(7)
   call one_layer_nano_tubes(ny_targ,xf0)
  end select
 endif
 !===================Data for all models===============
 tot_nploc=0
 pp=0
 do p=0,npe_xloc-1
  do ip=0,npe_zloc-1
   do l=0,npe_yloc-1
    pp=pp+1
    tot_nploc(pp)=sum(loc_npart(l,ip,p,1:nsp))
   end do
  end do
 end do
 np_max=maxval(tot_nploc(1:npe))
 np_min=minval(tot_nploc(1:npe))
 do ip=1,npe
  if(tot_nploc(ip)==np_max)pe_npmax=ip-1
  if(tot_nploc(ip)==np_min)pe_npmin=ip-1
 end do
 !===============
 end subroutine part_distribute
 !-------------------
 subroutine clean_field(ef,lp1,i1,j1,j2,k1,k2,nc)
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: lp1
 integer,intent(in) :: i1,j1,j2,k1,k2,nc
 integer :: ilp,i,j,k,ic

 ilp=int(dx_inv*lp1)
 do ic=1,nc
  do k=k1,k2
   do j=j1,j2
    do i=i1,ilp
     ef(i,j,k,ic)=0.0
    end do
   end do
  end do
 end do

 end subroutine clean_field

 !========================
 subroutine LP_pulse(lp_mod)

 integer,intent(in) :: lp_mod
 integer :: i1,i2,j1,j2,k1,k2,lp_ind
 real(dp) :: angle,shx_lp,sigm,eps,xm,tt,tau,lp_end

 lp_amp=oml*a0
 alp_HM=2.*acos(sqrt(0.5*sqrt(2.0)))/pi
 lp_xsize=w0_x
 lx_FWHM=alp_HM*w0_x     !The FWHM in microns
 tau_FWHM=10.*lx_FWHM/3. !The FWHM in fs
 !===========================
 ! field grid index defined on set_pgrid

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 xm=loc_xgrid(imodx)%gmin  ! => field index i1
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)

 tt=0.0
 tau=0.5*w0_x
 lp_in=xc_lp-tau     !centroid xc_lp=xf-ts at time t=0 lp_in=xc-tau
 lp_end=lp_in+w0_x
 eps=1./(oml*w0_y)
 sigm=lam0/w0_x
 angle=lpx(6)
 xf=xc_lp+t0_lp
 if(lp_amp >0.0)then
  !=======================
  lp_ind=lp_mod
  shx_lp=0.0
  if(angle >0.0)shx_lp=lpx(7)
  if(Plane_wave)lp_ind=0
  call init_lp_fields(ebf,lp_amp,tt,t0_lp,tau,w0_y,xf,&
   angle,shx_lp,lp_ind,i1,i2)
  !call divA(ebf,jc,i1,i2,j1,j2,k1,k2,1,1,dx_inv,dy_inv,dz_inv)
  !+++++++++++++++
  !  Holds a Plane wave pulse (lp_mod=0) and LP Gaussian pulses
  !=======================
  if(pe0)then
   write(6,*)'LP injected '
   write(6,'(a21,e11.4)')' Focal x-position xf=',xf
   write(6,'(a21,e11.4)')' Centr x-position xc=',xc_lp
   write(6,'(a20,e11.4)')' FWHM (microns) lsz=',lx_FWHM
   write(6,'(a14,e11.4)')' FWHM (fs) lx=',tau_FWHM
   write(6,'(a23,e11.4)')' Max Ey field(TV/m)  = ',E0*lp_amp
   if(Plane_wave)then
    write(6,'(a12)')' Plane wave '
    if(angle >0.0)write(6,'(a17,e11.4)')' Polarization angle ',angle
   else
    if(lp_mod==1)  write(6,'(a16)')' P Polarization '
    if(lp_mod==2)  write(6,'(a16)')' S Polarization '
    write(6,'(a19,e11.4)')' Waist(microns) ly=',w0_y
    write(6,'(a17,e11.4)')' FWHM(microns) = ',lp_rad
    if(angle >0.0)write(6,'(a17,e11.4)')' Incidence angle ',angle
   endif
  endif
 else
  lp_end=xm
  if(pe0)write(6,*)' NO INJECTED LASER PULSE'
 endif
 !==================== inject particles
 if(Part)then
  call part_distribute(dmodel_id,lp_end)
 endif
 !=================def part distr points
 end subroutine LP_pulse
 !===========================
 subroutine CP_pulse(cp_mod)

 integer,intent(in) :: cp_mod
 integer :: i1,i2,cp_ind
 real(dp) :: angle,shx_cp,eps,sigm,xm,tau,tt

 lp_amp=oml*a0/sqrt(2.)
 alp_HM=2.*acos(sqrt(0.5*sqrt(2.0)))/pi
 lp_xsize=w0_x
 lx_FWHM=alp_HM*w0_x     !The FWHM in microns
 tau_FWHM=10.*lx_FWHM/3. !The FWHM in fs
 !-------------------------
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 xm=loc_xgrid(imodx)%gmin  ! => field index i1
 tt=0.0
 tau=0.5*w0_x
 lp_in=xc_lp-tau          !LP centroid xc_lp=xf-ts at time t=0 lp_in=xc-tau
 lp_end=lp_in+w0_x
 eps=1./(oml*w0_y)
 sigm=lam0/w0_x
 angle=lpx(6)
 xf=xc_lp+t0_lp
 shx_cp=0.0
 if(angle >0.0)shx_cp=lpx(7)
 if(lp_amp >0.0)then
  cp_ind=cp_mod
  if(Plane_wave)cp_ind=0
  !=======================
  call init_cp_fields(ebf,lp_amp,tt,t0_lp,tau,w0_y,xf,&
   angle,shx_cp,cp_ind,i1,i2)
  !=================def part distr points
  if(pe0)then
   write(6,*)'CP injected '
   write(6,'(a21,e11.4)')' Focal x-position x0=',xf
   write(6,'(a21,e11.4)')' Pulse centroid xc = ',xc_lp
   write(6,'(a20,e11.4)')' Size (microns) lsz=',lp_xsize
   write(6,'(a14,e11.4)')' FWHM (fs) lx=',tau_FWHM
   write(6,'(a23,e11.4)')' Max Ey field(TV/m)  = ',E0*lp_amp
   write(6,'(a17,2e11.4)')'pulse x_in x_end ',lp_in,lp_end
   if(Plane_wave)then
    write(6,*)' Plane wave  '
   else
    write(6,'(a23,2e11.4)')'Model eps-sigma param. ',eps,sigm
    write(6,'(a19,e11.4)')' Waist(microns) ly=',w0_y
    write(6,'(a17,e11.4)')' FWHM(microns) = ',lp_rad
   endif
  endif
 else
  lp_end=xm
  if(pe0)write(6,*)'NO  CP laser pulse injected'
 endif
 if(Part)then
  call part_distribute(dmodel_id,lp_end)
 endif
 end subroutine CP_pulse
 !-------------------------
 subroutine set_envelope(ts)

 real(dp),intent(in) :: ts
 integer :: nyp,nzp,i1,i2,j1,k1
 real(dp) :: eps,sigm,xm,ym,zm,tt,tau

 lp_amp=a0
 alp_HM=2.*acos(sqrt(0.5*sqrt(2.0)))/pi
 lp_xsize=w0_x
 lx_FWHM=alp_HM*w0_x     !The FWHM in microns
 tau_FWHM=10.*lx_FWHM/3. !The FWHM in fs
 !===========================
 ! field grid index defined on set_pgrid

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 xm=loc_xgrid(imodx)%gmin
 !=========================
 tt=0.0
 tau=0.5*w0_x
 lp_in=xc_lp-tau     !centroid xc_lp=xf-ts at time t=0 lp_in=xc-tau
 lp_end=lp_in+w0_x
 ilp_in=nint(dx_inv*lp_in)-8
 ilp_end=nint(dx_inv*lp_end)+2
 eps=1./(oml*w0_y)
 sigm=lam0/w0_x
 !=======================
 if(lp_end >xm)then
  call init_envelope_field(&
   env,lp_amp,dt,tt,ts,tau,w0_y,xf,i1,i2)
 endif
 !==========================
 ebf=0.0
 !=====================
 if(Part)then
  call part_distribute(dmodel_id,lp_end)
 endif
 if(pe0)then
  write(6,*)'LP envelope injected '
  write(6,'(a21,e11.4)')' Focal x-position xf=',xf
  write(6,'(a21,e11.4)')' Centr x-position xc=',xc_lp
  write(6,'(a20,e11.4)')' Size (microns) lsz=',lp_xsize
  write(6,'(a14,e11.4)')' FWHM (fs) lx=',tau_FWHM
  write(6,'(a19,e11.4)')' Waist(microns) ly=',w0_y
  write(6,'(a17,e11.4)')' FWHM(microns) = ',lp_rad
 endif
 !=================def part distr points
 end subroutine set_envelope
 !=========================
 subroutine beam_data(ic,btype,np_tot)  !generates bpart(7,np_tot)
 integer,intent(in) :: ic,btype
 integer,intent(out) :: np_tot
 integer :: i,i1,i2,ip,ppcb,ix,iy,iz
 integer(dp_int) :: effecitve_cell_number
 real(dp) :: cut,xh(5),bch
 real(sp) :: ch(2)
 equivalence(bch,ch)
 integer :: nch
 logical :: sr
 !==========================================================
 ! bconf defines only the configuration of bunch charges
 !=======================
 ! default values
 !=======================
 np_tot=0
 do i=1,nsb
   if(ppc_bunch(i)>0 .and. nb_tot(i)==-1) then
      effecitve_cell_number=bunch_volume_incellnumber(bunch_shape(i),sxb(i),syb(i),syb(i),dx,dy,dz)
      nb_tot(i)=ppc_bunch(i)*effecitve_cell_number
      if(pe0) write(*,'(A,1I1,A)') 'bunch(',i,') :: weighted :: option'
      if(pe0) write(*,'(A,1I1,A)') 'bunch(',i,') :: changing total number of particles :: equal number of ppc'
      if(pe0) write(*,'(A,1I1,A,1I3,A,1I10)') 'bunch(',i,') :: ppc =',ppc_bunch(i),' :: total number of bunch particles =',nb_tot(i)
   endif
   if(ppc_bunch(i)==-1 .and. nb_tot(i)>0) then
       if(pe0) write(*,'(A,1I1,A)') 'bunch(',i,') :: equal :: option (all particle have the same weight)'
   endif
  np_tot=np_tot+nb_tot(i)
 end do
 cut=3.
 nch=7
 if(ic==2)nch=5
 allocate(bpart(nch,np_tot))
 select case(btype)
 case(1)
  !---!
  i1=1
  ch(2)=real(unit_charge(1),sp)      !nsb electron bunches
  do ip=1,nsb
   xh(ip)=xc_bunch(ip)
   ch(1)=real(j0_norm*jb_norm(ip),sp) !the bunch particles weights
   i2=i1+nb_tot(ip)-1

   if(bunch_shape(ip)==1 .and. ppc_bunch(i)>0) & !weighted-option
                          call generate_bunch_bigaussian_weighted(i1,i2,&
                               sxb(ip),xc_bunch(ip),&
                               syb(ip),yc_bunch(ip),&
                               syb(ip),zc_bunch(ip),&
                               gam(ip),&
                      epsy(ip),epsz(ip),cut,dg(ip),bpart,bch,dx,dy,dz,rhob(ip))
   if(bunch_shape(ip)==1 .and. ppc_bunch(i)==-1) & !equal-weight
                          call generate_bunch_bigaussian_equal(i1,i2,&
                               sxb(ip),xc_bunch(ip),&
                               syb(ip),yc_bunch(ip),&
                               syb(ip),zc_bunch(ip),&
                               gam(ip),&
                      epsy(ip),epsz(ip),cut,dg(ip),bpart,bch,dx,dy,dz,rhob(ip))

   if(bunch_shape(ip)==2 .and. ppc_bunch(i)>0) & !weighted-option
                          call generate_bunch_triangularZ_uniformR_weighted(i1,i2,&
                               xc_bunch(ip),yc_bunch(ip),zc_bunch(ip),&
                               sxb(ip),syb(ip),syb(ip),gam(ip),&
                               epsy(ip),epsz(ip),dg(ip),&
                               bpart,Charge_right(ip),Charge_left(ip),bch,dx,dy,dz)
   if(bunch_shape(ip)==2 .and. ppc_bunch(i)==-1) & !equal-weight
                           call generate_bunch_triangularZ_uniformR_equal(i1,i2,&
                                xc_bunch(ip),yc_bunch(ip),zc_bunch(ip),&
                                sxb(ip),syb(ip),syb(ip),gam(ip),&
                                epsy(ip),epsz(ip),dg(ip),&
                                bpart,Charge_right(ip),Charge_left(ip),bch)


   if(bunch_shape(ip)==3 .and. ppc_bunch(i)>0) & !weighted-option
                           call generate_bunch_triangularZ_normalR_weighted(i1,i2,&
                                xc_bunch(ip),yc_bunch(ip),zc_bunch(ip),&
                                sxb(ip),syb(ip),syb(ip),&
                                gam(ip),epsy(ip),epsz(ip),dg(ip),&
                                bpart,Charge_right(ip),Charge_left(ip),bch,dx,dy,dz)
   if(bunch_shape(ip)==3 .and. ppc_bunch(i)==-1) & !equal-weight
                           call generate_bunch_triangularZ_normalR_equal(i1,i2,&
                                xc_bunch(ip),yc_bunch(ip),zc_bunch(ip),&
                                sxb(ip),syb(ip),syb(ip),&
                                gam(ip),epsy(ip),epsz(ip),dg(ip),&
                                bpart,Charge_right(ip),Charge_left(ip),bch)


    !--- Twiss Rotation ---!
    if(L_TWISS(ip)) call bunch_twissrotation(i1,i2,bpart, &
      alpha_twiss(ip),beta_twiss(ip),alpha_twiss(ip),beta_twiss(ip), &
      syb(ip),syb(ip),epsy(ip),epsz(ip),xc_bunch(ip),yc_bunch(ip),zc_bunch(ip))

  !  if(L_TWISS .and. bunch_shape(ip)==1)  call bunch_gen_twissshifting(i1,i2,&
  !   sxb(ip),xc_bunch(ip),&
  !   syb(ip),yc_bunch(ip),&
  !   syb(ip),zc_bunch(ip),&
  !   gam(ip),&
  !   epsy(ip),epsz(ip),dg(ip),bpart,bch,&
  !   alpha_twiss(ip),beta_twiss(ip))
  !  if(.not. L_TWISS .and. bunch_shape(ip)==1)  call bunch_gen_alternative(i1,i2,&
  !   sxb(ip),xc_bunch(ip),&
  !   syb(ip),yc_bunch(ip),&
  !   syb(ip),zc_bunch(ip),&
  !   gam(ip),&
  !   epsy(ip),epsz(ip),cut,dg(ip),bpart,bch,dx,dy,dz,rhob(ip))
  !  if(.not. L_TWISS .and. bunch_shape(ip)==2) call generate_triangularZ_uniformR_bunch(i1,i2,&
  !   xc_bunch(ip),yc_bunch(ip),zc_bunch(ip),sxb(ip),syb(ip),syb(ip),gam(ip),epsy(ip),epsz(ip),dg(ip),&
  !   bpart,Charge_right(ip),Charge_left(ip),bch,dx,dy,dz)
  !  if(.not. L_TWISS .and. bunch_shape(ip)==3) call generate_triangularZ_normalR_bunch(i1,i2,&
  !   xc_bunch(ip),yc_bunch(ip),zc_bunch(ip),sxb(ip),syb(ip),syb(ip),gam(ip),epsy(ip),epsz(ip),dg(ip),&
  !   bpart,Charge_right(ip),Charge_left(ip),bch,dx,dy,dz)
  !  if(.not. L_TWISS .and. bunch_shape(ip)==4) call generate_cylindrical_bunch(i1,i2,&
  !   xc_bunch(ip),yc_bunch(ip),zc_bunch(ip),sxb(ip),syb(ip),syb(ip),gam(ip),epsy(ip),epsz(ip),dg(ip),&
  !   bpart,Charge_right(ip),Charge_left(ip),bch)
   !---end simplified version---!
   i1=i2+1
  end do

 case(2)         !x-uniform proton bunch on length sx(1)
  i1=1
  ch(2)= 1.           !one proton  bunch starting at xc_bunch
  ip=1
  xh(ip)=xc_bunch(ip)
  ch(1)=real(jb_norm(ip),sp) !the bunch particles weights
  i2=i1+nb_tot(ip)-1
  !========= a iproton beam starting at xc_bunc(1) of length sxb(ip)
  call pbunch_gen(2,i1,i2,sxb(ip),syb(ip),syb(ip),&
   epsy(ip),epsz(ip),bet0,bpart)
  do i=i1,i2
   bpart(1,i)=bpart(1,i)+xh(ip)
   bpart(nch,i)=bch
  end do
 end select

 ! Pe0 p data are copied to all MPI tasks
 if(pe0)then
  sr=.true.
  do ip=1,npe-1
   call exchange_2d_grdata(sr,bpart,nch,np_tot,ip,ip+10)
  end do
 else
  sr=.false.
  call exchange_2d_grdata(sr,bpart,nch,np_tot,0,mype+10)
 endif
 !==============================
 end subroutine beam_data
 !===================
 subroutine MPI_beam_distribute

 integer :: i,ii,i1,j
 integer :: ic,p,ip,ipp,nb_loc
 real(dp) :: y1,y2,z1,z2,x1,x2
 integer :: nps_loc(nsb),npmax,np_tot
 !========= count particles on each (yz) MPI domain
 np_tot=sum(nb_tot(1:nsb))
 ! ALL MPI tasks do
 if(model_id==5)then
  x1=loc_xgrid(imodx)%gmin
  x2=loc_xgrid(imodx)%gmax
 else
  x1=xc_bunch(1)       !the bunch starting x-position
  x2=x(nx)            !the beam max position
 endif
 i1=0
 do ic=1,nsb
  do p=0,npe_xloc-1
   do ip=0,npe_zloc-1
    z1=loc_zgrid(ip)%gmin
    z2=loc_zgrid(ip)%gmax
    do ipp=0,npe_yloc-1
     y1=loc_ygrid(ipp)%gmin
     y2=loc_ygrid(ipp)%gmax
     loc_nbpart(ipp,ip,p,ic)=0
     do j=1,nb_tot(ic)
      i=i1+j
      if(bpart(2,i) >y1.and.bpart(2,i) <=y2)then
       if(bpart(3,i) >z1.and.bpart(3,i) <=z2)then
        if(bpart(1,i) >x1.and.bpart(1,i) <=x2)then
         loc_nbpart(ipp,ip,p,ic)=loc_nbpart(ipp,ip,p,ic)+1
        endif
       endif
      endif
     enddo
    end do
   end do
  end do
  i1=i1+nb_tot(ic)
 end do
 nb_max=maxval(loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsb))
 nb_min=minval(loc_nbpart(0:npe_yloc-1,0:npe_zloc-1,0:npe_xloc-1,1:nsb))
 do ic=1,nsb
  do p=0,npe_xloc-1
   do ip=0,npe_zloc-1
    do ipp=0,npe_yloc-1
     i=ipp+npe_yloc*(ip+p*npe_zloc)
     if(loc_nbpart(ipp,ip,p,ic)==nb_max)pe_nbmax=i
     if(loc_nbpart(ipp,ip,p,ic)==nb_min)pe_nbmin=i
    end do
   end do
  end do
 end do
 !==================
 ! The local MPI task
 nps_loc(1:nsb)=loc_nbpart(imody,imodz,imodx,1:nsb)
 npmax=maxval(nps_loc(1:nsb))
 !++++++++++++++++++++++++++++++++++++++
 if(npmax >0)call p_alloc(npmax,nd2+1,nps_loc,nsb,LPf_ord,1,2,mem_psize)
 !=================================
 nb_loc=nps_loc(1)
 p=imodx
 ip=imodz
 z1=loc_zgrid(ip)%gmin
 z2=loc_zgrid(ip)%gmax
 ipp=imody
 y1=loc_ygrid(ipp)%gmin
 y2=loc_ygrid(ipp)%gmax
 !=========================
 ! Here 3D MPI decomp. allowed
 !===================================
 i1=0
 do ic=1,nsb
  ii=0
  do i=1,nb_tot(ic)
   j=i+i1
   if(bpart(2,j) >y1.and.bpart(2,j) <=y2)then
    if(bpart(3,j) >z1.and.bpart(3,j) <=z2)then
     if(bpart(1,j) >x1.and.bpart(1,j) <=x2)then
      ii=ii+1
      bunch(ic)%part(ii,1:7)=bpart(1:7,j)
     endif
    endif
   endif
  end do
  i1=i1+nb_tot(ic)
 enddo
 !=================================
 !=================================
 !============
 end subroutine MPI_beam_distribute
 !========================
 subroutine beam_model_pot(pot,sx,sy,sz,b_am,i1,i2,j1,j2,k1,k2)

 real(dp),intent(inout) :: pot(:,:,:,:)
 real(dp),intent(in) :: sx,sy,sz,b_am
 integer,intent(in) :: i1,i2,j1,j2,k1,k2
 integer :: i,j,k,jj,kk
 real(dp) :: r2,brad2,sx2_inv,fact,r2max,pot0
 real(dp) :: xx,yy,zz
 !-----------------------
 brad2=sy*sy+sz*sz
 r2max=ymax*ymax+zmax*zmax
 pot0=log(r2max/brad2)
 sx2_inv=1./(2.*sx*sx)
 do k=k1,k2
  kk=k-2
  zz=loc_zg(kk,1,imodz)
  do j=j1,j2
   jj=j-2
   yy=loc_yg(jj,1,imody)
   r2=zz*zz+yy*yy
   if(r2 > brad2)then
    fact=log(r2/brad2)-pot0
   else
    fact=(r2/brad2-1.)-pot0
   endif
   do i=i1,i2
    xx=(x(i1)-xc_bunch(1))
    pot(i,j,k,1)=0.25*brad2*b_am*fact*exp(-xx*xx*sx2_inv)
   end do
  end do
 end do
 end subroutine beam_model_pot
 !=========================
 !=========================
 subroutine pwfa_density(wp,kp,i2,j2,k2,dir)
 real(dp),intent(inout) :: wp(:,:,:)
 real(dp),intent(in) :: kp
 integer,intent(in) :: i2,j2,k2,dir
 integer :: ii,ik,i,j,k
 real(dp) :: kpx,sum0(2),skx

 ! in wp enters the beam charge density at staggered x-coordinate
 select case(dir)
 case(1)
  allocate(kern(i2))
  kpx=kp*dx
  kern=0.0
  do i=1,i2
   kern(i)=kpx*sin(kp*x(i))
  end do
  call ft_kern(kern,i2,-1)
  call ftw1d_sin(wp,i2,j2,k2,-1,1,1)
  do k=1,k2
   do j=1,j2
    do i=2,i2
     wp(i,j,k)=wp(i,j,k)*kern(i)
    end do
   end do
  end do
  call ftw1d_sin(wp,i2,j2,k2,1,1,1)
  !=============== use n(x)=sin(kp*x)*sum_y<x[cos(kp*y)*nb(y)]
  !                         +cos(kp*x)*sum_y>x[sin(kp*y)*nb(y)]
 case(2)
  kpx=kp*dx
  skx=kp*sin(0.5*kpx)/(0.5*kpx)
  allocate(kern2(i2,2))
  do i=1,i2
   kern2(i,1)=sin(kp*x(i))
   kern2(i,2)=cos(kp*x(i))
  end do
  do k=1,k2
   do j=1,j2
    w1(1:i2)=skx*wp(1:i2,j,k)
    wp(1:i2,j,k)=0.0
    do i=1,i2
     sum0=0.0
     ik=max(i,i2/2)
     do ii=ik,i2
      sum0(1)=sum0(1)+kern2(ii,1)*w1(ii)
      sum0(2)=sum0(2)+kern2(ii,2)*w1(ii)
     end do
     wp(i,j,k)=sum0(1)*kern2(i,2)-sum0(2)*kern2(i,1)
    end do
   end do
  end do
 end select
 end subroutine pwfa_density
 !======================
 subroutine beam_potential(pot,gam2,nxf,n2_loc,n3_loc)
 real(dp),intent(inout) :: pot(:,:,:)
 real(dp),intent(in) :: gam2
 integer,intent(in) :: nxf,n2_loc,n3_loc
 real(dp) :: ak2,ak2p
 integer :: ix,iy,iy1,iz,iz1
 !_________________________________
 ! Solves Poisson equation in Fourier speca
 do iz=1,n3_loc
  iz1=iz+imodz*n3_loc
  do iy=1,n2_loc
   iy1=iy+imody*n2_loc
   ak2p=skz(iz1)*skz(iz1)+sky(iy1)*sky(iy1)
   if(ak2p >0.0)then
    do ix=1,nxf
     ak2=ak2p+skx(ix)*skx(ix)/gam2
     pot(ix,iy,iz)=pot(ix,iy,iz)/ak2    !pot_b
    end do
   else
    do ix=2,nxf
     ak2=skx(ix)*skx(ix)/gam2
     pot(ix,iy,iz)=pot(ix,iy,iz)/ak2    !pot_b
    end do
   endif
  end do
 end do
 !=================
 end subroutine beam_potential
 !======================================
 subroutine FFT_Psolv(rho,g2,lxb,lyb,n1,n1_loc,n2,n2_loc,n3,n3_loc,&
  i1,i2,j1,j2,k1,k2,ft_mod)
 real(dp),intent(inout) :: rho(:,:,:,:)
 real(dp),intent(in) :: g2,lxb,lyb
 integer,intent(in) :: n1,n1_loc,n2,n2_loc,n3,n3_loc
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ft_mod
 integer :: i,ii,j,k

 allocate(wb(n1,n2_loc,n3_loc))
 call ftw_init(n1,n2,n3,2)
 call set_ftgrid(n1,n2,n3,2,lxb,lyb)   !sin/cos transf k=integer
 call mpi_ftw_alloc(n1,n2,n2_loc,n3,n3_loc)
 wb=0.0
 if(prlx)then
  do k=k1,k2
   do j=j1,j2
    aux1(1:n1)=0.0
    do i=i1,i2
     ii=i-2
     aux1(ii)=rho(i,j,k,1)
    end do
    call all_gather_dpreal(aux1,aux2,3,n1_loc)
    do i=1,n1
     wb(i,j-2,k-2)=aux2(i)
    end do
   end do
  end do
 else
  wb(1:n1,1:n2_loc,1:n3_loc)=rho(i1:i2,j1:j2,k1:k2,1)
 endif
 call pftw3d_sin(wb,n1,n2,n2_loc,n3,n3_loc,-1,ft_mod)
 !sin transform in (y,z) cos transform in x
 wb(1:n1,1:n2_loc,1:n3_loc)=ompe*wb(1:n1,1:n2_loc,1:n3_loc)
 !+++++++++++++++++++++++++++
 call beam_potential(wb,g2,n1,n2_loc,n3_loc)
 !exit fourier components for beam potential
 call pftw3d_sin(wb,n1,n2,n2_loc,n3,n2_loc,1,ft_mod)
 if(prlx)then
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     ii=i-2+imodx*n1_loc
     rho(i,j,k,1)=wb(ii,j-2,k-2)
    end do
   end do
  end do
 else
  rho(i1:i2,j1:j2,k1:k2,1)=wb(1:n1,1:n2_loc,1:n3_loc)
 endif
 call ftw_end
 if(allocated(wb))deallocate(wb)
 call mpi_ftw_dalloc
 end subroutine FFT_Psolv
 !========================================
 subroutine Bpulse

 integer :: i1,i2,i2b,j1,k1,nyp,nzp
 integer :: nptot,np,n_st,ic
 real(dp) :: gam2
 real(dp) :: xm,ym,zm
 real(dp) :: lxb,lyb

 gam2=gam0*gam0
 !!!!!!!!!!!!!!
 !=============================
 ! The fields of moving bunches in vacuum
 ! E_x=-(DPhi/Dx)/gamma^2  , E_y=-DPhi/Dy   E_z=-DPhi/Dz
 ! Poisson eq.  D_xE_x+D_yE_y+D_zE_z=omp^2\rho (x-V_b*t_0,y,z)
 ! B_x=0   B_y=-V_b*E_z    B_z= V_b*E_y
 !=========================================
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i2b=i2
 !=======================
 call beam_data(1,1,nptot)
 ! Generates phase space coordinateis for all beams on bpart(7,np_tot)
 ! bpart in common to all MPI tasks
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 !=======================
 call MPI_beam_distribute !bpart are loaded on bunc() struct for each MPI task
 !==================== Computes the total bunches density
 jc(:,:,:,1)=0.0
 n_st=0
 do ic= 1,nsb
  np=loc_nbpart(imody,imodz,imodx,ic)
  call set_grid_charge(bunch(ic),ebfb,jc,np,ndim,n_st,1,xm,ym,zm)
 end do
 !generates ebf_bunc(1)=den(i,j,k)
 !on local MPI computational grid[i2,nyp,nzb]
 if(prl)call fill_curr_yzxbdsdata(jc,i1,i2b,j1,nyp,k1,nzp,1)
 !============================
 ! BUNCH grid DENSITY already normalized
 ! index=3 for bunch current: a same bet0 assumed for all bunches
 !===========================================
 ebf_bunch(i1:i2b,j1:nyp,k1:nzp,1)=jc(i1:i2b,j1:nyp,k1:nzp,1)
 ebf_bunch(i1:i2b,j1:nyp,k1:nzp,2)=bet0*jc(i1:i2b,j1:nyp,k1:nzp,1)
 !=====================================================
 ! UNIFORM GRIDS ASSUMED
 !======================
 if(allocated(bpart))deallocate(bpart)
 lxb=x(nx)-x(1)
 lyb=y(ny)-y(1)
 call FFT_Psolv(ebf_bunch,gam2,lxb,lyb,nx,nx_loc,ny,ny_loc,nz,nz_loc,&
  i1,i2b,j1,nyp,k1,nzp,1)
 !Beam potential in ebf_bunch(1)
 !===================
 !====================
 call fill_ebfield_yzxbdsdata(ebf_bunch,i1,i2b,j1,nyp,k1,nzp,1,2,1,1)
 if(ibeam <2)then
  call initial_beam_fields(ebf_bunch,i1,i2b,j1,nyp,k1,nzp,gam2,bet0)
  ! generates (Ex,Ey,Ez,By,Bz) bunch fields stencils
 else
  call initial_beam_potential_and_fields(ebf_bunch,ebf1_bunch,ebf,&
   i1,i2b,j1,nyp,k1,nzp,dt,gam2,bet0)
  ! in ebf1_bunch ebf_bunch(1:4) (A)(t=-Dt/2,Dt/2,phi (t=-Dt, t=0)
  ! ebf(1:6) (Ex,Ey,Ez,Bx,By,Bz) bunch initial fields
 endif
 !========================================
 lp_end=xc_bunch(1)+sxb(1)
 !=====================================
 if(L_Bpoloidal) call set_poloidal_ex_fields( &
  ebf0_bunch,i1,i2b,j1,nyp,k1,nzp,B_ex_poloidal*T_unit,radius_poloidal)
 !=====================================
 call part_distribute(dmodel_id,lp_end)
 !============================
 !----------------------
 if(pe0)then
  !==================
  open(16,file='Initial_bunch_info.dat')
  write(16,'(a18,i4)')' Beam injected nb=',nsb
  write(16,'(a27,e11.4)')' Initial target x-position=',targ_in
  write(16,'(a20,e11.4)')' Plasma wave-length=',lambda_p
  write(16,*) '-------------------------------------'
  do i1=1,nsb
   write(16,'(a13,i4)')' Bunch number',i1
   write(16,'(a31,e11.4)')' relative bunch/particle weights',jb_norm(i1)
   write(16,'(a23,i8)')' bunch particle number ',nb_tot(i1)
   write(16,'(a17,3e11.4)')' sizes and gamma ',sxb(i1),syb(i1),gam(i1)
   write(16,'(a23,2e11.4)')' Transverse emittances=',epsy(i1),epsz(i1)
   write(16,'(a21,e11.4)')' Initial xc-position=',xc_bunch(i1)
   write(16,'(a20,2e11.4)')' b charge [pC],Qch =',bunch_charge(i1),reduced_charge(i1)
   write(16,'(a22,e11.4)')' bcharge_over_pcharge ', rhob(i1)
  end do
  close(16)
 endif
 end subroutine Bpulse
 !========================
 subroutine Prbeam

 integer :: i1,i2,i2b,j1,k1,nyp,nzp,ib_xdistr
 integer :: nptot,np,n_st,ic
 real(dp) :: gam2,x0b
 real(dp) :: xm,ym,zm
 real(dp) :: lxb,lyb
 real(dp) :: S_scale(3,2),B_s(2)

 gam2=gam0*gam0
 !!!!!!!!!!!!!!
 !=============================
 ! The fields of a uniform moving bunche in vacuum
 ! E_x=0      , E_y=-DPhi/Dy   E_z=-DPhi/Dz
 ! Poisson eq.  D_xE_x+D_yE_y+D_zE_z=omp^2\rho (x-V_b*t_0,y,z)
 ! B_x=0   B_y=-V_b*E_z    B_z= V_b*E_y
 !=========================================
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i2b=i2
 lxb=xmax  !the p_beam size
 lyb=y(ny)-y(1)
 ib_xdistr =2     !uniform beam
 !=======================
 if(pe0)call system("date")
 call beam_data(1,ib_xdistr,nptot)
 ! Generates phase space coordinateis for a beam on bpart(7,np_tot)
 ! bpart in common to all MPI tasks
 ! the beam has length sxb(1) and starts at xc_bunch(1) position
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 !=============================
 call MPI_beam_distribute
 !=============================
 !bpart are loaded on bunc() struct for each MPI task
 ! inside the computational box
 !========================
 jc(:,:,:,1)=0.0
 n_st=0
 ic=1
 np=loc_nbpart(imody,imodz,imodx,ic)
 call set_grid_charge(bunch(ic),ebfb,jc,np,ndim,n_st,1,xm,ym,zm)
 !=========== extend uniform x-distribution
 !on extended computational grid[i2b,nyp,nzb]
 if(prl)call fill_curr_yzxbdsdata(jc,i1,i2b,j1,nyp,k1,nzp,1)
 !============================
 ! BUNCH grid DENSITY already normalized
 ! index=3 for bunch current: a same bet0 assumed for all particles
 !===========================================
 ebf_bunch(i1:i2b,j1:nyp,k1:nzp,1)=jc(i1:i2b,j1:nyp,k1:nzp,1)
 ebf_bunch(i1:i2b,j1:nyp,k1:nzp,3)=bet0*ebf_bunch(i1:i2b,j1:nyp,k1:nzp,1)
 !=====================================================
 ! UNIFORM GRIDS ASSUMED
 !======================
 if(allocated(bpart))deallocate(bpart)
 !=====================
 !=======================
 call FFT_Psolv(ebf_bunch,gam2,lxb,lyb,nx,nx_loc,ny,ny_loc,nz,nz_loc,&
  i1,i2b,j1,nyp,k1,nzp,2)
 !Beam potential in ebf_bunch(1)
 !===================
 !====================
 call fill_ebfield_yzxbdsdata(ebf_bunch,i1,i2b,j1,nyp,k1,nzp,1,1,1,1)
 call initial_beam_fields(ebf_bunch,i1,i2b,j1,nyp,k1,nzp,gam2,bet0)
 !======================
 lp_end=xc_bunch(1)+sxb(1)
 S_scale(1,1)=600.
 S_scale(1,2)=600.
 S_scale(2,1)=300.
 S_scale(2,2)=250.
 S_scale(3,1)=30.
 S_scale(3,2)=50.
 B_s(1)=-0.2*T_unit
 B_s(2)=-0.28*T_unit
 x0b=x(i2b/2)
 ! generates (Ex,Ey,Ez,By,Bz) bunch fields stencils
 ! in ebf_bunch(4) the bunch current Jbx[i+1/2,j,k] at t=0
 ebf=0.0
 if(xmax > 1000.)call set_solenoid_fields(&
  ebf0_bunch,i1,i2b,j1,nyp,k1,nzp,x0b,S_scale,B_s)
 !========================================
 !=====================================
 call part_distribute(dmodel_id,lp_end+lpx(7))
 !============================
 !----------------------
 if(pe0)then
  !==================
  open(16,file='Initial_bunch_info.dat')
  write(16,'(a33,e11.4)')' A uniform p-beam of length (mm) ',sxb(1)
  write(16,'(a27,e11.4)')' Initial plasma x-position=',targ_in
  write(16,'(a20,e11.4)')' Plasma wave-length=',lambda_p
  write(16,*) '-------------------------------------'
  i1=1
  write(16,'(a13,i4)')' Bunch number',i1
  write(16,'(a31,e11.4)')' relative bunch/particle weights',jb_norm(i1)/j0_norm
  write(16,'(a23,i8)')' bunch particles number ',nb_tot(i1)
  write(16,'(a17,3e11.4)')' sizes and gamma ',sxb(i1),syb(i1),gam(i1)
  write(16,'(a23,2e11.4)')' Transverse emittances=',epsy(i1),epsz(i1)
  write(16,'(a20,e11.4)')' Longitudinal beta =',bet0
  write(16,'(a20,2e11.4)')' b charge [pC],Qch =',bunch_charge(i1),reduced_charge(i1)
  write(16,'(a22,e11.4)')' bcharge_over_pcharge ', rhob(i1)
  close(16)
 endif
 end subroutine Prbeam
 !=======================
 ! END for 3D BUNCHES in PBWA===============
 !==========================
 subroutine init

 ! 1=P-polarization, 2=S-polarization, 3=C-polarization`
 ! periodic BC iby=ibz=0 for plane wave
 !======================================
 if(model_id <3)then
  call LP_pulse(model_id)
 else
  select case(model_id)
  case(3)
   call CP_pulse(model_id)          !Circular polarization
  case(4)
   call set_envelope(t0_lp)      !Envelope  approximation for Ay
  case(5)
   !Plasma Wakefield Acceleration
   !Comb: nsb-1 equal driving buches +witness
   call Bpulse                      !3D-cartesian PBWA
  case(6)
   ! Proton bunch propagation in uniform plasma
   call Prbeam                      !3D-cartesian PBWA
  end select
 endif
 end subroutine init
 !+++++++++++++++++++++++++++++++++++++++
 end module pic_in
