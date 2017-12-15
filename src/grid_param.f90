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

 module grid_param

 use precision_def
 use struct_def
 use grid_and_particles

 implicit none

 !=======================
 type(grid),allocatable :: loc_rgrid(:),loc_ygrid(:),loc_zgrid(:),loc_xgrid(:)
 type(sgrid) :: str_xgrid,str_ygrid,str_zgrid
 !fft grid
 real(dp),allocatable :: akx(:,:),aky(:,:),akz(:,:),sty(:,:)
 real(dp),allocatable :: ak2x(:,:),ak2y(:,:),ak2z(:,:),kern(:),kern2(:,:)
 real(dp),allocatable :: skx(:,:),sky(:,:),skz(:,:)
 !==================
 real(dp),allocatable :: loc_yg(:,:,:),loc_zg(:,:,:),loc_xg(:,:,:)
 real(dp),allocatable :: x(:),xw(:),y(:),z(:),dx1(:),dy1(:),dz1(:)
 real(dp),allocatable :: xh(:),yh(:),zh(:),dx1h(:),dy1h(:),dz1h(:)
 real(dp),allocatable :: loc_rg(:,:,:),r(:),rh(:),dr1(:),dr1h(:),dvr(:)
 integer,allocatable :: str_indx(:,:)
 real(dp),allocatable :: rpt(:),wgp(:)
 real(dp) :: xtot,xmax,xmin,ymax,ymin,zmax,zmin,xw_min,xw_max
 real(dp) :: Lx_box,Ly_box,Lz_box
 real(dp) :: dx,dx_inv,dxi_inv,dy,dz,dy_inv,dyi_inv,dz_inv,dzi_inv
 real(dp) :: aph,L_s,Lx_s,dxi,dyi,dzi,sy_rat,sz_rat,sx_rat
 integer :: loc_ygr_max,loc_zgr_max,loc_xgr_max

 !--------------------------

 contains

 !--------------------------

 subroutine grid_alloc(n1,n1_loc,n2,n2_loc,n3,n3_loc,&
  n2_targ,pml_sz,npey,npez,npex)
 integer,intent(in) :: n1,n1_loc,n2,n2_loc,n3,n3_loc,&
  n2_targ,pml_sz,npey,npez,npex
 integer :: n_loc,nyt_loc,nyt_res,nzt_loc,nzt_res,nyt_hres,nzt_hres
 integer :: p

 allocate(loc_ygrid(0:npey-1),loc_zgrid(0:npez-1))
 allocate(loc_rgrid(0:npey-1),loc_xgrid(0:npex-1))
 allocate(x(n1+1),xw(n1+1),dx1(n1+1),y(n2+1),z(n3+1),dy1(n2+1),dz1(n3+1))
 allocate(dx1h(n1+1),dy1h(n2+1),dz1h(n3+1))
 allocate(xh(n1+1),yh(n2+1),zh(n3+1))
 allocate(r(n2+1),rh(n2+1),dr1(n2+1),dr1h(n2+1),dvr(0:n2+1))

 loc_xgr_max=n1_loc
 do p=0,npex-1
  loc_xgrid(p)%ng=n1_loc
 end do
 if(pml_sz==0)then
  loc_ygr_max=n2_loc
  loc_zgr_max=n3_loc
  do p=0,npey-1
   loc_ygrid(p)%ng=n2_loc
   loc_rgrid(p)%ng=n2_loc
  end do
  do p=0,npez-1
   loc_zgrid(p)%ng=n3_loc
  end do
 else
  n_loc=1
  if(npey>1)n_loc=pml_sz
  nyt_loc=0
  if(npey>2)then
   nyt_loc=n2_targ/(npey-2)
   loc_ygrid(1:npey-2)%ng=nyt_loc
   nyt_res=n2_targ-nyt_loc*(npey-2)
   if(nyt_res >0)then
    nyt_hres=nyt_res/2
    n_loc=n_loc+nyt_hres
   endif
  endif
  loc_ygrid(0)%ng=n_loc
  loc_ygrid(npey-1)%ng=n_loc
  loc_ygr_max=max(n_loc,nyt_loc)

  n_loc=1
  if(npez>1)n_loc=pml_sz
  nzt_loc=0
  if(npez>2)then
   nzt_loc=n2_targ/(npez-2)
   loc_zgrid(1:npez-2)%ng=nzt_loc
   nzt_res=n2_targ-nzt_loc*(npez-2)
   if(nzt_res >0)then
    nzt_hres=nzt_res/2
    n_loc=n_loc+nzt_hres
   endif
  endif
  loc_zgrid(0)%ng=n_loc
  loc_zgrid(npez-1)%ng=n_loc
  loc_zgr_max=max(n_loc,nzt_loc)
 endif

 allocate(loc_yg(0:loc_ygr_max+1,4,0:npey-1))
 allocate(loc_zg(0:loc_zgr_max+1,4,0:npez-1))
 allocate(loc_xg(0:loc_xgr_max+1,4,0:npex-1))
 allocate(loc_rg(0:loc_ygr_max+1,5,0:npey-1)) !FIX SERVE SE NON SIAMO IN CILINDRICO???

 allocate(str_indx(0:npey-1,0:npez-1))
 str_indx(0:npey-1,0:npez-1)=0

 end subroutine grid_alloc

 !--------------------------

 subroutine set_fyzxgrid(npey,npez,npex,sh)
 integer,intent(in) :: npey,npez,npex,sh
 integer :: i,ii,p,ip,n_loc

 ! Defines initial local p-grid coordinate and loc n_cell
 ! y-grid decomposed on n2_loc uniform grid size
 loc_ygrid(0)%gmin=y(1)
 loc_rgrid(0)%gmin=r(1)
 ip=loc_ygrid(0)%ng
 n_loc=ip
 loc_ygrid(0)%gmax=y(ip+1)
 loc_rgrid(0)%gmax=r(ip+1)
 loc_ygrid(0)%p_ind(1)=min(sh,n_loc)
 loc_rgrid(0)%p_ind(1)=min(sh,n_loc)
 loc_ygrid(0)%p_ind(2)=n_loc+loc_ygrid(0)%p_ind(1)-1
 loc_rgrid(0)%p_ind(2)=n_loc+loc_rgrid(0)%p_ind(1)-1

 p=0
 do i=1,n_loc+1
  loc_yg(i,1,p)=y(i)
  loc_yg(i,2,p)=yh(i)
  loc_yg(i,3,p)=dy1(i)
  loc_yg(i,4,p)=dy1h(i)

  loc_rg(i,1,p)=r(i) !FIX SERVE SE NON SIAMO IN CILINDRICO???
  loc_rg(i,2,p)=rh(i)
  loc_rg(i,3,p)=dr1(i)
  loc_rg(i,4,p)=dr1h(i)
  loc_rg(i,5,p)=dvr(i)
 end do

 if(npey >1)then
  ip=loc_ygrid(0)%ng
  if(npey >2) then
   do p=1,npey-2
    n_loc=loc_ygrid(p-1)%ng
    loc_yg(0,1:4,p)=loc_yg(n_loc,1:4,p-1)
    loc_rg(0,1:4,p)=loc_rg(n_loc,1:4,p-1) !FIX SERVE SE NON SIAMO IN CILINDRICO???
    n_loc=loc_ygrid(p)%ng
    do i=1,n_loc+1
     ii=i+ip
     loc_yg(i,1,p)=y(ii)
     loc_yg(i,2,p)=yh(ii)
     loc_yg(i,3,p)=dy1(ii)
     loc_yg(i,4,p)=dy1h(ii)
     loc_rg(i,1,p)=r(ii) !FIX SERVE SE NON SIAMO IN CILINDRICO???
     loc_rg(i,2,p)=rh(ii)
     loc_rg(i,3,p)=dr1(ii)
     loc_rg(i,4,p)=dr1h(ii)
     loc_rg(i,5,p)=dvr(ii)
    end do
    loc_ygrid(p)%gmin=loc_ygrid(p-1)%gmax
    loc_rgrid(p)%gmin=loc_rgrid(p-1)%gmax !FIX SERVE SE NON SIAMO IN CILINDRICO???

    ip=ip+n_loc
    loc_ygrid(p)%gmax=y(ip+1)
    loc_rgrid(p)%gmax=r(ip+1) !FIX SERVE SE NON SIAMO IN CILINDRICO???

    loc_ygrid(p)%p_ind(1)=sh
    loc_ygrid(p)%p_ind(2)=n_loc+loc_ygrid(p)%p_ind(1)-1

    loc_rgrid(p)%p_ind(1)=sh !FIX SERVE SE NON SIAMO IN CILINDRICO???
    loc_rgrid(p)%p_ind(2)=n_loc+loc_rgrid(p)%p_ind(1)-1
   end do
  endif
  p=npey-1
  n_loc=loc_ygrid(p-1)%ng
  loc_yg(0,1:4,p)=loc_yg(n_loc,1:4,p-1)
  loc_rg(0,1:4,p)=loc_rg(n_loc,1:4,p-1) !FIX SERVE SE NON SIAMO IN CILINDRICO???
  n_loc=loc_ygrid(p)%ng
  do i=1,n_loc+1
   ii=i+ip
   loc_yg(i,1,p)=y(ii)
   loc_yg(i,2,p)=yh(ii)
   loc_yg(i,3,p)=dy1(ii)
   loc_yg(i,4,p)=dy1h(ii)

   loc_rg(i,1,p)=r(ii) !FIX SERVE SE NON SIAMO IN CILINDRICO???
   loc_rg(i,2,p)=rh(ii)
   loc_rg(i,3,p)=dr1(ii)
   loc_rg(i,4,p)=dr1h(ii)
   loc_rg(i,5,p)=dvr(ii)
  end do
  loc_ygrid(p)%gmin=loc_ygrid(p-1)%gmax
  loc_rgrid(p)%gmin=loc_rgrid(p-1)%gmax
  ip=ip+n_loc
  loc_ygrid(p)%gmax=y(ip+1)
  loc_ygrid(p)%p_ind(1)=sh
  loc_ygrid(p)%p_ind(2)=n_loc+loc_ygrid(p)%p_ind(1)-1

  loc_rgrid(p)%gmax=r(ip+1)
  loc_rgrid(p)%p_ind(1)=sh
  loc_rgrid(p)%p_ind(2)=n_loc+loc_rgrid(p)%p_ind(1)-1
 endif
 !=========================
 loc_zgrid(0)%gmin=z(1)
 ip=loc_zgrid(0)%ng
 n_loc=ip
 loc_zgrid(0)%gmax=z(ip+1)
 loc_zgrid(0)%p_ind(1)=min(sh,n_loc)
 loc_zgrid(0)%p_ind(2)=n_loc+loc_zgrid(0)%p_ind(1)-1

 p=0
 do i=1,n_loc+1
  loc_zg(i,1,p)=z(i)
  loc_zg(i,2,p)=zh(i)
  loc_zg(i,3,p)=dz1(i)
  loc_zg(i,4,p)=dz1h(i)
 end do

 if(npez >1)then
  ip=loc_zgrid(0)%ng
  if(npez >2) then
   do p=1,npez-2
    n_loc=loc_zgrid(p-1)%ng
    loc_zg(0,1:4,p)=loc_zg(n_loc,1:4,p-1)
    n_loc=loc_zgrid(p)%ng
    do i=1,n_loc+1
     ii=i+ip
     loc_zg(i,1,p)=z(ii)
     loc_zg(i,2,p)=zh(ii)
     loc_zg(i,3,p)=dz1(ii)
     loc_zg(i,4,p)=dz1h(ii)
    end do
    loc_zgrid(p)%gmin=loc_zgrid(p-1)%gmax
    ip=ip+n_loc
    loc_zgrid(p)%gmax=z(ip+1)
    loc_zgrid(p)%p_ind(1)=sh
    loc_zgrid(p)%p_ind(2)=n_loc+loc_zgrid(p)%p_ind(1)-1
   end do
  endif
  p=npez-1
  n_loc=loc_zgrid(p-1)%ng
  loc_zg(0,1:4,p)=loc_zg(n_loc,1:4,p-1)
  n_loc=loc_zgrid(p)%ng
  do i=1,n_loc+1
   ii=i+ip
   loc_zg(i,1,p)=z(ii)
   loc_zg(i,2,p)=zh(ii)
   loc_zg(i,3,p)=dz1(ii)
   loc_zg(i,4,p)=dz1h(ii)
  end do
  loc_zgrid(p)%gmin=loc_zgrid(p-1)%gmax
  ip=ip+n_loc
  loc_zgrid(p)%gmax=z(ip+1)
  loc_zgrid(p)%p_ind(1)=sh
  loc_zgrid(p)%p_ind(2)=n_loc+loc_zgrid(p)%p_ind(1)-1
 endif
 !======================
 loc_xgrid(0)%gmin=x(1)
 ip=loc_xgrid(0)%ng
 n_loc=ip
 loc_xgrid(0)%gmax=x(ip+1)
 loc_xgrid(0)%p_ind(1)=min(sh,n_loc)
 loc_xgrid(0)%p_ind(2)=n_loc+loc_xgrid(0)%p_ind(1)-1

 p=0
 do i=1,n_loc+1
  loc_xg(i,1,p)=x(i)
  loc_xg(i,2,p)=xh(i)
  loc_xg(i,3,p)=dx1(i)
  loc_xg(i,4,p)=dx1h(i)
 end do

 if(npex >1)then
  ip=loc_xgrid(0)%ng
  if(npex >2) then
   do p=1,npex-2
    n_loc=loc_xgrid(p-1)%ng
    loc_xg(0,1:4,p)=loc_xg(n_loc,1:4,p-1)
    n_loc=loc_xgrid(p)%ng
    do i=1,n_loc+1
     ii=i+ip
     loc_xg(i,1,p)=x(ii)
     loc_xg(i,2,p)=xh(ii)
     loc_xg(i,3,p)=dx1(ii)
     loc_xg(i,4,p)=dx1h(ii)
    end do
    loc_xgrid(p)%gmin=loc_xgrid(p-1)%gmax
    ip=ip+n_loc
    loc_xgrid(p)%gmax=x(ip+1)
    loc_xgrid(p)%p_ind(1)=sh
    loc_xgrid(p)%p_ind(2)=n_loc+loc_xgrid(p)%p_ind(1)-1
   end do
  endif
  p=npex-1
  n_loc=loc_xgrid(p-1)%ng
  loc_xg(0,1:4,p)=loc_xg(n_loc,1:4,p-1)
  n_loc=loc_xgrid(p)%ng
  do i=1,n_loc+1
   ii=i+ip
   loc_xg(i,1,p)=x(ii)
   loc_xg(i,2,p)=xh(ii)
   loc_xg(i,3,p)=dx1(ii)
   loc_xg(i,4,p)=dx1h(ii)
  end do
  loc_xgrid(p)%gmin=loc_xgrid(p-1)%gmax
  ip=ip+n_loc
  loc_xgrid(p)%gmax=x(ip+1)
  loc_xgrid(p)%p_ind(1)=sh
  loc_xgrid(p)%p_ind(2)=n_loc+loc_xgrid(p)%p_ind(1)-1
 endif
 end subroutine set_fyzxgrid
 !======================
 subroutine set_fxgrid(npex,sh)
 integer,intent(in) :: npex,sh
 integer :: i,ii,p,ip,n_loc

 loc_xgrid(0)%gmin=x(1)
 ip=loc_xgrid(0)%ng
 n_loc=ip
 loc_xgrid(0)%gmax=x(ip+1)
 loc_xgrid(0)%p_ind(1)=min(sh,n_loc)
 loc_xgrid(0)%p_ind(2)=n_loc+loc_xgrid(0)%p_ind(1)-1

 p=0
 do i=1,n_loc+1
  loc_xg(i,1,p)=x(i)
  loc_xg(i,2,p)=xh(i)
  loc_xg(i,3,p)=dx1(i)
  loc_xg(i,4,p)=dx1h(i)
 end do

 if(npex >1)then
  ip=loc_xgrid(0)%ng
  if(npex >2) then
   do p=1,npex-2
    n_loc=loc_xgrid(p-1)%ng
    loc_xg(0,1:4,p)=loc_xg(n_loc,1:4,p-1)
    n_loc=loc_xgrid(p)%ng
    do i=1,n_loc+1
     ii=i+ip
     loc_xg(i,1,p)=x(ii)
     loc_xg(i,2,p)=xh(ii)
     loc_xg(i,3,p)=dx1(ii)
     loc_xg(i,4,p)=dx1h(ii)
    end do
    loc_xgrid(p)%gmin=loc_xgrid(p-1)%gmax
    ip=ip+n_loc
    loc_xgrid(p)%gmax=x(ip+1)
    loc_xgrid(p)%p_ind(1)=sh
    loc_xgrid(p)%p_ind(2)=n_loc+loc_xgrid(p)%p_ind(1)-1
   end do
  endif
  p=npex-1
  n_loc=loc_xgrid(p-1)%ng
  loc_xg(0,1:4,p)=loc_xg(n_loc,1:4,p-1)
  n_loc=loc_xgrid(p)%ng
  do i=1,n_loc+1
   ii=i+ip
   loc_xg(i,1,p)=x(ii)
   loc_xg(i,2,p)=xh(ii)
   loc_xg(i,3,p)=dx1(ii)
   loc_xg(i,4,p)=dx1h(ii)
  end do
  loc_xgrid(p)%gmin=loc_xgrid(p-1)%gmax
  ip=ip+n_loc
  loc_xgrid(p)%gmax=x(ip+1)
  loc_xgrid(p)%p_ind(1)=sh
  loc_xgrid(p)%p_ind(2)=n_loc+loc_xgrid(p)%p_ind(1)-1
 endif
 end subroutine set_fxgrid

 subroutine set_str_ind(npey,npez,ndm)
 integer,intent(in) :: npey,npez,ndm
 integer :: p,q,ip(4)

 str_indx(0:npey-1,0:npez-1)=0
 ip=0
 if(ndm <3)then
  do p=0,npey-1
   if(str_ygrid%smin >loc_ygrid(p)%gmin)ip(1)=p
   if(str_ygrid%smax >= loc_ygrid(p)%gmin)ip(2)=p
  end do

  p=0
  do q=0,ip(1)
   str_indx(q,p)=1
  end do
  do q=ip(2),npey-1
   str_indx(q,p)=2
  end do
  return
 endif
 do p=0,npey-1
  if(str_ygrid%smin > loc_ygrid(p)%gmin)ip(1)=p
  if(str_ygrid%smax >= loc_ygrid(p)%gmin)ip(2)=p
 end do
 do p=0,npez-1
  if(str_zgrid%smin >loc_zgrid(p)%gmin)ip(3)=p
  if(str_zgrid%smax >=loc_zgrid(p)%gmin)ip(4)=p
 end do

 do p=0,ip(3)
  str_indx(0:npey-1,p)=2
  do q=0,ip(1)
   str_indx(q,p)=1
  end do
  do q=ip(2),npey-1
   str_indx(q,p)=3
  end do
 end do
 do p=ip(3)+1,ip(4)-1
  do q=0,ip(1)
   str_indx(q,p)=8
  end do
  do q=ip(2),npey-1
   str_indx(q,p)=4
  end do
 end do
 do p=ip(4),npez-1
  str_indx(0:npey-1,p)=6
  do q=0,ip(1)
   str_indx(q,p)=7
  end do
  do q=ip(2),npey-1
   str_indx(q,p)=5
  end do
 end do
 end subroutine set_str_ind
 !--------------------------
 subroutine set_grid(n1,n2,n3,ib,x_stretch,y_stretch,xres,yxres,zxres)
 integer,intent(in) :: n1,n2,n3,ib,x_stretch,y_stretch
 real(dp),intent(in) :: xres,yxres,zxres
 integer :: i,ns1
 real(dp) :: yy,yyh,sm,sp

 aph=acos(-1.0)*0.4
 dxi=1.
 dyi=1.
 dzi=1.
 sx_rat=1.
 sy_rat=1.
 sz_rat=1.
 sm=0.0
 sp=0.0
 dx=1.
 if(xres>0.0)dx=1./xres
 dx_inv=1.0/dx
 do i=1,n1+1
  x(i)=dx*real(i-1,dp)    !xminx(1)=0,.....,xmax-dx=x(nx)
  xh(i)=x(i)+0.5*dx
  dx1(i)=1.
  dx1h(i)=1.
 end do
 dxi=dx
 dxi_inv=dx_inv
 ns1=n1+1-x_stretch
 if(x_stretch >0)then
  dxi=aph/real(x_stretch,dp)
  dxi_inv=1./dxi
  Lx_s=dx*dxi_inv
  sx_rat=dxi*dx_inv
  sp=x(ns1)
  do i=ns1,n1+1
   yy=dxi*real(i-ns1,dp)
   yyh=yy+dxi*0.5
   x(i)=sp+Lx_s*tan(yy)
   xh(i)=sp+Lx_s*tan(yyh)
   dx1h(i)=cos(yyh)*cos(yyh)
   dx1(i)=cos(yy)*cos(yy)
  end do
 endif
 str_xgrid%sind(1)=x_stretch
 str_xgrid%sind(2)=ns1
 str_xgrid%smin=x(1)
 str_xgrid%smax=x(ns1)
 xw=x
 xmax=x(n1)
 xmin=x(1)
 if(ib==2)xmax=x(n1+1)
 Lx_box=xmax-xmin
 xw_min=xmin
 xw_max=xmax

 dy=1.
 dy_inv=1./dy
 dyi=dy
 dyi_inv=1./dy
 ymin=0.0
 ymax=0.0
 y=0.0
 yh=0.0
 dy1=1.
 dy1h=1.
 Ly_box=1.
 if(n2 > 1)then
  dy=yxres*dx
  dy_inv=1./dy
  dyi=dy
  dyi_inv=dy_inv
  do i=1,n2+1
   y(i)=dy*real(i-1-n2/2,dp)
   yh(i)=y(i)+0.5*dy
   dy1(i)=1.
   dy1h(i)=1.
  end do
  ns1=n2+1-y_stretch
  if(y_stretch>0)then
   dyi=aph/real(y_stretch,dp)
   dyi_inv=1./dyi
   L_s=dy*dyi_inv
   sy_rat=dyi*dy_inv
   sm=y(y_stretch+1)
   sp=y(ns1)
   do i=1,y_stretch
    yy=dyi*real(i-1-y_stretch,dp)
    yyh=yy+0.5*dyi
    y(i)=sm+L_s*tan(yy)
    yh(i)=sm+L_s*tan(yyh)
    dy1h(i)=cos(yyh)*cos(yyh)
    dy1(i)=cos(yy)*cos(yy)
   end do
   do i=ns1,n2+1
    yy=dyi*real(i-ns1,dp)
    yyh=yy+dyi*0.5
    y(i)=sp+L_s*tan(yy)
    yh(i)=sp+L_s*tan(yyh)
    dy1h(i)=cos(yyh)*cos(yyh)
    dy1(i)=cos(yy)*cos(yy)
   end do
  endif
  str_ygrid%sind(1)=y_stretch
  str_ygrid%sind(2)=ns1
  str_ygrid%smin=y(y_stretch+1)
  str_ygrid%smax=y(ns1)
  ymin=y(1)
  ymax=y(n2+1)
  Ly_box=ymax-ymin

  r=y
  rh=yh
  dr1=dy1
  dr1h=dy1h
 endif
 dz=1.
 dz_inv=1./dz
 dzi=dz
 dzi_inv=1./dz
 zmin=0.0
 zmax=0.0
 z=0.0
 zh=0.0
 dz1=1.
 dz1h=1.
 Lz_box=1.
 if(n3 > 1)then
  dz=zxres*dx
  dz_inv=1./dz
  do i=1,n3+1
   z(i)=dz*real(i-1-n3/2,dp)
   zh(i)=z(i)+0.5*dz
   dz1(i)=1.
   dz1h(i)=1.
  end do
  ns1=n3+1-y_stretch
  if(y_stretch>0)then
   dzi=aph/real(y_stretch,dp)
   dzi_inv=1./dzi
   L_s=dz*dzi_inv
   sz_rat=dzi*dz_inv
   sm=z(y_stretch+1)
   sp=z(ns1)
   do i=1,y_stretch
    yy=dzi*real(i-1-y_stretch,dp)
    yyh=yy+0.5*dzi
    z(i)=sm+L_s*tan(yy)
    zh(i)=sm+L_s*tan(yyh)
    dz1h(i)=cos(yyh)*cos(yyh)
    dz1(i)=cos(yy)*cos(yy)
   end do
   do i=ns1,n3+1
    yy=dzi*real(i-ns1,dp)
    yyh=yy+dzi*0.5
    z(i)=sp+L_s*tan(yy)
    zh(i)=sp+L_s*tan(yyh)
    dz1h(i)=cos(yyh)*cos(yyh)
    dz1(i)=cos(yy)*cos(yy)
   end do
  endif
  str_zgrid%sind(1)=y_stretch
  str_zgrid%sind(2)=ns1
  str_zgrid%smin=sm
  str_zgrid%smax=sp
  zmin=z(1)
  zmax=z(n3+1)
  Lz_box=zmax-zmin
 endif
 !================
 end subroutine set_grid

 !--------------------------
 subroutine set_ftgrid(n1,n2,n3,lxbox,lybox)
 integer,intent(in) :: n1,n2,n3
 real(dp),intent(in) :: lxbox,lybox
 integer :: i
 real(dp) :: wkx,wky,wkz


 allocate(aky(n2+2,0:2),akz(n3+2,0:2))
 allocate(sky(n2+2,0:2),skz(n3+2,0:2))
 allocate(ak2y(n2+2,0:2),ak2z(n3+2,0:2),ak2x(n1+1,0:2))
 allocate(akx(1:n1+1,0:2),skx(1:n1+1,0:2))
  akx(:,0:2)=0.0
  ak2x(:,0:2)=0.0
  aky(:,0:2)=0.0
  ak2y(:,0:2)=0.0
  akz(:,0:2)=0.0
  ak2z(:,0:2)=0.0
  skx(:,0:2)=0.0
  sky(:,0:2)=0.0
  skz(:,0:2)=0.0
!================
!  Sets wave number grid for all configurations
!=============================================
                    !case(0)  ! staggered k-grid
 wkx=2.*acos(-1.)/lxbox !lxbox=x(n1+1)-x(1)
 wky=2.*acos(-1.)/lybox !lybox=y(n2+1)-y(1)
 wkz=wky
  do i=1,n1/2
   akx(i,0)=wkx*(real(i,dp)-0.5)
   skx(i,0)=2.*sin(0.5*dx*akx(i,0))/dx
  end do
  ak2x(1:n1,0)=akx(1:n1,0)*akx(1:n1,0)
  if(n2>1)then
   do i=1,n2/2
    aky(i,0)=wky*(real(i,dp)-0.5)
    aky(n2+1-i,0)=-aky(i,0)
   end do
   ak2y(1:n2,0)=aky(1:n2,0)*aky(1:n2,0)
   do i=1,n2
    sky(i,0)=2.*sin(0.5*dy*aky(i,0))/dy
   end do
  endif
  if(n3 >1)then
   do i=1,n3/2
    akz(i,0)=wkz*(real(i,dp)-0.5)
    akz(n3+1-i,0)=-akz(i,0)
   end do
   do i=1,n3
    skz(i,0)=2.*sin(0.5*dz*akz(i,0))/dz
   end do
   ak2z(1:n3,0)=akz(1:n3,0)*akz(1:n3,0)
  endif

                      !case(1)    !standard FT k-grid
  do i=1,n1/2
   akx(i,1)=wkx*real(i-1,dp)
   akx(n1+2-i,1)=-akx(i,1)
  end do
  ak2x(1:n1,1)=akx(1:n1,1)*akx(1:n1,1)
  do i=1,n1+1
   skx(i,1)=2.*sin(0.5*dx*akx(i,1))/dx
  end do
  if(n2 > 1)then
   do i=1,n2/2
    aky(i,1)=wky*real(i-1,dp)
    aky(n2+2-i,1)=-aky(i,1)
    sky(i,1)=2.*sin(0.5*dy*aky(i,1))/dy
   end do
   ak2y(1:n2,1)=aky(1:n2,1)*aky(1:n2,1)
  endif
  if(n3 > 1)then
   do i=1,n3/2
    akz(i,1)=wkz*real(i-1,dp)
    akz(n3+2-i,1)=-akz(i,1)
   end do
   do i=1,n3
    skz(i,1)=2.*sin(0.5*dz*akz(i,1))/dz
   end do
   ak2z(1:n3,1)=akz(1:n3,1)*akz(1:n3,1)
  endif

                         !case(2)  ! for the sine/cosine transform
  wkx=acos(-1.0)/lxbox
  wky=acos(-1.0)/lybox
  wkz=wky
  do i=1,n1+1
   akx(i,2)=wkx*real(i-1,dp)
   skx(i,2)=2.*sin(0.5*dx*akx(i,2))/dx
  end do
  if(n2>1)then
   do i=1,n2+1
    aky(i,2)=wky*real(i-1,dp)
    sky(i,2)=2.*sin(0.5*dy*aky(i,2))/dy
   end do
   ak2y(1:n2,2)=aky(1:n2,2)*aky(1:n2,2)
  endif
  if(n3 >1)then
   do i=1,n3+1
    akz(i,2)=wkz*real(i-1,dp)
    skz(i,2)=2.*sin(0.5*dz*akz(i,2))/dz
   end do
   ak2z(1:n3,2)=akz(1:n3,2)*akz(1:n3,2)
  endif
 end subroutine set_ftgrid

 end module grid_param
