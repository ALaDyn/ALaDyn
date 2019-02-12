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

 module particles
 use precision_def
 use pstruct_data
 use fstruct_data
 use all_param

 implicit none

 real(dp),parameter :: half=0.5,thr=0.75,two_third=2./3.,one_sixth=1./6.
 real(sp),parameter :: shx=3.,shy=3.,shz=3.
 integer(kind=2) :: err_ind

 contains
 !============== Mapping for stretched grids==========

 subroutine map2dy_part_sind(np,sind,ic1,ym,pt)
 integer,intent(in) :: np,sind,ic1
 real(dp),intent(in) :: ym
 real(dp),intent(inout) :: pt(:,:)
 real(dp) :: yp,ys,ximn, yp_loc
 integer :: n
 !========================
 !  enter the y=part(ic1,n) particle position in stretched grid
 !            y=y(xi)
 !  exit      xi=part(ic1,n) the  particle position in uniform grid
 !               normalized to the Dxi cell size
 !==========================================
 select case(sind)
 case(1)       !y<0
  ys=str_ygrid%smin
  ximn=-dyi_inv*atan(sy_rat*(ym-ys))
  do n=1,np
   yp=pt(n,ic1)
   yp_loc=ximn+dy_inv*(yp-ys)
   if(yp <= ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   pt(n,ic1)= yp_loc
  end do
 case(2)       !y>0
  ys=str_ygrid%smax
  if(ym>ys)then
   ximn=dyi_inv*atan(sy_rat*(ys-ym))
  else
   ximn=dy_inv*(ys-ym)
  endif
  do n=1,np
   yp=pt(n,ic1)
   yp_loc=dy_inv*(yp-ym)
   if(yp > ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   pt(n,ic1)= yp_loc
  end do
 end select
 end subroutine map2dy_part_sind

 subroutine map2dz_part_sind(np,sind,ic1,zm,pt)
 integer,intent(in) :: np,sind,ic1
 real(dp),intent(in) :: zm
 real(dp),intent(inout) :: pt(:,:)
 real(dp) :: zp,zs,zimn,zp_loc
 integer :: n
 !========================
 !  enter the y=part(ic1,n) particle position in stretched grid
 !            y=y(xi)
 !  exit      xi=part(ic1,n) the  particle position in uniform grid
 !               normalized to the Dxi cell size
 !==========================================
 select case(sind)
 case(1)       !z<0
  zs=str_zgrid%smin
  zimn=-dzi_inv*atan(sz_rat*(zm-zs))
  do n=1,np
   zp=pt(n,ic1)
   zp_loc=zimn+dz_inv*(zp-zs)
   if(zp <= zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic1)= zp_loc
  end do
 case(2)       !z>0
  zs=str_zgrid%smax
  if(zm>zs)then
   zimn=dzi_inv*atan(sz_rat*(zs-zm))
  else
   zimn=dz_inv*(zs-zm)
  endif
  do n=1,np
   zp=pt(n,ic1)
   zp_loc=dz_inv*(zp-zm)
   if(zp > zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic1)= zp_loc
  end do
 end select
 end subroutine map2dz_part_sind
 !--------------------------

 subroutine map3d_part_sind(pt,np,sind,ic1,ic2,ym,zm)
 integer,intent(in) :: np,sind,ic1,ic2
 real(dp),intent(in) :: ym,zm
 real(dp),intent(inout) :: pt(:,:)
 real(dp) :: yp,zp,yp_loc,zp_loc,ys,zs,ximn,zimn
 integer :: n
 !========================
 !  enter the y=part(n,ic1) z=part(n,ic2) particle positions
 !        in stretched grids    y=y(xi), z(zi)
 !  exit   xi=part(n,ic1) zi=part(n,ic2)
 !    particle positions in uniform grid
 !    normalized to the (Dxi Dzi) cell sizes
 !==========================================

 select case(sind)
 case(1)       !y<0 z<0 corner ys>ymn
  ys=str_ygrid%smin
  ximn=-dyi_inv*atan(sy_rat*(ym-ys))
  zs=str_zgrid%smin
  zimn=-dzi_inv*atan(sz_rat*(zm-zs))
  do n=1,np
   yp=pt(n,ic1)
   zp=pt(n,ic2)
   yp_loc=ximn+dy_inv*(yp-ys)
   zp_loc=zimn+dz_inv*(zp-zs)
   if(yp < ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   if(zp < zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic1)=yp_loc
   pt(n,ic2)=zp_loc
  end do
 case(2)       !z<0
  zs=str_zgrid%smin
  zimn=-dzi_inv*atan(sz_rat*(zm-zs))
  do n=1,np
   yp=pt(n,ic1)
   pt(n,ic1)=dy_inv*(yp-ym)
   zp=pt(n,ic2)
   zp_loc=zimn+dz_inv*(zp-zs)
   if(zp < zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic2)=zp_loc
  end do
 case(3)       !y>0 z<0 corner
  ys=str_ygrid%smax
  if(ym>ys)then
   ximn=dyi_inv*atan(sy_rat*(ys-ym))
  else
   ximn=dy_inv*(ys-ym)
  endif
  zs=str_zgrid%smin
  zimn=-dzi_inv*atan(sz_rat*(zm-zs))
  do n=1,np
   yp=pt(n,ic1)
   zp=pt(n,ic2)
   yp_loc=dy_inv*(yp-ym)
   zp_loc=zimn+dz_inv*(zp-zs)
   if(yp > ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   if(zp < zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic1)=yp_loc
   pt(n,ic2)=zp_loc
  end do
 case(4)       !y>0
  ys=str_ygrid%smax
  if(ym>ys)then
   ximn=dyi_inv*atan(sy_rat*(ys-ym))
  else
   ximn=dy_inv*(ys-ym)
  endif
  do n=1,np
   yp=pt(n,ic1)
   zp=pt(n,ic2)
   yp_loc=dy_inv*(yp-ym)
   if(yp > ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   pt(n,ic1)=yp_loc
   pt(n,ic2)=dz_inv*(zp-zm)
  end do
 case(5)       !y>0 z>0 corner
  ys=str_ygrid%smax
  if(ym>ys)then
   ximn=dyi_inv*atan(sy_rat*(ys-ym))
  else
   ximn=dy_inv*(ys-ym)
  endif
  zs=str_zgrid%smax
  if(zm>zs)then
   zimn=dzi_inv*atan(sz_rat*(zs-zm))
  else
   zimn=dz_inv*(zs-zm)
  endif
  do n=1,np
   yp=pt(n,ic1)
   zp=pt(n,ic2)
   yp_loc=dy_inv*(yp-ym)
   zp_loc=dz_inv*(zp-zm)
   if(yp > ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   if(zp > zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic1)=yp_loc
   pt(n,ic2)=zp_loc
  end do
 case(6)       !z>0
  zs=str_zgrid%smax
  if(zm>zs)then
   zimn=dzi_inv*atan(sz_rat*(zs-zm))
  else
   zimn=dz_inv*(zs-zm)
  endif
  do n=1,np
   yp=pt(n,ic1)
   pt(n,ic1)=dy_inv*(yp-ym)
   zp=pt(n,ic2)
   zp_loc=dz_inv*(zp-zm)
   if(zp > zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic2)=zp_loc
  end do
 case(7)       !y<0 z>0 corner
  ys=str_ygrid%smin
  ximn=-dyi_inv*atan(sy_rat*(ym-ys))
  zs=str_zgrid%smax
  if(zm>zs)then
   zimn=dzi_inv*atan(sz_rat*(zs-zm))
  else
   zimn=dz_inv*(zs-zm)
  endif
  do n=1,np
   yp=pt(n,ic1)
   zp=pt(n,ic2)
   yp_loc=ximn+dy_inv*(yp-ys)
   zp_loc=dz_inv*(zp-zm)
   if(yp < ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   if(zp > zs)zp_loc=zimn+dzi_inv*atan(sz_rat*(zp-zs))
   pt(n,ic1)=yp_loc
   pt(n,ic2)=zp_loc
  end do
 case(8)       !y<0
  ys=str_ygrid%smin
  ximn=-dyi_inv*atan(sy_rat*(ym-ys))
  do n=1,np
   yp=pt(n,ic1)
   zp=pt(n,ic2)
   yp_loc=ximn+dy_inv*(yp-ys)
   if(yp < ys)yp_loc=ximn+dyi_inv*atan(sy_rat*(yp-ys))
   pt(n,ic1)=yp_loc
   pt(n,ic2)=dz_inv*(zp-zm)
  end do
 end select
 end subroutine map3d_part_sind

 !========================================
 !    Linear and quadratic shape functions for grid-particle  connection
 !==================================================
 subroutine set_hint_pshape(spl,xx,ax,ind)
 integer,intent(in) :: spl
 real(dp),intent(in) :: xx
 real(dp),intent(out) :: ax(0:3)
 integer,intent(out) :: ind
 real(dp) :: sx,sx2,sx3
 !To half-integer grid points
 select case(spl)
 case(1)
  sx=xx+0.5
  ind=int(sx)
  ax(1)=sx-real(ind,dp)
  ax(0)=1.-ax(1)
 case(2)
  ind=int(xx)
  sx=xx-0.5-real(ind,dp)
  sx2=sx*sx
  ax(1)=0.75-sx2
  ax(2)=0.5*(0.25+sx2+sx)
  ax(0)=1.-ax(1)-ax(2)
 case(3)
  ind=int(xx+0.5)
  sx=xx-real(ind,dp)
  sx2=sx*sx
  sx3=sx2*sx
  ax(1)=two_third-sx2+0.5*sx3
  ax(2)=one_sixth+0.5*(sx+sx2-sx3)
  ax(3)=one_sixth*sx3
  ax(0)=1.-ax(1)-ax(2)-ax(3) ! to (i-1/2,i+1/2,i+3/2,i+5/2) half-int
 end select
 end subroutine set_hint_pshape
 !=======================
 subroutine set_int_pshape(spl,xx,ax,ind)
 integer,intent(in) :: spl
 real(dp),intent(in) :: xx
 real(dp),intent(out) :: ax(0:3)
 integer,intent(out) :: ind
 real(dp) :: sx,sx2,sx3
 !To integer grid points
 select case(spl)
 case(1)
  ind=int(xx)
  ax(1)=xx-real(ind,dp)
  ax(0)=1.-ax(1)
 case(2)
  ind=int(xx+0.5)
  sx=xx-real(ind,dp)
  sx2=sx*sx
  ax(1)=0.75-sx2
  ax(2)=0.5*(0.25+sx2+sx)
  ax(0)=1.-ax(1)-ax(2)
 case(3)
  ind=int(xx)
  sx=xx-real(ind,dp)
  sx2=sx*sx
  sx3=sx2*sx
  ax(1)=two_third-sx2+0.5*sx3
  ax(2)=one_sixth+0.5*(sx+sx2-sx3)
  ax(3)=one_sixth*sx3
  ax(0)=1.-ax(1)-ax(2)-ax(3)
 end select
 end subroutine set_int_pshape
 !===========================================
 !     Computes particle density charge on grid integer points
 !==============================================
 !DIR$ ATTRIBUTES INLINE :: qq_interpolate
!====================
 subroutine qq_density_spline(sh,x0,x1,a0,a1,ix0,ix1)
 real(sp),intent(in)  :: sh
 real(dp),intent(in)  :: x0,x1
 real(dp),intent(inout) :: a0(3),a1(3)
 integer, intent(out) :: ix0,ix1
 real(dp) :: xx,sx,sx2

 xx=sh+x0
 ix0=int(xx+0.5)
 sx=xx-real(ix0,dp)
 sx2=sx*sx
 a0(2)=0.75-sx2
 a0(3)=0.5*(0.25+sx2+sx)
 a0(1)=1.-a0(2)-a0(3)
 xx=sh+x1
 ix1=int(xx+0.5)
 sx=xx-real(ix1,dp)
 sx2=sx*sx
 a1(2)=0.75-sx2
 a1(3)=0.5*(0.25+sx2+sx)
 a1(1)=1.-a1(2)-a1(3)
 ix0=ix0-2
 ix1=ix1-2
 end subroutine qq_density_spline
!==========================================
 subroutine qlql_density_spline(sh,x0,x1,a0,a1,ah0,ah1,ix0,ix1,ih0,ih1)
 real(sp),intent(in)  :: sh
 real(dp),intent(in)  :: x0,x1
 real(dp),intent(inout) :: a0(3),a1(3),ah0(2),ah1(2)
 integer, intent(out) :: ix0,ix1,ih0,ih1
 real(dp) :: xx,sx,sx2

 xx=sh+x0
 ix0=int(xx+0.5)
 sx=xx-real(ix0,dp)
 sx2=sx*sx
 a0(2)=0.75-sx2
 a0(3)=0.5*(0.25+sx2+sx)
 a0(1)=1.-a0(2)-a0(3)

 ah0(2)=sx+0.5
 ah0(1)=1.-ah0(2)

 xx=sh+x1
 ix1=int(xx+0.5)
 sx=xx-real(ix1,dp)
 sx2=sx*sx
 a1(2)=0.75-sx2
 a1(3)=0.5*(0.25+sx2+sx)
 a1(1)=1.-a1(2)-a1(3)

 ah1(2)=sx+0.5
 ah1(1)=1.-ah1(2)

 ix0=ix0-2
 ih0=ix0
 ix1=ix1-2
 ih1=ix1
 end subroutine qlql_density_spline
!==========================================
!
 subroutine qq_interpolate(xp,ax1,axh,ay1,ayh,ix,iy,ixh,iyh)
 real(dp),intent(in)  :: xp(:)
 real(dp),intent(inout) :: ax1(:),axh(:),ay1(:),ayh(:)
 integer, intent(out) :: ix,iy,ixh,iyh
 real(dp) :: xx,sx,sx2

 xx=shx+xp(1)
 ix=int(xx+0.5)
 sx=xx-real(ix,dp)
 sx2=sx*sx
 ax1(2)=0.75-sx2
 ax1(3)=0.5*(0.25+sx2+sx)
 ax1(1)=1.-ax1(2)-ax1(3)

 ixh=int(xx)
 sx=xx-0.5-real(ixh,dp)
 sx2=sx*sx
 axh(2)=0.75-sx2
 axh(3)=0.5*(0.25+sx2+sx)
 axh(1)=1.-axh(3)-axh(2)

 xx=shy+xp(2)

 iy=int(xx+0.5)
 sx=xx-real(iy,dp)
 sx2=sx*sx
 ay1(2)=0.75-sx2
 ay1(3)=0.5*(0.25+sx2+sx)
 ay1(1)=1.-ay1(3)-ay1(2)

 iyh=int(xx)
 sx=xx-0.5-real(iyh,dp)
 sx2=sx*sx
 ayh(2)=0.75-sx2
 ayh(3)=0.5*(0.25+sx2+sx)
 ayh(1)=1.-ayh(3)-ayh(2)

 ix=ix-2
 iy=iy-2
 ixh=ixh-2
 iyh=iyh-2
 end subroutine qq_interpolate

 !DIR$ ATTRIBUTES INLINE :: ql_interpolate
 subroutine ql_interpolate(xp,ax1,axh,ay1,ayh,ix,iy,ixh,iyh)
 real(dp),intent(in)  :: xp(:)
 real(dp),intent(inout) :: ax1(:),axh(:),ay1(:),ayh(:)
 integer, intent(out) :: ix,iy,ixh,iyh
 real(dp) :: xx,sx,sx2

 xx=shx+xp(1)
 ix=int(xx+0.5)
 sx=xx-real(ix,dp)
 sx2=sx*sx
 ax1(2)=0.75-sx2
 ax1(3)=0.5*(0.25+sx2+sx)
 ax1(1)=1.-ax1(2)-ax1(3)

 axh(2)=sx+0.5
 axh(1)=1.-axh(2)

 xx=shy+xp(2)

 iy=int(xx+0.5)
 sx=xx-real(iy,dp)
 sx2=sx*sx
 ay1(2)=0.75-sx2
 ay1(3)=0.5*(0.25+sx2+sx)
 ay1(1)=1.-ay1(3)-ay1(2)

 ayh(2)=sx+0.5
 ayh(1)=1.-ayh(2)

 ix=ix-2
 iy=iy-2
 ixh=ix
 iyh=iy
 end subroutine ql_interpolate

 !DIR$ ATTRIBUTES INLINE :: set_local_positions
!================================
 subroutine set_local_positions(pt_loc,n1,np,ns,ndm,xmn,ymn,zmn)
 real(dp),intent(inout) :: pt_loc(:,:)
 integer,intent(in) :: n1,np,ns,ndm
 real(dp),intent(in) :: xmn,ymn,zmn
 integer :: n
 !=========================
 ! for ic=1  computes gamma
 ! for ic=2  computes velocities
 do n=n1,np
  pt_loc(n,1)=dx_inv*(pt_loc(n,1)-xmn)
 end do
 select case(ndm)
 case(2)
  if(ns==0)then
   do n=n1,np
    pt_loc(n,2)=dy_inv*(pt_loc(n,2)-ymn)              !
   end do
  else
   call map2dy_part_sind(np,ns,2,ymn,pt_loc)
  endif
 case(3)
  if(ns==0)then
   do n=n1,np
    pt_loc(n,2)=dy_inv*(pt_loc(n,2)-ymn)
    pt_loc(n,3)=dz_inv*(pt_loc(n,3)-zmn)
   end do
  else
   call map3d_part_sind(pt_loc,np,ns,2,3,ymn,zmn)
  endif
 end select
 end subroutine set_local_positions
!================
 subroutine set_part_gamma(pt_loc,n1,np,njc)
 real(dp),intent(inout) :: pt_loc(:,:)
 integer,intent(in) :: n1,np,njc
 integer :: n
 real(dp) :: gam2


  select case(njc)
  case(2)
   do n=n1,np
    gam2=pt_loc(n,3)*pt_loc(n,3)+pt_loc(n,4)*pt_loc(n,4)
    pt_loc(n,3)=sqrt(1.+gam2)
   end do
  case(3)
   do n=n1,np
    gam2=pt_loc(n,4)*pt_loc(n,4)+pt_loc(n,5)*pt_loc(n,5)+&
                                pt_loc(n,6)*pt_loc(n,6)
    pt_loc(n,4)=sqrt(1.+gam2)
   end do
  end select
!============exit gamma
 end subroutine set_part_gamma
!===================
 subroutine set_part_velocities(pt_loc,n1,np,njc)
 real(dp),intent(inout) :: pt_loc(:,:)
 integer,intent(in) :: n1,np,njc
 integer :: n
 real(dp) :: gam2,gam_inv

  select case(njc)
  case(2)
   do n=n1,np
    gam2=pt_loc(n,3)*pt_loc(n,3)+pt_loc(n,4)*pt_loc(n,4)
    gam_inv=1./sqrt(1.+gam2)
    pt_loc(n,3)=gam_inv*pt_loc(n,3)
    pt_loc(n,4)=gam_inv*pt_loc(n,4)
   end do
  case(3)
   do n=n1,np
    gam2=pt_loc(n,4)*pt_loc(n,4)+pt_loc(n,5)*pt_loc(n,5)+&
                                pt_loc(n,6)*pt_loc(n,6)
    gam_inv=1./sqrt(1.+gam2)
    pt_loc(n,4)=gam_inv*pt_loc(n,4)
    pt_loc(n,5)=gam_inv*pt_loc(n,5)
    pt_loc(n,6)=gam_inv*pt_loc(n,6)
   end do
  end select
 end subroutine set_part_velocities
 !==========================
 subroutine set_grid_charge(sp_loc,pt,den,np,ndm,n_st,ic,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 real(dp),intent(inout) :: den(:,:,:,:)
 integer,intent(in) :: np,ndm,n_st,ic
 real(dp),intent(in) :: xmn,ymn,zmn
 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: ax0(0:3),ay0(0:3),az0(0:3),xp(3)
 integer :: i,j,k,i1,j1,k1,i2,j2,k2,n,ch,spl
 real(sp) :: wght
 !======================
 ax0(0:3)=zero_dp;ay0(0:3)=zero_dp
 az0(0:3)=zero_dp
 spl=2
 select case(ndm)
 case(1)
  j2=1
  do n=1,np
   xp(1)=dx_inv*(sp_loc%part(n,1)-xmn)
   wgh_cmp=sp_loc%part(n,5)
   wght=charge*wgh
   xx=shx+xp(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)
   ax0(0:2)=wght*ax0(0:2)
   i=i-1
   do i1=0,2
    i2=i+i1
    den(i2,j2,1,ic)=den(i2,j2,1,ic)+ax0(i1)
   end do
  end do
 case(2)
  ch=5
  do n=1,np
   pt(n,1:2)=sp_loc%part(n,1:2)
   wgh_cmp=sp_loc%part(n,5)
   pt(n,4)=charge*wgh
  end do
  call set_local_positions(pt,1,np,n_st,2,xmn,ymn,zmn)
!==========================
  do n=1,np
   wght=real(pt(n,4),sp)
   xp(1:2)=pt(n,1:2)
   xx=shx+xp(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)
   ax0(0:2)=wght*ax0(0:2)
   i=i-1

   xx=shy+xp(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay0(1)=0.75-sx2
   ay0(2)=0.5*(0.25+sx2+sx)
   ay0(0)=1.-ay0(1)-ay0(2)
   j=j-1
   do j1=0,2
    j2=j+j1
    do i1=0,2
     i2=i+i1
     dvol=ax0(i1)*ay0(j1)
     den(i2,j2,1,ic)=den(i2,j2,1,ic)+dvol
    end do
   end do
  end do
 case(3)
  ch=7
  do n=1,np
   pt(n,1:3)=sp_loc%part(n,1:3)
   pt(n,4)=sp_loc%part(n,ch)
   wgh_cmp=pt(n,4)
   wght=charge*wgh
   pt(n,4)=wght
  end do
  call set_local_positions(pt,1,np,n_st,3,xmn,ymn,zmn)
  do n=1,np
   xp(1:3)=pt(n,1:3)
   wght=real(pt(n,4),sp)
   xx=shx+xp(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)
   ax0(0:2)=wght*ax0(0:2)

   xx=shy+xp(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay0(1)=0.75-sx2
   ay0(2)=0.5*(0.25+sx2+sx)
   ay0(0)=1.-ay0(1)-ay0(2)

   xx=shz+xp(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az0(1)=0.75-sx2
   az0(2)=0.5*(0.25+sx2+sx)
   az0(0)=1.-az0(1)-az0(2)
   !---------------
   i=i-1
   j=j-1
   k=k-1
   do k1=0,spl
    k2=k+k1
    do j1=0,spl
     j2=j+j1
     dvol=az0(k1)*ay0(j1)
     do i1=0,spl
      i2=i+i1
      den(i2,j2,k2,ic)=den(i2,j2,k2,ic)+ax0(i1)*dvol
     end do
    end do
   end do
  end do
  ! data on [3:n1+2,3:n2+2,3:n3+2,ic]
 end select
 end subroutine set_grid_charge
 !+++++++++++++++++++++++++++++++
 subroutine set_grid_charge_and_Jx(sp_loc,pt,den,np,ndm,dt_loc,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 real(dp),intent(inout) :: den(:,:,:,:)
 integer,intent(in) :: np,ndm
 real(dp),intent(in) :: dt_loc,xmn,ymn,zmn
 real(dp) :: dvol,dvol1,dvol2,gam
 real(dp) :: axh0(2),axh1(2)
 real(dp) :: ax0(3),ay0(3),ax1(3),ay1(3),az0(3),az1(3)
 integer :: i,j,i0,j0,i1,j1,i2,j2,k,k0,k1,k2,n
 integer :: ch,ih0,ih
 real(dp) :: xp1(3),xp0(3),pp(3)
 real(sp) :: wght
 !======================
 ax0=zero_dp;ay0=zero_dp
 ax1=zero_dp;ay1=zero_dp
 axh0=zero_dp; axh1=zero_dp
!========
 ch=size(sp_loc%part,2)
 select case(ndm)   !only two or three dimensional conf. considered
 case(2)
  do n=1,np
   pt(n,1:2)=sp_loc%part(n,1:2)
   pp(1:2)=sp_loc%part(n,3:4)
   gam=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2))!gamma
   wgh_cmp=sp_loc%part(n,ch)
   wght=real(0.5*charge*wgh,sp)
   pp(1:2)=pp(1:2)/gam                 !velocities
   pt(n,3)=pt(n,1)-dt_loc*pp(1)           !old positions
   pt(n,4)=pt(n,2)-dt_loc*pp(2)
   xp1(1)=dx_inv*(pt(n,1)-xmn)       !new positions
   xp1(2)=dy_inv*(pt(n,2)-ymn)       !new positions
   xp0(1)=dx_inv*(pt(n,3)-xmn)       !old positions
   xp0(2)=dy_inv*(pt(n,4)-ymn)       !old positions

   call qlql_density_spline(shx,xp0(1),xp1(1),ax0,ax1,axh0,axh1,i0,i,ih0,ih)
   call qq_density_spline(shy,xp0(2),xp1(2),ay0,ay1,j0,j)
   !=============================
   do j1=1,3
    j2=j0+j1
    dvol=wght*ay0(j1)
    dvol1=dvol*pp(1)
    do i1=1,3
     i2=i0+i1
     den(i2,j2,1,1)=den(i2,j2,1,1)+dvol*ax0(i1)    !density x^n
   end do
    do i1=1,2
     i2=ih0+i1
     den(i2,j2,1,2)=den(i2,j2,1,2)+dvol1*axh0(i1)  !  Jx
  end do
     j2=j+j1
    dvol=wght*ay1(j1)
    dvol1=dvol*pp(1)
    do i1=1,3
      i2=i+i1
     den(i2,j2,1,1)=den(i2,j2,1,1)+dvol*ax1(i1)    !density x^{n+1}
    end do
    do i1=1,2
     i2=ih+i1
     den(i2,j2,1,2)=den(i2,j2,1,2)+dvol1*axh1(i1)  ! Jx
     end do
    end do
   end do
   !=============
 case(3)
  ch=7
  az0=zero_dp
  az1=zero_dp
   do n=1,np
   pt(n,1:3)=sp_loc%part(n,1:3)
   pp(1:3)=sp_loc%part(n,4:6)
   gam=sqrt(1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3))!gamma
   wgh_cmp=sp_loc%part(n,ch)
   wght=real(0.5*charge*wgh,sp)
   pp(1:3)=pp(1:3)/gam                 !velocities
   pt(n,4)=pt(n,1)-dt_loc*pp(1)           !old positions
   pt(n,5)=pt(n,2)-dt_loc*pp(2)
   pt(n,6)=pt(n,3)-dt_loc*pp(3)
   xp1(1)=dx_inv*(pt(n,1)-xmn)       !new positions
   xp1(2)=dy_inv*(pt(n,2)-ymn)
   xp1(3)=dz_inv*(pt(n,3)-zmn)
   xp0(1)=dx_inv*(pt(n,4)-xmn)       !old positions
   xp0(2)=dy_inv*(pt(n,5)-ymn)
   xp0(3)=dz_inv*(pt(n,6)-zmn)
   call qlql_density_spline(shx,xp0(1),xp1(1),ax0,ax1,axh0,axh1,i0,i,ih0,ih)
   call qq_density_spline(shy,xp0(2),xp1(2),ay0,ay1,j0,j)
   call qq_density_spline(shz,xp0(3),xp1(3),az0,az1,k0,k)
!===================================
   do k1=1,3
    k2=k0+k1
    dvol2=az0(k1)
    do j1=1,3
     j2=j0+j1
     dvol=wght*ay0(j1)*dvol2
     dvol1=dvol*pp(1)
     do i1=1,3
      i2=i0+i1
      den(i2,j2,k2,1)=den(i2,j2,k2,1)+dvol*ax0(i1)    !density x^n
     end do
     do i1=1,2
      i2=ih0+i1
      den(i2,j2,k2,2)=den(i2,j2,k2,2)+dvol1*axh0(i1)  !Jx
    end do
   end do
  end do
   do k1=1,3
    k2=k+k1
    dvol2=az1(k1)
    do j1=1,3
     j2=j+j1
     dvol=wght*ay1(j1)*dvol2
     dvol1=dvol*pp(1)
     do i1=1,3
      i2=i+i1
      den(i2,j2,k2,1)=den(i2,j2,k2,1)+dvol*ax1(i1)    !density x^{n+1}
     end do
     do i1=1,2
      i2=ih+i1
      den(i2,j2,k2,2)=den(i2,j2,k2,2)+dvol1*axh1(i1)  !density -Jx
     end do
    end do
   end do
  end do
  !  Exit jc(1)=rho(i,j,k) jc(2)=Jx(i+1/2,j,k)
 end select
 !+++++++++++++++++++++++++++++++
 end subroutine set_grid_charge_and_Jx
 !==========================
 subroutine set_grid_den_env_energy(sp_loc,pt,eden,np,ndm,njc,n_st,icp,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 real(dp),intent(inout) :: eden(:,:,:,:)
 integer,intent(in) :: np,ndm,njc,n_st,icp
 real(dp),intent(in) :: xmn,ymn,zmn
 real(dp) :: xx,sx,sx2,dvol,gam2,gam_p
 real(dp) :: ax0(0:2),ay0(0:2),az0(0:2),xp(3),pp(3)
 integer :: i,j,k,i1,j1,k1,i2,j2,k2,n,ch,spl
 !======================
 !   Computes eden(grid,1)= n/n_0 and eden(grid,2)=<gam-1}n>/n_0
 !================================================
 ax0(0:2)=0.0;ay0(0:2)=0.0
 az0(0:2)=0.0
 spl=2
 if(np<1)return
 select case(ndm)
 case(1)
  ch=5
  j2=1
  do n=1,np
   xp(1)=dx_inv*(sp_loc%part(n,1)-xmn)
   pp(1:2)=sp_loc%part(n,3:4)
   gam2=pp(1)*pp(1)+pp(2)*pp(2)
   wgh_cmp=sp_loc%part(n,ch)
   xx=shx+xp(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)
   i=i-1
   do i1=0,2
    i2=i+i1
    dvol=ax0(i1)
    gam2=gam2+dvol*eden(i2,j2,1,icp)
   end do
   gam_p=sqrt(1.+gam2)
   ax0(0:2)=wgh*ax0(0:2)
   do i1=0,2
    i2=i+i1
    dvol=ax0(i1)
    eden(i2,j2,1,1)=eden(i2,j2,1,1)+dvol*charge
    eden(i2,j2,1,2)=eden(i2,j2,1,2)+(gam_p-1)*dvol
   end do
  end do
 case(2)
  ch=size(sp_loc%part,2)
  do n=1,np
   pt(n,1:ch)=sp_loc%part(n,1:ch)
  end do
  call set_local_positions(pt,1,np,n_st,ndm,xmn,ymn,zmn)
  if(njc==2)then
   do n=1,np
    pp(1:2)=sp_loc%part(n,3:4)
    gam2=pp(1)*pp(1)+pp(2)*pp(2)
    xp(1:2)=pt(n,1:2)
    wgh_cmp=pt(n,ch)
    xx=shx+xp(1)
    i=int(xx+0.5)
    sx=xx-real(i,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)
    i=i-1

    xx=shy+xp(2)
    j=int(xx+0.5)
    sx=xx-real(j,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)
    j=j-1
    do j1=0,2
     j2=j+j1
     do i1=0,2
      i2=i+i1
      dvol=ax0(i1)*ay0(j1)
      gam2=gam2+dvol*eden(i2,j2,1,icp)
     end do
    end do
    gam_p=sqrt(1.+gam2)
    ax0(0:2)=wgh*ax0(0:2)  !weights are inside
    do j1=0,2
     j2=j+j1
     do i1=0,2
      i2=i+i1
      dvol=ax0(i1)*ay0(j1)
      eden(i2,j2,1,1)=eden(i2,j2,1,1)+dvol*charge
      eden(i2,j2,1,2)=eden(i2,j2,1,2)+(gam_p-1.)*dvol
     end do
    end do
   end do
  endif
  if(njc==3)then
   do n=1,np
    pp(1:3)=sp_loc%part(n,4:6)
    gam2=pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)
    xp(1:2)=pt(n,1:2)
    wgh_cmp=pt(n,ch)

    xx=shx+xp(1)
    i=int(xx+0.5)
    sx=xx-real(i,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)
    i=i-1

    xx=shy+xp(2)
    j=int(xx+0.5)
    sx=xx-real(j,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)
    j=j-1
!============ adds [A^2/2]_p contribution
    do j1=0,2
     j2=j+j1
     do i1=0,2
      i2=i+i1
      dvol=ax0(i1)*ay0(j1)
      gam2=gam2+dvol*eden(i2,j2,1,icp)
     end do
    end do
    gam_p=sqrt(1.+gam2)
    ax0(0:2)=wgh*ax0(0:2)
    do j1=0,2
     j2=j+j1
     do i1=0,2
      i2=i+i1
      dvol=ax0(i1)*ay0(j1)
      eden(i2,j2,1,1)=eden(i2,j2,1,1)+dvol*charge
      eden(i2,j2,1,2)=eden(i2,j2,1,2)+(gam_p-1.)*dvol
     end do
    end do
   end do
  endif
 case(3)
  ch=7
  do n=1,np
   pt(n,1:ch)=sp_loc%part(n,1:ch)
  end do
  call set_local_positions(pt,1,np,n_st,3,xmn,ymn,zmn)
  do n=1,np
   pp(1:3)=sp_loc%part(n,4:6)
   gam2=pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)
   xp(1:3)=pt(n,1:3)
   wgh_cmp=pt(n,ch)
   xx=shx+xp(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)

   xx=shy+xp(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay0(1)=0.75-sx2
   ay0(2)=0.5*(0.25+sx2+sx)
   ay0(0)=1.-ay0(1)-ay0(2)

   xx=shz+xp(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az0(1)=0.75-sx2
   az0(2)=0.5*(0.25+sx2+sx)
   az0(0)=1.-az0(1)-az0(2)
   !---------------
   i=i-1
   j=j-1
   k=k-1
   do k1=0,spl
    k2=k+k1
    do j1=0,spl
     j2=j+j1
     dvol=az0(k1)*ay0(j1)
     do i1=0,spl
      i2=i+i1
      gam2=gam2+ax0(i1)*dvol*eden(i2,j2,k2,icp)
     end do
    end do
   end do
   gam_p=sqrt(1.+gam2)
   ax0(0:2)=wgh*ax0(0:2)
   do k1=0,spl
    k2=k+k1
    do j1=0,spl
     j2=j+j1
     dvol=az0(k1)*ay0(j1)
     do i1=0,spl
      i2=i+i1
      eden(i2,j2,k2,1)=eden(i2,j2,k2,1)+ax0(i1)*dvol*charge
      eden(i2,j2,k2,2)=eden(i2,j2,k2,2)+(gam_p-1.)*ax0(i1)*dvol
     end do
    end do
   end do
  end do
 end select
 !+++++++++++++++++++++++++++++++
 end subroutine set_grid_den_env_energy
!=================================================
 subroutine set_grid_den_energy(sp_loc,pt,eden,np,ndm,njc,n_st,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 real(dp),intent(inout) :: eden(:,:,:,:)
 integer,intent(in) :: np,ndm,njc,n_st
 real(dp),intent(in) :: xmn,ymn,zmn
 real(dp) :: xx,sx,sx2,dvol,gam
 real(dp) :: ax0(0:2),ay0(0:2),az0(0:2),xp(3),pp(3)
 integer :: i,j,k,i1,j1,k1,i2,j2,k2,n,ch,spl
 !======================
 !   Computes eden(grid,1)= n/n_0 and eden(grid,2)=<gam-1}n>/n_0
 !================================================
 ax0(0:2)=zero_dp;ay0(0:2)=zero_dp
 az0(0:2)=zero_dp
 spl=2
 if(np<1)return
 select case(ndm)
 case(1)
  ch=5
  j2=1
  do n=1,np
   xp(1)=dx_inv*(sp_loc%part(n,1)-xmn)
   pp(1:2)=sp_loc%part(n,3:4)
   gam=sqrt(pp(1)*pp(1)+pp(2)*pp(2)+1.)
   wgh_cmp=sp_loc%part(n,ch)
   xx=shx+xp(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)
   ax0(0:2)=wgh*ax0(0:2)
   i=i-1
   do i1=0,2
    i2=i+i1
    dvol=ax0(i1)
    eden(i2,j2,1,1)=eden(i2,j2,1,1)+dvol*charge
    eden(i2,j2,1,2)=eden(i2,j2,1,2)+(gam-1.)*dvol
   end do
  end do
 case(2)
  ch=size(sp_loc%part,2)
  do n=1,np
   pt(n,1:ch)=sp_loc%part(n,1:ch)
  end do
  call set_local_positions(pt,1,np,n_st,ndm,xmn,ymn,zmn)
  call set_part_gamma(pt,1,np,njc)
  if(njc==2)then
   do n=1,np
    xp(1:2)=pt(n,1:2)
    gam=pt(n,3)
    wgh_cmp=pt(n,ch)
    xx=shx+xp(1)
    i=int(xx+0.5)
    sx=xx-real(i,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)
    ax0(0:2)=wgh*ax0(0:2)  !weights are inside
    i=i-1

    xx=shy+xp(2)
    j=int(xx+0.5)
    sx=xx-real(j,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)
    j=j-1
    do j1=0,2
     j2=j+j1
     do i1=0,2
      i2=i+i1
      dvol=ax0(i1)*ay0(j1)
      eden(i2,j2,1,1)=eden(i2,j2,1,1)+dvol*charge
      eden(i2,j2,1,2)=eden(i2,j2,1,2)+(gam-1.)*dvol
     end do
    end do
   end do
  endif
  if(njc==3)then
   do n=1,np
    xp(1:2)=pt(n,1:2)
    gam=pt(n,4)
    wgh_cmp=pt(n,ch)

    xx=shx+xp(1)
    i=int(xx+0.5)
    sx=xx-real(i,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)
    ax0(0:2)=wgh*ax0(0:2)
    i=i-1

    xx=shy+xp(2)
    j=int(xx+0.5)
    sx=xx-real(j,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)
    j=j-1
    do j1=0,2
     j2=j+j1
     do i1=0,2
      i2=i+i1
      dvol=ax0(i1)*ay0(j1)
      eden(i2,j2,1,1)=eden(i2,j2,1,1)+dvol*charge
      eden(i2,j2,1,2)=eden(i2,j2,1,2)+(gam-1.)*dvol
     end do
    end do
   end do
  endif

 case(3)
  ch=7
  do n=1,np
   pt(n,1:ch)=sp_loc%part(n,1:ch)
  end do
  call set_local_positions(pt,1,np,n_st,3,xmn,ymn,zmn)
  call set_part_gamma(pt,1,np,3)
  do n=1,np
   xp(1:3)=pt(n,1:3)
   wgh_cmp=pt(n,ch)
   gam=pt(n,4)
   xx=shx+xp(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)
   ax0(0:2)=wgh*ax0(0:2)

   xx=shy+xp(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay0(1)=0.75-sx2
   ay0(2)=0.5*(0.25+sx2+sx)
   ay0(0)=1.-ay0(1)-ay0(2)

   xx=shz+xp(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az0(1)=0.75-sx2
   az0(2)=0.5*(0.25+sx2+sx)
   az0(0)=1.-az0(1)-az0(2)
   !---------------
   i=i-1
   j=j-1
   k=k-1
   do k1=0,spl
    k2=k+k1
    do j1=0,spl
     j2=j+j1
     dvol=az0(k1)*ay0(j1)
     do i1=0,spl
      i2=i+i1
      eden(i2,j2,k2,1)=eden(i2,j2,k2,1)+ax0(i1)*dvol*charge
      eden(i2,j2,k2,2)=eden(i2,j2,k2,2)+(gam-1.)*ax0(i1)*dvol
     end do
    end do
   end do
  end do
 end select
 !+++++++++++++++++++++++++++++++
 end subroutine set_grid_den_energy
 !==============================================
 !========= SECTION FOR ACCELERATION FIELDS ASSIGNEMENT
 !==========================================
 subroutine set_part1d_acc(ef,sp_loc,pt,np,ndf,xmn)

 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndf
 real(dp),intent(in) :: xmn

 real(dp) :: xx,sx,sx2
 real(dp) :: axh(0:3),xp1(3)
 real(dp) :: ax1(0:3),ap(6)
 integer :: i,ih,i1,i2,j2,n
 !=====================
 !================================
 select case(ndf)
 case(3)
  j2=1
  do n=1,np
   ap(1:3)=zero_dp
   xp1(1)=sp_loc%part(n,1)    !the current particle positions
   xx=shx+dx_inv*(xp1(1)-xmn)

   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   i=i-1
   ih=ih-1
   do i1=0,2
    i2=i1+ih
    ap(1)=ap(1)+axh(i1)*ef(i2,j2,1,1)      !Ex(i+1/2)
    ap(3)=ap(3)+axh(i1)*ef(i2,j2,1,3)      !Bz(i+1/2)
    i2=i+i1
    ap(2)=ap(2)+ax1(i1)*ef(i2,j2,1,2)      !Ey(i)
   end do
   pt(n,1:3)=ap(1:3)
  end do
  !========================
 case(6)
  j2=1
  do n=1,np
   ap(1:6)=zero_dp
   xp1(1)=sp_loc%part(n,1)    !the current particle positions
   xx=shx+dx_inv*(xp1(1)-xmn)

   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)     !For fields located on cell centers

   ! axh(1)=1.-sx2
   ! axh(2)=0.5*(sx2+sx)

   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)        !For fields located on node points
   i=i-1
   ih=ih-1
   do i1=0,2
    i2=i1+ih
    ap(1)=ap(1)+axh(i1)*ef(i2,j2,1,1) !Ex
    ap(5)=ap(5)+axh(i1)*ef(i2,j2,1,5) !By
    ap(6)=ap(6)+axh(i1)*ef(i2,j2,1,6) !Bz
   end do
   do i1=0,2
    i2=i+i1
    ap(2)=ap(2)+ax1(i1)*ef(i2,j2,1,2) !Ey
    ap(3)=ap(3)+ax1(i1)*ef(i2,j2,1,3) !Ez
    ap(4)=ap(4)+ax1(i1)*ef(i2,j2,1,4) !Bx
   end do
   pt(n,1:6)=ap(1:6)
  end do
 end select
 end subroutine set_part1d_acc
 !===========================
 subroutine set_part2d_acc(ef,sp_loc,pt,np,ndf,s_ind,xmn,ymn)

 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndf,s_ind
 real(dp),intent(in) :: xmn,ymn

 real(dp) :: dvol,dvol1
 real(dp) :: axh(3),ayh(3),xp1(3)
 real(dp) :: ax1(3),ay1(3),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,n

 !================================
 ! USES quadratic splines along each coordinate
 !=======================================
 ! ndf is the number of field component
 ax1=zero_dp;ay1=zero_dp
 axh=zero_dp;ayh=zero_dp
 dvol=zero_dp
 do n=1,np
  pt(n,1:3)=sp_loc%part(n,1:3)
 end do
 call set_local_positions(pt,1,np,s_ind,2,xmn,ymn,dvol)
 select case(ndf)
 case(3)
  do n=1,np
   ap(1:3)=zero_dp
   xp1(1:2)=pt(n,1:2)
   call qq_interpolate(xp1,ax1,axh,ay1,ayh,i,j,ih,jh)

   do j1=1,3
    j2=j+j1
    dvol=ay1(j1)
    do i1=1,3
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(1)=ap(1)+dvol1*ef(i2,j2,1,1) !Ex(i+1/2,j)ap(1)=sum_{i,j}S_{i,j}(x_p,y_p)
    end do
    j2=jh+j1
    dvol=ayh(j1)
    do i1=1,3
     i2=i+i1
     dvol1=ax1(i1)*dvol
     ap(2)=ap(2)+dvol1*ef(i2,j2,1,2) !Ey(i,j+1/2)
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(3)=ap(3)+dvol1*ef(i2,j2,1,3) !Bz(i+1/2,j+1/2)
    end do
   end do
   pt(n,1:3)=ap(1:3)
  end do
  !==============
 case(6)
  !=====================
  do n=1,np
   ap(1:6)=zero_dp
   xp1(1:2)=pt(n,1:2)
   call qq_interpolate(xp1,ax1,axh,ay1,ayh,i,j,ih,jh)

   do j1=1,3
    j2=j+j1
    dvol=ay1(j1)
    do i1=1,3
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(1)=ap(1)+dvol1*ef(i2,j2,1,1) !Ex(i+1/2,j)
     ap(5)=ap(5)+dvol1*ef(i2,j2,1,5) !By(i+1/2,j)
     i2=i1+i
     dvol1=ax1(i1)*dvol
     ap(3)=ap(3)+dvol1*ef(i2,j2,1,3) !Ez(i,j)
    end do
    j2=jh+j1
    dvol=ayh(j1)
    do i1=1,3
     i2=i+i1
     dvol1=ax1(i1)*dvol
     ap(2)=ap(2)+dvol1*ef(i2,j2,1,2) !Ey(i,j+1/2)
     ap(4)=ap(4)+dvol1*ef(i2,j2,1,4) !Bx(i,j+1/2)
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(3)=ap(3)+dvol1*ef(i2,j2,1,3) !Bz(i+1/2,j+1/2)
     ap(6)=ap(6)+dvol1*ef(i2,j2,1,6) !Bz(i+1/2,j+1/2)
    end do
   end do
   pt(n,1:6)=ap(1:6)
  end do
 end select
 !=====================
 end subroutine set_part2d_acc
 !=============================
 subroutine set_part2d_hcell_acc(ef,sp_loc,pt,np,ndf,s_ind,xmn,ymn)

 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndf,s_ind
 real(dp),intent(in) :: xmn,ymn

 real(dp) :: sx,dvol,dvol1
 real(dp) :: axh(3),ayh(3),xp1(3)
 real(dp) :: ax1(3),ay1(3),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,n
 !================================
 ! Uses quadratic or linear shapes depending on staggering
 ! ndf is the number of field component
 ax1=zero_dp;ay1=zero_dp
 axh=zero_dp;ayh=zero_dp
 xp1=zero_dp
 sx=zero_dp
 do n=1,np
  pt(n,1:3)=sp_loc%part(n,1:3)
 end do
 call set_local_positions(pt,1,np,s_ind,2,xmn,ymn,sx)

 select case(ndf)     !Field components
 case(3)
  do n=1,np
   ap(1:3)=zero_dp
   xp1(1:2)=pt(n,1:2)
   call qq_interpolate(xp1,ax1,axh,ay1,ayh,i,j,ih,jh)

   do j1=1,3
    j2=j+j1
    dvol=ay1(j1)
    do i1=1,3
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(1)=ap(1)+dvol1*ef(i2,j2,1,1) !Ex(i+1/2,j)
    end do
   end do
   do j1=1,3
    j2=jh+j1
    dvol=ayh(j1)
    do i1=1,3
     i2=i+i1
     dvol1=ax1(i1)*dvol
     ap(2)=ap(2)+dvol1*ef(i2,j2,1,2) !Ey(i,j+1/2)
    end do
    do i1=1,3
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(3)=ap(3)+dvol1*ef(i2,j2,1,3) !Bz(i+1/2,j+1/2)
    end do
   end do
   pt(n,1:3)=ap(1:3)
  end do
  !==============
 case(6)
  !=====================
  do n=1,np
   ap(1:6)=zero_dp
   xp1(1:2)=pt(n,1:2)
   call qq_interpolate(xp1,ax1,axh,ay1,ayh,i,j,ih,jh)

   do j1=1,3
    j2=j+j1
    dvol=ay1(j1)
    do i1=1,3
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(1)=ap(1)+dvol1*ef(i2,j2,1,1) !Ex(i+1/2,j)
     ap(5)=ap(5)+dvol1*ef(i2,j2,1,5) !By(i+1/2,j)
    end do
    do i1=1,3
     i2=i1+i
     dvol1=ax1(i1)*dvol
     ap(3)=ap(3)+dvol1*ef(i2,j2,1,3) !Ez(i,j,k+1/2)
    end do
   end do
   do j1=1,3
    j2=jh+j1
    dvol=ayh(j1)
    do i1=1,3
     i2=i+i1
     dvol1=ax1(i1)*dvol
     ap(2)=ap(2)+dvol1*ef(i2,j2,1,2) !Ey(i,j+1/2)
     ap(4)=ap(4)+dvol1*ef(i2,j2,1,4) !Bx(i,j+1/2)
    end do
    do i1=1,3
     i2=i1+ih
     dvol1=axh(i1)*dvol
     ap(6)=ap(6)+dvol1*ef(i2,j2,1,6) !Bz(i+1/2,j+1/2)
    end do
   end do
   pt(n,1:6)=ap(1:6)
  end do
 end select
 !=====================
 end subroutine set_part2d_hcell_acc
 !====================================
 subroutine set_part3d_acc(ef,sp_loc,pt,np,s_ind,xmn,ymn,zmn)

 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,s_ind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(3),ayh(3),xp1(3)
 real(dp) :: ax1(3),ay1(3),azh(3),az1(3),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! USES quadratic splines along each coordinate
 !====================================
 !=============================================================
 ! particle indexing : i=1,....,nx => weights 1,.....nx+2
 ! Fields data[1:n1+2,1:n2+2,1:n3+2] ax(0)=> data(i), ax(1)=> data(i+1) ax(3)=>data(i+2)
 !===================================================
 ax1=zero_dp;ay1=zero_dp
 az1=zero_dp;azh=zero_dp
 axh=zero_dp;ayh=zero_dp
 pt(1:np,1:3)=sp_loc%part(1:np,1:3)
 call set_local_positions(pt,1,np,s_ind,3,xmn,ymn,zmn)
 do n=1,np
  ap(1:6)=zero_dp
  xp1(1:3)=pt(n,1:3)    !the current particle positions
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(2)=0.75-sx2
  ax1(3)=0.5*(0.25+sx2+sx)
  ax1(1)=1.-ax1(2)-ax1(3)

  ih=int(xx)
  sx=xx-0.5-real(ih,dp)
  sx2=sx*sx
  axh(2)=0.75-sx2
  axh(3)=0.5*(0.25+sx2+sx)
  axh(1)=1.-axh(3)-axh(2)

  xx=shx+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(2)=0.75-sx2
  ay1(3)=0.5*(0.25+sx2+sx)
  ay1(1)=1.-ay1(3)-ay1(2)

  jh=int(xx)
  sx=xx-0.5-real(jh,dp)
  sx2=sx*sx
  ayh(2)=0.75-sx2
  ayh(3)=0.5*(0.25+sx2+sx)
  ayh(1)=1.-ayh(3)-ayh(2)

  xx=shz+xp1(3)
  k=int(xx+0.5)
  sx=xx-real(k,dp)
  sx2=sx*sx
  az1(2)=0.75-sx2
  az1(3)=0.5*(0.25+sx2+sx)
  az1(1)=1.-az1(3)-az1(2)

  kh=int(xx)
  sx=xx-0.5-real(kh,dp)
  sx2=sx*sx
  azh(2)=0.75-sx2
  azh(3)=0.5*(0.25+sx2+sx)
  azh(1)=1.-azh(3)-azh(2)

  i=i-2
  j=j-2
  k=k-2

  ih=ih-2
  jh=jh-2
  kh=kh-2
  !  Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do k1=1,3
   k2=k+k1
   do j1=1,3
    j2=j+j1
    dvol=ay1(j1)*az1(k1)
    do i1=1,3
     i2=i1+ih
     ap(1)=ap(1)+axh(i1)*dvol*ef(i2,j2,k2,1)
    end do
   end do
   do j1=1,3
    j2=jh+j1
    dvol=ayh(j1)*az1(k1)
    do i1=1,3
     i2=i+i1
     ap(2)=ap(2)+ax1(i1)*dvol*ef(i2,j2,k2,2)
    end do
    do i1=1,3
     i2=i1+ih
     ap(6)=ap(6)+axh(i1)*dvol*ef(i2,j2,k2,6)
    end do
   end do
  end do
  !==============
  ! Bx(i,j+1/2,k+1/2)
  !==============
  !==============
  ! By(i+1/2,j,k+1/2)
  !==============
  !==============
  ! Ez(i,j,k+1/2)
  !==============

  do k1=1,3
   k2=kh+k1
   do j1=1,3
    j2=jh+j1
    dvol=ayh(j1)*azh(k1)
    do i1=1,3
     i2=i1+i
     ap(4)=ap(4)+ax1(i1)*dvol*ef(i2,j2,k2,4)
    end do
   end do
   do j1=1,3
    j2=j+j1
    dvol=ay1(j1)*azh(k1)
    do i1=1,3
     i2=ih+i1
     ap(5)=ap(5)+axh(i1)*dvol*ef(i2,j2,k2,5)
    end do
    do i1=1,3
     i2=i1+i
     ap(3)=ap(3)+ax1(i1)*dvol*ef(i2,j2,k2,3)
    end do
   end do
  end do
  pt(n,1:6)=ap(1:6)
 end do
 !================================
 end subroutine set_part3d_acc
 !================================
 subroutine set_ion_Efield(ef,sp_loc,pt,np,s_ind,ndm,rionz,dt_loc,xmn,ymn,zmn)

 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,s_ind,ndm,rionz
 real(dp),intent(in) :: dt_loc,xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol, gam
 real(dp) :: axh(0:2),ayh(0:2),xp1(3),pp(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp;azh(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp
 !===== enter species positions at t^{n+1} level========
 ! fields are at t^n
 select case(ndm)
 case(2)
  k2=1
  if(rionz >1)then       !all species running
   do n=1,np
    pp(1:2)=sp_loc%part(n,3:4)
    gam=1.+pp(1)*pp(1)+pp(2)*pp(2)
    pp(1:2)=pp(1:2)/sqrt(gam)
    pt(n,1:2)=sp_loc%part(n,1:2)-dt_loc*pp(1:2) ! stores t^n part positions
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
   end do
  else
   do n=1,np
    pt(n,1:2)=sp_loc%part(n,1:2)
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
   end do
  endif
  if(s_ind==0)then
   do n=1,np
    pt(n,2)=dy_inv*(pt(n,2)-ymn)
   end do
  else
   call map2dy_part_sind(np,s_ind,2,ymn,pt)
  endif
  !==========================
  do n=1,np
   ap(1:2)=zero_dp
   xp1(1:2)=pt(n,1:2)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   !axh(1)=sx+0.5
   !axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)
   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   !ayh(1)=sx+0.5
   !ayh(0)=1.-ayh(1)

   i=i-1
   j=j-1

   ih=ih-1
   jh=jh-1
   ! Ex(i+1/2,j,k)
   !==============
   !==============
   ! Ey(i,j+1/2,k)
   !==============
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,2
     i2=i1+ih
     ap(1)=ap(1)+axh(i1)*dvol*ef(i2,j2,k2,1)*ef(i2,j2,k2,1)
    end do
   end do
   do j1=0,2
    j2=jh+j1
    dvol=ayh(j1)
    do i1=0,2
     i2=i+i1
     ap(1)=ap(1)+ax1(i1)*dvol*ef(i2,j2,k2,2)*ef(i2,j2,k2,2)
    end do
   end do
   !==============
   pt(n,5)=ap(1)               !Ex(p)^2 + Ey(p)^2
  end do

 case(3)
  if(rionz >1)then
   do n=1,np
    pp(1:3)=sp_loc%part(n,4:6)
    gam=1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)
    pt(n,1:3)=sp_loc%part(n,1:3)-dt_loc*pp(1:3) ! stores t^n part positions
    pp(1:3)=pp(1:3)/sqrt(gam)
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
   end do
  else
   do n=1,np
    pt(n,1)=dx_inv*(sp_loc%part(n,1)-xmn)
    pt(n,2:3)=sp_loc%part(n,2:3)
   end do
  endif
  if(s_ind==0)then
   do n=1,np
    xp1(2:3)=pt(n,2:3)
    pt(n,2)=dy_inv*(xp1(2)-ymn)
    pt(n,3)=dz_inv*(xp1(3)-zmn)
   end do
  else
   call map3d_part_sind(pt,np,s_ind,2,3,ymn,zmn)
  endif
  !==========================
  do n=1,np
   ap(1:3)=zero_dp
   xp1(1:3)=pt(n,1:3)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   axh(1)=sx+0.5
   axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   ayh(1)=sx+0.5
   ayh(0)=1.-ayh(1)

   !kh=int(xx)
   !sx=xx-0.5-real(kh,dp)
   !sx2=sx*sx
   !azh(1)=0.75-sx2
   !azh(2)=0.5*(0.25+sx2+sx)
   !azh(0)=1.-azh(1)-azh(2)
   !kh=kh-1
   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   azh(1)=sx+0.5
   azh(0)=1.-azh(1)

   i=i-1
   j=j-1
   k=k-1

   ih=i
   jh=j
   kh=k
   ! Ex(i+1/2,j,k)
   !==============
   !==============
   ! Ey(i,j+1/2,k)
   !==============
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,1
      i2=i1+ih
      ap(1)=ap(1)+axh(i1)*dvol*ef(i2,j2,k2,1)*ef(i2,j2,k2,1)
     end do
    end do
    do j1=0,1
     j2=jh+j1
     dvol=ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      ap(1)=ap(1)+ax1(i1)*dvol*ef(i2,j2,k2,2)*ef(i2,j2,k2,2)
     end do
    end do
   end do
   !==============
   ! Ez(i,j,k+1/2)
   !==============
   do k1=0,1
    k2=kh+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*azh(k1)
     do i1=0,2
      i2=i1+i
      ap(1)=ap(1)+ax1(i1)*dvol*ef(i2,j2,k2,3)*ef(i2,j2,k2,3)
     end do
    end do
   end do
   pt(n,7)=ap(1)
  end do
 end select
 !================================
 end subroutine set_ion_Efield
 !=======================================
 subroutine set_ion_env_field(ef,sp_loc,pt,np,ndm,s_ind,xmn,ymn,zmn,om0)

 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndm,s_ind
 real(dp),intent(in) :: xmn,ymn,zmn,om0

 real(dp) :: xx,sx,sx2,dvol, ddx,ddy
 real(dp) :: axh(0:1),ayh(0:1),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n
 !==============================
 ! Enter ef(1:2)<=  A=(A_R,A_I)
 ! Exit interpolated |E|^2= |E_y|^2 + |E_x|^2
 !===========================
 !  Up to O(epsilon)^2:
 ! |E_y|^2= k_0^2*|A|^2+2*k_0*[A_RDx(A_I)-A_IDx(A_R)] +(Dx[A_R])^2 +Dx[A_I}^2)
 ! |E_x|^2= (Dy[A_r])^2 +Dy[A_I}^2)
 !===============================================

 !===============================================
 ! Only linear shape at half-index and at integer index
 !====================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp;azh(0:1)=zero_dp
 axh(0:1)=zero_dp;ayh(0:1)=zero_dp
 ddx=dx_inv
 ddy=dy_inv
 !===== enter species positions at t^{n+1} level========
 ! fields are at t^n
 select case(ndm)
 case(2)
  k2=1
  do n=1,np
   pt(n,1:2)=sp_loc%part(n,1:2)
   pt(n,1)=dx_inv*(pt(n,1)-xmn)
  end do
  if(s_ind==0)then
   do n=1,np
   pt(n,2)=dy_inv*(pt(n,2)-ymn)
   end do
  else
   call map2dy_part_sind(np,s_ind,2,ymn,pt)
  endif
  !==========================
  do n=1,np
   ap(1:6)=zero_dp
   xp1(1:2)=pt(n,1:2)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   axh(1)=sx+0.5
   axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   ayh(1)=sx+0.5
   ayh(0)=1.-ayh(1)
   i=i-1
   j=j-1
   ih=i
   jh=j
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,2
     i2=i1+i
     ap(1)=ap(1)+ax1(i1)*dvol*ef(i2,j2,k2,1)        !A_R
     ap(2)=ap(2)+ax1(i1)*dvol*ef(i2,j2,k2,2)        !A_I
    end do
    do i1=0,1
     i2=i1+ih
     ap(3)=ap(3)+axh(i1)*dvol*(ef(i2+1,j2,k2,1)-ef(i2,j2,k2,1))        !DxA_R
     ap(4)=ap(4)+axh(i1)*dvol*(ef(i2+1,j2,k2,2)-ef(i2,j2,k2,2))        !DxA_I
    end do
   end do
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)
    do i1=0,2
     i2=i+i1
     ap(5)=ap(5)+ax1(i1)*dvol*(ef(i2,j2+1,k2,1)-ef(i2,j2,k2,1))    !DyA_R
     ap(6)=ap(6)+ax1(i1)*dvol*(ef(i2,j2+1,k2,2)-ef(i2,j2,k2,2))    !DyA_I
    end do
   end do
!==================
   pt(n,4)=sqrt(ap(1)*ap(1)+ap(2)*ap(2))   !The interpolated |A| potential
   ap(1)=om0*ap(1)
   ap(2)=om0*ap(2)
   ap(3)=ddx*ap(3)
   ap(4)=ddx*ap(4)
   ap(5)=ddy*ap(5)
   ap(6)=ddy*ap(6)
   pt(n,5)=ap(1)*ap(1)+ap(2)*ap(2)+ap(3)*ap(3)+ap(4)*ap(4)+ap(5)*ap(5)+ap(6)*ap(6)
   pt(n,5)=pt(n,5)+2.*(ap(1)*ap(4)-ap(2)*ap(3))
  end do
  !==========================
 case(3)

   do n=1,np
   pt(n,1:3)=sp_loc%part(n,1:3)
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
  end do
  if(s_ind==0)then
   do n=1,np
    xp1(2:3)=pt(n,2:3)
    pt(n,2)=dy_inv*(xp1(2)-ymn)
    pt(n,3)=dz_inv*(xp1(3)-zmn)
   end do
  else
   call map3d_part_sind(pt,np,s_ind,2,3,ymn,zmn)
  endif
  do n=1,np
   ap(1:6)=zero_dp
   xp1(1:3)=pt(n,1:3)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   axh(1)=sx+0.5
   axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   ayh(1)=sx+0.5
   ayh(0)=1.-ayh(1)

   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   azh(1)=sx+0.5
   azh(0)=1.-azh(1)

   i=i-1
   j=j-1
   k=k-1

   ih=i
   jh=j
   kh=k
!=============== Quadratic/linear assignements
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,2
      i2=i1+i
      ap(1)=ap(1)+ax1(i1)*dvol*ef(i2,j2,k2,1)        !A_R
      ap(2)=ap(2)+ax1(i1)*dvol*ef(i2,j2,k2,2)        !A_I
     end do
     do i1=0,1
      i2=i1+ih
      ap(3)=ap(3)+axh(i1)*dvol*(ef(i2+1,j2,k2,1)-ef(i2,j2,k2,1))        !DxA_R
      ap(4)=ap(4)+axh(i1)*dvol*(ef(i2+1,j2,k2,2)-ef(i2,j2,k2,2))        !DxA_I
     end do
    end do
    do j1=0,1
     j2=jh+j1
     dvol=ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      ap(5)=ap(5)+ax1(i1)*dvol*(ef(i2,j2+1,k2,1)-ef(i2,j2,k2,1))    !DyA_R
      ap(6)=ap(6)+ax1(i1)*dvol*(ef(i2,j2+1,k2,2)-ef(i2,j2,k2,2))    !DyA_I
     end do
    end do
   end do
   pt(n,6)=sqrt(ap(1)*ap(1)+ap(2)*ap(2))   !The interpolated |A| potential
   ap(1)=om0*ap(1)
   ap(2)=om0*ap(2)
   ap(3)=ddx*ap(3)
   ap(4)=ddx*ap(4)
   ap(5)=ddy*ap(5)
   ap(6)=ddy*ap(6)
   pt(n,7)=ap(1)*ap(1)+ap(2)*ap(2)+ap(3)*ap(3)+ap(4)*ap(4)+ap(5)*ap(5)+ap(6)*ap(6)
   pt(n,7)=pt(n,7)+2.*(ap(1)*ap(4)-ap(2)*ap(3))
  end do
 end select
 !================================
 end subroutine set_ion_env_field

 subroutine set_ion_Ebfield(ef,ef1,sp_loc,pt,np,s_ind,ndm,rionz,dt_loc,xmn,ymn,zmn)

 real(dp),intent(in) :: ef(:,:,:,:),ef1(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,s_ind,ndm,rionz
 real(dp),intent(in) :: dt_loc,xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol, gam,eftot
 real(dp) :: axh(0:2),ayh(0:2),xp1(3),pp(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp;azh(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp
 !===== enter species positions at t^{n+1} level========
 ! fields are at t^n
 select case(ndm)
 case(2)
  k2=1
  if(rionz >1)then
   do n=1,np
    pp(1:2)=sp_loc%part(n,3:4)
    gam=1.+pp(1)*pp(1)+pp(2)*pp(2)
    pp(1:2)=pp(1:2)/sqrt(gam)
    pt(n,1:2)=sp_loc%part(n,1:2)-dt_loc*pp(1:2) ! stores t^n part positions
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
   end do
  else
   do n=1,np
    pt(n,1)=dx_inv*(sp_loc%part(n,1)-xmn)
    pt(n,2)=sp_loc%part(n,2)
   end do
  endif
  if(s_ind==0)then
   do n=1,np
    pt(n,2)=dy_inv*(pt(n,2)-ymn)
   end do
  else
   call map2dy_part_sind(np,s_ind,2,ymn,pt)
  endif
  !==========================
  do n=1,np
   ap(1:2)=zero_dp
   xp1(1:2)=pt(n,1:2)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   !axh(1)=sx+0.5
   !axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)
   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   !ayh(1)=sx+0.5
   !ayh(0)=1.-ayh(1)

   i=i-1
   j=j-1

   ih=ih-1
   jh=jh-1
   ! Ex(i+1/2,j,k)
   !==============
   !==============
   ! Ey(i,j+1/2,k)
   !==============
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,2
     i2=i1+ih
     eftot=ef(i2,j2,k2,1)+ef1(i2,j2,k2,1)
     ap(1)=ap(1)+axh(i1)*dvol*eftot*eftot
    end do
   end do
   do j1=0,2
    j2=jh+j1
    dvol=ayh(j1)
    do i1=0,2
     i2=i+i1
     eftot=ef(i2,j2,k2,2)+ef1(i2,j2,k2,2)
     ap(2)=ap(2)+ax1(i1)*dvol*eftot*eftot
    end do
   end do
   !==============
   pt(n,5)=ap(1)+ap(2)
  end do

 case(3)
  if(rionz >1)then
   do n=1,np
    pp(1:3)=sp_loc%part(n,4:6)
    gam=1.+pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)
    pp(1:3)=pp(1:3)/sqrt(gam)
    pt(n,1:3)=sp_loc%part(n,1:3) -dt_loc*pp(1:3) ! the part positions
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
   end do
  else
   do n=1,np
    pt(n,1)=dx_inv*(sp_loc%part(n,1)-xmn)
    pt(n,2:3)=sp_loc%part(n,2:3)
   end do
  endif
  if(s_ind==0)then
   do n=1,np
    xp1(2:3)=pt(n,2:3)
    pt(n,2)=dy_inv*(xp1(2)-ymn)
    pt(n,3)=dz_inv*(xp1(3)-zmn)
   end do
  else
   call map3d_part_sind(pt,np,s_ind,2,3,ymn,zmn)
  endif
  !==========================
  do n=1,np
   ap(1:3)=zero_dp
   xp1(1:3)=pt(n,1:3)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   axh(1)=sx+0.5
   axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   ayh(1)=sx+0.5
   ayh(0)=1.-ayh(1)

   !kh=int(xx)
   !sx=xx-0.5-real(kh,dp)
   !sx2=sx*sx
   !azh(1)=0.75-sx2
   !azh(2)=0.5*(0.25+sx2+sx)
   !azh(0)=1.-azh(1)-azh(2)
   !kh=kh-1
   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   azh(1)=sx+0.5
   azh(0)=1.-azh(1)

   i=i-1
   j=j-1
   k=k-1

   ih=i
   jh=j
   kh=k
   ! Ex(i+1/2,j,k)
   !==============
   !==============
   ! Ey(i,j+1/2,k)
   !==============
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,1
      i2=i1+ih
      eftot=ef(i2,j2,k2,1)+ef1(i2,j2,k2,1)
      ap(1)=ap(1)+axh(i1)*dvol*eftot*eftot
     end do
    end do
    do j1=0,1
     j2=jh+j1
     dvol=ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      eftot=ef(i2,j2,k2,2)+ef1(i2,j2,k2,2)
      ap(2)=ap(2)+ax1(i1)*dvol*eftot*eftot
     end do
    end do
   end do
   !==============
   ! Ez(i,j,k+1/2)
   !==============
   do k1=0,1
    k2=kh+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*azh(k1)
     do i1=0,2
      i2=i1+i
      eftot=ef(i2,j2,k2,3)+ef1(i2,j2,k2,3)
      ap(3)=ap(3)+ax1(i1)*dvol*eftot*eftot
     end do
    end do
   end do
   pt(n,7)=ap(1)+ap(2)+ap(3)
  end do
 end select

 end subroutine set_ion_Ebfield
 !================================
 subroutine set_ion_two_Ebfield(&
              ef,ef1,ef2,sp_loc,pt,np,s_ind,ndm,xmn,ymn,zmn)

 real(dp),intent(in) :: ef(:,:,:,:),ef1(:,:,:,:),ef2(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,s_ind,ndm
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol,eftot
 real(dp) :: axh(0:2),ayh(0:2),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp;azh(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp
 !===== enter species positions at t^{n+1} level========
 ! fields are at t^n
 select case(ndm)
 case(2)
  k2=1
   do n=1,np
    pt(n,1)=dx_inv*(sp_loc%part(n,1)-xmn)
    pt(n,2)=sp_loc%part(n,2)
   end do
  if(s_ind==0)then
   do n=1,np
    pt(n,2)=dy_inv*(pt(n,2)-ymn)
   end do
  else
   call map2dy_part_sind(np,s_ind,2,ymn,pt)
  endif
  !==========================
  do n=1,np
   ap(1:2)=zero_dp
   xp1(1:2)=pt(n,1:2)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   !axh(1)=sx+0.5
   !axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)
   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   !ayh(1)=sx+0.5
   !ayh(0)=1.-ayh(1)

   i=i-1
   j=j-1

   ih=ih-1
   jh=jh-1
   ! Ex(i+1/2,j,k)
   !==============
   !==============
   ! Ey(i,j+1/2,k)
   !==============
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,2
     i2=i1+ih
     eftot=ef(i2,j2,k2,1)+ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1)
     ap(1)=ap(1)+axh(i1)*dvol*eftot*eftot
    end do
   end do
   do j1=0,2
    j2=jh+j1
    dvol=ayh(j1)
    do i1=0,2
     i2=i+i1
     eftot=ef(i2,j2,k2,2)+ef1(i2,j2,k2,2)+ef2(i2,j2,k2,2)
     ap(2)=ap(2)+ax1(i1)*dvol*eftot*eftot
    end do
   end do
   !==============
   pt(n,5)=ap(1)+ap(2)
  end do

 case(3)
   do n=1,np
    pt(n,1)=dx_inv*(sp_loc%part(n,1)-xmn)
    pt(n,2:3)=sp_loc%part(n,2:3)
   end do
  if(s_ind==0)then
   do n=1,np
    xp1(2:3)=pt(n,2:3)
    pt(n,2)=dy_inv*(xp1(2)-ymn)
    pt(n,3)=dz_inv*(xp1(3)-zmn)
   end do
  else
   call map3d_part_sind(pt,np,s_ind,2,3,ymn,zmn)
  endif
  !==========================
  do n=1,np
   ap(1:3)=zero_dp
   xp1(1:3)=pt(n,1:3)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   axh(1)=sx+0.5
   axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   ayh(1)=sx+0.5
   ayh(0)=1.-ayh(1)

   !kh=int(xx)
   !sx=xx-0.5-real(kh,dp)
   !sx2=sx*sx
   !azh(1)=0.75-sx2
   !azh(2)=0.5*(0.25+sx2+sx)
   !azh(0)=1.-azh(1)-azh(2)
   !kh=kh-1
   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   azh(1)=sx+0.5
   azh(0)=1.-azh(1)

   i=i-1
   j=j-1
   k=k-1

   ih=i
   jh=j
   kh=k
   ! Ex(i+1/2,j,k)
   !==============
   !==============
   ! Ey(i,j+1/2,k)
   !==============
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,1
      i2=i1+ih
      eftot=ef(i2,j2,k2,1)+ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1)
      ap(1)=ap(1)+axh(i1)*dvol*eftot*eftot
     end do
    end do
    do j1=0,1
     j2=jh+j1
     dvol=ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      eftot=ef(i2,j2,k2,2)+ef1(i2,j2,k2,2)+ef2(i2,j2,k2,2)
      ap(2)=ap(2)+ax1(i1)*dvol*eftot*eftot
     end do
    end do
   end do
   !==============
   ! Ez(i,j,k+1/2)
   !==============
   do k1=0,1
    k2=kh+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*azh(k1)
     do i1=0,2
      i2=i1+i
      eftot=ef(i2,j2,k2,3)+ef1(i2,j2,k2,3)+ef2(i2,j2,k2,3)
      ap(3)=ap(3)+ax1(i1)*dvol*eftot*eftot
     end do
    end do
   end do
   pt(n,7)=ap(1)+ap(2)+ap(3)
  end do
 end select
 !================================
 end subroutine set_ion_two_Ebfield
 !==================
 subroutine set_part3d_hcell_acc(ef,sp_loc,pt,np,s_ind,xmn,ymn,zmn)

 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,s_ind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(0:2),ayh(0:2),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 !Staggered shapes
 ! Linear shape at half-index
 ! Quadratic shape at integer index
 !====================================
 !=============================================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp;azh(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp

 do n=1,np
  pt(n,1:3)=sp_loc%part(n,1:3)
 end do
 call set_local_positions(pt,1,np,s_ind,3,xmn,ymn,zmn)
 !==========================
 do n=1,np
  ap(1:6)=zero_dp
  xp1(1:3)=pt(n,1:3)
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh(1)=sx+0.5
  axh(0)=1.-axh(1)

  xx=shy+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh(1)=sx+0.5
  ayh(0)=1.-ayh(1)

  xx=shz+xp1(3)
  k=int(xx+0.5)
  sx=xx-real(k,dp)
  sx2=sx*sx
  az1(1)=0.75-sx2
  az1(2)=0.5*(0.25+sx2+sx)
  az1(0)=1.-az1(1)-az1(2)

  azh(1)=sx+0.5
  azh(0)=1.-azh(1)

  i=i-1
  j=j-1
  k=k-1

  ih=i
  jh=j
  kh=k
  ! Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do k1=0,2
   k2=k+k1
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*az1(k1)
    do i1=0,1
     i2=i1+ih
     ap(1)=ap(1)+axh(i1)*dvol*ef(i2,j2,k2,1)
    end do
   end do
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*az1(k1)
    do i1=0,2
     i2=i+i1
     ap(2)=ap(2)+ax1(i1)*dvol*ef(i2,j2,k2,2)
    end do
    do i1=0,1
     i2=i1+ih
     ap(6)=ap(6)+axh(i1)*dvol*ef(i2,j2,k2,6)
    end do
   end do
  end do
  !==============
  ! Bx(i,j+1/2,k+1/2)
  !==============
  !==============
  ! By(i+1/2,j,k+1/2)
  !==============
  !==============
  ! Ez(i,j,k+1/2)
  !==============

  do k1=0,1
   k2=kh+k1
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*azh(k1)
    do i1=0,2
     i2=i1+i
     ap(4)=ap(4)+ax1(i1)*dvol*ef(i2,j2,k2,4)
    end do
   end do
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*azh(k1)
    do i1=0,1
     i2=ih+i1
     ap(5)=ap(5)+axh(i1)*dvol*ef(i2,j2,k2,5)
    end do
    do i1=0,2
     i2=i1+i
     ap(3)=ap(3)+ax1(i1)*dvol*ef(i2,j2,k2,3)
    end do
   end do
  end do
  pt(n,1:6)=ap(1:6)
 end do
 !================================
 end subroutine set_part3d_hcell_acc
 !=================================
 subroutine set_env_grad_interp(av,sp_loc,pt,np,ndm,nst_ind,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(in) :: av(:,:,:,:)
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndm,nst_ind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol,dvol1,dxe,dye,dze
 real(dp) :: axh(0:2),ayh(0:2),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(4)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! enters av(1)=|a|^2/2 envelope at integer grid nodes
 ! and av(2:4)=[Grad |a|2/2] at staggered grid points
 ! exit in pt(1:4) grad[|a|^2]/2 and |a|^2/2 at the particle positions
 ! On output => Reverse ordering of field variables is used
 !=========================
 ! Particle positions assigned using quadratic splines
 !  F=|a|^2/2
 !  ap(1)= [D_x(F)](i+1/2,j,k)
 !  ap(2)= [D_y(F)](i,j+1/2,k)
 !  ap(3)= [D_z(F)](i,j,k+1/2)
 !  ap(4)= [Phi](i,j,k)
 !===========================================
 ax1(0:2)=0.0;ay1(0:2)=0.0
 az1(0:2)=0.0
 axh(0:2)=0.0;ayh(0:2)=0.0
 azh(0:2)=0.0

 select case(ndm)
 case(2)
  dxe=dx_inv
  dye=dy_inv
  k2=1
  do n=1,np
   pt(n,1)=dxe*(sp_loc%part(n,1)-xmn) !loc x position
   pt(n,2)=sp_loc%part(n,2) !
  end do
  if(nst_ind==0)then
   do n=1,np
    pt(n,2)=dye*(pt(n,2)-ymn) !loc y position
   end do
  else
   call map2dy_part_sind(np,nst_ind,2,ymn,pt)
  endif
  do n=1,np
   ap=0.0
   xp1(1:2)=pt(n,1:2)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   i=i-1
   j=j-1

   ih=ih-1
   jh=jh-1
   !==========================
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,2
     i2=i1+ih
     dvol1=dvol*axh(i1)
     ap(1)=ap(1)+dvol1*av(i2,j2,k2,2)    !Dx[Phi]
     i2=i1+i
     ap(3)=ap(3)+ax1(i1)*dvol*av(i2,j2,k2,1) ![Phi]
    end do
   end do
   do j1=0,2
    j2=jh+j1
    dvol=ayh(j1)
    do i1=0,2
     i2=i+i1
     dvol1=dvol*ax1(i1)
     ap(2)=ap(2)+dvol1*av(i2,j2,k2,3)   !Dy[Phi]
    end do
   end do
   !pt(n,1)=dxe*ap(1)    !assigned grad[A^2/2]
   !pt(n,2)=dye*ap(2)
   pt(n,1:3)=ap(1:3)    !assigned grad[Phi] and Phi
  end do
!=================================
 case(3)
  dxe=dx_inv
  dye=dy_inv
  dze=dz_inv
  do n=1,np
   pt(n,1)=dxe*(sp_loc%part(n,1)-xmn) !loc x position
   pt(n,2:3)=sp_loc%part(n,2:3)
  end do
  if(nst_ind==00)then
   do n=1,np
    pt(n,2)=dye*(pt(n,2)-ymn) !loc y position
    pt(n,3)=dze*(pt(n,3)-zmn) !loc z position
   end do
  else
   call map3d_part_sind(pt,np,nst_ind,2,3,ymn,zmn)
  endif
  do n=1,np
   ap=0.0
   xp1(1:3)=pt(n,1:3)

   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   kh=int(xx)
   sx=xx-0.5-real(kh,dp)
   sx2=sx*sx
   azh(1)=0.75-sx2
   azh(2)=0.5*(0.25+sx2+sx)
   azh(0)=1.-azh(1)-azh(2)

   i=i-1
   j=j-1
   k=k-1

   ih=ih-1
   jh=jh-1
   kh=kh-1
   !==========================
   ap=0.0
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,2
      i2=i1+ih
      dvol1=dvol*axh(i1)
      ap(1)=ap(1)+dvol1*av(i2,j2,k2,2)   !Dx[F]
      i2=i1+i
      ap(4)=ap(4)+ax1(i1)*dvol*av(i2,j2,k2,1)  !Phi
     end do
    end do
    do j1=0,2
     j2=jh+j1
     dvol=ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      dvol1=dvol*ax1(i1)
      ap(2)=ap(2)+dvol1*av(i2,j2,k2,3) !Dy[F]
     end do
    end do
    k2=kh+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*azh(k1)
     do i1=0,2
      dvol1=dvol*ax1(i1)
      ap(3)=ap(3)+dvol1*av(i2,j2,k2,4)   !Dz[F]
     end do
    end do
   end do
   pt(n,1:4)=ap(1:4)    !Exit grad[Phi] and Phi
   !=================================
  end do
 end select
 end subroutine set_env_grad_interp

 subroutine set_env_interp(av,sp_loc,pt,np,ndm,nst_ind,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(in) :: av(:,:,:,:)
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndm,nst_ind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: xp1(3),dxe,dye,dze
 real(dp) :: ax1(0:2),ay1(0:2),az1(0:2),ap(1)
 integer :: i,j,i1,j1,i2,j2,k,k1,k2,n

 !===============================================
 ! enters av(1)=|a|^2 envelope at integer grid nodes
 ! exit |a|^2/2 at the particle positions
 !=========================
 ! Particle positions assigned using quadratic splines
 !  Phi=|a|^2/2
 !===========================================
 ax1(0:2)=0.0;ay1(0:2)=0.0
 az1(0:2)=0.0

 select case(ndm)
 case(2)
  k2=1
  dxe=dx_inv
  dye=dy_inv
  do n=1,np
   pt(n,2)=dxe*(sp_loc%part(n,1)-xmn) !loc x position
  end do
  if(nst_ind==00)then
   do n=1,np
    pt(n,3)=dye*(sp_loc%part(n,2)-ymn) !loc y position
   end do
  else
   call map2dy_part_sind(np,nst_ind,3,ymn,pt)
  endif
  do n=1,np
   ap(1)=0.0
   xp1(1:2)=pt(n,2:3)

   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   i=i-1
   j=j-1
   !==========================
   ap=zero_dp
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,2
     i2=i1+i
     ap(1)=ap(1)+ax1(i1)*dvol*av(i2,j2,k2,1)
    end do
   end do
   pt(n,1)=ap(1)    !assigned A^2/2
  end do
!=================================
 case(3)
  dxe=dx_inv
  dye=dy_inv
  dze=dz_inv
  do n=1,np
   pt(n,4)=dxe*(sp_loc%part(n,1)-xmn) !loc x position
  end do
  if(nst_ind==00)then
   do n=1,np
    pt(n,5)=dye*(sp_loc%part(n,2)-ymn) !loc y position
    pt(n,6)=dze*(sp_loc%part(n,3)-zmn) !loc z position
   end do
  else
   call map3d_part_sind(pt,np,nst_ind,5,6,ymn,zmn)
  endif
  do n=1,np
   ap(1)=0.0
   xp1(1:3)=pt(n,4:6)

   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)
   i=i-1
   j=j-1
   k=k-1
   !==========================
   ap=zero_dp
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,2
      i2=i1+i
      ap(1)=ap(1)+ax1(i1)*dvol*av(i2,j2,k2,1)  !F
     end do
    end do
   end do
   pt(n,1)=ap(1)
   !=================================
  end do
 end select
 end subroutine set_env_interp

 subroutine set_env_acc(ef,av,sp_loc,pt,np,ndm,str_ind,dt_loc,xmn,ymn,zmn)

 real(dp),intent(in) :: ef(:,:,:,:),av(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndm,str_ind
 real(dp),intent(in) :: dt_loc,xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol,dvol1
 real(dp) :: axh(0:2),ayh(0:2),up(3),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(12)
 real(dp) :: a1,b1,dgam,gam_inv,gam,gam2,dth
 integer :: i,ih,j,jh,i2,j2,k,kh,k2,n
 integer(kind=2) :: i1,j1,k1
 integer(kind=2),parameter :: stl=2
 !===============================================
 !===============================================
 ! Uses linear shapes for fields at half-index quadratic shape for fields
 !                         at integer index
 !====================================
 !===================================================
 ! enter ef(1:6) wake fields
 ! enters av(1)=F=|a|^2/2 envelope at integer grid nodes
 ! and av(2:4)=grad[F] at staggered points
 !  COMPUTES
 !(E,B), F, gradF  assignements to particle positions
 ! => ap(1:6)  in 2D
 ! => ap(1:10) in 3D
 ! approximated gamma function:
 ! gam_new= gam +0.25*charge*Dt(gam*E+0.5*grad[F]).p^{n-1/2}/gam^2
 ! EXIT
 ! (E+ 0.5grad[F]/gam_new) B/gam_new, F   and wgh/gam_new
 ! pt(1:5)  in 2D
 ! pt(1:7)  in 3D
 !========================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp
 azh(0:2)=zero_dp

 dth=0.5*dt_loc
 select case(ndm)
 !==========================
 case(2)
  k2=1
  do n=1,np   
   pt(n,1)=dx_inv*(sp_loc%part(n,1)-xmn)
   pt(n,2)=sp_loc%part(n,2)
  end do
  if(str_ind==0)then
   do n=1,np
    pt(n,2)=dy_inv*(pt(n,2)-ymn)
   end do
  else
   call map2dy_part_sind(np,str_ind,2,ymn,pt)
  endif
  do n=1,np
   ap(1:6)=0.0
   xp1(1:2)=pt(n,1:2)           !the current particle positions
   up(1:2)=sp_loc%part(n,3:4)    !the current particle  momenta
   wgh_cmp=sp_loc%part(n,5)          !the current particle (weight,charge)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   !axh(1)=sx+0.5
   !axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   !ayh(1)=sx+0.5
   !ayh(0)=1.-ayh(1)

   i=i-1
   j=j-1

   ih=ih-1
   jh=jh-1
   !==========================
   do j1=0,stl
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,stl
     i2=i+i1
     ap(6)=ap(6)+ax1(i1)*dvol*av(i2,j2,k2,1)!t^n p-assigned F=a^2/2 field
    end do
    do i1=0,stl
     i2=ih+i1
     dvol1=dvol*axh(i1)
     ap(1)=ap(1)+dvol1*ef(i2,j2,k2,1)    !Ex and Dx[F] (i+1/2,j,k))
     ap(4)=ap(4)+dvol1*av(i2,j2,k2,2)
                                         !ap(4)=ap(4)+dvol1*dx_inv*(av(i2+1,j2,k2,1)-av(i2,j2,k2,1))
    end do
   end do
   do j1=0,stl
    j2=jh+j1
    dvol=ayh(j1)
    do i1=0,stl
     i2=i+i1
     dvol1=dvol*ax1(i1)
     ap(2)=ap(2)+dvol1*ef(i2,j2,k2,2)  !Ey and Dy[F] (i,j+1/2,k)
     ap(5)=ap(5)+dvol1*av(i2,j2,k2,3)
                                       !ap(5)=ap(5)+dvol1*dy_inv*(av(i2,j2+1,k2,1)-av(i2,j2,k2,1))
    end do
    do i1=0,stl
     i2=ih+i1
     ap(3)=ap(3)+axh(i1)*dvol*ef(i2,j2,k2,3)   !Bz(i+1/2,j+1/2,k)
    end do
   end do
   !=========================
   gam2=1.+up(1)*up(1)+up(2)*up(2)+ap(6)   !gamma^{n-1/2}
   ap(1:3)=charge*ap(1:3)
   ap(4:5)=0.5*charge*charge*ap(4:5)
   !  ap(1:2)=q(Ex,Ey)   ap(3)=q*Bz,ap(4:5)=q*q*[Dx,Dy]F/2
   a1=dth*dot_product(ap(1:2),up(1:2))   !Dt*(qE_ip_i)/2 ==> a
   b1=dth*dot_product(ap(4:5),up(1:2))   !Dt*(qD_iFp_i)/4 ===> c
   gam=sqrt(gam2)
   dgam=(a1*gam-b1)/gam2
   gam_inv=(gam-dgam)/gam2
   ap(3:5)=ap(3:5)*gam_inv          !ap(3)=q*B/gamp, ap(4:5)= q*Grad[F]/2*gamp

   pt(n,1:2)=ap(1:2)-ap(4:5)   ! Lorentz force already multiplied by q
   pt(n,3)=ap(3)
   pt(n,5)=wgh*gam_inv     !weight/gamp
  end do
!=============================
 case(3)
  do n=1,np
   pt(n,1)=dx_inv*(sp_loc%part(n,1)-xmn)
   pt(n,2:3)=sp_loc%part(n,2:3)
  end do
  if(str_ind==0)then
   do n=1,np
    xp1(2:3)=pt(n,2:3)
    pt(n,2)=dy_inv*(xp1(2)-ymn)
    pt(n,3)=dz_inv*(xp1(3)-zmn)
   end do
  else
   call map3d_part_sind(pt,np,str_ind,2,3,ymn,zmn)
  endif
  do n=1,np
   ap=zero_dp
   xp1(1:3)=pt(n,1:3)
   up(1:3)=sp_loc%part(n,4:6)    !the current particle  momenta
   wgh_cmp=sp_loc%part(n,7)          !the current particle (weight,charge)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   !axh(1)=sx+0.5
   !axh(0)=1.-axh(1)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   !ayh(1)=sx+0.5
   !ayh(0)=1.-ayh(1)

   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   kh=int(xx)
   sx=xx-0.5-real(kh,dp)
   sx2=sx*sx
   azh(1)=0.75-sx2
   azh(2)=0.5*(0.25+sx2+sx)
   azh(0)=1.-azh(1)-azh(2)

   !azh(1)=sx+0.5
   !azh(0)=1.-azh(1)

   i=i-1
   j=j-1
   k=k-1

   ih=ih-1
   jh=jh-1
   kh=kh-1
   !==========================
   do k1=0,stl
    k2=k+k1
    do j1=0,stl
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,stl
      i2=i1+i
      ap(10)=ap(10)+ax1(i1)*dvol*av(i2,j2,k2,1)!t^n p-assigned Phi=a^2/2 field
     end do
     do i1=0,stl
      i2=i1+ih
      dvol1=dvol*axh(i1)
      ap(1)=ap(1)+dvol1*ef(i2,j2,k2,1)    !Ex and Dx[F] (i+1/2,j,k))
      ap(7)=ap(7)+dvol1*av(i2,j2,k2,2)
     end do
    end do
    do j1=0,stl
     j2=jh+j1
     dvol=ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      dvol1=dvol*ax1(i1)
      ap(2)=ap(2)+dvol1*ef(i2,j2,k2,2)  !Ey and Dy[F] (i,j+1/2,k)
      ap(8)=ap(8)+dvol1*av(i2,j2,k2,3)
     end do
     do i1=0,stl
      i2=i1+ih
      ap(6)=ap(6)+axh(i1)*dvol*ef(i2,j2,k2,6)   !Bz(i+1/2,j+1/2,k)
     end do
    end do
   end do
   !=========================
   do k1=0,stl
    k2=kh+k1
    do j1=0,stl
     j2=jh+j1
     dvol=ayh(j1)*azh(k1)
     do i1=0,stl
      i2=i1+i
      ap(4)=ap(4)+ax1(i1)*dvol*ef(i2,j2,k2,4) !Bx(i,j+1/2,k+1/2)
     end do
    end do
    do j1=0,stl
     j2=j+j1
     dvol=ay1(j1)*azh(k1)
     do i1=0,stl
      i2=ih+i1
      ap(5)=ap(5)+axh(i1)*dvol*ef(i2,j2,k2,5)  !By(i+1/2,j,k+1/2)
     end do
     do i1=0,stl
      i2=i1+i
      dvol1=dvol*ax1(i1)
      ap(3)=ap(3)+dvol1*ef(i2,j2,k2,3)      !Ez and Dz[F] (i,j,k=1/2)
      ap(9)=ap(9)+dvol1*av(i2,j2,k2,4)
     end do
    end do
   end do
   !=================================
   gam2=1.+up(1)*up(1)+up(2)*up(2)+up(3)*up(3)+ap(10)   !gamma^{n-1/2}
   ap(1:6)=charge*ap(1:6)
   ap(7:9)=0.5*charge*charge*ap(7:9)
   !  ap(1:3)=q(Ex,Ey,Ez)   ap(4:6)=q(Bx,By,Bz),ap(7:9)=q[Dx,Dy,Dz]F/2
   a1=dth*dot_product(ap(1:3),up(1:3))
   b1=dth*dot_product(ap(7:9),up(1:3))
   gam=sqrt(gam2)
   dgam=(a1*gam-b1)/gam2
   gam_inv=(gam-dgam)/gam2

   ap(4:9)=ap(4:9)*gam_inv          !ap(4:6)=B/gamp, ap(7:9)= Grad[F]/2*gamp

   pt(n,1:3)=ap(1:3)-ap(7:9)
   pt(n,4:6)=ap(4:6)
   pt(n,7)=wgh*gam_inv     !weight/gamp
  end do
 end select
 end subroutine set_env_acc
 !===========================
 subroutine set_env_rk_acc(ef,av,sp_loc,pt,np,ndm,xmn,ymn,zmn)

 real(dp),intent(in) :: ef(:,:,:,:)
 real(dp),intent(inout) :: av(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,ndm
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol,dvol1,gam2
 real(dp) :: axh(0:2),ayh(0:2),up(3),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),azh(0:2),az1(0:2),ap(12)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n
 !real(dp) :: gam
 real(dp) :: gam_inv
 !===============================================
 ! Linear shape for fields at half-index quadratic shape for fields
 !                         at integer index
 !====================================
 !=============================================================
 !   particle  indexing : i=1,....,nx => weights 1,.....nx+2
 !   Fields data[1:n1+2,1:n2+2,1:n3+2] ax(0)=> data(i), ax(1)=> data(i+1)
 !                                     ax(2)=>data(i+2)
 !===================================================
 ! enter ef(1:6) wake fields
 ! enters av(1)=F=|a|^2/2 envelope at integer grid nodes
 ! and av(2:4)=grad[F] at staggered points
 ! exit total Lorentz force and velocities at particle positions
 !========================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp
 azh(0:2)=zero_dp

 k2=1
 select case(ndm)
 case(2)
  !================== Works also for (z,r) Pz,Pr cylindrical coordinates

  do n=1,np
   ap=zero_dp
   xp1(1:2)=sp_loc%part(n,1:2)    !the t^{k-1} particle positions, momenta and weight
   up(1:2)=sp_loc%part(n,3:4)
   wgh_cmp=sp_loc%part(n,5)
   xx=shx+dx_inv*(xp1(1)-xmn)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)

   !axh(1)=sx+0.5
   !axh(0)=1.-axh(1)

   xx=shy+dy_inv*(xp1(2)-ymn)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   !ayh(1)=sx+0.5
   !ayh(0)=1.-ayh(1)

   i=i-1
   j=j-1

   ih=ih-1
   jh=jh-1
   !==========================
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)
    do i1=0,2
     i2=i1+ih
     dvol1=dvol*axh(i1)
     ap(1)=ap(1)+dvol1*ef(i2,j2,k2,1)
     ap(4)=ap(4)+dvol1*av(i2,j2,k2,2)   !D_xF
    end do
    do i1=0,2
     i2=i1+i
     ap(6)=ap(6)+ax1(i1)*dvol*av(i2,j2,k2,1)!the p-assigned |A|^2/2 env field
    end do
   end do
   do j1=0,2
    j2=jh+j1
    dvol=ayh(j1)
    do i1=0,2
     i2=i+i1
     dvol1=dvol*ax1(i1)
     ap(2)=ap(2)+dvol1*ef(i2,j2,k2,2)
     ap(5)=ap(5)+dvol1*av(i2,j2,k2,3)
    end do
    do i1=0,2
     i2=i1+ih
     ap(3)=ap(3)+axh(i1)*dvol*ef(i2,j2,k2,3)
    end do
   end do
   !=========================
   gam2=1.+up(1)*up(1)+up(2)*up(2)+ap(6)  !(gam_p)^{k-1}
   gam_inv=1./sqrt(gam2)

   ap(3:5)=ap(3:5)*gam_inv
   pt(n,1)=ap(1)+0.5*ap(4)
   pt(n,2)=ap(2)+0.5*ap(5)  !E+F_env
   pt(n,3)=ap(3)            !B_z/gam_p
   pt(n,5)=wgh*gam_inv  !wgh/gam_p
  end do
  !========================
 case(3)
  do n=1,np
   ap=zero_dp
   xp1(1:3)=sp_loc%part(n,1:3)   !the current t^{k-1} particle positions, momenta and weights
   up(1:3)=sp_loc%part(n,4:6)
   wgh_cmp=sp_loc%part(n,7)
   xx=shx+dx_inv*(xp1(1)-xmn)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   !ih=int(xx)
   !sx=xx-0.5-real(ih,dp)
   !sx2=sx*sx
   !axh(1)=0.75-sx2
   !axh(2)=0.5*(0.25+sx2+sx)
   !axh(0)=1.-axh(1)-axh(2)

   axh(1)=sx+0.5
   axh(0)=1.-axh(1)

   xx=shy+dy_inv*(xp1(2)-ymn)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)


   ayh(1)=sx+0.5
   ayh(0)=1.-ayh(1)

   xx=shz+dz_inv*(xp1(3)-zmn)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)


   azh(1)=sx+0.5
   azh(0)=1.-azh(1)

   i=i-1
   j=j-1
   k=k-1

   ih=i
   jh=j
   kh=k
   !==========================
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)
     do i1=0,2
      i2=i1+i
      ap(10)=ap(10)+ax1(i1)*dvol*av(i2,j2,k2,1)!t^n p-assigned F=a^2/2 field
     end do
     do i1=0,1
      i2=i1+ih
      dvol1=dvol*axh(i1)
      ap(1)=ap(1)+dvol1*ef(i2,j2,k2,1)    !Ex and Dx[F] (i+1/2,j,k))
      ap(7)=ap(7)+dvol1*av(i2,j2,k2,2)
     end do
    end do
    do j1=0,1
     j2=jh+j1
     dvol=ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      dvol1=dvol*ax1(i1)
      ap(2)=ap(2)+dvol1*ef(i2,j2,k2,2)  !Ey and Dy[F] (i,j+1/2,k)
      ap(8)=ap(8)+dvol1*av(i2,j2,k2,3)
     end do
     do i1=0,1
      i2=i1+ih
      ap(6)=ap(6)+axh(i1)*dvol*ef(i2,j2,k2,6)   !Bz(i+1/2,j+1/2,k)
     end do
    end do
   end do
   !=========================
   do k1=0,1
    k2=kh+k1
    do j1=0,1
     j2=jh+j1
     dvol=ayh(j1)*azh(k1)
     do i1=0,2
      i2=i1+i
      ap(4)=ap(4)+ax1(i1)*dvol*ef(i2,j2,k2,4) !Bx(i,j+1/2,k+1/2)
     end do
    end do
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*azh(k1)
     do i1=0,1
      i2=ih+i1
      ap(5)=ap(5)+axh(i1)*dvol*ef(i2,j2,k2,5)  !By(i+1/2,j,k+1/2)
     end do
     do i1=0,2
      i2=i1+i
      dvol1=dvol*ax1(i1)
      ap(3)=ap(3)+dvol1*ef(i2,j2,k2,3)      !Ez and Dz[F] (i,j,k=1/2)
      ap(9)=ap(9)+dvol1*av(i2,j2,k2,4)
     end do
    end do
   end do
   !=================================
   gam2=1.+up(1)*up(1)+up(2)*up(2)+up(3)*up(3)+ap(10)   !gamma^{k-1}
   !  ap(1:3)=(Ex,Ey,Ez)   ap(4:6)=(Bx,By,Bz),ap(7:9)=[Dx,Dy,Dz]F
   gam_inv=1./sqrt(gam2)
   ap(4:9)=ap(4:9)*gam_inv          !ap(4:6)=B/gamp, ap(7:9)= Grad[F]/gamp

   pt(n,1:3)=ap(1:3)+0.5*ap(7:9)
   pt(n,4:6)=ap(4:6)
   pt(n,7)=wgh*gam_inv  !wgh/gam_p
  end do
 end select
 end subroutine set_env_rk_acc
 !-------------------------
 !=============================
 subroutine set_env_density(efp,av,np,ndm,ic,nstr_ind,xmn,ymn,zmn)

 real(dp),intent(inout) :: efp(:,:)
 real(dp),intent(inout) :: av(:,:,:,:)
 integer,intent(in) :: np,ndm,ic,nstr_ind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol,dvol1,wghp
 real(dp) :: xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),az1(0:2)
 integer :: i,j,i1,j1,i2,j2,k,k1,k2,n
 !===============================================
 ! enter efp(1:4) positions and wgh/gamp at time level n
 ! exit av(:,:,:,ic) the den source in envelope equation :  <n*wgh/gamp> > 0
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp

 select case(ndm)
 case(2)
  k2=1
  if(nstr_ind==0)then
   do n=1,np
    efp(n,2)=dy_inv*(efp(n,2)-ymn)                ! local y
   end do
  else
   call map2dy_part_sind(np,nstr_ind,2,ymn,efp)
  endif 
  do n=1,np
   xp1(1)=dx_inv*(efp(n,1)-xmn)        ! local x
   xp1(2)=efp(n,2)                     ! local y
   wghp=efp(n,5)                       !the particle  q*wgh/gamp at current time
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   i=i-1
   j=j-1
   !==========================
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*wghp
    do i1=0,2
     i2=i1+i
     dvol1=dvol*ax1(i1)
     av(i2,j2,k2,ic)=av(i2,j2,k2,ic)+dvol1
    end do
   end do
  end do
  !========================
 case(3)
  if(nstr_ind==0)then
   do n=1,np
    efp(n,2)=dy_inv*(efp(n,2)-ymn)                ! local y
    efp(n,3)=dz_inv*(efp(n,3)-zmn)                ! local z
   end do
  else
   call map3d_part_sind(efp,np,nstr_ind,2,3,ymn,zmn)
  endif
  do n=1,np
   xp1(1)=dx_inv*(efp(n,1)-xmn)      ! local x
   xp1(2:3)=efp(n,2:3)               ! local y-z
   wghp=efp(n,7)          !the particle  wgh/gamp at current time

   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)

   xx=shy+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   i=i-1
   j=j-1
   k=k-1
   !==========================
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=ay1(j1)*az1(k1)*wghp
     do i1=0,2
      i2=i1+i
      dvol1=dvol*ax1(i1)
      av(i2,j2,k2,ic)=av(i2,j2,k2,ic)+dvol1
     end do
    end do
   end do
  end do
 end select
 end subroutine set_env_density
 !=============================
 subroutine set_part2d_twofield_acc(ef1,ef2,sp_loc,pt,np,sind,xmn,ymn)

 real(dp),intent(in) :: ef1(:,:,:,:),ef2(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,sind
 real(dp),intent(in) :: xmn,ymn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(0:2),ayh(0:2),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),ap(3),zmn
 integer :: i,ih,j,jh,i1,j1,i2,j2,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 !=============================================================
 ! ef1 plasma wakefield
 ! ef2 bunch fields
 !====================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp
 k2=1
 zmn=zero_dp
 pt(1:np,1:3)=sp_loc%part(1:np,1:3)
 call set_local_positions(pt,1,np,sind,2,xmn,ymn,zmn)
 do n=1,np
  ap(1:3)=zero_dp
  wgh_cmp=sp_loc%part(n,5) ! the particle charge
  xp1(1:2)=pt(n,1:2)    !the current particle positions
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh(1)=sx+0.5
  axh(0)=1.-axh(1)

  xx=shy+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh(1)=sx+0.5
  ayh(0)=1.-ayh(1)

  i=i-1
  j=j-1

  ih=i
  jh=j
  ! Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do j1=0,2
   j2=j+j1
   dvol=ay1(j1)
   do i1=0,1
    i2=i1+ih
    ap(1)=ap(1)+axh(i1)*dvol*(ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1))
   end do
  end do
  do j1=0,1
   j2=jh+j1
   dvol=ayh(j1)
   do i1=0,2
    i2=i+i1
    ap(2)=ap(2)+ax1(i1)*dvol*(ef1(i2,j2,k2,2) +ef2(i2,j2,k2,2))
   end do
   do i1=0,1
    i2=i1+ih
    ap(3)=ap(3)+axh(i1)*dvol*(ef1(i2,j2,k2,3)+ef2(i2,j2,k2,3))
   end do
  end do
  pt(n,1:3)=charge*ap(1:3)
 end do
 !================================
 end subroutine set_part2d_twofield_acc

 subroutine set_part2d_two_bfield_acc(ef1,ef2,ef3,sp_loc,pt,np,sind,xmn,ymn)

 real(dp),intent(in) :: ef1(:,:,:,:),ef2(:,:,:,:),ef3(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,sind
 real(dp),intent(in) :: xmn,ymn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(0:2),ayh(0:2),xp1(3)
 real(dp) :: ax1(0:2),ay1(0:2),ap(3),zmn
 integer :: i,ih,j,jh,i1,j1,i2,j2,k2,n
 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 !=============================================================
 ! ef1 plasma wakefield
 ! ef2 bunch fields
 !====================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 axh(0:2)=zero_dp;ayh(0:2)=zero_dp
 k2=1
 zmn=zero_dp
 pt(1:np,1:3)=sp_loc%part(1:np,1:3)
 call set_local_positions(pt,1,np,sind,2,xmn,ymn,zmn)
 do n=1,np
  ap(1:3)=zero_dp
  wgh_cmp=sp_loc%part(n,5) ! the particle charge
  xp1(1:2)=pt(n,1:2)    !the current particle positions
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh(1)=sx+0.5
  axh(0)=1.-axh(1)

  xx=shy+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh(1)=sx+0.5
  ayh(0)=1.-ayh(1)

  i=i-1
  j=j-1

  ih=i
  jh=j
  ! Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do j1=0,2
   j2=j+j1
   dvol=ay1(j1)
   do i1=0,1
    i2=i1+ih
    ap(1)=ap(1)+axh(i1)*dvol*(ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1)+ef3(i2,j2,k2,1))
   end do
  end do
  do j1=0,1
   j2=jh+j1
   dvol=ayh(j1)
   do i1=0,2
    i2=i+i1
    ap(2)=ap(2)+ax1(i1)*dvol*(ef1(i2,j2,k2,2) +ef2(i2,j2,k2,2)+ef3(i2,j2,k2,2))
   end do
   do i1=0,1
    i2=i1+ih
    ap(3)=ap(3)+axh(i1)*dvol*(ef1(i2,j2,k2,3)+ef2(i2,j2,k2,3)+ef3(i2,j2,k2,3))
   end do
  end do
  pt(n,1:3)=charge*ap(1:3)
 end do
 end subroutine set_part2d_two_bfield_acc
!=====================================================
 subroutine set_part3d_two_bfield_acc(ef1,ef2,ef3,&
  sp_loc,pt,n0,np,sind,xmn,ymn,zmn)

 real(dp),intent(in) :: ef1(:,:,:,:),ef2(:,:,:,:),ef3(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: n0,np,sind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(0:3),ayh(0:3),xp1(3)
 real(dp) :: ax1(0:3),ay1(0:3),azh(0:3),az1(0:3),ap(6),eftot
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 !=============================================================
 ! particle indexing : i=1,....,nx => weights 1,.....nx+2
 ! Fields data[1:n1+2,1:n2+2,1:n3+2] ax(0)=> data(i), ax(1)=> data(i+1) ax(3)=>data(i+2)
 !===================================================
 ! ef1 plasma wakefield
 ! ef2 , ef3 bunch self-fields  Bx=ef2(4)=0.0
 !====================
 pt(1:np,1:3)=sp_loc%part(1:np,1:3)
 call set_local_positions(pt,n0,np,sind,3,xmn,ymn,zmn)
 do n=n0,np
  ap(1:6)=zero_dp
  xp1(1:3)=pt(n,1:3)    !the local current particle positions
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh(1)=sx+0.5
  axh(0)=1.-axh(1)

  xx=shy+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh(1)=sx+0.5
  ayh(0)=1.-ayh(1)

  xx=shz+xp1(3)
  k=int(xx+0.5)
  sx=xx-real(k,dp)
  sx2=sx*sx
  az1(1)=0.75-sx2
  az1(2)=0.5*(0.25+sx2+sx)
  az1(0)=1.-az1(1)-az1(2)

  azh(1)=sx+0.5
  azh(0)=1.-azh(1)

  i=i-1
  j=j-1
  k=k-1

  ih=i
  jh=j
  kh=k
  ! Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do k1=0,2
   k2=k+k1
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*az1(k1)
    do i1=0,1
     i2=i1+ih
     eftot=ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1)+ef3(i2,j2,k2,1)
     ap(1)=ap(1)+axh(i1)*dvol*eftot
    end do
   end do
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*az1(k1)
    do i1=0,2
     i2=i+i1
     eftot=ef1(i2,j2,k2,2)+ef2(i2,j2,k2,2)+ef3(i2,j2,k2,2)
     ap(2)=ap(2)+ax1(i1)*dvol*eftot
    end do
    do i1=0,1
     i2=i1+ih
     eftot=ef1(i2,j2,k2,6)+ef2(i2,j2,k2,6)+ef3(i2,j2,k2,6)
     ap(6)=ap(6)+axh(i1)*dvol*eftot
    end do
   end do
  end do
  !==============
  ! Bx(i,j+1/2,k+1/2)
  !==============
  !==============
  ! By(i+1/2,j,k+1/2)
  !==============
  !==============
  ! Ez(i,j,k+1/2)
  !==============

  do k1=0,1
   k2=kh+k1
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*azh(k1)
    do i1=0,2
     eftot=ef1(i2,j2,k2,4)+ef3(i2,j2,k2,4)
     i2=i1+i
     ap(4)=ap(4)+ax1(i1)*dvol*eftot
    end do
   end do
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*azh(k1)
    do i1=0,1
     i2=ih+i1
     eftot=ef1(i2,j2,k2,5)+ef2(i2,j2,k2,5)+ef3(i2,j2,k2,5)
     ap(5)=ap(5)+axh(i1)*dvol*eftot
    end do
    do i1=0,2
     i2=i1+i
     eftot=ef1(i2,j2,k2,3)+ef2(i2,j2,k2,3)+ef3(i2,j2,k2,3)
     ap(3)=ap(3)+ax1(i1)*dvol*eftot
    end do
   end do
  end do
  pt(n,1:6)=ap(1:6)
 end do
 !================================
 end subroutine set_part3d_two_bfield_acc

 subroutine set_part3d_twofield_acc(ef1,ef2,sp_loc,pt,n0,np,sind,xmn,ymn,zmn)

 real(dp),intent(in) :: ef1(:,:,:,:),ef2(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: n0,np,sind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(0:3),ayh(0:3),xp1(3)
 real(dp) :: ax1(0:3),ay1(0:3),azh(0:3),az1(0:3),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 !=============================================================
 ! particle indexing : i=1,....,nx => weights 1,.....nx+2
 ! Fields data[1:n1+2,1:n2+2,1:n3+2] ax(0)=> data(i), ax(1)=> data(i+1) ax(3)=>data(i+2)
 !===================================================
 ! ef1 plasma wakefield
 ! ef2 bunch self-fields
 !====================

 pt(1:np,1:3)=sp_loc%part(1:np,1:3)
 call set_local_positions(pt,n0,np,sind,3,xmn,ymn,zmn)
 do n=n0,np
  ap(1:6)=zero_dp
  xp1(1:3)=pt(n,1:3)    !the current particle positions
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh(1)=sx+0.5
  axh(0)=1.-axh(1)

  xx=shy+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh(1)=sx+0.5
  ayh(0)=1.-ayh(1)

  xx=shz+xp1(3)
  k=int(xx+0.5)
  sx=xx-real(k,dp)
  sx2=sx*sx
  az1(1)=0.75-sx2
  az1(2)=0.5*(0.25+sx2+sx)
  az1(0)=1.-az1(1)-az1(2)

  azh(1)=sx+0.5
  azh(0)=1.-azh(1)

  i=i-1
  j=j-1
  k=k-1

  ih=i
  jh=j
  kh=k
  ! Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do k1=0,2
   k2=k+k1
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*az1(k1)
    do i1=0,1
     i2=i1+ih
     ap(1)=ap(1)+axh(i1)*dvol*(ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1))
    end do
   end do
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*az1(k1)
    do i1=0,2
     i2=i+i1
     ap(2)=ap(2)+ax1(i1)*dvol*(ef1(i2,j2,k2,2) +ef2(i2,j2,k2,2))
    end do
    do i1=0,1
     i2=i1+ih
     ap(6)=ap(6)+axh(i1)*dvol*(ef1(i2,j2,k2,6)+ef2(i2,j2,k2,6))
    end do
   end do
  end do
  !==============
  ! Bx(i,j+1/2,k+1/2)
  !==============
  !==============
  ! By(i+1/2,j,k+1/2)
  !==============
  !==============
  ! Ez(i,j,k+1/2)
  !==============

  do k1=0,1
   k2=kh+k1
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*azh(k1)
    do i1=0,2
     i2=i1+i
     ap(4)=ap(4)+ax1(i1)*dvol*ef1(i2,j2,k2,4)
    end do
   end do
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*azh(k1)
    do i1=0,1
     i2=ih+i1
     ap(5)=ap(5)+axh(i1)*dvol*(ef1(i2,j2,k2,5) +ef2(i2,j2,k2,5))
    end do
    do i1=0,2
     i2=i1+i
     ap(3)=ap(3)+ax1(i1)*dvol*(ef1(i2,j2,k2,3)+ef2(i2,j2,k2,3))
    end do
   end do
  end do
  pt(n,1:6)=ap(1:6)
 end do
 !================================
 end subroutine set_part3d_twofield_acc
 !==================================

 subroutine set_part3d_three_bfield_acc(ef1,ef2,ef3,ef4,sp_loc,pt,n0,np,sind,xmn,ymn,zmn)

 real(dp),intent(in) :: ef1(:,:,:,:),ef2(:,:,:,:),ef3(:,:,:,:),ef4(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: n0,np,sind
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(0:3),ayh(0:3),xp1(3)
 real(dp) :: ax1(0:3),ay1(0:3),azh(0:3),az1(0:3),ap(6),eftot
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 !=============================================================
 ! particle indexing : i=1,....,nx => weights 1,.....nx+2
 ! Fields data[1:n1+2,1:n2+2,1:n3+2] ax(0)=> data(i), ax(1)=> data(i+1) ax(3)=>data(i+2)
 !===================================================
 ! ef1 plasma wakefield
 ! ef2 bunch self-fields
 ! ef4 poloidal external field
 !====================
 do n=n0,np
  pt(1:np,1:3)=sp_loc%part(1:np,1:3)
  pt(1,n)=dx_inv*(pt(1,n)-xmn)
 end do
 if(sind==0)then
  do n=n0,np
   xp1(2:3)=pt(2:3,n)
   pt(2,n)=dy_inv*(xp1(2)-ymn)
   pt(3,n)=dz_inv*(xp1(3)-zmn)
  end do
 else
  call map3d_part_sind(pt,np,sind,2,3,ymn,zmn)
 endif
 do n=n0,np
  ap(1:6)=zero_dp
  xp1(1:3)=pt(1:3,n)    !the current particle positions
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh(1)=sx+0.5
  axh(0)=1.-axh(1)

  xx=shy+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh(1)=sx+0.5
  ayh(0)=1.-ayh(1)

  xx=shz+xp1(3)
  k=int(xx+0.5)
  sx=xx-real(k,dp)
  sx2=sx*sx
  az1(1)=0.75-sx2
  az1(2)=0.5*(0.25+sx2+sx)
  az1(0)=1.-az1(1)-az1(2)

  azh(1)=sx+0.5
  azh(0)=1.-azh(1)

  i=i-1
  j=j-1
  k=k-1

  ih=i
  jh=j
  kh=k
  ! Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do k1=0,2
   k2=k+k1
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*az1(k1)
    do i1=0,1
     i2=i1+ih
     eftot=ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1)+ef3(i2,j2,k2,1)!+ef4(i2,j2,k2,---)
     ap(1)=ap(1)+axh(i1)*dvol*eftot
    end do
   end do
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*az1(k1)
    do i1=0,2
     i2=i+i1
     eftot=ef1(i2,j2,k2,2)+ef2(i2,j2,k2,2)+ef3(i2,j2,k2,2)!+ef4(i2,j2,k2,---)
     ap(2)=ap(2)+ax1(i1)*dvol*eftot
    end do
    do i1=0,1
     i2=i1+ih
     eftot=ef1(i2,j2,k2,6)+ef2(i2,j2,k2,6)+ef3(i2,j2,k2,6)+ef4(i2,j2,k2,3)
     ap(6)=ap(6)+axh(i1)*dvol*eftot
    end do
   end do
  end do
  !==============
  ! Bx(i,j+1/2,k+1/2)
  !==============
  !==============
  ! By(i+1/2,j,k+1/2)
  !==============
  !==============
  ! Ez(i,j,k+1/2)
  !==============

  do k1=0,1
   k2=kh+k1
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*azh(k1)
    do i1=0,2
     eftot=ef1(i2,j2,k2,4)+ef3(i2,j2,k2,4)+ef4(i2,j2,k2,1)
     i2=i1+i
     ap(4)=ap(4)+ax1(i1)*dvol*eftot
    end do
   end do
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*azh(k1)
    do i1=0,1
     i2=ih+i1
     eftot=ef1(i2,j2,k2,5)+ef2(i2,j2,k2,5)+ef3(i2,j2,k2,5)+ef4(i2,j2,k2,2)
     ap(5)=ap(5)+axh(i1)*dvol*eftot
    end do
    do i1=0,2
     i2=i1+i
     eftot=ef1(i2,j2,k2,3)+ef2(i2,j2,k2,3)+ef3(i2,j2,k2,3)!+ef4(i2,j2,k2,---)
     ap(3)=ap(3)+ax1(i1)*dvol*eftot
    end do
   end do
  end do
  pt(1:6,n)=ap(1:6)
 end do
 !================================
 end subroutine set_part3d_three_bfield_acc


 subroutine set_part3d_threefield_acc(&
  ef1,ef2,ef3,sp_loc,pt,np1,np,xmn,ymn,zmn)

 real(dp),intent(in) :: ef1(:,:,:,:),ef2(:,:,:,:), ef3(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np1,np
 real(dp),intent(in) :: xmn,ymn,zmn

 real(dp) :: xx,sx,sx2,dvol
 real(dp) :: axh(0:3),ayh(0:3),xp1(3)
 real(dp) :: ax1(0:3),ay1(0:3),azh(0:3),az1(0:3),ap(6)
 integer :: i,ih,j,jh,i1,j1,i2,j2,k,kh,k1,k2,n

 !===============================================
 ! Linear shape at half-index quadratic shape at integer index
 !====================================
 !=============================================================
 ! particle indexing : i=1,....,nx => weights 1,.....nx+2
 ! Fields data[1:n1+2,1:n2+2,1:n3+2] ax(0)=> data(i), ax(1)=> data(i+1) ax(3)=>data(i+2)
 !===================================================
 ! ef1 plasma wakefield
 ! ef2 bunch self-fields
 !====================
 do n=np1,np
  pt(n,1:3)=sp_loc%part(n,1:3)
 end do
 !=================
 call set_local_positions(pt,np1,np,0,3,xmn,ymn,zmn)
 do n=np1,np
  ap(1:6)=zero_dp
  xp1(1:3)=pt(n,1:3)    !the current particle positions
  xx=shx+xp1(1)
  i=int(xx+0.5)
  sx=xx-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh(1)=sx+0.5
  axh(0)=1.-axh(1)

  xx=shy+xp1(2)
  j=int(xx+0.5)
  sx=xx-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh(1)=sx+0.5
  ayh(0)=1.-ayh(1)

  xx=shz+xp1(3)
  k=int(xx+0.5)
  sx=xx-real(k,dp)
  sx2=sx*sx
  az1(1)=0.75-sx2
  az1(2)=0.5*(0.25+sx2+sx)
  az1(0)=1.-az1(1)-az1(2)

  azh(1)=sx+0.5
  azh(0)=1.-azh(1)

  i=i-1
  j=j-1
  k=k-1

  ih=i
  jh=j
  kh=k
  ! Ex(i+1/2,j,k)
  !==============
  !==============
  ! Ey(i,j+1/2,k)
  !==============
  !==============
  ! Bz(i+1/2,j+1/2,k)
  !==============
  do k1=0,2
   k2=k+k1
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*az1(k1)
    do i1=0,1
     i2=i1+ih
     ap(1)=ap(1)+axh(i1)*dvol*(ef1(i2,j2,k2,1)+ef2(i2,j2,k2,1))
    end do
   end do
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*az1(k1)
    do i1=0,2
     i2=i+i1
     ap(2)=ap(2)+ax1(i1)*dvol*(ef1(i2,j2,k2,2) +ef2(i2,j2,k2,2))
    end do
    do i1=0,1
     i2=i1+ih
     ap(6)=ap(6)+axh(i1)*dvol*(&
      ef1(i2,j2,k2,6)+ef2(i2,j2,k2,6)+ef3(i2,j2,k2,3))
    end do
   end do
  end do
  !==============
  ! Bx(i,j+1/2,k+1/2)
  !==============
  !==============
  ! By(i+1/2,j,k+1/2)
  !==============
  !==============
  ! Ez(i,j,k+1/2)
  !==============

  do k1=0,1
   k2=kh+k1
   do j1=0,1
    j2=jh+j1
    dvol=ayh(j1)*azh(k1)
    do i1=0,2
     i2=i1+i
     ap(4)=ap(4)+ax1(i1)*dvol*(ef1(i2,j2,k2,4)+ef3(i2,j2,k2,1))
    end do
   end do
   do j1=0,2
    j2=j+j1
    dvol=ay1(j1)*azh(k1)
    do i1=0,1
     i2=ih+i1
     ap(5)=ap(5)+axh(i1)*dvol*(&
      ef1(i2,j2,k2,5) +ef2(i2,j2,k2,5)+ef3(i2,j2,k2,2))
    end do
    do i1=0,2
     i2=i1+i
     ap(3)=ap(3)+ax1(i1)*dvol*(ef1(i2,j2,k2,3)+ef2(i2,j2,k2,3))
    end do
   end do
  end do
  pt(n,1:6)=ap(1:6)
 end do
 !================================
 end subroutine set_part3d_threefield_acc
 !==================================
 !========= END OF SECTION for ACCELERATION FIELDS ASSIGNEMENT
 !==========================================
 !====================================================
 !========= PARTICLE ASSIGNEMENT TO GRID FOR CURRENT DENSITY
 !=============================
 subroutine esirkepov_2d_curr(sp_loc,pt,jcurr,n0,np,n_st,njc,ndm,xmn,ymn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:),jcurr(:,:,:,:)
 integer,intent(in) :: n0,np,n_st,njc,ndm
 real(dp),intent(in) :: xmn,ymn
 real(dp) :: ax,sx,sx2,dvol
 real(dp) :: ax0(0:3),ay0(0:3),xp1(3),xp0(3)
 real(dp) :: ax1(0:3),ay1(0:3),vp(3),vyp
 real(dp) :: axh(0:4),axh0(0:4),axh1(0:4),ayh(0:4)
 real(dp) :: currx(0:4),curry(0:4)
 real(sp) :: wght
 integer :: i,j,i0,j0,i1,j1,i2,j2,n
 integer :: ih,jh,ix0,ix1,iy0,iy1
 !==========================
 !Iform=0 or 1 IMPLEMENTS the ESIRKEPOV SCHEME for LINEAR-QUADRATIC SHAPE
 ! ==============================Only new and old positions needed
 ax1=zero_dp
 ay1=zero_dp
 ax0=zero_dp
 ay0=zero_dp
 select case(ndm)
 case(1)
  do n=n0,np
   xp1(1:2)=sp_loc%part(n,1:2) !x-new  t^(n+1)
   xp0(1:2)=pt(n,3:4)             !x-old  t^n
   vp(1:2)=xp1(1:2)-xp0(1:2)
   wgh_cmp=sp_loc%part(n,5)
   wght=charge*wgh
   vyp=0.5*wght*vp(2)                !q*Dt*Vy time level t^(n+1/2)

   ax=shx+dx_inv*(xp0(1)-xmn)
   i0=int(ax+0.5)
   sx=ax-real(i0,dp)
   sx2=sx*sx
   ax0(1)=0.75-sx2
   ax0(2)=0.5*(0.25+sx2+sx)
   ax0(0)=1.-ax0(1)-ax0(2)

   ax=shx+dx_inv*(xp1(1)-xmn)
   i=int(ax+0.5)
   sx=ax-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   axh(0:4)=zero_dp
   !-----------------------
   ih=i-i0+1
   do i1=0,2
    axh(ih+i1)=ax1(i1)
   end do
   i=i-1
   i0=i0-1

   currx(0)=-axh(0)
   do i1=1,3
    currx(i1)=currx(i1-1)+ax0(i1-1)-axh(i1)
   end do
   currx(4)=currx(3)-axh(4)
   currx(0:4)=wgh*currx(0:4)
   ih=i0-1
   do i1=0,4
    i2=ih+i1
    jcurr(i2,1,1,1)=jcurr(i2,1,1,1)+currx(i1)
   end do
   !++++++++++++++
   ! jc(1)=[rho_old-rho_new]=dt*Der_xJ_x
   ! Jx has to be computed by inversion on the x-grid
   !=============================
   do i1=0,2
    i2=i0+i1-1
    jcurr(i2,1,1,2)=jcurr(i2,1,1,2)+vyp*ax0(i1)
    i2=i+i1-1
    jcurr(i2,1,1,2)=jcurr(i2,1,1,2)+vyp*ax1(i1)
   end do
  end do
  !======================
 case(2)
  if(njc==2)then            !Two current components
   do n=1,np
    pt(n,1:2)=sp_loc%part(n,1:2) !x-y-new  t^(n+1)
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
    pt(n,3)=dx_inv*(pt(n,3)-xmn)
    wgh_cmp=sp_loc%part(n,5)
    wght=charge*wgh
    pt(n,5)=wght
   end do
   if(n_st==0)then
    do n=n0,np
     pt(n,2)=dy_inv*(pt(n,2)-ymn)
     pt(n,4)=dy_inv*(pt(n,4)-ymn)
    end do
   else
    call map2dy_part_sind(np,n_st,2,ymn,pt)
    call map2dy_part_sind(np,n_st,4,ymn,pt)
   endif
!========================
   do n=n0,np
    xp1(1:2)=pt(n,1:2)        !x-y  -new
    xp0(1:2)=pt(n,3:4)        !x-y  -old
    wght=real(pt(n,5),sp)              !w*q
    !=====================
    ax=shx+xp0(1)
    i0=int(ax+0.5)
    sx=ax-real(i0,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)

    ax=shx+xp1(1)
    i=int(ax+0.5)
    sx=ax-real(i,dp)
    sx2=sx*sx
    ax1(1)=0.75-sx2
    ax1(2)=0.5*(0.25+sx2+sx)
    ax1(0)=1.-ax1(1)-ax1(2)
    axh(0:4)=zero_dp
    ih=i-i0+1
    do i1=0,2
     axh(ih+i1)=ax1(i1)
    end do
    currx(0)=-axh(0)
    do i1=1,3
     currx(i1)=currx(i1-1)+ax0(i1-1)-axh(i1)
    end do
    currx(4)=currx(3)-axh(4)
    do i1=1,3
     axh(i1)=axh(i1)+ax0(i1-1)
    end do
    currx(0:4)=wght*currx(0:4)
    ix0=min(ih,1)
    ix1=max(ih+2,3)
    !-------
    ax=shy+xp0(2)
    j0=int(ax+0.5)
    sx=ax-real(j0,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)

    ax=shy+xp1(2)
    j=int(ax+0.5)
    sx=ax-real(j,dp)
    sx2=sx*sx
    ay1(1)=0.75-sx2
    ay1(2)=0.5*(0.25+sx2+sx)
    ay1(0)=1.-ay1(1)-ay1(2)

    jh=j-j0+1
    ayh(0:4)=zero_dp
    do i1=0,2
     ayh(jh+i1)=ay1(i1)
    end do
    curry(0)=-ayh(0)
    do i1=1,3
     curry(i1)=curry(i1-1)+ay0(i1-1)-ayh(i1)
    end do
    curry(4)=curry(3)-ayh(4)
    curry(0:4)=wght*curry(0:4)
    !========================================
    do i1=1,3
     ayh(i1)=ayh(i1)+ay0(i1-1)
    end do
    iy0=min(jh,1)
    iy1=max(jh+2,3)
    !================dt*J_x
    j0=j0-1
    j=j-1
    jh=j0-1

    i=i-1
    i0=i0-1
    ih=i0-1

    do j1=iy0,iy1
     j2=jh+j1
     do i1=ix0,ix1
      i2=ih+i1
      jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)+ayh(j1)*currx(i1)
     end do
    end do
    !================dt*J_y
    do j1=iy0,iy1
     j2=jh+j1
     do i1=ix0,ix1
      i2=ih+i1
      jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+axh(i1)*curry(j1)
     end do
    end do
   end do
  endif
  if(njc==3)then  !Three currents conditions in 2D grid
   do n=n0,np
    pt(n,1:3)=sp_loc%part(n,1:3) !x-y-z -new  t^(n+1)
    pt(n,1)=dx_inv*(pt(n,1)-xmn)
    pt(n,4)=dx_inv*(pt(n,4)-xmn)
    wgh_cmp=sp_loc%part(n,7)
    wght=charge*wgh
    pt(n,7)=wght
   end do
   if(n_st==0)then
    do n=n0,np
     pt(n,2)=dy_inv*(pt(n,2)-ymn)  !loc y new
     pt(n,5)=dy_inv*(pt(n,5)-ymn)  !loc y-old
    end do
   else
    call map2dy_part_sind(np,n_st,2,ymn,pt)
    call map2dy_part_sind(np,n_st,5,ymn,pt)
   endif
!==============================
   do n=n0,np
    xp1(1:3)=pt(n,1:3)                !increments xyz-new
    xp0(1:3)=pt(n,4:6)              !increments xyz z-old
    wght=real(pt(n,7),sp)
    vp(3)=xp1(3)-xp0(3)                    !dt*v_z(n+1/2)
    vp(3)=wght*vp(3)/3.                     !dt*q*w*vz/3
    !=====================
    ax=shx+xp0(1)
    i0=int(ax+0.5)
    sx=ax-real(i0,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)

    ax=shx+xp1(1)
    i=int(ax+0.5)
    sx=ax-real(i,dp)
    sx2=sx*sx
    ax1(1)=0.75-sx2
    ax1(2)=0.5*(0.25+sx2+sx)
    ax1(0)=1.-ax1(1)-ax1(2)
    axh(0:4)=zero_dp
    ih=i-i0+1
    ix0=min(ih,1)
    ix1=max(ih+2,3)
    do i1=0,2
     axh(ih+i1)=ax1(i1)
    end do
    currx(0)=-axh(0)
    do i1=1,3
     currx(i1)=currx(i1-1)+ax0(i1-1)-axh(i1)
    end do
    currx(4)=currx(3)-axh(4)
    do i1=1,3
     axh(i1)=axh(i1)+ax0(i1-1)
    end do
    currx(0:4)=wght*currx(0:4)

    axh0(0:4)=0.5*axh(0:4)
    axh1(0:4)=axh(0:4)
    do i1=1,3
     axh0(i1)=axh0(i1)+ax0(i1-1)
     axh1(i1)=axh1(i1)+0.5*ax0(i1-1)
     axh(i1)=axh(i1)+ax0(i1-1)         !Wx^0+Wx^1)
    end do
    !-------
    i=i-1
    i0=i0-1

    ax=shy+xp0(2)
    j0=int(ax+0.5)
    sx=ax-real(j0,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)

    ax=shy+xp1(2)
    j=int(ax+0.5)
    sx=ax-real(j,dp)
    sx2=sx*sx
    ay1(1)=0.75-sx2
    ay1(2)=0.5*(0.25+sx2+sx)
    ay1(0)=1.-ay1(1)-ay1(2)

    jh=j-j0+1
    iy0=min(jh,1)
    iy1=max(jh+2,3)

    ayh(0:4)=zero_dp
    do i1=0,2
     ayh(jh+i1)=ay1(i1)
    end do
    curry(0)=-ayh(0)
    do i1=1,3
     curry(i1)=curry(i1-1)+ay0(i1-1)-ayh(i1)
    end do
    curry(4)=curry(3)-ayh(4)
    curry(0:4)=wght*curry(0:4)
    do i1=1,3
     ayh(i1)=ayh(i1)+ay0(i1-1)
    end do
    !-----------
    j0=j0-1
    j=j-1
    !================dt*J_x= currx*(Wy^0+Wy^1) to be multiplied by dx/2
    ih=i0-1
    jh=j0-1
    do j1=iy0,iy1
     j2=jh+j1
     do i1=ix0,ix1
      i2=ih+i1
      jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)+ayh(j1)*currx(i1)
     end do
    end do
    !================dt*J_y= curry*(Wx^0+Wx^1)
    do j1=iy0,iy1
     j2=jh+j1
     do i1=ix0,ix1
      i2=ih+i1
      jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+axh(i1)*curry(j1)
     end do
    end do
    !========== dt*J_z Vz*[Wy^0(Wx^0+0.5*Wx^1)+Wy^1*(Wx^1+0.5*Wx^0)]
    do j1=0,2
     j2=j0+j1
     dvol=ay0(j1)*vp(3)
     do i1=ix0,ix1
      i2=i1+ih
      jcurr(i2,j2,1,3)=jcurr(i2,j2,1,3)+axh0(i1)*dvol
     end do
     j2=j+j1
     dvol=ay1(j1)*vp(3)
     do i1=ix0,ix1
      i2=i1+ih
      jcurr(i2,j2,1,3)=jcurr(i2,j2,1,3)+axh1(i1)*dvol
     end do
    end do
   end do
  endif
 end select
 !-----------------------
 end subroutine esirkepov_2d_curr
 !==========================================
 !=============3D=================
 subroutine esirkepov_3d_curr(sp_loc,pt,jcurr,n0,np,s_ind,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:),jcurr(:,:,:,:)
 integer,intent(in) :: n0,np,s_ind
 real(dp),intent(in) :: xmn,ymn,zmn
 real(dp) :: ax,sx,sx2,dvol,dvolh
 real(dp) :: ax0(0:2),ay0(0:2),az0(0:2),xp1(3),xp0(3)
 real(dp) :: ax1(0:2),ay1(0:2),az1(0:2)
 real(dp) :: axh(0:4),ayh(0:4),azh(0:4)
 real(dp) :: axh0(0:4),axh1(0:4),ayh0(0:4),ayh1(0:4)
 real(dp) :: currx(0:4),curry(0:4),currz(0:4)
 real(sp) :: wght
 integer :: i,j,k,i0,j0,k0,i1,j1,k1,i2,j2,k2,n
 integer :: ih,jh,kh
 integer :: ix0,ix1,iy0,iy1,iz0,iz1
 !=======================
 !Enter pt(4:6) old positions sp_loc(1:3) new positions

 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp;az0(0:2)=zero_dp
 ax0(0:2)=zero_dp;ay0(0:2)=zero_dp
 axh(0:4)=zero_dp;ayh(0:4)=zero_dp
 azh(0:4)=zero_dp
 currx(0:4)=zero_dp;curry(0:4)=zero_dp
 currz(0:4)=zero_dp
 axh0(0:4)=zero_dp;ayh0(0:4)=zero_dp
 axh1(0:4)=zero_dp;ayh1(0:4)=zero_dp
! ==============================Only new and old positions needed
 do n=n0,np
  pt(n,1:3)=sp_loc%part(n,1:3) !x-y-z -new  t^(n+1)
  pt(n,1)=dx_inv*(pt(n,1)-xmn)
  pt(n,4)=dx_inv*(pt(n,4)-xmn)
  wgh_cmp=sp_loc%part(n,7)
  wght=charge*wgh
  pt(n,7)=wght
 end do
 if(s_ind==0)then
  do n=n0,np
   pt(n,2)=dy_inv*(pt(n,2)-ymn)  !loc y new
   pt(n,3)=dz_inv*(pt(n,3)-zmn)  !loc z new
   pt(n,5)=dy_inv*(pt(n,5)-ymn)  !loc y-old
   pt(n,6)=dz_inv*(pt(n,6)-zmn)  !loc z-old
  end do
 else
  call map3d_part_sind(pt,np,s_ind,2,3,ymn,zmn)
  call map3d_part_sind(pt,np,s_ind,5,6,ymn,zmn)
 endif
 do n=n0,np
  xp1(1:3)=pt(n,1:3)        !increments of the new positions
  xp0(1:3)=pt(n,4:6)        !increments of old positions
  wght=real(pt(n,7),sp)
  ax=shx+xp0(1)
  i0=int(ax+0.5)
  sx=ax-real(i0,dp)
  sx2=sx*sx
  ax0(1)=0.75-sx2
  ax0(2)=0.5*(0.25+sx2+sx)
  ax0(0)=1.-ax0(1)-ax0(2)

  ax=shx+xp1(1)
  i=int(ax+0.5)
  sx=ax-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)
  axh(0:4)=zero_dp
  ih=i-i0+1
  !========== direct Jx-inversion
  do i1=0,2
   axh(ih+i1)=ax1(i1)
  end do
  currx(0)=-axh(0)
  do i1=1,3
   currx(i1)=currx(i1-1)+ax0(i1-1)-axh(i1)
  end do
  currx(4)=currx(3)-axh(4)
  currx(0:4)=wght*currx(0:4)
  !=======================
  axh0(0:4)=0.5*axh(0:4)
  axh1(0:4)=axh(0:4)
  do i1=1,3
   axh0(i1)=axh0(i1)+ax0(i1-1)
   axh1(i1)=axh1(i1)+0.5*ax0(i1-1)
  end do

  ix0=min(ih,1)
  ix1=max(ih+2,3)
  ! 3 data ih=1 [i0-1:i0+1] (no cell cross)
  ! 4 data ih=0 [i0-2:i0+1] ih=2 [i0-1:i0+2](cell cross)
  !-------
  i=i-1
  i0=i0-1

  ax=shy+xp0(2)
  j0=int(ax+0.5)
  sx=ax-real(j0,dp)
  sx2=sx*sx
  ay0(1)=0.75-sx2
  ay0(2)=0.5*(0.25+sx2+sx)
  ay0(0)=1.-ay0(1)-ay0(2)

  ax=shy+xp1(2)
  j=int(ax+0.5)
  sx=ax-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  !========== direct Jy-inversion
  jh=j-j0+1 !=[0,1,2]
  ayh(0:4)=zero_dp
  do i1=0,2
   ayh(jh+i1)=ay1(i1)
  end do
  curry(0)=-ayh(0)
  do i1=1,3
   curry(i1)=curry(i1-1)+ay0(i1-1)-ayh(i1)
  end do
  curry(4)=curry(3)-ayh(4)
  curry(0:4)=wght*curry(0:4)
  !=====================================
  !                                 Jx =>    Wz^0(0.5*wy^1+Wy^0)=Wz^0*ayh0
  !                                          Wz^1(wy^1+0.5*Wy^0)=Wz^1*ayh1
  !==============================
  ayh0(0:4)=0.5*ayh(0:4)
  ayh1(0:4)=ayh(0:4)
  do i1=1,3
   ayh0(i1)=ayh0(i1)+ay0(i1-1)
   ayh1(i1)=ayh1(i1)+0.5*ay0(i1-1)
  end do
  iy0=min(jh,1) ![0,1]
  iy1=max(jh+2,3)![3,4]
  !-----------
  j0=j0-1
  j=j-1

  ax=shz+xp0(3)
  k0=int(ax+0.5)
  sx=ax-real(k0,dp)
  sx2=sx*sx
  az0(1)=0.75-sx2
  az0(2)=0.5*(0.25+sx2+sx)
  az0(0)=1.-az0(1)-az0(2)

  ax=shz+xp1(3)
  k=int(ax+0.5)
  sx=ax-real(k,dp)
  sx2=sx*sx
  az1(1)=0.75-sx2
  az1(2)=0.5*(0.25+sx2+sx)
  az1(0)=1.-az1(1)-az1(2)
  ! Direct Jz inversion
  kh=k-k0+1
  azh(0:4)=zero_dp
  do i1=0,2
   azh(kh+i1)=az1(i1)
  end do
  currz(0)=-azh(0)
  do i1=1,3
   currz(i1)=currz(i1-1)+az0(i1-1)-azh(i1)
  end do
  currz(4)=currz(3)-azh(4)
  currz(0:4)=wght*currz(0:4)
  !----------
  k0=k0-1
  k=k-1
  iz0=min(kh,1)
  iz1=max(kh+2,3)
  !================Jx=DT*drho_x to be inverted==================
  jh=j0-1
  !====================
  ih=i0-1
  do k1=0,2
   do j1=iy0,iy1
    j2=jh+j1
    dvol=ayh0(j1)*az0(k1)
    dvolh=ayh1(j1)*az1(k1)
    do i1=ix0,ix1
     i2=ih+i1
     jcurr(i2,j2,k0+k1,1)=jcurr(i2,j2,k0+k1,1)+dvol*currx(i1)
     jcurr(i2,j2,k+k1,1)=jcurr(i2,j2,k+k1,1)+dvolh*currx(i1)
    end do
   end do
  end do
  !================Jy
  do k1=0,2
   do j1=iy0,iy1
    j2=jh+j1
    dvol=curry(j1)*az0(k1)
    dvolh=curry(j1)*az1(k1)
    do i1=ix0,ix1
     i2=ih+i1
     jcurr(i2,j2,k0+k1,2)=jcurr(i2,j2,k0+k1,2)+axh0(i1)*dvol
     jcurr(i2,j2,k+k1,2)=jcurr(i2,j2,k+k1,2)+axh1(i1)*dvolh
    end do
   end do
  end do
  !================Jz
  kh=k0-1

  do k1=iz0,iz1
   k2=kh+k1
   do j1=0,2
    dvol=ay0(j1)*currz(k1)
    dvolh=ay1(j1)*currz(k1)
    do i1=ix0,ix1
     i2=ih+i1
     jcurr(i2,j0+j1,k2,3)=jcurr(i2,j0+j1,k2,3)+axh0(i1)*dvol
     jcurr(i2,j+j1,k2,3)=jcurr(i2,j+j1,k2,3)+axh1(i1)*dvolh
    end do
   end do
  end do
 end do
 !============= Curr data on [1:n+4] extended range
 end subroutine esirkepov_3d_curr
 !===============================
 subroutine ionization_energy(ef,curr,pt,np,ndm,xmn,ymn,zmn)

 real(dp),intent(in) :: pt(:,:)
 real,intent(inout) :: ef(:,:,:,:),curr(:,:,:,:)
 integer,intent(in) :: np,ndm
 ! real(dp),intent(in) :: dt_loc
 real(dp),intent(in) :: xmn,ymn,zmn
 real(dp) :: ax1(0:2),ay1(0:2),az1(0:2)
 real(dp) :: axh(0:2),ayh(0:2),azh(0:2),gx(0:2,0:2),gy(0:2,0:2)
 real(dp) :: xx,sx,sx2,energy_loss,xp1(3),emod,dvol
 integer :: i,ih,i1,i2,ih2,j,jh,j1,j2,jh2,k,kh,k1,k2,n
 !==============================
 select case(ndm)
 case(2)
  k2=1
  emod=1.
  do n=1,np
   xp1(1)=dx_inv*(pt(n,1)-xmn)
   xp1(2)=dy_inv*(pt(n,2)-ymn)
   energy_loss=pt(n,5)

   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   ih=int(xx)
   sx=xx-0.5-real(ih,dp)
   sx2=sx*sx
   axh(1)=0.75-sx2
   axh(2)=0.5*(0.25+sx2+sx)
   axh(0)=1.-axh(1)-axh(2)
   !axh(1)=sx+0.5
   !axh(0)=1.-axh(1)

   xx=shx+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)
   jh=int(xx)
   sx=xx-0.5-real(jh,dp)
   sx2=sx*sx
   ayh(1)=0.75-sx2
   ayh(2)=0.5*(0.25+sx2+sx)
   ayh(0)=1.-ayh(1)-ayh(2)

   !ayh(1)=sx+0.5
   !ayh(0)=1.-ayh(1)

   i=i-1
   j=j-1
   ih=ih-1
   jh=jh-1
   do j1=0,2
    do i1=0,2
     gx(i1,j1)=energy_loss*axh(i1)*ay1(j1)
    end do
   end do
   do j1=0,2
    do i1=0,2
     gy(i1,j1)=energy_loss*ax1(i1)*ayh(j1)
    end do
   end do

   do j1=0,2
    j2=j+j1
    jh2=jh+j1
    do i1=0,2
     ih2=ih+i1
     i2=i+i1
     emod=ef(ih2,j2,k2,1)*ef(ih2,j2,k2,1)+ef(i2,jh2,k2,2)*ef(i2,j2,k2,2)
     curr(ih2,j2,k2,1)=curr(ih2,j2,k2,1)+ef(ih2,j2,k2,1)*gx(i1,j1)/emod
     curr(i2,jh2,k2,2)=curr(i2,jh2,k2,2)+ef(i2,jh2,k2,2)*gy(i1,j1)/emod
    end do
   end do
  end do
 case(3)
  do n=1,np
   xp1(1)=dx_inv*(pt(n,1)-xmn)
   xp1(2)=dy_inv*(pt(n,2)-ymn)
   xp1(3)=dz_inv*(pt(n,3)-zmn)

   energy_loss=pt(n,7)
   xx=shx+xp1(1)
   i=int(xx+0.5)
   sx=xx-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   axh(1)=sx+0.5
   axh(0)=1.-axh(1)

   xx=shx+xp1(2)
   j=int(xx+0.5)
   sx=xx-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   ayh(1)=sx+0.5
   ayh(0)=1.-ayh(1)

   xx=shz+xp1(3)
   k=int(xx+0.5)
   sx=xx-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   azh(1)=sx+0.5
   azh(0)=1.-azh(1)

   i=i-1
   j=j-1
   k=k-1

   ih=i
   jh=j
   kh=k

   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvol=energy_loss*ay1(j1)*az1(k1)
     do i1=0,1
      i2=ih+i1
      curr(i2,j2,k2,1)=curr(i2,j2,k2,1)+ef(i2,j2,k2,1)*dvol*axh(i1)
     end do
    end do
    do j1=0,1
     j2=jh+j1
     dvol=energy_loss*ayh(j1)*az1(k1)
     do i1=0,2
      i2=i+i1
      curr(i2,j2,k2,2)=curr(i2,j2,k2,2)+ef(i2,j2,k2,2)*dvol*ax1(i1)
     end do
    end do
   end do
   do k1=0,1
    k2=kh+k1
    do j1=0,2
     j2=j+j1
     dvol=energy_loss*ay1(j1)*azh(k1)
     do i1=0,2
      i2=i+i1
      curr(i2,j2,k2,3)=curr(i2,j2,k2,3)+ef(i2,j2,k2,3)*dvol*ax1(i1)
     end do
    end do
   end do
  end do
 end select
 !==============
 end subroutine ionization_energy
 !======================
 ! NO CHARGE PRESERVING SCHEMES
 !=========================
 subroutine ncdef_2d_curr(loc_sp,pt,jcurr,n0,np,s_ind,njc,ndm,xmn,ymn)

 type(species),intent(in) :: loc_sp
 real(dp),intent(inout) :: pt(:,:),jcurr(:,:,:,:)
 integer,intent(in) :: n0,np,s_ind,njc,ndm
 ! real(dp),intent(in) :: dt_loc
 real(dp),intent(in) :: xmn,ymn
 real(dp) :: ax,sx,sx2,gam_inv
 real(dp) :: axh0(2),axh1(2),ayh0(2),ayh1(2)
 real(dp) :: ax0(3),ay0(3),xp1(3),xp0(3)
 real(dp) :: ax1(3),ay1(3),vp(3),dvol(3)
 real(sp) :: wght
 integer :: i,j,i0,j0,i1,j1,i2,j2,n
 integer :: ih0,jh0,ih,jh
 !=======================
 !Enter pt(3:4) old x-y positions
 !=====================================
 if(ndm <2)then
  j2=1
  do n=n0,np
   wgh_cmp=loc_sp%part(n,5)
   wght=charge*wgh
   gam_inv=pt(n,5)           !stores dt/gam
   vp(1)=0.5*wght*pt(n,1)                !q*wgh*dt*Vx also in comoving
   vp(2)=loc_sp%part(n,4)  !(Py) momenta
   vp(2)=0.5*wght*gam_inv*vp(2) !1/2 *V*w*q*dt
   vp(3)=wght
   xp1(1:2)=loc_sp%part(n,1:2)  !(x,y) new
   xp1(1)=dx_inv*(xp1(1)-xmn)                !new positions
   xp1(2)=dy_inv*(xp1(2)-ymn)                !new positions
   xp0(1)=dx_inv*(pt(n,3)-xmn)     !old positions
   xp0(2)=dy_inv*(pt(n,4)-ymn)     !old positions

   ax=shx+xp0(1)
   i0=int(ax+0.5)
   sx=ax-real(i0,dp)
   sx2=sx*sx
   ax0(2)=0.75-sx2
   ax0(3)=0.5*(0.25+sx2+sx)
   ax0(1)=1.-ax0(3)-ax0(2)
   axh0(2)=sx+0.5
   axh0(1)=1.-axh0(2)

   ax=shx+xp1(1)
   i=int(ax+0.5)
   sx=ax-real(i,dp)
   sx2=sx*sx
   ax1(2)=0.75-sx2
   ax1(3)=0.5*(0.25+sx2+sx)
   ax1(1)=1.-ax1(3)-ax1(2)
   axh1(2)=sx+0.5
   axh1(1)=1.-axh1(2)

   i=i-2
   i0=i0-2
   ih=i
   ih0=i0
   do i1=1,2
    i2=ih0+i1
    jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)+vp(1)*axh0(i1)
    i2=ih+i1
    jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)+vp(1)*axh1(i1)
   end do

   do i1=1,3
    i2=i0+i1
    jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+vp(2)*ax0(i1)
    i2=i+i1
    jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+vp(2)*ax1(i1)
   end do
  end do
  !=============
  return
 endif
 !==============================
 select case(njc)
 case(2)      !2D-2V
  do n=n0,np
   pt(n,2)=loc_sp%part(n,2)  !(y) new  pt(n,3:4) x-y old
  end do
  if(s_ind==0)then
   do n=1,np
    pt(n,2)=dy_inv*(pt(n,2)-ymn)  !loc y new
    pt(n,4)=dy_inv*(pt(n,4)-ymn)  !loc y old
   end do
  else
   call map2dy_part_sind(np,s_ind,2,ymn,pt)
   call map2dy_part_sind(np,s_ind,4,ymn,pt)
  endif
!========== In pt(n,1)=dt*V_x in  pt(n,5) = dt/gam
  do n=n0,np
   wgh_cmp=loc_sp%part(n,5)
   wght=charge*wgh                   !w*q for  q=e, ion_charge
   vp(2)=wght*pt(n,5)*loc_sp%part(n,4)    !dt*q*wgh*Vy
   vp(2)=0.5*vp(2)               !1/2 * V*q*wgh*dt

   pt(n,1)=loc_sp%part(n,1)          !new x position
   xp1(1)=dx_inv*(pt(n,1)-xmn)
   xp0(1)=dx_inv*(pt(n,3)-xmn)       !old x position
!============================
   xp1(2)=pt(n,2)                !norm new and y-positions
   xp0(2)=pt(n,4)

   ax=shx+xp0(1)
   i0=int(ax+0.5)
   sx=ax-real(i0,dp)
   sx2=sx*sx
   ax0(2)=0.75-sx2
   ax0(3)=0.5*(0.25+sx2+sx)
   ax0(1)=1.-ax0(3)-ax0(2)

   ax=shx+xp1(1)
   i=int(ax+0.5)
   sx=ax-real(i,dp)
   sx2=sx*sx
   ax1(2)=0.75-sx2
   ax1(3)=0.5*(0.25+sx2+sx)
   ax1(1)=1.-ax1(3)-ax1(2)

   i=i-1
   i0=i0-1

   ax=shy+xp0(2)
   j0=int(ax+0.5)
   sx=ax-real(j0,dp)
   sx2=sx*sx
   ay0(2)=0.75-sx2
   ay0(3)=0.5*(0.25+sx2+sx)
   ay0(1)=1.-ay0(3)-ay0(2)
   !=========
   ax=shy+xp1(2)
   j=int(ax+0.5)
   sx=ax-real(j,dp)
   sx2=sx*sx
   ay1(2)=0.75-sx2
   ay1(3)=0.5*(0.25+sx2+sx)
   ay1(1)=1.-ay1(3)-ay1(2)
   !-----------
   j0=j0-1
   j=j-1
   !===============[Jx=  rho^{new} - rho_{old}==============
   do j1=1,3
    j2=j+j1
    dvol(1)=wght*ay1(j1)
    do i1=1,3
     i2=i+i1
     jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)+dvol(1)*ax1(i1)
    end do
    j2=j0+j1
    dvol(1)=wght*ay0(j1)
    do i1=1,3
     i2=i0+i1
     jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)-dvol(1)*ax0(i1)
    end do
   end do
   !=============
   do j1=1,2
    j2=jh0+j1
    dvol(2)=vp(2)*ayh0(j1)
    do i1=1,3
     i2=i0+i1
     jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+dvol(2)*ax0(i1)
    end do
    j2=jh+j1
    dvol(2)=vp(2)*ayh1(j1)
    do i1=1,3
     i2=i+i1
     jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+dvol(2)*ax1(i1)
    end do
   end do
  end do
  !==============
 case(3)  !2D +3V
  !===================
  do n=n0,np
   pt(n,2)=loc_sp%part(n,2)  !(y) new
   pt(n,4)=dx_inv*(pt(n,4)-xmn)
  end do
  if(s_ind==0)then
   do n=n0,np
    pt(n,2)=dy_inv*(pt(n,2)-ymn)  !loc y new
    pt(n,5)=dy_inv*(pt(n,5)-ymn)  !loc y new
   end do
  else
   call map2dy_part_sind(np,s_ind,2,ymn,pt)
   call map2dy_part_sind(np,s_ind,5,ymn,pt)
  endif
  do n=n0,np
   wgh_cmp=loc_sp%part(n,7)
   wght=real(charge*wgh,sp)      !w*q for  q=ion_charge
   vp(1)=0.5*wght*pt(n,1)        !dt*w*q*V_x/2
   vp(2:3)=loc_sp%part(n,5:6)
   gam_inv=wght*pt(n,7)
   vp(2:3)=0.5*gam_inv*vp(2:3)  !1/2*dt*q*w*vp (y-z)comp

   pt(n,1)=loc_sp%part(n,1)  !(x) new
   pt(n,1)=dx_inv*(pt(n,1)-xmn)
   xp1(1:2)=pt(n,1:2)        !new (x,y) positions
   xp0(1:2)=pt(n,4:5)        !old (x,y) positions

   ax=shx+xp0(1)
   i0=int(ax+0.5)
   sx=ax-real(i0,dp)
   sx2=sx*sx
   ax0(2)=0.75-sx2
   ax0(3)=0.5*(0.25+sx2+sx)
   ax0(1)=1.-ax0(3)-ax0(2)

   ax=shx+xp1(1)
   i=int(ax+0.5)
   sx=ax-real(i,dp)
   sx2=sx*sx
   ax1(2)=0.75-sx2
   ax1(3)=0.5*(0.25+sx2+sx)
   ax1(1)=1.-ax1(3)-ax1(2)

   i=i-1
   i0=i0-1

   ax=shy+xp0(2)
   j0=int(ax+0.5)
   sx=ax-real(j0,dp)
   sx2=sx*sx
   ay0(2)=0.75-sx2
   ay0(3)=0.5*(0.25+sx2+sx)
   ay0(1)=1.-ay0(3)-ay0(2)
   !=========
   ax=shy+xp1(2)
   j=int(ax+0.5)
   sx=ax-real(j,dp)
   sx2=sx*sx
   ay1(2)=0.75-sx2
   ay1(3)=0.5*(0.25+sx2+sx)
   ay1(1)=1.-ay1(3)-ay1(2)
   !-----------
   j0=j0-1
   j=j-1
   !===============[Jx=Drho]==================
   do j=1,3
    j2=j+j1
    dvol(1)=wght*ay1(j1)
    do i1=1,3
     i2=i+i1
     jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)+dvol(1)*ax1(i1)
    end do
    j2=j0+j1
    dvol(1)=wght*ay0(j1)
    do i1=1,3
     i2=i0+i1
     jcurr(i2,j2,1,1)=jcurr(i2,j2,1,1)-dvol(1)*ax0(i1)
    end do
   end do
   do j1=1,3
   j2=j0+j1
    dvol(2:3)=vp(2:3)*ay0(j1)
    do i1=1,3
     i2=i0+i1
     jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+dvol(2)*ax0(i1)
     jcurr(i2,j2,1,3)=jcurr(i2,j2,1,3)+dvol(3)*ax0(i1)
    end do
    j2=j+j1
    dvol(2:3)=vp(2:3)*ay1(j1)
    do i1=1,3
     i2=i+i1
     jcurr(i2,j2,1,2)=jcurr(i2,j2,1,2)+dvol(2)*ax1(i1)
     jcurr(i2,j2,1,3)=jcurr(i2,j2,1,3)+dvol(3)*ax1(i1)
    end do
   end do
  end do
 end select
 !============= Curr data on [0:n+3] extended range
 end subroutine ncdef_2d_curr
 !===============
 !========================
 subroutine ncdef_3d_curr(sp_loc,pt,jcurr,n0,np,s_ind,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pt(:,:),jcurr(:,:,:,:)
 integer,intent(in) :: n0,np,s_ind
 ! real(dp),intent(in) :: dt_loc
 real(dp),intent(in) :: xmn,ymn,zmn
 real(dp) :: ax,sx,sx2,dvol(3),gam_inv
 real(dp) :: xp0(3),xp1(3)
 real(dp) :: ax0(0:2),ay0(0:2),az0(0:2)
 real(dp) :: ax1(0:2),ay1(0:2),az1(0:2),vp(3)
 real(dp) :: axh0(0:1),axh1(0:1),ayh0(0:1),ayh1(0:1),azh0(0:1),azh1(0:1)
 real(sp) :: wght
 integer :: i,j,k,i0,j0,k0,i1,j1,k1,i2,j2,k2,n
 integer :: ih,jh,kh,ih0,jh0,kh0
 !=======================
 ! WARNING: NO X-stretch allowed
 ! Current densities defined by alternating order (quadratic/linear) shapes
 ! Enter pt(4:6)=old positions sp_loc(1:3)=new positions
 !WARNING : to be used ONLY within the one cycle partcle integration scheme
 !==========================================
 ! Exit in jcurr(1:3) =[Drho,J_y,J_z]   !Drho= rho^{new}-rho^{old}
 ! Component J_x recovered by enforcing the continuity equation on a grid
 !=============================================
 ax1(0:2)=zero_dp;ay1(0:2)=zero_dp
 az1(0:2)=zero_dp;az0(0:2)=zero_dp
 ax0(0:2)=zero_dp;ay0(0:2)=zero_dp
 axh0(0:1)=zero_dp;ayh0(0:1)=zero_dp
 azh0(0:1)=zero_dp;azh1(0:1)=zero_dp
 axh1(0:1)=zero_dp;ayh1(0:1)=zero_dp
!====================================
 do n=n0,np
  pt(n,2:3)=sp_loc%part(n,2:3)  !(y,z) new
 end do
 if(s_ind==0)then
  do n=n0,np
   pt(n,2)=dy_inv*(pt(n,2)-ymn)  !loc y new
   pt(n,3)=dz_inv*(pt(n,3)-zmn)  !loc z new
   pt(n,5)=dy_inv*(pt(n,5)-ymn)  !loc y-old
   pt(n,6)=dz_inv*(pt(n,6)-zmn)  !loc z-old
  end do
 else
  call map3d_part_sind(pt,np,s_ind,2,3,ymn,zmn)
  call map3d_part_sind(pt,np,s_ind,5,6,ymn,zmn)
 endif
 do n=n0,np
  vp(2:3)=sp_loc%part(n,5:6)     !Momenta at t^{n+1/2}
  wgh_cmp=sp_loc%part(n,7)
  wght=real(charge*wgh,sp)       !w*q for  q=charge
  gam_inv=wght*pt(n,7)           !q*wgh*dt/gam
  vp(2:3)=0.5*gam_inv*vp(2:3)    !wgh*q*dt*V factor 1/2 from density average

  pt(n,1)=sp_loc%part(n,1)  !(x) new
  xp1(1)=dx_inv*(pt(n,1)-xmn)
  xp0(1)=dx_inv*(pt(n,4)-xmn)
  xp0(2:3)=pt(n,5:6)
  xp1(2:3)=pt(n,2:3)
  !================================== old  x^n
  ax=shx+xp0(1)
  i0=int(ax+0.5)
  sx=ax-real(i0,dp)
  sx2=sx*sx
  ax0(1)=0.75-sx2
  ax0(2)=0.5*(0.25+sx2+sx)
  ax0(0)=1.-ax0(1)-ax0(2)

  axh0(1)=sx+0.5
  axh0(0)=1.-axh0(1)
  !===================================== new x^{n+1}
  ax=shx+xp1(1)
  i=int(ax+0.5)
  sx=ax-real(i,dp)
  sx2=sx*sx
  ax1(1)=0.75-sx2
  ax1(2)=0.5*(0.25+sx2+sx)
  ax1(0)=1.-ax1(1)-ax1(2)

  axh1(1)=sx+0.5
  axh1(0)=1.-axh1(1)
  i=i-1
  i0=i0-1
  ih=i
  ih0=i0

  ax=shy+xp0(2)
  j0=int(ax+0.5)
  sx=ax-real(j0,dp)
  sx2=sx*sx
  ay0(1)=0.75-sx2
  ay0(2)=0.5*(0.25+sx2+sx)
  ay0(0)=1.-ay0(1)-ay0(2)
  ayh0(1)=sx+0.5
  ayh0(0)=1.-ayh0(1)
  !=========
  ax=shy+xp1(2)
  j=int(ax+0.5)
  sx=ax-real(j,dp)
  sx2=sx*sx
  ay1(1)=0.75-sx2
  ay1(2)=0.5*(0.25+sx2+sx)
  ay1(0)=1.-ay1(1)-ay1(2)

  ayh1(1)=sx+0.5
  ayh1(0)=1.-ayh1(1)
  !-----------
  j0=j0-1
  j=j-1
  jh=j
  jh0=j0

  ax=shz+xp0(3)
  k0=int(ax+0.5)
  sx=ax-real(k0,dp)
  sx2=sx*sx
  az0(1)=0.75-sx2
  az0(2)=0.5*(0.25+sx2+sx)
  az0(0)=1.-az0(1)-az0(2)

  azh0(1)=sx+0.5
  azh0(0)=1.-azh0(1)

  ax=shz+xp1(3)
  k=int(ax+0.5)
  sx=ax-real(k,dp)
  sx2=sx*sx
  az1(1)=0.75-sx2
  az1(2)=0.5*(0.25+sx2+sx)
  az1(0)=1.-az1(1)-az1(2)

  azh1(1)=sx+0.5
  azh1(0)=1.-azh1(1)

  k0=k0-1
  k=k-1
  kh=k
  kh0=k0
!======================in jcurr(1)=rho^new-rho_old
  do k1=0,2
   k2=k+k1
   do j1=0,2
    j2=j+j1
    dvol(1)=wght*ay1(j1)*az1(k1)
    do i1=0,2
     i2=i+i1
     jcurr(i2,j2,k2,1)=jcurr(i2,j2,k2,1)+dvol(1)*ax1(i1)
    end do
   end do
   k2=k0+k1
   do j1=0,2
    j2=j0+j1
    dvol(1)=wght*ay0(j1)*az0(k1)
    do i1=0,2
     i2=i0+i1
     jcurr(i2,j2,k2,1)=jcurr(i2,j2,k2,1)-dvol(1)*ax0(i1)
    end do
   end do
  end do
  !================Jy-Jz=============
  do k1=0,2
   k2=k0+k1
   do j1=0,1
    j2=jh0+j1
    dvol(2)=vp(2)*ayh0(j1)*az0(k1)
    do i1=0,2
     i2=i0+i1
     jcurr(i2,j2,k2,2)=jcurr(i2,j2,k2,2)+dvol(2)*ax0(i1)
    end do
   end do
   k2=k+k1
   do j1=0,1
    j2=jh+j1
    dvol(2)=vp(2)*ayh1(j1)*az1(k1)
    do i1=0,2
     i2=i+i1
     jcurr(i2,j2,k2,2)=jcurr(i2,j2,k2,2)+dvol(2)*ax1(i1)
    end do
   end do
  end do
  do k1=0,1
   k2=kh0+k1
   do j1=0,2
    j2=j0+j1
    dvol(3)=vp(3)*ay0(j1)*azh0(k1)
    do i1=0,2
     i2=i0+i1
     jcurr(i2,j2,k2,3)=jcurr(i2,j2,k2,3)+dvol(3)*ax0(i1)
    end do
   end do
   k2=kh+k1
   do j1=0,2
    j2=j+j1
    dvol(3)=vp(3)*ay1(j1)*azh1(k1)
    do i1=0,2
     i2=i+i1
     jcurr(i2,j2,k2,3)=jcurr(i2,j2,k2,3)+dvol(3)*ax1(i1)
    end do
   end do
  end do
 end do
 !============= Curr and density data on [0:n+3] extended range
 end subroutine ncdef_3d_curr
 !==========================
 subroutine ncdef_rk_curr(pt,curr,s_ind,np,ndm,xmn,ymn,zmn)

 real(dp),intent(inout) :: pt(:,:)
 real(dp),intent(out) :: curr(:,:,:,:)
 integer,intent(in) :: s_ind,np,ndm
 real(dp),intent(in) :: xmn,ymn,zmn
 real(dp) :: ax,sx,sx2,dvolh,dvol(3)
 real(dp) :: ax1(0:2),ay1(0:2),az1(0:2),xp1(3),vp(3)
 real(dp) :: axh1(0:2),ayh1(0:2),azh1(0:2)
 integer :: i,ih,j,jh,k,kh,i1,j1,k1,i2,j2,k2,n
 !====================
 ! NON CONSERVATIVE CURRENTS J^{i-1}=<V^{i-1}*rho^{i-1}>
 !===========================
 select case(ndm)
 case(1)
  j2=1;k2=1
  do n=1,np
   xp1(1)=pt(n,1)       !x-positions at current times (t^{i-1})
   vp(1:2)=pt(n,3:4)    !wgh*dt_rk*velocity at current times t^{i-1}

   ax=shx+dx_inv*(xp1(1)-xmn)
   i=int(ax+0.5)
   sx=ax-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   !=====================
   ih=int(ax)
   sx=ax-0.5-real(ih,dp)
   sx2=sx*sx
   axh1(1)=0.75-sx2
   axh1(2)=0.5*(0.25+sx2+sx)
   axh1(0)=1.-axh1(1)-axh1(2)
   !=========
   i=i-1
   ih=ih-1
   do i1=0,2
    i2=ih+i1
    curr(i2,j2,k2,1)=curr(i2,j2,k2,1)+vp(1)*axh1(i1)
    i2=i+i1
    curr(i2,j2,k2,2)=curr(i2,j2,k2,2)+vp(2)*ax1(i1)
   end do
  end do
 case(2)
  k2=1
  if(s_ind==0)then
   do n=1,np
    pt(n,2)=dy_inv*(pt(n,2)-ymn)
   end do
  else
   call map2dy_part_sind(np,s_ind,2,ymn,pt)
  endif
  do n=1,np
   xp1(1:2)=pt(n,1:2)       !positions at current times (t^{i-1})
   vp(1:2)=pt(n,3:4)        !wgh*dt_rk*velocity at current times t^{i-1}

   ax=shx+dx_inv*(xp1(1)-xmn)
   i=int(ax+0.5)
   sx=ax-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   axh1(1)=sx+0.5
   axh1(0)=1.-axh1(1)
   !=========
   ax=shy+xp1(2)
   j=int(ax+0.5)
   sx=ax-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)

   ayh1(1)=sx+0.5
   ayh1(0)=1.-ayh1(1)

   i=i-1
   ih=i
   j=j-1
   jh=j
   do j1=0,2
    j2=j+j1
    dvolh=ay1(j1)
    dvol(1)=vp(1)*dvolh
    do i1=0,1
     i2=ih+i1
     curr(i2,j2,k2,1)=curr(i2,j2,k2,1)+dvol(1)*axh1(i1)
    end do
   end do
   do j1=0,1
    j2=jh+j1
    dvolh=ayh1(j1)
    dvol(2)=vp(2)*dvolh
    do i1=0,2
     i2=i+i1
     curr(i2,j2,k2,2)=curr(i2,j2,k2,2)+dvol(2)*ax1(i1)
    end do
   end do
  end do
 case(3)
  if(s_ind==0)then
   do n=1,np
    xp1(2:3)=pt(n,2:3)                !new y-z positions
    pt(n,2)=dy_inv*(xp1(2)-ymn)
    pt(n,3)=dy_inv*(xp1(3)-zmn)
   end do
  else
   call map3d_part_sind(pt,np,s_ind,2,3,ymn,zmn)
  endif
  do n=1,np
   xp1(1:3)=pt(n,1:3)         !positions at current time t^{i-1}
   vp(1:3)=pt(n,4:6)          !wgh*dt_rk*velocities at current time

   ax=shx+dx_inv*(xp1(1)-xmn)
   i=int(ax+0.5)
   sx=ax-real(i,dp)
   sx2=sx*sx
   ax1(1)=0.75-sx2
   ax1(2)=0.5*(0.25+sx2+sx)
   ax1(0)=1.-ax1(1)-ax1(2)
   axh1(1)=sx+0.5
   axh1(0)=1.-axh1(1)
   !=========
   ax=shy+xp1(2)
   j=int(ax+0.5)
   sx=ax-real(j,dp)
   sx2=sx*sx
   ay1(1)=0.75-sx2
   ay1(2)=0.5*(0.25+sx2+sx)
   ay1(0)=1.-ay1(1)-ay1(2)
   ayh1(1)=sx+0.5
   ayh1(0)=1.-ayh1(1)

   ax=shz+xp1(3)
   k=int(ax+0.5)
   sx=ax-real(k,dp)
   sx2=sx*sx
   az1(1)=0.75-sx2
   az1(2)=0.5*(0.25+sx2+sx)
   az1(0)=1.-az1(1)-az1(2)

   azh1(1)=sx+0.5
   azh1(0)=1.-azh1(1)
   i=i-1
   j=j-1
   k=k-1
   ih=i
   jh=j
   kh=k
   do k1=0,2
    k2=k+k1
    do j1=0,2
     j2=j+j1
     dvolh=ay1(j1)*az1(k1)
     dvol(1)=vp(1)*dvolh
     do i1=0,1
      i2=ih+i1
      curr(i2,j2,k2,1)=curr(i2,j2,k2,1)+dvol(1)*axh1(i1)
     end do
    end do
    do j1=0,1
     j2=jh+j1
     dvolh=ayh1(j1)*az1(k1)
     dvol(2)=vp(2)*dvolh
     do i1=0,2
      i2=i+i1
      curr(i2,j2,k2,2)=curr(i2,j2,k2,2)+dvol(2)*ax1(i1)
     end do
    end do
   end do
   do k1=0,1
    k2=kh+k1
    do j1=0,2
     j2=j+j1
     dvolh=ay1(j1)*azh1(k1)
     dvol(3)=vp(3)*dvolh
     do i1=0,2
      i2=i+i1
      curr(i2,j2,k2,3)=curr(i2,j2,k2,3)+dvol(3)*ax1(i1)
     end do
    end do
   end do
  end do
 end select
 end subroutine ncdef_rk_curr
 !====================
 end module particles
 !==========================
