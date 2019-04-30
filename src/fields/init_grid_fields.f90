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

 module init_grid_field

 use grid_field_param
 use array_wspace

 implicit none
 contains
 !=====================
 subroutine initial_beam_fields(pot,efb,i1,nxp,j1,nyp,k1,nzp,g2,bet)
 real(dp),intent(inout) :: pot(:,:,:,:)
 real(dp),intent(out) :: efb(:,:,:,:)
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp
 real(dp),intent(in) :: g2,bet
 integer :: i,j,k,ic,jj,kk
 real(dp) :: sdhy,sdhz

 ! Enter
 ! in pot(1) enters pot_b(i,j,k) => (Ex,Ey, Ez)
 ! in pot(2) enters Jx(i,j,k)=bet*rho => efb[4]=jx[i+1/2,j,k]
 !Computes
 !Ey=-Dy[pot] Ez=-Dz[pot]  Ex=-Dx[pot]/gam2
 !Bz=-Dy[Ax]=bet*Ey     By=Dz[Ax]=-bet*Ez   Bx=0
 !============================
 !Interpolation to the Yee grid is needed for [By,Bz] fields 
 !==============================================================
 ic=1
 if(pe1y)then
  j=nyp
  do k=k1,nzp
   do i=i1,nxp
    pot(i,j+1,k,ic)=3.*(pot(i,j,k,ic)-pot(i,j-1,k,ic))+pot(i,j-2,k,ic)
   end do
  end do
 endif
 if(pe1x)then
  do k=k1,nzp
   do j=j1,nyp
    pot(nxp+1,j,k,1)=pot(nxp,j,k,1)
    pot(nxp+1,j,k,2)=2.*pot(nxp,j,k,2)-pot(nxp-1,j,k,2)
   end do
  end do
 endif
! Interpolates jx=Jx[i+1/2,j,k]
 do k=k1,nzp
  do j=j1,nyp
   do i=i1,nxp
    efb(i,j,k,4)=0.5*(pot(i,j,k,2)+pot(i+1,j,k,2))
   end do
  end do
 end do
 do k=k1,nzp
  do j=j1,nyp
   jj=j-2
   sdhy=loc_yg(jj,3,imody)*dy_inv
   do i=i1,nxp+1
    efb(i,j,k,2)=-sdhy*(pot(i,j+1,k,1)-pot(i,j,k,1))
   end do
   do i=i1,nxp
    efb(i,j,k,1)=-dx_inv*(pot(i+1,j,k,1)-pot(i,j,k,1))/g2
   end do
  end do
 end do
 if(ndim==2)then  !defines Bz[i+1/2,j+1/2,k] using interpolated Ey
 do k=k1,nzp
  do j=j1,nyp
   do i=i1,nxp
    efb(i,j,k,3)=0.5*bet*(efb(i,j,k,2)+efb(i+1,j,k,2))
   end do
  end do
 end do
  !Bz[i+1/2,j+1/2,k]
  return
 endif
!==============Here only 3D case
  if(pe1z)then
   k=nzp
   do j=j1,nyp
    do i=i1,nxp
     pot(i,j,k+1,1)=2.*pot(i,j,k,1)-pot(i,j,k-1,1)
    end do
   end do
  endif
 do k=k1,nzp
  do j=j1,nyp
   do i=i1,nxp
    efb(i,j,k,6)=0.5*bet*(efb(i,j,k,2)+efb(i+1,j,k,2))
   end do
  end do
 end do
  do k=k1,nzp
   kk=k-2
   sdhz=loc_zg(kk,3,imodz)*dz_inv
   do j=j1,nyp
    do i=i1,nxp
     efb(i,j,k,3)=-sdhz*(pot(i,j,k+1,1)-pot(i,j,k,1))
    end do
   end do
  end do                ![Ex,Ey,Ez, Bz]defined

 do k=k1,nzp
  do j=j1,nyp
   do i=i1,nxp
    pot(i,j,k,2)=0.5*(efb(i,j,k,3)+efb(i+1,j,k,3))
    efb(i,j,k,5)=-bet*pot(i,j,k,2)
   end do
  end do
 end do
 !  By[i+1/2,j+1/2,k=--bet*Ez
!======================================
 end subroutine initial_beam_fields
 !===========================================
 ! END SECTION FOR initial beam fields
 !==================================
 ! SECTION for initial fields in ENVELOPE MODEL
 !======================================
 subroutine init_envelope_field(ef,e0,dt_loc,t_loc,tf,wx,&
  wy,xf0,om0,pw,i1,i2,ycent,zcent)

 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,dt_loc,t_loc,tf,wx,wy,xf0,om0
 integer,intent(in) :: pw,i1,i2
 integer :: j1,j2,k1,k2
 real(dp) :: xx,yy,zz,r2,w2
 real(dp) :: t,tm,zra,ycent,zcent
 real(dp) :: pih,phi,phi0,phi1,phx
 real(dp) :: A0,Ar,Ai
 integer :: i,j,k,ii,jj,kk


 ! inviluppo temporale= cos^2(pi*(t-x))/wx)
 ! eps=1./k0*wy k0=omega_0=omgl
 ! Ay(i,j,k) complex envelope in paraxial approximation
 !========================
 t=t_loc-tf
 tm=t-dt_loc
 zra=0.5*om0*wy*wy
 pih=0.5*acos(-1.0)
 if(pw ==0)then       !plane wave model xf0=xc (center) tf=0
  j1=loc_ygrid(imody)%p_ind(1)
  j2=loc_ygrid(imody)%p_ind(2)
  if(ndim<3)then
   k1=1;k2=1
  else
   k1=loc_zgrid(imodz)%p_ind(1)
   k2=loc_zgrid(imodz)%p_ind(2)
  endif
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xx=xx-xf0
     phi1=pi*(xx-t)/wx
     if(abs(phi1)>pih)phi1=pih
     phi0=pi*(xx-tm)/wx
     if(abs(phi0)>pih)phi0=pih
     phi=0.0

     Ar=e0*cos(phi)
     Ai=-e0*sin(phi)
     A0=cos(phi1)*cos(phi1)
                                     !A0=A0*A0
     ef(i,j,k,1)=ef(i,j,k,1)+A0*Ar    !Re[Ay](t_loc)
     ef(i,j,k,2)=ef(i,j,k,2)+A0*Ai    !Im[Ay]
     A0=cos(phi0)*cos(phi0)
                                     !A0=A0*A0
     ef(i,j,k,3)=ef(i,j,k,3)+A0*Ar    !Re[Ay](t_loc-Dt)
     ef(i,j,k,4)=ef(i,j,k,4)+A0*Ai    !Im[Ay]
    end do
   end do
  end do
  return
 endif
 if(ndim < 3)then
  j1=loc_ygrid(imody)%p_ind(1)
  j2=loc_ygrid(imody)%p_ind(2)
  k=1
  do j=j1,j2
   jj=j-2
   yy=loc_yg(jj,1,imody)
   yy=(yy-ycent)/wy
   r2=yy*yy
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xx=xx-xf0
    phi1=pi*(xx-t)/wx
    if(abs(phi1)>pih)phi1=pih
    phi0=pi*(xx-tm)/wx
    if(abs(phi0)>pih)phi0=pih
    xx=xx/zra
    w2=1./(1.+xx*xx)
    phx=atan(xx)
    phi=phx-xx*r2*w2

    Ar=e0*cos(phi)*sqrt(sqrt(w2))*exp(-w2*r2)
    Ai=-e0*sin(phi)*sqrt(sqrt(w2))*exp(-w2*r2)
    A0=cos(phi1)*cos(phi1)
                                     !A0=A0*A0
    ef(i,j,k,1)=ef(i,j,k,1)+A0*Ar    !Re[Ay](t_loc)
    ef(i,j,k,2)=ef(i,j,k,2)+A0*Ai    !Im[Ay]
    A0=cos(phi0)*cos(phi0)
                                     !A0=A0*A0
    ef(i,j,k,3)=ef(i,j,k,3)+A0*Ar    !Re[Ay](t_loc-Dt)
    ef(i,j,k,4)=ef(i,j,k,4)+A0*Ai    !Im[Ay]
   end do
  end do
  return
 endif
 !=============== 3D cartesian
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
 do k=k1,k2
  kk=k-2
  zz=loc_zg(kk,1,imodz)
  zz=(zz-zcent)/wy
  do j=j1,j2
   jj=j-2
   yy=loc_yg(jj,1,imody)
   yy=(yy-ycent)/wy
   r2=(yy*yy+zz*zz)
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xx=xx-xf0
    phi1=pi*(t-xx)/wx
    if(abs(phi1)>pih)phi1=pih
    phi0=pi*(tm-xx)/wx
    if(abs(phi0)>pih)phi0=pih
    xx=xx/zra
    w2=1./(1.+xx*xx)
    phx=atan(xx)
    phi=phx-xx*r2*w2

    A0=cos(phi1)*cos(phi1)
    Ar=e0*cos(phi)*sqrt(w2)*exp(-w2*r2)
    Ai=-e0*sin(phi)*sqrt(w2)*exp(-w2*r2)
    ef(i,j,k,1)=ef(i,j,k,1)+A0*Ar    !Re[Ay](t_loc)
    ef(i,j,k,2)=ef(i,j,k,2)+A0*Ai    !Im[Ay]
    A0=cos(phi0)*cos(phi0)
    ef(i,j,k,3)=ef(i,j,k,3)+A0*Ar    !Re[Ay](t_loc-Dt)
    ef(i,j,k,4)=ef(i,j,k,4)+A0*Ai    !Im[Ay]
   end do
  end do
 end do
 end subroutine init_envelope_field
!========================
 subroutine init_gprof_envelope_field(ef,e0,dt_loc,t_loc,tf,wx,&
  wy,xf0,om0,pw,i1,i2,ycent,zcent)

 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,dt_loc,t_loc,tf,wx,wy,xf0,om0
 integer,intent(in) :: i1,i2,pw
 integer :: j1,j2,k1,k2
 real(dp) :: xx,yy,zz,r2,w2
 real(dp) :: t,tm,zra,ycent,zcent
 real(dp) :: pih,phi,phi0,phi1,phx
 real(dp) :: A0,Ar,Ai
 integer :: i,j,k,ii,jj,kk


 ! inviluppo temporale= 
 ! eps=1./k0*wy k0=omega_0=omgl
 ! Ay(i,j,k) complex envelope in paraxial approximation
 ! xf0= xc+tf
 !========================
 t=t_loc-tf
 tm=t-dt_loc
 zra=0.5*om0*wy*wy
 pih=0.5*acos(-1.0)
 if(pw==0)then
  j1=loc_ygrid(imody)%p_ind(1)
  j2=loc_ygrid(imody)%p_ind(2)
  if(ndim<3)then
   k1=1;k2=1
  else
   k1=loc_zgrid(imodz)%p_ind(1)
   k2=loc_zgrid(imodz)%p_ind(2)
  endif
 do k=k1,k2
  do j=j1,j2
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xx=xx-xf0
    phi1=(xx-t)/wx        !phi1=(x-xf+tf)/wx=(x-xc)/wx > longitudinal shape 
    phi0=(xx-tm)/wx
    phx=atan(xx)
    phi=phx

    Ar=e0*cos(phi)
    Ai=-e0*sin(phi)
    A0=exp(-phi1*phi1)
    ef(i,j,k,1)=ef(i,j,k,1)+A0*Ar    !Re[Ay](t_loc)
    ef(i,j,k,2)=ef(i,j,k,2)+A0*Ai    !Im[Ay]
    A0=exp(-phi0*phi0)
    ef(i,j,k,3)=ef(i,j,k,3)+A0*Ar    !Re[Ay](t_loc-Dt)
    ef(i,j,k,4)=ef(i,j,k,4)+A0*Ai    !Im[Ay]
   end do
  end do
 end do
 return
endif
!==========================
 if(ndim < 3)then
  j1=loc_ygrid(imody)%p_ind(1)
  j2=loc_ygrid(imody)%p_ind(2)
  k=1
  do j=j1,j2
   jj=j-2
   yy=loc_yg(jj,1,imody)
   yy=(yy-ycent)/wy
   r2=yy*yy
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xx=xx-xf0
    phi1=(xx-t)/wx        !phi1=(x-xf+tf)/wx=(x-xc)/wx > longitudinal shape 
    phi0=(xx-tm)/wx
    xx=xx/zra             !xx=(x-xf)/Zr
    w2=1./(1.+xx*xx)
    phx=atan(xx)
    phi=phx-xx*r2*w2

    Ar=e0*cos(phi)*sqrt(sqrt(w2))*exp(-w2*r2)
    Ai=-e0*sin(phi)*sqrt(sqrt(w2))*exp(-w2*r2)
    A0=exp(-phi1*phi1)
    ef(i,j,k,1)=ef(i,j,k,1)+A0*Ar    !Re[Ay](t_loc)
    ef(i,j,k,2)=ef(i,j,k,2)+A0*Ai    !Im[Ay]
    A0=exp(-phi0*phi0)
    ef(i,j,k,3)=ef(i,j,k,3)+A0*Ar    !Re[Ay](t_loc-Dt)
    ef(i,j,k,4)=ef(i,j,k,4)+A0*Ai    !Im[Ay]
   end do
  end do
  return
 endif
 !=============== 3D cartesian
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
 do k=k1,k2
  kk=k-2
  zz=loc_zg(kk,1,imodz)
  zz=(zz-zcent)/wy
  do j=j1,j2
   jj=j-2
   yy=loc_yg(jj,1,imody)
   yy=(yy-ycent)/wy
   r2=(yy*yy+zz*zz)
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xx=xx-xf0
    phi1=(xx-t)/wx
    phi0=(xx-tm)/wx
    xx=xx/zra
    w2=1./(1.+xx*xx)
    phx=atan(xx)
    phi=phx-xx*r2*w2

    Ar=e0*cos(phi)*sqrt(w2)*exp(-w2*r2)
    Ai=-e0*sin(phi)*sqrt(w2)*exp(-w2*r2)
    A0=exp(-phi1*phi1)
    ef(i,j,k,1)=ef(i,j,k,1)+A0*Ar    !Re[Ay](t_loc)
    ef(i,j,k,2)=ef(i,j,k,2)+A0*Ai    !Im[Ay]
    A0=exp(-phi0*phi0)
    ef(i,j,k,3)=ef(i,j,k,3)+A0*Ar    !Re[Ay](t_loc-Dt)
    ef(i,j,k,4)=ef(i,j,k,4)+A0*Ai    !Im[Ay]
   end do
  end do
 end do
 end subroutine init_gprof_envelope_field
 !==============
 ! END INIT_ENV SECTION
 !==================================
 !=================================
 ! INITIAL (E,B) Laser FIELDS
 !==============================
 subroutine get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
 real(dp),intent(in) :: coords(4),par_lp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1, phig00, phig10, csphig01
 real(dp) :: x1, y1, t1,r2,w2
 real(dp):: A0, A1,tshape,phx,wshape
 !========== enter
 !par_lp(1)=oml
 !par_lp(3)=wx
 !par_lp(4)=wy
 !par_lp(5)=zra
 !par_lp(6)=eps
 !par_lp(7)=sigma    =1/(oml*wx)
 !===============================
 x1=coords(1)     !x-x_f
 y1=coords(2)/par_lp(4)
 t1=coords(4)     !t-t_f        => (t1-x1)= t-(x-xc)
 !====================
 r2=y1*y1
 phi0=par_lp(1)*(t1-x1)
 phi1=(t1-x1)/par_lp(3)
 x1=x1/par_lp(5)
 w2=1./(1.+x1*x1)   !     w2=(w0/w)^2
 phx=0.5*atan(x1)
 phig00=phi0+phx-x1*r2*w2    !phi_g ,(phi_g)^1=phi_g+phx
 phig10=phig00+phx
 tshape=exp(-phi1*phi1)
 wshape=sqrt(sqrt(w2))*exp(-w2*r2)
 A0=tshape*sin(phig00)
 fields(2)=wshape*A0  !Ey
 A1=tshape*2.*par_lp(6)*w2*exp(-w2*r2)
 fields(1)=y1*A1*cos(phig10)  !Ex
 fields(4)=0.0
 fields(6)=fields(2)          !Bz
 fields(3)=0.0
 fields(5)=0.0
 end subroutine get_2Dlaser_gprof_fields_lp
!=======================
 subroutine get_2Dlaser_fields_lp(coords,par_lp,fields)
 real(dp),intent(in) :: coords(4),par_lp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1, phig00, phig10, csphig01
 real(dp) :: x1, y1, t1,pih
 real(dp) :: w2,A0, A1
 real(dp) :: tshape, phx, r2, wshape
 !========== enter
 !par_lp(1)=oml
 !par_lp(2)=xc
 !par_lp(3)=wx
 !par_lp(4)=wy
 !par_lp(5)=zra
 !par_lp(6)=eps
 !par_lp(7)=sigma
 !===============================
 x1=coords(1)
 y1=coords(2)/par_lp(4)
 t1=coords(4)
 pih=0.5*pi
 !====================
 r2=y1*y1
 phi0=par_lp(1)*(t1-x1)
 phi1=pi*(t1-x1)/par_lp(3)
 if(abs(phi1)>pih)phi1=pih
 x1=x1/par_lp(5)
 w2=1./(1.+x1*x1)   !     w2=(w0/w)^2
 phx=0.5*atan(x1)
 phig00=phi0+phx-x1*r2*w2    !phi_g ,(phi_g)^1=phi_g+phx
 phig10=phig00+phx
 tshape=cos(phi1)*cos(phi1)
 wshape=sqrt(sqrt(w2))*exp(-w2*r2)
 A0=tshape*sin(phig00)
 fields(2)=wshape*A0  !Ey
 A1=tshape*2.*par_lp(6)*w2*exp(-w2*r2)
 fields(1)=y1*A1*cos(phig10)  !Ex
 fields(4)=0.0
 fields(6)=fields(2)          !Bz
!===================== O(sigma) correction
 fields(3)=0.0
 fields(5)=0.0
 end subroutine get_2Dlaser_fields_lp
!=============================
 subroutine get_laser_fields_lp(coords,par_lp,fields)
 real(dp),intent(in) :: coords(4),par_lp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1, phig00, phig10
 real(dp) :: x1, y1, z1, t1,pih
 real(dp) :: w2,A0, A1
 real(dp) :: tshape, phx, r2, wshape
 !========== enter
 !par_lp(1)=oml
 !par_lp(2)=xc
 !par_lp(3)=wx
 !par_lp(4)=wy
 !par_lp(5)=zra
 !par_lp(6)=eps
 !par_lp(7)=sigma
 !===============================
 x1=coords(1)          !x-xf
 y1=coords(2)/par_lp(4)
 z1=coords(3)/par_lp(4)
 t1=coords(4)           !t-t_f
 pih=0.5*pi
 !====================
 r2=y1*y1+z1*z1
 phi0=par_lp(1)*(t1-x1)
 phi1=pi*(t1-x1)/par_lp(3)
 if(abs(phi1)>pih)phi1=pih
 x1=x1/par_lp(5)
 w2=1./(1.+x1*x1)   !     w2=(w0/w)^2
 phx=atan(x1)
 phig00=phi0+phx-x1*r2*w2    !phi_g ,(phi_g)^1=phi_g+phx
 phig10=phig00+phx
 tshape=cos(phi1)*cos(phi1)
 wshape=sqrt(w2)*exp(-w2*r2)
 A0=tshape*sin(phig00)
 fields(2)=wshape*A0  !Ey
 A1=tshape*2.*par_lp(6)*w2*exp(-w2*r2)
 fields(1)=y1*A1*cos(phig10)  !Ex
 fields(4)=z1*A1*cos(phig10)  !Bx
 fields(6)=fields(2)          !Bz
 fields(3)=0.0
 fields(5)=0.0
 end subroutine get_laser_fields_lp
!=================
 subroutine get_laser_gprof_fields_lp(coords,par_lp,fields)
 real(dp),intent(in) :: coords(4),par_lp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1, phig00, phig10, csphig01
 real(dp) :: x1, y1, z1, t1,pih
 real(dp) :: A0, A1,w2
 real(dp) :: phx, r2, wshape,tshape
 !========== enter
 !par_lp(1)=oml
 !par_lp(2)=xc
 !par_lp(3)=wx
 !par_lp(4)=wy
 !par_lp(5)=zra
 !par_lp(6)=eps
 !par_lp(7)=sigma                  =1/(wx*oml)
 !===============================
 !        t_profile is gaussian exp(-(t-x)*(t-x)/wx2)
 x1=coords(1)          !x-xf
 y1=coords(2)/par_lp(4)
 z1=coords(3)/par_lp(4)
 t1=coords(4)           !t-t_f
 pih=0.5*pi
 !====================
 r2=y1*y1+z1*z1
 phi0=par_lp(1)*(t1-x1)    !fast oscillations
 phi1=(t1-x1)/par_lp(3)   !t_envelope
!-----------
 x1=x1/par_lp(5)
 w2=1./(1.+x1*x1)   !     w2=(w0/w)^2
 phx=atan(x1)
 phig00=phi0+phx-x1*r2*w2    !phi_g ,(phi_g)^1=phi_g+phx
 phig10=phig00+phx
 tshape=exp(-phi1*phi1)
 wshape=sqrt(w2)*exp(-w2*r2)
 A0=tshape*sin(phig00)
 fields(2)=wshape*A0  !Ey
!==============
 A1=2.*par_lp(6)*tshape*w2*exp(-w2*r2)
 fields(1)=y1*A1*cos(phig10)             !Ex
 fields(4)=z1*A1*cos(phig10)             !Bx
 fields(6)=fields(2)                     !Bz
 fields(3)=0.0
 fields(5)=0.0
 end subroutine get_laser_gprof_fields_lp
 !====================
 subroutine get_plane_wave_lp(coords,par_pp,fields)
 real(dp),intent(in) :: coords(4),par_pp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1,pih
 real(dp) :: x1,t1
 real(dp) :: A0, ev0
 !========== enter
 x1=coords(1)
 t1=coords(4)
 pih=0.5*pi
 !====================
 !oml=par_pp(1)   par_pp(3)=wx
 phi0=par_pp(1)*(t1-x1)
 phi1=pi*(t1-x1)/par_pp(3)
 if(abs(phi1)>pih)phi1=pih
 ev0=cos(phi1)*cos(phi1)
 A0=ev0*sin(phi0)
 fields(2)=A0  !Ey
 fields(6)=fields(2)          !Bz
 end subroutine get_plane_wave_lp
 !====================================
 subroutine get_plane_wave_cp(coords,par_pp,fields)
 real(dp),intent(in) :: coords(4),par_pp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1,pih
 real(dp) :: x1,t1
 real(dp) :: A0, A1,ev0
 !========== enter
 x1=coords(1)
 t1=coords(4)
 pih=0.5*pi
 !====================
 phi0=par_pp(1)*(t1-x1)
 phi1=par_pp(2)*(t1-x1)/par_pp(3)
 if(abs(phi1)>pih)phi1=pih
 ev0=cos(phi1)*cos(phi1)
 A0=ev0*sin(phi0)
 A1=ev0*sin(phi0-pih)
 fields(2)=A0  !Ey
 fields(3)=-A1  !Ez
 fields(5)=-fields(3)          !By
 fields(6)=fields(2)           !Bz
 end subroutine get_plane_wave_cp
 !======================
 subroutine get_laser_fields_cp(coords,par_cp,fields)
 real(dp),intent(in) :: coords(4),par_cp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1, phig00, phig10, csphig01, snphig01
 real(dp) :: x1, y1, z1, t1
 real(dp) :: w2, ar, rho, ss0 ,cs0
 real(dp) :: A0, A1,pih
 real(dp) :: ev0, ev1,phx, psi, r2, wshape
 !========== enter
 !par_cp(1)=om0=k0
 !par_cp(2)=xc
 !par_cp(3)=wx
 !par_cp(4)=wy
 !par_cp(5)=zra
 !par_cp(6)=eps
 !par_cp(7)=sigma
 pih=0.5*pi
 x1=coords(1)
 y1=coords(2)
 z1=coords(3)
 t1=coords(4)
 y1=y1/par_cp(4)
 z1=z1/par_cp(4)
 r2=y1*y1+z1*z1
 phi0=par_cp(1)*(t1-x1)
 phi1=pi*(t1-x1)/par_cp(3)
 if(abs(phi1)>pih)phi1=pih
 x1=x1/par_cp(5)
 w2=1./(1.+x1*x1)   !     w2=(w0/w)^2
 phx=atan(x1)
 phig00=phi0+phx-x1*r2*w2    !phi_g ,(phi_g)^1=phi_g+phx
 phig10=phig00+phx
 psi=phig00+2.*phx
 ar=1.-r2
 rho=sqrt(ar*ar+x1*x1)  !the module of (1-r^2)+x^2
 ss0=0.0
 cs0=1.0
 if(rho>0.0)then
  ss0=x1/rho
  cs0=ar/rho
 endif
 csphig01=cos(psi)*cs0+sin(psi)*ss0
 snphig01=cos(psi-pih)*cs0+sin(psi-pih)*ss0
 ev0=cos(phi1)*cos(phi1)
 ev1=cos(phi1)*sin(phi1)
 wshape=sqrt(w2)*exp(-w2*r2)
 A0=ev0*sin(phig00)
 A1=ev1*par_cp(7)*x1*w2*rho
 A1=A1*csphig01
 fields(2)=wshape*(A0+A1)  !Ey(x,yh)
 A0=ev0*sin(phig00-pih)
 A1=ev1*par_cp(7)*x1*w2*rho
 A1=A1*snphig01
 fields(3)=-wshape*(A0-A1)
 A1=ev0*2.*par_cp(6)*w2*exp(-w2*r2)
 fields(1)=y1*A1*cos(phig10)-z1*A1*cos(phig10-pih)
 fields(4)=z1*A1*cos(phig10)+y1*A1*cos(phig10-pih)
 !Bz=Ey
 !By=-Ez
 fields(5)=-fields(3)
 fields(6)=fields(2)
 end subroutine get_laser_fields_cp
 !==============================
 subroutine inflow_lp_fields(ef,e0,t_loc,tf,wx,wy,xf0,om0,&
  lp,i,j1,j2,k1,k2)
 !==========================
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,t_loc,tf,wx,wy,xf0,om0
 integer,intent(in) :: lp,i,j1,j2,k1,k2
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,sigma,eps
 real(dp) :: xp,yp
 real(dp) :: xc,zra
 real(dp) :: Ex,Ey,Ez,Bx,By,Bz
 integer :: j,k,jj,kk
 real(dp) :: coords(4), fields(6),par_lp(7)
 ! inviluppo temporale= cos^2(pi*(t-x))/wx)
 ! eps=1./k0*wy k0=omega_0=omgl
 sigma=2.*pi/(om0*wx) !sigma=lambda/wx
 eps=1./(om0*wy)
 zra=0.5*om0*wy*wy
 xx=loc_xg(1,1,0)
 xxh=loc_xg(1,2,0)
 xc=xf0-tf
 coords(4)=t_loc-tf
 par_lp(1)=om0
 par_lp(2)=xc
 par_lp(3)=wx
 par_lp(4)=wy
 par_lp(5)=zra
 par_lp(6)=eps
 par_lp(7)=sigma
 !Linear polarization (P-mode)
 !Ex   half-integer on x
 !Bx   half-integer on y and z
 !Ey   half-integer on y
 !Bz   half-integer on y and x
 select case(lp)
 case(0)           !Plane 2D wave
  if(ndim<3)then   !Holds also the 1D case
   k=1
   do j=j1,j2
    !===ora Ex(xxh,yy)=========
    ef(i,j,k,1)=0.0
    !==== Ey(xx,yyh)!
    xp=xx
    coords(1)=xp-xf0
    call get_plane_wave_lp(coords,par_lp,fields)
    Ey=e0*fields(2)
    ef(i,j,k,2)=Ey
    !===ora Bz(xxh,yyh)=========
    xp=xxh
    coords(1)=xp-xf0
    call get_plane_wave_lp(coords,par_lp,fields)
    Bz=e0*fields(6)
    ef(i,j,k,3)=Bz
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   do j=j1,j2
    !==== Ex(xxh,yy,zz)=========
    !==== Ez(xx,yy,zzh) =========
    ef(i,j,k,1)=0.0
    ef(i,j,k,3)=0.0
    !==== Ey(xx,yyh,zz) =========
    xp=xx
    coords(1)=xp-xf0
    call get_plane_wave_lp(coords,par_lp,fields)
    Ey=e0*fields(2)
    ef(i,j,k,2)=Ey
    ef(i,j,k,4)=0.0
    ef(i,j,k,5)=0.0
    !==== Bz(xxh,yyh,zz)=========
    xp=xxh
    coords(1)=xp-xf0
    call get_plane_wave_lp(coords,par_lp,fields)
    Bz=e0*fields(6)
    ef(i,j,k,5)=0.0
    ef(i,j,k,6)=Bz
   end do
  end do
  !+++++++++++++ Gaussian field
 case(1)
  if(ndim<2)then
   k=1; j=1
   coords(2:3)=0.0
   !===ora Ex(xxh,yy)=========
   xp=xxh
   coords(1)=xp-xf0
   !==== Ex(xxh,yy)!
   if(G_prof)then
    call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
   else
    call get_2Dlaser_fields_lp(coords,par_lp,fields)
   endif
   Ex=e0*fields(1)
   ef(i,j,k,1)=Ex
   Bz=e0*fields(6)
   ef(i,j,k,3)=Bz
   !==== Ey(xx,yyh)!
   xp=xx
   coords(1)=xp-xf0
   if(G_prof)then
    call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
   else
    call get_2Dlaser_fields_lp(coords,par_lp,fields)
   endif
   Ey=e0*fields(2)
   ef(i,j,k,2)=Ey
   return
  endif
  if(ndim<3)then
   k=1
   coords(3)=0.0
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    !===ora Ex(xxh,yy)=========
    xp=xxh
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    !==== Ex(xxh,yy)!
    if(G_prof)then
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    else
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
    endif
    Ex=e0*fields(1)
    ef(i,j,k,1)=Ex
    !==== Ey(xx,yyh)!
    xp=xx
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    if(G_prof)then
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    else
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
    endif
    Ey=e0*fields(2)
    ef(i,j,k,2)=Ey
    !===ora Bz(xxh,yyh)=========
    xp=xxh
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    if(G_prof)then
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    else
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
    endif
    Bz=e0*fields(6)
    ef(i,j,k,3)=Bz
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   kk=k-2
   zz=loc_zg(kk,1,imodz)
   zzh=loc_zg(kk,2,imodz)
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    xp=xxh
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zz
    call get_laser_fields_lp(coords,par_lp,fields)
    Ex=e0*fields(1)
    ef(i,j,k,1)=Ex
    !==== Ey(xx,yyh,zz) =========
    xp=xx
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    call get_laser_fields_lp(coords,par_lp,fields)
    Ey=e0*fields(2)
    ef(i,j,k,2)=Ey
    !==== Ez(xx,yy,zzh) =========
    xp=xx
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_lp(coords,par_lp,fields)
    Ez=e0*fields(3)
    ef(i,j,k,3)=Ez
    !==== Bx(xx,yyh,zzh)=========
    xp=xx
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_lp(coords,par_lp,fields)
    Bx=e0*fields(4)
    ef(i,j,k,4)=Bx
    !==== By(xxh,yy,zzh) =========
    xp=xxh
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_lp(coords,par_lp,fields)
    By=e0*fields(5)
    ef(i,j,k,5)=By
    !==== Bz(xxh,yyh,zz)=========
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zz
    call get_laser_fields_lp(coords,par_lp,fields)
    Bz=e0*fields(6)
    ef(i,j,k,6)=Bz
   end do
  end do
  !====== S POLARIZATION ==============================================
 case(2)
  if(ndim<3)then
   k=1
   coords(3)=0
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    !===ora Ez(xx,yy)=========
    xp=xx
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    call get_laser_fields_lp(coords,par_lp,fields)
    Ez=e0*fields(2)    !  Ez(s-pol)=Ey(p-pol)
    ef(i,j,k,3)=Ez
    !==== Bx(xx,yyh)=========
    yp=yyh
    coords(2)=yp
    call get_laser_fields_lp(coords,par_lp,fields)
    Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
    ef(i,j,k,4)=Bx
    By=-e0*fields(2)  !  By(s-pol)=-Bz(p-pol)
    ef(i,j,k,5)=By
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   kk=k-2
   zz=loc_zg(kk,1,imodz)
   zzh=loc_zg(kk,2,imodz)
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    !==== Ex(xxh,yy,zz)=========
    xp=xxh
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zz
    call get_laser_fields_lp(coords,par_lp,fields)
    Ex=e0*fields(4)    !  Ex(s-pol)= Bx(p-pol)
    Ey=-e0*fields(3)   !  Ey(s-pol)=-Ez(p-pol)
    ef(i,j,k,1)=Ex
    !==== Ey(xx,yyh,zz) =========
    yp=yyh
    coords(2)=yp
    call get_laser_fields_lp(coords,par_lp,fields)
    Ey=-e0*fields(3)    !  Ey(s-pol)=-Ez(p-pol)
    ef(i,j,k,2)=Ey
    !==== Ez(xx,yy,zzh) =========
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_lp(coords,par_lp,fields)
    Ez=e0*fields(2)     !  Ez(s-pol)= Ey(p-pol)
    ef(i,j,k,3)=Ez
    !==== Bx(xx,yyh,zzh)=========
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_lp(coords,par_lp,fields)
    Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
    ef(i,j,k,4)=Bx
    !==== By(xxh,yy,zzh) =========
    xp=xxh
    yp=yy
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_lp(coords,par_lp,fields)
    By=-e0*fields(6)     !  By(s-pol)=-Bz(p-pol)
    ef(i,j,k,5)=By
    !==== Bz(xxh,yyh,zz)=========
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zz
    call get_laser_fields_lp(coords,par_lp,fields)
    Bz=e0*fields(5)     !  Bz(s-pol)= By(p-pol)
    ef(i,j,k,6)=Bz
   end do
  end do
 end select
 end subroutine inflow_lp_fields
 !===================================
 subroutine init_lp_inc0_fields(ef,e0,t_loc,tf,wx,wy,xf0,om0,&
                              lp,i1,i2,ycent,zcent)
 !==========================
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,t_loc,tf,wx,wy,xf0,om0
 integer,intent(in) :: lp,i1,i2
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,sigma,eps
 real(dp) :: xp,xc,yp,yc,zc,zra,ycent,zcent
 real(dp) :: Ex,Ey,Ez,Bx,By,Bz
 integer :: i,j,k,ii,jj,kk
 integer :: j1,j2,k1,k2
 real(dp) :: coords(4), fields(6),par_lp(7)

 ! inviluppo temporale= cos^2(pi*(t-x))/wx)
 ! inviluppo temporale -gprof = exp-(t-x)^2/w2x)
 ! eps=1./k0*wy k0=omega_0=omgl
 ! NORMAL INCIDENCE

 sigma=1./(om0*wx) 
 eps=1./(om0*wy)
 zra=0.5*om0*wy*wy
 xc=xf0-tf
 yc=ycent ! yc centroid y coordinate
 zc=zcent ! zc centroid z coordinate
 par_lp(1)=om0
 par_lp(2)=xc
 par_lp(3)=wx
 par_lp(4)=wy
 par_lp(5)=zra
 par_lp(6)=eps
 par_lp(7)=sigma
 coords(4)=t_loc-tf

 !Linear polarization (P-mode)
 !Ex half-integer on x
 !Bx half-integer on y and z
 !Ey half-integer on y
 !Bz half-integer on y and x
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)

 select case(lp)
 case(0)           !Plane 2D wave
  if(ndim<3)then   !Holds also the 1D case
   k=1
   do j=j1,j2
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !===ora Ex(xxh,yy)=========
     ef(i,j,k,1)=0.0
     !==== Ey(xx,yyh)!
     xp=xx
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Ey=e0*fields(2)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey
     !===ora Bz(xxh,yyh)=========
     xp=xxh
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,3)=ef(i,j,k,3)+Bz
    end do
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !==== Ex(xxh,yy,zz)=========
     !==== Ez(xx,yy,zzh) =========
     ef(i,j,k,1)=0.0
     ef(i,j,k,3)=0.0
     !==== Ey(xx,yyh,zz) =========
     coords(1)=xx-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Ey=e0*fields(2)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey
     ef(i,j,k,4)=0.0
     ef(i,j,k,5)=0.0
     !==== Bz(xxh,yyh,zz)=========
     coords(1)=xxh-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,6)=ef(i,j,k,6)+Bz
    end do
   end do
  end do
  !+++++++++++++ Gaussian radial shape  longitudinal Gaussian or cos^2 profile
 case(1)
  if(ndim<2)then
   k=1; j=1
   coords(2:3)=0.0
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xxh=loc_xg(ii,2,imodx)
    !===ora Ex(xxh,yy)=========
    coords(1)=xxh-xf0
    !==== Ex(xxh,yy)!
    if(G_prof)then
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    else
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
    endif
    Ex=e0*fields(1)
    ef(i,j,k,1)=ef(i,j,k,1)+Ex
    !==== Ey(xx,yyh)!
    coords(1)=xx-xf0
    if(G_prof)then
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    else
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
    endif
    Ey=e0*fields(2)
    ef(i,j,k,2)=ef(i,j,k,2)+Ey
    !===ora Bz(xxh,yyh)=========
    coords(1)=xxh-xf0
    if(G_prof)then
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    else
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
    endif
    Bz=e0*fields(6)
    ef(i,j,k,3)=ef(i,j,k,3)+Bz
   end do
   return
  endif
  if(ndim<3)then
   k=1
   coords(3)=0.0
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    do i=i1,i2
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !===ora Ex(xxh,yy)=========
     coords(1)=xxh-xf0
     coords(2)=yy-yc
     !==== Ex(xxh,yy)!
     if(G_prof)then
      call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_2Dlaser_fields_lp(coords,par_lp,fields)
     endif
     Ex=e0*fields(1)
     ef(i,j,k,1)=ef(i,j,k,1)+Ex
     !==== Ey(xx,yyh)!
     coords(1)=xx-xf0
     coords(2)=yyh-yc
     if(G_prof)then
      call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_2Dlaser_fields_lp(coords,par_lp,fields)
     endif
     Ey=e0*fields(2)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey
     !===ora Bz(xxh,yyh)=========
     coords(1)=xxh-xf0
     coords(2)=yyh-yc
     if(G_prof)then
      call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_2Dlaser_fields_lp(coords,par_lp,fields)
     endif
     Bz=e0*fields(6)
     ef(i,j,k,3)=ef(i,j,k,3)+Bz
    end do
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   kk=k-2
   zz=loc_zg(kk,1,imodz)
   zzh=loc_zg(kk,2,imodz)
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !==== Ex(xxh,yy,zz)=========
     coords(1)=xxh-xf0
     coords(2)=yy-yc
     coords(3)=zz-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ex=e0*fields(1)
     ef(i,j,k,1)=ef(i,j,k,1)+Ex
     !==== Ey(xx,yyh,zz) =========
     coords(1)=xx-xf0
     coords(2)=yyh-yc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ey=e0*fields(2)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey
     !==== Ez(xx,yy,zzh) =========
     coords(1)=xx-xf0
     coords(2)=yy-yc
     coords(3)=zzh-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ez=e0*fields(3)
     ef(i,j,k,3)=ef(i,j,k,3)+Ez
     !==== Bx(xx,yyh,zzh)=========
     coords(1)=xx-xf0
     coords(2)=yyh-yc
     coords(3)=zzh-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Bx=e0*fields(4)
     ef(i,j,k,4)=ef(i,j,k,4)+Bx
     !==== By(xxh,yy,zzh) =========
     coords(1)=xxh-xf0
     coords(2)=yy-yc
     coords(3)=zzh-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     By=e0*fields(5)
     ef(i,j,k,5)=ef(i,j,k,5)+By
     !==== Bz(xxh,yyh,zz)=========
     coords(1)=xxh-xf0
     coords(2)=yyh-yc
     coords(3)=zz-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Bz=e0*fields(6)
     ef(i,j,k,6)=ef(i,j,k,6)+Bz
    end do
   end do
  end do
  !====== S POLARIZATION ==============================================
 case(2)
  if(ndim<3)then
   k=1
   coords(3)=0
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !===ora Ez(xx,yy)=========
     coords(1)=xx-xf0
     coords(2)=yy-yc
     if(G_prof)then
      call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_2Dlaser_fields_lp(coords,par_lp,fields)
     endif
     Ez=e0*fields(2)    !  Ez(s-pol)=Ey(p-pol)
     ef(i,j,k,3)=ef(i,j,k,3)+Ez
     !==== Bx(xx,yyh)=========
     coords(1)=xx-xf0
     coords(2)=yyh-yc
     if(G_prof)then
      call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_2Dlaser_fields_lp(coords,par_lp,fields)
     endif
     Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
     ef(i,j,k,4)=ef(i,j,k,4)+Bx
     !==== By(xx,yyh) =======
     coords(1)=xx-xf0
     coords(2)=yyh-yc
     if(G_prof)then
      call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_2Dlaser_fields_lp(coords,par_lp,fields)
     endif
     By=-e0*fields(2)  !  By(s-pol)=-Bz(p-pol)
     ef(i,j,k,5)=ef(i,j,k,5)+By
    end do
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   kk=k-2
   zz=loc_zg(kk,1,imodz)
   zzh=loc_zg(kk,2,imodz)
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !==== Ex(xxh,yy,zz)=========
     coords(1)=xxh-xf0
     coords(2)=yy-yc
     coords(3)=zz-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ex=e0*fields(4)    !  Ex(s-pol)= Bx(p-pol)
     ef(i,j,k,1)=ef(i,j,k,1)+Ex
     !==== Ey(xx,yyh,zz) =========
     coords(1)=xx-xf0
     coords(2)=yyh-yc
     coords(3)=zz-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ey=-e0*fields(3)    !  Ey(s-pol)=-Ez(p-pol)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey
     !==== Ez(xx,yy,zzh) =========
     coords(1)=xx-xf0
     coords(2)=yy-yc
     coords(3)=zzh-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ez=e0*fields(2)     !  Ez(s-pol)= Ey(p-pol)
     ef(i,j,k,3)=ef(i,j,k,3)+Ez
     !==== Bx(xx,yyh,zzh)=========
     coords(1)=xx-xf0
     coords(2)=yyh-yc
     coords(3)=zzh-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
     ef(i,j,k,4)=ef(i,j,k,4)+Bx
     !==== By(xxh,yy,zzh) =========
     coords(1)=xxh-xf0
     coords(2)=yy-yc
     coords(3)=zzh-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     By=e0*fields(6)     !  By(s-pol)=-Bz(p-pol)
     ef(i,j,k,5)=ef(i,j,k,5)+By
     !==== Bz(xxh,yyh,zz)=========
     coords(1)=xxh-xf0
     coords(2)=yyh-yc
     coords(3)=zz-zc
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Bz=e0*fields(5)     !  Bz(s-pol)= By(p-pol)
     ef(i,j,k,6)=Bz
    end do
   end do
  end do
 end select
 end subroutine init_lp_inc0_fields
!=====================================
 subroutine init_lp_fields(ef,e0,t_loc,tf,wx,wy,xf0,om0,&
                              angle,lp_shx,lp,i1,i2,ycent,zcent)
 !==========================
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,t_loc,tf,wx,wy,xf0,angle,lp_shx,om0
 integer,intent(in) :: lp,i1,i2
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,sigma,eps
 real(dp) :: xp,xc,yp,yc,zc,ycent,zcent
 real(dp) :: zra,sf,cf
 real(dp) :: Ex,Ey,Ez,Bx,By,Bz
 integer :: i,j,k,ii,jj,kk
 integer :: j1,j2,k1,k2
 real(dp) :: coords(4), fields(6),par_lp(7)

 ! inviluppo temporale= cos^2(pi*(t-x))/wx)
 ! inviluppo temporale -gprof = exp-(t-x)^2/w2x)
 ! eps=1./k0*wy k0=omega_0=omgl
 sf=0.0
 cf=1.
 if(angle >0.0)then
  sf=sin(pi*angle/180.)
  cf=cos(pi*angle/180.)
 endif
 sigma=1./(om0*wx) 
 eps=1./(om0*wy)
 zra=0.5*om0*wy*wy
 xc=xf0-tf+lp_shx
 yc=ycent
 zc=zcent ! yc centroid y coordinate
 par_lp(1)=om0
 par_lp(2)=xc
 par_lp(3)=wx
 par_lp(4)=wy
 par_lp(5)=zra
 par_lp(6)=eps
 par_lp(7)=sigma
 coords(4)=t_loc-tf

 ! for normal incidence
 ! rotates the laser pulse around the (xc,yc) point
 ! the (xp,yp) coordinates of the rotated pulse
 ! xp=xc+(x-xc)*cos+y*sin yp=y*cos-(x-xc)*sin
 !================================
 !Linear polarization (P-mode)
 !Ex half-integer on x
 !Bx half-integer on y and z
 !Ey half-integer on y
 !Bz half-integer on y and x
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)

 select case(lp)
 case(0)           !Plane 2D wave
  if(ndim<3)then   !Holds also the 1D case
   k=1
   do j=j1,j2
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !===ora Ex(xxh,yy)=========
     ef(i,j,k,1)=0.0
     !==== Ey(xx,yyh)!
     xp=xx
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Ey=e0*fields(2)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey
     !===ora Bz(xxh,yyh)=========
     xp=xxh
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,3)=ef(i,j,k,3)+Bz
    end do
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !==== Ex(xxh,yy,zz)=========
     !==== Ez(xx,yy,zzh) =========
     ef(i,j,k,1)=0.0
     ef(i,j,k,3)=0.0
     !==== Ey(xx,yyh,zz) =========
     xp=xx
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Ey=e0*fields(2)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey*cf
     ef(i,j,k,3)=ef(i,j,k,3)+Ey*sf
     ef(i,j,k,4)=0.0
     ef(i,j,k,5)=0.0
     !==== Bz(xxh,yyh,zz)=========
     xp=xxh
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,5)=ef(i,j,k,5)-Bz*sf
     ef(i,j,k,6)=ef(i,j,k,6)+Bz*cf
    end do
   end do
  end do
  !+++++++++++++ Gaussian field
 case(1)
  if(ndim<2)then
   k=1; j=1
   coords(2:3)=0.0
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xxh=loc_xg(ii,2,imodx)
    !===ora Ex(xxh,yy)=========
    xp=xxh
    coords(1)=xp-xf0
    !==== Ex(xxh,yy)!
                        !call get_laser_fields_lp(coords,par_lp,fields)
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    Ex=e0*fields(1)
    ef(i,j,k,1)=ef(i,j,k,1)+Ex
    !==== Ey(xx,yyh)!
    xp=xx
    coords(1)=xp-xf0
    call get_laser_fields_lp(coords,par_lp,fields)
    Ey=e0*fields(2)
    ef(i,j,k,2)=ef(i,j,k,2)+Ey
    !===ora Bz(xxh,yyh)=========
    xp=xc+(xxh-xc)
    coords(1)=xp-xf0
                       !call get_laser_fields_lp(coords,par_lp,fields)
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
    Bz=e0*fields(6)
    ef(i,j,k,3)=ef(i,j,k,3)+Bz
   end do
   return
  endif
  if(ndim<3)then
   k=1
   coords(3)=0.0
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    yy=yy-yc
    yyh=yyh-yc
    do i=i1,i2
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !===ora Ex(xxh,yy)=========
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     !==== Ex(xxh,yy)!
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
                                       !call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,1)=ef(i,j,k,1)+Ex*cf-Ey*sf
     !==== Ey(xx,yyh)!
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
                                       !call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,2)=ef(i,j,k,2)+Ey*cf+Ex*sf
     !===ora Bz(xxh,yyh)=========
     xp=xc+(xxh-xc)*cf+yyh*sf
     yp=yyh*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_2Dlaser_fields_lp(coords,par_lp,fields)
                           !call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,3)=ef(i,j,k,3)+Bz
    end do
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   kk=k-2
   zz=loc_zg(kk,1,imodz)
   zzh=loc_zg(kk,2,imodz)
   zz=zz-zc
   zzh=zzh-zc
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    yy=yy-yc
    yyh=yyh-yc
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !==== Ex(xxh,yy,zz)=========
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zz
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,1)=Ex*cf-Ey*sf
     !==== Ey(xx,yyh,zz) =========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,2)=Ey*cf+Ex*sf
     !==== Ez(xx,yy,zzh) =========
     xp=xc+(xx-xc)*cf+yy*sf
     yp=yy*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Ez=e0*fields(3)
     ef(i,j,k,3)=Ez
     !==== Bx(xx,yyh,zzh)=========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Bx=e0*fields(4)
     By=e0*fields(5)
     ef(i,j,k,4)=Bx*cf-By*sf
     !==== By(xxh,yy,zzh) =========
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Bx=e0*fields(4)
     By=e0*fields(5)
     ef(i,j,k,5)=By*cf+Bx*sf
     !==== Bz(xxh,yyh,zz)=========
     xp=xc+(xxh-xc)*cf+yyh*sf
     yp=yyh*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zz
     if(G_prof)then
      call get_laser_gprof_fields_lp(coords,par_lp,fields)
     else
      call get_laser_fields_lp(coords,par_lp,fields)
     endif
     Bz=e0*fields(6)
     ef(i,j,k,6)=Bz
    end do
   end do
  end do
  !====== S POLARIZATION ==============================================
 case(2)
  if(ndim<3)then
   k=1
   coords(3)=0
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    yy=yy-yc
    yyh=yyh-yc
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !===ora Ez(xx,yy)=========
     xp=xc+(xx-xc)*cf+yy*sf
     yp=yy*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
                              !call get_2Dlaser_fields_lp(coords,par_lp,fields)
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     Ez=e0*fields(2)    !  Ez(s-pol)=Ey(p-pol)
     ef(i,j,k,3)=Ez
     !==== Bx(xx,yyh)=========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
                           !call get_2Dlaser_fields_lp(coords,par_lp,fields)
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
     By=-e0*fields(6)    !  By(s-pol)=-Bz(p-pol)
     ef(i,j,k,4)=Bx*cf-By*sf
     !==== By(xx,yyh) =======
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     !call get_2Dlaser_fields_lp(coords,par_lp,fields)
     call get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
     Bx=e0*fields(1)   !  Bx(s-pol)= Ex(p-pol
     By=-e0*fields(2)  !  By(s-pol)=-Bz(p-pol)
     ef(i,j,k,5)=By*cf+Bx*sf
    end do
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   kk=k-2
   zz=loc_zg(kk,1,imodz)
   zzh=loc_zg(kk,2,imodz)
   zz=zz-zc
   zzh=zzh-zc
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
    yy=yy-yc
    yyh=yyh-yc
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !==== Ex(xxh,yy,zz)=========
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zz
     call get_laser_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(4)    !  Ex(s-pol)= Bx(p-pol)
     Ey=-e0*fields(3)   !  Ey(s-pol)=-Ez(p-pol)
     ef(i,j,k,1)=Ex*cf-Ey*sf
     !==== Ey(xx,yyh,zz) =========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zz
     call get_laser_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(4)     !  Ex(s-pol)= Bx(p-pol)
     Ey=-e0*fields(3)    !  Ey(s-pol)=-Ez(p-pol)
     ef(i,j,k,2)=Ey*cf+Ex*sf
     !==== Ez(xx,yy,zzh) =========
     xp=xc+(xx-xc)*cf+yy*sf
     yp=yy*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     call get_laser_fields_lp(coords,par_lp,fields)
     Ez=e0*fields(2)     !  Ez(s-pol)= Ey(p-pol)
     ef(i,j,k,3)=Ez
     !==== Bx(xx,yyh,zzh)=========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     call get_laser_fields_lp(coords,par_lp,fields)
     Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
     By=-e0*fields(6)     !  By(s-pol)=-Bz(p-pol)
     ef(i,j,k,4)=Bx*cf-By*sf
     !==== By(xxh,yy,zzh) =========
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     call get_laser_fields_lp(coords,par_lp,fields)
     Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
     By=e0*fields(6)     !  By(s-pol)=-Bz(p-pol)
     ef(i,j,k,5)=By*cf+Bx*sf
     !==== Bz(xxh,yyh,zz)=========
     xp=xc+(xxh-xc)*cf+yyh*sf
     yp=yyh*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zz
     call get_laser_fields_lp(coords,par_lp,fields)
     Bz=e0*fields(5)     !  Bz(s-pol)= By(p-pol)
     ef(i,j,k,6)=Bz
    end do
   end do
  end do
 end select
 end subroutine init_lp_fields
 !===============================
 subroutine inflow_cp_fields(ef,e0,t_loc,tf,wx,wy,xf0,&
  cp,i,j1,j2,k1,k2)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer, intent(in) :: cp,i,j1,j2,k1,k2
 real(dp),intent(in) :: e0,t_loc,tf,wx,wy,xf0
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,xp,yp
 real(dp) :: xc,eps,sigma,zra
 real(dp) :: Ey,Bz,Ex,Bx,Ez,By
 real(dp) :: coords(4),fields(6),par_lp(7)
 integer :: j,k,jj,kk
 !============
 ! inviluppo temporale= cos^2(pi*(t-x))/wx)
 ! eps=1./k0*wy k0=omega_0=omgl
 sigma=pi/(oml*wx)     !sigma=lambda/wx
 eps=1./(oml*wy)
 zra=0.5*oml*wy*wy
 xc=xf0-tf
 coords(4)=t_loc-tf
 par_lp(1)=oml
 par_lp(2)=xc
 par_lp(3)=wx
 par_lp(4)=wy
 par_lp(5)=zra
 par_lp(6)=eps
 par_lp(7)=sigma
 xx=loc_xg(1,1,0)
 xxh=loc_xg(1,2,0)
 !==============================
 !Circular polarization
 !Ey half-integer on y
 !Bz half-integer on y and x
 !Ez half-integer on z
 !By half-integer on z and x
 !Ex half-integer on x
 !Bx half-integer on y and z
 !=============================
 if(cp==0)then          !CP plane wave
  if(ndim<3)then   !Holds also the 1D case
   k=1
   do j=j1,j2
    !=== Ex(xxh,yy)=========
    ef(i,j,k,1)=0.0
    !==== Ey(xx,yyh), Ez(xx,yy), By(xxh,yy), Bz(xxh,yyh)!
    xp=xx
    coords(1)=xp-xf0
    call get_plane_wave_cp(coords,par_lp,fields)
    ef(i,j,k,2)=e0*fields(2)
    ef(i,j,k,3)=e0*fields(3)
    !===ora Bz(xxh,yyh)=========
    xp=xxh
    coords(1)=xp-xf0
    call get_plane_wave_cp(coords,par_lp,fields)
    ef(i,j,k,5)=e0*fields(5)
    ef(i,j,k,6)=e0*fields(6)
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   do j=j1,j2
    !==== Ex(xxh,yy,zz)=========
    !==== Ez(xx,yy,zzh) =========
    ef(i,j,k,1)=0.0
    ef(i,j,k,4)=0.0
    xp=xx
    coords(1)=xp-xf0
    call get_plane_wave_cp(coords,par_lp,fields)
    Ey=e0*fields(2)
    ef(i,j,k,2)=Ey
    Ez=e0*fields(3)
    ef(i,j,k,3)=Ez

    xp=xxh
    coords(1)=xp-xf0
    call get_plane_wave_cp(coords,par_lp,fields)
    ef(i,j,k,5)=e0*fields(5)
    ef(i,j,k,6)=e0*fields(6)
   end do
  end do
  return
 endif
 !============================= CP with gaussian envelope
 kk=1
 do k=k1,k2
  zz=loc_zg(kk,1,imodz)
  zzh=loc_zg(kk,2,imodz)
  if(ndim <3)then
   zz=0.0
   zzh=0.0
  endif
  jj=1
  do j=j1,j2
   yy=loc_yg(jj,1,imody)
   yyh=loc_yg(jj,2,imody)
   !==================
   xp=xxh
   yp=yy
   coords(1)=xp-xf0
   coords(2)=yp
   coords(3)=zz
   call get_laser_fields_cp(coords,par_lp, fields) !(xh,y,z)
   Ex=e0*fields(1)
   ef(i,j,k,1)=Ex

   xp=xx
   yp=yyh
   coords(1)=xp-xf0
   coords(2)=yp
   call get_laser_fields_cp(coords,par_lp, fields) !(x,yh,z)
   Ey=e0*fields(2)
   ef(i,j,k,2)=Ey
   yp=yy
   coords(2)=yp
   coords(3)=zzh
   call get_laser_fields_cp(coords,par_lp,fields) !(x,y,zh)
   Ez=e0*fields(3)
   ef(i,j,k,3)=Ez
   yp=yyh
   coords(2)=yp
   coords(3)=zzh
   call get_laser_fields_cp(coords,par_lp,fields) !(x,yh,zh)
   Bx=e0*fields(4)
   ef(i,j,k,4)=Bx

   xp=xxh
   yp=yy
   coords(1)=xp-xf0
   coords(2)=yp
   call get_laser_fields_cp(coords,par_lp,fields) !(xh,y,zh)
   By=e0*fields(5)
   ef(i,j,k,5)=By
   yp=yyh
   coords(2)=yp
   coords(3)=zz
   call get_laser_fields_cp(coords,par_lp,fields) !(xh,yh,z)
   Bz=e0*fields(6)
   ef(i,j,k,6)=Bz
   jj=jj+1
  end do
  kk=kk+1
 end do
 end subroutine inflow_cp_fields
 !=================================
 subroutine init_cp_fields(ef,e0,t_loc,tf,wx,wy,xf0,&
  angle,lp_shx,cp,i1,i2)

 real(dp),intent(inout) :: ef(:,:,:,:)
 integer, intent(in) :: cp,i1,i2
 real(dp),intent(in) :: e0,t_loc,tf,wx,wy,xf0,angle,lp_shx
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,xp,yp
 real(dp) :: eps,sigma,sf,cf,zra,xc
 real(dp) :: Ey,Bz,Ex,Bx,Ez,By
 real(dp) :: coords(4),fields(6),par_lp(7)
 integer :: j1,j2,k1,k2,i,j,k,ii,jj,kk
 !============
 ! inviluppo temporale= cos^2(pi*(t-x))/wx)
 ! eps=1./k0*wy k0=omega_0=omgl
 sf=sin(pi*angle/180.)
 cf=cos(pi*angle/180.)
 sigma=pi/(oml*wx) !sigma=lambda/wx
 eps=1./(oml*wy)
 zra=0.5*oml*wy*wy
 xc=xf0-tf+lp_shx
 coords(4)=t_loc-tf
 par_lp(1)=oml
 par_lp(2)=xc
 par_lp(3)=wx
 par_lp(4)=wy
 par_lp(5)=zra
 par_lp(6)=eps
 par_lp(7)=sigma
 !==============================
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
 !Circular polarization
 !Ey half-integer on y
 !Bz half-integer on y and x
 !Ez half-integer on z
 !By half-integer on z and x
 !Ex half-integer on x
 !Bx half-integer on y and z
 !=============================
 if(cp==0)then          !CP plane wave
  if(ndim<3)then   !Holds also the 1D case
   k=1
   do j=j1,j2
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !=== Ex(xxh,yy)=========
     ef(i,j,k,1)=0.0
     !==== Ey(xx,yyh), Ez(xx,yy), By(xxh,yy), Bz(xxh,yyh)!
     xp=xx
     coords(1)=xp-xf0
     call get_plane_wave_cp(coords,par_lp,fields)
     ef(i,j,k,2)=e0*fields(2)
     ef(i,j,k,3)=e0*fields(3)
     !===ora Bz(xxh,yyh)=========
     xp=xxh
     coords(1)=xp-xf0
     call get_plane_wave_cp(coords,par_lp,fields)
     ef(i,j,k,5)=e0*fields(5)
     ef(i,j,k,6)=e0*fields(6)
    end do
   end do
   return
  endif
  !====3D ========================
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !==== Ex(xxh,yy,zz)=========
     !==== Ez(xx,yy,zzh) =========
     ef(i,j,k,1)=0.0
     ef(i,j,k,4)=0.0
     xp=xx
     coords(1)=xp-xf0
     call get_plane_wave_cp(coords,par_lp,fields)
     Ey=e0*fields(2)
     ef(i,j,k,2)=Ey
     Ez=e0*fields(3)
     ef(i,j,k,3)=Ez

     xp=xxh
     coords(1)=xp-xf0
     call get_plane_wave_cp(coords,par_lp,fields)
     ef(i,j,k,5)=e0*fields(5)
     ef(i,j,k,6)=e0*fields(6)
    end do
   end do
  end do
  return
 endif
 !============================= CP with gaussian envelope
 kk=1
 do k=k1,k2
  zz=loc_zg(kk,1,imodz)
  zzh=loc_zg(kk,2,imodz)
  if(ndim <3)then
   zz=0.0
   zzh=0.0
  endif
  jj=1
  do j=j1,j2
   yy=loc_yg(jj,1,imody)
   yyh=loc_yg(jj,2,imody)
   ii=1
   do i=i1,i2
    xx=loc_xg(ii,1,imodx)
    xxh=loc_xg(ii,2,imodx)
    !==================
    xp=xc+(xxh-xc)*cf+yy*sf
    yp=yy*cf-(xxh-xc)*sf
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zz
    call get_laser_fields_cp(coords,par_lp, fields) !(xh,y,z)
    Ex=e0*fields(1)
    Ey=e0*fields(2)
    ef(i,j,k,1)=ef(i,j,k,1)+Ex*cf-Ey*sf

    xp=xc+(xx-xc)*cf+yyh*sf
    yp=yyh*cf-(xx-xc)*sf
    coords(1)=xp-xf0
    coords(2)=yp
    call get_laser_fields_cp(coords,par_lp, fields) !(x,yh,z)
    Ex=e0*fields(1)
    Ey=e0*fields(2)
    ef(i,j,k,2)=ef(i,j,k,2)+Ey*cf+Ex*sf

    xp=xc+(xx-xc)*cf+yy*sf
    yp=yy*cf-(xx-xc)*sf
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_cp(coords,par_lp,fields) !(x,y,zh)
    Ez=e0*fields(3)
    ef(i,j,k,3)=ef(i,j,k,3)+Ez

    xp=xc+(xx-xc)*cf+yyh*sf
    yp=yyh*cf-(xx-xc)*sf
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zzh
    call get_laser_fields_cp(coords,par_lp,fields) !(x,yh,zh)
    Bx=e0*fields(4)
    By=e0*fields(5)

    ef(i,j,k,4)=ef(i,j,k,4)+Bx*cf-By*sf

    xp=xc+(xxh-xc)*cf+yy*sf
    yp=yy*cf-(xxh-xc)*sf
    coords(1)=xp-xf0
    coords(2)=yp
    call get_laser_fields_cp(coords,par_lp,fields) !(xh,y,zh)
    Bx=e0*fields(4)
    By=e0*fields(5)
    ef(i,j,k,5)=ef(i,j,k,5)+By*cf+Bx*sf

    xp=xc+(xxh-xc)*cf+yyh*sf
    yp=yyh*cf-(xxh-xc)*sf
    coords(1)=xp-xf0
    coords(2)=yp
    coords(3)=zz
    call get_laser_fields_cp(coords,par_lp,fields) !(xh,yh,z)
    Bz=e0*fields(6)
    ef(i,j,k,6)=ef(i,j,k,6)+Bz

    ii=ii+1
   end do
   jj=jj+1
  end do
  kk=kk+1
 end do

 end subroutine init_cp_fields
 !++++++++++++++++++++++++++++++++++++++++++++++
 !  END SECTION FOR Laser field init
 !==============================================================
 !   FLUID density-momenta
 !===========================================================
 subroutine init_fluid_density_momenta(dmodel,part_in)
  integer,intent(in) :: dmodel
  real(dp),intent(in) :: part_in
  integer :: i,i0,j,k,ic,nxl(5),ntot,i0_targ,i1_targ
  integer :: i1,i2,j1,j2,k1,k2
  integer :: j01,j02,k01,k02,jj,kk
  real(dp) :: uu,xtot,l_inv,np1_loc,peak_fluid_density
  real(dp) :: yy,zz,r2
  real(dp),parameter :: one_dp=1.
!==================================
  i1=loc_xgrid(imodx)%p_ind(1)
  i2=loc_xgrid(imodx)%p_ind(2)
  j1=loc_ygrid(imody)%p_ind(1)
  j2=loc_ygrid(imody)%p_ind(2)
  k1=loc_zgrid(imodz)%p_ind(1)
  k2=loc_zgrid(imodz)%p_ind(2)

  xtot=0.0
  ntot=0
 do i=1,5
  nxl(i)=nint(dx_inv*lpx(i))
  lpx(i)=nxl(i)*dx
  xtot=xtot+lpx(i)
  ntot=ntot+nxl(i)
 end do
 if(part_in >0.)then
  targ_in= part_in
  i0_targ=nint(dx_inv*targ_in)
  targ_end=targ_in+xtot
  nxf=i0_targ+ntot
 else
  targ_in=0.0
  i0_targ=0
  targ_end=xtot+part_in     ! targ_end < xtot
  nxf=ntot
 endif
 !=============================
 allocate(fluid_x_profile(nxf))
 !allocate(fluid_yz_profile(j2,k2)) already allocated in comon file
!====================
 peak_fluid_density=1.-ratio_mpfluid
 fluid_x_profile(:)=0.0
 fluid_yz_profile(:,:)=0.0
 np1_loc=0.005
 if(np1>0.0)np1_loc=np1
 l_inv=log(1./np1_loc)
!==============
  j01=j1
  k01=k1
  j02=j2
  k02=k2
  if(pe0y)j01=sh_targ+1
  if(pe1y)j02=j2-sh_targ
  if(ndim >2)then
   if(pe0z)k01=sh_targ+1
   if(pe1z)k02=k2-sh_targ
  endif
  do k=k01,k02
   do j=j01,j02
    fluid_yz_profile(j,k)=1.0
   end do
  end do
  if(pe0)write(6,'(a15,e11.4)')&
  'Uniform fluid density on the (y-z) target coordinates '
  if(Channel)then
   if(ndim <3)then
    k=k01
    zz=0.0
    do j=j01,j02
     jj=j-2
     yy=loc_yg(jj,1,imody)
     r2=(yy*yy+zz*zz)/(w0_y*w0_y)
     fluid_yz_profile(j,k)=1.0 +chann_fact*r2
    end do
   else
    do k=k01,k02
     kk=k-2
     zz=loc_zg(kk,1,imodz)
     do j=j01,j02
      jj=j-2
      yy=loc_yg(jj,1,imody)
      r2=(yy*yy+zz*zz)/(w0_y*w0_y)
      fluid_yz_profile(j,k)=1.0 +chann_fact*r2
     end do
    end do
   endif
   if(pe0)write(6,'(a15,e11.4)')'channel factor=',chann_fact
  endif
 i0=i0_targ
 select case(dmodel)
  case(1)
   if(nxl(1) >0)then
    do i=1,nxl(1)
     i0=i0+1
     fluid_x_profile(i0)=peak_fluid_density*np1
    end do
   endif
   if(nxl(2) >0)then    !sigma=nxl(2)/3
    do i=1,nxl(2)
     i0=i0+1
     uu=(float(i)-float(nxl(2)))/float(nxl(2))
     fluid_x_profile(i0)=peak_fluid_density*exp(-4.5*uu*uu)
    end do
   endif
   do i=1,nxl(3)
    i0=i0+1
    fluid_x_profile(i0)=peak_fluid_density
   end do
   if(nxl(4) >0)then
    do i=1,nxl(4)
     i0=i0+1
     uu=peak_fluid_density*float(i)/nxl(4)
     fluid_x_profile(i0)=peak_fluid_density-uu*(1.-np2)
    end do
   endif
   if(nxl(5) >0)then
    do i=1,nxl(5)
     i0=i0+1
     fluid_x_profile(i0)=peak_fluid_density*np2
    end do
   endif
  case(2)
   if(nxl(1) >0)then            !a ramp 0==>np1
    do i=1,nxl(1)
     i0=i0+1
     uu=(float(i)-float(nxl(1)))/float(nxl(1))
     fluid_x_profile(i0)=np1*peak_fluid_density*exp(-4.5*uu*uu)
    end do
   endif
   if(nxl(2) >0)then    ! np1 plateau
    do i=1,nxl(2)
     i0=i0+1
     fluid_x_profile(i0)=np1*peak_fluid_density
    end do
   endif
   if(nxl(3) >0)then  ! a down ramp np1=> np2
    do i=1,nxl(3)
     i0=i0+1
     uu=peak_fluid_density*float(i)/nxl(3)
     fluid_x_profile(i0)=np1*peak_fluid_density-uu*(np1-np2)
    end do
   endif
   if(nxl(4) >0)then   !a np2 plateau
    do i=1,nxl(4)
     i0=i0+1
     fluid_x_profile(i0)=np2*peak_fluid_density
    end do
   endif
   if(nxl(5) >0)then
    do i=1,nxl(5)
     i0=i0+1
     uu=peak_fluid_density*float(i)/nxl(5)
     fluid_x_profile(i0)=peak_fluid_density*np2*(1.-uu)
    end do
   endif
  case(3)
   if(pe0)then
    write(6,*)'dmodel_id =3 not activated for one-species fluid scheme'
   endif
  return
 !initial plateau, cos^2 bump, central plateau and exit ramp. 
  !See model_id=4 for pic case
  case(4)
  !================ cos^2 upramp with peak np2/n0 =================
   if(nxl(1) >0)then     !a cos^2() ramp
    do i=1,nxl(1)
     i0=i0+1
     uu=(float(i)-0.5)/float(nxl(1))-one_dp
     fluid_x_profile(i0)=peak_fluid_density*&
     np2*cos(0.5*pi*(uu))*cos(0.5*pi*(uu))
    end do
   endif
 !   a first plateau
   if(nxl(2) >0)then
    do i=1,nxl(2)
     i0=i0+1
     fluid_x_profile(i0)=np2*peak_fluid_density
    end do
   end if
    !================ cos^2 downramp to the second plateau =================
   if(nxl(3) >0)then
    do i=1,nxl(3)
     i0=i0+1
     uu=(real(i,dp)-0.5)/float(nxl(3))-one_dp
     fluid_x_profile(i0)=peak_fluid_density*&
     (1.+(np2-1.)*sin(0.5*pi*(uu))*sin(0.5*pi*(uu)))
    end do
   end if
    !================ Central layer=================
   if(nxl(4) >0)then
    do i=1,nxl(4)
     i0=i0+1
     fluid_x_profile(i0)=peak_fluid_density
    end do
   end if
    !================ second linear ramp =================
   if(nxl(5) >0)then
    do i=1,nxl(5)
     i0=i0+1
     uu=(real(i,dp)-0.5)/float(nxl(4))
     fluid_x_profile(i0)=peak_fluid_density*&
     (one_dp-uu)
    end do
   end if
  end select
  if(part_in < 0.0)then
   i1_targ=nint(dx_inv*abs(part_in))
   i0=0
   do i=1,ntot-i1_targ
    i0=i0+1
    fluid_x_profile(i0)=fluid_x_profile(i+i1_targ)
   end do
  endif
!-------------------------------
! target profile of length nxf (xtot)
! now put target on the computationale grid
!
  ic=size(up,4)     !the particle number density
  do k=k1,k2
   do j=j1,j2
    do i=1,i2
     up(i,j,k,ic)=fluid_x_profile(i)*fluid_yz_profile(j,k)
     up0(i,j,k,ic)=up(i,j,k,ic)
    end do
   end do
  end do
 !  Momenta set to zero in the allocation procedure
 !========================
 end subroutine init_fluid_density_momenta
 !========================
 subroutine set_poloidal_ex_fields(ef1,i1,i2,j1,j2,k1,k2,Bpoloidal,rpoloidal)
 real(dp),intent(inout) :: ef1(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2
 real(dp),intent(in) :: Bpoloidal,rpoloidal
 real(dp) :: xx,yy,zz
 integer :: i,j,k

 do k=k1,k2
  do j=j1,j2
   do i=i1,i2
    zz=loc_zg(k-2,1,imodz)
    yy=loc_yg(j-2,1,imody)
    xx=loc_xg(i-2,1,imodx)

    ef1(i,j,k,1)= 0.0   !B_x(i,j+1/2,k+1/2)
    ef1(i,j,k,2)=-Bpoloidal*zz/rpoloidal   !B_y(i+1/2,j,k+1/2)
    ef1(i,j,k,3)= Bpoloidal*yy/rpoloidal   !B_z(i+1/2,j+1/2,k)
   end do
  end do
 end do
 end subroutine set_poloidal_ex_fields


 subroutine set_solenoid_fields(ef1,i1,i2,j1,j2,k1,k2,x0,L_s,Bs)
 real(dp),intent(inout) :: ef1(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2
 real(dp),intent(in) :: x0,L_s(3,2),Bs(2)
 real(dp) :: L,D,B0,ff
 integer :: i,j,k,ii,jj,kk
 real(dp) :: f1,f2,fd1,fd2,xx,yy,zz

 ! Enter parameters B0= B size  L_s geometrical sizes, x0 initial point
 ! in ef1(3) the (Bx,By,Bz) components of two solenoinds

 !------------------------------------
 ! First element
 B0=Bs(1)
 D=x0+L_s(1,1)
 L=L_s(2,1)
 ff=L_s(3,1)
 do k=k1,k2
  kk=k-2
  zz=B0*loc_zg(kk,1,imodz)
  do j=j1,j2
   jj=j-2
   yy=B0*loc_yg(jj,1,imody)
   do i=i1,i2
    ii=i-2
    xx=(loc_xg(ii,1,imodx)-D)/ff
    f1=1./(1.+exp(-xx))
    f2=1./(1.+exp(L/ff-xx))
    ef1(i,j,k,1)=B0*(f1-f2)      !B_x(i,j+1/2,k+1/2)
    xx=(loc_xg(ii,2,imodx)-D)/ff
    f1=1./(1.+exp(-xx))
    f2=1./(1.+exp(L/ff-xx))
    fd1=exp(-xx)*f1*f1/ff
    fd2=exp(L/ff-xx)*f2*f2/ff
    ef1(i,j,k,2)=-yy*(fd1-fd2)   !B_y(i+1/2,j,k+1/2)
    ef1(i,j,k,3)=-zz*(fd1-fd2)   !B_z(i+1/2,j+1/2,k)
   end do
  end do
 end do
 B0=Bs(2)
 D=D+L+L_s(1,2)
 L=L_s(2,2)
 ff=L_s(3,2)
 do k=k1,k2
  kk=k-2
  zz=B0*loc_zg(kk,1,imodz)
  do j=j1,j2
   jj=j-2
   yy=B0*loc_yg(jj,1,imody)
   do i=i1,i2
    ii=i-2
    xx=(loc_xg(ii,1,imodx)-D)/ff
    f1=1./(1.+exp(-xx))
    f2=1./(1.+exp(L/ff-xx))
    ef1(i,j,k,1)=ef1(i,j,k,1)+B0*(f1-f2)      !B_x(i,j+1/2,k+1/2)
    xx=(loc_xg(ii,1,imodx)-D)/ff
    f1=1./(1.+exp(-xx))
    f2=1./(1.+exp(L/ff-xx))
    fd1=exp(-xx)*f1*f1/ff
    fd2=exp(L/ff-xx)*f2*f2/ff
    ef1(i,j,k,2)=ef1(i,j,k,2)-yy*(fd1-fd2)   !B_y(i+1/2,j,k+1/2)
    ef1(i,j,k,3)=ef1(i,j,k,3)-zz*(fd1-fd2)   !B_z(i+1/2,j+1/2,k)
   end do
  end do
 end do
 end subroutine set_solenoid_fields
 !===============================
 end module init_grid_field
