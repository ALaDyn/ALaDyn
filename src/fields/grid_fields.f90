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

 module grid_fields

 use grid_field_param
 use parallel

 implicit none
 contains
!=============================
 subroutine trid_der1(a,b,c,b1,c1,an,bn,n,ic1,ic2,ord)
 real(dp),intent(in) :: a,b,c,b1,c1,an,bn
 integer,intent(in) :: n,ic1,ic2,ord
 integer :: k,ic
 real(dp) :: bet
 !==========================
 ! Solves
 ! a*ww(i-1)+b*ww(i)+c*ww(i+1)=u(i), i=2,3,..,n-1
 ! at the first row b1*ww(1)+c1*ww(2)=u(1)
 ! at the n-last row an*ww(n-1)+bn*ww(n)=u(n)
 ! first order boundary clusure
 !===============================
 if(ord >0)then
  do ic=ic1,ic2
   ww0(1,ic)=ww0(1,ic)+ww0(2,ic)
   ww0(n,ic)=ww0(n,ic)+ww0(n-1,ic)
  end do
 endif
!===================
 do ic=ic1,ic2
  ww1(1)=0.0
  bet=b1
  ww0(1,ic)=ww0(1,ic)/bet
  k=2
  ww1(k)=c1/bet
  bet=b-a*ww1(k)
  ww0(k,ic)=(ww0(k,ic)-a*ww0(k-1,ic))/bet
  do k=3,n-1
   ww1(k)=c/bet
   bet=b-a*ww1(k)
   ww0(k,ic)=(ww0(k,ic)-a*ww0(k-1,ic))/bet
  end do
  k=n
  ww1(k)=c/bet
  bet=bn-an*ww1(k)
  ww0(k,ic)=(ww0(k,ic)-an*ww0(k-1,ic))/bet
  do k=n-1,1,-1
   ww0(k,ic)=ww0(k,ic)-ww1(k+1)*ww0(k+1,ic)
  end do
 end do
 end subroutine trid_der1

 subroutine enforce_continuity(curr,i1,n1p,j1,j2,k1,k2)

 real(dp),intent(inout) :: curr(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,j2,k1,k2
 real(dp) :: aphy,aphz,shy,shz
 integer :: i,ii,j,k,jj,j01,k01
 !===================== 3D Cartesian 
 !Solves DJ_x/Dx =-[DJ_y/Dy+DJ_z/Dz +[Rho^{n+1}-Rho^n]/Dt]=
 ! Eneter curr(1)= Drho, curr(2)=J_y*Dt  curr(3)= J_z*Dt
 !=======================================
 aphy=dy_inv
 aphz=dz_inv
 j01=j1
 k01=k1
                           !shy(3)=Dxi/Dy centered on node y_j
 if(ndim==1)return
 if(pe0y)then
  j=j1
  shy=loc_yg(j-1,3,imody)*aphy
  do k=k1,k2
   do i=i1,n1p
    curr(i,j,k,1)=curr(i,j,k,1)+shy*(curr(i,j+1,k,2)-curr(i,j,k,2))
   end do
  end do
  j01=j1+1
 endif
 do k=k1,k2
  do j=j01,j2
   jj=j-2
   shy=loc_yg(jj,3,imody)*aphy
   do i=i1,n1p
    curr(i,j,k,1)=curr(i,j,k,1)+shy*(curr(i,j,k,2)-curr(i,j-1,k,2))
   end do
  end do
 end do
 !================ ndim >2
 if(ndim ==3)then
  if(pe0z)then
   k=k1
   shz=loc_zg(k-1,3,imodz)*aphz
   do j=j1,j2
    do i=i1,n1p
     curr(i,j,k,1)=curr(i,j,k,1)+shz*(curr(i,j,k+1,3)-curr(i,j,k,3))
    end do
   end do
   k01=k1+1
  endif
  do k=k01,k2
   shz=loc_zg(k-2,3,imodz)*aphz
   do j=j1,j2
    do i=i1,n1p
     curr(i,j,k,1)=curr(i,j,k,1)+shz*(curr(i,j,k,3)-curr(i,j,k-1,3))
    end do
   end do
  end do
 endif
!++++++++++++ 1D invertion of first derivative
 ww0(:,:)=0.0
 do k=k1,k2
  do j=j1,j2
   do i=n1p,i1,-1
    ii=i-2
    ww0(ii,1)=ww0(ii+1,1)+dx*curr(i+1,j,k,1)
   end do
   do i=1,n1p
    ii=i-2
    curr(i,j,k,1)=ww0(ii,1)
   end do
  end do
 end do

 end subroutine enforce_continuity
 subroutine field_xcomov_advect(ef,i1,n1p,j1,n2p,k1,n3p,ic1,ic2,aphx,v_adv,tsch)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,ic1,ic2,tsch
 real(dp),intent(in) :: aphx,v_adv
 integer :: i,j,k,ii,n1,n1_loc,ic,ind
 real(dp) :: aphx_adv,aphx_adv1,a,b,c,b1,c1,an,bn
 real(dp),dimension(3),parameter :: rder=(/-3.,4.,-1./)
 !=====================
 ! APPLIES also for prlx=.true. (MPI x decomposition)
 !=============================================
 ! Solves Df/Dt=-v_adv*Df/Dx
 ! forward advection for v_adv >
 ! backward advection for v_adv <0
 ! In comoving system in the Maxwell eqs. enters v_adv <0
 !==================
 !Implicit advection scheme in x-coordinate 
 !          E^n+1=E^n-aphx_adv*[D_xE^n+D_xE^{n+1}]
 !          (1+aphx_adv*D_x)E^{n+1}=(1-aphx_adv*D_x)E^n
 !================================
       aphx_adv1=0.5*v_adv*aphx     ! v_b*Dt/(2*Dx) for first order explicit
       aphx_adv=0.5*aphx_adv1  !semi-implicit second order
                                !v_adv*(Dt/2Dx)/2=>  1/2 for time centering
 ind=1
 b1=1.
 c1=0.0
 bn=1.
 an=0.0
 !bn = 1.-2.*aphx_adv
 !cn = 2.*aphx_adv
 !=====================
 n1_loc=loc_xgrid(imodx)%ng
 n1=n1_loc
 !=========================
 if(prlx)n1=nx
 !=====================
 if(pe0x)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(i1-1,j,k,ic)=ef(i1+1,j,k,ic)
    enddo
   end do
  end do
 endif
 if(pe1x)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(n1p+1,j,k,ic)=ef(n1p,j,k,ic)
    enddo
   end do
  end do
 endif
 !===============================
 select case(tsch)
 case(0)             !pure explicit
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     ii=i-2
     ww0(ii,1)=ef(i,j,k,ic)-aphx_adv1*(ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
    end do
    do i=i1,n1p
     ii=i-2
     ef(i,j,k,ic)=ww0(ii,1)
    end do
   end do
  end do
 end do
 case(1)      !semi-implicit
  a=-aphx_adv
  b=1.
  c= aphx_adv
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     ii=i-2
     ww0(ii,1)=ef(i,j,k,ic)-aphx_adv*(ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
    end do
    call trid_der1(a,b,c,b1,c1,an,bn,ii,1,1,0)
    do i=i1,n1p
     ii=i-2
     ef(i,j,k,ic)=ww0(ii,1)
    end do
   end do
  end do
 end do
 case(2)      !implicit (1+aphx_adv1*Dx]ef=ef
  a=-aphx_adv1
  b=1.
  c= aphx_adv1
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     ii=i-2
     ww0(ii,1)=ef(i,j,k,ic)
    end do
    call trid_der1(a,b,c,b1,c1,an,bn,ii,1,1,0)
    do i=i1,n1p
     ii=i-2
     ef(i,j,k,ic)=ww0(ii,1)
    end do
   end do
  end do
 end do
 end select
 end subroutine field_xcomov_advect
!==================================
 subroutine field_xadvect(ef,i1,n1p,j1,n2p,k1,n3p,ic1,ic2,aphx,v_adv)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,ic1,ic2
 real(dp),intent(in) :: aphx,v_adv
 integer :: i,j,k,ii,n1,n1_loc,ic,ind
 real(dp) :: aphx_adv,aphx_adv1,a,b,c,b1,c1,an,bn
 real(dp),dimension(3),parameter :: rder=(/-3.,4.,-1./)
 !=====================
 ! APPLIES also for prlx=.true. (MPI x decomposition)
 !=============================================
 ! Solves Df/Dt=-v_adv*Df/Dx
 ! forward advection for v_adv >
 ! backward advection for v_adv <0
 ! In comoving system in the Maxwell eqs. enters v_adv <0
 !==================
 !Implicit advection scheme in x-coordinate 
 !          E^n+1=E^n-aphx_adv*[D_xE^n+D_xE^{n+1}]
 !          (1+aphx_adv*D_x)E^{n+1}=(1-aphx_adv*D_x)E^n
 !================================
       aphx_adv1=v_adv*aphx     ! for first order upwind
       aphx_adv=0.25*aphx_adv1  !implicit second order
                                !v_adv*(Dt/Dx)/4=>  1/2 for space derivative and 
       !                                            1/2 for time averaging
 ind=1
 b1=1.
 c1=0.0
 bn=1.
 an=0.0
 a=-aphx_adv
 b=1.
 c= aphx_adv
 !bn = 1.-2.*aphx_adv
 !cn = 2.*aphx_adv
 !=====================
 n1_loc=loc_xgrid(imodx)%ng
 n1=n1_loc
 !=========================
 if(prlx)n1=nx
 !=====================
 if(pe0x)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(i1-1,j,k,ic)=ef(i1+1,j,k,ic)
    enddo
   end do
  end do
 endif
 if(pe1x)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(n1p+1,j,k,ic)=ef(n1p,j,k,ic)
    enddo
   end do
  end do
 endif
 if(prlx)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ww1(1:n1)=0.0
     do i=i1,n1p
      ii=i-2
      ww1(ii)=ef(i,j,k,ic)-aphx_adv*(&
       ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
     end do
     call all_gather_dpreal(ww1,ww2,3,n1_loc)
     do i=1,n1
      ww0(i,1)=ww2(i)
     end do
     call trid_der1(a,b,c,b1,c1,an,bn,n1,1,1,0)
     do i=i1,n1p
      ii=i-2+imodx*n1_loc
      ef(i,j,k,ic)=ww0(ii,1)
     end do
    end do
   end do
  end do
  return
 endif
 !===============================
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     ii=i-2
     ww0(ii,1)=ef(i,j,k,ic)-aphx_adv*(ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
    end do
                                  !call upper_trid(a,b,c,bn,cn,ii,ic1,ic2)
    call trid_der1(a,b,c,b1,c1,an,bn,ii,1,1,0)
    do i=i1,n1p
     ii=i-2
     ef(i,j,k,ic)=ww0(ii,1)
    end do
   end do
  end do
 end do
 end subroutine field_xadvect
 !======================
 subroutine pp_lapl(&
            av,source,ic1,ic2,ord,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)

 real(dp),intent(inout) :: av(:,:,:,:)
 real(dp),intent(inout) :: source(:,:,:,:)

 integer,intent(in) :: ic1,ic2,ord,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: dhy,dhz
 integer :: i,j,k,ic,jj,j01,j02,k01,k02
 real(dp) :: dy2_inv,dz2_inv,cf(2),shy,shz,sphy,smhy,sphz,smhz
 real(dp) :: dy4_inv(2),dz4_inv(2)
 !==========================
 ! is=1 adds  is=-1 subtracts the laaplacian term
 !--------------------------------------------------
 dy2_inv=dhy*dhy
 dz2_inv=dhz*dhz
 cf(1)=1.
 cf(2)=.0
 if(ord==4)then
  cf(1)=4./3.       !1-8*hord_der2/3
  cf(2)=-1./12.      !2*(2*hord_der2-1)
 endif
 dy4_inv(1)=cf(1)*dy2_inv
 dy4_inv(2)=cf(2)*dy2_inv
 dz4_inv(1)=cf(1)*dz2_inv
 dz4_inv(2)=cf(2)*dz2_inv
 !===========================
 !Second order derivative. At boundaries D^3[av]=0
 shy=1.
 shz=1.
 sphy=1.
 smhy=1.
 sphz=1.
 smhz=1.
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 if(iby <2)then
  if(pe0y)then
   j=j1
   jj=j-2
   shy=dy2_inv*loc_yg(jj+1,3,imody)
   sphy=loc_yg(jj+1,4,imody)
   smhy=loc_yg(jj,4,imody)
   do ic=ic1,ic2
    do k=k1,n3p
     do i=i1,n1p
     source(i,j,k,ic)=source(i,j,k,ic)+shy*(&
       sphy*(av(i,j+2,k,ic)-av(i,j+1,k,ic))-smhy*(av(i,j+1,k,ic)-av(i,j,k,ic)))
     end do
     j01=j1+1
    end do
   end do
   if(der_ord==4)then
   j=j1+1
   jj=j-2
   shy=dy2_inv*loc_yg(jj,3,imody)
   sphy=loc_yg(jj,4,imody)
   smhy=loc_yg(jj-1,4,imody)
   do ic=ic1,ic2
     do k=k1,n3p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+shy*(&
       sphy*(av(i,j+1,k,ic)-av(i,j,k,ic))-smhy*(av(i,j,k,ic)-av(i,j-1,k,ic)))
      end do
     end do
    end do
    j01=j1+2
   endif
  endif        !Pe0y end
  if(pe1y)then
   j=n2p
   jj=j-2
   shy=dy2_inv*loc_yg(jj-1,3,imody)
   sphy=loc_yg(jj-1,4,imody)
   smhy=loc_yg(jj-2,4,imody)
   do ic=ic1,ic2
    do k=k1,n3p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+shy*(&
       sphy*(av(i,j,k,ic)-av(i,j-1,k,ic)) -&
       smhy*(av(i,j-1,k,ic)-av(i,j-2,k,ic)))
     end do
    end do
   end do
   j02=n2p-1
   if(der_ord==4)then
    j=n2p-1
    jj=j-2
    shy=dy2_inv*loc_yg(jj,3,imody)
    sphy=loc_yg(jj,4,imody)
    smhy=loc_yg(jj-1,4,imody)
    do ic=ic1,ic2
     do k=k1,n3p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+shy*(&
        sphy*(av(i,j+1,k,ic)-av(i,j,k,ic)) -&
        smhy*(av(i,j,k,ic)-av(i,j-1,k,ic)))
      end do
     end do
    end do
    j02=n2p-2
   endif
  endif
 endif            !END non periodic BCs
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j01,j02
    jj=j-2
    shy=dy4_inv(1)*loc_yg(jj,3,imody)
    sphy=loc_yg(jj,4,imody)
    smhy=loc_yg(jj-1,4,imody)
    do i=i1,n1p
     source(i,j,k,ic)=source(i,j,k,ic)+shy*(&
      sphy*(av(i,j+1,k,ic)-av(i,j,k,ic))-&
      smhy*(av(i,j,k,ic)-av(i,j-1,k,ic)))
    end do
   end do
  end do
 end do
 if(der_ord==4)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j01,j02
     jj=j-2
     shy=dy4_inv(2)*loc_yg(jj,3,imody)
     sphy=loc_yg(jj+1,3,imody)
     smhy=loc_yg(jj-1,3,imody)
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+shy*(&
                    sphy*(av(i,j+2,k,ic)-av(i,j,k,ic))-&
                    smhy*(av(i,j,k,ic)-av(i,j-2,k,ic)))
     end do
    end do
   end do
  end do
 endif
 if(ndim< 3)return
 !====================
 if(ibz <2)then
  if(pe0z)then
   k=k1
   jj=k-2
   shz=dz2_inv*loc_zg(jj+1,3,imodz)
   sphz=loc_zg(jj+1,4,imodz)
   smhz=loc_zg(jj,4,imodz)
   do ic=ic1,ic2
    do j=j1,n2p
     do i=i1,n1p
     source(i,j,k,ic)=source(i,j,k,ic)+shz*(&
       sphz*(av(i,j,k+2,ic)-av(i,j,k+1,ic))-smhz*(av(i,j,k+1,ic)-av(i,j,k,ic)))
     end do
    end do
   end do
   k01=k1+1
   if(der_ord==4)then
    k=k1+1
    jj=k-2
    shz=dz2_inv*loc_zg(jj,3,imodz)
    sphz=loc_zg(jj,4,imodz)
    smhz=loc_zg(jj-1,4,imodz)
    do ic=ic1,ic2
     do j=j1,n2p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+shz*(&
       sphz*(av(i,j,k+1,ic)-av(i,j,k,ic))-&
       smhz*(av(i,j,k,ic)-av(i,j,k-1,ic)))
      end do
     end do
    end do
    k01=k1+2
   endif
  endif
  if(pe1z)then
   k=n3p
   jj=k-2
   shz=dz2_inv*loc_zg(jj-1,3,imodz)
   sphz=loc_zg(jj-1,4,imodz)
   smhz=loc_zg(jj-2,4,imodz)
   do ic=ic1,ic2
    do j=j1,n2p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+shz*(&
       sphz*(av(i,j,k,ic)-av(i,j,k-1,ic))-&
       smhz*(av(i,j,k-1,ic)-av(i,j,k-2,ic)))
     end do
    end do
   end do
   k02=n3p-1
   if(der_ord==4)then
    k=n3p-1
    jj=k-2
    shz=dz2_inv*loc_zg(jj,3,imodz)
    sphz=loc_zg(jj,4,imodz)
    smhz=loc_zg(jj-1,4,imodz)
    do ic=ic1,ic2
     do j=j1,n2p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+shz*(&
        sphz*(av(i,j,k+1,ic)-av(i,j,k,ic))-&
        smhz*(av(i,j,k,ic)-av(i,j,k-1,ic)))
      end do
     end do
    end do
    k01=n3p-2
   endif
  endif
 endif
 do ic=ic1,ic2
  do k=k01,k02
   jj=k-2
   shz=dz4_inv(1)*loc_zg(jj,3,imodz)
   sphz=loc_zg(jj,4,imodz)
   smhz=loc_zg(jj-1,4,imodz)
   do j=j1,n2p
    do i=i1,n1p
     source(i,j,k,ic)=source(i,j,k,ic)+shz*(&
      sphz*(av(i,j,k+1,ic)-av(i,j,k,ic))- &
      smhz*(av(i,j,k,ic)-av(i,j,k-1,ic)))
    end do
   end do
  end do
 end do
 if(der_ord==4)then
  do ic=ic1,ic2
   do k=k01,k02
    jj=k-2
    shz=dz4_inv(2)*loc_zg(jj,3,imodz)
    sphz=loc_zg(jj+1,3,imodz)
    smhz=loc_zg(jj-1,3,imodz)
    do j=j1,n2p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+shz*(&
                    sphz*(av(i,j,k+2,ic)-av(i,j,k,ic))-&
                    smhz*(av(i,j,k,ic)-av(i,j,k-2,ic)))
     end do
    end do
   end do
  end do
 endif
 !======================================
 end subroutine pp_lapl
 !========================
 subroutine env_grad(envg,i1,n1p,j1,n2p,k1,n3p,ider,dhx,dhy,dhz)

 real(dp),intent(inout) :: envg(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,ider
 real(dp),intent(in) :: dhx,dhy,dhz
 integer :: i,j,k,j01,j02,k01,k02
 real(dp) :: ax1,ax2,ay1,ay2,az1,az2,shz,shy,shp,shm
 real(dp),parameter :: a_hcd=13./12., b_hcd=-1./24.
 !=== second or fourth order central flux derivatives
 !==========================
 ! Enters envg(1)= |A|^2/2 exit grad|A|^2/2

 if(ider< 4)then
  ax1=dhx
  ay1=dhy
  az1=dhz
  ax2=0.
  ay2=0.
  az2=0.
 else
  ax1=dhx*a_hcd
  ay1=dhy*a_hcd
  az1=dhz*a_hcd
  ax2=dhx*b_hcd
  ay2=dhy*b_hcd
  az2=dhz*b_hcd
 endif
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 !================
 do k=k1,n3p
  do j=j1,n2p
   i=i1
   envg(i,j,k,2)=dhx*(envg(i+1,j,k,1)-envg(i,j,k,1)) !at i+1/2
   do i=i1+1,n1p-2
    envg(i,j,k,2)=ax1*(envg(i+1,j,k,1)-envg(i,j,k,1)) !at i+1/2
   end do
   i=n1p-1
   envg(i,j,k,2)=dhx*(envg(i+1,j,k,1)-envg(i,j,k,1))
   envg(i+1,j,k,2)=dhx*(2.*envg(i,j,k,1)-3.*envg(i-1,j,k,1)+envg(i-2,j,k,1))
  end do
 end do
 if(ider==4)then
  do k=k1,n3p
   do j=j1,n2p
    do i=i1+1,n1p-2
     envg(i,j,k,2)=envg(i,j,k,2)+ax2*(envg(i+2,j,k,1)-envg(i+1,j,k,1)+&
                                      envg(i,j,k,1)-envg(i-1,j,k,1))
    end do
   end do
  end do
 endif
 if(pe1y)then
  do k=k1,n3p
   j=n2p
   shy=loc_yg(j-2,4,imody)*dhy
   do i=i1,n1p
    envg(i,j,k,3)=shy*(2.*envg(i,j,k,1)-3.*envg(i,j-1,k,1)+envg(i,j-2,k,1))
   end do
   j=n2p-1
   shy=loc_yg(j-2,4,imody)*dhy
   do i=i1,n1p
    envg(i,j,k,3)=shy*(envg(i,j+1,k,1)-envg(i,j,k,1))
   end do
  end do
  j02=n2p-2
 end if
!===================
 if(pe0y)then
  j=j1
  shy=loc_yg(j-2,4,imody)*dhy
  do k=k1,n3p
   do i=i1,n1p
    envg(i,j,k,3)=shy*(envg(i,j+1,k,1)-envg(i,j,k,1))
   end do
  end do
  j01=j1+1
 endif
 do k=k1,n3p
  do j=j01,j02
   shy=loc_yg(j-2,4,imody)*ay1
   do i=i1,n1p
    envg(i,j,k,3)=shy*(envg(i,j+1,k,1)-envg(i,j,k,1))
   end do
  end do
 end do
 if(ider==4)then
  do k=k1,n3p
   do j=j01,j02
    shp=loc_yg(j-1,4,imody)*ay2
    shm=loc_yg(j-3,4,imody)*ay2
    do i=i1,n1p
     envg(i,j,k,3)=envg(i,j,k,3)+shp*(envg(i,j+2,k,1)-envg(i,j+1,k,1))+&
                                 shm*(envg(i,j,k,1)-envg(i,j-1,k,1))
    end do
   end do
  end do
 endif
 if(ndim ==2)return
 if(pe1z)then
  k=n3p
  shz=loc_zg(k-2,4,imodz)*dhz
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k+1,1)=2.*envg(i,j,k,1)-envg(i,j,k-1,1)
    envg(i,j,k,4)=shz*(envg(i,j,k+1,1)-envg(i,j,k,1))
   end do
  end do
  k02=n3p-1
 end if
 if(pe0z)then
  k=k1
  shz=loc_zg(k-2,4,imodz)*dhz
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k-1,1)=2.*envg(i,j,k,1)-envg(i,j,k+1,1)
    envg(i,j,k,4)=shz*(envg(i,j,k+1,1)-envg(i,j,k,1))
   end do
  end do
  k01=k1+1
 end if
 !==================
 do k=k01,k02
  shz=loc_zg(k-2,4,imodz)*az1
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k,4)=shz*(envg(i,j,k+1,1)-envg(i,j,k,1))
   end do
  end do
 end do
 if(ider==4)then
  do k=k01,k02
   shp=loc_zg(k-1,4,imodz)*az2
   shm=loc_zg(k-3,4,imodz)*az2
   do j=j1,n2p
    do i=i1,n1p
     envg(i,j,k,4)=envg(i,j,k,4)+shp*(envg(i,j,k+2,1)-envg(i,j,k+1,1))+&
                                 shm*(envg(i,j,k,1)-envg(i,j,k-1,1))
    end do
   end do
  end do
 endif
 end subroutine env_grad
!====================================
 subroutine env_maxw_solve(curr,evf,i1,n1p,j1,n2p,k1,n3p,&
  om0,dhx,dhy,dhz,dt_loc)
  real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:)
  integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
  real(dp),intent(in) :: om0,dhx,dhy,dhz,dt_loc
  integer :: i,j,k,ic
  real(dp) ::dt2,dx1_inv,dhx1_inv,aph_opt(2)
  real(dp) ::kfact,k2_fact,skfact
  real(dp),dimension(0:2),parameter :: lder=(/1.0,-4.0,3.0/)
 !==========================
 ! EXPLICIT INTEGRATION of Maxwell ENVELOPE EVOLUTION EQUATION
 !See: D.Terzani P. Londrillo " A faster and accurate field solver...."
 !         CPC 2019
 !============================
  dt2=dt_loc*dt_loc
  !khfact=2.*sin(0.5*om0*dt_loc)/dt_loc
  !kh2_fact=khfact*khfact
  !khfact=2.*dhx*sin(0.5*om0*dx)
  !kh2_sfact=khfact*khfact

  !kfact=sin(om0*dt_loc)
  kfact=om0*dt_loc
  k2_fact=1./(1.+kfact*kfact)
  skfact=om0
  !skfact=dhx*sin(om0*dx)
  dx1_inv=skfact*dhx
  dhx1_inv=2.*dx1_inv
  aph_opt(1)=1.
  aph_opt(2)=0.
  if(der_ord ==3)then
   aph_opt(1)=dx1_inv*opt_der1
   aph_opt(2)=dx1_inv*0.5*(1.-opt_der1)
  endif
  ic=2
 !========Enter  jc(1:2)= - omp2*<q^2*chi*env(1:2)
 !                        chi <q^2*wgh*n/gam_p> >0
 ! Computes the full Laplacian of A^{n}=env(1:2) components
 !========and adds to  jc(1:2)
  call potential_lapl(evf,curr,1,ic,der_ord,i1,n1p,j1,n2p,k1,n3p,dhx,dhy,dhz)
 !=====================
 ! =>   jc(1:2)=[D^2-omp^2*chi]A= S(A);
 !=================
 !  Computes D_{x} centered first derivatives of A and adds to S(A)
  call first_Ader
 !S_R => S_R -2*k0[D_xA_I]  
 !S_I => S_I +2*k0[D_xA_R]
 !
   do k=k1,n3p
    do j=j1,n2p
     do i=i1,n1p
      curr(i,j,k,1)=dt2*curr(i,j,k,1)+2.*evf(i,j,k,1)-evf(i,j,k,3)+kfact*evf(i,j,k,4)
      curr(i,j,k,2)=dt2*curr(i,j,k,2)+2.*evf(i,j,k,2)-evf(i,j,k,4)-kfact*evf(i,j,k,3)
     end do
    end do
   end do
      
 !====================
 !curr(1)=F_R=dt2*S_R+2*A_R^n-A_R^{n-1}-kfact*A_I^{n-1} 
 !curr(2)=F_I=dt2*S_I+2*A_I^n-A_I^{n-1}+kfact*A_R^{n-1} 
    do k=k1,n3p
     do j=j1,n2p
      do i=i1,n1p
       evf(i,j,k,3)=evf(i,j,k,1)  !A^{n}=> A^{n-1}
       evf(i,j,k,4)=evf(i,j,k,2)
       evf(i,j,k,1)=k2_fact*(curr(i,j,k,1)-kfact*curr(i,j,k,2))
       evf(i,j,k,2)=k2_fact*(curr(i,j,k,2)+kfact*curr(i,j,k,1))
     end do
    end do
   enddo
 contains
 subroutine first_Ader
!============
  ! explicit second order [-2isin(k0dx)*Dx]A and add to S(A)
 if(der_ord <3)then
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    curr(i,j,k,1)=curr(i,j,k,1)-dhx1_inv*(&
             evf(i+1,j,k,2)-evf(i,j,k,2))
    curr(i,j,k,2)=curr(i,j,k,2)+dhx1_inv*(&
             evf(i+1,j,k,1)-evf(i,j,k,1))
    do i=i1+1,n1p-1
     curr(i,j,k,1)=curr(i,j,k,1)-dx1_inv*(&
              evf(i+1,j,k,2)-evf(i-1,j,k,2))
     curr(i,j,k,2)=curr(i,j,k,2)+dx1_inv*(&
              evf(i+1,j,k,1)-evf(i-1,j,k,1))
    end do
    i=n1p
    curr(i,j,k,1)=curr(i,j,k,1)-dx1_inv*(&
             evf(i,j,k,2)-evf(i-1,j,k,2))
    curr(i,j,k,2)=curr(i,j,k,2)+dx1_inv*(&
             evf(i,j,k,1)-evf(i-1,j,k,1))
   end do
  end do
 else
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    curr(i,j,k,1)=curr(i,j,k,1)-dhx1_inv*(&
             evf(i+1,j,k,2)-evf(i,j,k,2))
    curr(i,j,k,2)=curr(i,j,k,2)+dhx1_inv*(&
             evf(i+1,j,k,1)-evf(i,j,k,1))
    i=i+1
     curr(i,j,k,1)=curr(i,j,k,1)-dx1_inv*(&
              evf(i+1,j,k,2)-evf(i-1,j,k,2))
     curr(i,j,k,2)=curr(i,j,k,2)+dx1_inv*(&
              evf(i+1,j,k,1)-evf(i-1,j,k,1))
    do i=i1+2,n1p-2
     curr(i,j,k,1)=curr(i,j,k,1)- &
                 aph_opt(1)*(evf(i+1,j,k,2)-evf(i-1,j,k,2))-&
                 aph_opt(2)*(evf(i+2,j,k,2)-evf(i-2,j,k,2))
     curr(i,j,k,2)=curr(i,j,k,2)+ &
                 aph_opt(1)*(evf(i+1,j,k,1)-evf(i-1,j,k,1))+&
                 aph_opt(2)*(evf(i+2,j,k,1)-evf(i-2,j,k,1))
    end do
    i=n1p-1
     curr(i,j,k,1)=curr(i,j,k,1)-dx1_inv*(&
              evf(i+1,j,k,2)-evf(i-1,j,k,2))
     curr(i,j,k,2)=curr(i,j,k,2)+dx1_inv*(&
              evf(i+1,j,k,1)-evf(i-1,j,k,1))
    i=n1p
    curr(i,j,k,1)=curr(i,j,k,1)-dx1_inv*(&
             evf(i,j,k,2)-evf(i-1,j,k,2))
    curr(i,j,k,2)=curr(i,j,k,2)+dx1_inv*(&
             evf(i,j,k,1)-evf(i-1,j,k,1))
   end do
  end do
 endif
 end subroutine first_Ader

 end subroutine env_maxw_solve
!==================================
 subroutine env_lpf_solve(curr,evf,ib,i1,n1p,j1,n2p,k1,n3p,&
  om0,dhx,dhy,dhz,dt_loc)
 real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:)
 integer,intent(in) :: ib,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: om0,dhx,dhy,dhz,dt_loc
 integer :: i,j,k,ii,ic,ic1,n1
 real(dp) :: dx1_inv,om2,aph1,dx_norm,dx2_norm
 real(dp) :: adv,an,bn,der2_norm
 !==========================
 ! EXPLICIT INTEGRATION of REDUCED ENVELOPE FIELD SOLVER
 !============================
 ! Fourth order first derivative
 ! D_xu= 2/3[u_{i+1}-u_{i-1}]- [u_{i+2}-u_{i-2}]/12
 !====================
 
 om2=om0*om0
 dx1_inv=0.5*dhx
 aph1=dx1_inv
 n1=n1p+1-i1
 dx_norm=dhx/om0
 dx2_norm=dx_norm*dx_norm
 der2_norm=0.25*dx2_norm
 !========Enter  jc(1:2)= -om2*<q^2*chi*env(1:2)
 !        chi <q^2*wgh*n/gam_p> >0
 ! Computes the transverse Laplacian of A^{n}=env(1:2) components
 !========and adds to jc(1:2)
 ic=2
 call pp_lapl(evf,curr,1,ic,2,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)
 !=====================
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     curr(i,j,k,ic)=-dt_loc*curr(i,j,k,ic)
    end do
   end do
  end do
 end do
 !=======================================================
 ! =>   jc(1:2)=2*Delta t*S(A)=-dt*[D^2_{pp}-omp^2*chi]A;
 !=================
 !  Computes D_{xi} centered first derivatives of S(A)
 !      ww0(1)= k0*S(A_I) + D_xi[A_R]= F_R
 !      ww0(2)= -k0*S(A_R) + D_xi[A_I]= F_I
 !====================
  call first_der
 !curr(1)=F^R
 !curr(2)=F^I
!==================
! The M operator M=[k0*k0+D_xD_x]X = F   X=DA/Dtau

 !   Explicit inversion 
 !M^{-1}=([1-Dx_norm^2]F)/k0*k0
 !===============
     call explicit_mat_inv
 !   curr=M^{-1}F
 !===============
 !   Implicit inversion of a tridiagonal matrix
 !   aX_{i-1}+bX_i + aX_{i+1}=F
 !=============
     !call implicit_mat_inv
 
 
 !
 if(ib>0)then     !fixed coordinate system (x,t)
 !=======================
  !(1+Dt*D_x]A^{n+1}=(1-Dt*D_x)A^{n-1}+ M^{-1}F
  !==================================
  select case(ib)   !ib=der-1
  case(1)
!================= Explicit second order
   adv=dt_loc*dhx     !cfl=dt/dx
   do ic=1,2
    ic1=ic+2
    do k=k1,n3p
     do j=j1,n2p
      i=i1
      evf(i-1,j,k,ic)=evf(i,j,k,ic)
      curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)-adv*(&
                evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
      do i=i1+1,n1p-1
       curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)-adv*(&
        evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
      end do
      i=n1p
      evf(i+1,j,k,ic)=evf(i,j,k,ic)
       curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)-adv*(&
       evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
      do i=i1,n1p
       evf(i,j,k,ic1)=evf(i,j,k,ic)
       evf(i,j,k,ic)=curr(i,j,k,ic)
      end do
     end do
    end do
   enddo
  case(2)     !Explicit  optimized
!=======================
   ! u^{n+1}=u^{n-1}+adv*(u_{i+1}-u_{i-1})+0.5*adv*(
   !adv=dt_loc*dhx     !cfl=dt/dx
!========================================
   adv=dt_loc*dhx
   an=(4.-adv*adv)/3.
   bn=0.5*adv*(1.-an)
   an=an*adv
   do ic=1,2
    ic1=ic+2
    do k=k1,n3p
     do j=j1,n2p
      i=i1
      evf(i-1,j,k,ic)=evf(i,j,k,ic)
      do i=i1,i1+1
       curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)-adv*(&
                   evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
      end do
      do i=i1+2,n1p-2
       curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)-an*(&
        evf(i+1,j,k,ic)-evf(i-1,j,k,ic))-bn*(&
        evf(i+2,j,k,ic)-evf(i-2,j,k,ic))
      end do
      i=n1p
      evf(i+1,j,k,ic)=evf(i,j,k,ic)
      do i=n1p-1,n1p
       curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)-adv*(&
        evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
      end do
      do i=i1,n1p
       evf(i,j,k,ic1)=evf(i,j,k,ic)
       evf(i,j,k,ic)=curr(i,j,k,ic)
      end do
     end do
    end do
   enddo
  end select
 else                   !ib=0 comoving coordinate system
  do ic=1,2
   ic1=ic+2
   do k=k1,n3p
    do j=j1,n2p
     do i=i1,n1p
      curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)  !Curr=A^{n-1}+curr
      evf(i,j,k,ic1)=evf(i,j,k,ic)                  !A^{n-1}=> A^n
      evf(i,j,k,ic)=curr(i,j,k,ic)                  !A^{n+1}=curr
     end do
    end do
   end do
  end do
 endif
 contains
 subroutine first_der
!============
  ! explicit second order

 do k=k1,n3p
  do j=j1,n2p
   i=i1
   ii=i-2
   ww0(ii,1)=om0*curr(i,j,k,2)+dhx*(&
             curr(i+1,j,k,1)-curr(i,j,k,1))
   ww0(ii,2)= -om0*curr(i,j,k,1)+dhx*(&
             curr(i+1,j,k,2)-curr(i,j,k,2))
   do i=i1+1,n1p-1
    ii=i-2
    ww0(ii,1)=om0*curr(i,j,k,2)+dx1_inv*(&
              curr(i+1,j,k,1)-curr(i-1,j,k,1))
    ww0(ii,2)= -om0*curr(i,j,k,1)+dx1_inv*(&
              curr(i+1,j,k,2)-curr(i-1,j,k,2))
   end do
   i=n1p
   ii=i-2
   ww0(ii,1)=om0*curr(i,j,k,2)+dhx*(&
             curr(i,j,k,1)-curr(i-1,j,k,1))
   ww0(ii,2)=-om0*curr(i,j,k,1)+dhx*(&
             curr(i,j,k,2)-curr(i-1,j,k,2))
   do i=i1,n1p
    ii=i-2
    curr(i,j,k,1)=ww0(ii,1)
    curr(i,j,k,2)=ww0(ii,2)
   end do
  end do
 end do
 end subroutine first_der

!============================
  subroutine explicit_mat_inv
   integer :: ic
!================== Uses three-point numerical secon derivative
   do ic=1,2
    do k=k1,n3p
     do j=j1,n2p
      i=i1
      ii=i-2
      ww0(ii,1)=dx2_norm*(&
              curr(i,j,k,ic)-2.*curr(i+1,j,k,ic)+curr(i+2,j,k,ic))
      do i=i1+1,n1p-1
       ii=i-2
       ww0(ii,1)=dx2_norm*(&
              curr(i+1,j,k,ic)-2.*curr(i,j,k,ic)+curr(i-1,j,k,ic))
      end do
      i=n1p
      ii=i-2
      ww0(ii,1)=dx2_norm*(&
              curr(i,j,k,ic)-2.*curr(i-1,j,k,ic)+curr(i-2,j,k,ic))
      do i=i1,n1p
       ii=i-2
       curr(i,j,k,ic)=(curr(i,j,k,ic)-ww0(ii,1))/om2
      end do
     end do
    end do
   end do
  end subroutine explicit_mat_inv
!=======================
  subroutine implicit_mat_inv
!================== Uses three-point numerical secon derivative
   do k=k1,n3p
    do j=j1,n2p
     do i=i1,n1p
      ii=i-2
      ww0(ii,1)=curr(i,j,k,1)
      ww0(ii,2)=curr(i,j,k,2)
     end do
     do i=i1,n1p
      ii=i-2
      curr(i,j,k,1)=ww0(ii,1)/om2
      curr(i,j,k,2)=ww0(ii,2)/om2
     end do
    end do
   end do
  end subroutine implicit_mat_inv
 !===========================
 end subroutine env_lpf_solve
!========================
 ! END ENV SECTION
 !========== LASER FIELDS SECTION
 !            (E,B) BC in open boundaries (lowest order Yee method
 !==========================================
 subroutine bf_bds(ef,i1,n1p,j1,n2p,k1,n3p,dt_loc,imbd)

 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,imbd
 real(dp),intent(in) :: dt_loc

 integer :: i,j,k,ii
 real(dp) :: aphx,aphy,aphz

 !=================
 ! Enter bf(4:6)=[Bx,By,Bz]
 !============================
 !=========Hegquist-Majda ABC (=>> Mur)=====================
 !===============
 ! Ey+Bz are right-moving
 !at x=0 minim. reflection (d/dt-d/dx)^{p-1}(Ey+Bz)=0
 ! first order p=1 Bz=-Ey at x=0 and equal time
 ! Ey-Bz are left-moving
 !at x=L minim. reflection (d/dt+d/dx)^{p-1}(Ey-Bz)=0
 ! first order p=1 Bz=Ey at x=L and equal time
 !============================
 ! B[i1,n1p]=> extended to [i1-1,n1p]
 ! boundaries for E_t=rotB
 !========================
 ! aphx centered as Ey at ii=1
 if(pe0x)then
  if(ibx <2)then
   aphx=loc_xg(1,3,imodx)*dx_inv*dt_loc
   do k=k1,n3p
    do j=j1,n2p
     ef(i1-1,j,k,nfield)=-&
      (2.*ef(i1,j,k,2)+(1.-aphx)*ef(i1,j,k,nfield))/(1.+aphx)
    end do
   end do
   if(nfield>3)then
    !==========================
    !at x=0 minim. reflection (d/dt-d/dx)^{p-1}(Ez-By)=0
    ! first order p=1 By=Ez at x=0 and equal time
    !==========================
    ii=1
    do k=k1,n3p
     do j=j1,n2p
      ef(i1-1,j,k,5)=&
       (2.*ef(i1,j,k,3)-(1.-aphx)*ef(i1,j,k,5))/(1.+aphx)
     end do
    end do
   endif
  endif
 endif
 if(ndim<2)return
 !------------------------------------
 !++++++++++++++++++++++++++++++++++++++ (Bz,Ex)
 !at y=-Ly minim. reflection (d/dt-d/dy)^{p-1}(Ex-Bz)=0
 ! first order p=1 Bz=Ex at y=-Ly and equal time
 !========================
 !==============================
 ! aphy centered as Ex j=1 (the Bz derivative)
 ii=1
 if(iby < 2)then
  if(pe0y)then
   select case(imbd)
   case(0)
    aphy=loc_yg(ii,3,imody)*dy_inv*dt_loc
    do k=k1,n3p
     do i=i1,n1p
      ef(i,j1-1,k,nfield)=2.*ef(i,j1,k,1)-(1.-aphy)*ef(i,j1,k,nfield)
      ef(i,j1-1,k,nfield)=ef(i,j1-1,k,nfield)/(1.+aphy)
     end do
    end do
    if(nfield>3)then
     !================================== (Bx,Ez)
     !at y=-Ly minim. reflection (d/dt-d/dy)^{p-1}(Ez+Bx)=0
     ! first order p=1 Bx=-Ez at y=-Ly and equal time
     !==========================================
     ii=-1
     do k=k1,n3p
      do i=i1,n1p
       ef(i,j1-1,k,4)=-2.*ef(i,j1,k,3)-(1.-aphy)*ef(i,j1,k,4)
       ef(i,j1-1,k,4)=ef(i,j1-1,k,4)/(1.+aphy)
      end do
     end do
    endif
   case(1)        !symmetric bds for (Bz,Bx) at ymin
    do k=k1,n3p
     do i=i1-1,n1p
      ef(i,j1-1,k,nfield)=ef(i,j1,k,nfield)
      !ef(i,j1-1,k,nfield)=2.*ef(i,j1,k,nfield)-ef(i,j1+1,k,nfield)
     end do
    end do
    if(nfield>3)then
     do k=k1,n3p
      do i=i1,n1p
       ef(i,j1-1,k,4)=ef(i,j1,k,4)
       !ef(i,j1-1,k,4)=2.*ef(i,j1,k,4)-ef(i,j1+1,k,4)
      end do
     end do
    endif
   end select
  endif
 endif
 if(ndim <3)return
 !at z=-Lz minim. reflection (d/dt-d/dz)^{p-1}(Bx-Ey)=0
 ! first order p=1 Bx=Ey at z=-Lz and equal time
 !at z=-Lz minim. reflection (d/dt-d/dz)^{p-1}(By+Ex)=0
 ! first order p=1 By=-Ex at z=-Lz and equal time
 !==============================
 ii=1
 if(ibz <2)then
  if(pe0z)then
   select case(imbd)
   case(0)
    aphz=loc_zg(ii,3,imodz)*dz_inv*dt_loc
    do j=j1,n2p
     do i=i1,n1p
      ef(i,j,k1-1,4)=2.*ef(i,j,k1,2)-(1.-aphz)*ef(i,j,k1,4)
      ef(i,j,k1-1,4)=ef(i,j,k1-1,4)/(1.+aphz)
      ef(i,j,k1-1,5)=-2.*ef(i,j,k1,1)-(1.-aphz)*ef(i,j,k1,5)
      ef(i,j,k1-1,5)=ef(i,j,k1-1,5)/(1.+aphz)
     end do
    end do
   case(1)        !symmetric bds for (Nx,By) at zmin
    do j=j1,n2p
     do i=i1,n1p
      ef(i,j,k1-1,4)=ef(i,j,k1,4)
      ef(i,j,k1-1,5)=ef(i,j,k1,5)
      !ef(i,j,k1-1,4)=2.*ef(i,j,k1,4)-ef(i,j,k1+1,4)
      !ef(i,j,k1-1,5)=2.*ef(i,j,k1,5)-ef(i,j,k1+1,5)
     end do
    end do
   end select
  endif
 endif
 end subroutine bf_bds
 !====================================
 subroutine ef_bds(ef,i1,n1p,j1,n2p,k1,n3p,dt_loc,imbd)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,imbd
 real(dp),intent(in) :: dt_loc
 integer :: i,j,k,ii
 real(dp) :: aphx,aphy,aphz

 aphx=1
 aphy=1
 aphz=1

 ! Enter ebf(1:3)=[Ex,Ey,Ez]
 ! DATA: ef[1:n1p][1:n2p+1][1:n3p+1] bds are on the right
 !===============
 ! to be used to advance B_t=-rot(E)
 !=========Hegquist-Majda ABC (=>> Mur)=====================
 !===============
 ! Ey+Bz are right-moving, Ey-Bz left-moving
 !at x=0 minim. reflection (d/dt-d/dx)^{p-1}(Ey+Bz)=0
 ! first order p=1 Ey=-Bz at x=0
 !at x=Lx minim. reflection (d/dt+d/dx)^{p-1}(Ey-Bz)=0
 ! first order p=1 Ey=Bz at x=L and equal time level
 !=====================
 ! aphx centered as Bz nx+1/2
 ii=nx
 if(ibx <2)then
  if(pe1x)then
   ii=loc_xgrid(imodx)%ng
   select case(ibx)
   case(0)
    aphx=loc_xg(ii,4,imodx)*dx_inv*dt_loc
    do k=k1,n3p
     do j=j1,n2p
      ef(n1p+1,j,k,2)=&
       (2.*ef(n1p,j,k,nfield)-(1.-aphx)*ef(n1p,j,k,2))/(1.+aphx)
     end do
    end do
   case(1)   !reflecting  only on the right boundary: (Ey,Ez, By,Bz) symmetric (continuous)
    do k=k1,n3p
     do j=j1,n2p
      ef(n1p+1,j,k,2)=ef(n1p,j,k,2)
      !ef(n1p+1,j,k,2)=2.*ef(n1p,j,k,2)-ef(n1p-1,j,k,2)
     end do
    end do
   end select
   if(nfield>3)then
    !====================
    !at x=Lx minim. reflection (d/dt+d/dx)^{p-1}(Ez+By)=0
    ! first order p=1 Ez=-Bz at x=L and equal time level
    !===========================
    select case(ibx)
    case(0)
     do k=k1,n3p
      do j=j1,n2p
       ef(n1p+1,j,k,3)=-&
        (2.*ef(n1p,j,k,5)+(1.-aphx)*ef(n1p,j,k,3))/(1.+aphx)
      end do
     end do
    case(1)   !reflecting
     do k=k1,n3p
      do j=j1,n2p
       ef(n1p+1,j,k,3)=ef(n1p,j,k,3)
       !ef(n1p+1,j,k,3)=2.*ef(n1p,j,k,3)-ef(n1p-1,j,k,3)
      end do
     end do
    end select
   endif
  endif
 endif
 !------------------------------------
 if(ndim<2)return
 !++++++++++++++++++++++++++++++++++++++ (Bz,Ex)
 !at y=Ly minim. reflection (d/dt+d/dy)^{p-1}(Ex+Bz)=0
 ! first order p=1 Ex=-Bz at y=Ly and equal time level
 !========================
 ! aphy centered as Bz field ny+1/2
 if(iby < 2)then
  if(pe1y)then
   select case(imbd)
   case(0)
    ii=loc_ygrid(imody)%ng
    aphy=loc_yg(ii,4,imody)*dy_inv*dt_loc
    do k=k1,n3p
     do i=i1,n1p
      ef(i,n2p+1,k,1)=-&
       (2.*ef(i,n2p,k,nfield)+(1.-aphy)*ef(i,n2p,k,1))/(1.+aphy)
     end do
    end do
    if(nfield>3)then
     !++++++++++++++++++++++++++++++++++++++ (Bz,Ex)
     !at y=Ly minim. reflection (d/dt+d/dy)^{p-1}(Ez-Bx)=0
     ! first order p=1 Ez=Bx at y=Ly and equal time level
     !================================
     do k=k1,n3p
      do i=i1,n1p
       ef(i,n2p+1,k,3)=&
        (2.*ef(i,n2p,k,4)-(1.-aphy)*ef(i,n2p,k,3))/(1.+aphy)
      end do
     end do
    endif
   case(1)         !symmetric bds for (Ex,Ez) at ymax boundary
    do k=k1,n3p
     do i=i1,n1p
      ef(i,n2p+1,k,1)=ef(i,n2p,k,1)
      !ef(i,n2p+1,k,1)=2.*ef(i,n2p,k,1)-ef(i,n2p-1,k,1)
     end do
    end do
    if(nfield>3)then
     do k=k1,n3p
      do i=i1,n1p
       ef(i,n2p+1,k,3)=ef(i,n2p,k,3)
       !ef(i,n2p+1,k,3)=2.*ef(i,n2p,k,3)-ef(i,n2p-1,k,3)
      end do
     end do
    endif
   end select
  endif
 endif
 !==============================
 if(ndim <3)return
 !==============================
 !at z=Lz minim. reflection
 ! (d/dt+d/dz)^{p-1}(Ex-By)=0
 ! first order p=1 Ex=By at z=Lz and equal time level
 ! (d/dt+d/dz)^{p-1}(Ey+Bx)=0
 ! first order p=1 Ey=-Bx at z=Lz and equal time level
 !================================
 !========================================
 ! aphz centered as Bx,By at nz+1/2
 if(ibz <2)then
  if(pe1z)then
   select case(imbd)
   case(0)
    ii=loc_zgrid(imodz)%ng
    aphz=loc_zg(ii,4,imodz)*dz_inv*dt_loc
    do j=j1,n2p
     do i=i1,n1p
      ef(i,j,n3p+1,1)=&
       (2.*ef(i,j,n3p,5)-(1.-aphz)*ef(i,j,n3p,1))/(1.+aphz)
      ef(i,j,n3p+1,2)=-&
       (2.*ef(i,j,n3p,4)+(1.-aphz)*ef(i,j,n3p,2))/(1.+aphz)
     end do
    end do
   case(1)     !symmetric bds for (Ex,Ey) at zmax boundary
    do j=j1,n2p
     do i=i1,n1p
      ef(i,j,n3p+1,1)=ef(i,j,n3p,1)
      ef(i,j,n3p+1,2)=ef(i,j,n3p,2)
      !ef(i,j,n3p+1,1)=2.*ef(i,j,n3p,1)-ef(i,j,n3p-1,1)
      !ef(i,j,n3p+1,2)=2.*ef(i,j,n3p,2)-ef(i,j,n3p-1,2)
     end do
    end do
   end select
  endif
 endif
 end subroutine ef_bds
 !=========================================
 subroutine potential_lapl(apf,curr,ic1,ic2,dord,i1,n1p,j1,n2p,k1,n3p,&
                                                      dhx,dhy,dhz)
 real(dp),intent(inout)  :: apf(:,:,:,:),curr(:,:,:,:)

 integer,intent(in):: ic1,ic2,dord,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: dhx,dhy,dhz
 integer :: i,j,k,ic,i01,i02
 real(dp) :: dx2,cf(2),dx4(2)
 !Computes the Laplacian(apf) and accumulates on the source array curr
 !                 curr=laplcian(apf)+curr
 !========================================
 dx2=dhx*dhx       !1/(dx*dx)
 i01=i1
 i02=n1p
 !2============= ALL FIELDS ic=ic1,ic2
 cf(1)=1.
 cf(2)=0.0
 ! Holds opt-second order or fourth order
 ! for second derivative with: 
 ! dord=3  hord_der2=-(1-nu*nu)/12  dord=4 hord_der2=-1/12    
 if(dord > 2)then        
  cf(1)=1.-4.*hord_der2
  cf(2)=hord_der2
 endif
 dx4(1)=cf(1)*dx2
 dx4(2)=cf(2)*dx2
 if(pe0x)then
  i=i1
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     apf(i-1,j,k,ic)=apf(i,j,k,ic)
     curr(i,j,k,ic)=curr(i,j,k,ic)+dx2*(apf(i+1,j,k,ic)+apf(i-1,j,k,ic)-&
                                        2.*apf(i,j,k,ic))
    enddo
   end do
  end do
  i01=i1+1
  if(dord >2)then
   i=i1+1
   do ic=ic1,ic2
    do k=k1,n3p
     do j=j1,n2p
      curr(i,j,k,ic)=curr(i,j,k,ic)+dx2*(apf(i+1,j,k,ic)+apf(i-1,j,k,ic)-&
                                        2.*apf(i,j,k,ic))
     enddo
    end do
   end do
   i01=i1+2
  endif
 endif
 if(pe1x)then
  i=n1p
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     apf(i+1,j,k,ic)=apf(i,j,k,ic)
     curr(i,j,k,ic)=curr(i,j,k,ic)+dx2*(apf(i+1,j,k,ic)+apf(i-1,j,k,ic)-&
                                        2.*apf(i,j,k,ic))
    enddo
   end do
  end do
  i02=n1p-1
  if(dord >2)then
   i=n1p-1
   do ic=ic1,ic2
    do k=k1,n3p
     do j=j1,n2p
      curr(i,j,k,ic)=curr(i,j,k,ic)+dx2*(apf(i+1,j,k,ic)+apf(i-1,j,k,ic)-&
                                        2.*apf(i,j,k,ic))
     enddo
    end do
   end do
   i02=n1p-2
  endif
 endif
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j1,n2p
    do i=i01,i02
     curr(i,j,k,ic)=curr(i,j,k,ic)+dx4(1)*(&
                 apf(i+1,j,k,ic)+apf(i-1,j,k,ic)-2.*apf(i,j,k,ic))
    end do
   end do
  end do
 end do
 if(dord> 2)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     do i=i01,i02
      curr(i,j,k,ic)=curr(i,j,k,ic)+dx4(2)*(&
                 apf(i+2,j,k,ic)+apf(i-2,j,k,ic)-2.*apf(i,j,k,ic))

     end do
    end do
   end do
  end do
 endif
 if(ndim >1)call pp_lapl(&
                   apf,curr,ic1,ic2,dord,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)
 end subroutine potential_lapl
 !===========================
 subroutine rotE(ef,i1,n1p,j1,j2,k1,k2,aphx,aphy,aphz)

 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,j2,k1,k2
 real(dp),intent(in) :: aphx,aphy,aphz
 real(dp) :: sdhy,sdhz
 integer :: i,j,k,jj,kk
 real(dp) :: aph1,aph2

 ! Enter ef(1:3)=[Ex,Ey,Ez], ef(4:6)=[Bx,By,Bz]
 ! SOLVES B=B-DT*rot[E]
 ! enter boundary fields
 !==================== B=B-dt*rot(E) interior domain==========
 aph1=aphx*se_coeff(1)
 aph2=aphx*se_coeff(2)
 !============================
 if(ndim==1)then
  k=1;j=1
  do i=i1,n1p
   ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
                    aph1*(ef(i+1,j,k,2)-ef(i,j,k,2))
  end do
  do i=i1+1,n1p-1
   ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
                    aph2*(ef(i+2,j,k,2)-ef(i-1,j,k,2))
  end do
  return
 endif
 !=================================
 do k=k1,k2
  do j=j1,j2
   jj=j-2
   sdhy=loc_yg(jj,4,imody)*aphy
   do i=i1,n1p
    ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
        aph1*(ef(i+1,j,k,2)-ef(i,j,k,2))+&
        sdhy*(ef(i,j+1,k,1)-ef(i,j,k,1))
   end do
   do i=i1+1,n1p-1
    ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
     aph2*(ef(i+2,j,k,2)-ef(i-1,j,k,2))
   end do
  end do
 end do
 if(nfield <6)return
 if(ndim==3)then
  do k=k1,k2
   kk=k-2
   sdhz=loc_zg(kk,4,imodz)*aphz
   do j=j1,j2
    jj=j-2
    sdhy=loc_yg(jj,4,imody)*aphy
    do i=i1,n1p
     ef(i,j,k,4)=ef(i,j,k,4)-sdhy*(ef(i,j+1,k,3)-ef(i,j,k,3))+&
      sdhz*(ef(i,j,k+1,2)-ef(i,j,k,2))
     ef(i,j,k,5)=ef(i,j,k,5)+&
      aph1*(ef(i+1,j,k,3)-ef(i,j,k,3))-&
      sdhz*(ef(i,j,k+1,1)-ef(i,j,k,1))
    end do
    do i=i1+1,n1p-1
     ef(i,j,k,5)=ef(i,j,k,5)+&
      aph2*(ef(i+2,j,k,3)-ef(i-1,j,k,3))
    end do
   end do
  end do
 else
  k=1
  do j=j1,j2
   jj=j-2
   sdhy=loc_yg(jj,4,imody)*aphy
   do i=i1,n1p
    ef(i,j,k,4)=ef(i,j,k,4)-sdhy*(ef(i,j+1,k,3)-ef(i,j,k,3))
    ef(i,j,k,5)=ef(i,j,k,5)+aph1*(ef(i+1,j,k,3)-ef(i,j,k,3))
   end do
   do i=i1+1,n1p-1
    ef(i,j,k,5)=ef(i,j,k,5)+&
     aph2*(ef(i+2,j,k,3)-ef(i-1,j,k,3))
   end do
  end do
 endif
 !================== interior domains
 end subroutine rotE
 !===============================
 subroutine rotB(ef,i1,n1p,j1,j2,k1,k2,aphx,aphy,aphz)

 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,j2,k1,k2
 real(dp),intent(in) :: aphx,aphy,aphz
 real(dp) :: sdy,sdz,aph1,aph2
 integer :: i,j,k,ii,jj,kk
 ! E=E+DT*rot[B]          Two-point Second order derivatives
 !==================== B=B-dt*rot(E) interior domain==========
 ! enter boundary fields
 !=================== interior domains
 aph1=aphx*se_coeff(1)
 aph2=aphx*se_coeff(2)
 if(ndim==1)then
  k=1;j=1
  do i=i1,n1p
   ef(i,j,k,2)=ef(i,j,k,2)-aphx*(ef(i,j,k,nfield)-ef(i-1,j,k,nfield))
  end do
 endif
 !=========================== NDIM > 1
 do k=k1,k2
  do j=j1,j2
   jj=j-2
   sdy=loc_yg(jj,3,imody)*aphy
   do i=i1,n1p
    ii=i-2
    ef(i,j,k,1)=ef(i,j,k,1)+sdy*(ef(i,j,k,nfield)-ef(i,j-1,k,nfield))
    ef(i,j,k,2)=ef(i,j,k,2)-aph1*(ef(i,j,k,nfield)-ef(i-1,j,k,nfield))
   end do
   do i=i1+2,n1p-1
    ii=i-2
    ef(i,j,k,2)=ef(i,j,k,2)-&
     aph2*(ef(i+1,j,k,nfield)-ef(i-2,j,k,nfield))
   end do
  end do
 end do
 if(nfield <6)return
 if(ndim==3)then
  do k=k1,k2
   kk=k-2
   sdz=aphz*loc_zg(kk,3,imodz)
   do j=j1,j2
    jj=j-2
    sdy=aphy*loc_yg(jj,3,imody)
    do i=i1,n1p
     ii=i-2
     ef(i,j,k,1)=ef(i,j,k,1)-&
                sdz*(ef(i,j,k,5)-ef(i,j,k-1,5))
     ef(i,j,k,2)=ef(i,j,k,2)+sdz*(ef(i,j,k,4)-ef(i,j,k-1,4))
     ef(i,j,k,3)=ef(i,j,k,3)+&
                aph1*(ef(i,j,k,5)-ef(i-1,j,k,5))-&
                sdy*(ef(i,j,k,4)-ef(i,j-1,k,4))
    end do
    do i=i1+2,n1p-1
     ii=i-2
     ef(i,j,k,3)=ef(i,j,k,3)+&
                aph2*(ef(i+1,j,k,5)-ef(i-2,j,k,5))
    end do
   end do
  end do
 else
  k=1
  do j=j1,j2
   jj=j-2
   sdy=aphy*loc_yg(jj,3,imody)
   do i=i1,n1p
    ii=i-2
    ef(i,j,k,3)=ef(i,j,k,3)+&
     aph1*(ef(i,j,k,5)-ef(i-1,j,k,5))-&
     sdy*(ef(i,j,k,4)-ef(i,j-1,k,4))
   end do
   do i=i1+2,n1p-1
    ii=i-2
    ef(i,j,k,3)=ef(i,j,k,3)+&
                aph2*(ef(i+1,j,k,5)-ef(i-2,j,k,5))
   end do
  end do
 endif
 end subroutine rotB
 !=====================================
!=================================  
 subroutine nc_fluid_density_momenta(flx,ef,i1,n1p,j1,n2p,k1,n3p,fcomp,aphx,aphy,aphz)
  real(dp),intent(in) :: flx(:,:,:,:)
  real(dp),intent(inout) :: ef(:,:,:,:)
  integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,fcomp
  integer(kind=4) :: flux_ind
  real(dp),intent(in) :: aphx,aphy,aphz
  integer :: i,j,k,ic,j01,j02,k01,k02
  real(dp) :: shy,shz
  real(dp) :: dw(3),sl(2),sr(2),omgl(2),vv,s0
  real(dp),parameter :: eps=1.e-06
  real(dp),dimension(2),parameter :: w03=(/1./3.,2./3./)
  real(dp),dimension(3),parameter :: lder=(/0.5,-2.,1.5/)
  real(dp),dimension(3),parameter :: rder=(/-1.5,2.,-0.5/)
  real(dp),dimension(4),parameter :: lder4=(/1./6.,-1.,0.5,1./3./)  ![i-2,i+1] stencil
  real(dp),dimension(4),parameter :: rder4=(/-1./3.,-0.5,1.,-1./6./)![i-1,i+2] stencil
!=========================
! Enter primitive variables in flux array flx(Px,Py,Pz,den,vx,vy,vz)
! flcomp=fcomp+ndim components
  flux_ind=1                         !=1 for pure upwind   
                                           !=2 for LxF fluxin density equation
  j01=j1
  if(yl_bd)j01=j1+2
  j02=n2p
  if(yr_bd)j02=n2p-2
  k01=k1
  if(zl_bd)k01=k1+2
  k02=n3p
  if(zr_bd)k02=n3p-2
!===========================
! momenta-density
  do k=k1,n3p
   do j=j1,n2p
    do ic=1,fcomp+1
     do i=i1,n1p
      var(i,ic)=flx(i,j,k,ic)
     end do
    end do
   call weno3_nc(fcomp+1,i1,n1p,xl_bd,xr_bd) 
   do ic=1,fcomp               !var=momenta
    do i=i1,n1p
     ef(i,j,k,ic)=ef(i,j,k,ic)+aphx*ww0(i,ic)
    end do
   end do
  end do
 end do
!====================
  do k=k1,n3p
   do i=i1,n1p
    do ic=1,fcomp
     do j=j01-2,j02+2            !Extended range[j1-2,n2p+2] in interior domains
      var(j,ic)=flx(i,j,k,ic)
     end do
    end do
    do j=j01-2,j02+2
     var(j,fcomp+1)=flx(i,j,k,fcomp+2)
    end do
    call weno3_nc(fcomp+1,j01-2,j02+2,yl_bd,yr_bd)    !rec[flux][j01-1,j02+1] 
    do ic=1,fcomp
     do j=j01,j02
      shy=aphy*loc_yg(j-2,3,imody)
      ef(i,j,k,ic)=ef(i,j,k,ic)+shy*ww0(j,ic)
     end do
    end do
   end do
  end do
  if(ndim <3)return
  do j=j1,n2p
   do i=i1,n1p
    do ic=1,fcomp
     do k=k01-2,k02+2
      var(k,ic)=flx(i,j,k,ic)
     end do
    end do
    ic=fcomp+1
    do k=k01-2,k02+2
     var(k,ic)=flx(i,j,k,ic+2)
    end do
    call weno3_nc(fcomp+1,k01-2,k02+2,zl_bd,zr_bd)   
    do ic=1,fcomp-1
     do k=k01,k02
      shz=aphz*loc_zg(k-2,3,imodz)
      ef(i,j,k,ic)=ef(i,j,k,ic)+shz*ww0(k,ic)
     end do
    end do
   end do
  end do
!=================================
 contains
 subroutine weno3_nc(nc,i1,np,lbd,rbd)
  integer,intent(in)  :: nc,i1,np
  logical,intent(in) :: lbd,rbd
!  enter data [i1,np]  
  integer :: i,l,ic

!=======ENTER DATA [i1,np]
!wl_{i+1/2}  uses stencil [i-1,i,i+1] in range [i=i1+1,np-1] 
!wr_{i+1/2}  uses stencil [i,i+1,i+2] in range [i=i1,np-2] 
!            common interior points [i1+1,np-2
!            Dw first derivative in range[i1+2,np-2]
!            L-Boundary    Dw^r[i1+1] uses the [i1:i1+3] stencil for v<0
!            R-Boundary    Dw^L[np-1] uses the [np-3:np1] stencil
!===========================================

  ic=nc-1
  do i=i1,np
   var(i,nc+1)=var(i,ic)*var(i,nc)  !in den array var(nc+1) => den*v
  end do
!================= reconstract nc primitives (Px,Py,Pz,Den,V)
  do ic=1,nc
   do i=i1+1,np-1
    dw(1)=var(i,ic)-var(i-1,ic)    !DW_{i-1/2}
    dw(2)=var(i+1,ic)-var(i,ic)    !DW_{i+1/2}
    omgl(1)=1./(dw(1)*dw(1)+eps)
    omgl(2)=1./(dw(2)*dw(2)+eps)
    sl(1)=w03(1)*omgl(1)
    sl(2)=w03(2)*omgl(2)
    sr(1)=w03(2)*omgl(1)
    sr(2)=w03(1)*omgl(2)
    s0=sl(1)+sl(2)
    wl(i,ic)=var(i,ic)+0.5*(dw(1)*sl(1)+dw(2)*sl(2))/s0
    s0=sr(1)+sr(2)
    wr(i-1,ic)=var(i,ic)-0.5*(dw(1)*sr(1)+dw(2)*sr(2))/s0
   end do  
  end do
!===================================
  !upwind boundary derivatives
  if(lbd)then
   do ic=1,nc-2
    i=i1
    ww0(i,ic)=0.0
    vv=var(i,nc)
    if(vv <0.0)ww0(i,ic)=vv*(var(i+1,ic)-var(i,ic))
    i=i1+1
    vv=var(i,nc)
    ww0(i,ic)=vv*(var(i,ic)-var(i-1,ic))
    if(vv <0.0)ww0(i,ic)=vv*dot_product(rder(1:3),var(i:i+2,ic))
   end do
   ic=nc-1
   i=i1
   ww0(i,ic)=0.0
   vv=var(i,nc)
   if(vv <0.0)ww0(i,ic)=var(i+1,nc+1)-var(i,nc+1)
   i=i1+1
   vv=var(i,nc)
   ww0(i,ic)=var(i,nc+1)-var(i-1,nc+1)
   if(vv <0.0)ww0(i,ic)=dot_product(rder(1:3),var(i:i+2,nc+1))
  end if
  if(rbd)then
   do ic=1,nc-2
    i=np-1
    vv=var(i,nc)
    ww0(i,ic)=vv*(var(i+1,ic)-var(i,ic))
    if(vv >0.0)ww0(i,ic)=vv*dot_product(lder(1:3),var(i-2:i,ic))
    i=np
    vv=var(i,nc)
    ww0(i,ic)=0.0
    if(vv >0.0)ww0(i,ic)=vv*(var(i,ic)-var(i-1,ic))
   end do
   ic=nc-1
   i=np-1
   vv=var(i,nc)
   ww0(i,ic)=var(i+1,nc+1)-var(i,nc+1)
   if(vv >0.0)ww0(i,ic)=dot_product(lder(1:3),var(i-2:i,nc+1))
   i=np
   vv=var(i,nc)
   ww0(i,ic)=0.0
   if(vv >0.0)ww0(i,ic)=var(i,nc+1)-var(i-1,nc+1)
  endif
!===================================
!   UPWINDING at interior points
!          Momenta
   do ic=1,nc-2
    do i=i1+1,np-2
     vv=wr(i,nc)+wl(i,nc)
     s0=sign(1.,vv)        !s0=1*sign(vv)
     var(i,ic)=max(0.,s0)*wl(i,ic)-min(0.,s0)*wr(i,ic)
    end do
    do i=i1+2,np-2 
     ww0(i,ic)=var(i,nc)*(var(i,ic)-var(i-1,ic))
    end do
   end do
! LxF flux for density variable
!   F=nv=> 1/2(F_L+F_R)-|V_{max}|(den_R-den_L)]
   ic=nc-1
   do i=i1+1,np-2
    dw(1)=var(i-1,nc)
    dw(2)=var(i,nc)
    dw(3)=var(i+1,nc)
    vv=maxval(abs(dw(1:3)))
    var(i,ic)=wr(i,nc)*wr(i,ic)+wl(i,nc)*wl(i,ic)-vv*(wr(i,ic)-wl(i,ic))
    var(i,ic)=0.5*var(i,ic)
   end do
   do i=i1+2,np-2 
    ww0(i,ic)=var(i,ic)-var(i-1,ic)
   end do
  end subroutine weno3_nc
 end subroutine nc_fluid_density_momenta
!====================================
  end module grid_fields
