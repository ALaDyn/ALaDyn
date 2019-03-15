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

 use precision_def
 use der_lib
 use fstruct_data
 use all_param
 use parallel

 implicit none
 real(dp),allocatable :: ww(:),ww0(:,:),sf1(:),sf2(:),wr(:,:),wl(:,:),var(:,:)
 real(dp) :: sigm_opt
 real(dp) :: mat_env_coeff(5,5)
 logical :: xl_bd,xr_bd,yl_bd,yr_bd,zl_bd,zr_bd
 contains

 subroutine w_alloc(opt_der)
 real(dp),intent(in) :: opt_der
 real(dp) :: cfl_loc,as0,alp
 integer :: ndmx,nd1mx,ng
 ! sets logical bd flags
 xl_bd=.true.
 xr_bd=.true.
 yl_bd=.false.
 yr_bd=.false.
 zl_bd=.false.
 zr_bd=.false.
 if(pe0y)yl_bd=.true.
 if(pe1y)yr_bd=.true.
 if(pe0z)zl_bd=.true.
 if(pe1z)zr_bd=.true.

 !============================
 ! allocate auxiliary arrays ww() dw() and
 ndmx=max(nx,ny,nz)
 nd1mx=max(nx_loc,nx1_loc)
 allocate(ww(ndmx+5),ww0(ndmx+5,5+max(ny_loc,nz_loc)))
 allocate(wr(ndmx+6,10),wl(ndmx+6,10))
 allocate(var(ndmx+5,10))
 var(:,:)=0.0
 wr(:,:)=0.0
 wl(:,:)=0.0
 ww(:)=0.0
 ww0(:,:)=0.0
 cfl_loc=0.0
 ng=nx

 if(LPf_ord >0)then
  cfl_loc=cfl
  if(ndim >1)cfl_loc=cfl*yx_rat/sqrt(yx_rat*yx_rat+real(ndim)-1.)
 endif
 call set_mat_der(cfl_loc,opt_der,nx,ny,nz,ndim,&
                  ibx,der_ord,ifilt,iform,Comoving)
!============= k-space data for fft
  allocate(sf1(nx/2+1),sf2(nx/2+1))
  if(Envelope)then
   allocate(mat_env(ng,5))
   alp=dt*oml
   as0=dt*dx_inv
   call set_env5_coeff(alp,as0)
   call set_mat_env5(mat_env_coeff,ng)
  endif
 !============================
 end subroutine w_alloc
 !====================================
 subroutine set_env5_coeff(ap,a)
  real(dp),intent(in) :: ap,a
  real(dp) :: ap2,a2
  ap2=ap*ap
  a2=a*a
!======== The discrete operator 1+(dt*k0)*(dt*k0)+dt*dtD_xD_x-2*dt*D_x
!   First row
  mat_env_coeff(1,1)=1.+ap2+2*a+0.5*a2
  mat_env_coeff(1,2)=-a2-2.*a
  mat_env_coeff(1,3)= 0.5*a2

!   second row
  mat_env_coeff(2,1)=0.5*a2+a
  mat_env_coeff(2,2)=1.+ap2+0.25*a2+a
  mat_env_coeff(2,3)=-a
  mat_env_coeff(2,4)=0.25*a2

!   interior rows
  mat_env_coeff(3,1)=0.25*a2
  mat_env_coeff(3,2)=a
  mat_env_coeff(3,3)=1.+ap2-0.5*a2
  mat_env_coeff(3,4)=-a
  mat_env_coeff(3,5)=0.25*a2
!   n-1-th row
  mat_env_coeff(4,1)=0.25*a2
  mat_env_coeff(4,2)=a
  mat_env_coeff(4,3)=-0.5*a2
  mat_env_coeff(4,4)=1.+ap2+0.25*a2-a
!   n-th row
  mat_env_coeff(5,1)=0.25*a2
  mat_env_coeff(5,2)=a-0.5*a2
  mat_env_coeff(5,3)=1+ap2-a
!=============================
 end subroutine set_env5_coeff
!====================
 subroutine matenv_inv(n,nc)
 integer,intent(in) :: n,nc
 integer :: ix,ic
 ! Invert a pentadiagonal matrix
 ! mat_env(n,5) LU factorized in set_mat_env5()

 do ic=1,nc
  ix=1
  ww0(ix,ic)=mat_env(ix,3)*ww0(ix,ic)
  ix=2
  ww0(ix,ic)=ww0(ix,ic)-mat_env(ix,2)*ww0(ix-1,ic)
  ww0(ix,ic)=mat_env(ix,3)*ww0(ix,ic)
  do ix=3,n
   ww0(ix,ic)=ww0(ix,ic)-mat_env(ix,1)*ww0(ix-2,ic)-&
    mat_env(ix,2)*ww0(ix-1,ic)
   ww0(ix,ic)=mat_env(ix,3)*ww0(ix,ic)
  end do
  ix=n-1
  ww0(ix,ic)=ww0(ix,ic)-mat_env(ix,4)*ww0(ix+1,ic)
  do ix=n-2,1,-1
   ww0(ix,ic)=ww0(ix,ic)-mat_env(ix,4)*ww0(ix+1,ic)-&
    mat_env(ix,5)*ww0(ix+2,ic)
  end do
 end do
 end subroutine matenv_inv
 !=======================
 subroutine trid2_loc(bd,b1,c1,bn,an,n,nc)
 real(dp),intent(in) :: bd,b1,c1,bn,an
 integer,intent(in) :: n,nc
 integer :: k,ic
 real(dp) :: bet
 !==========================
 do ic=1,nc
  dw(1)=0.0
  bet=b1
  ww0(1,ic)=ww0(1,ic)-ww0(2,ic)
  ww0(n,ic)=ww0(n,ic)-ww0(n-1,ic)
  ww0(1,ic)=ww0(1,ic)/bet
  k=2
  dw(k)=c1/bet
  bet=bd-dw(k)
  ww0(k,ic)=(ww0(k,ic)-ww0(k-1,ic))/bet
  do k=3,n-1
   dw(k)=1./bet
   bet=bd-dw(k)
   ww0(k,ic)=(ww0(k,ic)-ww0(k-1,ic))/bet
  end do
  k=n
  dw(k)=1./bet
  bet=bn-an*dw(k)
  ww0(k,ic)=(ww0(k,ic)-an*ww0(k-1,ic))/bet
  do k=n-1,1,-1
   ww0(k,ic)=ww0(k,ic)-dw(k+1)*ww0(k+1,ic)
  end do
 end do
 end subroutine trid2_loc
 !===========================
 subroutine trid_odd_even_inv(a,b,c,b1,an,bn,n,ic1,ic2)
 ! subroutine trid_odd_even_inv(a,b,c,b1,c1,an,bn,n,ic1,ic2)
 real(dp),intent(in) :: a,b,c,b1,an,bn
 ! real(dp),intent(in) :: c1
 integer,intent(in) :: n,ic1,ic2
 integer :: k,ic,nh,i1,i2
 real(dp) :: bet
 !==========================
 ! Solves
 ! a*ww(i-1)+b*ww(i)+c*ww(i+1)=u(i), i=2,3,..,n-1
 ! first order boundary clusure
 !===============================
 nh=n/2
 do ic=ic1,ic2
  k=1
  i2=2*k
  i1=i2-1
  dw(k)=0.0
  bet=b1
  ww0(i1,ic)=ww0(i1,ic)/bet
  ww0(i2,ic)=ww0(i2,ic)/bet
  do k=2,nh-1
   i2=2*k
   i1=i2-1
   dw(k)=c/bet
   bet=b-a*dw(k)
   ww0(i1,ic)=(ww0(i1,ic)-a*ww0(i1-2,ic))/bet
   ww0(i2,ic)=(ww0(i2,ic)-a*ww0(i2-2,ic))/bet
  end do
  k=nh
  i2=2*k
  i1=i2-1
  dw(k)=c/bet
  bet=bn-an*dw(k)
  ww0(i1,ic)=(ww0(i1,ic)-an*ww0(i1-2,ic))/bet
  ww0(i2,ic)=(ww0(i2,ic)-an*ww0(i2-2,ic))/bet
  do k=nh-1,1,-1
   i2=2*k
   i1=i2-1
   ww0(i1,ic)=ww0(i1,ic)-dw(k+1)*ww0(i1+2,ic)
   ww0(i2,ic)=ww0(i2,ic)-dw(k+1)*ww0(i2+2,ic)
  end do
 end do
 end subroutine trid_odd_even_inv
 !==================================
 subroutine cycle_trid_inv(a,b,c,n)
 real(dp),intent(in) :: a,b,c
 integer,intent(in) :: n
 integer :: k,ic,ic1
 real(dp) :: b1,bn
 real(dp) :: fact,alp,gm,bet
 !==========================
 ! Solves
 ! a*ww(i-1)+b*ww(i)+c*ww(i+1)=u(i), i=1,2,..,n
 ! for periodic boundary clusure
 !===============================
 alp=c
 bet=a
 gm=-b
 b1=b-gm
 bn=b-alp*bet/gm
 ic=1
 ic1=ic+1
 !============= diagonal b1, b..,bn
 dw(1)=0.0
 bet=b1
 ww0(1,ic)=ww0(1,ic)/bet
 do k=2,n-1
  dw(k)=c/bet
  bet=b-a*dw(k)
  ww0(k,ic)=(ww0(k,ic)-a*ww0(k-1,ic))/bet
 end do
 k=n
 dw(k)=c/bet
 bet=bn-a*dw(k)
 ww0(k,ic)=(ww0(k,ic)-a*ww0(k-1,ic))/bet
 do k=n-1,1,-1
  ww0(k,ic)=ww0(k,ic)-dw(k+1)*ww0(k+1,ic)
 end do
!----------------------
 ww0(1,ic1)=gm
 ww0(n,ic1)=alp
 ww0(2:n-1,ic1)=0.0
 bet=b1
 ww0(1,ic1)=ww0(1,ic1)/bet
 do k=2,n-1
  dw(k)=c/bet
  bet=b-a*dw(k)
  ww0(k,ic1)=(ww0(k,ic1)-a*ww0(k-1,ic1))/bet
 end do
 k=n
 dw(k)=c/bet
 bet=bn-a*dw(k)
 ww0(k,ic1)=(ww0(k,ic1)-a*ww0(k-1,ic1))/bet
 do k=n-1,1,-1
  ww0(k,ic1)=ww0(k,ic1)-dw(k+1)*ww0(k+1,ic1)
 end do
 fact=(ww0(1,ic)+bet*ww0(n,ic)/gm)/(1.+ww0(1,ic1)+bet*ww0(n,ic1)/gm)
 ww0(1:n,ic)=ww0(1:n,ic)-fact*ww0(1:n,ic1)
 end subroutine cycle_trid_inv
 !=================================
 subroutine upper_trid(a,b,c,bn,cn,n,ic1,ic2)
 real(dp),intent(in) :: a,b,c,bn,cn
 integer,intent(in) :: n,ic1,ic2
 integer :: k,ic
 !==========================
 ! Solves
 ! a*ww(i)+b*ww(i+1)+c*ww(i+2)=u(i), i=1,3,..,n-2
 ! at the n-1 row bn*ww(n-1)+cn*ww(n)=u(n-1)
 ! at the n row ww(n)=u(n)
 ! first order right boundary clusure
 !===============================
 do ic=ic1,ic2
  k=n-1
  ww0(k,ic)=ww0(k,ic)-cn*ww0(k+1,ic)
  ww0(k,ic)=ww0(k,ic)/bn
  do k=n-2,1,-1
   ww0(k,ic)=ww0(k,ic)-b*ww0(k+1,ic)-c*ww0(k+2,ic)
   ww0(k,ic)=ww0(k,ic)/a
  end do
 end do
 end subroutine upper_trid
 !================
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
  dw(1)=0.0
  bet=b1
  ww0(1,ic)=ww0(1,ic)/bet
  k=2
  dw(k)=c1/bet
  bet=b-a*dw(k)
  ww0(k,ic)=(ww0(k,ic)-a*ww0(k-1,ic))/bet
  do k=3,n-1
   dw(k)=c/bet
   bet=b-a*dw(k)
   ww0(k,ic)=(ww0(k,ic)-a*ww0(k-1,ic))/bet
  end do
  k=n
  dw(k)=c/bet
  bet=bn-an*dw(k)
  ww0(k,ic)=(ww0(k,ic)-an*ww0(k-1,ic))/bet
  do k=n-1,1,-1
   ww0(k,ic)=ww0(k,ic)-dw(k+1)*ww0(k+1,ic)
  end do
 end do
 end subroutine trid_der1
 !================
 subroutine hder_inv(ngm,ib)
 integer,intent(in) :: ngm,ib
 integer :: ix
 !============== ww collocated at half-integers; ww1 collocated at integers
 ! Enter ww(ngm+1) right boundary
 select case(ib)
 case(0)
  dw(1)=ww(1)
  do ix=2,ngm-1
   dw(ix)=avg_cmp*ww(ix)
  end do
  dw(ngm)=ww(ngm)
  if(derinv)call trid_der_inv(ngm,ib)
  ww(ngm)=-ww(ngm+1)
  do ix=ngm-1,1,-1
   ww(ix)=ww(ix+1)-dw(ix+1)
  end do
  !==========================
 case(1)
  dw(0)=ww(ngm+1)    !the current average <J>
  !                    !periodicity conditions
  ww(0)=ww(ngm-1)
  ww(ngm)=ww(1)
  do ix=1,ngm-1
   dw(ix)=avg_cmp*ww(ix)
  end do
  if(derinv)call trid_der_inv(ngm-1,1)
  ww(1)=dw(1)
  do ix=2,ngm-1
   ww(ix)=ww(ix-1)+dw(ix)
  end do
  ww(0)=(dw(0)-sum(ww(1:ngm-1)))/real(ngm-1,dp)
  !ww(i)=DJ_i=J_{i+1/2)-J_{0+1/2}
  !ww(0)=<J>-<DJ>= J_{0+1/2}
  do ix=1,ngm-1
   ww(ix)=ww(ix)+ww(0)
  end do
  ww(ngm)=ww(1)
 end select
 !===========================
 end subroutine hder_inv
 !===================================
 subroutine field_xcomov_advect(ef,i1,n1p,j1,n2p,k1,n3p,ic1,ic2,aphx,v_adv,tsch)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,ic1,ic2,tsch
 real(dp),intent(in) :: aphx,v_adv
 integer :: i,j,k,ii,n1,n1_loc,ic,ind
 real(dp) :: aphx_adv,aphx_adv1,a,b,c,b1,c1,an,bn
 !real(dp),dimension(3),parameter :: rder=(/-3.,4.,-1./)
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
 if(pex0)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(i1-1,j,k,ic)=ef(i1+1,j,k,ic)
    enddo
   end do
  end do
 endif
 if(pex1)then
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
 !real(dp),dimension(3),parameter :: rder=(/-3.,4.,-1./)
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
 if(pex0)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(i1-1,j,k,ic)=ef(i1+1,j,k,ic)
    enddo
   end do
  end do
 endif
 if(pex1)then
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
     aux1(1:n1)=0.0
     do i=i1,n1p
      ii=i-2
      aux1(ii)=ef(i,j,k,ic)-aphx_adv*(&
       ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
     end do
     call all_gather_dpreal(aux1,aux2,3,n1_loc)
     do i=1,n1
      ww0(i,1)=aux2(i)
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
 ! SECTION FOR initial beam fields
 !==================================
 subroutine init_cbeam_pot(ef,i1,n1,j1,n2,g2)
 real(dp),intent(inout) :: ef(:,:,:)
 integer,intent(in) :: i1,n1,j1,n2
 real(dp),intent(in) :: g2
 integer :: i,j,ic,jj,ng1
 real(dp) :: dx2_inv,dy2
 !==================================
 !  Solves Rad[Lapl](pot_0)=rho
 !  Solves Rad[Lapl(pot_1)]=-D^2_x(pot_0) =>> pot=pot_0+pot_1/gam^2
 !===============================
 ! Enter
 ! ef(1) rho(i,j)  => pot(i,j,k)  ef(3)=aux
 dx2_inv=dx_inv*dx_inv
 dy2=dy*dy
 !==========================
 ng1=n2+1-j1
 do j=j1,n2
  do i=i1,n1
   ef(i,j,1)=dy2*ef(i,j,1)
  end do
 end do
 call set_radlpl_mat(rh,r,ng1)
 ic=1
 j=j1
 jj=j-2
 do i=i1,n1
  ef(i,j,ic)=rmat(jj,2)*ef(i,j,ic)
 end do
 do j=j1+1,n2
  jj=j-2
  do i=i1,n1
   ef(i,j,ic)=ef(i,j,ic)-rmat(jj,1)*ef(i,j-1,ic)
   ef(i,j,ic)=rmat(jj,2)*ef(i,j,ic)
  end do
 end do
 do j=n2-1,j1,-1
  jj=j-2
  do i=i1,n1
   ef(i,j,ic)=ef(i,j,ic)-rmat(jj,3)*ef(i,j+1,ic)
  end do
 end do
 ic=3
 !ef(1)=pot_0
 !ef(3)=pot_1
 do j=j1,n2
  ef(n1+1,j,1)=0.0
  ef(i1-1,j,1)=0.0
  do i=i1,n1
   ef(i,j,ic)=dy2*dx2_inv*(ef(i-1,j,1)-2.*ef(i,j,1)+ef(i+1,j,1))
  end do
 end do
 j=j1
 jj=j-2
 do i=i1,n1
  ef(i,j,ic)=rmat(jj,2)*ef(i,j,ic)
 end do
 do j=j1+1,n2
  jj=j-2
  do i=i1,n1
   ef(i,j,ic)=ef(i,j,ic)-rmat(jj,1)*ef(i,j-1,ic)
   ef(i,j,ic)=rmat(jj,2)*ef(i,j,ic)
  end do
 end do
 do j=n2-1,j1,-1
  jj=j-2
  do i=i1,n1
   ef(i,j,ic)=ef(i,j,ic)-rmat(jj,3)*ef(i,j+1,ic)
  end do
 end do
 do j=j1,n2
  do i=i1,n1
   ef(i,j,1)=ef(i,j,1)-ef(i,j,ic)/g2
  end do
 end do
 end subroutine init_cbeam_pot
 !===============================
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
 if(pex1)then
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
  wy,xf0,om0,i1,i2,ycent,zcent)

 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,dt_loc,t_loc,tf,wx,wy,xf0,om0
 integer,intent(in) :: i1,i2
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
 if(ndim < 3)then
  j1=loc_rgrid(imody)%p_ind(1)
  j2=loc_rgrid(imody)%p_ind(2)
  k=1
  do j=j1,j2
   jj=j-2
   yy=loc_rg(jj,1,imody)
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
  wy,xf0,om0,i1,i2,ycent,zcent)

 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,dt_loc,t_loc,tf,wx,wy,xf0,om0
 integer,intent(in) :: i1,i2
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
 if(ndim < 3)then
  j1=loc_rgrid(imody)%p_ind(1)
  j2=loc_rgrid(imody)%p_ind(2)
  k=1
  do j=j1,j2
   jj=j-2
   yy=loc_rg(jj,1,imody)
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
 !============================
 subroutine init_env_filtering(av,i1,n1p,j1,n2p,k1,n3p)

 real(dp),intent(inout) :: av(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 integer :: ii,i,j,k,ic

 do ic=1,4
  do k=k1,n3p
   do j=j1,n2p
    ii=1
    do i=i1+1,n1p-1
     ww0(ii,1)=0.5*av(i,j,k,ic)+0.25*(av(i+1,j,k,ic)+av(i-1,j,k,ic))
     ii=ii+1
    end do
    ii=1
    do i=i1+1,n1p-1
     av(i,j,k,ic)=ww0(ii,1)
     ii=ii+1
    end do
   end do
  end do
 end do
 end subroutine init_env_filtering
!================================
 subroutine env_matrix_inv(lk0,dhx,b_diag,i1,n1p,j1,n2p,k1,n3p)

 real(dp),intent(in) :: lk0,dhx,b_diag
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 integer :: ii,i,j,k,ic
 real(dp) :: dx2_inv,aph2,b_1,c_1,b_n,a_n

 dx2_inv=dhx*dhx
 aph2=1./dx2_inv         !dx*dx
 b_1=b_diag+2.
 c_1=-b_1
 b_n=b_1
 a_n=c_1
 do k=k1,n3p
  do j=j1,n2p
   ii=1
   i=i1
   ww0(ii,1)=dhx*(jc(i+1,j,k,1)-jc(i,j,k,1))+lk0*jc(i,j,k,2)
   ww0(ii,2)=dhx*(jc(i+1,j,k,2)-jc(i,j,k,2))-lk0*jc(i,j,k,1)
   do i=i1+1,n1p-1
    ii=ii+1
    ww0(ii,1)=0.5*dhx*(jc(i+1,j,k,1)-jc(i-1,j,k,1)) +lk0*jc(i,j,k,2)
    ww0(ii,2)=0.5*dhx*(jc(i+1,j,k,2)-jc(i-1,j,k,2)) -lk0*jc(i,j,k,1)
   end do
   ii=ii+1
   i=n1p
   ww0(ii,1)=dhx*(jc(i,j,k,1)-jc(i-1,j,k,1))+lk0*jc(i,j,k,2)
   ww0(ii,2)=dhx*(jc(i,j,k,2)-jc(i-1,j,k,2))-lk0*jc(i,j,k,1)
   call trid2_loc(b_diag,b_1,c_1,b_n,a_n,ii,2)
   ii=1
   do i=i1,n1p
    jc(i,j,k,1)=ww0(ii,1)
    jc(i,j,k,2)=ww0(ii,2)
    ii=ii+1
   end do
  end do
 end do
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     jc(i,j,k,ic)=aph2*jc(i,j,k,ic)
    end do
   end do
  end do
 end do
 end subroutine env_matrix_inv
 !==============
 subroutine pp_lapl(&
 av,source,ic1,ic2,ord,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)

 real(dp),intent(inout) :: av(:,:,:,:)
 real(dp),intent(inout) :: source(:,:,:,:)

 integer,intent(in) :: ic1,ic2,ord,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: dhy,dhz
 integer :: i,j,k,ic,jj,kk,j01,j02,k01,k02
 real(dp) :: dy2_inv,dz2_inv,cf(0:2),shy,shz,sphy,smhy,sphz,smhz
 !==========================
 ! is=1 adds  is=-1 subtracts the laaplacian term
 !--------------------------------------------------
 dy2_inv=dhy*dhy
 dz2_inv=dhz*dhz
 cf(0)=0.0
 cf(1)=1.
 cf(2)=-2.
 if(ord==4)then
  cf(0)=-1./12.     !2.*hord_der2/3
  cf(1)=4./3.       !1-8*hord_der2/3
  cf(2)=-5./2.      !2*(2*hord_der2-1)
 endif
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
    do ic=ic1,ic2
     do k=k1,n3p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+dy2_inv*(&
       av(i,j-1,k,ic)-2.*av(i,j,k,ic)+av(i,j+1,k,ic))
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
    do ic=ic1,ic2
     do k=k1,n3p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+dy2_inv*(&
       av(i,j-1,k,ic)-2.*av(i,j,k,ic)+av(i,j+1,k,ic))
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
    shy=dy2_inv*loc_yg(jj,3,imody)
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
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dy2_inv*(&
      cf(1)*(av(i,j+1,k,ic)+av(i,j-1,k,ic))+cf(2)*av(i,j,k,ic))
     end do
    end do
   end do
  end do
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j01,j02
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dy2_inv*cf(0)*(&
      av(i,j+2,k,ic)+av(i,j-2,k,ic))
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
    do ic=ic1,ic2
     do j=j1,n2p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+dz2_inv*(&
       av(i,j,k-1,ic)-2.*av(i,j,k,ic)+av(i,j,k+1,ic))
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
      sphz*(av(i,j,k,ic)-av(i,j,k-1,ic))-smhz*(av(i,j,k-1,ic)-av(i,j,k-2,ic)))
     end do
    end do
   end do
   k02=n3p-1
   if(der_ord==4)then
    k=n3p-1
    do ic=ic1,ic2
     do j=j1,n2p
      do i=i1,n1p
       source(i,j,k,ic)=source(i,j,k,ic)+dz2_inv*(&
       av(i,j,k-1,ic)-2.*av(i,j,k,ic)+av(i,j,k+1,ic))
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
   shz=dz2_inv*loc_zg(jj,3,imodz)
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
    do j=j1,n2p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dz2_inv*(&
      cf(1)*(av(i,j,k-1,ic)+av(i,j,k+1,ic))+cf(2)*av(i,j,k,ic))
     end do
    end do
   end do
  end do
  do ic=ic1,ic2
   do k=k01+1,k02-1
    do j=j1,n2p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dz2_inv*cf(0)*(&
      av(i,j,k-2,ic)+av(i,j,k+2,ic))
     end do
    end do
   end do
  end do
 endif
 !======================================
 end subroutine pp_lapl
 !========================
 subroutine env_grad_nostag(envg,i1,n1p,j1,n2p,k1,n3p,ider,dhx,dhy,dhz)

 real(dp),intent(inout) :: envg(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,ider
 real(dp),intent(in) :: dhx,dhy,dhz
 integer :: n1,i,j,k,j01,j02,k01,k02
 real(dp) :: ax1,ax2,ay1,ay2,az1,az2
 real(dp),parameter :: a_cd=2./3., b_cd=-1./12.

 !===========central derivatives
 !==========================
 ! Enters envg(1)= |A|^2/2 exit grad|A|^2/2
 !=========================================

 n1=n1p+1-i1
 ax2=dhx*b_cd
 ay2=dhy*b_cd
 az2=dhz*b_cd
 if(ider==2)then
  ax1=0.5*dhx
  ay1=0.5*dhy
  az1=0.5*dhz
 else
  ax1=dhx*a_cd
  ay1=dhy*a_cd
  az1=dhz*a_cd
 endif
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 !================
 if(pex0)then
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    envg(i-1,j,k,1)=2.*envg(i,j,k,1)-envg(i+1,j,k,1)
   end do
  end do
 endif
 if(pex1)then
  do k=k1,n3p
   do j=j1,n2p
    i=n1p
    envg(i+1,j,k,1)=2.*envg(i,j,k,1)-envg(i-1,j,k,1)
   end do
  end do
 endif
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,i1+1
    envg(i,j,k,2)=0.5*dhx*(envg(i+1,j,k,1)-envg(i-1,j,k,1))
   end do
   do i=i1+2,n1p-2
    envg(i,j,k,2)=ax1*(envg(i+1,j,k,1)-envg(i-1,j,k,1)) !centered at x_i
   end do
   do i=n1p-1,n1p
    envg(i,j,k,2)=0.5*dhx*(envg(i+1,j,k,1)-envg(i-1,j,k,1))
   end do
  end do
 end do
 if(ider==4)then
  do k=k1,n3p
   do j=j1,n2p
    do i=i1+2,n1p-2
     envg(i,j,k,2)=envg(i,j,k,2)+ax2*(envg(i+2,j,k,1)-envg(i-2,j,k,1))
    end do
   end do
  end do
 endif
 if(pe1y)then
  j=n2p
  do k=k1,n3p
   do i=i1,n1p
    envg(i,j+1,k,1)=2.*envg(i,j,k,1)-envg(i,j-1,k,1)
    envg(i,j-1,k,3)=0.5*dhy*(envg(i,j,k,1)-envg(i,j-2,k,1))
    envg(i,j,k,3)=0.5*dhy*(envg(i,j+1,k,1)-envg(i,j-1,k,1))
   end do
  end do
  j02=n2p-2
 end if

 if(pe0y)then
  j=j1
   do k=k1,n3p
    do i=i1,n1p
     envg(i,j-1,k,1)=2.*envg(i,j,k,1)-envg(i,j+1,k,1)
     envg(i,j,k,3)=0.5*dhy*(envg(i,j+1,k,1)-envg(i,j-1,k,1))
     envg(i,j+1,k,3)=0.5*dhy*(envg(i,j+2,k,1)-envg(i,j,k,1))
    end do
   end do
  j01=j1+2
 endif
 do k=k1,n3p
  do j=j01,j02
   do i=i1,n1p
    envg(i,j,k,3)=ay1*(envg(i,j+1,k,1)-envg(i,j-1,k,1))
   end do
  end do
 end do
 if(ider==4)then
  do k=k1,n3p
   do j=j01,j02
    do i=i1,n1p
     envg(i,j,k,3)=envg(i,j,k,3)+ay2*(envg(i,j+2,k,1)-envg(i,j-2,k,1))
    end do
   end do
  end do
 endif
 if(ndim ==2)return
 if(pe1z)then
  k=n3p
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k+1,1)=2.*envg(i,j,k,1)-envg(i,j,k-1,1)
    envg(i,j,k-1,4)=0.5*dhz*(envg(i,j,k,1)-envg(i,j,k-2,1))
    envg(i,j,k,4)=0.5*dhz*(envg(i,j,k+1,1)-envg(i,j,k-1,1))
   end do
  end do
  k02=n3p-2
 end if
 if(pe0z)then
  k=k1
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k-1,1)=2.*envg(i,j,k,1)-envg(i,j,k+1,1)
    envg(i,j,k,4)=0.5*dhz*(envg(i,j,k+1,1)-envg(i,j,k-1,1))
    envg(i,j,k+1,4)=0.5*dhz*(envg(i,j,k+2,1)-envg(i,j,k,1))
   end do
  end do
  k01=k1+2
 end if
 !==================
 do k=k01,k02
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k,4)=az1*(envg(i,j,k+1,1)-envg(i,j,k,1))
   end do
  end do
 end do
 if(ider==4)then
  do k=k01,k02
   do j=j1,n2p
    do i=i1,n1p
     envg(i,j,k,4)=envg(i,j,k,4)+az2*(envg(i,j,k+2,1)-envg(i,j,k-1,1))
    end do
   end do
  end do
 endif
 end subroutine env_grad_nostag
 !========================
 subroutine env_grad(envg,i1,n1p,j1,n2p,k1,n3p,ider,dhx,dhy,dhz)

 real(dp),intent(inout) :: envg(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,ider
 real(dp),intent(in) :: dhx,dhy,dhz
 integer :: n1,i,j,k,j01,j02,k01,k02
 real(dp) :: ax1,ax2,ay1,ay2,az1,az2,shz,shy
 real(dp),parameter :: a_hcd=9./8., b_hcd=-1./24.
 !===========fourth order central flux derivatives

 !==========================
 ! Enters envg(1)= |A|^2/2 exit grad|A|^2/2

 n1=n1p+1-i1
 ax2=dhx*b_hcd
 ay2=dhy*b_hcd
 az2=dhz*b_hcd
 if(ider==2)then
  ax1=dhx
  ay1=dhy
  az1=dhz
 else
  ax1=dhx*a_hcd
  ay1=dhy*a_hcd
  az1=dhz*a_hcd
 endif
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 !================
 if(pex0)then
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    envg(i-1,j,k,1)=2.*envg(i,j,k,1)-envg(i+1,j,k,1)
   end do
  end do
 endif
 if(pex1)then
  do k=k1,n3p
   do j=j1,n2p
    i=n1p
    envg(i+1,j,k,1)=2.*envg(i,j,k,1)-envg(i-1,j,k,1)
   end do
  end do
 endif
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,n1p-1
    envg(i,j,k,2)=ax1*(envg(i+1,j,k,1)-envg(i,j,k,1)) !at i+1/2
   end do
   i=n1p
   envg(i,j,k,2)=dhx*(envg(i+1,j,k,1)-envg(i,j,k,1))
  end do
 end do
 if(ider==4)then
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    envg(i,j,k,2)=dhx*(envg(i+1,j,k,1)-envg(i,j,k,1))
    do i=i1+1,n1p-1
     envg(i,j,k,2)=envg(i,j,k,2)+ax2*(envg(i+2,j,k,1)-envg(i-1,j,k,1))
    end do
   end do
  end do
 endif
 if(pe1y)then
  j=n2p
  shy=loc_yg(j-2,4,imody)*ay1
  do k=k1,n3p
   do i=i1,n1p
    envg(i,j+1,k,1)=2.*envg(i,j,k,1)-envg(i,j-1,k,1)
    envg(i,j,k,3)=shy*(envg(i,j+1,k,1)-envg(i,j,k,1))
   end do
  end do
  j02=n2p-1
 end if

 if(pe0y)then
  j=j1
  shy=loc_yg(j-2,4,imody)*ay1
  do k=k1,n3p
   do i=i1,n1p
    envg(i,j-1,k,1)=2.*envg(i,j,k,1)-envg(i,j+1,k,1)
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
    do i=i1,n1p
     envg(i,j,k,3)=envg(i,j,k,3)+ay2*(envg(i,j+2,k,1)-envg(i,j-1,k,1))
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
   do j=j1,n2p
    do i=i1,n1p
     envg(i,j,k,4)=envg(i,j,k,4)+az2*(envg(i,j,k+2,1)-envg(i,j,k-1,1))
    end do
   end do
  end do
 endif
 end subroutine env_grad
!----------------------------------------
 subroutine env_maxw_solve(curr,evf,i1,n1p,j1,n2p,k1,n3p,&
 om0,dhx,dhy,dhz,dt_loc)
 real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: om0,dhx,dhy,dhz,dt_loc
 integer :: i,j,k,ic
 real(dp) ::dt2,dx1_inv,dhx1_inv,aph_opt(2)
 real(dp) ::kfact,k2_fact,skfact
 !real(dp),dimension(0:2),parameter :: lder=(/1.0,-4.0,3.0/)
 !==========================
 ! EXPLICIT INTEGRATION of Maxwell ENVELOPE EVOLUTION EQUATION
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
 !Ensures that the envelope field at left boundary (tail of the box)
 !is kept zero
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,i1+1
    evf(i,j,k,1)=zero_dp
    evf(i,j,k,2)=zero_dp
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
 subroutine env_comov_maxw_solve(curr,evf,i1,n1p,j1,n2p,k1,n3p,&
 om0,dhx,dhy,dhz,dt_loc)
 real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: om0,dhx,dhy,dhz,dt_loc
 integer :: i,j,k,ii,ic
 real(dp) ::dt2,dx1_inv,dhx1_inv
 real(dp) ::kfact,k2_fact,a_fact
 real(dp),dimension(0:2),parameter :: lder=(/1.0,-4.0,3.0/)
!==========================
! EXPLICIT INTEGRATION of Maxwell ENVELOPE EVOLUTION EQUATION
!============================
 dt2=dt_loc*dt_loc
 kfact=om0*dt_loc
 a_fact=dt_loc*dhx
 a_fact=0.0            !NO MIXED DER
 k2_fact=1./(1.+kfact*kfact)
 dx1_inv=0.5*a_fact
 dhx1_inv=2.*dx1_inv
 ic=2
!========Enter  jc(1:2)= - omp2*<q^2*chi*env(1:2)
!                        chi <q^2*wgh*n/gam_p> >0
! Computes the transverse Laplacian of A^{n}=env(1:2) components
!========and adds to  jc(1:2)
call pp_lapl(evf,curr,1,ic,der_ord,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)
!=====================
! =>   jc(1:2)=[D_pp^2-omp^2*chi]A= S(A);
!=================
!A^n-A_{n-1} def[B^{n-1/2}=> A^n
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,n1p
    evf(i,j,k,1)=evf(i,j,k,1)-evf(i,j,k,3)
    evf(i,j,k,2)=evf(i,j,k,2)-evf(i,j,k,4)
   end do
  end do
 end do
!===============
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     curr(i,j,k,1)=dt2*curr(i,j,k,1)-kfact*evf(i,j,k,2)
     curr(i,j,k,2)=dt2*curr(i,j,k,2)+kfact*evf(i,j,k,1)
    end do
   end do
  end do
!curr(1)=F_R=dt2*S_R-kfact*B_I^{n-1/2}
!curr(2)=F_I=dt2*S_I+kfact*B_R^{n-1/2}
!====================
 if(a_fact >0.0)call AF_der(1)
!F_R => F_R +dt*[D_xB_R]
!F_I => F_I +dt*[D_xB_I]
!======================
 call AF_der(2)
!F_R => (1+dt*D_x)F_R-kfact*F_I
!F_I => (1+dt*D_x)F_I+kfact*F_R
!======================
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,n1p
    evf(i,j,k,1)=evf(i,j,k,1)+evf(i,j,k,3) !A^{n}=> B^{n+1/2}+A^{n-1}
    evf(i,j,k,2)=evf(i,j,k,2)+evf(i,j,k,4)
    evf(i,j,k,3)=evf(i,j,k,1)
    evf(i,j,k,4)=evf(i,j,k,2)
   end do
  end do
 end do
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,n1p
    ii=i-2
    ww0(ii,1)=curr(i,j,k,1)
    ww0(ii,2)=curr(i,j,k,2)
   end do
   !call matenv_inv(ii,2)
   do i=i1,n1p
    ii=i-2
    evf(i,j,k,1)=evf(i,j,k,1)+k2_fact*ww0(ii,1)
    evf(i,j,k,2)=evf(i,j,k,2)+k2_fact*ww0(ii,2)
   end do
  end do
 end do
 contains
 subroutine AF_der(id)
 integer,intent(in) :: id
!============
 select case(id)
 case(1)
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    curr(i,j,k,ic)=curr(i,j,k,ic)+dhx1_inv*(&
            evf(i+1,j,k,ic)-evf(i,j,k,ic))
    do i=i1+1,n1p-1
     curr(i,j,k,ic)=curr(i,j,k,ic)+dx1_inv*(&
             evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
    end do
    i=n1p
    curr(i,j,k,ic)=curr(i,j,k,ic)+dx1_inv*(&
            evf(i,j,k,ic)-evf(i-1,j,k,ic))
   end do
  end do
 end do
 case(2)
 do k=k1,n3p
  do j=j1,n2p
   i=i1
   ii=i-2
   ww0(ii,1)=curr(i,j,k,1)+dhx1_inv*(&
           curr(i+1,j,k,1)-curr(i,j,k,1))
   ww0(ii,2)=curr(i,j,k,2)+dhx1_inv*(&
           curr(i+1,j,k,2)-curr(i,j,k,2))
   do i=i1+1,n1p-1
    ii=i-2
    ww0(ii,1)=curr(i,j,k,1)+dx1_inv*(&
            curr(i+1,j,k,1)+curr(i-1,j,k,1))
    ww0(ii,2)=curr(i,j,k,2)+dx1_inv*(&
            curr(i+1,j,k,2)-curr(i-1,j,k,2))
   end do
    i=n1p
    ii=i-2
    ww0(ii,1)=curr(i,j,k,1)+dx1_inv*(&
            curr(i,j,k,1)-curr(i-1,j,k,1))
    ww0(ii,2)=curr(i,j,k,2)+dx1_inv*(&
            curr(i,j,k,2)-curr(i-1,j,k,2))
   do i=i1,n1p
    ii=i-2
    ww0(ii,1)=ww0(ii,1)-kfact*curr(i,j,k,2)
    ww0(ii,2)=ww0(ii,2)+kfact*curr(i,j,k,1)
   end do
   do i=i1,n1p
    ii=i-2
    curr(i,j,k,1)=ww0(ii,1)
    curr(i,j,k,2)=ww0(ii,2)
   end do
  end do
 end do
 end select
 end subroutine AF_der

 end subroutine env_comov_maxw_solve
 !==============================
 subroutine env_lpf_solve(curr,evf,ib,i1,n1p,j1,n2p,k1,n3p,&
 om0,dhx,dhy,dhz,dt_loc)
 real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:)
 integer,intent(in) :: ib,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: om0,dhx,dhy,dhz,dt_loc
 integer :: i,j,k,ii,ic,ic1,n1
 real(dp) :: dx1_inv,om2,aph1,dx_norm,dx2_norm
 real(dp) :: adv,an,bn,der2_norm
 !==========================
 ! EXPLICIT INTEGRATION of ENVELOPE EVOLUTION EQUATION
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
   ! u^{n+1}=u^{n-1}+adv*(u_{i+1}-u_{i-1})+0.5*adv*(
   adv=dt_loc*dhx     !cfl=dt/dx
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
 subroutine env0_rk_field(&
 curr,evf,ib,i1,n1p,j1,n2p,k1,n3p,om0,dhx,dhy,dhz)
 real(dp),intent(inout) :: curr(:,:,:,:), evf(:,:,:,:)
 integer,intent(in) :: ib,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: om0,dhx,dhy,dhz
 integer :: n1,i,j,k,ii,ic
 real(dp) :: dx1_inv,om2,dx2_norm
 real(dp) :: alp2,b2,a,adv,c,c1,b1,an,bn
 real(dp) :: c1_der(0:1),c2_der(0:2)
 real(dp),parameter :: alp=0.25
	!real(dp),parameter :: a1=0.75
 !==========================

 n1=n1p+1-i1
 om2=om0*om0
 dx1_inv=0.5*dhx
 dx2_norm=dhx*dhx/om2
 c1_der(0)=2.*dhx/3.
 c1_der(1)=-c1_der(0)/12.
 c2_der(0)=-5./2.
 c2_der(1)= 4./3.
 c2_der(2)=-1./12.
 !==============================================
 !Compact Fourth order first derivatieve
 ! (alp,1,alp)D_x[f]=(a/dhx)[f_{i+1}-f_{i-1}]
 ! a=3/4 alp=1/4
 !=====================
 alp2=alp*alp
 !=======================
 b2=dhx*dhx/om2
 a=b2
 c=b2
 adv=1.-2.*b2
 c1=c-a
 b1=b2+2.*a
 an=a
 bn=adv+c
 !================
 ! Computes the Laplacian
 !======== in jc(1:2)= omp2*<w*n/gam_p>*env(1:2)=> omp2*chi*A
 ic=2
 call pp_lapl(evf,curr,1,ic,der_ord,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     curr(i,j,k,ic)=0.5*curr(i,j,k,ic)
    end do
   end do
  end do
 end do
 ! IN curr(1:2) the sotrce term S[A]=-1/2[D^2_{perp}-omp2*chi]A
 !=====================
 !=================
 call rk_der
 !==== curr(1) =  F_R=D_{xi}S(A_R)+k0*S(A_I)
 !==== curr(2) =  F_I=D_{xi}S(A_I)-k0*S(A_R)
   !=============================
 call rk_mat_inv

 call norm_mat_inv
 !   curr=M^{-1}F = D_{tau}A =
 !=======================
 if(ib==0)return
 !=======================
 !==================================
 !   adds explicit advection term fourth order scheme
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    evf(i-1,j,k,ic)=2.*evf(i,j,k,ic)-evf(i+1,j,k,ic)
    curr(i,j,k,ic)=curr(i,j,k,ic)-&
                   dx1_inv*(evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
    i=i1+1
    curr(i,j,k,ic)=curr(i,j,k,ic)-&
     dx1_inv*(evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
    do i=i1+2,n1p-2
     curr(i,j,k,ic)=curr(i,j,k,ic)-dhx*(&
      c1_der(0)*(evf(i+1,j,k,ic)-evf(i-1,j,k,ic))+&
      c1_der(1)*(evf(i+2,j,k,ic)-evf(i-2,j,k,ic)))
    end do
    i=n1p-1
    curr(i,j,k,ic)=curr(i,j,k,ic)-&
     dx1_inv*(evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
    i=n1p
    evf(i+1,j,k,ic)=evf(i,j,k,ic)
    curr(i,j,k,ic)=curr(i,j,k,ic)-&
     dx1_inv*(evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
   end do
  end do
 end do
 contains
 subroutine rk_der

 do k=k1,n3p
  do j=j1,n2p
    i=i1
    ii=i-2
    ww0(ii,1)=om0*curr(i,j,k,2)+dhx*(&
             curr(i+1,j,k,1)-curr(i,j,k,1))
    ww0(ii,2)= -om0*curr(i,j,k,1)+dhx*(&
             curr(i+1,j,k,2)-curr(i,j,k,2))
    i=i1+1
    ii=i-2
    ww0(ii,1)=om0*curr(i,j,k,2)+dx1_inv*(&
     curr(i+1,j,k,1)-curr(i-1,j,k,1))
    ww0(ii,2)= -om0*curr(i,j,k,1)+dx1_inv*(&
     curr(i+1,j,k,2)-curr(i-1,j,k,2))
    do i=i1+2,n1p-2
     ii=i-2
     ww0(ii,1)=om0*curr(i,j,k,2)+&
             c1_der(0)*(curr(i+1,j,k,1)-curr(i-1,j,k,1))+&
             c1_der(1)*(curr(i+2,j,k,1)-curr(i-2,j,k,1))
     ww0(ii,2)= -om0*curr(i,j,k,1)+&
             c1_der(0)*(curr(i+1,j,k,2)-curr(i-1,j,k,2))+&
             c1_der(1)*(curr(i+2,j,k,2)-curr(i-2,j,k,2))
   end do
    i=n1p-1
    ii=i-2
    ww0(ii,1)=om0*curr(i,j,k,2)+dx1_inv*(&
             curr(i+1,j,k,1)-curr(i-1,j,k,1))
    ww0(ii,2)= -om0*curr(i,j,k,1)+dx1_inv*(&
             curr(i+1,j,k,2)-curr(i-1,j,k,2))
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
  end subroutine rk_der
!===============================
  subroutine rk_mat_inv
   integer :: ic
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    i=i1
      ii=i-2
      ww0(ii,ic)=dx2_norm*(&
                  curr(i,j,k,ic)-2.*curr(i+1,j,k,ic)+curr(i+2,j,k,ic))
    i=i1+1
      ii=i-2
      ww0(ii,ic)=dx2_norm*(&
              curr(i+1,j,k,ic)-2.*curr(i,j,k,ic)+curr(i-1,j,k,ic))
      do i=i1+2,n1p-2
       ii=i-2
       ww0(ii,ic)=dx2_norm*(c2_der(0)*curr(i,j,k,ic)+&
              c2_der(1)*(curr(i+1,j,k,ic)+curr(i-1,j,k,ic))+&
              c2_der(2)*(curr(i+2,j,k,ic)+curr(i-2,j,k,ic)))
    end do
    i=n1p
      ii=i-2
      ww0(ii,ic)=dx2_norm*(&
              curr(i,j,k,ic)-2.*curr(i-1,j,k,ic)+curr(i-2,j,k,ic))
      i=n1p-1
      ii=i-2
      ww0(ii,ic)=dx2_norm*(&
              curr(i+1,j,k,ic)-2.*curr(i,j,k,ic)+curr(i-1,j,k,ic))
      do i=i1,n1p
       ii=i-2
       curr(i,j,k,ic)=(curr(i,j,k,ic)-ww0(ii,ic))/om2
      end do
     end do
    end do
   end do
  end subroutine rk_mat_inv
  subroutine norm_mat_inv
!================== Uses three-point numerical secon derivative
   do k=k1,n3p
    do j=j1,n2p
     do i=i1,n1p
      curr(i,j,k,1)=curr(i,j,k,1)/om2
      curr(i,j,k,2)=curr(i,j,k,2)/om2
   end do
  end do
 end do
  end subroutine norm_mat_inv
 !===========================
 end subroutine env0_rk_field
 !==========================================
 ! END ENV SECTION
 !==================================
 !========== LASER FIELDS SECTION
 !=================================
 ! INITIAL FIELDS
 !==============================
 subroutine get_2Dlaser_gprof_fields_lp(coords,par_lp,fields)
 real(dp),intent(in) :: coords(4),par_lp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1, phig00, phig10
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
 real(dp) :: phi0, phi1, phig00, phig10
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
 real(dp) :: phi0, phi1, phig00, phig10
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
 real(dp) :: xp,xc,yc,zc,zra,ycent,zcent
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
 !==========================================
 subroutine divcurr_inv(curr,ix1,np1,iy1,np2,iz1,np3)

 real(dp),intent(inout) :: curr(:,:,:,:)
 integer,intent(in) :: ix1,np1,iy1,np2,iz1,np3
 integer :: i,ii,j,k,ihx,n1
 !======================

 ihx=ix1-1
 n1=np1-ihx
 ww(n1+1)=0.0
 do k=iz1,np3
  do j=iy1,np2
   do i=ix1,np1
    ii=i-ihx
    ww(ii)=curr(i,j,k,1)
   end do
   call hder_inv(n1,ibx)
   do i=ix1,np1
    ii=i-ihx
    curr(i,j,k,1)=ww(ii)
   end do
  end do
 end do
 end subroutine divcurr_inv
 !===================
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
 if(Pex0)then
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
  if(pex1)then
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
 subroutine enforce_continuity(curr,i1,n1p,j1,j2,k1,k2)

 real(dp),intent(inout) :: curr(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,j2,k1,k2
 real(dp) :: aphy,aphz,shy,shz
 integer :: i,ii,j,k,jj,kk,j01,k01
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
    curr(i,j,k,1)=curr(i,j,k,1)+aphy*(curr(i,j,k,2)-curr(i,j-1,k,2))
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
 ww0(:,1)=0.0
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
!===============================
 subroutine divA(ef,curr,i1,n1p,j1,j2,k1,k2,ic,aphx,aphy,aphz)

 real(dp),intent(inout) :: ef(:,:,:,:),curr(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,j2,k1,k2,ic
 real(dp),intent(in) :: aphx,aphy,aphz
 integer :: i,j,k
 !===================== 3D Cartesian div(A)
 !=======================================
 if(pex0)then
  i=i1
  do k=k1,k2
   do j=j1,j2
    ef(i-1,j,k,1)=2.*ef(i,j,k,1)-ef(i+1,j,k,1)
   end do
  end do
 endif
 do k=k1,k2
  do j=j1,j2
   do i=i1,n1p
    curr(i,j,k,ic)=curr(i,j,k,ic)+aphx*(ef(i,j,k,1)-ef(i-1,j,k,1))
   end do
  end do
 end do
 if(ndim==1)return
 if(pe0y)then
  j=j1
  do k=k1,k2
   do i=i1,n1p
    ef(i,j-1,k,2)=2.*ef(i,j,k,2)-ef(i,j+1,k,2)
   end do
  end do
 endif
 do k=k1,k2
  do j=j1,j2
   do i=i1,n1p
    curr(i,j,k,ic)=curr(i,j,k,ic)+aphy*(ef(i,j,k,2)-ef(i,j-1,k,2))
   end do
  end do
 end do
 !================ ndim >2
 if(ndim ==3)then
  if(pe0z)then
   k=k1
   do j=j1,j2
    do i=i1,n1p
     ef(i,j,k-1,3)=2.*ef(i,j,k,3)-ef(i,j,k+1,3)
    end do
   end do
  endif
  do k=k1,k2
   do j=j1,j2
    do i=i1,n1p
     curr(i,j,k,ic)=curr(i,j,k,ic)+aphz*(ef(i,j,k,3)-ef(i,j,k-1,3))
    end do
   end do
  end do
 endif

 end subroutine divA
 !==============================
 subroutine grad_pot(apf,grad,i1,i2,j1,j2,k1,k2,ic,dhx,dhy,dhz)
 real(dp),intent(inout) :: apf(:,:,:,:)
 real(dp),intent(inout) :: grad(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ic
 real(dp),intent(in) :: dhx,dhy,dhz
 integer :: i,j,k,j01,j02,k01,k02

 j01=j1
 j02=j2
 k01=k1
 k02=k2
 if(pex1)then
  i=i2+1
  do k=k1,k2
   do j=j1,j2
    apf(i,j,k,ic)=2.*apf(i-1,j,k,ic)-apf(i-2,j,k,ic)
   end do
  end do
 endif
 do k=k1,k2
  do j=j1,j2
   do i=i1,i2
    grad(i,j,k,1)= dhx*(apf(i+1,j,k,ic)-apf(i,j,k,ic))    ! (D_x)_{i+1/2}
   end do
  end do
 end do
 if(ndim >1)then
  if(pe1y)then
   do k=k1,k2
    j=j2+1
    do i=i1,i2
     apf(i,j,k,ic)=2.*apf(i,j-1,k,ic)-apf(i,j-2,k,ic)
    end do
   end do
  endif
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     grad(i,j,k,2)= dhy*(apf(i,j+1,k,ic)-apf(i,j,k,ic))   !(D_y)_{j+1/2}
    end do
   end do
  end do
 endif
 if(ndim==2)return

 !==================================
 if(pe1z)then
  k=k2+1
  do j=j1,j2
   do i=i1,i2
    apf(i,j,k,ic)=2.*apf(i,j,k-1,ic)-apf(i,j,k-2,ic)
   end do
  end do
 endif
 do k=k1,k2
  do j=j1,j2
   do i=i1,i2
    grad(i,j,k,3)= dhz*(apf(i,j,k+1,ic)-apf(i,j,k,ic)) !Dz
   end do
  end do
 end do
 end subroutine grad_pot
 !==================================
 subroutine potential_lapl(apf,curr,ic1,ic2,dord,i1,n1p,j1,n2p,k1,n3p,&
                                                      dhx,dhy,dhz)
 real(dp),intent(inout)  :: apf(:,:,:,:),curr(:,:,:,:)

 integer,intent(in):: ic1,ic2,dord,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: dhx,dhy,dhz
 integer :: i,j,k,ic,i01,i02
 real(dp) :: dx2,cf(0:2)
 !Computes the Laplacian(apf) and accumulates on the source array curr
 !                 curr=laplcian(apf)+curr
 !========================================
 dx2=dhx*dhx
 i01=i1
 i02=n1p
 !2============= ALL FIELDS ic=ic1,ic2
 cf(0)=0.0
 cf(1)=1.
 cf(2)=-2.
 if(dord > 2)then              ! dord=3  se_coeff(2)=-(1-nu*nu)/8  dord=4 se_coeff(2)=-1/8
  cf(0)=hord_der2
  cf(1)=1.-4.*hord_der2
  cf(2)=6.*hord_der2-2.
 endif
 if(pex0)then
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
 if(pex1)then
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
     curr(i,j,k,ic)=curr(i,j,k,ic)+dx2*(cf(1)*(apf(i+1,j,k,ic)+apf(i-1,j,k,ic))+&
                                        cf(2)*apf(i,j,k,ic))

    end do
   end do
  end do
 end do
 if(dord> 2)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     do i=i01,i02
      curr(i,j,k,ic)=curr(i,j,k,ic)+dx2*cf(0)*(apf(i+2,j,k,ic)+apf(i-2,j,k,ic))

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
 subroutine rotB_rk4(ef,ef1,nef,i1,n1p,j1,n2p,k1,n3p,vb,aphx,aphy,aphz)
 real(dp),intent(inout) :: ef(:,:,:,:),ef1(:,:,:,:)

 integer,intent(in) :: nef,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: vb,aphx,aphy,aphz
 integer :: i,j,k,ic,ii,jj,j01,j02,kk,k01,k02
 !real(dp),dimension(2),parameter :: e4_coeff=(/1.125,-1./24/)
 real(dp) :: aphx1,aphx2,advx,advx1,advx2
 real(dp) :: aphy1,aphy2,sdy
 real(dp) :: aphz1,aphz2,sdz

 aphx1=aphx*se_coeff(1)
 aphx2=aphx*se_coeff(2)
 aphy1=aphy*se_coeff(1)
 aphy2=aphy*se_coeff(2)
 aphz1=aphz*se_coeff(1)
 aphz2=aphz*se_coeff(2)

 advx=0.5*vb*aphx
 advx1=2.*vb*aphx/3.
 advx2=-vb*aphx/12.
 !================
 ii=1
 !=========== fourth order
 ! advances E^k=E^0-dt*ompe*J^{k-1}+dt*rot(B)^{k-1}
 ! enter Ef=B^{k-1}   Ef1=E^0-dt*ompe*J^{k-1}
 ! Updates  Ef1=Ef1+rot[Ef]
 !===================================
 if(Comoving)then

  ! ADDS -vb*D_x(E) advection with vb x<0 speed in comoving
  ! coordinate system
  do ic=1,nef
   do k=k1,n3p
    do j=j1,n2p
     i=i1
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*advx*(ef(i+1,j,k,ic)-ef(i,j,k,ic))
     i=i1+1
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-advx*(ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
     do i=i1+2,n1p-2
      ef1(i,j,k,ic)=ef1(i,j,k,ic)-(&
       advx1*(ef(i+1,j,k,ic)-ef(i-1,j,k,ic))+&
       advx2*(ef(i+2,j,k,ic)-ef(i-2,j,k,ic)))
     end do
     i=n1p-1
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-advx*(ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
     i=n1p
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*advx*(ef(i,j,k,ic)-ef(i-1,j,k,ic))
    end do
   end do
  end do
 endif
 !===============================
 do k=k1,n3p
  do j=j1,n2p
   ef1(i1,j,k,2)=ef1(i1,j,k,2)-aphx*(ef(i1,j,k,nfield)-ef(i1-1,j,k,nfield))
   do i=i1+1,n1p-1
    ef1(i,j,k,2)=ef1(i,j,k,2)-(&
     aphx1*(ef(i,j,k,nfield)-ef(i-1,j,k,nfield))+&
     aphx2*(ef(i+1,j,k,nfield)-ef(i-2,j,k,nfield)))
   end do
   ef1(n1p,j,k,2)=ef1(n1p,j,k,2)-&
    aphx*(ef(n1p,j,k,nfield)-ef(n1p-1,j,k,nfield))
  end do
 end do
 !D_tE_y=-dBz/dx-omp2*jy
 if(nfield>3)then
  do k=k1,n3p
   do j=j1,n2p
    ef1(i1,j,k,3)=ef1(i1,j,k,3)+aphx*(ef(i1,j,k,5)-ef(i1-1,j,k,5))
    do i=i1+1,n1p-1
     ef1(i,j,k,3)=ef1(i,j,k,3)+(&
      aphx1*(ef(i,j,k,5)-ef(i-1,j,k,5))+&
      aphx2*(ef(i+1,j,k,5)-ef(i-2,j,k,5)))
    end do
    ef1(n1p,j,k,3)=ef1(n1p,j,k,3)+aphx*(ef(n1p,j,k,5)-ef(n1p-1,j,k,5))
   end do
  end do
  !D_tE_z=dBy/dx-omp2*jz
 endif
 ! data [j1-2:n2p+1]
 !================================
 if(ndim >1)then
  j01=j1;j02=n2p
  if(pe0y)then
   j=j1
   jj=j-2
   sdy=loc_yg(jj,3,imody)*aphy
   do k=k1,n3p
    do i=i1,n1p
     ef1(i,j,k,1)=ef1(i,j,k,1)+sdy*(&
      ef(i,j,k,nfield)-ef(i,j-1,k,nfield))
    end do
   end do
   if(nfield >3)then
    do k=k1,n3p
     do i=i1,n1p
      ef1(i,j,k,3)=ef1(i,j,k,3)-sdy*(&
       ef(i,j,k,4)-ef(i,j-1,k,4))
     end do
    end do
   endif
   j01=j1+1
  endif
  if(pe1y)then
   j=n2p
   jj=j-2
   sdy=loc_yg(jj,3,imody)*aphy
   do k=k1,n3p
    do i=i1,n1p
     ef1(i,j,k,1)=ef1(i,j,k,1)+sdy*(&
      ef(i,j,k,nfield)-ef(i,j-1,k,nfield))
    end do
   end do
   if(nfield >3)then
    do k=k1,n3p
     do i=i1,n1p
      ef1(i,j,k,3)=ef1(i,j,k,3)-sdy*(&
       ef(i,j,k,4)-ef(i,j-1,k,4))
     end do
    end do
   endif
   j02=n2p-1
  endif
  !interior data [j1-2:n2p+1] boundary j=j1-1
  do k=k1,n3p
   do j=j01,j02
    jj=j-2
    sdy=loc_yg(jj,3,imody)
    do i=i1,n1p
     ef1(i,j,k,1)=ef1(i,j,k,1)+sdy*(&
      aphy1*(ef(i,j,k,nfield)-ef(i,j-1,k,nfield))+&
      aphy2*(ef(i,j+1,k,nfield)-ef(i,j-2,k,nfield)))
    end do
   end do
  end do
  !D_tE_x=+dBz/dy
  !=============================
  if(nfield>3)then
   do k=k1,n3p
    do j=j01,j02
     jj=j-2
     sdy=loc_yg(jj,3,imody)
     do i=1,n1p
      ef1(i,j,k,3)=ef1(i,j,k,3)-sdy*(&
       aphy1*(ef(i,j,k,4)-ef(i,j-1,k,4))+&
       aphy2*(ef(i,j+1,k,4)-ef(i,j-2,k,4)))
      !D_tE_z=-dBx/dy
     end do
    end do
   end do
  endif
  if(ndim> 2)then
   !===============
   ! Interior Data [k1-2:n3p+1] boundary at k=k1-1 (pe0z)
   k01=k1;k02=n3p
   if(pe1z)then
    k=n3p
    kk=k-2
    sdz=loc_zg(kk,3,imodz)*aphz
    do j=j1,n2p
     do i=i1,n1p
      ef1(i,j,k,1)=ef1(i,j,k,1)-sdz*(&
       ef(i,j,k,5)-ef(i,j,k-1,5))
      !D_tE_x=-dBy/dz
      ef1(i,j,k,2)=ef1(i,j,k,2)+sdz*(&
       ef(i,j,k,4)-ef(i,j,k-1,4))
      !D_tE_y=dBx/dz
     end do
    end do
    k02=n3p-1
   endif
   if(pe0z)then
    k=k1
    kk=k-2
    sdz=loc_zg(kk,3,imodz)*aphz
    do j=j1,n2p
     do i=i1,n1p
      ef1(i,j,k,1)=ef1(i,j,k,1)-sdz*(&
       ef(i,j,k,5)-ef(i,j,k-1,5))
      !D_tE_x=-dBy/dz
      ef1(i,j,k,2)=ef1(i,j,k,2)+sdz*(&
       ef(i,j,k,4)-ef(i,j,k-1,4))
      !D_tE_y=dBx/dz
     end do
    end do
    k01=k1+1
   endif
   do k=k01,k02
    kk=k-2
    sdz=loc_zg(kk,3,imodz)
    do j=j1,n2p
     do i=i1,n1p
      ef1(i,j,k,1)=ef1(i,j,k,1)-sdz*(&
       aphz1*(ef(i,j,k,5)-ef(i,j,k-1,5))+&
       aphz2*(ef(i,j,k+1,5)-ef(i,j,k-2,5)))
      !D_tE_x=-dBy/dz
      ef1(i,j,k,2)=ef1(i,j,k,2)+sdz*(&
       aphz1*(ef(i,j,k,4)-ef(i,j,k-1,4))+&
       aphz2*(ef(i,j,k+1,4)-ef(i,j,k-2,4)))
      !D_tE_y=dBx/dz
     end do
    end do
   end do
  endif
 endif               !END ndim >1 case
 !==========================================
 !======== in ef1 exit dt_k*rot(B)-ompe*dt_k*J +dt_k*vb*D_x[E]
 !==================================
 end subroutine rotB_rk4
!========================
 subroutine rotE_rk4(ef,ef0,nef,i1,n1p,j1,n2p,k1,n3p,vb,aphx,aphy,aphz)
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: ef0(:,:,:,:)

 integer,intent(in) :: nef,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: vb,aphx,aphy,aphz
 integer :: i,j,k,ic,ic1,jj,j01,j02,kk,k01,k02
 !real(dp),dimension(2),parameter :: e4_coeff=(/1.125,-1./24/)
 real(dp) :: aphx1,aphx2,advx,advx1,advx2
 real(dp) :: aphy1,aphy2,sdy
 real(dp) :: aphz1,aphz2,sdz

 aphx1=aphx*se_coeff(1)
 aphx2=aphx*se_coeff(2)
 aphy1=aphy*se_coeff(1)
 aphy2=aphy*se_coeff(2)
 aphz1=aphz*se_coeff(1)
 aphz2=aphz*se_coeff(2)

 advx=0.5*vb*aphx
 advx1=2.*vb*aphx/3.
 advx2=-vb*aphx/12.
!================ advances B
 !          B^k= B^0-dt_rk*rot[E^{k-1}] using fourth-order space derivatives
 !          Update B field  B^0 untouched
 !============================
 if(Comoving)then
  ! ADDS -vb*D_x(B) advection with vbr<0 speed
  do ic=1,nef
   ic1=ic+3
   do k=k1,n3p
    do j=j1,n2p
     i=i1
     ww0(i,ic)=2.*advx*(ef(i+1,j,k,ic1)-ef(i,j,k,ic1))
     i=i1+1
     ww0(i,ic)=advx*(ef(i+1,j,k,ic1)-ef(i-1,j,k,ic1))
     do i=i1+2,n1p-2
      ww0(i,ic)= (&
       advx1*(ef(i+1,j,k,ic1)-ef(i-1,j,k,ic1))+&
       advx2*(ef(i+2,j,k,ic1)-ef(i-2,j,k,ic1)))
     end do
     i=n1p-1
     ww0(i,ic)=advx*(ef(i+1,j,k,ic1)-ef(i-1,j,k,ic1))
     i=n1p
     ww0(i,ic)=2.*advx*(ef(i,j,k,ic1)-ef(i-1,j,k,ic1))
     do i=i1,n1p
      ef(i,j,k,ic1)=ef0(i,j,k,ic)-ww0(i,ic)
     end do
    end do
   end do
  end do
 endif
 !==============================
 !     rotE operators
 !===============================
 do k=k1,n3p
  do j=j1,n2p
   ef(i1,j,k,nfield)=ef0(i1,j,k,nfield)-aphx*(ef(i1+1,j,k,2)-ef(i1,j,k,2))
   do i=i1+1,n1p-1
    ef(i,j,k,nfield)=ef0(i,j,k,nfield)-(&
     aphx1*(ef(i+1,j,k,2)-ef(i,j,k,2))+&
     aphx2*(ef(i+2,j,k,2)-ef(i-1,j,k,2)))
   end do
   ef(n1p,j,k,nfield)=ef0(n1p,j,k,nfield)-&
    aphx*(ef(n1p+1,j,k,2)-ef(n1p,j,k,2))
  end do
 end do
 if(nfield>3)then
  !====================
  do k=k1,n3p
   do j=j1,n2p
    ef(i1,j,k,5)=ef0(i1,j,k,5)+aphx*(ef(i1+1,j,k,3)-ef(i1,j,k,3))
    do i=i1+1,n1p-1
     ef(i,j,k,5)=ef0(i,j,k,5)+(&
      aphx1*(ef(i+1,j,k,3)-ef(i,j,k,3))+&
      aphx2*(ef(i+2,j,k,3)-ef(i-1,j,k,3)))
    end do
    ef(n1p,j,k,5)=ef0(n1p,j,k,5)+&
     aphx*(ef(n1p+1,j,k,3)-ef(n1p,j,k,3))
   end do
  end do
  !D_tB_y=dEz/dx
 endif
 if(ndim==1)return
 !========================
 ! Data [j1-1:n2p+2]
 j01=j1;j02=n2p
 if(pe1y)then
  j=n2p
  jj=j-2
  sdy=loc_yg(jj,4,imody)*aphy
  do k=k1,n3p
   do i=i1,n1p
    ef(i,j,k,nfield)=ef(i,j,k,nfield)+sdy*(&
     ef(i,j+1,k,1)-ef(i,j,k,1))
   end do
  end do
  !D_tB_z=+dEx/dy
  if(nfield >3)then
   do k=k1,n3p
    do i=i1,n1p
     ef(i,j,k,4)=ef0(i,j,k,4)-sdy*(ef(i,j+1,k,3)-ef(i,j,k,3))
     !D_tB_x=-dEz/dy
    end do
   end do
  endif
  j02=n2p-1
 endif
 if(pe0y)then
  j=j1
  jj=j-2
  sdy=loc_yg(jj,4,imody)*aphy
  do k=k1,n3p
   do i=i1,n1p
    ef(i,j,k,nfield)=ef(i,j,k,nfield)+sdy*(&
                     ef(i,j+1,k,1)-ef(i,j,k,1))
   end do
  end do
  !D_tB_z=+dEx/dy
  if(nfield >3)then
   do k=k1,n3p
    do i=i1,n1p
     ef(i,j,k,4)=ef(i,j,k,4)-sdy*(ef(i,j+1,k,3)-ef(i,j,k,3))
     !D_tB_x=-dEz/dy
    end do
   end do
  endif
  j01=j1+1
 endif
 !=========================== data [j1-1:n2p+2]
 do k=k1,n3p
  do j=j01,j02
   jj=j-2
   sdy=loc_yg(jj,4,imody)
   do i=i1,n1p
    ef(i,j,k,nfield)=ef(i,j,k,nfield)+sdy*(&
     aphy1*(ef(i,j+1,k,1)-ef(i,j,k,1))+&
     aphy2*(ef(i,j+2,k,1)-ef(i,j-1,k,1)))
    !D_tB_z=+dEx/dy-dEy/dx
   end do
  end do
 end do
 if(nfield >3)then
  do k=k1,n3p
   do j=j01,j02
    jj=j-2
    sdy=loc_yg(jj,4,imody)
    do i=i1,n1p
     ef(i,j,k,4)=ef(i,j,k,4)-sdy*(&
      aphy1*(ef(i,j+1,k,3)-ef(i,j,k,3))+&
      aphy2*(ef(i,j+2,k,3)-ef(i,j-1,k,3)))
     !D_tB_z=+dEx/dy-dEy/dx
    end do
   end do
  end do
 endif
 if(ndim>2)then
  ! interior data [k1-1:n3p+2] boundary at k=n3p+1
  k01=k1;k02=n3p
  if(pe1z)then
   k=n3p
   kk=k-2
   sdz=loc_zg(kk,4,imodz)*aphz
   do j=j1,n2p
    do i=i1,n1p
     ef(i,j,k,4)=ef(i,j,k,4)+&
      sdz*(ef(i,j,k+1,2)-ef(i,j,k,2))
     !D_tB_x=dEy/dz
     ef(i,j,k,5)=ef(i,j,k,5)-&
      sdz*(ef(i,j,k+1,1)-ef(i,j,k,1))
     !D_tB_y=-dEx/dz
    end do
   end do
   k02=n3p-1
  endif
  if(pe0z)then
   k=k1
   kk=k-2
   sdz=loc_zg(kk,4,imodz)*aphz
   do j=j1,n2p
    do i=i1,n1p
     ef(i,j,k,4)=ef(i,j,k,4)+&
      sdz*(ef(i,j,k+1,2)-ef(i,j,k,2))
     !D_tB_x=dEy/dz
     ef(i,j,k,5)=ef(i,j,k,5)-&
      sdz*(ef(i,j,k+1,1)-ef(i,j,k,1))
     !D_tB_y=-dEx/dz
    end do
   end do
   k01=k1+1
  endif
  do k=k01,k02
   kk=k-2
   sdz=loc_zg(kk,4,imodz)
   do j=j1,n2p
    do i=i1,n1p
     ef(i,j,k,4)=ef(i,j,k,4)+sdz*(&
      aphz1*(ef(i,j,k+1,2)-ef(i,j,k,2))+&
      aphz2*(ef(i,j,k+2,2)-ef(i,j,k-1,2)))
     !D_tB_x=dEy/dz
     ef(i,j,k,5)=ef(i,j,k,5)-sdz*(&
      aphz1*(ef(i,j,k+1,1)-ef(i,j,k,1))+&
      aphz2*(ef(i,j,k+2,1)-ef(i,j,k-1,1)))
     !D_tB_y=-dEx/dz
    end do
   end do
  end do
 endif
 end subroutine rotE_rk4
 !========= FLUID SECTION
!=============================
 subroutine fluid_filter(fdata,i1,if2,jf1,jf2,kf1,kf2,ic1,ic2)
  real(dp),intent(inout) :: fdata(:,:,:,:)
  integer,intent (in) :: i1,if2,jf1,jf2,kf1,kf2,ic1,ic2 
  real(dp) :: aph
  integer :: i,ii,j,k,ng,ic,j01,j02,k01,k02

!  filtered data overwrite input data
!===========================
!  filtering data along the x coordinte  i1=3  if2 assigned on input data available at if2+1,if2+2 grid point
!===========================================
  j01=jf1-2
  if(yl_bd)j01=jf1
  j02=jf2+2
  if(yr_bd)j02=jf2
  if(ndim==2)then
   k01=kf1
   k02=kf2
  endif
  if(ndim==3)then
   k01=kf1-2
   if(zl_bd)k01=kf1
   k02=kf2+2
   if(zr_bd)k02=kf2
  endif
!===================
 aph=filt_coeff(0)
 ng=if2-4
 do ic=ic1,ic2
  do k=k01,k02
   do j=j01,j02
    do i=i1+2,if2
     ii=i-4
     ww0(ii,j)=filt_coeff(4)*fdata(i,j,k,ic)+&
                filt_coeff(5)*(fdata(i-1,j,k,ic)+fdata(i+1,j,k,ic))+&
                filt_coeff(6)*(fdata(i-2,j,k,ic)+fdata(i+2,j,k,ic))
    end do
    i=i1+1
    fdata(i,j,k,ic)=0.5*fdata(i,j,k,ic)+0.25*(fdata(i+1,j,k,ic)+fdata(i-1,j,k,ic))
!==============
   end do
                           !call tridx_inv
   do j=j01,j02
    do i=i1+2,if2
     ii=i-4
     fdata(i,j,k,ic)=ww0(ii,j)
   end do
  end do
   end do
  end do
!============== filtering along the y coordinate
 do ic=ic1,ic2
  do k=k01,k02
   do j=j01+2,j02-2     !jf1,jf2 for interior points
    do i=i1,if2
     ii=i-2
     ww0(ii,j)=filt_coeff(4)*fdata(i,j,k,ic)+&
                filt_coeff(5)*(fdata(i,j-1,k,ic)+fdata(i,j+1,k,ic))+&
                filt_coeff(6)*(fdata(i,j-2,k,ic)+fdata(i,j+2,k,ic))
    end do
   end do
!==============
   do j=j01+2,j02-2
    do i=i1,if2
     ii=i-2
     fdata(i,j,k,ic)=ww0(ii,j)
    end do
  end do
  end do
 end do
 if(ndim <3)return
!============== filtering along the z coordinate
 do ic=ic1,ic2
  do j=j01,j02
   do k=k01+2,k02-2
    do i=i1,if2
     ii=i-2
     ww0(ii,k)=filt_coeff(4)*fdata(i,j,k,ic)+&
                filt_coeff(5)*(fdata(i,j,k-1,ic)+fdata(i,j,k+1,ic))+&
                filt_coeff(6)*(fdata(i,j,k-2,ic)+fdata(i,j,k+2,ic))
    end do
   end do
!==============
   do k=k01+2,k02-2
    do i=i1,if2
     ii=i-2
     fdata(i,j,k,ic)=ww0(ii,k)
    end do
   end do
   end do
  end do

 contains
  subroutine tridx_inv
   real(dp) :: bet
  do j=jf1,jf2
   dw(1)=0.0
   bet=1.
   ww0(1,j)=ww0(1,j)/bet
   do i=2,ng-1
    dw(i)=aph/bet
    bet=1.-aph*dw(i)
    ww0(i,j)=(ww0(i,j)-aph*ww0(i-1,j))/bet
   end do
   i=ng
   dw(i)=aph/bet
   bet=1.-aph*dw(i)
   ww0(i,j)=(ww0(i,j)-aph*ww0(i-1,j))/bet
   do i=ng-1,1,-1
    ww0(i,j)=ww0(i,j)-dw(i+1)*ww0(i+1,j)
   end do
  end do

  end subroutine tridx_inv
 end subroutine fluid_filter
    
 subroutine weno5(nc,i0,ng)
 integer,intent(in)  :: nc,i0,ng
 real(dp) :: pl(0:2),pr(0:2),dw(0:3),sl(0:2),sr(0:2),omgl(0:2),vv,s0,s1,ssl,ssr
 real(dp) :: vl,vr
 real(dp),parameter :: eps=1.e-06,cs2=13./12.
 real(dp),dimension(6),parameter :: lw5=(/-3./8.,7./8.,1./8.,3./8.,5./8.,-1./8./)
 real(dp),dimension(6),parameter :: rw5=(/-1./8.,5./8.,3./8.,1./8.,7./8.,-3./8./)
 real(dp),dimension(3),parameter :: dw5=(/1./16.,5./8.,5./16./) !?Check if correct
 integer :: i,l,ic
 ! enter data [i0,ng]

 do ic=1,nc
  i=i0+2
  dw(0)=var(i-1,ic)-var(i-2,ic)    !Dv_{i-2}
  dw(1)=var(i,ic)-var(i-1,ic)
  dw(2)=var(i+1,ic)-var(i,ic)
  do i=i0+2,ng-2
   dw(3)=var(i+2,ic)-var(i+1,ic)     !Dv_{i+1}
   ssl=0.0;ssr=0.0
   s0=0.0;s1=0.0
   call smooth_ind
   do l=0,2
    pl(l)=lw5(2*l+1)*dw(l)+lw5(2*l+2)*dw(l+1)
    sl(l)=dw5(l+1)*omgl(l)
    ssl=ssl+sl(l)
    s0=s0 +sl(l)*pl(l)
    pr(l)=rw5(2*l+1)*dw(l)+rw5(2*l+2)*dw(l+1)
    sr(l)=dw5(3-l)*omgl(l)
    ssr=ssr+sr(l)
    s1=s1 +sr(l)*pr(l)
   end do
   wl(i,ic)=var(i,ic)+s0/ssl
   wr(i-1,ic)=var(i,ic)-s1/ssr
   dw(0)=dw(1)
   dw(1)=dw(2)
   dw(2)=dw(3)
  end do
 !  wl[i0+2,ng-2]   wr[i0+1,ng-3]


 end do           !STORES LxF flux on var arrays
 do i=i0+2,ng-3
  vl=wl(i,nc)
  vr=wr(i,nc)
  vv=abs(vl+vr)
  do ic=1,nc-1
   var(i,ic)=vl*wl(i,ic)+vr*wr(i,ic)-vv*(wr(i,ic)-wl(i,ic))
   var(i,ic)=0.5*var(i,ic)
  end do
 end do
 ! EXIT FLUX (L,R) [i0+2,ng-3]
 contains
 subroutine smooth_ind
 real(dp) :: der1(0:2),der2(0:2),sfact
 integer :: k,m
 do k=0,2
  m=1-k
  der2(k)=dw(k+1)-dw(k)
  der1(k)=0.5*(dw(k)+dw(k+1))+m*der2(k)
  sfact=eps+der1(k)*der1(k)+cs2*der2(k)*der2(k)
  omgl(k)=1./(sfact*sfact)
 end do
 end subroutine smooth_ind
 end subroutine weno5
!=================================
 subroutine nc_fluid_density_momenta(flx,ef,i1,n1p,j1,n2p,k1,n3p,fcomp,aphx,aphy,aphz)
 real(dp),intent(in) :: flx(:,:,:,:)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,fcomp
 integer(kind=4) :: flux_ind
 real(dp),intent(in) :: aphx,aphy,aphz
 integer :: i,ii,j,k,ic,j01,j02,k01,k02
 real(dp) :: vx,vy,vz,shy,shz
  real(dp) :: dw(3),sl(2),sr(2),omgl(2),vv,s0
 real(dp) :: vl,vr
 real(dp),parameter :: eps=1.e-06
 real(dp),dimension(2),parameter :: w03=(/1./3.,2./3./)
 real(dp),dimension(3),parameter :: lder=(/0.5,-2.,1.5/)
 real(dp),dimension(3),parameter :: rder=(/-1.5,2.,-0.5/)
	! Uncomment to change weno weights
 !real(dp),dimension(4),parameter :: lder4=(/1./6.,-1.,0.5,1./3./)  ![i-2,i+1] stencil
 !real(dp),dimension(4),parameter :: rder4=(/-1./3.,-0.5,1.,-1./6./)![i-1,i+2] stencil
!=========================
! Enter primitive variables in flux array flx(Px,Py,Pz,den,vx,vy,vz)
! flcomp=fcomp+ndim components
 flux_ind=1     !=1 for pure upwind   
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
    s0=sign(one_dp,vv)        !s0=1*sign(vv)
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
!====================================
 end subroutine nc_fluid_density_momenta
 !================================
 !================================
 subroutine cons_fluid_density_momenta(ef,flx,i1,n1p,j1,n2p,k1,n3p,fcomp,aphx,aphy,aphz)
  real(dp),intent(inout) :: ef(:,:,:,:),flx(:,:,:,:)
  integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,fcomp
  real(dp),intent(in) :: aphx,aphy,aphz
  integer :: i,ii,j,k,ic,j01,j02,k01,k02
  real(dp) :: vx,vy,vz
  real(dp),dimension(3),parameter :: lder=(/0.5,-2.,1.5/)
  real(dp),dimension(3),parameter :: rder=(/-1.5,2.,-0.5/)
		! Uncomment to change weno weights
  !real(dp),dimension(4),parameter :: lder4=(/1./6.,-1.,0.5,1./3./)  ![i-2,i+1] stencil
  !real(dp),dimension(4),parameter :: rder4=(/-1./3.,-0.5,1.,-1./6./)![i-1,i+2] stencil
!=========================
! Enter conservative variables in flux array (ux,uy,uz,den,vx,vy,vz)
! fcomp+ndim components
  j01=j1
  j02=n2p
  k01=k1
  k02=n3p
! momenta-density
  do k=k1,n3p
   do j=j1,n2p
    do ic=1,fcomp+1
     do i=i1,n1p
      var(i,ic)=flx(i,j,k,ic)
     end do
    end do
   call weno3(fcomp+1,i1,n1p)   !Exit ww0() first derivative
   do ic=1,fcomp
    do i=i1+2,n1p-2
     ef(i,j,k,ic)=ef(i,j,k,ic)+aphx*(var(i,ic)-var(i-1,ic))
    end do
   end do
  end do
 end do
!========== density flux
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
    call weno3(fcomp+1,j01-2,j02+2)    !rec[flux][j01-1,ij02], [j1-1,n2p] for interior points
    do ic=1,fcomp
     do j=j01,j02
      ef(i,j,k,ic)=ef(i,j,k,ic)+aphy*(var(j,ic)-var(j-1,ic))
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
    call weno3(fcomp+1,k01-2,k02+2)    !rec[flux][j01-1,ij02], [j1-1,n2p] for interior points
    do ic=1,fcomp
     do k=k01,k02
      ef(i,j,k,ic)=ef(i,j,k,ic)+aphz*(var(k,ic)-var(k-1,ic))
     end do
    end do
   end do
  end do
!=================================
 contains
 subroutine weno3(nc,i1,np)
  integer,intent(in)  :: nc,i1,np
  real(dp) :: dw(3),sl(2),sr(2),omgl(2),vv,s0
  real(dp) :: vl,vr
  real(dp),parameter :: eps=1.e-06
  real(dp),dimension(2),parameter :: w03=(/1./3.,2./3./)
  !real(dp),dimension(2),parameter :: w03=(/0.1250,0.3750/)
!  enter data [i1,np]  
  integer :: i,l,ic

!=======ENTER DATA [i1,np]
!wl_{i+1/2}in range [i=i1+1,np-1] 
!wr_{i+1/2} in range[i=i1,np-2]
!(wr+wl)_{i+1/2} in common range [i1+1,np-2]
! Dw first derivative in range[i1+2,np-2]
!===========================================

  do ic=1,nc-1
   do i=i1,np
    var(i,ic)=var(i,ic)*var(i,nc)
       end do
      end do
  do ic=1,nc
   do i=i1+1,np-1
    dw(1)=var(i,ic)-var(i-1,ic)    !DW_{i-1}
    dw(2)=var(i+1,ic)-var(i,ic)    !DW_i
    omgl(1)=1./(dw(1)*dw(1)+eps)
    omgl(2)=1./(dw(2)*dw(2)+eps)
    !omgl(1)=omgl(1)*omgl(1)
    !omgl(2)=omgl(2)*omgl(2)
    sl(1)=w03(1)*omgl(1)
    sl(2)=w03(2)*omgl(2)
    sr(1)=w03(2)*omgl(1)
    sr(2)=w03(1)*omgl(2)
    s0=sl(1)+sl(2)
    wl(i,ic)=var(i,ic)+0.5*(dw(1)*sl(1)+dw(2)*sl(2))/s0
    s0=sr(1)+sr(2)
    wr(i-1,ic)=var(i,ic)-0.5*(dw(1)*sr(1)+dw(2)*sr(2))/s0
    !s0=0.5*(sign(dw(1),1.)+sign(dw(2),1.))
    !s0=s0*min(abs(dw(1)),abs(dw(2)))
    !wl(i,ic)=var(i,ic)+0.5*s0
    !wr(i-1,ic)=var(i,ic)-0.5*s0
   end do
  end do
!===================================
   do ic=1,nc-1
    do i=i1+1,np-2
     vv=wr(i,nc)+wl(i,nc)
     s0=sign(1.,vv)        !s0=1*sign(vv)
     !if(vv>0.0)then
      var(i,ic)=max(0.,s0)*wl(i,ic)-min(0.,s0)*wr(i,ic)
     !else
     ! var(i,ic)=wr(i,ic)
     !endif
     !var(i,ic)=var(i,ic)-0.5*vv*(wr(i,ic)-wl(i,ic))
   end do
  end do
 end subroutine weno3
 end subroutine cons_fluid_density_momenta
 !================================
 !================================
 subroutine rk_fluid_density_momenta(ef,flx,i1,n1p,j1,n2p,k1,n3p,fcomp,aphx,aphy)
 real(dp),intent(inout) :: ef(:,:,:,:),flx(:,:,:,:)

 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,fcomp
 real(dp),intent(in) :: aphx,aphy
 integer :: i,ii,j,k,ic,j01,j02
 real(dp) :: vx,vy
	! Uncomment to change weno weights
 !real(dp),dimension(4),parameter :: lder_0=(/-3./8.,13./8.,-25./8.,15./8./)
 !real(dp),dimension(4),parameter :: rder_0=(/-15./8.,25./8.,-13./8.,3./8./)
 real(dp),dimension(4),parameter :: lder_1=(/1./8.,-7./8.,3./8.,3./8./)
 real(dp),dimension(4),parameter :: rder_1=(/-3./8.,-3./8.,7./8.,-1./8./)
  real(dp),dimension(3),parameter :: lder3=(/0.5,-2.,1.5/)
  real(dp),dimension(3),parameter :: rder3=(/-1.5,2.,-0.5/)
!=========================
! Enter conservative variables ef=(ux,uy,uz,den]  fcomp dimension
! Enter variables ef1=(ux,uy,uz,den,vx,vy,vz]     flcomp dimension
 j01=j1
 j02=n2p

  call x_boundary_closure ! boundary closere at three-point [i1, i1+1,i1+2] and  [n1p,n1p-1,n1p-2]
  do k=k1,n3p
   do j=j1,n2p
   do i=i1,n1p
     var(i,1:fcomp+1)=flx(i,j,k,1:fcomp+1)  !data [i1:n1p]
   end do
    call weno5(fcomp+1,i1,n1p)   !exit flux(i+1/2) [i1+2,n1p-3]=>
    do ic=1,fcomp
     do i=i1+3,n1p-3
      ef(i,j,k,ic)=ef(i,j,k,ic)+aphx*(var(i,ic)-var(i-1,ic))
  end do
 end do
   end do
  end do
!=============== y-flux
  if(pe0y)call y_left_boundary_closure  !boundary at j=j1, j1+1 j1+2 ==> j01=j1+3
  if(pe1y)call y_right_boundary_closure !boundary at n2p,n2p-1,n2p-2  ==> j02=n2p-3
  do k=k1,n3p
   do i=i1,n1p
    do j=j01-3,j02+3
     var(j,1:fcomp)=flx(i,j,k,1:fcomp)
     var(j,fcomp+1)=flx(i,j,k,fcomp+2)
 end do
    call weno5(fcomp+1,j01-3,j02+3)   !exit flux var[j1-1,n2p]
    do ic=1,fcomp
   do j=j01,j02
      ef(i,j,k,ic)=ef(i,j,k,ic)+aphy*(var(j,ic)-var(j-1,ic)) !interior flux der. [j1+1,n2p+1]
     end do
    end do
   end do
  end do
!=================================
 contains
 subroutine x_boundary_closure
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    vx=flx(i,j,k,fcomp+1)
    if(vx <0.0)then
     do ic=1,fcomp
      ii=1
      do i=i1,i1+2
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
      end do
      ef(i1,j,k,ic)=ef(i1,j,k,ic)+aphx*(dot_product(rder3(1:3),ww(1:3)))
 end do
    endif
    i=i1+1
    vx=flx(i,j,k,fcomp+1)
    if(vx <0.0)then
     do ic=1,fcomp
      ii=1
      do i=i1,i1+3
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
    end do
   end do
      ef(i1+1,j,k,ic)=ef(i+1,j,k,ic)+aphx*(dot_product(rder_1(1:4),ww(1:4)))
    else
     do ic=1,fcomp
      ww(1)=flx(i1,j,k,ic)*flx(i1,j,k,fcomp+1)
      ww(2)=flx(i1+1,j,k,ic)*flx(i1+1,j,k,fcomp+1)
      ef(i1+1,j,k,ic)=ef(i+1,j,k,ic)+aphx*(ww(1)-ww(2))
  end do
    end if
    i=i1+2
    vx=flx(i,j,k,fcomp+1)
    if(vx <0.0)then
     do ic=1,fcomp
      ii=1
      do i=i1+1,i1+4
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
    end do
      ef(i1+1,j,k,ic)=ef(i+1,j,k,ic)+aphx*(dot_product(rder_1(1:4),ww(1:4)))
   end do
    else
     do ic=1,fcomp
      ii=1
      do i=i1,i1+2
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
  end do
      ef(i1+2,j,k,ic)=ef(i+2,j,k,ic)+aphx*(dot_product(lder3(1:3),ww(1:3)))
    end do
    end if
   end do
  end do
!====================
  do k=k1,n3p
   do j=j1,n2p
    i=n1p-2
    vx=flx(i,j,k,fcomp+1)
    if(vx >0.0)then
     do ic=1,fcomp
      ii=1
      do i=n1p-4,n1p-1
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
 end do
      ef(n1p-2,j,k,ic)=ef(n1p-2,j,k,ic)+aphx*(dot_product(lder_1(1:4),ww(1:4)))
    end do
    else
     do ic=1,fcomp
      ii=1
      do i=n1p-2,n1p
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
   end do
      ef(n1p-2,j,k,ic)=ef(n1p-2,j,k,ic)+aphx*(dot_product(rder3(1:3),ww(1:3)))
  end do
 endif
    i=n1p-1
    vx=flx(i,j,k,fcomp+1)
    if(vx >0.0)then
     do ic=1,fcomp
      ii=1
      do i=n1p-3,n1p
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
    end do
      ef(n1p-1,j,k,ic)=ef(n1p-1,j,k,ic)+aphx*(dot_product(lder_1(1:4),ww(1:4)))
   end do
    else
     do ic=1,fcomp
      ef(n1p-1,j,k,ic)=ef(n1p-1,j,k,ic)+aphx*(flx(n1p,j,k,ic)*flx(n1p,j,k,fcomp+1)-&
                                             flx(n1p-1,j,k,ic)*flx(n1p-1,j,k,fcomp+1))
  end do
 endif
    i=n1p
    vx=flx(i,j,k,fcomp+1)
    if(vx >0.0)then
     do ic=1,fcomp
      ii=1
      do i=n1p-2,n1p
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+1)
       ii=ii+1
    end do
      ef(n1p,j,k,ic)=ef(n1p,j,k,ic)+aphx*(dot_product(lder3(1:3),ww(1:3)))
   end do
    endif
  end do
 end do
 end subroutine x_boundary_closure
 subroutine y_left_boundary_closure
  do k=k1,n3p
   do i=i1,n1p
    j=j1
    vy=flx(i,j,k,fcomp+2)
    if(vy <0.0)then
     do ic=1,fcomp
      ii=1
      do j=j1,j1+2
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
      end do
      ef(i,j1,k,ic)=ef(i,j1,k,ic)+aphy*(dot_product(rder3(1:3),ww(1:3)))
     end do
    endif
   end do
  end do
  do k=k1,n3p
   do i=i1,n1p
    j=j1+1
    vy=flx(i,j,k,fcomp+2)
    if(vy <0.0)then
     do ic=1,fcomp
      ii=1
      do j=j1+1,j1+4
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
 end do
      ef(i,j1+1,k,ic)=ef(i,j1+1,k,ic)+aphy*(dot_product(rder_1(1:4),ww(1:4)))
   end do
    else
     do ic=1,fcomp
      ii=1
      ww(ii)=flx(i,j1,k,ic)*flx(i,j1,k,fcomp+2)
      ii=2
      ww(ii)=flx(i,j1+1,k,ic)*flx(i,j1+1,k,fcomp+2)
      ef(i,j1+1,k,ic)=ef(i,j1+1,k,ic)+aphy*(ww(1)-ww(2))
     enddo
    endif
    j=j1+2
    vy=flx(i,j,k,fcomp+2)
    if(vy <0.0)then
     do ic=1,fcomp
      ii=1
      do j=j1+1,j1+4
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
  end do
      ef(i,j1+2,k,ic)=ef(i,j1+2,k,ic)+aphy*(dot_product(rder_1(1:4),ww(1:4)))
 end do
    else
     do ic=1,fcomp
      ii=1
      do j=j1,j1+2
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
    end do
      ef(i,j1+2,k,ic)=ef(i,j1+2,k,ic)+aphy*(dot_product(lder3(1:3),ww(1:3)))
   end do
    endif
  end do
 end do
  j01=j1+3
 end subroutine y_left_boundary_closure

 subroutine y_right_boundary_closure
  do k=k1,n3p
    do i=i1,n1p
    j=n2p-2
    vy=flx(i,j,k,fcomp+2)
    if(vy >0.0)then
     do ic=1,fcomp
      ii=1
      do j=n2p-4,n2p-1
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
      end do
      ef(i,n2p-2,k,ic)=ef(i,n2p-2,k,ic)+aphy*(dot_product(lder_1(1:4),ww(1:4)))
    end do
    else
     do ic=1,fcomp
      ii=1
      do j=n2p-2,n2p
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
   end do
      ef(i,n2p-2,k,ic)=ef(i,n2p-2,k,ic)+aphy*(dot_product(rder3(1:3),ww(1:3)))
  end do
 endif
    j=n2p-1
    vy=flx(i,j,k,fcomp+2)
    if(vy >0.0)then
     do ic=1,fcomp
      ii=1
      do j=n2p-3,n2p
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
    end do
      ef(i,n2p-1,k,ic)=ef(i,n2p-1,k,ic)+aphy*(dot_product(lder_1(1:4),ww(1:4)))
   end do
    else
     do ic=1,fcomp
      ii=1
      j=n2p-1
      ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
      ii=2
      j=n2p
      ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
      ef(i,n2p-1,k,ic)=ef(i,n2p-1,k,ic)+aphy*(ww(2)-ww(1))
  end do
 endif
    j=n2p
    vy=flx(i,j,k,fcomp+2)
    if(vy >0.0)then
     do ic=1,fcomp
      ii=1
      do j=n2p-2,n2p
       ww(ii)=flx(i,j,k,ic)*flx(i,j,k,fcomp+2)
       ii=ii+1
    end do
      ef(i,n2p,k,ic)=ef(i,n2p,k,ic)+aphy*(dot_product(lder3(1:3),ww(1:3)))
   end do
    endif
  end do
 end do
  j02=n2p-3
 end subroutine y_right_boundary_closure
 end subroutine rk_fluid_density_momenta
 !===================================
 end module grid_fields
