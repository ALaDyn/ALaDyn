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

 module grid_fields

 use precision_def
 use der_lib
 use fstruct_data
 use all_param
 use fft_lib
 use parallel

 implicit none
 real(dp),allocatable :: ww(:),ww0(:,:),wft(:,:,:),sf1(:),sf2(:)
 real(dp),allocatable :: x1_pml(:),x1h_pml(:)
 real(dp),allocatable :: x2_pml(:),x2h_pml(:)
 real(dp),allocatable :: sxp_pml(:),shxp_pml(:)
 real(dp),allocatable :: sxm_pml(:),shxm_pml(:)
 real(dp) :: sigm_opt
 contains

 subroutine w_alloc
 real(dp) :: cfl_loc,lxb,lyb,a0,alp
 integer :: ii,ndmx,nd1mx

 !============================
 ! allocate auxiliary arrays ww() dw() and
 ndmx=max(nx,ny,nz)
 nd1mx=max(nx_loc,nx1_loc)
 allocate(ww(ndmx+2),ww0(nx+2,max(ny,nz)))
 cfl_loc=0.0

 if(LPf_ord >0)then
  cfl_loc=cfl
  if(ndim >1) cfl_loc=cfl*yx_rat/sqrt(yx_rat*yx_rat+ndim-1.)
 endif
 call set_mat_der(cfl_loc,nx,ny,nz,ndim,&
  ibx,der_ord,ifilt,iform)
 if(Envelope)then
  lxb=x(nx+1)-x(1)
  lyb=y(ny+1)-y(1)
  call ftw_init(nx,1,1,1)      !for 1D FFT
  call set_ftgrid(nx,1,1,1,lxb,lyb)
  allocate(wft(nx,2,1))
  allocate(sf1(nx/2+1),sf2(nx/2+1))
  if(der_ord==4)then
   do ii=1,nx/2+1
    sf1(ii)=dx_inv*sin(dx*akx(ii))*(4./3.-cos(dx*akx(ii))/3.)
    sf2(ii)=sf1(ii)*sf1(ii)
   end do
   allocate(mat_env(nx,5))
   allocate(amat(nx,nx))
   alp=0.25
   a0=3.*alp
   call set_mat_env5(oml,alp,a0,dx_inv,nx)
   deallocate(amat)
  else
   do ii=1,nx/2+1
    sf1(ii)=dx_inv*sin(dx*akx(ii))
    sf2(ii)=sf1(ii)*sf1(ii)
   end do
  endif
  !                        numerical first derivative (f(i+1)-f(i-1))/2Dx
  !call set_mat_env2(b0,c0,nx)
  !if(pe0)call env_test(oml,dx_inv,nx)
 endif
 !============================
 end subroutine w_alloc
 !====================================
 subroutine matenv_inv(n,nc)
 integer,intent(in) :: n,nc
 integer :: ix,ic

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
 subroutine trid_der1(a,b1,c1,an,bn,n,ic1,ic2,ord)
 real(dp),intent(in) :: a,b1,c1,an,bn
 integer,intent(in) :: n,ic1,ic2,ord
 integer :: k,ic
 real(dp) :: bet,a_0,c_0
 !==========================
 ! Solves
 ! a_0*ww(i-1)+ww(i)+c_0*ww(i+1)=u(i), i=2,3,..,n-1
 ! at the first row (1+2a)*ww(1)-2a*ww(2)=u(1)
 ! at the n-last row (1-2a)*ww(n-1)+2a*ww(n)=u(n)
 ! first order boundary clusure
 !===============================
 a_0=-a
 c_0=a
 if(ord >0)then
  do ic=ic1,ic2
   ww0(1,ic)=ww0(1,ic)+ww0(2,ic)
   ww0(n,ic)=ww0(n,ic)+ww0(n-1,ic)
  end do
 endif
 do ic=ic1,ic2
  dw(1)=0.0
  bet=b1
  ww0(1,ic)=ww0(1,ic)/bet
  k=2
  dw(k)=c1/bet
  bet=1.-a_0*dw(k)
  ww0(k,ic)=(ww0(k,ic)-a_0*ww0(k-1,ic))/bet
  do k=3,n-1
   dw(k)=c_0/bet
   bet=1.-a_0*dw(k)
   ww0(k,ic)=(ww0(k,ic)-a_0*ww0(k-1,ic))/bet
  end do
  k=n
  dw(k)=c_0/bet
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
 subroutine field_xadvect(ef,i1,n1p,j1,n2p,k1,n3p,ic1,ic2,aphx,v_adv)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p,ic1,ic2
 real(dp),intent(in) :: aphx,v_adv
 integer :: i,j,k,ii,n1,n1_loc,ic,ind
 real(dp) :: aphx_adv,b1,c1,an,bn
 !=====================
 ! APPLIES also for prlx=.true. (MPI x decomposition)
 !=============================================
 ! for positive advection v_adv>0
 ! solves Df/Dt=-v_adv*Df/Dx
 ! In comoving system Maxwell eqs. have v_adv <0
 aphx_adv=0.25*v_adv*aphx !v_adv*(Dt/Dx)/4
 !b_cmp=1.5+0.25*aphx*aphx*aphx
 !a_cmp=0.5*(b_cmp-1.)
 ind=1
 b1=1.-2.*aphx_adv
 c1=2.*aphx_adv
 !bn=1.+aphx_adv
 !an=-c1
 bn=1.
 an=0.0

 !=====================
 n1_loc=loc_xgrid(imodx)%ng
 n1=n1_loc
 !=========================
 if(prlx)n1=nx
 ! Advection in x-coordinate E^n+1=E^n-aphx_adv*[D_xE^n+D_xE^{n+1}]
 ! (1+aphx_adv)E^{n+1}=(1-aphx_adv*D_x)E^n
 !=====================
 if(pex0)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(i1-1,j,k,ic)=2.*ef(i1,j,k,ic)-ef(i1+1,j,k,ic)
    enddo
   end do
  end do
 endif
 if(pex1)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     ef(n1p+1,j,k,ic)=ef(n1p-1,j,k,ic)
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
     call trid_der1(aphx_adv,b1,c1,an,bn,n1,1,1,0)
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
     ww0(ii,1)=ef(i,j,k,ic)-aphx_adv*(&
      ef(i+1,j,k,ic)-ef(i-1,j,k,ic))
    end do
    call trid_der1(aphx_adv,b1,c1,an,bn,ii,1,1,0)
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
 !               2D Cyl
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
 subroutine initial_beam_fields(ef1,i1,nxp,j1,nyp,k1,nzp,g2,bet)
 real(dp),intent(inout) :: ef1(:,:,:,:)
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp
 real(dp),intent(in) :: g2,bet
 integer :: i,j,k,ic,jj,kk
 real(dp) :: a1,b1,sdhy,sdhz

 ! Enter
 ! in ef1(1) enters pot_b(i,j,k) => (Ex,Ey, Ez)
 ! in ef1(2) enters Jxb(i,j,k) => ef1(4)=Jxb[i+1/2,j,k]
 ! => ef1(2) pot_b(i+1/2,j,k)==> By, Bz

 b1=-1./16.
 a1=0.5-b1
 !------------------------------------
 ic=1
 if(pe1y)then
  j=nyp
  do k=k1,nzp
   do i=i1,nxp
    ef1(i,j+1,k,ic)=3.*(ef1(i,j,k,ic)-ef1(i,j-1,k,ic))+ef1(i,j-2,k,ic)
   end do
  end do
 endif
 if(pe1z)then
  k=nzp
  do j=j1,nyp
   do i=i1,nxp
    ef1(i,j,k+1,ic)=3.*(ef1(i,j,k,ic)-ef1(i,j,k-1,ic))+ef1(i,j,k-2,ic)
   end do
  end do
 endif
 if(pex1)then
  do k=k1,nzp+1
   do j=j1,nyp+1
    ef1(nxp+1,j,k,1)=ef1(nxp,j,k,1)
    ef1(nxp+1,j,k,2)=3.*(ef1(nxp,j,k,2)-ef1(nxp-1,j,k,2))+ef1(nxp-2,j,k,2)
   end do
  end do
 endif
 ! defines pot(i+1/2,j,k) in ef1(3) and Jb(i+1/2) in ef1(4)
 do k=k1,nzp+1
  do j=j1,nyp+1
   ef1(i1,j,k,3)=0.5*(ef1(i1,j,k,1)+ef1(i1+1,j,k,1))
   ef1(nxp,j,k,3)=0.5*(ef1(nxp,j,k,1)+ef1(nxp+1,j,k,1))
   ef1(i1,j,k,4)=0.5*(ef1(i1,j,k,2)+ef1(i1+1,j,k,2))
   ef1(nxp,j,k,4)=0.5*(ef1(nxp,j,k,2)+ef1(nxp+1,j,k,2))
   do i=i1+1,nxp-1
    ef1(i,j,k,3)=a1*(ef1(i,j,k,1)+ef1(i+1,j,k,1))+b1*(&
     ef1(i-1,j,k,1)+ef1(i+2,j,k,1))
    ef1(i,j,k,4)=a1*(ef1(i,j,k,2)+ef1(i+1,j,k,2))+b1*(&
     ef1(i-1,j,k,2)+ef1(i+2,j,k,2))
   end do
  end do
 end do
 ! defines (By,Bz)
 do k=k1,nzp
  kk=k-2
  sdhz=loc_zg(kk,3,imodz)*dz_inv
  do j=j1,nyp
   jj=j-2
   sdhy=loc_yg(jj,3,imody)*dy_inv
   do i=i1,nxp
    ef1(i,j,k,6)=-bet*sdhy*(ef1(i,j+1,k,3)-ef1(i,j,k,3))
    ef1(i,j,k,5)=bet*sdhz*(ef1(i,j,k+1,3)-ef1(i,j,k,3))
   end do
  end do
 end do
 ! defines (EX,Ey,Ez) => ef1(1:3)
 do k=k1,nzp
  kk=k-2
  sdhz=loc_zg(kk,3,imodz)*dz_inv
  do j=j1,nyp
   jj=j-2
   sdhy=loc_yg(jj,3,imody)*dy_inv
   do i=i1,nxp
    ef1(i,j,k,2)=-sdhy*(ef1(i,j+1,k,1)-ef1(i,j,k,1))
    ef1(i,j,k,3)=-sdhz*(ef1(i,j,k+1,1)-ef1(i,j,k,1))
    ef1(i,j,k,1)=-dx_inv*(ef1(i+1,j,k,1)-ef1(i,j,k,1))/g2
   end do
  end do
 end do
 end subroutine initial_beam_fields
 !===========================================
 subroutine initial_beam_potential_and_fields(ef1,ef0,ef,&
  i1,nxp,j1,nyp,k1,nzp,dt_loc,g2,bet)
 real(dp),intent(inout) :: ef1(:,:,:,:),ef0(:,:,:,:),ef(:,:,:,:)
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp
 real(dp),intent(in) :: dt_loc,g2,bet
 integer :: i,j,k,ic,jj,kk
 real(dp) :: a1,b1,sdhy,sdhz,aphx

 ! Enter
 ! in ef1(1) enters pot_b(i,j,k) => (Ex,Ey, Ez)
 ! in ef1(3) enters Jxb(i,j,k) = ef1(4)=Jxb[i+1/2,j,k]
 ! => ef1(2) pot_b(i+1/2,j,k)==> By, Bz

 b1=-1./16.
 a1=0.5-b1
 !------------------------------------
 aphx=dt_loc*dx_inv
 ef0=0.0

 ! advects phi^n => phi^{n-1}

 ic=4
 do k=1,nzp
  do j=j1,nyp
   do i=i1,nxp
    ef1(i,j,k,ic)=ef1(i,j,k,1)
    ef0(i,j,k,ic)=ef1(i,j,k,1)
   end do
  end do
 end do
 ! phi^n at ef1(4)
 call field_xadvect(ef0,i1,nxp,j1,nyp,k1,nzp,ic,ic,aphx,-bet)
 if(pe1y)then
  j=nyp
  do k=k1,nzp
   do i=i1,nxp
    ef1(i,j+1,k,ic)=3.*(ef1(i,j,k,ic)-ef1(i,j-1,k,ic))+ef1(i,j-2,k,ic)
   end do
  end do
 endif
 if(pe1z)then
  k=nzp
  do j=j1,nyp
   do i=i1,nxp
    ef1(i,j,k+1,ic)=3.*(ef1(i,j,k,ic)-ef1(i,j,k-1,ic))+ef1(i,j,k-2,ic)
   end do
  end do
 endif
 do k=k1,nzp+1
  do j=j1,nyp+1
   ef1(nxp+1,j,k,ic)=3.*(ef1(nxp,j,k,ic)-ef1(nxp-1,j,k,ic))+ef1(nxp-2,j,k,ic)
  end do
 end do
 ! defines Ax(i+1/2,j,k) iat leven n in ef1(1)
 do k=k1,nzp+1
  do j=j1,nyp+1
   ef1(i1,j,k,1)=0.5*(ef1(i1,j,k,ic)+ef1(i1+1,j,k,ic))
   ef1(nxp,j,k,1)=0.5*(ef1(nxp,j,k,ic)+ef1(nxp+1,j,k,ic))
   do i=i1+1,nxp-1
    ef1(i,j,k,1)=a1*(ef1(i,j,k,ic)+ef1(i+1,j,k,ic))+b1*(&
     ef1(i-1,j,k,ic)+ef1(i+2,j,k,ic))
   end do
   do i=i1,nxp
    ef1(i,j,k,1)=bet*ef1(i,j,k,1)
   end do
  end do
 end do
 ! ef1(1)=[Ax(i+1/2)]^n defines (By,Bz)
 do k=k1,nzp
  kk=k-2
  sdhz=loc_zg(kk,3,imodz)*dz_inv
  do j=j1,nyp
   jj=j-2
   sdhy=loc_yg(jj,3,imody)*dy_inv
   do i=i1,nxp
    ef(i,j,k,4)=0.0
    ef(i,j,k,5)=sdhz*(ef1(i,j,k+1,1)-ef1(i,j,k,1))
    ef(i,j,k,6)=-sdhy*(ef1(i,j+1,k,1)-ef1(i,j,k,1))
   end do
  end do
 end do
 !=======================
 ! Defines Ax(i+1/2) ef0(1) at level n-1/2 ief1(1) at level n+1/2
 do k=1,nzp
  do j=j1,nyp
   do i=i1+1,nxp
    ef0(i,j,k,1)=ef0(i-1,j,k,1)-(ef1(i,j,k,4)-ef0(i,j,k,4))/aphx
   end do
   do i=i1,nxp
    ef1(i,j,k,1)=2.*ef1(i,j,k,1)-ef0(i,j,k,1)
    ef1(i,j,k,2:3)=0.0
   end do
  end do
 end do
 do k=k1,nzp
  kk=k-2
  sdhz=loc_zg(kk,3,imodz)*dz_inv
  do j=j1,nyp
   jj=j-2
   sdhy=loc_yg(jj,3,imody)*dy_inv
   do i=i1,nxp
    ef(i,j,k,2)=-sdhy*(ef1(i,j+1,k,4)-ef1(i,j,k,4))
    ef(i,j,k,3)=-sdhz*(ef1(i,j,k+1,4)-ef1(i,j,k,4))
    ef(i,j,k,1)=-dx_inv*(ef1(i+1,j,k,4)-ef1(i,j,k,4))/g2
   end do
  end do
 end do

 end subroutine initial_beam_potential_and_fields
 ! END SECTION FOR initial beam fields
 !==================================
 ! SECTION for initial fields in ENVELOPE MODEL
 !======================================
 subroutine init_envelope_field(ef,e0,dt_loc,t_loc,tf,tau,&
  wy,xf0,i1,i2)

 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,dt_loc,t_loc,tf,tau,wy,xf0
 integer,intent(in) :: i1,i2
 integer :: j1,j2,k1,k2
 real(dp) :: xx,yy,zz,r2,w2
 real(dp) :: t,tm,zra
 real(dp) :: pih,phi,phi0,phi1,phx
 real(dp) :: A0,Ar,Ai
 integer :: i,j,k,ii,jj,kk


 !enter sigma=0.5*lam0/tau:
 ! inviluppo temporale= cos^2(pi*(t-x))/2tau)
 ! eps=1./k0*wy k0=omega_0=omgl
 ! Ay(i,j,k) complex envelope in paraxial approximation
 !========================
 t=t_loc-tf
 tm=t-dt_loc
 zra=0.5*oml*wy*wy
 pih=0.5*acos(-1.0)
 if(ndim < 3)then
  j1=loc_rgrid(imody)%p_ind(1)
  j2=loc_rgrid(imody)%p_ind(2)
  k=1
  do j=j1,j2
   jj=j-2
   yy=loc_rg(jj,1,imody)
   yy=yy/wy
   r2=yy*yy
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xx=xx-xf0
    phi1=pih*(xx-t)/tau
    if(abs(phi1)>pih)phi1=pih
    phi0=pih*(xx-tm)/tau
    if(abs(phi0)>pih)phi0=pih
    xx=xx/zra
    w2=1./(1.+xx*xx)
    phx=atan(xx)
    phi=phx-xx*r2*w2

    A0=cos(phi1)*cos(phi1)
    Ar=e0*cos(phi)*sqrt(w2)*exp(-w2*r2)
    Ai=e0*sin(phi)*sqrt(w2)*exp(-w2*r2)
    ef(i,j,k,1)=A0*Ar    !Re[Ay](t_loc)
    ef(i,j,k,2)=A0*Ai    !Im[Ay]
    A0=cos(phi0)*cos(phi0)   !t_0-Dt
    ef(i,j,k,3)=A0*Ar
    ef(i,j,k,4)=A0*Ai
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
  zz=zz/wy
  do j=j1,j2
   jj=j-2
   yy=loc_yg(jj,1,imody)
   yy=yy/wy
   r2=(yy*yy+zz*zz)
   do i=i1,i2
    ii=i-2
    xx=loc_xg(ii,1,imodx)
    xx=xx-xf0
    phi1=pih*(t-xx)/tau
    if(abs(phi1)>pih)phi1=pih
    phi0=pih*(tm-xx)/tau
    if(abs(phi0)>pih)phi0=pih
    xx=xx/zra
    w2=1./(1.+xx*xx)
    phx=atan(xx)
    phi=phx-xx*r2*w2

    A0=cos(phi1)*cos(phi1)
    Ar=e0*cos(phi)*sqrt(w2)*exp(-w2*r2)
    Ai=e0*sin(phi)*sqrt(w2)*exp(-w2*r2)
    ef(i,j,k,1)=A0*Ar    !Re[Ay](t_loc)
    ef(i,j,k,2)=A0*Ai    !Im[Ay]
    A0=cos(phi0)*cos(phi0)
    ef(i,j,k,3)=A0*Ar   !Re[Ay](tloc-Dt)
    ef(i,j,k,4)=A0*Ai    !Im[Ay]
   end do
  end do
 end do
 end subroutine init_envelope_field
 !============================
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
  av,source,ic1,ic2,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)

 real(dp),intent(inout) :: av(:,:,:,:)
 real(dp),intent(inout) :: source(:,:,:,:)

 integer,intent(in) :: ic1,ic2,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: dhy,dhz
 integer :: i,j,k,ic,j01,j02,k01,k02
 real(dp) :: dy2_inv,dz2_inv
 !==========================
 dy2_inv=dhy*dhy
 dz2_inv=dhz*dhz
 !===========================
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 if(pe0y)then
  do ic=ic1,ic2
   do k=k1,n3p
    j=j1
    do i=i1,n1p
     av(i,j-1,k,ic)=2.*av(i,j,k,ic)-av(i,j+1,k,ic)
    end do
    do j=j1,j1+1
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dy2_inv*(&
       av(i,j+1,k,ic)-2.*av(i,j,k,ic)+av(i,j-1,k,ic))
     end do
    end do
   end do
  end do
  j01=j1+2
 endif
 if(pe1y)then
  do ic=ic1,ic2
   do k=k1,n3p
    j=n2p
    do i=i1,n1p
     av(i,j+1,k,ic)=2.*av(i,j,k,ic)-av(i,j-1,k,ic)
    end do
    do j=n2p-1,n2p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dy2_inv*(&
       av(i,j+1,k,ic)-2.*av(i,j,k,ic)+av(i,j-1,k,ic))
     end do
    end do
   end do
  end do
  j02=n2p-2
 endif
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j01,j02
    do i=i1,n1p
     source(i,j,k,ic)=source(i,j,k,ic)+dy2_inv*(&
      av(i,j+1,k,ic)-2.*av(i,j,k,ic)+av(i,j-1,k,ic))
    end do
   end do
  end do
 end do
 if(ndim< 3)return
 !====================
 if(pe0z)then
  do ic=ic1,ic2
   k=k1
   do j=j1,n2p
    do i=i1,n1p
     av(i,j,k-1,ic)=2.*av(i,j,k,ic)-av(i,j,k+1,ic)
    end do
   end do
   do k=k1,k1+1
    do j=j1,n2p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dz2_inv*(&
       av(i,j,k+1,ic)-2.*av(i,j,k,ic)+av(i,j,k-1,ic))
     end do
    end do
   end do
  end do
  k01=k1+2
 endif
 if(pe1z)then
  do ic=ic1,ic2
   k=n3p
   do j=j1,n2p
    do i=i1,n1p
     av(i,j,k+1,ic)=2.*av(i,j,k,ic)-av(i,j,k-1,ic)
    end do
   end do
   do k=n3p-1,n3p
    do j=j1,n2p
     do i=i1,n1p
      source(i,j,k,ic)=source(i,j,k,ic)+dz2_inv*(&
       av(i,j,k-1,ic)-2.*av(i,j,k,ic)+av(i,j,k+1,ic))
     end do
    end do
   end do
  end do
  k02=n3p-2
 endif
 do ic=ic1,ic2
  do k=k01,k02
   do j=j1,n2p
    do i=i1,n1p
     source(i,j,k,ic)=source(i,j,k,ic)+dz2_inv*(&
      av(i,j,k+1,ic)-2.*av(i,j,k,ic)+av(i,j,k-1,ic))
    end do
   end do
  end do
 end do
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
  if(Cyl_coord)then
   do k=k1,n3p
    do i=i1,n1p
     envg(i,j-1,k,1)=envg(i,j+1,k,1)
     envg(i,j-2,k,1)=envg(i,j+2,k,1)
    end do
   end do
  else
   do k=k1,n3p
    do i=i1,n1p
     envg(i,j-1,k,1)=2.*envg(i,j,k,1)-envg(i,j+1,k,1)
     envg(i,j,k,3)=0.5*dhy*(envg(i,j+1,k,1)-envg(i,j-1,k,1))
     envg(i,j+1,k,3)=0.5*dhy*(envg(i,j+2,k,1)-envg(i,j,k,1))
    end do
   end do
  endif
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
 real(dp) :: ax1,ax2,ay1,ay2,az1,az2
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
  do k=k1,n3p
   do i=i1,n1p
    envg(i,j+1,k,1)=2.*envg(i,j,k,1)-envg(i,j-1,k,1)
    envg(i,j-1,k,3)=dhy*(envg(i,j,k,1)-envg(i,j-1,k,1))
    envg(i,j,k,3)=dhy*(envg(i,j+1,k,1)-envg(i,j,k,1))
   end do
  end do
  j02=n2p-2
 end if

 if(pe0y)then
  j=j1
  do k=k1,n3p
   do i=i1,n1p
    envg(i,j-1,k,1)=2.*envg(i,j,k,1)-envg(i,j+1,k,1)
    envg(i,j,k,3)=dhy*(envg(i,j+1,k,1)-envg(i,j,k,1))
   end do
  end do
  j01=j1+1
 endif
 do k=k1,n3p
  do j=j01,j02
   do i=i1,n1p
    envg(i,j,k,3)=ay1*(envg(i,j+1,k,1)-envg(i,j,k,1))
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
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k+1,1)=2.*envg(i,j,k,1)-envg(i,j,k-1,1)
    envg(i,j,k-1,4)=dhz*(envg(i,j,k,1)-envg(i,j,k-1,1))
    envg(i,j,k,4)=dhz*(envg(i,j,k+1,1)-envg(i,j,k,1))
   end do
  end do
  k02=n3p-2
 end if
 if(pe0z)then
  k=k1
  do j=j1,n2p
   do i=i1,n1p
    envg(i,j,k-1,1)=2.*envg(i,j,k,1)-envg(i,j,k+1,1)
    envg(i,j,k,4)=dhz*(envg(i,j,k+1,1)-envg(i,j,k,1))
   end do
  end do
  k01=k1+1
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
 end subroutine env_grad
 !==============================
 subroutine env_lpf_solve(curr,evf,ib,i1,n1p,j1,n2p,k1,n3p,&
  om0,dhx,dhy,dhz,dt_loc)
 real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:)
 integer,intent(in) :: ib,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: om0,dhx,dhy,dhz,dt_loc
 integer :: i,j,k,ii,ic,ic1,n1
 real(dp) :: dx1_inv,om2,aph1
 real(dp) :: adv,a,c,b1,c1,an,bn,der2_norm
 !==========================
 ! EXPLICIT INTEGRATION of ENVELOPE EVOLUTION OPERATOR
 !============================

 om2=om0*om0
 dx1_inv=0.5*dhx
 aph1=dx1_inv
 n1=n1p+1-i1
 der2_norm=0.25*dhx*dhx/om2
 a=der2_norm
 c=der2_norm
 adv=1.-2.*der2_norm
 c1=c
 b1=adv+a
 an=a
 bn=adv+c          !symm BC
 !================
 ! Computes the transverse Laplacian of A^{n}=env(1:2) components
 !========and adds to  jc(1:2)= -om2*<q^2*wgh*n/gam_p>*env(1:2) => jc(1:2)
 ic=2
 call pp_lapl(evf,curr,1,ic,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)
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
 ! jc(1:2)=S(A)=dt*[D^2_{pp}-omp^2*chi]A;  chi=<q^2*wgh*n/gam_p> >0
 !=================
 !  Computes D_{xi} centered first derivatives of S(A)
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    curr(i-1,j,k,ic)=curr(i+1,j,k,ic)
    i=n1p
    curr(i+1,j,k,ic)=curr(i-1,j,k,ic)
   end do
  end do
 end do
 !      ww0(1)= k0*S(A_I) + D_xi[A_R]= F_R
 !      ww0(2)= -k0*S(A_R) + D_xi[A_I]= F_I
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,n1p
    ii=i-2
    ww0(ii,1)=om0*curr(i,j,k,2)+dx1_inv*(&
     curr(i+1,j,k,1)-curr(i-1,j,k,1))
    ww0(ii,2)= -om0*curr(i,j,k,1)+dx1_inv*(&
     curr(i+1,j,k,2)-curr(i-1,j,k,2))
   end do
   !call trid_odd_even_inv(a,adv,c,b1,an,bn,n1,1,2)  !Inversion of [a,adv,c] M trid matrix
   do i=i1,n1p
    ii=i-2
    curr(i,j,k,1)=ww0(ii,1)/om2
    curr(i,j,k,2)=ww0(ii,2)/om2
   end do
  end do
 end do
 ! curr(1:2_= iD_{tau}=[D_t+D_xi]A
 if(ib>0)then     !fixed coordinate system
  !=======================
  !==================================
  select case(ib)
  case(1)
   adv=dt_loc*dx1_inv      !0.5*dt/dx
   b1=1.-2.*adv   !x0=2*x1-x2
   c1=2.*adv
   an=-adv
   bn=1.+adv
   !implicit centered advection term
   do ic=3,4
    do k=k1,n3p
     do j=j1,n2p
      i=i1
      evf(i-1,j,k,ic)=2.*evf(i,j,k,ic)-evf(i+1,j,k,ic)
      i=n1p
      evf(i+1,j,k,ic)=evf(i,j,k,ic)
     end do
    end do
   end do
   do k=k1,n3p
    do j=j1,n2p
     do i=i1,n1p
      ii=i-2
      ww0(ii,1)=curr(i,j,k,1)+evf(i,j,k,3)-adv*(&
       evf(i+1,j,k,3)-evf(i-1,j,k,3))
      ww0(ii,2)=curr(i,j,k,2)+evf(i,j,k,4)-adv*(&
       evf(i+1,j,k,4)-evf(i-1,j,k,4))
     end do
     call trid_der1(adv,b1,c1,an,bn,n1,1,2,0)
     do i=i1,n1p
      ii=i-2
      evf(i,j,k,3)=evf(i,j,k,1)
      evf(i,j,k,4)=evf(i,j,k,2)
      evf(i,j,k,1)=ww0(ii,1)
      evf(i,j,k,2)=ww0(ii,2)
     end do
    end do
   end do
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
      evf(i-1,j,k,ic)=2.*evf(i,j,k,ic)-evf(i+1,j,k,ic)
      ii=i-2
      ww0(ii,1)=curr(i,j,k,ic)+evf(i,j,k,ic1)-adv*(&
       evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
      i=n1p
      evf(i+1,j,k,ic)=evf(i,j,k,ic)
      do i=i1+1,n1p-1
       ii=i-2
       ww0(ii,1)=curr(i,j,k,ic)+evf(i,j,k,ic1)-an*(&
        evf(i+1,j,k,ic)-evf(i-1,j,k,ic))-bn*(&
        evf(i+2,j,k,ic)-evf(i-2,j,k,ic))
      end do
      i=n1p
      ii=i-2
      ww0(ii,1)=curr(i,j,k,ic)+evf(i,j,k,ic1)-adv*(&
       evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
      do i=i1,n1p
       ii=i-2
       evf(i,j,k,ic1)=evf(i,j,k,ic)
       evf(i,j,k,ic)=ww0(ii,1)
      end do
     end do
    end do
   enddo
  end select
 else                   !ib=0 comoving coordinate system
  do ic=1,2
   ic1=ic+1
   do k=k1,n3p
    do j=j1,n2p
     do i=i1,n1p
      curr(i,j,k,ic)=curr(i,j,k,ic)+evf(i,j,k,ic1)
      evf(i,j,k,ic1)=evf(i,j,k,ic)
      evf(i,j,k,ic)=curr(i,j,k,ic)
     end do
    end do
   end do
  end do
 endif
 !===========================
 end subroutine env_lpf_solve

 subroutine env0_rk_field(&
  curr,evf,d2_ord,ib,i1,n1p,j1,n2p,k1,n3p,om0,dhx,dhy,dhz)
 real(dp),intent(inout) :: curr(:,:,:,:), evf(:,:,:,:)
 integer,intent(in) :: d2_ord,ib,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: om0,dhx,dhy,dhz
 integer :: n1,i,j,k,ii,ic
 real(dp) :: dx1_inv,om2,aph1,aph2
 real(dp) :: alp2,b2,a,adv,c,c1,b1,an,bn
 real(dp),parameter :: alp=0.25, a1=0.75
 !==========================

 n1=n1p+1-i1
 om2=om0*om0
 dx1_inv=a1*dhx
 if(der_ord==2)dx1_inv=0.5*dhx
 !==============================================
 !Compact Fourth order derivatieve
 ! (alp,1,alp)D_x[f]=(a/dhx)[f_{i+1}-f_{i-1}]
 ! a=3/4 alp=1/4
 !=====================
 aph1=4.*dx1_inv/3.
 aph2=-dx1_inv/6.
 alp2=alp*alp
 !=======================
 if(d2_ord==2)then
  aph1=0.5*dhx
  aph2=0.0
 endif
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
 !======== in jc(1:2)= -omp2*<w*n/gam_p>*env(1:2)=> -omp2*chi*A
 ic=2
 call pp_lapl(evf,curr,1,ic,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     curr(i,j,k,ic)=-0.5*curr(i,j,k,ic)
    end do
   end do
  end do
 end do
 ! IN curr(1:2) the sotrce term S[A]=-1/2[D^2_{perp}-omp2*chi]A
 !=====================
 !=================
 do ic=1,2
  do k=k1,n3p
   do j=j1,n2p
    i=i1
    curr(i-1,j,k,ic)=curr(i+1,j,k,ic)
    curr(i-2,j,k,ic)=curr(i+2,j,k,ic)
    i=n1p
    curr(i+1,j,k,ic)=curr(i-1,j,k,ic)
    curr(i+2,j,k,ic)=curr(i-2,j,k,ic)
   end do
  end do
 end do
 do k=k1,n3p
  do j=j1,n2p
   do i=i1,n1p
    ii=i-2
    ww0(ii,1)=-om0*(alp2*(curr(i+2,j,k,2)+curr(i-2,j,k,2))+2.*alp*(&
     curr(i+1,j,k,2)+curr(i-1,j,k,2))+2.*alp2*curr(i,j,k,2))
    ww0(ii,1)=ww0(ii,1)+a1*dhx*(alp*(curr(i+2,j,k,1)-curr(i-2,j,k,1))+&
     curr(i+1,j,k,1)-curr(i-1,j,k,1))
    ww0(ii,2)=om0*(alp2*(curr(i+2,j,k,1)+curr(i-2,j,k,1))+2.*alp*(&
     curr(i+1,j,k,1)+curr(i-1,j,k,1))+2.*alp2*curr(i,j,k,2))
    ww0(ii,2)=ww0(ii,2)+a1*dhx*(alp*(curr(i+2,j,k,2)-curr(i-2,j,k,2))+&
     curr(i+1,j,k,2)-curr(i-1,j,k,2))
   end do
   !============= in ww0(1:2)
   !==== F_R=D_{xi}S(A_R)+k0*S(A_I)
   !==== F_I=D_{xi}S(A_I)-k0*S(A_R)
   !=============================
   call matenv_inv(n1,2)
   do i=i1,n1p
    ii=i-2
    curr(i,j,k,1)=ww0(ii,1)/om2
    curr(i,j,k,2)=ww0(ii,2)/om2
   end do
  end do
 end do
 !=======================
 if(ib==0)return
 !=======================
 !==================================
 !RK4 only adds explicit advection term
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
    do i=i1+2,n1p-1
     curr(i,j,k,ic)=curr(i,j,k,ic)-&
      aph1*(evf(i+1,j,k,ic)-evf(i-1,j,k,ic))-&
      aph2*(evf(i+2,j,k,ic)-evf(i-2,j,k,ic))
    end do
    i=n1p
    evf(i+1,j,k,ic)=evf(i,j,k,ic)
    curr(i,j,k,ic)=curr(i,j,k,ic)-&
     dx1_inv*(evf(i+1,j,k,ic)-evf(i-1,j,k,ic))
   end do
  end do
 end do
 !===========================
 end subroutine env0_rk_field
 !==========================================
 ! END ENV SECTION
 !==================================
 !========== LASER FIELDS SECTION
 !=================================
 ! INITIAL FIELDS
 !==============================
 subroutine get_laser_fields_lp(coords,par_lp,fields)
 real(dp),intent(in) :: coords(4),par_lp(7)
 real(dp),intent(out) :: fields(6)
 real(dp) :: phi0, phi1, phig00, phig10, csphig01
 real(dp) :: x1, y1, z1, t1,pih
 real(dp) :: w2,ar,rho, ss0 ,cs0
 real(dp) :: A0, A1
 real(dp) :: ev0, ev1,phx, psi, r2, wshape
 !========== enter
 !par_lp(1)=oml
 !par_lp(2)=pih
 !par_lp(3)=tau
 !par_lp(4)=wy
 !par_lp(5)=zra
 !par_lp(6)=eps
 !par_lp(7)=sigma
 !===============================
 x1=coords(1)
 y1=coords(2)/par_lp(4)
 z1=coords(3)/par_lp(4)
 t1=coords(4)
 pih=par_lp(2)
 !====================
 r2=y1*y1+z1*z1
 phi0=par_lp(1)*(t1-x1)
 phi1=pih*(t1-x1)/par_lp(3)
 if(abs(phi1)>pih)phi1=pih
 x1=x1/par_lp(5)
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
 ev0=cos(phi1)*cos(phi1)
 ev1=cos(phi1)*sin(phi1)
 wshape=sqrt(w2)*exp(-w2*r2)
 A0=ev0*sin(phig00)
 A1=ev1*par_lp(7)*x1*w2*rho
 A1=A1*csphig01
 fields(2)=wshape*(A0+A1)  !Ey
 A1=ev0*2.*par_lp(6)*w2*exp(-w2*r2)
 fields(1)=y1*A1*cos(phig10)  !Ex
 fields(4)=z1*A1*cos(phig10)  !Bx
 fields(6)=fields(2)          !Bz
 fields(3)=0.0
 fields(5)=0.0
 end subroutine get_laser_fields_lp
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
 pih=par_pp(2)
 !====================
 !oml=par_pp(1)  pih=par_pp(2)  tau=par_pp(3)
 phi0=par_pp(1)*(t1-x1)
 phi1=par_pp(2)*(t1-x1)/par_pp(3)
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
 pih=par_pp(2)
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
 !par_cp(1)=oml
 !par_cp(2)=pih
 !par_cp(3)=tau
 !par_cp(4)=wy
 !par_cp(5)=zra
 !par_cp(6)=eps
 !par_cp(7)=sigma
 pih=par_cp(2)
 x1=coords(1)
 y1=coords(2)
 z1=coords(3)
 t1=coords(4)
 y1=y1/par_cp(4)
 z1=z1/par_cp(4)
 r2=y1*y1+z1*z1
 phi0=par_cp(1)*(t1-x1)
 phi1=par_cp(2)*(t1-x1)/par_cp(3)
 if(abs(phi1)>par_cp(2))phi1=par_cp(2)
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
 subroutine inflow_lp_fields(ef,e0,t_loc,tf,tau,wy,xf0,&
  lp,i,j1,j2,k1,k2)
 !==========================
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,t_loc,tf,tau,wy,xf0
 integer,intent(in) :: lp,i,j1,j2,k1,k2
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,sigma,eps
 real(dp) :: xp,yp
 real(dp) :: pih,zra
 real(dp) :: Ex,Ey,Ez,Bx,By,Bz
 integer :: j,k,jj,kk
 real(dp) :: coords(4), fields(6),par_lp(7)
 !enter sigma=0.5*lam0/tau:
 ! inviluppo temporale= cos^2(pi*(t-x))/2tau)
 ! eps=1./k0*wy k0=omega_0=omgl
 pih=0.5*acos(-1.0)
 sigma=pi/(oml*tau) !sigma=0.5*lambda/tau
 eps=1./(oml*wy)
 zra=0.5*oml*wy*wy
 xx=loc_xg(1,1,0)
 xxh=loc_xg(1,2,0)
 coords(4)=t_loc-tf
 par_lp(1)=oml
 par_lp(2)=pih
 par_lp(3)=tau
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
   call get_laser_fields_lp(coords,par_lp,fields)
   Ex=e0*fields(1)
   ef(i,j,k,1)=Ex
   Bz=e0*fields(6)
   ef(i,j,k,3)=Bz
   !==== Ey(xx,yyh)!
   xp=xx
   coords(1)=xp-xf0
   call get_laser_fields_lp(coords,par_lp,fields)
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
    call get_laser_fields_lp(coords,par_lp,fields)
    Ex=e0*fields(1)
    ef(i,j,k,1)=Ex
    !==== Ey(xx,yyh)!
    xp=xx
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    call get_laser_fields_lp(coords,par_lp,fields)
    Ey=e0*fields(2)
    ef(i,j,k,2)=Ey
    !===ora Bz(xxh,yyh)=========
    xp=xxh
    yp=yyh
    coords(1)=xp-xf0
    coords(2)=yp
    call get_laser_fields_lp(coords,par_lp,fields)
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
 subroutine init_lp_fields(ef,e0,t_loc,tf,tau,wy,xf0,&
  angle,lp_shx,lp,i1,i2)
 !==========================
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: e0,t_loc,tf,tau,wy,xf0,angle,lp_shx
 integer,intent(in) :: lp,i1,i2
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,sigma,eps
 real(dp) :: xp,xc,yp,yc
 real(dp) :: pih,zra,sf,cf
 real(dp) :: Ex,Ey,Ez,Bx,By,Bz
 integer :: i,j,k,ii,jj,kk
 integer :: j1,j2,k1,k2
 real(dp) :: coords(4), fields(6),par_lp(7)

 !enter sigma=0.5*lam0/tau:
 ! inviluppo temporale= cos^2(pi*(t-x))/2tau)
 ! eps=1./k0*wy k0=omega_0=omgl
 pih=0.5*acos(-1.0)
 sf=sin(pih*angle/90.)
 cf=cos(pih*angle/90.)
 sigma=pi/(oml*tau)     !sigma=0.5*lambda/tau
 eps=1./(oml*wy)
 zra=0.5*oml*wy*wy
 par_lp(1)=oml
 par_lp(2)=pih
 par_lp(3)=tau
 par_lp(4)=wy
 par_lp(5)=zra
 par_lp(6)=eps
 par_lp(7)=sigma

 coords(4)=t_loc-tf
 ! for normal incidence
 xc=xf0-tf+tau+lp_shx
 !LP centroid + tau= end of envelope
 ! xc= end of envelope x-coordinate +offset
 yc=0.0 ! yc centroid y coordinate
 ! for angle non eq 0
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
     ef(i,j,k,2)=Ey
     !===ora Bz(xxh,yyh)=========
     xp=xxh
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,3)=Bz
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
     ef(i,j,k,2)=Ey*cf
     ef(i,j,k,3)=Ey*sf
     ef(i,j,k,4)=0.0
     ef(i,j,k,5)=0.0
     !==== Bz(xxh,yyh,zz)=========
     xp=xxh
     coords(1)=xp-xf0
     call get_plane_wave_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,5)=-Bz*sf
     ef(i,j,k,6)=Bz*cf
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
    call get_laser_fields_lp(coords,par_lp,fields)
    Ex=e0*fields(1)
    ef(i,j,k,1)=Ex
    !==== Ey(xx,yyh)!
    xp=xx
    coords(1)=xp-xf0
    call get_laser_fields_lp(coords,par_lp,fields)
    Ey=e0*fields(2)
    ef(i,j,k,2)=Ey
    !===ora Bz(xxh,yyh)=========
    xp=xc+(xxh-xc)
    coords(1)=xp-xf0
    call get_laser_fields_lp(coords,par_lp,fields)
    Bz=e0*fields(6)
    ef(i,j,k,3)=Bz
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
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     !==== Ex(xxh,yy)!
     call get_laser_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,1)=Ex*cf-Ey*sf
     !==== Ey(xx,yyh)!
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_laser_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,2)=Ey*cf+Ex*sf
     !===ora Bz(xxh,yyh)=========
     xp=xc+(xxh-xc)*cf+yyh*sf
     yp=yyh*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_laser_fields_lp(coords,par_lp,fields)
     Bz=e0*fields(6)
     ef(i,j,k,3)=Bz
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
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zz
     call get_laser_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,1)=Ex*cf-Ey*sf
     !==== Ey(xx,yyh,zz) =========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_laser_fields_lp(coords,par_lp,fields)
     Ex=e0*fields(1)
     Ey=e0*fields(2)
     ef(i,j,k,2)=Ey*cf+Ex*sf
     !==== Ez(xx,yy,zzh) =========
     xp=xc+(xx-xc)*cf+yy*sf
     yp=yy*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     call get_laser_fields_lp(coords,par_lp,fields)
     Ez=e0*fields(3)
     ef(i,j,k,3)=Ez
     !==== Bx(xx,yyh,zzh)=========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     call get_laser_fields_lp(coords,par_lp,fields)
     Bx=e0*fields(4)
     By=e0*fields(5)
     ef(i,j,k,4)=Bx*cf-By*sf
     !==== By(xxh,yy,zzh) =========
     xp=xc+(xxh-xc)*cf+yy*sf
     yp=yy*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zzh
     call get_laser_fields_lp(coords,par_lp,fields)
     Bx=e0*fields(4)
     By=e0*fields(5)
     ef(i,j,k,5)=By*cf+Bx*sf
     !==== Bz(xxh,yyh,zz)=========
     xp=xc+(xxh-xc)*cf+yyh*sf
     yp=yyh*cf-(xxh-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     coords(3)=zz
     call get_laser_fields_lp(coords,par_lp,fields)
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
    do i=i1,i2          !xp=x*cos+y*sin  yp=y*cos-x*sin
     ii=i-2
     xx=loc_xg(ii,1,imodx)
     xxh=loc_xg(ii,2,imodx)
     !===ora Ez(xx,yy)=========
     xp=xc+(xx-xc)*cf+yy*sf
     yp=yy*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_laser_fields_lp(coords,par_lp,fields)
     Ez=e0*fields(2)    !  Ez(s-pol)=Ey(p-pol)
     ef(i,j,k,3)=Ez
     !==== Bx(xx,yyh)=========
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_laser_fields_lp(coords,par_lp,fields)
     Bx=e0*fields(1)     !  Bx(s-pol)= Ex(p-pol)
     By=-e0*fields(6)    !  By(s-pol)=-Bz(p-pol)
     ef(i,j,k,4)=Bx*cf-By*sf
     !==== By(xx,yyh) =======
     xp=xc+(xx-xc)*cf+yyh*sf
     yp=yyh*cf-(xx-xc)*sf
     coords(1)=xp-xf0
     coords(2)=yp
     call get_laser_fields_lp(coords,par_lp,fields)
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
   do j=j1,j2
    jj=j-2
    yy=loc_yg(jj,1,imody)
    yyh=loc_yg(jj,2,imody)
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
 subroutine inflow_cp_fields(ef,e0,t_loc,tf,tau,wy,xf0,&
  cp,i,j1,j2,k1,k2)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer, intent(in) :: cp,i,j1,j2,k1,k2
 real(dp),intent(in) :: e0,t_loc,tf,tau,wy,xf0
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,xp,yp
 real(dp) :: pih,eps,sigma,zra
 real(dp) :: Ey,Bz,Ex,Bx,Ez,By
 real(dp) :: coords(4),fields(6),par_lp(7)
 integer :: j,k,jj,kk
 !============
 !enter sigma=0.5*lam0/tau:
 ! inviluppo temporale= cos^2(pi*(t-x))/2tau)
 ! eps=1./k0*wy k0=omega_0=omgl
 pih=0.5*acos(-1.0)
 sigma=pi/(oml*tau)     !sigma=0.5*lambda/tau
 eps=1./(oml*wy)
 zra=0.5*oml*wy*wy
 coords(4)=t_loc-tf
 par_lp(1)=oml
 par_lp(2)=pih
 par_lp(3)=tau
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
 subroutine init_cp_fields(ef,e0,t_loc,tf,tau,wy,xf0,&
  angle,lp_shx,cp,i1,i2)

 real(dp),intent(inout) :: ef(:,:,:,:)
 integer, intent(in) :: cp,i1,i2
 real(dp),intent(in) :: e0,t_loc,tf,tau,wy,xf0,angle,lp_shx
 real(dp) :: xxh,xx,yy,yyh,zz,zzh,xp,yp
 real(dp) :: pih,eps,sigma,sf,cf,zra,xc
 real(dp) :: Ey,Bz,Ex,Bx,Ez,By
 real(dp) :: coords(4),fields(6),par_lp(7)
 integer :: j1,j2,k1,k2,i,j,k,ii,jj,kk
 !============
 !enter sigma=0.5*lam0/tau:
 ! inviluppo temporale= cos^2(pi*(t-x))/2tau)
 ! eps=1./k0*wy k0=omega_0=omgl
 pih=0.5*acos(-1.0)
 sf=sin(pih*angle/90.)
 cf=cos(pih*angle/90.)
 sigma=pi/(oml*tau) !sigma=0.5*lambda/tau
 eps=1./(oml*wy)
 zra=0.5*oml*wy*wy
 coords(4)=t_loc-tf
 par_lp(1)=oml
 par_lp(2)=pih
 par_lp(3)=tau
 par_lp(4)=wy
 par_lp(5)=zra
 par_lp(6)=eps
 par_lp(7)=sigma
 xc=xf0-tf+tau+lp_shx
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
 !=========================
 subroutine divE(ef,dv,ix1,n1p,iy1,n2p,iz1,n3p,ndm,ic)
 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(out) :: dv(:,:,:,:)
 integer,intent(in) :: ix1,n1p,iy1,n2p,iz1,n3p,ndm,ic
 integer :: i,j,k
 !===================
 ! needs points from the left

 do k=iz1,n3p
  do j=iy1,n2p
   ef(ix1-1,j,k,1)=3.*(ef(ix1,j,k,1)-ef(ix1+1,j,k,1))+&
    ef(ix1+2,j,k,1)
   do i=ix1,n1p
    dv(i,j,k,ic)=dx_inv*(&
     se_coeff(1)*(ef(i,j,k,1)-ef(i-1,j,k,1)))
   end do
  end do
 end do
 if(der_ord >2)then
  do k=iz1,n3p
   do j=iy1,n2p
    do i=ix1+1,n1p-1
     dv(i,j,k,ic)=dv(i,j,k,ic)+dx_inv*(&
      se_coeff(2)*(ef(i+1,j,k,1)-ef(i-2,j,k,1)))
    end do
   end do
  end do
 end if
 if(ndm==1)return
 if(pe0y)then
  do k=iz1,n3p
   do i=ix1,n1p
    ef(i,iy1-1,k,2)=3.*(ef(i,iy1,k,2)-ef(i,iy1+1,k,2))+&
     ef(i,iy1+2,k,2)
   end do
  end do
 endif
 do k=iz1,n3p
  do j=iy1,n2p
   do i=ix1,n1p
    dv(i,j,k,ic)=dv(i,j,k,ic)+&
     dy_inv*(ef(i,j,k,2)-ef(i,j-1,k,2))
   end do
  end do
 end do
 if(ndm==2)return
 if(pe0z)then
  do j=iy1,n2p
   do i=ix1,n1p
    ef(i,j,iz1-1,3)=3.*(ef(i,j,iz1,3)-ef(i,j,iz1+1,3))+&
     ef(i,j,iz1+2,3)
   end do
  end do
 endif
 do k=iz1,n3p
  do j=iy1,n2p
   do i=ix1,n1p
    dv(i,j,k,ic)=dv(i,j,k,ic)+&
     dz_inv*(ef(i,j,k,3)-ef(i,j,k-1,3))
   end do
  end do
 end do
 end subroutine divE
 !==========================
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
   case(1)
    do k=k1,n3p
     do i=i1-1,n1p
      ef(i,j1-1,k,nfield)=ef(i,j1,k,nfield)
     end do
    end do
    if(nfield>3)then
     do k=k1,n3p
      do i=i1,n1p
       ef(i,j1-1,k,4)=ef(i,j1,k,4)
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
   case(1)
    do j=j1,n2p
     do i=i1,n1p
      ef(i,j,k1-1,4)=ef(i,j,k1,4)
      ef(i,j,k1-1,5)=ef(i,j,k1,5)
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
   case(1)
    do k=k1,n3p
     do i=i1,n1p
      ef(i,n2p+1,k,1)=ef(i,n2p,k,1)
     end do
    end do
    if(nfield>3)then
     do k=k1,n3p
      do i=i1,n1p
       ef(i,n2p+1,k,3)=ef(i,n2p,k,3)
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
   case(1)
    do j=j1,n2p
     do i=i1,n1p
      ef(i,j,n3p+1,1)=ef(i,j,n3p,1)
      ef(i,j,n3p+1,2)=ef(i,j,n3p,2)
     end do
    end do
   end select
  endif
 endif
 end subroutine ef_bds
 !=========================================
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
 subroutine potential_lapl(apf,curr,ic1,ic2,i1,n1p,j1,n2p,k1,n3p,&
  dhx,dhy,dhz)
 real(dp),intent(inout)  :: apf(:,:,:,:),curr(:,:,:,:)

 integer,intent(in):: ic1,ic2,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: dhx,dhy,dhz
 integer :: i,j,k,ic,d2_ord,j01,j02,k01,k02
 real(dp) :: dx2,dy2,dz2
 !Computes the Laplacian(apf) and accumulates on the source array curr
 !                 curr=(Laplacian(apf)+curr)
 !========================================


 d2_ord=2
 dx2=dhx*dhx
 dy2=dhy*dhy
 dz2=dhz*dhz
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 d2_ord=2
 !============= ALL FIELDS ic=ic1,ic2
 if(pex0)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     apf(i1-1,j,k,ic)=2.*apf(i1,j,k,ic)-apf(i1+1,j,k,ic)
    enddo
   end do
  end do
 endif
 if(pex1)then
  do ic=ic1,ic2
   do k=k1,n3p
    do j=j1,n2p
     apf(n1p+1,j,k,ic)=apf(n1p-1,j,k,ic)
    enddo
   end do
  end do
 endif
 do ic=ic1,ic2
  do k=k1,n3p
   do j=j1,n2p
    do i=i1,n1p
     curr(i,j,k,ic)=curr(i,j,k,ic)+dx2*(apf(i+1,j,k,ic)-&
      2.*apf(i,j,k,ic)+apf(i-1,j,k,ic))
    end do
   end do
  end do
 end do
 if(ndim >1)call pp_lapl(&
  apf,curr,ic1,ic2,i1,n1p,j1,n2p,k1,n3p,dhy,dhz)
 end subroutine potential_lapl
 !===========================
 subroutine rotE(ef,i1,n1p,j1,j2,k1,k2,aphx,aphy,aphz)

 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,j2,k1,k2
 real(dp),intent(in) :: aphx,aphy,aphz
 real(dp) :: sdhx,sdhy,sdhz
 integer :: i,j,k,ii,jj,kk
 real(dp) :: aph1,aph2

 ! Enter ef(1:3)=[Ex,Ey,Ez], ef(4:6)=[Bx,By,Bz]
 ! B=B-DT*rot[E]
 ! enter boundary fields
 !==================== B=B-dt*rot(E) interior domain==========
 aph1=aphx*se_coeff(1)
 aph2=aphx*se_coeff(2)
 !============================
 if(ndim==1)then
  k=1;j=1
  do i=i1,n1p
   ii=i-2
   sdhx=aph1*loc_xg(ii,4,imodx)
   ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
    sdhx*(ef(i+1,j,k,2)-ef(i,j,k,2))
  end do
  if(der_ord >2)then
   do i=i1+1,n1p-1
    ii=i-2
    sdhx=aph2*loc_xg(ii,4,imodx)
    ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
     sdhx*(ef(i+2,j,k,2)-ef(i-1,j,k,2))
   end do
  endif
  return
 endif
 !=================================
 do k=k1,k2
  do j=j1,j2
   jj=j-2
   sdhy=loc_yg(jj,4,imody)*aphy
   do i=i1,n1p
    ii=i-2
    sdhx=aph1*loc_xg(ii,4,imodx)
    ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
     sdhx*(ef(i+1,j,k,2)-ef(i,j,k,2))+&
     sdhy*(ef(i,j+1,k,1)-ef(i,j,k,1))
   end do
  end do
 end do
 if(der_ord >2)then
  do k=k1,k2
   do j=j1,j2
    do i=i1+1,n1p-1
     ii=i-2
     sdhx=aph2*loc_xg(ii,4,imodx)
     ef(i,j,k,nfield)=ef(i,j,k,nfield)-&
      sdhx*(ef(i+2,j,k,2)-ef(i-1,j,k,2))
    end do
   end do
  end do
 endif
 if(nfield <6)return
 if(ndim==3)then
  do k=k1,k2
   kk=k-2
   sdhz=loc_zg(kk,4,imodz)*aphz
   do j=j1,j2
    jj=j-2
    sdhy=loc_yg(jj,4,imody)*aphy
    do i=i1,n1p
     ii=i-2
     sdhx=aph1*loc_xg(ii,4,imodx)
     ef(i,j,k,4)=ef(i,j,k,4)-sdhy*(ef(i,j+1,k,3)-ef(i,j,k,3))+&
      sdhz*(ef(i,j,k+1,2)-ef(i,j,k,2))
     ef(i,j,k,5)=ef(i,j,k,5)+&
      sdhx*(ef(i+1,j,k,3)-ef(i,j,k,3))-&
      sdhz*(ef(i,j,k+1,1)-ef(i,j,k,1))
    end do
   end do
  end do
 else
  k=1
  do j=j1,j2
   jj=j-2
   sdhy=loc_yg(jj,4,imody)*aphy
   do i=i1,n1p
    ii=i-2
    sdhx=aph1*loc_xg(ii,4,imodx)
    ef(i,j,k,4)=ef(i,j,k,4)-sdhy*(ef(i,j+1,k,3)-ef(i,j,k,3))
    ef(i,j,k,5)=ef(i,j,k,5)+sdhx*(ef(i+1,j,k,3)-ef(i,j,k,3))
   end do
  end do
 endif
 if(der_ord >2)then
  do k=k1,k2
   do j=j1,j2
    do i=i1+1,n1p-1
     ii=i-2
     sdhx=aph2*loc_xg(ii,4,imodx)
     ef(i,j,k,5)=ef(i,j,k,5)+&
      sdhx*(ef(i+2,j,k,3)-ef(i-1,j,k,3))
    end do
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
 real(dp) :: sdx,sdy,sdz
 integer :: i,j,k,ii,jj,kk
 real(dp) :: aph1,aph2
 ! E=E+DT*rot[B]
 !==================== B=B-dt*rot(E) interior domain==========
 aph1=aphx*se_coeff(1)
 aph2=aphx*se_coeff(2)
 ! enter boundary fields
 !=================== interior domains
 if(ndim==1)then
  k=1;j=1
  do i=i1,n1p
   ii=i-2
   sdx=loc_xg(ii,3,imodx)*aph1
   ef(i,j,k,2)=ef(i,j,k,2)-sdx*(ef(i,j,k,nfield)-ef(i-1,j,k,nfield))
  end do
  if(der_ord >2)then
   do i=i1+1,n1p-1
    ii=i-2
    sdx=loc_xg(ii,3,imodx)*aph2
    ef(i,j,k,2)=ef(i,j,k,2)-&
     sdx*(ef(i+1,j,k,nfield)-ef(i-2,j,k,nfield))
   end do
  endif
  return
 endif
 !=========================== NDIM > 1
 do k=k1,k2
  do j=j1,j2
   jj=j-2
   sdy=loc_yg(jj,3,imody)*aphy
   do i=i1,n1p
    ii=i-2
    sdx=loc_xg(ii,3,imodx)*aph1
    ef(i,j,k,1)=ef(i,j,k,1)+sdy*(ef(i,j,k,nfield)-ef(i,j-1,k,nfield))
    ef(i,j,k,2)=ef(i,j,k,2)-sdx*(ef(i,j,k,nfield)-ef(i-1,j,k,nfield))
   end do
  end do
 end do
 if(der_ord >2)then
  do k=k1,k2
   do j=j1,j2
    do i=i1+1,n1p-1
     ii=i-2
     sdx=loc_xg(ii,3,imodx)*aph2
     ef(i,j,k,2)=ef(i,j,k,2)-&
      sdx*(ef(i+1,j,k,nfield)-ef(i-2,j,k,nfield))

    end do
   end do
  end do
 endif
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
     sdx=loc_xg(ii,3,imodx)*aph1
     ef(i,j,k,1)=ef(i,j,k,1)-&
      sdz*(ef(i,j,k,5)-ef(i,j,k-1,5))
     ef(i,j,k,2)=ef(i,j,k,2)+sdz*(ef(i,j,k,4)-ef(i,j,k-1,4))
     ef(i,j,k,3)=ef(i,j,k,3)+&
      sdx*(ef(i,j,k,5)-ef(i-1,j,k,5))-&
      sdy*(ef(i,j,k,4)-ef(i,j-1,k,4))
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
    sdx=loc_xg(ii,3,imodx)*aph1
    ef(i,j,k,3)=ef(i,j,k,3)+&
     sdx*(ef(i,j,k,5)-ef(i-1,j,k,5))-&
     sdy*(ef(i,j,k,4)-ef(i,j-1,k,4))
   end do
  end do
 endif
 if(der_ord >2)then
  do k=k1,k2
   do j=j1,j2
    do i=i1+1,n1p-1
     ii=i-2
     sdx=loc_xg(ii,3,imodx)*aph2
     ef(i,j,k,3)=ef(i,j,k,3)+&
      sdx*(ef(i+1,j,k,5)-ef(i-2,j,k,5))
    end do
   end do
  end do
 endif
 end subroutine rotB
 !=====================================
 subroutine rotEB_rk4(ef,ef1,ef0,nef,i1,n1p,j1,n2p,k1,n3p,vb,aphx,aphy,aphz)
 real(dp),intent(inout) :: ef(:,:,:,:),ef1(:,:,:,:)
 real(dp),intent(in) :: ef0(:,:,:,:)

 integer,intent(in) :: nef,i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: vb,aphx,aphy,aphz
 integer :: i,j,k,ic,ic1,ii,jj,j01,j02,kk,k01,k02
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
 ! Fourth-order data [i1-1:n+2]
 !================
 ii=1
 !=========== fourth order
 ! advances DE/DT=rot(B)-omp*J
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
 ! rot(E)=> B
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
     ef(i,j,k,4)=ef0(i,j,k,4)-sdy*(ef(i,j+1,k,3)-ef(i,j,k,3))
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
     ef(i,j,k,4)=ef0(i,j,k,4)-sdy*(&
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
 end subroutine rotEB_rk4
 !========= FLUID SECTION
 !! SECTION FOR FLUID MODEL
 !================================
 subroutine density_momenta(ef,ef1,i1,n1p,j1,n2p,k1,n3p,aphx,aphy,aphz)
 real(dp),intent(inout) :: ef1(:,:,:,:)
 real(dp),intent(inout) :: ef(:,:,:,:)

 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: aphx,aphy,aphz
 integer :: i,j,k,ic,j01,j02,k01,k02
 real(dp) :: f0,f2,f1,mom(4),gam2,aphx1,aphy1,aphz1


 aphx1=0.5*aphx
 aphy1=0.5*aphy
 aphz1=0.5*aphz
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 ! ordering ef[ux,uy,uz,rho,gam_inv] five components
 !================
 ! defines gam_inv
 do k=k01,k02
  do j=j01,j02
   do i=i1,n1p
    mom(1:4)=ef(i,j,k,1:4)
    gam2=dot_product(mom(1:4),mom(1:4))     !rho*rho+u_i*u_i
    ef(i,j,k,5)=1./sqrt(gam2)
   end do
  end do
 end do
 !========= x-bds
 do ic=1,5
  do k=k01,k02
   do j=j01,j02
    i=i1
    ef(i-1,j,k,ic)=2.*ef(i,j,k,ic)-ef(i+1,j,k,ic)
    i=n1p
    ef(i+1,j,k,ic)=2.*ef(i,j,k,ic)-ef(i-1,j,k,ic)
   end do
  end do
 end do
 !===================
 !=============== left upwind for positive v_x advection
 do ic=1,4
  do k=k01,k02
   do j=j01,j02
    do i=i1+1,n1p
     f2=ef(i,j,k,ic)*ef(i,j,k,1)*ef(i,j,k,5)
     f1=ef(i-1,j,k,ic)*ef(i-1,j,k,1)*ef(i-1,j,k,5)
     f0=ef(i-2,j,k,ic)*ef(i-2,j,k,1)*ef(i-2,j,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-aphx1*(3.*f2-4.*f1+f0)
    end do
   end do
  end do
 end do
 !=========== y derivatives
 if(pe0y)then
  do ic=1,4
   do k=k01,k02
    j=j1
    do i=i1,n1p
     f2=ef(i,j+1,k,ic)*ef(i,j+1,k,2)*ef(i,j+1,k,5)
     f1=ef(i,j,k,ic)*ef(i,j,k,2)*ef(i,j,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*aphy1*(f2-f1)
    end do
   end do
  end do
  j01=j1+1
 endif
 if(pe1y)then
  do ic=1,4
   do k=k01,k02
    j=n2p
    do i=i1,n1p
     f2=ef(i,j,k,ic)*ef(i,j,k,2)*ef(i,j,k,5)
     f1=ef(i,j-1,k,ic)*ef(i,j-1,k,2)*ef(i,j-1,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*aphy1*(f2-f1)
    end do
   end do
  end do
  j02=n2p-1
 endif
 do ic=1,4
  do k=k01,k02
   do j=j01,j02
    do i=i1,n1p
     f2=ef(i,j+1,k,ic)*ef(i,j+1,k,2)*ef(i,j+1,k,5)
     f1=ef(i,j-1,k,ic)*ef(i,j-1,k,2)*ef(i,j-1,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-aphy1*(f2-f1)
    end do
   end do
  end do
 end do
 !=========== z derivatives
 if(pe0z)then
  do ic=1,4
   k=k1
   do j=j01,j02
    do i=i1,n1p
     f2=ef(i,j,k+1,ic)*ef(i,j,k+1,3)*ef(i,j,k+1,5)
     f1=ef(i,j,k,ic)*ef(i,j,k,3)*ef(i,j,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*aphz1*(f2-f1)
    end do
   end do
  end do
  k01=k1+1
 endif
 if(pe1z)then
  do ic=1,4
   k=n3p
   do j=j01,j02
    do i=i1,n1p
     f2=ef(i,j,k,ic)*ef(i,j,k,3)*ef(i,j,k,5)
     f1=ef(i,j,k-1,ic)*ef(i,j,k-1,3)*ef(i,j,k-1,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*aphz1*(f2-f1)
    end do
   end do
  end do
  k02=n3p-1
 endif
 do ic=1,4
  do k=k01,k02
   do j=j01,j02
    do i=i1,n1p
     f2=ef(i,j,k+1,ic)*ef(i,j,k+1,3)*ef(i,j,k+1,5)
     f1=ef(i,j,k-1,ic)*ef(i,j,k-1,3)*ef(i,j,k-1,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-aphz1*(f2-f1)
    end do
   end do
  end do
 end do
 end subroutine density_momenta
 !===================================
 subroutine rk_density_momenta(ef,ef1,i1,n1p,j1,n2p,k1,n3p,aphx,aphy,aphz)
 real(dp),intent(inout) :: ef(:,:,:,:),ef1(:,:,:,:)

 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 real(dp),intent(in) :: aphx,aphy,aphz
 integer :: i,j,k,ic,j01,j02,k01,k02
 real(dp) :: aphx2,aphy2,aphz2
 real(dp) :: f0,f2,f1,mom(4),gam2,aphx1,aphy1,aphz1
 real(dp),parameter :: a_cd=2./3., b_cd=-1./12.
 !=========== fourth order
 ! advances RK4 density momenta

 aphx1=aphx*a_cd
 aphx2=aphx*b_cd
 aphy1=aphy*a_cd
 aphy2=aphy*b_cd
 aphz1=aphz*a_cd
 aphz2=aphz*b_cd
 !================
 ! Fourth-order data [i1-1:n+2]
 !================
 j01=j1
 j02=n2p
 k01=k1
 k02=n3p
 ! ordering ef[ux,uy,uz,rho,gam_inv] five components
 !================
 ! defines gam_inv
 do k=k01,k02
  do j=j01,j02
   do i=i1,n1p
    mom(1:4)=ef(i,j,k,1:4)
    gam2=dot_product(mom(1:4),mom(1:4))     !rho*rho+u_i*u_i
    ef(i,j,k,5)=1./sqrt(gam2)
   end do
  end do
 end do
 !========= x-bds
 do ic=1,5
  do k=k01,k02
   do j=j01,j02
    i=i1
    ef(i-1,j,k,ic)=2.*ef(i,j,k,ic)-ef(i+1,j,k,ic)
    i=n1p
    ef(i+1,j,k,ic)=2.*ef(i,j,k,ic)-ef(i-1,j,k,ic)
   end do
  end do
 end do
 !===================
 !=============== left upwind for positive v_x advection
 do ic=1,4
  do k=k01,k02
   do j=j01,j02
    do i=i1+1,n1p
     f2=ef(i,j,k,ic)*ef(i,j,k,1)*ef(i,j,k,5)
     f1=ef(i-1,j,k,ic)*ef(i-1,j,k,1)*ef(i-1,j,k,5)
     f0=ef(i-2,j,k,ic)*ef(i-2,j,k,1)*ef(i-2,j,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-aphx1*(3.*f2-4.*f1+f0)
    end do
   end do
  end do
 end do
 !=========== y derivatives
 if(pe0y)then
  do ic=1,4
   do k=k01,k02
    j=j1
    do i=i1,n1p
     f2=ef(i,j+1,k,ic)*ef(i,j+1,k,2)*ef(i,j+1,k,5)
     f1=ef(i,j,k,ic)*ef(i,j,k,2)*ef(i,j,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*aphy1*(f2-f1)
    end do
   end do
  end do
  j01=j1+1
 endif
 if(pe1y)then
  do ic=1,4
   do k=k01,k02
    j=n2p
    do i=i1,n1p
     f2=ef(i,j,k,ic)*ef(i,j,k,2)*ef(i,j,k,5)
     f1=ef(i,j-1,k,ic)*ef(i,j-1,k,2)*ef(i,j-1,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-2.*aphy1*(f2-f1)
    end do
   end do
  end do
  j02=n2p-1
 endif
 do ic=1,4
  do k=k01,k02
   do j=j01,j02
    do i=i1,n1p
     f2=ef(i,j+1,k,ic)*ef(i,j+1,k,2)*ef(i,j+1,k,5)
     f1=ef(i,j-1,k,ic)*ef(i,j-1,k,2)*ef(i,j-1,k,5)
     ef1(i,j,k,ic)=ef1(i,j,k,ic)-aphy1*(f2-f1)
    end do
   end do
  end do
 end do
 !=========== z derivatives
 !=========== fourth order
 ! advances RK4 density momenta
 !===================================
 end subroutine rk_density_momenta




 end module grid_fields
