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

 module fft_lib

 use precision_def

#ifdef USE_OLD_FFTW_INTERFACE
 !---------------------------------------
 use, intrinsic :: iso_c_binding
 implicit none
 include 'fftw3.f03'

 integer(dp) :: plan1,iplan1
 integer(dp) :: plan2,iplan2
 integer(dp) :: plan3,iplan3

 real(dp),allocatable :: cw(:,:),w1(:),w1_st(:),w2(:),w3(:)
 real(dp),allocatable :: cfhx(:),sfhx(:)
 real(dp),allocatable :: cfhy(:),sfhy(:)
 real(dp),allocatable :: cfhz(:),sfhz(:)

 contains

 subroutine ftw_init(n1,n2,n3,ind_ft)
 integer,intent(in) :: n1,n2,n3,ind_ft
 integer :: i,nm
 real(dp) :: wk

 select case(ind_ft)
 case(0)
  nm=max(n1,n2)
  allocate(w1(n1+2),w1_st(nm+1),cfhx(n1),sfhx(n1))
  wk=acos(-1.0)/real(n1,dp)
  do i=1,n1
   cfhx(i)=cos(real(i-1,dp)*wk)
   sfhx(i)=sin(real(i-1,dp)*wk)
  end do
  w1=0.0
  call dfftw_plan_dft_r2c_1d(plan1,n1,w1,w1,FFTW_ESTIMATE)
  call dfftw_plan_dft_c2r_1d(iplan1,n1,w1,w1, FFTW_ESTIMATE)

  allocate(w2(n2+2),cfhy(n2),sfhy(n2))
  w2=0.0
  call dfftw_plan_dft_r2c_1d(plan2,n2,w2,w2,FFTW_ESTIMATE)
  call dfftw_plan_dft_c2r_1d(iplan2,n2,w2,w2,FFTW_ESTIMATE)
  wk=acos(-1.0)/real(n2,dp)
  do i=1,n2
   cfhy(i)=cos(real(i-1,dp)*wk)
   sfhy(i)=sin(real(i-1,dp)*wk)
  end do
   allocate(w3(n3+2),cfhz(n3),sfhz(n3))
   w3=0.0
   call dfftw_plan_dft_r2c_1d(plan3,n3,w3,w3,FFTW_ESTIMATE)
   call dfftw_plan_dft_c2r_1d(iplan3,n3,w3,w3,FFTW_ESTIMATE)
   wk=acos(-1.0)/real(n3,dp)
   do i=1,n3
    cfhz(i)=cos(real(i-1,dp)*wk)
    sfhz(i)=sin(real(i-1,dp)*wk)
   end do
 case(1)
  allocate(w1(n1+2))
  w1=0.0
  call dfftw_plan_dft_r2c_1d(plan1,n1,w1,w1,FFTW_ESTIMATE)
  call dfftw_plan_dft_c2r_1d(iplan1,n1,w1,w1, FFTW_ESTIMATE)

  allocate(w2(n2+2))
  w2=0.0
  call dfftw_plan_dft_r2c_1d(plan2,n2,w2,w2,FFTW_ESTIMATE)
  call dfftw_plan_dft_c2r_1d(iplan2,n2,w2,w2,FFTW_ESTIMATE)
   allocate(w3(n3+2))
   w3=0.0
   call dfftw_plan_dft_r2c_1d(plan3,n3,w3,w3,FFTW_ESTIMATE)
   call dfftw_plan_dft_c2r_1d(iplan3,n3,w3,w3,FFTW_ESTIMATE)
 case(2) !for sin/cos transforms
  allocate(w1(2*n1+2))
  w1=0.0
  call dfftw_plan_dft_r2c_1d(plan1,2*n1,w1,w1,FFTW_ESTIMATE)
  call dfftw_plan_dft_c2r_1d(iplan1,2*n1,w1,w1, FFTW_ESTIMATE)
  if(n2>1)then
   allocate(w2(2*n2+2))
   w2=0.0
   call dfftw_plan_dft_r2c_1d(plan2,2*n2,w2,w2,FFTW_ESTIMATE)
   call dfftw_plan_dft_c2r_1d(iplan2,2*n2,w2,w2,FFTW_ESTIMATE)
  endif
   allocate(w3(2*n3+2))
   w3=0.0
   call dfftw_plan_dft_r2c_1d(plan3,2*n3,w3,w3,FFTW_ESTIMATE)
   call dfftw_plan_dft_c2r_1d(iplan3,2*n3,w3,w3,FFTW_ESTIMATE)
 end select
 end subroutine ftw_init
 !----------------------
 subroutine ftw_end

 if(allocated(w1))deallocate(w1)
 if(allocated(w1_st))deallocate(w1_st)
 if(allocated(w2))deallocate(w2)
 if(allocated(w3))deallocate(w3)
 if(allocated(cw))deallocate(cw)

 end subroutine ftw_end

 subroutine ftw1d_st(w,n1,n2,n3,is,dir)
 real(dp),intent(inout) :: w(:,:,:)

 integer,intent(in) :: n1,n2,n3,is,dir
 integer :: ii,ix,iy,iz,i1,i2,n1_tr,n2_tr
 real(dp) :: sc,wrr,wir,wri,wii

 !=============================
 !staggered (k_x,k_y,k_z)
 !===================
 select case(dir)
 case(1)
  if(is <0)then
   sc=1./real(n1,dp)
   do iz=1,n3
    do iy=1,n2
     do ix=1,n1
      w1(ix)=cfhx(ix)*w(ix,iy,iz)
     end do
     call dfftw_execute(plan1)
     w1_st(1:n1)=w1(1:n1)
     do ix=1,n1
      w1(ix)=sfhx(ix)*w(ix,iy,iz)
     end do
     call dfftw_execute(plan1)
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      w(i1,iy,iz)=sc*(w1_st(i1)+w1(i2))
      w(i2,iy,iz)=sc*(w1_st(i2)-w1(i1))
     end do
    end do
   end do
  else
   n1_tr=n1/2+n1/4
   do iz=1,n3
    do iy=1,n2
     w(n1_tr+1:n1,iy,iz)=0.0
     w1(n1+1:n1+2)=0.0
     w1(1:n1)=w(1:n1,iy,iz)
     call dfftw_execute(iplan1)
     w1_st(1:n1)=w1(1:n1)
     w1(1:n1+2)=0.0
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      w1(i1)=-w(i2,iy,iz)
      w1(i2)=w(i1,iy,iz)
     end do
     call dfftw_execute(iplan1)
     do ix=1,n1
      w(ix,iy,iz)=cfhx(ix)*w1_st(ix)+ &
       sfhx(ix)*w1(ix)
     end do
    end do
   end do
  endif
 case(2)
  allocate(cw(n1,n2))
  if(is <0)then
   sc=1./real(n2,dp)
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      do iy=1,n2
       w2(iy)=cfhy(iy)*w(ii,iy,iz)
      end do
      call dfftw_execute(plan2)
      do iy=1,n2
       cw(ii,iy)=sc*w2(iy)
       w2(iy)=sfhy(iy)*w(ii,iy,iz)
      end do
      call dfftw_execute(plan2)
      do iy=1,n2/2
       cw(ii,2*iy-1)=cw(ii,2*iy-1)+sc*w2(2*iy)
       cw(ii,2*iy)=cw(ii,2*iy)-sc*w2(2*iy-1)
      end do
     end do
    end do
    !=========== reordering as 2D fft
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2/2
      wrr=cw(i1,2*iy-1)
      wri=cw(i1,2*iy)
      wir=cw(i2,2*iy-1)
      wii=cw(i2,2*iy)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,n2+1-iy,iz)=wrr+wii
      w(i2,n2+1-iy,iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n2/2+n2/4
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2/2
      cw(i1,2*iy-1)=0.5*(w(i1,iy,iz)+w(i1,n2+1-iy,iz)) !wrr
      cw(i1,2*iy)=0.5*(w(i2,iy,iz)-w(i2,n2+1-iy,iz))   !wri
      cw(i2,2*iy-1)=0.5*(w(i2,iy,iz)+w(i2,n2+1-iy,iz)) !wir
      cw(i2,2*iy)=0.5*(w(i1,n2+1-iy,iz)-w(i1,iy,iz))   !wii
     end do
    end do
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      cw(ii,n2_tr+1:n2)=0.0
      w2(n2+1:n2+2)=0.0
      do iy=1,n2
       w2(iy)=cw(ii,iy)
      end do
      call dfftw_execute(iplan2)
      do iy=1,n2
       w(ii,iy,iz)=cfhy(iy)*w2(iy)
      end do
      w2(n2+1:n2+2)=0.0
      do iy=1,n2/2
       w2(2*iy-1)=-cw(ii,2*iy)
       w2(2*iy)=cw(ii,2*iy-1)
      end do
      call dfftw_execute(iplan2)
      do iy=1,n2
       w(ii,iy,iz)=w(ii,iy,iz)+sfhy(iy)*w2(iy)
      end do
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 case(3)
  allocate(cw(n1,n3))
  if(is <0)then
   sc=1./real(n3,dp)
   do iy=1,n2
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      do iz=1,n3
       w3(iz)=cfhz(iz)*w(ii,iy,iz)
      end do
      call dfftw_execute(plan3)
      do iz=1,n3
       cw(ii,iz)=sc*w3(iz)
       w3(iz)=sfhz(iz)*w(ii,iy,iz)
      end do
      call dfftw_execute(plan3)
      do iz=1,n3/2
       cw(ii,2*iz-1)=cw(ii,2*iz-1)+sc*w3(2*iz)
       cw(ii,2*iz)=cw(ii,2*iz)-sc*w3(2*iz-1)
      end do
     end do
    end do
    !=========== reordering as 2D fft
    do iz=1,n3/2
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      wrr=cw(i1,2*iz-1)
      wri=cw(i1,2*iz)
      wir=cw(i2,2*iz-1)
      wii=cw(i2,2*iz)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,iy,n3+1-iz)=wrr+wii
      w(i2,iy,n3+1-iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n3/2+n3/4
   ! First reorders
   do iy=1,n2
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iz=1,n3/2
      cw(i1,2*iz-1)=0.5*(w(i1,iy,iz)+w(i1,iy,n3+1-iz))  !wrr
      cw(i1,2*iz)=0.5*(w(i2,iy,iz)-w(i2,iy,n3+1-iz))    !wri
      cw(i2,2*iz-1)=0.5*(w(i2,iy,iz)+w(i2,iy,n3+1-iz)) !wir
      cw(i2,2*iz)=0.5*(w(i1,iy,n3+1-iz)-w(i1,iy,iz))   !wii
     end do
    end do
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      cw(ii,n2_tr+1:n3)=0.0
      w3(n3+1:n3+2)=0.0
      do iz=1,n3
       w3(iz)=cw(ii,iz)
      end do
      call dfftw_execute(iplan3)
      do iz=1,n3
       w(ii,iy,iz)=cfhz(iz)*w3(iz)
      end do
      w3(n3+1:n3+2)=0.0
      do iz=1,n3/2
       w3(2*iz-1)=-cw(ii,2*iz)
       w3(2*iz)=cw(ii,2*iz-1)
      end do
      call dfftw_execute(iplan3)
      do iz=1,n3
       w(ii,iy,iz)=w(ii,iy,iz)+sfhz(iz)*w3(iz)
      end do
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 end select
 end subroutine ftw1d_st
 !====================
 subroutine ftw1d(w,n1,n2,n3,is,dir)
 real(dp),intent(inout) :: w(:,:,:)

 integer,intent(in) :: n1,n2,n3,is,dir
 integer :: ix,iy,iz,i1,i2,n1_tr,n2_tr
 real(dp) :: sc,wrr,wir,wri,wii

 select case(dir)
 case(1)
  if(is <0)then
   sc=1./real(n1,dp)
   do iz=1,n3
    do iy=1,n2
     w1(1:n1)=w(1:n1,iy,iz)
     call dfftw_execute(plan1)
     w(1:n1,iy,iz)=sc*w1(1:n1)
    end do
   end do
  else
   n1_tr=n1
   do iz=1,n3
    do iy=1,n2
     w1(n1+1:n1+2)=0.0
     w1(1:n1_tr)=w(1:n1_tr,iy,iz)
     call dfftw_execute(iplan1)
     w(1:n1,iy,iz)=w1(1:n1)
    end do
   end do
  endif
 case(2)
  allocate(cw(n1,n2))
  if(is <0)then
   sc=1./real(n2,dp)
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2
      w2(iy)=w(i1,iy,iz)
     end do
     call dfftw_execute(plan2)
     do iy=1,n2
      cw(i1,iy)=sc*w2(iy)
      w2(iy)=w(i2,iy,iz)
     end do
     call dfftw_execute(plan2)
     do iy=1,n2
      cw(i2,iy)=sc*w2(iy)
     end do
    end do
    !=========== reordering as 2D fft
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     w(i1:i2,1:n2,iz)=0.
     w(i1:i2,1,iz)=cw(i1:i2,1)
     do iy=2,n2/2
      wrr=cw(i1,2*iy-1)
      wri=cw(i1,2*iy)
      wir=cw(i2,2*iy-1)
      wii=cw(i2,2*iy)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,n2+2-iy,iz)=wrr+wii
      w(i2,n2+2-iy,iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n2/2
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2
      cw(i1:i2,iy)=w(i1:i2,iy,iz)
     end do
     w2(1:n2+2)=0.0
     w2(1)=cw(i1,1)
     do iy=2,n2_tr
      w2(2*iy-1)=0.5*(cw(i1,iy)+cw(i1,n2+2-iy))
      w2(2*iy)=0.5*(cw(i2,iy)-cw(i2,n2+2-iy))
     end do
     call dfftw_execute(iplan2)
     do iy=1,n2
      w(i1,iy,iz)=w2(iy)
     end do
     w2(1:n2+2)=0.0
     w2(1)=cw(i2,1)
     do iy=2,n2_tr
      w2(2*iy-1)=0.5*(cw(i2,iy)+cw(i2,n2+2-iy))
      w2(2*iy)=0.5*(cw(i1,n2+2-iy)-cw(i1,iy))
     end do
     call dfftw_execute(iplan2)
     do iy=1,n2
      w(i2,iy,iz)=w2(iy)
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 case(3)
  allocate(cw(n1,n3))
  if(is <0)then
   sc=1./real(n3,dp)
   do iy=1,n2
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iz=1,n3
      w3(iz)=w(i1,iy,iz)
     end do
     call dfftw_execute(plan3)
     do iz=1,n3
      cw(i1,iz)=sc*w3(iz)
      w3(iz)=w(i2,iy,iz)
     end do
     call dfftw_execute(plan3)
     do iz=1,n3
      cw(i2,iz)=sc*w3(iz)
     end do
    end do
    !=========== reordering as 2D fft
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     w(i1:i2,iy,1)=cw(i1:i2,1)
    end do
    do iz=2,n3/2
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      wrr=cw(i1,2*iz-1)
      wri=cw(i1,2*iz)
      wir=cw(i2,2*iz-1)
      wii=cw(i2,2*iz)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,iy,n3+2-iz)=wrr+wii
      w(i2,iy,n3+2-iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n3/2
   do iy=1,n2
    do iz=1,n3
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      cw(i1:i2,iz)=w(i1:i2,iy,iz)
     end do
    end do
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     w3(1:n3+2)=0.0
     w3(1)=cw(i1,1)
     do iz=2,n2_tr
      w3(2*iz-1)=0.5*(cw(i1,iz)+cw(i1,n3+2-iz))  !wrr
      w3(2*iz)=0.5*(cw(i2,iz)-cw(i2,n3+2-iz))    !wri
     end do
     call dfftw_execute(iplan3)
     do iz=1,n3
      w(i1,iy,iz)=w3(iz)
     end do
     w3(1:n3+2)=0.0
     w3(1)=cw(i2,1)
     do iz=2,n2_tr
      w3(2*iz-1)=0.5*(cw(i2,iz)+cw(i2,n3+2-iz)) !wir
      w3(2*iz)=0.5*(cw(i1,n3+2-iz)-cw(i1,iz))   !wii
     end do
     call dfftw_execute(iplan3)
     do iz=1,n3
      w(i2,iy,iz)=w3(iz)
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 end select
 end subroutine ftw1d
 !================
 subroutine ft_kern(w,n1,is)
 real(dp),intent(inout) :: w(:)
 integer,intent(in) :: n1,is
 integer :: ix,i2
 real(dp) :: sc
 integer :: ndb

 ndb=2*n1
 sc=1.
 if(is <0)then
  w1(1:n1)=w(1:n1)
  w1(n1+1)=0.0
  do ix=n1+2,ndb
   w1(ix)=-w1(ndb+2-ix)
  end do
  call dfftw_execute(plan1)
  do ix=1,n1
   i2=2*ix
   w(ix)=sc*w1(i2)
  end do
 else
  w1(1:n1)=w(1:n1)
  do ix=n1+2,ndb
   w1(ix)=w1(ndb+2-ix)
  end do
  w1(n1+1)=0.5*(w1(n1)+w1(n1+2))
  call dfftw_execute(plan1)
  do ix=1,n1
   i2=2*ix-1
   w(ix)=sc*w1(i2)
  end do
 endif
 end subroutine ft_kern
 !==========================
 subroutine ftw1d_sc(w,n1,n2,n3,is,dir,sym)
 real(dp),intent(inout) :: w(:,:,:)

 integer,intent(in) :: n1,n2,n3,is,dir,sym
 integer :: ix,iy,iz,i2,n1_tr,n2_tr
 real(dp) :: sc
 integer :: ndb

 select case(dir)
 case(1)
  ndb=2*n1
  if(is <0)then    !grid to Fourier space
   sc=1./real(ndb,dp)
   if(sym==1)then          !sin
    do iz=1,n3
     do iy=1,n2
      w1(1:n1)=w(1:n1,iy,iz)
      w1(n1+1)=0.0
      do ix=n1+2,ndb
       w1(ix)=-w1(ndb+2-ix)
      end do
      call dfftw_execute(plan1)
      do ix=1,n1
       i2=2*ix
       w(ix,iy,iz)=sc*w1(i2)
      end do
     end do
    end do
   else
    do iz=1,n3          !cos
     do iy=1,n2
      w1(1:n1)=w(1:n1,iy,iz)
      do ix=n1+2,ndb
       w1(ix)=w1(ndb+2-ix)
      end do
      w1(n1+1)=0.5*(w1(n1)+w1(n1+2))
      call dfftw_execute(plan1)
      do ix=1,n1
       i2=2*ix-1
       w(ix,iy,iz)=sc*w1(i2)
      end do
     end do
    end do
   endif
  else
   n1_tr=n1
   if(sym==1)then
    do iz=1,n3
     do iy=1,n2
      w1=0.0
      do ix=1,n1
       i2=2*ix
       w1(i2)=w(ix,iy,iz)
      end do
      call dfftw_execute(iplan1)
      w(1:n1,iy,iz)=w1(1:n1)
     end do
    end do
   else
    do iz=1,n3
     do iy=1,n2
      w1=0.0
      do ix=1,n1
       i2=2*ix-1
       w1(i2)=w(ix,iy,iz)
      end do
      call dfftw_execute(iplan1)
      w(1:n1,iy,iz)=w1(1:n1)
     end do
    end do
   endif
  endif
 case(2)
  ndb=2*n2
  allocate(cw(n1,n2))
  if(is <0)then
   sc=1./real(ndb,dp)
   do iz=1,n3
    do ix=1,n1
     do iy=1,n2
      w2(iy)=w(ix,iy,iz)
     end do
     w2(n2+1)=0.0
     do iy=n2+2,ndb
      w2(iy)=-w2(ndb+2-iy)
     end do
     call dfftw_execute(plan2)
     do iy=1,n2
      cw(ix,iy)=sc*w2(2*iy)
     end do
    end do
    do iy=1,n2
     do ix=1,n1
      w(ix,iy,iz)=cw(ix,iy)
     end do
    end do
   end do
  else
   n2_tr=n2
   do iz=1,n3
    do ix=1,n1
     w2=0.0
     do iy=1,n2
      w2(2*iy)=w(ix,iy,iz)
     end do
     call dfftw_execute(iplan2)
     do iy=1,n2
      cw(ix,iy)=w2(iy)
     end do
    end do
    w(1:n1,1:n2,iz)=cw(1:n1,1:n2)
   end do
  endif
  if(allocated(cw))deallocate(cw)
 case(3)
  ndb=2*n3
  allocate(cw(n1,n3))
  if(is <0)then
   sc=1./real(ndb,dp)
   do iy=1,n2
    do ix=1,n1
     do iz=1,n3
      w3(iz)=w(ix,iy,iz)
     end do
     w3(n3+1)=0.0
     do iz=n3+2,ndb
      w3(iz)=-w3(ndb+2-iz)
     end do
     call dfftw_execute(plan3)
     do iz=1,n3
      cw(ix,iz)=sc*w3(2*iz)
     end do
    end do
    do iz=1,n3
     do ix=1,n1
      w(ix,iy,iz)=cw(ix,iz)
     end do
    end do
   end do
  else
   n2_tr=n3
   do iy=1,n2
    do ix=1,n1
     w3=0.0
     do iz=1,n3
      w3(2*iz)=w(ix,iy,iz)
     end do
     call dfftw_execute(iplan3)
     do iz=1,n3
      cw(ix,iz)=w3(iz)
     end do
    end do
    do iz=1,n3
     do ix=1,n1
      w(ix,iy,iz)=cw(ix,iz)
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 end select
 end subroutine ftw1d_sc


#else

 use, intrinsic :: iso_c_binding
 implicit none
 include 'fftw3.f03'

 type(c_ptr) :: plan1,iplan1,plan2,iplan2,plan3,iplan3

 real(dp),allocatable :: cw(:,:),w1(:),w1_st(:),w2(:),w3(:)
 complex(dp),allocatable :: w1c(:),w2c(:),w3c(:)
 real(dp),allocatable :: cfhx(:),sfhx(:)
 real(dp),allocatable :: cfhy(:),sfhy(:)
 real(dp),allocatable :: cfhz(:),sfhz(:)

 contains

 subroutine ftw_init(n1,n2,n3,ind_ft)
 integer,intent(in) :: n1,n2,n3,ind_ft
 integer :: i,nm
 real(dp) :: wk

 select case(ind_ft)
 case(0)
  nm=max(n1,n2)
  allocate(w1(n1+2),w1_st(nm+1),cfhx(n1),sfhx(n1))
  allocate(w1c(n1+2))
  w1=0.0
  w1c=0.0
  wk=acos(-1.0)/real(n1,dp)
  do i=1,n1
   cfhx(i)=cos(real(i-1,dp)*wk)
   sfhx(i)=sin(real(i-1,dp)*wk)
  end do
  plan1 = fftw_plan_dft_r2c_1d(n1,w1,w1c,FFTW_ESTIMATE)
  iplan1 = fftw_plan_dft_c2r_1d(n1,w1c,w1, FFTW_ESTIMATE)

  allocate(w2(n2+2),cfhy(n2),sfhy(n2))
  allocate(w2c(n2+2))
  w2=0.0
  w2c=0.0
  plan2 = fftw_plan_dft_r2c_1d(n2,w2,w2c,FFTW_ESTIMATE)
  iplan2 = fftw_plan_dft_c2r_1d(n2,w2c,w2,FFTW_ESTIMATE)
  wk=acos(-1.0)/real(n2,dp)
  do i=1,n2
   cfhy(i)=cos(real(i-1,dp)*wk)
   sfhy(i)=sin(real(i-1,dp)*wk)
  end do

   allocate(w3(n3+2),cfhz(n3),sfhz(n3))
   allocate(w3c(n3+2))
   w3=0.0
   w3c=0.0
   plan3 = fftw_plan_dft_r2c_1d(n3,w3,w3c,FFTW_ESTIMATE)
   iplan3 = fftw_plan_dft_c2r_1d(n3,w3c,w3,FFTW_ESTIMATE)
   wk=acos(-1.0)/real(n3,dp)
   do i=1,n3
    cfhz(i)=cos(real(i-1,dp)*wk)
    sfhz(i)=sin(real(i-1,dp)*wk)
   end do
 case(1)
  allocate(w1(n1+2))
  allocate(w1c(n1+2))
  w1=0.0
  w1c=0.0
  plan1 = fftw_plan_dft_r2c_1d(n1,w1,w1c,FFTW_ESTIMATE)
  iplan1 = fftw_plan_dft_c2r_1d(n1,w1c,w1, FFTW_ESTIMATE)

  allocate(w2(n2+2))
  allocate(w2c(n2+2))
  w2=0.0
  w2c=0.0
  plan2 = fftw_plan_dft_r2c_1d(n2,w2,w2c,FFTW_ESTIMATE)
  iplan2 = fftw_plan_dft_c2r_1d(n2,w2c,w2,FFTW_ESTIMATE)

   allocate(w3(n3+2))
   allocate(w3c(n3+2))
   w3=0.0
   w3c=0.0
   plan3 = fftw_plan_dft_r2c_1d(n3,w3,w3c,FFTW_ESTIMATE)
   iplan3 = fftw_plan_dft_c2r_1d(n3,w3c,w3,FFTW_ESTIMATE)

 case(2)              !for sin/cosine transforms
  allocate(w1(2*n1+2))
  allocate(w1c(2*n1+2))
  w1=0.0
  w1c=0.0
  plan1 = fftw_plan_dft_r2c_1d(2*n1,w1,w1c,FFTW_ESTIMATE)
  iplan1 = fftw_plan_dft_c2r_1d(2*n1,w1c,w1,FFTW_ESTIMATE)

  allocate(w2(2*n2+2))
  allocate(w2c(2*n2+2))
  w2=0.0
  w2c=0.0
  plan2 = fftw_plan_dft_r2c_1d(2*n2,w2,w2c,FFTW_ESTIMATE)
  iplan2 = fftw_plan_dft_c2r_1d(2*n2,w2c,w2,FFTW_ESTIMATE)

   allocate(w3(2*n3+2))
   allocate(w3c(2*n3+2))
   w3=0.0
   w3c=0.0
   plan3 = fftw_plan_dft_r2c_1d(2*n3,w3,w3c,FFTW_ESTIMATE)
   iplan3 = fftw_plan_dft_c2r_1d(2*n3,w3c,w3,FFTW_ESTIMATE)
 end select
 end subroutine ftw_init

 !=============================

 subroutine ftw_end

 if(allocated(w1))deallocate(w1)
 if(allocated(w1_st))deallocate(w1_st)
 if(allocated(w2))deallocate(w2)
 if(allocated(w3))deallocate(w3)
 if(allocated(cw))deallocate(cw)

 end subroutine ftw_end

 !=============================

 subroutine ftw1d_st(w,n1,n2,n3,is,dir)
 real(dp),intent(inout) :: w(:,:,:)
 integer,intent(in) :: n1,n2,n3,is,dir
 integer :: ii,ix,iy,iz,i1,i2,n1_tr,n2_tr
 real(dp) :: sc,wrr,wir,wri,wii


 !=============================
 !staggered (k_x,k_y,k_z)
 !===================
 select case(dir)
 case(1)
  if(is <0)then
   sc=1./real(n1,dp)
   do iz=1,n3
    do iy=1,n2
     do ix=1,n1
      w1(ix)=cfhx(ix)*w(ix,iy,iz)
     end do
     call fftw_execute_dft_r2c(plan1,w1,w1c)
     w1=w1c
     w1_st(1:n1)=w1(1:n1)
     do ix=1,n1
      w1(ix)=sfhx(ix)*w(ix,iy,iz)
     end do
     call fftw_execute_dft_r2c(plan1,w1,w1c)
     w1=w1c
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      w(i1,iy,iz)=sc*(w1_st(i1)+w1(i2))
      w(i2,iy,iz)=sc*(w1_st(i2)-w1(i1))
     end do
    end do
   end do
  else
   n1_tr=n1/2+n1/4
   do iz=1,n3
    do iy=1,n2
     w(n1_tr+1:n1,iy,iz)=0.0
     w1(n1+1:n1+2)=0.0
     w1(1:n1)=w(1:n1,iy,iz)
     call fftw_execute_dft_c2r(iplan1,w1c,w1)
     w1c=w1
     w1_st(1:n1)=w1(1:n1)
     w1(1:n1+2)=0.0
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      w1(i1)=-w(i2,iy,iz)
      w1(i2)=w(i1,iy,iz)
     end do
     call fftw_execute_dft_c2r(iplan1,w1c,w1)
     w1c=w1
     do ix=1,n1
      w(ix,iy,iz)=cfhx(ix)*w1_st(ix)+ &
       sfhx(ix)*w1(ix)
     end do
    end do
   end do
  endif
 case(2)
  allocate(cw(n1,n2))
  if(is <0)then
   sc=1./real(n2,dp)
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      do iy=1,n2
       w2(iy)=cfhy(iy)*w(ii,iy,iz)
      end do
      call fftw_execute_dft_r2c(plan2,w2,w2c)
      w2=w2c
      do iy=1,n2
       cw(ii,iy)=sc*w2(iy)
       w2(iy)=sfhy(iy)*w(ii,iy,iz)
      end do
      call fftw_execute_dft_r2c(plan2,w2,w2c)
      w2=w2c
      do iy=1,n2/2
       cw(ii,2*iy-1)=cw(ii,2*iy-1)+sc*w2(2*iy)
       cw(ii,2*iy)=cw(ii,2*iy)-sc*w2(2*iy-1)
      end do
     end do
    end do
    !=========== reordering as 2D fft
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2/2
      wrr=cw(i1,2*iy-1)
      wri=cw(i1,2*iy)
      wir=cw(i2,2*iy-1)
      wii=cw(i2,2*iy)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,n2+1-iy,iz)=wrr+wii
      w(i2,n2+1-iy,iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n2/2+n2/4
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2/2
      cw(i1,2*iy-1)=0.5*(w(i1,iy,iz)+w(i1,n2+1-iy,iz)) !wrr
      cw(i1,2*iy)=0.5*(w(i2,iy,iz)-w(i2,n2+1-iy,iz))   !wri
      cw(i2,2*iy-1)=0.5*(w(i2,iy,iz)+w(i2,n2+1-iy,iz)) !wir
      cw(i2,2*iy)=0.5*(w(i1,n2+1-iy,iz)-w(i1,iy,iz))   !wii
     end do
    end do
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      cw(ii,n2_tr+1:n2)=0.0
      w2(n2+1:n2+2)=0.0
      do iy=1,n2
       w2(iy)=cw(ii,iy)
      end do
      call fftw_execute_dft_c2r(iplan2,w2c,w2)
      w2c=w2
      do iy=1,n2
       w(ii,iy,iz)=cfhy(iy)*w2(iy)
      end do
      w2(n2+1:n2+2)=0.0
      do iy=1,n2/2
       w2(2*iy-1)=-cw(ii,2*iy)
       w2(2*iy)=cw(ii,2*iy-1)
      end do
      call fftw_execute_dft_c2r(iplan2,w2c,w2)
      w2c=w2
      do iy=1,n2
       w(ii,iy,iz)=w(ii,iy,iz)+sfhy(iy)*w2(iy)
      end do
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 case(3)
  allocate(cw(n1,n3))
  if(is <0)then
   sc=1./real(n3,dp)
   do iy=1,n2
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      do iz=1,n3
       w3(iz)=cfhz(iz)*w(ii,iy,iz)
      end do
      call fftw_execute_dft_r2c(plan3,w3,w3c)
      w3=w3c
      do iz=1,n3
       cw(ii,iz)=sc*w3(iz)
       w3(iz)=sfhz(iz)*w(ii,iy,iz)
      end do
      call fftw_execute_dft_r2c(plan3,w3,w3c)
      w3=w3c
      do iz=1,n3/2
       cw(ii,2*iz-1)=cw(ii,2*iz-1)+sc*w3(2*iz)
       cw(ii,2*iz)=cw(ii,2*iz)-sc*w3(2*iz-1)
      end do
     end do
    end do
    !=========== reordering as 2D fft
    do iz=1,n3/2
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      wrr=cw(i1,2*iz-1)
      wri=cw(i1,2*iz)
      wir=cw(i2,2*iz-1)
      wii=cw(i2,2*iz)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,iy,n3+1-iz)=wrr+wii
      w(i2,iy,n3+1-iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n3/2+n3/4
   ! First reorders
   do iy=1,n2
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iz=1,n3/2
      cw(i1,2*iz-1)=0.5*(w(i1,iy,iz)+w(i1,iy,n3+1-iz))  !wrr
      cw(i1,2*iz)=0.5*(w(i2,iy,iz)-w(i2,iy,n3+1-iz))    !wri
      cw(i2,2*iz-1)=0.5*(w(i2,iy,iz)+w(i2,iy,n3+1-iz)) !wir
      cw(i2,2*iz)=0.5*(w(i1,iy,n3+1-iz)-w(i1,iy,iz))   !wii
     end do
    end do
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do ii=i1,i2
      cw(ii,n2_tr+1:n3)=0.0
      w3(n3+1:n3+2)=0.0
      do iz=1,n3
       w3(iz)=cw(ii,iz)
      end do
      call fftw_execute_dft_c2r(iplan3,w3c,w3)
      w3c=w3
      do iz=1,n3
       w(ii,iy,iz)=cfhz(iz)*w3(iz)
      end do
      w3(n3+1:n3+2)=0.0
      do iz=1,n3/2
       w3(2*iz-1)=-cw(ii,2*iz)
       w3(2*iz)=cw(ii,2*iz-1)
      end do
      call fftw_execute_dft_c2r(iplan3,w3c,w3)
      w3c=w3
      do iz=1,n3
       w(ii,iy,iz)=w(ii,iy,iz)+sfhz(iz)*w3(iz)
      end do
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 end select
 end subroutine ftw1d_st

 !====================

 subroutine ftw1d(w,n1,n2,n3,is,dir)
 real(dp),intent(inout) :: w(:,:,:)
 integer,intent(in) :: n1,n2,n3,is,dir
 integer :: ix,iy,iz,i1,i2,n1_tr,n2_tr
 real(dp) :: sc,wrr,wir,wri,wii
!============
! 1D fft along one of the dir=x,y,z coordinate

 select case(dir)
 case(1)
  if(is <0)then
   sc=1./real(n1,dp)
   do iz=1,n3
    do iy=1,n2
     w1(1:n1)=w(1:n1,iy,iz)
     call fftw_execute_dft_r2c(plan1,w1,w1c)
     w1=w1c
     w(1:n1,iy,iz)=sc*w1(1:n1)
    end do
   end do
  else
   n1_tr=n1
   do iz=1,n3
    do iy=1,n2
     w1(1:n1+2)=0.0
     w1(1:n1_tr)=w(1:n1_tr,iy,iz)
     call fftw_execute_dft_c2r(iplan1,w1c,w1)
     w1c=w1
     w(1:n1,iy,iz)=w1(1:n1)
    end do
   end do
  endif
 case(2)
  allocate(cw(n1,n2))
  if(is <0)then
   sc=1./real(n2,dp)
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2
      w2(iy)=w(i1,iy,iz)
     end do
     call fftw_execute_dft_r2c(plan2,w2,w2c)
     w2=w2c
     do iy=1,n2
      cw(i1,iy)=sc*w2(iy)
      w2(iy)=w(i2,iy,iz)
     end do
     call fftw_execute_dft_r2c(plan2,w2,w2c)
     w2=w2c
     do iy=1,n2
      cw(i2,iy)=sc*w2(iy)
     end do
    end do
    !=========== reordering as 2D fft
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     w(i1:i2,1:n2,iz)=0.
     w(i1:i2,1,iz)=cw(i1:i2,1)
     do iy=2,n2/2
      wrr=cw(i1,2*iy-1)
      wri=cw(i1,2*iy)
      wir=cw(i2,2*iy-1)
      wii=cw(i2,2*iy)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,n2+2-iy,iz)=wrr+wii
      w(i2,n2+2-iy,iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n2/2
   do iz=1,n3
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iy=1,n2
      cw(i1:i2,iy)=w(i1:i2,iy,iz)
     end do
     w2(1:n2+2)=0.0
     w2(1)=cw(i1,1)
     do iy=2,n2_tr
      w2(2*iy-1)=0.5*(cw(i1,iy)+cw(i1,n2+2-iy))
      w2(2*iy)=0.5*(cw(i2,iy)-cw(i2,n2+2-iy))
     end do
     call fftw_execute_dft_c2r(iplan2,w2c,w2)
     w2c=w2
     do iy=1,n2
      w(i1,iy,iz)=w2(iy)
     end do
     w2(1:n2+2)=0.0
     w2(1)=cw(i2,1)
     do iy=2,n2_tr
      w2(2*iy-1)=0.5*(cw(i2,iy)+cw(i2,n2+2-iy))
      w2(2*iy)=0.5*(cw(i1,n2+2-iy)-cw(i1,iy))
     end do
     call fftw_execute_dft_c2r(iplan2,w2c,w2)
     w2c=w2
     do iy=1,n2
      w(i2,iy,iz)=w2(iy)
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 case(3)
  allocate(cw(n1,n3))
  if(is <0)then
   sc=1./real(n3,dp)
   do iy=1,n2
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     do iz=1,n3
      w3(iz)=w(i1,iy,iz)
     end do
     call fftw_execute_dft_r2c(plan3,w3,w3c)
     w3=w3c
     do iz=1,n3
      cw(i1,iz)=sc*w3(iz)
      w3(iz)=w(i2,iy,iz)
     end do
     call fftw_execute_dft_r2c(plan3,w3,w3c)
     w3=w3c
     do iz=1,n3
      cw(i2,iz)=sc*w3(iz)
     end do
    end do
    !=========== reordering as 2D fft
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     w(i1:i2,iy,1)=cw(i1:i2,1)
    end do
    do iz=2,n3/2
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      wrr=cw(i1,2*iz-1)
      wri=cw(i1,2*iz)
      wir=cw(i2,2*iz-1)
      wii=cw(i2,2*iz)
      w(i1,iy,iz)=wrr-wii
      w(i2,iy,iz)=wri+wir
      w(i1,iy,n3+2-iz)=wrr+wii
      w(i2,iy,n3+2-iz)=wir-wri
     end do
    end do
   end do
  else
   n2_tr=n3/2
   do iy=1,n2
    do iz=1,n3
     do ix=1,n1/2
      i2=2*ix
      i1=i2-1
      cw(i1:i2,iz)=w(i1:i2,iy,iz)
     end do
    end do
    do ix=1,n1/2
     i2=2*ix
     i1=i2-1
     w3(1:n3+2)=0.0
     w3(1)=cw(i1,1)
     do iz=2,n2_tr
      w3(2*iz-1)=0.5*(cw(i1,iz)+cw(i1,n3+2-iz))  !wrr
      w3(2*iz)=0.5*(cw(i2,iz)-cw(i2,n3+2-iz))    !wri
     end do
     call fftw_execute_dft_c2r(iplan3,w3c,w3)
     w3c=w3
     do iz=1,n3
      w(i1,iy,iz)=w3(iz)
     end do
     w3(1:n3+2)=0.0
     w3(1)=cw(i2,1)
     do iz=2,n2_tr
      w3(2*iz-1)=0.5*(cw(i2,iz)+cw(i2,n3+2-iz)) !wir
      w3(2*iz)=0.5*(cw(i1,n3+2-iz)-cw(i1,iz))   !wii
     end do
     call fftw_execute_dft_c2r(iplan3,w3c,w3)
     w3c=w3
     do iz=1,n3
      w(i2,iy,iz)=w3(iz)
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 end select
 end subroutine ftw1d

 !================

 subroutine ft_kern(w,n1,is)
 real(dp),intent(inout) :: w(:)
 integer,intent(in) :: n1,is
 integer :: ix,i2
 real(dp) :: sc
 integer :: ndb

 ndb=2*n1
 sc=1.
 if(is <0)then
  w1(1:n1)=w(1:n1)
  w1(n1+1)=0.0
  do ix=n1+2,ndb
   w1(ix)=-w1(ndb+2-ix)
  end do
  call fftw_execute_dft_r2c(plan1,w1,w1c)
  w1=w1c
  do ix=1,n1
   i2=2*ix
   w(ix)=sc*w1(i2)
  end do
 else
  w1(1:n1)=w(1:n1)
  do ix=n1+2,ndb
   w1(ix)=w1(ndb+2-ix)
  end do
  w1(n1+1)=0.5*(w1(n1)+w1(n1+2))
  call fftw_execute_dft_r2c(plan1,w1,w1c)
  w1=w1c
  do ix=1,n1
   i2=2*ix-1
   w(ix)=sc*w1(i2)
  end do
 endif
 end subroutine ft_kern

 !==========================

 subroutine ftw1d_sc(w,n1,n2,n3,is,dir,sym)
 real(dp),intent(inout) :: w(:,:,:)

 integer,intent(in) :: n1,n2,n3,is,dir,sym
 integer :: ix,iy,iz,i2,n1_tr,n2_tr
 real(dp) :: sc
 integer :: ndb
!======================================
!  1D sin/cos transform along a dir=x,y,z coordinate
! sym =1 is for sin transform
! else is for cosine transform
!========================================

 select case(dir)
 case(1)
  ndb=2*n1
  if(is <0)then
   sc=1./real(ndb,dp)
   if(sym==1)then
    do iz=1,n3
     do iy=1,n2
      w1(1:n1)=w(1:n1,iy,iz)
      w1(n1+1)=0.0
      do ix=n1+2,ndb
       w1(ix)=-w1(ndb+2-ix)
      end do
      call fftw_execute_dft_r2c(plan1,w1,w1c)
      w1=w1c
      do ix=1,n1
       i2=2*ix
       w(ix,iy,iz)=sc*w1(i2)
      end do
     end do
    end do
   else
    do iz=1,n3
     do iy=1,n2
      w1(1:n1)=w(1:n1,iy,iz)
      do ix=n1+2,ndb
       w1(ix)=w1(ndb+2-ix)
      end do
      w1(n1+1)=0.5*(w1(n1)+w1(n1+2))
      call fftw_execute_dft_r2c(plan1,w1,w1c)
      w1=w1c
      do ix=1,n1
       i2=2*ix-1
       w(ix,iy,iz)=sc*w1(i2)
      end do
     end do
    end do
   endif
  else
   n1_tr=n1
   if(sym==1)then
    do iz=1,n3
     do iy=1,n2
      w1=0.0
      do ix=1,n1
       i2=2*ix
       w1(i2)=w(ix,iy,iz)
      end do
      call fftw_execute_dft_c2r(iplan1,w1c,w1)
      w1c=w1
      w(1:n1,iy,iz)=w1(1:n1)
     end do
    end do
   else
    do iz=1,n3
     do iy=1,n2
      w1=0.0
      do ix=1,n1
       i2=2*ix-1
       w1(i2)=w(ix,iy,iz)
      end do
      call fftw_execute_dft_c2r(iplan1,w1c,w1)
      w1c=w1
      w(1:n1,iy,iz)=w1(1:n1)
     end do
    end do
   endif
  endif
 case(2)
  ndb=2*n2
  allocate(cw(n1,n2))
  if(is <0)then
   sc=1./real(ndb,dp)
   do iz=1,n3
    do ix=1,n1
     do iy=1,n2
      w2(iy)=w(ix,iy,iz)
     end do
     w2(n2+1)=0.0
     do iy=n2+2,ndb
      w2(iy)=-w2(ndb+2-iy)
     end do
     call fftw_execute_dft_r2c(plan2,w2,w2c)
     w2=w2c
     do iy=1,n2
      cw(ix,iy)=sc*w2(2*iy)
     end do
    end do
    do iy=1,n2
     do ix=1,n1
      w(ix,iy,iz)=cw(ix,iy)
     end do
    end do
   end do
  else
   n2_tr=n2
   do iz=1,n3
    do ix=1,n1
     w2=0.0
     do iy=1,n2
      w2(2*iy)=w(ix,iy,iz)
     end do
     call fftw_execute_dft_c2r(iplan2,w2c,w2)
     w2c=w2
     do iy=1,n2
      cw(ix,iy)=w2(iy)
     end do
    end do
    w(1:n1,1:n2,iz)=cw(1:n1,1:n2)
   end do
  endif
  if(allocated(cw))deallocate(cw)
 case(3)
  ndb=2*n3
  allocate(cw(n1,n3))
  if(is <0)then
   sc=1./real(ndb,dp)
   do iy=1,n2
    do ix=1,n1
     do iz=1,n3
      w3(iz)=w(ix,iy,iz)
     end do
     w3(n3+1)=0.0
     do iz=n3+2,ndb
      w3(iz)=-w3(ndb+2-iz)
     end do
     call fftw_execute_dft_r2c(plan3,w3,w3c)
     w3=w3c
     do iz=1,n3
      cw(ix,iz)=sc*w3(2*iz)
     end do
    end do
    do iz=1,n3
     do ix=1,n1
      w(ix,iy,iz)=cw(ix,iz)
     end do
    end do
   end do
  else
   n2_tr=n3
   do iy=1,n2
    do ix=1,n1
     w3=0.0
     do iz=1,n3
      w3(2*iz)=w(ix,iy,iz)
     end do
     call fftw_execute_dft_c2r(iplan3,w3c,w3)
     w3c=w3
     do iz=1,n3
      cw(ix,iz)=w3(iz)
     end do
    end do
    do iz=1,n3
     do ix=1,n1
      w(ix,iy,iz)=cw(ix,iz)
     end do
    end do
   end do
  endif
  if(allocated(cw))deallocate(cw)
 end select
 end subroutine ftw1d_sc


#endif

 end module fft_lib
