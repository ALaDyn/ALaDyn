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

 module psolv
 use parallel
 use all_param
 implicit none
  real(dp),allocatable :: wb(:,:,:)
 !--------------------------

 contains
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
  call ftw1d_sc(wp,i2,j2,k2,-1,1,1)   !ft_sin along the x direction
  do k=1,k2
   do j=1,j2
    do i=2,i2
     wp(i,j,k)=wp(i,j,k)*kern(i)
    end do
   end do
  end do
  call ftw1d_sc(wp,i2,j2,k2,1,1,1)
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
 !--------------------------
 subroutine beam_2D_potential(pot,nxf,n2_loc,n3_loc,ft_ind)
 real(dp),intent(inout) :: pot(:,:,:)
 integer,intent(in) :: nxf,n2_loc,n3_loc,ft_ind
 real(dp) :: ak2p
 integer :: ix,iy,iy1,iz,iz1
 !_________________________________
 ! Laplacian(y,z)(pot)=-rho =>  [k^2_y+k^2_z][pot(ky,kz)]=rho[ky,kz]
 ! Solves Poisson equation in Fourier space
 ! ft_ind >1  sin/cosine transform
 ! ft_mod=0,1   periodic fft
 do iz=1,n3_loc
  iz1=iz+imodz*n3_loc
  do iy=1,n2_loc
   iy1=iy+imody*n2_loc
   ak2p=skz(iz1,ft_ind)*skz(iz1,ft_ind)+sky(iy1,ft_ind)*sky(iy1,ft_ind)
   if(ak2p >0.0)then
    do ix=1,nxf
     pot(ix,iy,iz)=pot(ix,iy,iz)/ak2p    !pot
    end do
   else
    pot(1:nxf,iy,iz)=0.0
   endif
  end do
 end do
 end subroutine beam_2D_potential
!==========================
 subroutine beam_potential(pot,gam2,nxf,n2_loc,n3_loc,ft_ind)
 real(dp),intent(inout) :: pot(:,:,:)
 real(dp),intent(in) :: gam2
 integer,intent(in) :: nxf,n2_loc,n3_loc,ft_ind
 real(dp) :: ak2,ak2p
 integer :: ix,iy,iy1,iz,iz1
 !_________________________________
 ! ft_ind=0,1 solves Poisson equation in Fourier space (kx/gam,ky,kz)
 ! ft_ind=2 solves Poisson equation in sin/cosine Fourier space (kx/gam,ky,kz)
 ! Laplacian(pot)=-rho =>  K^2[pot(kx,ky,kz]=rho[kx,ky,kz]
 if(n3_loc==1)then
  iz=1
  do iy=1,n2_loc
   iy1=iy+imody*n2_loc
   ak2p=sky(iy1,ft_ind)*sky(iy1,ft_ind)
   if(ak2p >0.0)then
    do ix=1,nxf
     ak2=ak2p+skx(ix,ft_ind)*skx(ix,ft_ind)/gam2
     pot(ix,iy,iz)=pot(ix,iy,iz)/ak2    !pot_b
    end do
   else
    do ix=2,nxf
     ak2=skx(ix,ft_ind)*skx(ix,ft_ind)/gam2
     pot(ix,iy,iz)=pot(ix,iy,iz)/ak2    !pot_b
    end do
   endif
  end do
 else
  do iz=1,n3_loc
   iz1=iz+imodz*n3_loc
   do iy=1,n2_loc
    iy1=iy+imody*n2_loc
    ak2p=skz(iz1,ft_ind)*skz(iz1,ft_ind)+sky(iy1,ft_ind)*sky(iy1,ft_ind)
    if(ak2p >0.0)then
     do ix=1,nxf
      ak2=ak2p+skx(ix,ft_ind)*skx(ix,ft_ind)/gam2
      pot(ix,iy,iz)=pot(ix,iy,iz)/ak2    !pot_b
     end do
    else
     do ix=2,nxf
      ak2=skx(ix,ft_ind)*skx(ix,ft_ind)/gam2
      pot(ix,iy,iz)=pot(ix,iy,iz)/ak2    !pot_b
     end do
    endif
   end do
  end do
 endif
 !=================
 end subroutine beam_potential
!===============================================
 subroutine FFT_Psolv(rho,g2,omp0,n1,n1_loc,n2,n2_loc,n3,n3_loc,&
                i1,i2,j1,j2,k1,k2,ft_mod,sym)
 real(dp),intent(inout) :: rho(:,:,:,:)
 real(dp),intent(in) :: g2,omp0
 integer,intent(in) :: n1,n1_loc,n2,n2_loc,n3,n3_loc
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ft_mod,sym
 integer :: i,ii,j,k
! ft_mod=0,1 for standard fft in periodic BC
! ft_mod=2  for sin(sym=1) cos(sym=2) transforms
! In rho(1) enters charge density rho(x,y,z)=q*n(x,y,z)
! In rho(1) exit pot(x,y,z)
!===========================

 allocate(wb(n1,n2_loc,n3_loc))
 call mpi_ftw_alloc(n1,n2,n2_loc,n3,n3_loc)
 call ftw_init(n1,n2,n3,ft_mod)     !set wavenumber grid
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
 if(ft_mod >1)then
  call pftw3d_sc(wb,n1,n2,n2_loc,n3,n3_loc,-1,sym)
  wb(1:n1,1:n2_loc,1:n3_loc)=omp0*wb(1:n1,1:n2_loc,1:n3_loc)
 !+++++++++++++++++++++++++++
  call beam_potential(wb,g2,n1,n2_loc,n3_loc,ft_mod)
 !exit sin/cos fourier components for beam potential
  call pftw3d_sc(wb,n1,n2,n2_loc,n3,n3_loc,1,sym)
 else
  call pftw3d(wb,n1,n2,n2_loc,n3,n3_loc,-1)
  wb(1:n1,1:n2_loc,1:n3_loc)=omp0*wb(1:n1,1:n2_loc,1:n3_loc)
 !+++++++++++++++++++++++++++
  call beam_potential(wb,g2,n1,n2_loc,n3_loc,ft_mod)
 !exit fourier components for beam potential
  call pftw3d(wb,n1,n2,n2_loc,n3,n3_loc,1)
 endif

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
 !EXIT rho(1) 3D beam potential
 if(allocated(wb))deallocate(wb)
 call ftw_end
 call mpi_ftw_dalloc
 end subroutine FFT_Psolv
!===============================
 subroutine FFT_2D_Psolv(rho,omp0,n1,n1_loc,n2,n2_loc,n3,n3_loc,&
                                                i1,i2,j1,j2,k1,k2,ft_mod,sym)
 real(dp),intent(inout) :: rho(:,:,:,:)
 real(dp),intent(in) :: omp0
 integer,intent(in) :: n1,n1_loc,n2,n2_loc,n3,n3_loc
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ft_mod,sym
 integer :: i,ii,j,k

 allocate(wb(n1,n2_loc,n3_loc))
 call mpi_ftw_alloc(n1,n2,n2_loc,n3,n3_loc)
 call ftw_init(n1,n2,n3,ft_mod)
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
 if(ft_mod >1)then
                       !sin/cosine transform
  call pftw2d_sc(wb,n1,n2,n2_loc,n3,n3_loc,-1,sym)
  wb(1:n1,1:n2_loc,1:n3_loc)=omp0*wb(1:n1,1:n2_loc,1:n3_loc)
 !+++++++++++++++++++++++++++
  call beam_2D_potential(wb,n1,n2_loc,n3_loc,ft_mod)
 !exit fourier components for potential
  call pftw2d_sc(wb,n1,n2,n2_loc,n3,n3_loc,1,sym)
 else
                       !periodic fft transform
  call pftw2d(wb,n1,n2,n2_loc,n3,n3_loc,-1)
  wb(1:n1,1:n2_loc,1:n3_loc)=omp0*wb(1:n1,1:n2_loc,1:n3_loc)
 !+++++++++++++++++++++++++++
  call beam_2D_potential(wb,n1,n2_loc,n3_loc,ft_mod)
 !exit fourier components for potential
  call pftw2d(wb,n1,n2,n2_loc,n3,n3_loc,1)
 endif
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
 !EXIT rho(1) 2D beam potential
 if(allocated(wb))deallocate(wb)
 call ftw_end
 call mpi_ftw_dalloc
 end subroutine FFT_2D_Psolv
 !--------------------------
 end module psolv
 !======================================
