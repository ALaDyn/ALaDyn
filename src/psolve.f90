!*****************************************************************************************************!
!                            Copyright 2008-2020  The ALaDyn Collaboration                            !
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

 module psolve
  use pstruct_data
  use fstruct_data
  use common_param
  use grid_param
  use prl_fft
  use grid_fields

  implicit none
  private
  public :: fft_3d_psolv, fft_2d_psolv

  real(dp), allocatable :: wb(:, :, :), wa(:, :, :)
  !==========================

 contains

  subroutine beam_2d_potential(poten, nxf_in, n2_loc, n3_loc, ft_ind)
   real(dp), intent(inout) :: poten(:, :, :)
   integer, intent(in) :: nxf_in, n2_loc, n3_loc, ft_ind
   real(dp) :: ak2p
   integer :: ix, iy, iy1, iz, iz1
   !_________________________________
   ! Laplacian(y,z)(poten)=-rho =>  [k^2_y+k^2_z][poten(ky,kz)]=rho[ky,kz]
   ! Solves Poisson equation in Fourier space
   ! ft_ind >1  sin/cosine transform
   ! ft_mod=0,1   periodic fft
   do iz = 1, n3_loc
    iz1 = iz + imodz*n3_loc
    do iy = 1, n2_loc
     iy1 = iy + imody*n2_loc
     ak2p = skz(iz1, ft_ind)*skz(iz1, ft_ind) + sky(iy1, ft_ind)*sky(iy1 &
                                                                     , ft_ind)
     if (ak2p > 0.0) then
      do ix = 1, nxf_in
       poten(ix, iy, iz) = poten(ix, iy, iz)/ak2p !poten
      end do
     else
      poten(1:nxf_in, iy, iz) = 0.0
     end if
    end do
   end do
  end subroutine
  !==========================
  subroutine beam_potential(poten, gam2, nxf_in, n2_loc, n3_loc, ft_ind)
   real(dp), intent(inout) :: poten(:, :, :)
   real(dp), intent(in) :: gam2
   integer, intent(in) :: nxf_in, n2_loc, n3_loc, ft_ind
   real(dp) :: ak2, ak2p
   integer :: ix, iy, iy1, iz, iz1
   !_________________________________
   ! ft_ind=0,1 solves Poisson equation in Fourier space (kx/gam,ky,kz)
   ! ft_ind=2 solves Poisson equation in sin/cosine Fourier space (kx/gam,ky,kz)
   ! Laplacian(poten)=-rho =>  K^2[poten(kx,ky,kz]=rho[kx,ky,kz]
   if (n3_loc == 1) then
    iz = 1
    do iy = 1, n2_loc
     iy1 = iy + imody*n2_loc
     ak2p = sky(iy1, ft_ind)*sky(iy1, ft_ind)
     if (ak2p > 0.0) then
      do ix = 1, nxf_in
       ak2 = ak2p + skx(ix, ft_ind)*skx(ix, ft_ind)/gam2
       poten(ix, iy, iz) = poten(ix, iy, iz)/ak2 !pot_b
      end do
     else
      do ix = 2, nxf_in
       ak2 = skx(ix, ft_ind)*skx(ix, ft_ind)/gam2
       poten(ix, iy, iz) = poten(ix, iy, iz)/ak2 !pot_b
      end do
     end if
    end do
   else
    do iz = 1, n3_loc
     iz1 = iz + imodz*n3_loc
     do iy = 1, n2_loc
      iy1 = iy + imody*n2_loc
      ak2p = skz(iz1, ft_ind)*skz(iz1, ft_ind) + &
             sky(iy1, ft_ind)*sky(iy1, ft_ind)
      if (ak2p > 0.0) then
       do ix = 1, nxf_in
        ak2 = ak2p + skx(ix, ft_ind)*skx(ix, ft_ind)/gam2
        poten(ix, iy, iz) = poten(ix, iy, iz)/ak2 !pot_b
       end do
      else
       do ix = 2, nxf_in
        ak2 = skx(ix, ft_ind)*skx(ix, ft_ind)/gam2
        poten(ix, iy, iz) = poten(ix, iy, iz)/ak2 !pot_b
       end do
      end if
     end do
    end do
   end if
   !=================
  end subroutine
  !===============================================
  subroutine fft_3d_psolv(rho, pot1, g2, omp0, n1, n1_loc, n2, n2_loc, n3, &
                          n3_loc, i1, i2, j1, j2, k1, k2, ft_mod, sym, s_ind)
   real(dp), intent(inout) :: rho(:, :, :, :), pot1(:, :, :, :)
   real(dp), intent(in) :: g2, omp0
   integer, intent(in) :: n1, n1_loc, n2, n2_loc, n3, n3_loc, ft_mod, sym, s_ind
   integer, intent(in) :: i1, i2, j1, j2, k1, k2
   integer :: i, ii, j, k
   ! ft_mod=0,1 for standard fft in periodic BC
   ! ft_mod=2  for sin(sym=1) cos(sym=2) transforms
   ! In rho(1) enters charge density rho(x,y,z)=q*n(x,y,z)
   ! In rho(1) exit pot(x,y,z)
   !===========================
   allocate (wb(n1, n2_loc, n3_loc))
   call mpi_ftw_alloc(n1, n2, n2_loc, n3, n3_loc)
   call ftw_init(n1, n2, n3, ft_mod) !set wavenumber grid
   wb = 0.0
   if (prlx) then
    do k = k1, k2
     do j = j1, j2
      aux1(1:n1) = 0.0
      do i = i1, i2
       ii = i - 2
       aux1(ii) = rho(i, j, k, 1)
      end do
      call all_gather_dpreal(aux1, aux2, 3, n1_loc)
      do i = 1, n1
       wb(i, j - 2, k - 2) = aux2(i)
      end do
     end do
    end do
   else
    wb(1:n1, 1:n2_loc, 1:n3_loc) = rho(i1:i2, j1:j2, k1:k2, 1)
   end if
   if (ft_mod > 1) then
    call pftw3d_sc(wb, n1, n2, n2_loc, n3, n3_loc, -1, sym)
    wb(1:n1_loc, 1:n2_loc, 1:n3_loc) = omp0*wb(1:n1_loc, 1:n2_loc, 1:n3_loc)
    !==========================
    call beam_potential(wb, g2, n1, n2_loc, n3_loc, ft_mod)
    !exit sin/cos fourier components for beam potential
    call pftw3d_sc(wb, n1, n2, n2_loc, n3, n3_loc, 1, sym)
   else
    call pftw3d(wb, n1, n2, n2_loc, n3, n3_loc, -1)
    wb(1:n1, 1:n2_loc, 1:n3_loc) = omp0*wb(1:n1, 1:n2_loc, 1:n3_loc)
    !==========================
    call beam_potential(wb, g2, n1, n2_loc, n3_loc, ft_mod)
    !exit fourier components for beam potential
    call pftw3d(wb, n1, n2, n2_loc, n3, n3_loc, 1)
   end if
   call mpi_ftw_dalloc
   if (s_ind > 0) then
    !Two new routines added
    allocate (wa(n1, 4*n2_loc, 4*n3_loc))
    if (.not. allocated(fp1)) allocate (fp1(n1, n2_loc, n3_loc))
    call mpi_yzft_ord(n2_loc, n3_loc)
    call ft_overset_grid(wb, wa, n1, n2_loc, n3_loc)    !in fft/prl_fft  module
    call unif_to_str_field_interp(wa, pot1, 1)           !put data in in fields/grid_fields module
    if (allocated(wa)) deallocate (wa)
    deallocate (fp1)
   else
    rho(i1:i2, j1:j2, k1:k2, 1) = wb(1:n1, 1:n2_loc, 1:n3_loc)
   end if
   !EXIT rho(1) 3D beam potential
   if (allocated(wb)) deallocate (wb)
   call ftw_end
  end subroutine
  !===============================
  subroutine fft_2d_psolv(rho, pot1, omp0, n1, n1_loc, n2, n2_loc, n3, n3_loc, &
                          i1, i2, j1, j2, k1, k2, ft_mod, sym, sind)
   real(dp), intent(inout) :: rho(:, :, :, :), pot1(:, :, :, :)
   real(dp), intent(in) :: omp0
   integer, intent(in) :: n1, n1_loc, n2, n2_loc, n3, n3_loc
   integer, intent(in) :: ft_mod, sym, sind
   integer, intent(in) :: i1, i2, j1, j2, k1, k2
   integer :: i, ii, j, k

   allocate (wb(n1, n2_loc, n3_loc))
   call mpi_ftw_alloc(n1, n2, n2_loc, n3, n3_loc)
   call ftw_init(n1, n2, n3, ft_mod)

   wb = 0.0
   if (prlx) then
    do k = k1, k2
     do j = j1, j2
      aux1(1:n1) = 0.0
      do i = i1, i2
       ii = i - 2
       aux1(ii) = rho(i, j, k, 1)
      end do
      call all_gather_dpreal(aux1, aux2, 3, n1_loc)
      do i = 1, n1
       wb(i, j - 2, k - 2) = aux2(i)
      end do
     end do
    end do
   else
    wb(1:n1, 1:n2_loc, 1:n3_loc) = rho(i1:i2, j1:j2, k1:k2, 1)
   end if
   if (ft_mod > 1) then
    !sin/cosine transform
    call pftw2d_sc(wb, n1, n2, n2_loc, n3, n3_loc, -1, sym)
    wb(1:n1, 1:n2_loc, 1:n3_loc) = omp0*wb(1:n1, 1:n2_loc, 1:n3_loc)
    !==========================
    call beam_2d_potential(wb, n1, n2_loc, n3_loc, ft_mod)
    !exit fourier components for potential
    call pftw2d_sc(wb, n1, n2, n2_loc, n3, n3_loc, 1, sym)
   else
    !periodic fft transform
    call pftw2d(wb, n1, n2, n2_loc, n3, n3_loc, -1)
    wb(1:n1, 1:n2_loc, 1:n3_loc) = omp0*wb(1:n1, 1:n2_loc, 1:n3_loc)
    !==========================
    call beam_2d_potential(wb, n1, n2_loc, n3_loc, ft_mod)
    !exit fourier components for potential
    call pftw2d(wb, n1, n2, n2_loc, n3, n3_loc, 1)
   end if
   call mpi_ftw_dalloc
   if (sind > 0) then
    !Two new routines added
    allocate (wa(n1, 4*n2_loc, n3_loc))
    if (.not. allocated(fp1)) allocate (fp1(n1, n2_loc, n3_loc))
    call mpi_yzft_ord(n2_loc, n3_loc)
    call ft_overset_grid(wb, wa, n1, n2_loc, n3_loc)    !in fft/prl_fft  module
    call unif_to_str_field_interp(wa, pot1, 1)           !in fields/grid_fields module
    if (allocated(wa)) deallocate (wa)
    deallocate (fp1)
   else
    rho(i1:i2, j1:j2, k1:k2, 1) = wb(1:n1, 1:n2_loc, 1:n3_loc)
   end if
   !EXIT rho(1) 2D beam potential
   if (allocated(wb)) deallocate (wb)
   call ftw_end
  end subroutine
  !==========================
 end module

