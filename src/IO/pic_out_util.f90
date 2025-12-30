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

 module pic_out_util

  use grid_part_util
  use mpi_curr_interface
  use mpi_field_interface
  use psolve
  use phys_param, only: electron_mass

  implicit none
  !=====Contains functions to prepare selected output variables=======

  interface prl_den_energy_interp
   module procedure prl_den_energy_interp_new
   module procedure prl_den_energy_interp_old
  end interface
   
 contains
  subroutine fill_density_data(den, ic)
   real(dp), intent(inout) :: den(:, :, :, :)
   integer, intent(in) :: ic
   integer :: i, j, k, iy, iz, n_loc

   n_loc = loc_ygrid(imody)%ng
   do k = kz1, kz2
    do j = jy1, jy2 - 1
     iy = j + imody*n_loc
     if (y(iy) > ymin_t .and. y(iy) < ymax_t) then
      do i = ix1, ix2 - 1
       if (x(i) >= targ_in) den(i, j, k, ic) = den(i, j, k, ic) + 1.
      end do
     end if
    end do
   end do
   if (ndim < 3) return
   n_loc = loc_zgrid(imodz)%ng
   do k = kz1, kz2 - 1
    iz = k + imodz*n_loc
    if (z(iz) > zmin_t .and. z(iz) < zmax_t) then
     do j = jy1, jy2 - 1
      do i = ix1, ix2 - 1
       den(i, j, k, ic) = den(i, j, k, ic) + 1.
      end do
     end do
    end if
   end do
  end subroutine
  !============================
  subroutine prl_den_energy_interp_new(spec_in, ic, cmp_out, mempool)
   type(species_new), intent(in) :: spec_in
   integer, intent (in) :: ic, cmp_out
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp) :: dery, derz, ar, ai
   integer :: np, i, j, k, jj, kk

   !=============================
   ! nden=1 only charge density
   ! nden =2 charge and energy <n(gamma-1)> density
   !=============================
   do i = 1, cmp_out
    jc(:, :, :, i) = 0.0
   end do
   !===========================
   np = loc_npart(imody, imodz, imodx, ic)
   if(part)then
    select case (cmp_out)
    case (1)
     call set_grid_charge(spec_in, jc, np, 1, mempool)
     !nden=1 exit density for each ic species
    case default
     !nden=2 exit density and energy density for each species
     if (envelope) then
      if(ic==1)then
       do k = kz1, kz2
        do j = jy1, jy2
         do i = ix1, ix2
          ar = .5*(env(i,j,k,1)+env(i,j,k,3))
          ai = .5*(env(i,j,k,2)+env(i,j,k,4))
          jc(i, j, k, 3) = 0.5*(ar*ar+ai*ai)
         end do
        end do
       end do
       if (prl) call fill_ebfield_yzxbdsdata(jc, 3, 3, 2, 2)
       call set_grid_env_den_energy(spec_in, jc, np, 3, mempool)
      else
       call set_grid_den_energy(spec_in, jc, np, mempool) !ic >1 in envelope scheme
      end if
     else
      call set_grid_den_energy(spec_in, jc, np, mempool)
     ! in jc(1) is plasma norm density in jc(2) <(gam-1)density> using kinetic
     ! gamma  for each species
     end if
    end select
    if (prl) call fill_curr_yzxbdsdata(jc,cmp_out)
    do kk = 1, cmp_out
     call den_zyxbd(jc, kk)
    end do
    if (ic==1) jc(:, :, :, 1) = -jc(:, :, :, 1)
    if (cmp_out==2) jc(:, :, :, 2) = mass(ic)*electron_mass* &
     jc(:, :, :, 2)
   !=========== energy density in Mev*n/n_0

    if (stretch) then
     select case (ndim)
     case (2)
     k = 1
     do j = jy1, jy2
      jj = j - gcy + 1
      dery = loc_yg(jj, 3, imody)
      do i = ix1, ix2
       jc(i, j, k, 1:cmp_out) = dery*jc(i, j, k, 1:cmp_out)
      end do
     end do
     case (3)
     do k = kz1, kz2
      kk = k - gcz + 1
      derz = loc_zg(kk, 3, imodz)
      do j = jy1, jy2
       jj = j - gcy + 1
       dery = loc_yg(jj, 3, imody)*derz
       do i = ix1, ix2
        jc(i, j, k, 1:cmp_out) = dery*jc(i, j, k, 1:cmp_out)
       end do
      end do
     end do
     end select
    end if
   end if
   !======================
  end subroutine
  !============================
  subroutine prl_den_energy_interp_old(spec_in, spec_aux_in, ic, cmp_out)
   type(species), intent(in) :: spec_in
   real(dp), dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: ic, cmp_out
   real (dp) :: dery, derz, ar, ai
   integer :: np, i, j, k, jj, kk

   !=============================
   ! nden=1 only charge density
   ! nden =2 charge and energy <n(gamma-1)> density
   !=============================
   do i = 1, cmp_out
    jc(:, :, :, i) = 0.0
   end do
   !===========================
   np = loc_npart(imody, imodz, imodx, ic)
   if (part) then
    select case (cmp_out)
    case (1)
     call set_grid_charge(spec_in, spec_aux_in, jc, np, 1)
     !nden=1 exit density for each ic species
    case (2)
     !nden=2 exit density and energy density for each species
     if (envelope) then
      if (ic == 1) then
       do k = kz1, kz2
        do j = jy1, jy2
         do i = ix1, ix2
          ar = .5*(env(i, j, k, 1) + env(i, j, k, 3))
          ai = .5*(env(i, j, k, 2) + env(i, j, k, 4))
          jc(i, j, k, 3) = 0.5*(ar*ar + ai*ai)
         end do
        end do
       end do
       if (prl) call fill_ebfield_yzxbdsdata(jc, 3, 3, 2, 2)
       call set_grid_env_den_energy(spec_in, spec_aux_in, jc, np, 3)
      else
       call set_grid_den_energy(spec_in, spec_aux_in, jc, np) !ic >1 in envelope scheme
      end if
     else
      call set_grid_den_energy(spec_in, spec_aux_in, jc, np)
     ! in jc(1) is plasma norm density in jc(2) <(gam-1)density> using kinetic
     ! gamma  for each species
     end if
    end select
    if (prl) call fill_curr_yzxbdsdata(jc, cmp_out)
    do kk = 1, cmp_out
     call den_zyxbd(jc, kk)
    end do
    if (ic == 1) jc(:, :, :, 1) = -jc(:, :, :, 1)
    if (cmp_out == 2) jc(:, :, :, 2) = mass(ic)*electron_mass* &
                                       jc(:, :, :, 2)
    !=========== energy density in Mev*n/n_0

    if (stretch) then
     select case (ndim)
     case (2)
      k = 1
      do j = jy1, jy2
       jj = j - gcy + 1
       dery = loc_yg(jj, 3, imody)
       do i = ix1, ix2
        jc(i, j, k, 1:cmp_out) = dery*jc(i, j, k, 1:cmp_out)
       end do
      end do
     case (3)
      do k = kz1, kz2
      kk = k - gcz + 1
       derz = loc_zg(kk, 3, imodz)
       do j = jy1, jy2
        jj = j - gcy + 1
        dery = loc_yg(jj, 3, imody)*derz
        do i = ix1, ix2
         jc(i, j, k, 1:cmp_out) = dery*jc(i, j, k, 1:cmp_out)
        end do
       end do
      end do
     end select
    end if
   end if
   !======================
  end subroutine
  !=====================
  subroutine set_wake_potential( spec_in, spec_aux_in )

   type(species), dimension(:), intent(in) :: spec_in
   real(dp), dimension(:, :), intent(inout) :: spec_aux_in
   integer :: np, ic, ft_mod, ft_sym
   integer :: i1, i2, j1, j2, k1, k2

   jc(:, :, :, 1:2) = 0.0
   !curr_clean
   do ic = 1, nsp
    np = loc_npart(imody, imodz, imodx, ic)
    if (np>0) call set_grid_charge_and_jx(spec_in(ic), spec_aux_in, jc, np)
   end do
   !========= jc(1)=charge density jc(2)= Jx at the same current t^{n} time
   if (prl) then
    call fill_curr_yzxbdsdata(jc, 2)
   end if
   if (nsp == 1) then
    call fill_density_data(jc, 1)
   else
    if (dmodel_id == 3) call fill_density_data(jc, 1)
   end if
   jc(ix1:ix2, jy1:jy2, kz1:kz2, 1) = jc(ix1:ix2, jy1:jy2, kz1:kz2, 1) - &
                                      jc(ix1:ix2, jy1:jy2, kz1:kz2, 2)
   !============== jc(1)=rho-Jx=======================

   ft_mod = 2 !for cosine transform
   ft_sym = 2
   call fft_2d_psolv(jc, jc, ompe, nx, nx_loc, ny, ny_loc, nz, nz_loc, &
                     i1, i2, j1, j2, k1, k2, ft_mod, ft_sym, 0)

   !==================================
  end subroutine
  !============================
 end module
