!*****************************************************************************************************!
!                            Copyright 2008-2019  The ALaDyn Collaboration                            !
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

 module init_part_distrib

  use common_param
  use util
  use grid_param
  use stretched_grid
  use array_alloc
  use mpi_var
  use code_util, only: maxv, mem_psize
  use phys_param, only: pi

  implicit none
  private
  public :: part_distribute

  integer, allocatable :: loc_imax(:, :), loc_jmax(:, :), loc_kmax(:, :)

  interface pspecies_distribute
   module procedure old_pspecies_distribute
   module procedure new_pspecies_distribute
  end interface

  interface multi_layer_gas_target
   module procedure multi_layer_gas_target_new
   module procedure multi_layer_gas_target_old
  end interface

  interface preplasma_multisp
   module procedure preplasma_multisp_new
   module procedure preplasma_multisp_old
  end interface

  interface multi_layer_twosp_target
   module procedure multi_layer_twosp_target_new
   module procedure multi_layer_twosp_target_old
  end interface

  interface multi_layer_threesp_target
   module procedure multi_layer_threesp_target_new
   module procedure multi_layer_threesp_target_old
  end interface

  interface one_layer_nano_wires
   module procedure one_layer_nano_wires_new
   module procedure one_layer_nano_wires_old
  end interface

  interface one_layer_nano_tubes
   module procedure one_layer_nano_tubes_new
   module procedure one_layer_nano_tubes_old
  end interface

  interface part_distribute
   module procedure part_distribute_new
   module procedure part_distribute_old
  end interface
 contains

  subroutine set_pgrid_xind(npx, ic)
   integer, intent(in) :: npx, ic
   integer :: i, p, ip
   real(dp) :: xp, x1, x2

   !Defines the the number of particles on each mpi x-domain
   ! Enter the total particle number npx and the particle species ic
   ! Particle x-distribution is already defined by xpt(nptx,ic)
   ! Exit loc_imax(pex,ic) for each pex task
   !=================================================
   loc_imax(0:npe_xloc - 1, ic) = 1
   p = 0
   ip = 0
   x1 = loc_xgrid(0)%gmin
   x2 = loc_xgrid(0)%gmax
   do i = 1, npx
    xp = xpt(i, ic)
    if (xp >= x1 .and. xp < x2) ip = ip + 1
   end do
   loc_imax(p, ic) = ip !number of grid points in [loc_xmin,loc_xmax]
   if (npe_xloc > 1) then
    do p = 1, npe_xloc - 1
     x1 = loc_xgrid(p)%gmin
     x2 = loc_xgrid(p)%gmax
     ip = 0
     do i = 1, npx
      xp = xpt(i, ic)
      if (xp >= x1 .and. xp < x2) ip = ip + 1
     end do
     loc_imax(p, ic) = ip
    end do
   end if
  end subroutine

  subroutine set_pgrid_ind(npy, npz, ic)
   integer, intent(in) :: npy, npz, ic
   integer :: i, p, ip
   real(dp) :: yp, y1, y2

   ! Particles number index on each y-z mpi domain
   ! Enter the total particle number npy, npz and the particle species ic
   ! Particle y-z-distributions are ypt(npy,ic), zpt(npz,ic)
   ! Exit loc_jmax(pey,ic) loc_kmax(pez,ic)for each pey peztask
   !=======================================
   loc_jmax(0:npe_yloc - 1, ic) = 1
   loc_kmax(0:npe_zloc - 1, ic) = 1

   p = 0
   ip = 0
   y1 = max(ymin_t, loc_ygrid(p)%gmin)
   y2 = min(loc_ygrid(p)%gmax, ymax_t)
   do i = 1, npy
    yp = ypt(i, ic)
    if (yp >= y1 .and. yp < y2) ip = ip + 1
   end do
   loc_jmax(p, ic) = ip !number of particle y-positions in [loc_ymin,loc_ymax]
   if (npe_yloc > 2) then
    do p = 1, npe_yloc - 2
     ip = 0
     y1 = max(ymin_t, loc_ygrid(p)%gmin)
     y2 = min(loc_ygrid(p)%gmax, ymax_t)
     do i = 1, npy
      yp = ypt(i, ic)
      if (yp >= y1 .and. yp < y2) ip = ip + 1
     end do
     loc_jmax(p, ic) = ip
    end do
   end if
   p = npe_yloc - 1
   ip = 0
   y1 = max(ymin_t, loc_ygrid(p)%gmin)
   y2 = min(loc_ygrid(p)%gmax, ymax_t)
   do i = 1, npy
    yp = ypt(i, ic)
    if (yp >= y1 .and. yp < y2) ip = ip + 1
   end do
   loc_jmax(p, ic) = ip
   if (npz == 1) return

   p = 0
   ip = 0
   y1 = max(zmin_t, loc_zgrid(p)%gmin)
   y2 = min(loc_zgrid(p)%gmax, zmax_t)
   do i = 1, npz
    yp = zpt(i, ic)
    if (yp >= y1 .and. yp < y2) ip = ip + 1
   end do
   loc_kmax(p, ic) = ip
   if (npe_zloc > 2) then
    do p = 1, npe_zloc - 2
     ip = 0
     y1 = max(zmin_t, loc_zgrid(p)%gmin)
     y2 = min(loc_zgrid(p)%gmax, zmax_t)
     do i = 1, npz
      yp = zpt(i, ic)
      if (yp >= y1 .and. yp < y2) ip = ip + 1
     end do
     loc_kmax(p, ic) = ip
    end do
   end if
   p = npe_zloc - 1
   ip = 0
   y1 = max(zmin_t, loc_zgrid(p)%gmin)
   y2 = min(loc_zgrid(p)%gmax, zmax_t)
   do i = 1, npz
    yp = zpt(i, ic)
    if (yp >= y1 .and. yp < y2) ip = ip + 1
   end do
   loc_kmax(p, ic) = ip
  end subroutine

  !--------------------------
  subroutine new_pspecies_distribute(loc_sp, t_x, ch, p, ic, i2, q)
   type (species_new), intent (inout) :: loc_sp
   real (dp), intent (in) :: t_x, ch
   integer, intent (in) :: ic, i2
   integer, intent(inout) :: p, q
   integer :: j2, k2, n_parts

   q = p
   call init_random_seed(mype)
   k2 = loc_nptz(ic)
   j2 = loc_npty(ic)
   n_parts = i2*j2*k2
   !index = index_array(n_parts)
   call loc_sp%set_temperature(t_x)
   call loc_sp%set_charge(ch)
   call loc_sp%initialize_data(loc_xpt(:, ic), loc_ypt(:, ic), loc_zpt(:, ic), &
   loc_wghx(:, ic), loc_wghyz(:, :, ic), i2, j2, k2)
   q = q + n_parts
  end subroutine

  subroutine old_pspecies_distribute(loc_sp, t_x, ch, q, ic, i2, p)
   type (species), intent (inout) :: loc_sp
   real (dp), intent (in) :: t_x, ch
   integer, intent (in) :: q, ic, i2
   integer, intent (inout) :: p
   integer :: i, j, k, j2, k2
   real(dp) :: u, whz

   call init_random_seed(mype)
   p = q
   charge = int(ch, hp_int)
   part_ind = 0
   k2 = loc_nptz(ic)
   j2 = loc_npty(ic)
   if (curr_ndim > 2) then
    do k = 1, k2
     do j = 1, j2
      do i = 1, i2
       whz = loc_wghx(i, ic)*loc_wghyz(j, k, ic)
       wgh = real(whz, sp)
       p = p + 1
       loc_sp%part(p, 1) = loc_xpt(i, ic)
       loc_sp%part(p, 2) = loc_ypt(j, ic)
       loc_sp%part(p, 3) = loc_zpt(k, ic)
       call gasdev(u)
       loc_sp%part(p, 4) = t_x*u
       call gasdev(u)
       loc_sp%part(p, 5) = t_x*u
       call gasdev(u)
       loc_sp%part(p, 6) = t_x*u
       loc_sp%part(p, 7) = wgh_cmp
      end do
     end do
    end do
    return
   end if

   do j = 1, j2
    do i = 1, i2
     whz = loc_wghx(i, ic)*loc_wghyz(j, 1, ic)
     wgh = real(whz, sp)
     p = p + 1
     loc_sp%part(p, 1) = loc_xpt(i, ic)
     loc_sp%part(p, 2) = loc_ypt(j, ic)
     call gasdev(u)
     loc_sp%part(p, 3) = t_x*u
     call gasdev(u)
     loc_sp%part(p, 4) = t_x*u
     loc_sp%part(p, 5) = wgh_cmp
    end do
   end do
  end subroutine
  !==============================
  subroutine mpi_x_part_distrib(nc)
   !Local to imodx distribution
   integer, intent(in) :: nc
   integer :: ic, i2, ix_min
   real(dp) :: x1

   x1 = loc_xgrid(imodx)%gmin
   do ic = 1, nc
    ix_min = 0
    do i2 = 1, nptx(ic) !the total particle number
     if (xpt(i2, ic) < x1) ix_min = i2
    end do
    do i2 = 1, loc_nptx(ic) !the local particle number
     ix_min = ix_min + 1
     loc_xpt(i2, ic) = xpt(ix_min, ic)
     loc_wghx(i2, ic) = wghpt(ix_min, ic)
    end do
   end do
  end subroutine
  !=====================================
  subroutine mpi_yz_part_distrib(nc, ky2_in, kz2_in, nyc, nzc, ymt, zmt, whyz)

   integer, intent(in) :: nc, ky2_in(:), kz2_in(:), nyc(:), nzc(:)
   real(dp), intent(in) :: ymt, zmt, whyz(:, :, :)
   integer :: ic, i2, k1, j1, j2
   real(dp) :: loc_ym, loc_zm

   if (ndim < 3) then
    loc_ym = loc_ygrid(imody)%gmin
    if (imody == 0) loc_ym = ymt
    do ic = 1, nc
     k1 = 0
     do i2 = 1, nyc(ic)
      if (ypt(i2, ic) < loc_ym) k1 = i2
     end do
     do i2 = 1, ky2_in(ic)
      k1 = k1 + 1
      loc_ypt(i2, ic) = ypt(k1, ic)
      loc_wghyz(i2, 1, ic) = whyz(k1, 1, ic)
     end do
    end do
    zpt(1, 1:nc) = 0.0
    loc_zpt(1, 1:nc) = zpt(1, 1:nc)
    return
   end if
   !==========================
   loc_zm = loc_zgrid(imodz)%gmin
   if (imodz == 0) loc_zm = zmt
   loc_ym = loc_ygrid(imody)%gmin
   if (imody == 0) loc_ym = ymt

   do ic = 1, nc
    k1 = 0
    do i2 = 1, nzc(ic)
     if (zpt(i2, ic) < loc_zm) k1 = i2
    end do
    do i2 = 1, kz2_in(ic)
     k1 = k1 + 1
     loc_zpt(i2, ic) = zpt(k1, ic)
     j1 = 0
     do j2 = 1, nyc(ic)
      if (ypt(j2, ic) < loc_ym) j1 = j2
     end do
     do j2 = 1, ky2_in(ic)
      j1 = j1 + 1
      loc_ypt(j2, ic) = ypt(j1, ic)
      loc_wghyz(j2, i2, ic) = whyz(j1, k1, ic)
     end do
    end do
   end do
  end subroutine
  !===========================================
  subroutine set_uniform_yz_distrib(nyh_in, nc)

   integer, intent(in) :: nyh_in, nc
   integer :: j, i, i1, i2, ic
   integer :: npyc(6), npzc(6), npty_ne, nptz_ne
   real(dp) :: yy, zz, dxip, dpy, dpz
   real(dp) :: zp_min, zp_max, yp_min, yp_max
   integer :: nyl1, nzl1
   real(dp), allocatable :: wy(:, :), wz(:, :), wyz(:, :, :)
   !=================
   !========= gridding the transverse target size
   nyl1 = 1 + ny/2 - nyh_in/2 !=1 if nyh_in=ny
   nzl1 = 1 + nz/2 - nyh_in/2 !=1 if nyh_in=nz
   yp_min = ymin_t
   yp_max = ymax_t
   !=============================
   ! Multispecies
   !=============================
   do ic = 1, nc
    npyc(ic) = nyh_in*np_per_yc(ic)
   end do
   npty = maxval(npyc(1:nc))
   nptz = 1
   zp_min = zero_dp
   zp_max = zero_dp
   if (ndim == 3) then
    npzc(1:nc) = nyh_in*np_per_zc(1:nc)
    zp_min = zmin_t !-Lz
    zp_max = zmax_t !+Lz
    nptz = maxval(npzc(1:nc))
   end if
   allocate (ypt(npty, nc))
   allocate (zpt(nptz, nc))
   allocate (wy(npty, nc))
   allocate (wz(nptz, nc))
   allocate (wyz(npty, nptz, nc))
   ypt = 0.
   zpt = 0.
   wyz = 1.
   wy = 1.
   wz = 1.
   !==================
   allocate (loc_jmax(0:npe_yloc - 1, 1:nc))
   allocate (loc_kmax(0:npe_zloc - 1, 1:nc))
   allocate (loc_imax(0:npe_xloc - 1, 1:nc))
   !====================
   ! Uniform layers along y-z coordinates
   !===============
   do ic = 1, nc
    npty_ne = npyc(ic)
    if (npty_ne > 0) then
     dpy = (yp_max - yp_min)/real(npty_ne, dp)
     do i = 1, npty_ne
      ypt(i, ic) = yp_min + dpy*(real(i, dp) - 0.5)
     end do
     if (stretch) then
      yy = str_ygrid%smin
      if (yy > yp_min) then
       dpy = dyi/real(np_per_yc(ic), dp)
       i1 = (str_ygrid%sind(1) - nyl1 + 1)*np_per_yc(ic)
       i2 = npty_ne - i1
       do i = 1, i1
        dxip = dpy*(real(i - i1, dp) - 0.5)
        ypt(i, ic) = str_ygrid%smin + l_s*tan(dxip)
        wy(i, ic) = 1./(cos(dxip)*cos(dxip))
       end do
       dxip = dy/real(np_per_yc(ic), dp)
       do i = i1 + 1, i2
        ypt(i, ic) = str_ygrid%smin + dxip*(real(i - i1, dp) - 0.5)
       end do
       do i = i2 + 1, npty_ne
        dxip = dpy*(real(i - i2, dp) - 0.5)
        ypt(i, ic) = str_ygrid%smax + l_s*tan(dxip)
        wy(i, ic) = 1./(cos(dxip)*cos(dxip))
       end do
      end if
     end if
     do i = 1, npty_ne
      wyz(i, 1, ic) = wy(i, ic)*wz(1, ic)
     end do
    end if ! end np_per_yc >0
    nptz_ne = 1
    if (ndim == 3) then
     nptz_ne = npzc(ic)
     if (nptz_ne > 0) then
      dpz = (zp_max - zp_min)/real(nptz_ne, dp)
      do i = 1, nptz_ne
       zpt(i, ic) = zp_min + dpz*(real(i, dp) - 0.5)
      end do
      if (stretch) then
       zz = str_zgrid%smin
       if (zz > zp_min) then
        dpz = dzi/real(np_per_zc(ic), dp)
        i1 = (str_zgrid%sind(1) - nzl1 + 1)*np_per_zc(ic)
        i2 = nptz_ne - i1
        do i = 1, i1
         dxip = dpy*(real(i - i1, dp) - 0.5)
         zpt(i, ic) = str_zgrid%smin + l_s*tan(dxip)
         wz(i, ic) = 1./(cos(dxip)*cos(dxip))
        end do
        dxip = dz/real(np_per_zc(ic), dp)
        do i = i1 + 1, i2
         zpt(i, ic) = str_zgrid%smin + dxip*(real(i - i1, dp) - 0.5)
        end do
        do i = i2 + 1, nptz_ne
         dxip = dpy*(real(i - i2, dp) - 0.5)
         zpt(i, ic) = str_zgrid%smax + l_s*tan(dxip)
         wz(i, ic) = 1./(cos(dxip)*cos(dxip))
        end do
       end if
      end if
     end if
     do i = 1, nptz_ne
      do j = 1, npty_ne
       wyz(j, i, ic) = wy(j, ic)*wz(i, ic)
      end do
     end do
    end if !end ndim=3
    if (chann_fact > 0.0) then
     do i = 1, nptz_ne
      zz = zpt(i, ic)
      do j = 1, npty_ne
       yy = ypt(j, ic)
       wyz(j, i, ic) = 1.+chann_fact*(yy*yy + zz*zz)/(w0_y*w0_y)
      end do
     end do
    end if
    call set_pgrid_ind(npty_ne, nptz_ne, ic) !exit loc_jmax,loc_kmax
   end do
   !===========================
   loc_npty(1:nc) = loc_jmax(imody, 1:nc)
   loc_nptz(1:nc) = loc_kmax(imodz, 1:nc)
   !=============================
   npty_ne = 1
   nptz_ne = 1
   npty_ne = maxval(loc_npty(1:nc))
   nptz_ne = maxval(loc_nptz(1:nc))
   !======================
   allocate (loc_wghyz(npty_ne, nptz_ne, nc))
   allocate (loc_ypt(npty_ne, nc))
   allocate (loc_zpt(nptz_ne, nc))
   loc_wghyz = 1.
   call mpi_yz_part_distrib(nc, loc_npty, loc_nptz, npyc, npzc, ymin_t, &
                            zmin_t, wyz)

   !=EXIT local to mpi (imody,imodz) tasks (loc_ypt,loc_zpt), weights (loc_wghyz)
   ! => set in common in pstruct_data.f90 file/
   ! and particle numbers (loc_npty,loc_nptz)
   ! => set in common in grid_and_partices.f90 file/
   !=====================================
   !==================
  end subroutine
  !===========================================
  subroutine set_decreasing_yz_distrib(nyh_in, nc)
   !! Defines a decreasing transverse macroparticle distribution
   !! using the exponential function.
   !! Starting from the center and considering the distribution symmetric,
   !! the final distribution is constant for \Delta cells,
   !! then falls exponentially with a exp(-y/L) law to 1 ppc,
   !! where L is \Delta/3
   integer, intent(in) :: nyh_in, nc
   integer :: j, i, ic, delta, ip, kk
   integer :: npty_ne, nptz_ne
   integer, allocatable, dimension(:, :) :: part_per_cell_y, part_per_cell_z
   integer, allocatable, dimension(:) :: total_particles_y, total_particles_z
   integer, allocatable, dimension(:) :: stretched_particles_y, stretched_particles_z
   integer, allocatable, dimension(:) :: npyc, npzc
   real(dp) :: yy, zz, dpy, dpz, disp, yold, zold, LL, ycell, zcell, temp
   real(dp) :: zp_min, zp_max, yp_min, yp_max, ypart_rat, zpart_rat
   integer :: nyl1, nzl1, in_idx, head, tail, min_part_y, min_part_z
   real(dp), allocatable :: wy(:, :), wz(:, :), wyz(:, :, :)
   type(str_params) :: y_grid, z_grid
   !========================
   !Initializing grid params
   !========================
   ! For details on how the stretched grid inversion works,
   ! look into stretched_grid.f90
 
   if (stretch) then
    y_grid%const = one_dp*ny*dy/2
    y_grid%smin = str_ygrid%smin
    y_grid%smax = str_ygrid%smax
    y_grid%xs = ny_stretch*dy
    y_grid%dl_inv = dy_inv
    y_grid%dli_inv = dyi_inv
    y_grid%ratio = sy_rat
    y_grid%nl_stretch = ny_stretch
    y_grid%init_cell = loc_ygrid(imody)%min_cell
   end if
   if (stretch .and. ndim > 2) then
    z_grid%const = one_dp*nz*dz/2
    z_grid%smin = str_zgrid%smin
    z_grid%smax = str_zgrid%smax
    z_grid%xs = nz_stretch*dz
    z_grid%dl_inv = dz_inv
    z_grid%dli_inv = dzi_inv
    z_grid%ratio = sz_rat
    z_grid%nl_stretch = nz_stretch
    z_grid%init_cell = loc_zgrid(imodz)%min_cell
   end if
   !========= gridding the transverse target size
   nyl1 = 1 + ny/2 - nyh_in/2 !=1 if nyh_in=ny
   nzl1 = 1 + nz/2 - nyh_in/2 !=1 if nyh_in=nz
   yp_min = ymin_t
   yp_max = ymax_t
   allocate(part_per_cell_y(ny/2, nc), source = zero)
   allocate(total_particles_y(nc))
   allocate(stretched_particles_y(nc), source = zero)
   allocate(npyc(nc))
   allocate(part_per_cell_z(nz/2, nc), source = zero)
   allocate(total_particles_z(nc))
   allocate(stretched_particles_z(nc), source = zero)
   allocate(npzc(nc))
   !=============================
   ! Multispecies
   !=============================

   do ic = 1, nc
    if (np_per_yc(ic) == 1) then
     delta = 0
     min_part_y = 1
    else
     delta = (nyh_in/2)/3
     ! delta defines the number of cells in which the macroparticle number is
     ! decreased
     min_part_y = 1
    end if
    LL = 4.*delta/5*1/LOG(real(np_per_yc(ic) + min_part_y, dp))
    ! LL is the exponential characteristic length
    do i = 1, nyh_in/2
     kk = int(ABS(ny/2 - (i + nyl1) + 1) + 1)
     if (kk - (nyh_in/2 - delta) < 0) then
      part_per_cell_y(kk, ic) = np_per_yc(ic)
     else
      part_per_cell_y(kk, ic) = &
      int(EXP(-real(kk - (nyh_in/2 - delta), dp)/LL)*(np_per_yc(ic) - min_part_y) + min_part_y)
     end if
    end do
   end do
   do ic = 1, nc
    total_particles_y(ic) = 2*SUM(part_per_cell_y(:, ic), DIM=1)
    if (stretch) then
     in_idx = ny/2 - str_ygrid%sind(1) + 1
     stretched_particles_y(ic) = SUM(part_per_cell_y(in_idx:ny/2, ic), DIM=1)
    end if
   end do
   if (ndim == 3) then
    do ic = 1, nc
     if (np_per_zc(ic) == 1) then
      delta = 0
      min_part_y = 1
     else
      delta = (nyh_in/2)/3
      ! delta defines the number of cells in which the macroparticle number is
      ! decreased
      min_part_z = 1
     end if
     LL = 4.*delta/5*1/LOG(real(np_per_yc(ic) + min_part_y, dp))
     ! LL is the exponential characteristic length
     do i = 1, nyh_in/2
      kk = int(ABS(nz/2 - (i + nzl1) + 1) + 1)
      if (kk - (nyh_in/2 - delta) < 0) then
       part_per_cell_z(kk, ic) = np_per_zc(ic)
      else
       part_per_cell_z(kk, ic) = &
       int(EXP(-real(kk - (nyh_in/2 - delta), dp)/LL)*(np_per_zc(ic) - min_part_z) + min_part_z)
      end if
     end do
    end do
   end if
   
   do ic = 1, nc
    total_particles_z(ic) = 2*SUM(part_per_cell_z(:, ic), DIM=1)
    if (stretch .and. ndim > 2) then
     in_idx = nz/2 - str_zgrid%sind(1) + 1
     stretched_particles_z(ic) = SUM(part_per_cell_z(in_idx:nz/2, ic), DIM=1)
    end if
   end do

   npyc(1:nc) = total_particles_y(1:nc)
   npzc(1:nc) = 1
   npty_ne = total_particles_y(1)
   nptz_ne = total_particles_z(1)
   npty = maxval(npyc(1:nc))
   nptz = 1
   
   zp_min = zero_dp
   zp_max = zero_dp
   if (ndim == 3) then
    zp_min = zmin_t !-Lz
    zp_max = zmax_t !+Lz
    npzc(1:nc) = total_particles_z(1:nc)
    nptz = maxval(npzc(1:nc))
   end if
   allocate (ypt(npty, nc), source=zero_dp)
   allocate (zpt(nptz, nc), source=zero_dp)
   allocate (wy(npty, nc), source=one_dp)
   allocate (wz(nptz, nc), source=one_dp)
   allocate (wyz(npty, nptz, nc), source=one_dp)
   !==================
   allocate (loc_jmax(0:npe_yloc - 1, 1:nc))
   allocate (loc_kmax(0:npe_zloc - 1, 1:nc))
   allocate (loc_imax(0:npe_xloc - 1, 1:nc))
   !====================
   ! Uniform layers along y-z coordinates
   !===============
   ! Y direction
   if (stretch) then
    do ic = 1, nc
     if (total_particles_y(ic) <= 0) cycle
     !=============================
     ! Stretched zone
     !=============================
     ! Initializing particles in reverse order to ensure symmetry and
     ! a well defined starting point
     ip = 0
     i = str_ygrid%sind(1)
     dpy = dy/dy1h(i - 1)
     kk = int(ABS(i - ny/2 - 0.5))
     dpy = dpy/part_per_cell_y(kk, ic)
     disp = dpy*0.5
     do i = str_ygrid%sind(1), 1, -1
      kk = int(ABS(i - ny/2 - 0.5) + 1)
      if ( part_per_cell_y(kk, ic) <= 0 ) cycle
      dpy = dy/dy1(i)
      dpy = dpy/part_per_cell_y(kk, ic)
      ypart_rat = real(np_per_yc(ic), dp)/part_per_cell_y(kk, ic)
      do j = 1, part_per_cell_y(kk, ic)
       disp = disp - dpy
       ip = ip + 1
       ypt(ip, ic) = str_ygrid%smin + disp
       ycell = invert_stretched_grid(ypt(ip, ic), y_grid)
       ycell = dyi*(ycell - str_ygrid%sind(1))
       wy(ip, ic) = ypart_rat/(cos(ycell)*cos(ycell))
      end do
     end do
     !=============================
     ! Non stretched zone
     !=============================
     i = str_ygrid%sind(1)
     dpy = dy/dy1(i - 1)
     kk = int(ABS(i - ny/2 - 0.5))
     dpy = dpy/part_per_cell_y(kk, ic)
     do i = str_ygrid%sind(1) + 1, str_ygrid%sind(2) - 1
      kk = int(ABS(i - ny/2 - 0.5) + 1)
      if (part_per_cell_y(kk, ic) <= 0 ) cycle 
      yold = y(i) - 0.5*dpy
      dpy = dy/dy1(i)
      dpy = dpy/part_per_cell_y(kk, ic)
      do j = 1, part_per_cell_y(kk, ic)
       ip = ip + 1
       ypt(ip, ic) = yold + dpy
       yold = ypt(ip, ic)
       wy(ip, ic) = real(np_per_yc(ic), dp)/part_per_cell_y(kk, ic)
      end do
     end do
     !=============================
     ! Stretched zone
     !=============================
     ! i is kept from the end of the previous cycle
     dpy = dy/dy1h(i - 1)
     kk = int(ABS(i - ny/2 - 0.5))
     dpy = dpy/part_per_cell_y(kk, ic)
     disp = -dpy*0.5
     do i = str_ygrid%sind(2), ny
      kk = int(ABS(i - ny/2 - 0.5) + 1)
      if ( part_per_cell_y(kk, ic) <= 0 ) cycle
      dpy = dy/dy1(i)
      dpy = dpy/part_per_cell_y(kk, ic)
      ypart_rat = real(np_per_yc(ic), dp)/part_per_cell_y(kk, ic)
      do j = 1, part_per_cell_y(kk, ic)
       disp = disp + dpy
       ip = ip + 1
       ypt(ip, ic) = str_ygrid%smax + disp
       ycell = invert_stretched_grid(2*SYMM_CENTER - ypt(ip, ic), y_grid)
       ycell = ny - ycell
       ycell = dyi*(ycell - str_ygrid%sind(2) + 1)
       wy(ip, ic) = ypart_rat/(cos(ycell)*cos(ycell))
      end do
     end do
     if (ip /= total_particles_y(ic)) then
      write(6, *) 'Particle counting problem in set_decreasing_yz_distrib'
     end if
     !Reversing order of the particles in the first stretched zone
     head = 1
     tail = stretched_particles_y(ic)
     do i = 1, stretched_particles_y(ic)
      if (head >= tail) exit
      temp = ypt(head, ic)
      ypt(head, ic) = ypt(tail, ic)
      ypt(tail, ic) = temp
      temp = wy(head, ic)
      wy(head, ic) = wy(tail, ic)
      wy(tail, ic) = temp
      head = head + 1
      tail = tail - 1
     end do
    end do
   else
    do ic = 1, nc
     ip = 0
     disp = -0.5*dy/part_per_cell_y(nyh_in/2, ic)
     do i = 1, ny
      ! Here we can have a cycle from 1 to ny because part_per_cell
      ! is only different from zero in the domain where there are particles
      kk = int(ABS(i - ny/2 - 0.5) + 1)
      if ( part_per_cell_y(kk, ic) <= 0 ) cycle
      dpy = dy/part_per_cell_y(kk, ic)
      do j = 1, part_per_cell_y(kk, ic)
       disp = disp + dpy
       ip = ip + 1
       ypt(ip, ic) = yp_min + disp
       wy(ip, ic) = real(np_per_yc(ic), dp)/part_per_cell_y(kk, ic)
      end do
     end do
     if (ip /= total_particles_y(ic)) then
      write(6, *) 'Particle counting problem in set_decreasing_yz_distrib'
     end if
    end do
   end if

   do ic = 1, nc
    do i = 1, npyc(ic)
     wyz(i, 1, ic) = wy(i, ic)*wz(1, ic)
    end do
   end do

   if (ndim == 3) then
    ! Z direction
    if (stretch) then
     do ic = 1, nc
      if (total_particles_z(ic) <= 0) cycle
     !=============================
     ! Stretched zone
     !=============================
     ! Initializing particles in reverse order to ensure symmetry and
     ! a well defined starting point
      ip = 0
      i = str_zgrid%sind(1)
      dpz = dz/dz1h(i - 1)
      kk = int(ABS(i - nz/2 - 0.5))
      dpz = dpz/part_per_cell_z(kk, ic)
      disp = dpz*0.5
      do i = str_zgrid%sind(1), 1, -1
       kk = int(ABS(i - nz/2 - 0.5) + 1)
       if ( part_per_cell_z(kk, ic) <= 0 ) cycle
       dpz = dz/dz1(i)
       dpz = dpz/part_per_cell_z(kk, ic)
       zpart_rat = real(np_per_zc(ic), dp)/part_per_cell_z(kk, ic)
       do j = 1, part_per_cell_z(kk, ic)
        disp = disp - dpz
        ip = ip + 1
        zpt(ip, ic) = str_zgrid%smin + disp
        zcell = invert_stretched_grid(zpt(ip, ic), z_grid)
        zcell = dzi*(zcell - str_zgrid%sind(1))
        wz(ip, ic) = zpart_rat/(cos(zcell)*cos(zcell))
       end do
      end do
     !=============================
     ! Non stretched zone
     !=============================
      i = str_zgrid%sind(1)
      dpz = dz/dz1(i - 1)
      kk = int(ABS(i - nz/2 - 0.5))
      dpz = dpz/part_per_cell_z(kk, ic)
      do i = str_zgrid%sind(1) + 1, str_zgrid%sind(2) - 1
       kk = int(ABS(i - nz/2 - 0.5) + 1)
       if (part_per_cell_z(kk, ic) <= 0 ) cycle 
       zold = z(i) - 0.5*dpz
       dpz = dz/dz1(i)
       dpz = dpz/part_per_cell_z(kk, ic)
       do j = 1, part_per_cell_z(kk, ic)
        ip = ip + 1
        zpt(ip, ic) = zold + dpz
        zold = zpt(ip, ic)
        wz(ip, ic) = real(np_per_zc(ic), dp)/part_per_cell_z(kk, ic)
       end do
      end do
     !=============================
     ! Stretched zone
     !=============================
     ! i is kept from the end of the previous cycle
      dpz = dz/dz1h(i - 1)
      kk = int(ABS(i - nz/2 - 0.5))
      dpz = dpz/part_per_cell_z(kk, ic)
      disp = -dpz*0.5
      do i = str_zgrid%sind(2), nz
       kk = int(ABS(i - nz/2 - 0.5) + 1)
       if ( part_per_cell_z(kk, ic) <= 0 ) cycle
       dpz = dz/dz1(i)
       dpz = dpz/part_per_cell_z(kk, ic)
       zpart_rat = real(np_per_zc(ic), dp)/part_per_cell_z(kk, ic)
       do j = 1, part_per_cell_z(kk, ic)
        disp = disp + dpz
        ip = ip + 1
        zpt(ip, ic) = str_zgrid%smax + disp
        zcell = invert_stretched_grid(2*SYMM_CENTER - zpt(ip, ic), z_grid)
        zcell = nz - zcell
        zcell = dzi*(zcell - str_zgrid%sind(2) + 1)
        wz(ip, ic) = zpart_rat/(cos(zcell)*cos(zcell))
       end do
      end do
      if (ip /= total_particles_z(ic)) then
       write(6, *) 'Particle counting problem in set_decreasing_yz_distrib'
      end if
     !Reversing order of the particles in the first stretched zone
      head = 1
      tail = stretched_particles_z(ic)
      do i = 1, stretched_particles_z(ic)
       if (head >= tail) exit
       temp = zpt(head, ic)
       zpt(head, ic) = zpt(tail, ic)
       zpt(tail, ic) = temp
       temp = wz(head, ic)
       wz(head, ic) = wz(tail, ic)
       wz(tail, ic) = temp
       head = head + 1
       tail = tail - 1
      end do
     end do
    else
     do ic = 1, nc
      ip = 0
      disp = -0.5*dz/part_per_cell_z(nyh_in/2, ic)
      do i = 1, nz
       kk = int(ABS(i - nz/2 - 0.5) + 1)
       if ( part_per_cell_z(kk, ic) <= 0 ) cycle
       dpz = dz/part_per_cell_z(kk, ic)
       do j = 1, part_per_cell_z(kk, ic)
        disp = disp + dpz
        ip = ip + 1
        zpt(ip, ic) = zp_min + disp
        wz(ip, ic) = real(np_per_zc(ic), dp)/part_per_cell_z(kk, ic)
       end do
      end do
      if (ip /= total_particles_z(ic)) then
       write(6, *) 'Particle counting problem in set_decreasing_yz_distrib'
      end if
     end do
    end if
    do ic = 1, nc
     do i = 1, npzc(ic)
      do j = 1, npyc(ic)
       wyz(j, i, ic) = wy(j, ic)*wz(i, ic)
      end do
     end do
    end do
   end if

   if (chann_fact > 0.0) then
    do ic = 1, nc
     do i = 1, npzc(ic)
      zz = zpt(i, ic)
      do j = 1, npyc(ic)
       yy = ypt(j, ic)
       wyz(j, i, ic) = 1.+chann_fact*(yy*yy + zz*zz)/(w0_y*w0_y)
      end do
     end do
    end do
   end if

   do ic = 1, nc
    call set_pgrid_ind(npyc(ic), npzc(ic), ic) !exit loc_jmax,loc_kmax
   end do
   !===========================
   loc_npty(1:nc) = loc_jmax(imody, 1:nc)
   loc_nptz(1:nc) = loc_kmax(imodz, 1:nc)
   !=============================
   npty_ne = 1
   nptz_ne = 1
   npty_ne = maxval(loc_npty(1:nc))
   nptz_ne = maxval(loc_nptz(1:nc))
   !======================
   allocate (loc_wghyz(npty_ne, nptz_ne, nc))
   allocate (loc_ypt(npty_ne, nc))
   allocate (loc_zpt(nptz_ne, nc))
   loc_wghyz = 1.
   call mpi_yz_part_distrib(nc, loc_npty, loc_nptz, npyc, npzc, ymin_t, &
                            zmin_t, wyz)

   !=EXIT local to mpi (imody,imodz) tasks (loc_ypt,loc_zpt), weights (loc_wghyz)
   ! => set in common in pstruct_data.f90 file/
   ! and particle numbers (loc_npty,loc_nptz)
   ! => set in common in grid_and_partices.f90 file/
   !=====================================
   !==================
  end subroutine
  !===========================================
  subroutine set_yz_distrib( nyh_in, nc )
   integer, intent(in) :: nyh_in, nc
   logical :: decreasing, many_parts_y, many_parts_z

   decreasing = decreasing_transverse
   many_parts_y = ANY(np_per_yc(1:nc) > 1)
   many_parts_z = ANY(np_per_zc(1:nc) > 1)

   if (decreasing .and. (many_parts_y .or. many_parts_z)) then
    call set_decreasing_yz_distrib( nyh_in, nc )
   else
    call set_uniform_yz_distrib( nyh_in, nc )
   end if

  end subroutine
  !===========================================
  subroutine multi_layer_gas_target_new(spec_in, spec_aux_in, layer_mod, nyh_in, xf0)

   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (in) :: layer_mod, nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, i, j, i1, i2, ic
   integer :: n_peak, npmax, nxtot, len_conc
   real(dp) :: uu, u2, xp_min, xp_max, u3, ramp_prefactor
   real(dp) :: xfsh, un(2), wgh_sp(nsp)
   real(dp), allocatable :: conc(:)
   logical, allocatable, dimension(:) :: mobilebool
   integer :: nxl(6)
   integer :: nps_loc(4), last_particle_index(4), nptx_alloc(4)
   !==========================
   p = 0
   i = 0
   j = 0
   i1 = 0
   i2 = 0
   ic = 0
   call set_yz_distrib(nyh_in, nsp)
   !==========================
   xp_min = xmin
   xp_max = xmax
   !==========================
   nxl = 0
   !=================================
   ! Array that defines if species is mobile.
   ! We should consider setting it in the input file
   allocate(mobilebool(nsp), source=.false.)
   mobilebool(1) = .true.
   !============================
   ! Parameters for particle distribution along the x-coordinate
   !============================
   ! Layers nxl(1:5) all containing the same ion species
   len_conc = size(concentration)
   allocate (conc(len_conc))
   conc(:) = concentration(:)
   xtot = 0.0
   nxtot = 0
   do i = 1, 6
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
    nxtot = nxtot + nxl(i)
   end do
   if (xf0 > 0.0) then
    targ_in = xf0
    targ_end = targ_in + xtot
   else
    targ_in = xmin
    targ_end = xtot + xf0
   end if
   xfsh = xf0
   !=============================
   loc_nptx = 0
   loc_nptx(1:nsp) = (nxl(1) + nxl(2) + nxl(3) + nxl(4) + nxl(5) + nxl(6))* &
                     np_per_xc(1:nsp)

   nptx_max = maxval(loc_nptx(1:nsp))
   allocate (xpt(nptx_max, nsp))
   allocate (wghpt(nptx_max, nsp))
   allocate (loc_xpt(nptx_max, nsp))
   !=============================
   wghpt = one_dp
   un = one_dp
   ramp_prefactor = one_dp
   !===================================
   ! WARNING for charge distribution
   !====================================
   ! wgh_sp(1:nsp) already set by initial conditions
   !=====================================================
   ! Longitudinal distribution
   nptx = 0
   !Weights for multispecies target
   !wgh_sp(1:3)=j0_norm
   !if(nsp==2)then
   !wgh_sp(2)=1./(real(mp_per_cell(2),dp))

   wgh_sp(1) = j0_norm*n0_ref*n_plasma
   do i = 2, nsp
    if (mp_per_cell(i) > 0) wgh_sp(i) = n0_ref/real(mp_per_cell(i), dp)
    wgh_sp(i) = conc(i - 1)*wgh_sp(i)
   end do
   select case (layer_mod)
    !================ first uniform layer np1=================
   case (1)
    if (nxl(1) > 0) then
     ramp_prefactor = one_dp - np1
     do ic = 1, nsp
      n_peak = nxl(1)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(1)*uu
       wghpt(i1, ic) = np1*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    !================ first CUBIC ramp np1 => 1 --linear or exponential still available but commented =================
    if (nxl(2) > 0) then
     do ic = 1, nsp
      n_peak = nxl(2)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(2)*uu
       !u2=(uu-1.)*(uu-1.)
       u2 = uu*uu
       u3 = u2*uu
       !wghpt(i1,ic)=(np1+exp(-4.5*u2)*(1.-np1))*wgh_sp(ic)
       !wghpt(i1,ic)=exp(-4.5*u2)*wgh_sp(ic)
       wghpt(i1, ic) = (-2.*ramp_prefactor*u3 + 3.*ramp_prefactor*u2 + &
                        one_dp - ramp_prefactor)*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================ Central layer=================
    if (nxl(3) > 0) then
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(3)*uu
       wghpt(i1, ic) = wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ second linear ramp =================
    if (nxl(4) > 0) then
     do ic = 1, nsp
      n_peak = nxl(4)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(4)*uu
       wghpt(i1, ic) = (1.-uu*(1.-np2))*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    if (nxl(5) > 0) then
     do ic = 1, nsp
      n_peak = nxl(5)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(5)*uu
       wghpt(i1, ic) = np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !=========================================
   case (2)
    !                 two bumps  (n1/n_c, n2/n_c) of length x_1 and x_2
    !                 n_over_nc enters as average n_over nc= (n1* x_1+n2*x_2)/(x_1+x_2)
    !                 weight j0_norm =>> j0_norm*np1 in x_1       =>> j0_norm*np2 in x_2
    !                 particle per cell uniform
    !================================================
    !================ first linear ramp to first plateau n1/n_c =================
    if (nxl(1) > 0) then
     do ic = 1, nsp
      n_peak = nxl(1)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(1)*uu
       wghpt(i1, ic) = uu*np1*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    if (nxl(2) > 0) then !first plateau
     do ic = 1, nsp
      n_peak = nxl(2)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(2)*uu
       wghpt(i1, ic) = np1*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================ np1 => np2 down-ramp =================
    if (nxl(3) > 0) then
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(3)*uu
       wghpt(i1, ic) = wgh_sp(ic)*(np1 + uu*(np2 - np1))
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ second plateau n2/n_c < n1/n_c =================
    if (nxl(4) > 0) then
     do ic = 1, nsp
      n_peak = nxl(4)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(4)*uu
       wghpt(i1, ic) = np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    if (nxl(5) > 0) then !second down-ramp n2/n_c ==> 0
     do ic = 1, nsp
      n_peak = nxl(5)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(5)*uu
       wghpt(i1, ic) = (1.-uu)*np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !=====================================
   case (3)
    !                 three layers
    !                 lpx(1)[ramp]+lpx(2)[plateau]  and lpx(4) plateu lpx(5) downramp n_ion=0 n_e=n_0
    !                 lpx(3)[plateau]  with a (A1-Z1) dopant with % density np1=n1_per_nc/n_per_nc
    !                 and electronic density n_e=n_0+Z1*n_ion  n0=n_H(+)
    !---------------
    !================================================
    !Z1 electrons are accounted for by a larger electron weight
    un(1) = 1.+ion_min(1)*np1
    un(2) = np1 !float(mp_per_cell(1))/float(mp_per_cell(ic))

    if (nxl(1) > 0) then
     do ic = 1, nsp_run
      n_peak = nxl(1)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(1)*uu
       wghpt(i1, ic) = uu*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    if (nxl(2) > 0) then !first plateau
     do ic = 1, nsp_run
      n_peak = nxl(2)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(2)*uu
       wghpt(i1, ic) = wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================
    if (nxl(3) > 0) then !el+H(+) + dopant un(1:2) correct (electron,Z1) weights
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(3)*uu
       wghpt(i1, ic) = un(ic)*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ second plateau only electrons =================
    if (nxl(4) > 0) then
     do ic = 1, nsp_run
      n_peak = nxl(4)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(4)*uu
       wghpt(i1, ic) = wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    if (nxl(5) > 0) then !second down-ramp ==> 0
     do ic = 1, nsp_run
      n_peak = nxl(5)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(5)*uu
       wghpt(i1, ic) = (1.-uu)*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !===================================
   case (4)
    !================ cos^2 upramp with peak n0 =================
    if (nxl(1) > 0) then
     do ic = 1, nsp
      n_peak = nxl(1)*np_per_xc(ic)
      if (n_peak > 0) then
       do i = 1, n_peak
        uu = (real(i, dp) - 0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(1)*uu
        uu = uu - 1.
        wghpt(i1, ic) = one_dp*cos(0.5*pi*(uu))*cos(0.5*pi*(uu))* &
                        wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    !================ uniform layer n0=================
    if (nxl(2) > 0) then
     do ic = 1, nsp
      n_peak = nxl(2)*np_per_xc(ic)
      if (n_peak > 0) then
       do i = 1, n_peak
        uu = (real(i, dp) - 0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(2)*uu
        wghpt(i1, ic) = one_dp*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================ cos^2 downramp to the plateau np1*n0 =================
    if (nxl(3) > 0) then
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      if (n_peak > 0) then
       do i = 1, n_peak
        uu = (real(i, dp) - 0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(3)*uu
        uu = uu - 1.
        wghpt(i1, ic) = (np1 + (one_dp - np1)*sin(0.5*pi*(uu))*sin(0.5*pi*( &
                                                                   uu)))*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ Central layer of density np1*n0 =================
    if (nxl(4) > 0) then
     do ic = 1, nsp
      n_peak = nxl(4)*np_per_xc(ic)
      if (n_peak > 0) then
       do i = 1, n_peak
        uu = (real(i, dp) - 0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(4)*uu
        wghpt(i1, ic) = np1*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    !================ cos^2 downramp to second plateau np2*n0 =================
    if (nxl(5) > 0) then
     do ic = 1, nsp
      n_peak = nxl(5)*np_per_xc(ic)
      if (n_peak > 0) then
       do i = 1, n_peak
        uu = (real(i, dp) - 0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(5)*uu
        wghpt(i1, ic) = (np2 + (np1 - np2)*cos(0.5*pi*(uu))*cos(0.5*pi*( &
                                                                uu)))*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    !================ Second plateau of density np2*n0 =================
    if (nxl(6) > 0) then
     do ic = 1, nsp
      n_peak = nxl(6)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(6)*uu
       wghpt(i1, ic) = np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(6)
    end if

    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !=========================================
   end select
   if (xf0 < 0.) then
    do ic = 1, nsp
     i1 = 0
     if (pe0) write (6, *) 'tot part number', ic, nptx(ic)
     do i = 1, nptx(ic)
      if (xpt(i, ic) > xmin) then
       i1 = i1 + 1
       xpt(i1, ic) = xpt(i, ic)
       wghpt(i1, ic) = wghpt(i, ic)
      end if
     end do
     nptx(ic) = i1
     if (pe0) write (6, *) 'new tot part number', ic, nptx(ic)
    end do
   end if
   !=============================
   do ic = 1, nsp
    nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
   end do
   do ic = 1, nsp
    sptx_max(ic) = nptx(ic)
   end do
   !================================
   ! END of section setting global coordinates
   !=================================
   ! Restricts to the computational box
   !=================================
   if (pe0) then
    open (12, file='Initial_gas_target_x-profiles', form='formatted')
    do ic = 1, nsp
     i1 = sptx_max(ic)
     write (12, *) 'species ', ic, 'max x-coordinate', i1
     write (12, *) 'particle x-coordinate'
     write (12, '(6e11.4)') xpt(1:i1, ic)
     write (12, *) 'particle weight'
     write (12, '(6e11.4)') wghpt(1:i1, ic)
    end do
    close (12)
   end if
   !================================
   ! END of section setting global coordinates
   !=================================
   !Resets nptx(ic)=last particle coordinate inside the computational box
   !in the initial condition: for t>0  nptx(ic) updated by mowing window
   !==============================
   do ic = 1, nsp
    i1 = 0
    do j = 1, nptx(ic)
     if (xpt(j, ic) < xmax) i1 = i1 + 1
    end do
    nptx(ic) = i1
   end do
   !=========== Local x-distribution
   !Local to the x-cordinate MPI domain particle number
   !==================
   allocate (loc_wghx(nptx_max, nsp))
   do ic = 1, nsp
    call set_pgrid_xind(nptx(ic), ic)
   end do
   loc_nptx(1:nsp) = loc_imax(imodx, 1:nsp)
   ! Alocation using a large buffer npt_max=mp_per_cell(1)*nx_loc*ny_loc*nz_loc
   do ic = 1, nsp
    nptx_alloc(ic) = min(loc_nptx(ic), nx_loc*np_per_xc(ic))
   end do
   do ic = 1, nsp
    nps_loc(ic) = nptx_alloc(ic)*loc_jmax(imody, ic)*loc_kmax(imodz, ic)
   end do
   npmax = maxval(nps_loc(1:nsp))
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, &
    lpf_ord, 1, mem_psize, mobilebool)
   !==================================
   !==================== Local distribution of nptx particles==================
   call mpi_x_part_distrib(nsp)
   !===========================
   last_particle_index = 0
   !============
   !Particles are distributed according to the local
   ![loc_xpt,loc_ypt,loc_zpt] coordinates
   do ic = 1, nsp
    p = 0
    i2 = loc_nptx(ic)
    call pspecies_distribute(spec_in(ic), t0_pl(ic), unit_charge(ic), &
     p, ic, i2, last_particle_index(ic))
    loc_npart(imody, imodz, imodx, ic) = last_particle_index(ic)
   end do
  end subroutine
  !=============================
  subroutine multi_layer_gas_target_old(spec_in, spec_aux_in, layer_mod, nyh_in, xf0)

   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: layer_mod, nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, i, j, i1, i2, ic
   integer :: n_peak, npmax, nxtot, len_conc
   real (dp) :: uu, u2, xp_min, xp_max, u3, ramp_prefactor
   real (dp) :: xfsh, un(2), wgh_sp(nsp)
   real (dp), allocatable :: conc(:)
   integer :: nxl(6)
   integer :: nps_loc(4), last_particle_index(4), nptx_alloc(4)
   !==========================
   p = 0
   i = 0
   j = 0
   i1 = 0
   i2 = 0
   ic = 0
   call set_yz_distrib(nyh_in, nsp)
   !==========================
   xp_min = xmin
   xp_max = xmax
   !==========================
   nxl = 0
   !============================
   ! Parameters for particle distribution along the x-coordinate
   !============================
   ! Layers nxl(1:5) all containing the same ion species
   len_conc = size(concentration)
   allocate (conc(len_conc))
   conc(:) = concentration(:)   
   xtot = 0.0
   nxtot = 0
   do i = 1, 6
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
    nxtot = nxtot + nxl(i)
   end do
   if (xf0>0.0) then
    targ_in = xf0
    targ_end = targ_in + xtot
   else
    targ_in = xmin
    targ_end = xtot + xf0
   end if
   xfsh = xf0
   !=============================
   loc_nptx = 0
   loc_nptx(1:nsp) = (nxl(1)+nxl(2)+nxl(3)+nxl(4)+nxl(5)+nxl(6))* &
     np_per_xc(1:nsp)

   nptx_max = maxval(loc_nptx(1:nsp))
   allocate (xpt(nptx_max,nsp))
   allocate (wghpt(nptx_max,nsp))
   allocate (loc_xpt(nptx_max,nsp))
   !=============================
   wghpt = one_dp
   un = one_dp
   ramp_prefactor = one_dp
   !===================================
   ! WARNING for charge distribution
   !====================================
   ! wgh_sp(1:nsp) already set by initial conditions
   !=====================================================
   ! Longitudinal distribution
   nptx = 0
   !Weights for multispecies target
   !wgh_sp(1:3)=j0_norm
   !if(nsp==2)then
   !wgh_sp(2)=1./(real(mp_per_cell(2),dp))

   wgh_sp(1) = j0_norm*n0_ref*n_plasma
   do i = 2, nsp
    if (mp_per_cell(i)>0) wgh_sp(i) = n0_ref/real(mp_per_cell(i), dp)
    wgh_sp(i) = conc(i-1)*wgh_sp(i)
   end do
   select case (layer_mod)
   !================ first uniform layer np1=================
   case (1)
    if (nxl(1)>0) then
     ramp_prefactor = one_dp - np1
     do ic = 1, nsp
      n_peak = nxl(1)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(1)*uu
       wghpt(i1, ic) = np1*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    !================ first CUBIC ramp np1 => 1 --linear or exponential still available but commented =================
    if (nxl(2)>0) then
     do ic = 1, nsp
      n_peak = nxl(2)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(2)*uu
       !u2=(uu-1.)*(uu-1.)
       u2 = uu*uu
       u3 = u2*uu
       !wghpt(i1,ic)=(np1+exp(-4.5*u2)*(1.-np1))*wgh_sp(ic)
       !wghpt(i1,ic)=exp(-4.5*u2)*wgh_sp(ic)
       wghpt(i1, ic) = (-2.*ramp_prefactor*u3+3.*ramp_prefactor*u2+ &
         one_dp-ramp_prefactor)*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================ Central layer=================
    if (nxl(3)>0) then
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(3)*uu
       wghpt(i1, ic) = wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ second linear ramp =================
    if (nxl(4)>0) then
     do ic = 1, nsp
      n_peak = nxl(4)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(4)*uu
       wghpt(i1, ic) = (1.-uu*(1.-np2))*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    if (nxl(5)>0) then
     do ic = 1, nsp
      n_peak = nxl(5)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(5)*uu
       wghpt(i1, ic) = np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !=========================================
   case (2)
    !                 two bumps  (n1/n_c, n2/n_c) of length x_1 and x_2
    !                 n_over_nc enters as average n_over nc= (n1* x_1+n2*x_2)/(x_1+x_2)
    !                 weight j0_norm =>> j0_norm*np1 in x_1       =>> j0_norm*np2 in x_2
    !                 particle per cell uniform
    !================================================
    !================ first linear ramp to first plateau n1/n_c =================
    if (nxl(1)>0) then
     do ic = 1, nsp
      n_peak = nxl(1)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(1)*uu
       wghpt(i1, ic) = uu*np1*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    if (nxl(2)>0) then !first plateau
     do ic = 1, nsp
      n_peak = nxl(2)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(2)*uu
       wghpt(i1, ic) = np1*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================ np1 => np2 down-ramp =================
    if (nxl(3)>0) then
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(3)*uu
       wghpt(i1, ic) = wgh_sp(ic)*(np1+uu*(np2-np1))
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ second plateau n2/n_c < n1/n_c =================
    if (nxl(4)>0) then
     do ic = 1, nsp
      n_peak = nxl(4)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(4)*uu
       wghpt(i1, ic) = np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    if (nxl(5)>0) then !second down-ramp n2/n_c ==> 0
     do ic = 1, nsp
      n_peak = nxl(5)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(5)*uu
       wghpt(i1, ic) = (1.-uu)*np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !=====================================
   case (3)
    !                 three layers
    !                 lpx(1)[ramp]+lpx(2)[plateau]  and lpx(4) plateu lpx(5) downramp n_ion=0 n_e=n_0
    !                 lpx(3)[plateau]  with a (A1-Z1) dopant with % density np1=n1_per_nc/n_per_nc
    !                 and electronic density n_e=n_0+Z1*n_ion  n0=n_H(+)
    !---------------
    !================================================
    !Z1 electrons are accounted for by a larger electron weight
    un(1) = 1. + ion_min(1)*np1
    un(2) = np1 !float(mp_per_cell(1))/float(mp_per_cell(ic))

    if (nxl(1)>0) then
     do ic = 1, nsp_run
      n_peak = nxl(1)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(1)*uu
       wghpt(i1, ic) = uu*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    if (nxl(2)>0) then !first plateau
     do ic = 1, nsp_run
      n_peak = nxl(2)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(2)*uu
       wghpt(i1, ic) = wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================
    if (nxl(3)>0) then !el+H(+) + dopant un(1:2) correct (electron,Z1) weights
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(3)*uu
       wghpt(i1, ic) = un(ic)*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ second plateau only electrons =================
    if (nxl(4)>0) then
     do ic = 1, nsp_run
      n_peak = nxl(4)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(4)*uu
       wghpt(i1, ic) = wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    if (nxl(5)>0) then !second down-ramp ==> 0
     do ic = 1, nsp_run
      n_peak = nxl(5)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(5)*uu
       wghpt(i1, ic) = (1.-uu)*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !===================================
   case (4)
    !================ cos^2 upramp with peak n0 =================
    if (nxl(1)>0) then
     do ic = 1, nsp
      n_peak = nxl(1)*np_per_xc(ic)
      if (n_peak>0) then
       do i = 1, n_peak
        uu = (real(i,dp)-0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(1)*uu
        uu = uu - 1.
        wghpt(i1, ic) = one_dp*cos(0.5*pi*(uu))*cos(0.5*pi*(uu))* &
          wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(1)
    end if
    !================ uniform layer n0=================
    if (nxl(2)>0) then
     do ic = 1, nsp
      n_peak = nxl(2)*np_per_xc(ic)
      if (n_peak>0) then
       do i = 1, n_peak
        uu = (real(i,dp)-0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(2)*uu
        wghpt(i1, ic) = one_dp*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(2)
    end if
    !================ cos^2 downramp to the plateau np1*n0 =================
    if (nxl(3)>0) then
     do ic = 1, nsp
      n_peak = nxl(3)*np_per_xc(ic)
      if (n_peak>0) then
       do i = 1, n_peak
        uu = (real(i,dp)-0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(3)*uu
        uu = uu - 1.
        wghpt(i1, ic) = (np1+(one_dp-np1)*sin(0.5*pi*(uu))*sin(0.5*pi*( &
          uu)))*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(3)
    end if
    !================ Central layer of density np1*n0 =================
    if (nxl(4)>0) then
     do ic = 1, nsp
      n_peak = nxl(4)*np_per_xc(ic)
      if (n_peak>0) then
       do i = 1, n_peak
        uu = (real(i,dp)-0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(4)*uu
        wghpt(i1, ic) = np1*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(4)
    end if
    !================ cos^2 downramp to second plateau np2*n0 =================
    if (nxl(5)>0) then
     do ic = 1, nsp
      n_peak = nxl(5)*np_per_xc(ic)
      if (n_peak>0) then
       do i = 1, n_peak
        uu = (real(i,dp)-0.5)/real(n_peak, dp)
        i1 = nptx(ic) + i
        xpt(i1, ic) = xfsh + lpx(5)*uu
        wghpt(i1, ic) = (np2+(np1-np2)*cos(0.5*pi*(uu))*cos(0.5*pi*( &
          uu)))*wgh_sp(ic)
       end do
      end if
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(5)
    end if
    !================ Second plateau of density np2*n0 =================
    if (nxl(6)>0) then
     do ic = 1, nsp
      n_peak = nxl(6)*np_per_xc(ic)
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       i1 = nptx(ic) + i
       xpt(i1, ic) = xfsh + lpx(6)*uu
       wghpt(i1, ic) = np2*wgh_sp(ic)
      end do
      nptx(ic) = nptx(ic) + n_peak
     end do
     xfsh = xfsh + lpx(6)
    end if

    do ic = 1, nsp
     nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
    end do
    !=========================================
   end select
   if (xf0<0.) then
    do ic = 1, nsp
     i1 = 0
     if (pe0) write (6, *) 'tot part number', ic, nptx(ic)
     do i = 1, nptx(ic)
      if (xpt(i,ic)>xmin) then
       i1 = i1 + 1
       xpt(i1, ic) = xpt(i, ic)
       wghpt(i1, ic) = wghpt(i, ic)
      end if
     end do
     nptx(ic) = i1
     if (pe0) write (6, *) 'new tot part number', ic, nptx(ic)
    end do
   end if
   !============================= 
   do ic = 1, nsp
    nptx_alloc(ic) = min(nptx(ic), nx*np_per_xc(ic))
   end do
   do ic = 1, nsp
    sptx_max(ic) = nptx(ic)
   end do
   !================================
   ! END of section setting global coordinates
   !=================================
   ! Restricts to the computational box
   !=================================
   if (pe0) then
    open (12, file='Initial_gas_target_x-profiles', form='formatted')
    do ic = 1, nsp
     i1 = sptx_max(ic)
     write (12, *) 'species ', ic, 'max x-coordinate', i1
     write (12, *) 'particle x-coordinate'
     write (12, '(6e11.4)') xpt(1:i1, ic)
     write (12, *) 'particle weight'
     write (12, '(6e11.4)') wghpt(1:i1, ic)
    end do
    close (12)
   end if
   !================================
   ! END of section setting global coordinates
   !=================================
   !Resets nptx(ic)=last particle coordinate inside the computational box
   !in the initial condition: for t>0  nptx(ic) updated by mowing window 
   !==============================
   do ic = 1, nsp
    i1 = 0
    do j = 1, nptx(ic)
     if (xpt(j,ic)<xmax) i1 = i1 + 1
    end do
    nptx(ic) = i1
   end do
   !=========== Local x-distribution
   !Local to the x-cordinate MPI domain particle number
   !==================
   allocate (loc_wghx(nptx_max,nsp))
   do ic = 1, nsp
    call set_pgrid_xind(nptx(ic), ic)
   end do
   loc_nptx(1:nsp) = loc_imax(imodx, 1:nsp)
   ! Alocation using a large buffer npt_max=mp_per_cell(1)*nx_loc*ny_loc*nz_loc
   do ic = 1, nsp
    nptx_alloc(ic) = min(loc_nptx(ic), nx_loc*np_per_xc(ic))
   end do
   do ic = 1, nsp
    nps_loc(ic) = nptx_alloc(ic)*loc_jmax(imody, ic)*loc_kmax(imodz, ic)
   end do
   npmax = maxval(nps_loc(1:nsp))
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
   !==================================
   !==================== Local distribution of nptx particles==================
   call mpi_x_part_distrib(nsp)
   !===========================
   last_particle_index = 0
   !============
   !Particles are distributed according to the local
   ![loc_xpt,loc_ypt,loc_zpt] coordinates
   do ic = 1, nsp
    p = 0
    i2 = loc_nptx(ic)
    if (i2>0) call pspecies_distribute(spec_in(ic), t0_pl(ic), &
      unit_charge(ic), p, ic, i2, last_particle_index(ic))
    loc_npart(imody, imodz, imodx, ic) = last_particle_index(ic)
   end do
  end subroutine
  !=============================
  !===============================
  subroutine preplasma_multisp_old(spec_in, spec_aux_in, nyh_in, xf0)

   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip
   integer :: l, i, i1, i2, ic
   integer :: n_peak, nptx_loc(6)
   integer :: npmax, nps_loc(4)
   real (dp) :: uu, np1_loc
   real (dp) :: xp_min, xp_max
   real (dp) :: xfsh, l_inv
   integer :: nxl(6)
   integer :: ip_ion, ip_el, ip_pr
   !=================
   xp_min = xmin
   xp_max = xmax
   nxl = 0
   !========== uniform y-z distribution of El-Ion layers
   call set_uniform_yz_distrib(nyh_in, 6)
   !===============
   xtot = 0.0
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   nxl(4) = 0 !attached coating
   if (nsp<3) then
    nxl(5) = 0
    if (pe0) then
     write (6, *) &
       'Warning : for nsp=2 nxl(5) forced to zero => NO coating layer'
    end if
   end if
   xfsh = xf0 + lpx(7)
   targ_in = xfsh
   targ_end = targ_in + xtot
   !               nsp=4 species x-distribution
   !               preplasma
   !==================================
   !            lx(2:3) (Z1-A1+El) central target
   nptx_loc(1:2) = (nxl(2)+nxl(3))*np_per_xc(1:2)
   !            lx(1) (Z1-A1-El) uniform with np1 density pre-plasma 
   nptx_loc(3:4) = nxl(1)*np_per_xc(3:4)
   !            lx(5) (Z2-A2+El) ) uniform with np2 density coating
   nptx_loc(5:6) = nxl(5)*np_per_xc(5:6)
   !========================
   nptx(1) = nptx_loc(1) + nptx_loc(3) + nptx_loc(5) !electrons
   nptx(2) = nptx_loc(2) !Z1-A1 species
   nptx(3) = nptx_loc(4) !Z1-A1 species
   nptx(4) = nptx_loc(6) !Z2-A2  species nlx(5)
   nptx_max = maxval(nptx_loc(1:6))
   !=======================
   allocate (xpt(nptx_max,6))
   allocate (wghpt(nptx_max,6))

   allocate (loc_xpt(nptx_max,6))
   allocate (loc_wghx(nptx_max,6))
   wghpt(1:nptx_max, 1:6) = one_dp
   !=============================
   ! first layer: electrons and Z1 ions
   !x distribution
   loc_imax(imodx, 1:6) = nptx_loc(1:6)
   nps_loc = 0
   if (nxl(1)>0) then
    do ic = 3, 4
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      xpt(i, ic) = xfsh + lpx(1)*uu
     end do
    end do
    wghpt(1:nptx_loc(3), 3) = np1*j0_norm
    wghpt(1:nptx_loc(4), 4) = np1*j0_norm
    if (mp_per_cell(3)>0) then
     uu = ratio_mpc(3) !float(mp_per_cell(1))/float(mp_per_cell(3))
     wghpt(1:nptx_loc(3), 3) = wghpt(1:nptx_loc(3), 3)*uu
     uu = ratio_mpc(4)/unit_charge(2) !float(mp_per_cell(1))/float(mp_per_cell(4))
     wghpt(1:nptx_loc(4), 4) = wghpt(1:nptx_loc(4), 4)*uu
    end if
    xfsh = xfsh + lpx(1)
    !=========== Distributes on x-MPI tasks the first layer
    do ic = 3, 4
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    !========================
    !========================
    p = imodx
    l = imody
    ip = imodz
    ! Counts particles in first layer

    nps_loc(1) = nps_loc(1) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip, &
      3)
    nps_loc(2) = nps_loc(2) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip, &
      4)

   end if
   !------------------------------
   ! Electrons and Z1_ions : central layer
   ! x distribution
   !====================
   do ic = 1, 2
    n_peak = nptx_loc(ic)
    do i = 1, n_peak
     xpt(i, ic) = xfsh + (lpx(2)+lpx(3))*(real(i,dp)-0.5)/real(n_peak, &
       dp)
     wghpt(i, ic) = j0_norm
    end do
   end do
   uu = ratio_mpc(2)/real(ion_min(1), dp)
   ic = 2
   n_peak = nptx_loc(ic)
   wghpt(1:n_peak, ic) = j0_norm*uu
   !================================
   ! Electrons and Z1 ions have the same weight if uu=1
   !=================================
   xfsh = xfsh + lpx(3) + lpx(2)
   if (np1>0.0) then
    np1_loc = np1
   else
    np1_loc = 0.005
   end if
   !======== a preplasma rump
   if (nxl(2)>0) then
    do ic = 1, 2
     n_peak = nxl(2)*np_per_xc(ic)
     l_inv = log(1./np1_loc)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      ! rampa esponenziale v1
      ! wghpt(i,ic)=wghpt(i,ic)*exp(-5.*(1.-uu))
      ! rampa esponenziale v2
      ! Same species as later 2 (electrons+Z1-ions)
      wghpt(i, ic) = wghpt(i, ic)*np1_loc*exp(uu*l_inv)
      ! rampa lineare
      ! wghpt(i,ic)=wghpt(i,ic)*uu
     end do
    end do
   end if
   !=========== Distributes on x-MPI tasks
   do ic = 1, 2
    i1 = 0
    do i = 1, nptx_loc(ic)
     if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. xpt(i,ic)<loc_xgrid( &
       imodx)%gmax) then
      i1 = i1 + 1
      loc_xpt(i1, ic) = xpt(i, ic)
      loc_wghx(i1, ic) = wghpt(i, ic)
     end if
    end do
    loc_imax(imodx, ic) = i1
   end do
   p = imodx
   l = imody
   ip = imodz

   nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
     1)
   nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
     2)
   !============================
   if (nptx_loc(5)>0.0) then
    do ic = 5, 6
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      xpt(i, ic) = xfsh + lpx(5)*(real(i,dp)-0.5)/real(n_peak, dp)
     end do
     wghpt(1:n_peak, ic) = np2*j0_norm
    end do
    do ic = 5, 6
     if (mp_per_cell(ic)>0) then
      uu = ratio_mpc(ic) !float(mp_per_cell(1))/float(mp_per_cell(ic))
      n_peak = nptx_loc(ic)
      wghpt(1:n_peak, ic) = wghpt(1:n_peak, ic)*uu
     end if
    end do
    xfsh = xfsh + lpx(5)
    !===============================
    ! Warning : holds for Z2_ion= H(+)
    !===============================
    do ic = 5, 6
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax(ip, &
      5)
    nps_loc(3) = nps_loc(3) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
      6)

   end if
   !======================
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   !============
   ! The first electron-proton(or C) foam layer
   if (nxl(1)>0) then
    p = 0
    i2 = loc_imax(imodx, 3)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 3, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 4)
    call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 4, &
      i2, ip_ion)
   end if
   !=========================
   ! The second electron-ion solid electron-Z1 layer
   p = ip_el
   i2 = loc_imax(imodx, 1)
   call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, i2, &
     ip_el)

   p = ip_ion
   i2 = loc_imax(imodx, 2)
   call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, i2, &
     ip_ion)
   !============
   ! The third electron-proton layer
   !=========================
   if (nxl(5)>0.0) then
    p = ip_el
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 5, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 6, &
      i2, ip_pr)
   end if

   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

   !============
  end subroutine
  !===============================
  subroutine preplasma_multisp_new(spec_in, spec_aux_in, nyh_in, xf0)

   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip
   integer :: l, i, i1, i2, ic
   integer :: n_peak, nptx_loc(6)
   integer :: npmax, nps_loc(4)
   real (dp) :: uu, np1_loc
   real (dp) :: xp_min, xp_max
   real (dp) :: xfsh, l_inv
   integer :: nxl(6)
   integer :: ip_ion, ip_el, ip_pr
   logical, allocatable, dimension(:) :: mobilebool
   !=================
   xp_min = xmin
   xp_max = xmax
   nxl = 0
   !=================================
   ! Array that defines if species is mobile.
   ! We should consider setting it in the input file
   allocate(mobilebool(nsp), source=.true.)
   !=======================
   !========== uniform y-z distribution of El-Ion layers
   call set_uniform_yz_distrib(nyh_in, 6)
   !===============
   xtot = 0.0
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   nxl(4) = 0 !attached coating
   if (nsp<3) then
    nxl(5) = 0
    if (pe0) then
     write (6, *) &
       'Warning : for nsp=2 nxl(5) forced to zero => NO coating layer'
    end if
   end if
   xfsh = xf0 + lpx(7)
   targ_in = xfsh
   targ_end = targ_in + xtot
   !               nsp=4 species x-distribution
   !               preplasma
   !==================================
   !            lx(2:3) (Z1-A1+El) central target
   nptx_loc(1:2) = (nxl(2)+nxl(3))*np_per_xc(1:2)
   !            lx(1) (Z1-A1-El) uniform with np1 density pre-plasma 
   nptx_loc(3:4) = nxl(1)*np_per_xc(3:4)
   !            lx(5) (Z2-A2+El) ) uniform with np2 density coating
   nptx_loc(5:6) = nxl(5)*np_per_xc(5:6)
   !========================
   nptx(1) = nptx_loc(1) + nptx_loc(3) + nptx_loc(5) !electrons
   nptx(2) = nptx_loc(2) !Z1-A1 species
   nptx(3) = nptx_loc(4) !Z1-A1 species
   nptx(4) = nptx_loc(6) !Z2-A2  species nlx(5)
   nptx_max = maxval(nptx_loc(1:6))
   !=======================
   allocate (xpt(nptx_max,6))
   allocate (wghpt(nptx_max,6))

   allocate (loc_xpt(nptx_max,6))
   allocate (loc_wghx(nptx_max,6))
   wghpt(1:nptx_max, 1:6) = one_dp
   !=============================
   ! first layer: electrons and Z1 ions
   !x distribution
   loc_imax(imodx, 1:6) = nptx_loc(1:6)
   nps_loc = 0
   if (nxl(1)>0) then
    do ic = 3, 4
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      xpt(i, ic) = xfsh + lpx(1)*uu
     end do
    end do
    wghpt(1:nptx_loc(3), 3) = np1*j0_norm
    wghpt(1:nptx_loc(4), 4) = np1*j0_norm
    if (mp_per_cell(3)>0) then
     uu = ratio_mpc(3) !float(mp_per_cell(1))/float(mp_per_cell(3))
     wghpt(1:nptx_loc(3), 3) = wghpt(1:nptx_loc(3), 3)*uu
     uu = ratio_mpc(4)/unit_charge(2) !float(mp_per_cell(1))/float(mp_per_cell(4))
     wghpt(1:nptx_loc(4), 4) = wghpt(1:nptx_loc(4), 4)*uu
    end if
    xfsh = xfsh + lpx(1)
    !=========== Distributes on x-MPI tasks the first layer
    do ic = 3, 4
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    !========================
    !========================
    p = imodx
    l = imody
    ip = imodz
    ! Counts particles in first layer

    nps_loc(1) = nps_loc(1) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip, &
      3)
    nps_loc(2) = nps_loc(2) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip, &
      4)

   end if
   !------------------------------
   ! Electrons and Z1_ions : central layer
   ! x distribution
   !====================
   do ic = 1, 2
    n_peak = nptx_loc(ic)
    do i = 1, n_peak
     xpt(i, ic) = xfsh + (lpx(2)+lpx(3))*(real(i,dp)-0.5)/real(n_peak, &
       dp)
     wghpt(i, ic) = j0_norm
    end do
   end do
   uu = ratio_mpc(2)/real(ion_min(1), dp)
   ic = 2
   n_peak = nptx_loc(ic)
   wghpt(1:n_peak, ic) = j0_norm*uu
   !================================
   ! Electrons and Z1 ions have the same weight if uu=1
   !=================================
   xfsh = xfsh + lpx(3) + lpx(2)
   if (np1>0.0) then
    np1_loc = np1
   else
    np1_loc = 0.005
   end if
   !======== a preplasma rump
   if (nxl(2)>0) then
    do ic = 1, 2
     n_peak = nxl(2)*np_per_xc(ic)
     l_inv = log(1./np1_loc)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      ! rampa esponenziale v1
      ! wghpt(i,ic)=wghpt(i,ic)*exp(-5.*(1.-uu))
      ! rampa esponenziale v2
      ! Same species as later 2 (electrons+Z1-ions)
      wghpt(i, ic) = wghpt(i, ic)*np1_loc*exp(uu*l_inv)
      ! rampa lineare
      ! wghpt(i,ic)=wghpt(i,ic)*uu
     end do
    end do
   end if
   !=========== Distributes on x-MPI tasks
   do ic = 1, 2
    i1 = 0
    do i = 1, nptx_loc(ic)
     if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. xpt(i,ic)<loc_xgrid( &
       imodx)%gmax) then
      i1 = i1 + 1
      loc_xpt(i1, ic) = xpt(i, ic)
      loc_wghx(i1, ic) = wghpt(i, ic)
     end if
    end do
    loc_imax(imodx, ic) = i1
   end do
   p = imodx
   l = imody
   ip = imodz

   nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
     1)
   nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
     2)
   !============================
   if (nptx_loc(5)>0.0) then
    do ic = 5, 6
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      xpt(i, ic) = xfsh + lpx(5)*(real(i,dp)-0.5)/real(n_peak, dp)
     end do
     wghpt(1:n_peak, ic) = np2*j0_norm
    end do
    do ic = 5, 6
     if (mp_per_cell(ic)>0) then
      uu = ratio_mpc(ic) !float(mp_per_cell(1))/float(mp_per_cell(ic))
      n_peak = nptx_loc(ic)
      wghpt(1:n_peak, ic) = wghpt(1:n_peak, ic)*uu
     end if
    end do
    xfsh = xfsh + lpx(5)
    !===============================
    ! Warning : holds for Z2_ion= H(+)
    !===============================
    do ic = 5, 6
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax(ip, &
      5)
    nps_loc(3) = nps_loc(3) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
      6)

   end if
   !======================
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, &
   mem_psize, mobilebool)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   !============
   ! The first electron-proton(or C) foam layer
   if (nxl(1)>0) then
    p = 0
    i2 = loc_imax(imodx, 3)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 3, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 4)
    call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 4, &
      i2, ip_ion)
   end if
   !=========================
   ! The second electron-ion solid electron-Z1 layer
   p = ip_el
   i2 = loc_imax(imodx, 1)
   call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, &
    i2, ip_el)

   p = ip_ion
   i2 = loc_imax(imodx, 2)
   call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, &
    i2, ip_ion)
   !============
   ! The third electron-proton layer
   !=========================
   if (nxl(5)>0.0) then
    p = ip_el
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 5, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 6, &
      i2, ip_pr)
   end if

   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

   !============
  end subroutine
  !=====================
  subroutine multi_layer_twosp_target_new(spec_in, spec_aux_in, nyh_in, xf0)

   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip, l, i, i1, i2, ic, n_peak, nptx_loc(6)
   integer :: npmax, nps_loc(4)
   real (dp) :: l_inv, uu, np1_loc, xp_min, xp_max
   real (dp) :: xfsh, wgh_sp(6)
   integer :: nxl(6)
   integer :: ip_ion, ip_el, ip_pr
   logical, allocatable, dimension(:) :: mobilebool
   !=================
   xp_min = xmin
   xp_max = xmax
   !=================================
   ! Array that defines if species is mobile.
   ! We should consider setting it in the input file
   allocate(mobilebool(nsp), source=.true.)
   call set_uniform_yz_distrib(nyh_in, 6)
   !==============================
   nxl = 0
   !x- distribution

   xtot = 0.0
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   xfsh = xf0 + lpx(7)
   targ_in = xfsh
   targ_end = targ_in + xtot
   if (nsp<3) then
    nxl(1) = 0
    nxl(5) = 0
    if (pe0) then
     write (6, *) 'Warning : for nsp=2 nxl(1) and nxl(5) forced to zero'
    end if
   end if
   ! Species x-distribution
   !=========np_per_xc(1:2) electrons and Z1-ions in   ramp +  central layer
   !sizes lpx(2)+lpx(3)
   !========= np_per_xc(3:4) electrons and Z2-ions in layer 1
   !size lpx(1)
   !========= np_per_xc(5:6) electrons and Z3-ions in layer 3 +ramp
   !                                                  sizes lpx(4)+lpx(5)
   nptx_loc(1:2) = (nxl(2)+nxl(3))*np_per_xc(1:2) !Z1 ions
   nptx_loc(3:4) = nxl(1)*np_per_xc(3:4) !Z2 ions
   nptx_loc(5:6) = nxl(5)*np_per_xc(5:6) !Z3 ions
   nptx(1) = nptx_loc(1) + nptx_loc(3) + nptx_loc(5) !electrons
   nptx(2) = nptx_loc(2) !Z1-A1 species
   nptx(3) = nptx_loc(4) + nptx_loc(6) !Z2-A2  species nxl(1)+nlx(4)
   nptx_max = maxval(nptx_loc(1:6))
   !=======================
   allocate (xpt(nptx_max,6))
   allocate (wghpt(nptx_max,6))

   allocate (loc_xpt(nptx_max,6))
   allocate (loc_wghx(nptx_max,6))
   wghpt(1:nptx_max, 1:6) = 1.
   !==================
   ! nsp=1-4 ordering: electrons+ Z1 ions + Z2 ions +Z3 ions
   !Weights for multilayer multisp targets
   ! first layer: electrons and Z2 (protons) ions
   wgh_sp(3) = 1./real(mp_per_cell(3), dp)
   wgh_sp(4) = 1./(real(ion_min(2),dp)*real(mp_per_cell(4),dp))
   ! central layer: electrons and Z1 ions
   wgh_sp(1) = j0_norm
   wgh_sp(2) = wgh_ion !ion spec 1 (ionizable)
   ! coiating layer: electrons and Z3 (H+)
   wgh_sp(5) = 1./real(mp_per_cell(5), dp)
   wgh_sp(6) = 1./(real(ion_min(2),dp)*real(mp_per_cell(6),dp))
   !================================================
   !x distribution
   loc_imax(imodx, 1:6) = nptx_loc(1:6)
   nps_loc = 0
   if (nxl(1)>0) then
    do ic = 3, 4
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      xpt(i, ic) = xfsh + lpx(1)*uu
      wghpt(i, ic) = np1*wgh_sp(ic)
     end do
    end do
    xfsh = xfsh + lpx(1)
    !=========== Distributes on x-MPI tasks
    do ic = 3, 4
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    !========================
    !========================
    p = imodx
    l = imody
    ip = imodz
    ! Counts particles (e+Z2 ions)

    nps_loc(1) = nps_loc(1) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip, &
      3)
    nps_loc(3) = nps_loc(3) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip, &
      4)
   end if
   !------------------------------
   ! Electrons and Z1_ions : central layer
   ! x distribution
   !====================
   do ic = 1, 2
    n_peak = nptx_loc(ic)
    n_peak = max(1, n_peak)
    do i = 1, n_peak
     xpt(i, ic) = xfsh + (lpx(2)+lpx(3))*(real(i,dp)-0.5)/real(n_peak, &
       dp)
     wghpt(i, ic) = wgh_sp(ic)
    end do
   end do
   !================================
   ! WARNING : electrons and Z1 ions have the same weight 
   ! if mp_per_cell(1)=Z1*mp_per_cell(2)
   !=================================
   xfsh = xfsh + lpx(3) + lpx(2)
   if (np1>0.0) then
    np1_loc = np1
   else
    np1_loc = 0.005
   end if
   !======== a preplasma rump
   if (nxl(2)>0) then
    do ic = 1, 2
     n_peak = nxl(2)*np_per_xc(ic)
     l_inv = log(1./np1_loc)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      ! rampa esponenziale v1
      ! wghpt(i,ic)=wghpt(i,ic)*exp(-5.*(1.-uu))
      ! rampa esponenziale v2
      ! Same species as later 2 (electrons+Z1-ions)
      wghpt(i, ic) = wghpt(i, ic)*np1_loc*exp(uu*l_inv)
      ! rampa lineare
      ! wghpt(i,ic)=wghpt(i,ic)*uu
     end do
    end do
   end if
   !=========== Distributes on x-MPI tasks
   do ic = 1, 2
    i1 = 0
    do i = 1, nptx_loc(ic)
     if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. xpt(i,ic)<loc_xgrid( &
       imodx)%gmax) then
      i1 = i1 + 1
      loc_xpt(i1, ic) = xpt(i, ic)
      loc_wghx(i1, ic) = wghpt(i, ic)
     end if
    end do
    loc_imax(imodx, ic) = i1
   end do
   p = imodx
   l = imody
   ip = imodz

   nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
     1)
   nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
     2)
   !============================
   if (nptx_loc(5)>0.0) then
    do ic = 5, 6
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      xpt(i, ic) = xfsh + lpx(5)*(real(i,dp)-0.5)/real(n_peak, dp)
      wghpt(i, ic) = np2*wgh_sp(ic)
     end do
    end do
    xfsh = xfsh + lpx(5)
    !===================================
    do ic = 5, 6
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax(ip, &
      5)
    nps_loc(3) = nps_loc(3) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
      6)

   end if
   !======================
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1,&
   mem_psize, mobilebool)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   !============
   ! The first electron-proton(or C) foam layer
   if (nxl(1)>0) then
    p = 0
    i2 = loc_imax(imodx, 3)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 3, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 4)
    call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 4, &
      i2, ip_pr)
   end if
   !=========================
   ! The second electron-ion solid electron-Z1 layer
   p = ip_el
   i2 = loc_imax(imodx, 1)
   call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, &
    i2, ip_el)

   p = 0
   i2 = loc_imax(imodx, 2)
   call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, &
    i2, ip_ion)
   !============
   ! The third electron-proton layer
   !=========================
   if (nxl(5)>0.0) then
    p = ip_el
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 5, &
      i2, ip)
    p = ip_pr
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 6, &
      i2, p)
   end if

   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

   !============
  end subroutine
  !=======================
  subroutine multi_layer_twosp_target_old(spec_in, spec_aux_in, nyh_in, xf0)

   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip, l, i, i1, i2, ic, n_peak, nptx_loc(6)
   integer :: npmax, nps_loc(4)
   real (dp) :: l_inv, uu, np1_loc, xp_min, xp_max
   real (dp) :: xfsh, wgh_sp(6)
   integer :: nxl(6)
   integer :: ip_ion, ip_el, ip_pr
   !=================
   xp_min = xmin
   xp_max = xmax
   call set_uniform_yz_distrib(nyh_in, 6)
   !==============================
   nxl = 0
   !x- distribution

   xtot = 0.0
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   xfsh = xf0 + lpx(7)
   targ_in = xfsh
   targ_end = targ_in + xtot
   if (nsp<3) then
    nxl(1) = 0
    nxl(5) = 0
    if (pe0) then
     write (6, *) 'Warning : for nsp=2 nxl(1) and nxl(5) forced to zero'
    end if
   end if
   ! Species x-distribution
   !=========np_per_xc(1:2) electrons and Z1-ions in   ramp +  central layer
   !sizes lpx(2)+lpx(3)
   !========= np_per_xc(3:4) electrons and Z2-ions in layer 1
   !size lpx(1)
   !========= np_per_xc(5:6) electrons and Z3-ions in layer 3 +ramp
   !                                                  sizes lpx(4)+lpx(5)
   nptx_loc(1:2) = (nxl(2)+nxl(3))*np_per_xc(1:2) !Z1 ions
   nptx_loc(3:4) = nxl(1)*np_per_xc(3:4) !Z2 ions
   nptx_loc(5:6) = nxl(5)*np_per_xc(5:6) !Z3 ions
   nptx(1) = nptx_loc(1) + nptx_loc(3) + nptx_loc(5) !electrons
   nptx(2) = nptx_loc(2) !Z1-A1 species
   nptx(3) = nptx_loc(4) + nptx_loc(6) !Z2-A2  species nxl(1)+nlx(4)
   nptx_max = maxval(nptx_loc(1:6))
   !=======================
   allocate (xpt(nptx_max,6))
   allocate (wghpt(nptx_max,6))

   allocate (loc_xpt(nptx_max,6))
   allocate (loc_wghx(nptx_max,6))
   wghpt(1:nptx_max, 1:6) = 1.
   !==================
   ! nsp=1-4 ordering: electrons+ Z1 ions + Z2 ions +Z3 ions
   !Weights for multilayer multisp targets
   ! first layer: electrons and Z2 (protons) ions
   wgh_sp(3) = 1./real(mp_per_cell(3), dp)
   wgh_sp(4) = 1./(real(ion_min(2),dp)*real(mp_per_cell(4),dp))
   ! central layer: electrons and Z1 ions
   wgh_sp(1) = j0_norm
   wgh_sp(2) = wgh_ion !ion spec 1 (ionizable)
   ! coiating layer: electrons and Z3 (H+)
   wgh_sp(5) = 1./real(mp_per_cell(5), dp)
   wgh_sp(6) = 1./(real(ion_min(2),dp)*real(mp_per_cell(6),dp))
   !================================================
   !x distribution
   loc_imax(imodx, 1:6) = nptx_loc(1:6)
   nps_loc = 0
   if (nxl(1)>0) then
    do ic = 3, 4
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      xpt(i, ic) = xfsh + lpx(1)*uu
      wghpt(i, ic) = np1*wgh_sp(ic)
     end do
    end do
    xfsh = xfsh + lpx(1)
    !=========== Distributes on x-MPI tasks
    do ic = 3, 4
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    !========================
    !========================
    p = imodx
    l = imody
    ip = imodz
    ! Counts particles (e+Z2 ions)

    nps_loc(1) = nps_loc(1) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip, &
      3)
    nps_loc(3) = nps_loc(3) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip, &
      4)
   end if
   !------------------------------
   ! Electrons and Z1_ions : central layer
   ! x distribution
   !====================
   do ic = 1, 2
    n_peak = nptx_loc(ic)
    n_peak = max(1, n_peak)
    do i = 1, n_peak
     xpt(i, ic) = xfsh + (lpx(2)+lpx(3))*(real(i,dp)-0.5)/real(n_peak, &
       dp)
     wghpt(i, ic) = wgh_sp(ic)
    end do
   end do
   !================================
   ! WARNING : electrons and Z1 ions have the same weight 
   ! if mp_per_cell(1)=Z1*mp_per_cell(2)
   !=================================
   xfsh = xfsh + lpx(3) + lpx(2)
   if (np1>0.0) then
    np1_loc = np1
   else
    np1_loc = 0.005
   end if
   !======== a preplasma rump
   if (nxl(2)>0) then
    do ic = 1, 2
     n_peak = nxl(2)*np_per_xc(ic)
     l_inv = log(1./np1_loc)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      ! rampa esponenziale v1
      ! wghpt(i,ic)=wghpt(i,ic)*exp(-5.*(1.-uu))
      ! rampa esponenziale v2
      ! Same species as later 2 (electrons+Z1-ions)
      wghpt(i, ic) = wghpt(i, ic)*np1_loc*exp(uu*l_inv)
      ! rampa lineare
      ! wghpt(i,ic)=wghpt(i,ic)*uu
     end do
    end do
   end if
   !=========== Distributes on x-MPI tasks
   do ic = 1, 2
    i1 = 0
    do i = 1, nptx_loc(ic)
     if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. xpt(i,ic)<loc_xgrid( &
       imodx)%gmax) then
      i1 = i1 + 1
      loc_xpt(i1, ic) = xpt(i, ic)
      loc_wghx(i1, ic) = wghpt(i, ic)
     end if
    end do
    loc_imax(imodx, ic) = i1
   end do
   p = imodx
   l = imody
   ip = imodz

   nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
     1)
   nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
     2)
   !============================
   if (nptx_loc(5)>0.0) then
    do ic = 5, 6
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      xpt(i, ic) = xfsh + lpx(5)*(real(i,dp)-0.5)/real(n_peak, dp)
      wghpt(i, ic) = np2*wgh_sp(ic)
     end do
    end do
    xfsh = xfsh + lpx(5)
    !===================================
    do ic = 5, 6
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax(ip, &
      5)
    nps_loc(3) = nps_loc(3) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
      6)

   end if
   !======================
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   !============
   ! The first electron-proton(or C) foam layer
   if (nxl(1)>0) then
    p = 0
    i2 = loc_imax(imodx, 3)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 3, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 4)
    call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 4, &
      i2, ip_pr)
   end if
   !=========================
   ! The second electron-ion solid electron-Z1 layer
   p = ip_el
   i2 = loc_imax(imodx, 1)
   call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, i2, &
     ip_el)

   p = 0
   i2 = loc_imax(imodx, 2)
   call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, i2, &
     ip_ion)
   !============
   ! The third electron-proton layer
   !=========================
   if (nxl(5)>0.0) then
    p = ip_el
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 5, &
      i2, ip)
    p = ip_pr
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 6, &
      i2, ip)
   end if

   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

   !============
  end subroutine
  !=======================
  subroutine multi_layer_threesp_target_new(spec_in, spec_aux_in, nyh_in, xf0)

   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout)  :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip
   integer :: l, i, i1, i2, ic
   integer :: n_peak, nptx_loc(7)
   integer :: npmax, nps_loc(4)
   real (dp) :: uu, xp_min, xp_max, np1_loc
   real (dp) :: xfsh, l_inv, wgh_sp(7)
   integer :: nxl(6)
   integer :: ip_ion, ip_el, ip_pr
   logical, allocatable, dimension(:) :: mobilebool
   !=================================
   ! Array that defines if species is mobile.
   ! We should consider setting it in the input file
   allocate(mobilebool(nsp), source=.true.)
   !=================
   call set_uniform_yz_distrib(nyh_in, 7)
   !===========================
   xp_min = xmin
   xp_max = xmax
   !=============
   ! weights in central layer are wgh_sp(1:3)
   nxl = 0
   !=============================
   ! Multispecies  designed for nsp >2
   ! Particles distribution np_per_yc(1:3) and np_per_xc(1:3) central target
   ! (El+Z1+ Z2
   !                        np_per_yc(5:6) and np_per_xc(5:6)
   !                        contaminants (El+Z3) in front and rear layers
   !                        if contaminants: nsp=4(Z3/= Z2) if nps=3 Z3=Z2
   !=============================================
   !WARNING  nm_per_cell(1:3) and mp_per_cell(5:6) have to be set in a way global
   !zero charge per cell is assured
   ! In central layer:
   ! Electron number mp_per_cell(1)= Z1*mp_per_cell(2) +Z2*mp_per_cell(3)
   !=============================
   xtot = 0.0
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   xfsh = xf0 + lpx(7)
   targ_in = xfsh
   targ_end = targ_in + xtot
   ! Species x-distribution
   !=  np_per_xc(1:3) electrons, Z1 and Z2 ions in central layer lpx(2)+lpx(3)
   !=  np_per_xc(5:6) electrons and Z3-ions front and rear side contaminants (same
   !composition) If nsp=3 Z3=Z2
   !======================================
   nptx_loc(1:3) = (nxl(2)+nxl(3))*np_per_xc(1:3)
   nptx_loc(4:5) = nxl(1)*np_per_xc(5:6)
   nptx_loc(6:7) = nxl(5)*np_per_xc(5:6)
   !============ nptx(nsp)  distribution
   nptx(1) = nptx_loc(1) + nptx_loc(4) + nptx_loc(6) !electrons
   nptx(2) = nptx_loc(2) !Z1-A1 species
   nptx(3) = nptx_loc(3) !Z2-A2 species 
   nptx(4) = nptx_loc(5) + nptx_loc(7) !Z3-A3  species in nxl(1) and nxl(5) layer
   nptx_max = maxval(nptx_loc(1:7))
   !=======================
   allocate (xpt(nptx_max,7))
   allocate (wghpt(nptx_max,7))

   allocate (loc_xpt(nptx_max,7))
   allocate (loc_wghx(nptx_max,7))
   wghpt(1:nptx_max, 1:7) = 1.
   !=============================
   ! nsp ordering: electrons+ Z1 ions + Z2 ions
   ! first and last layers: electrons and Z3 (protons) ions
   loc_imax(imodx, 1:7) = nptx_loc(1:7)
   nps_loc = 0
   !==========================================
   wgh_sp(1) = j0_norm*ratio_mpc(5)
   wgh_sp(2) = j0_norm*ratio_mpc(6)/real(ion_min(nsp-1), dp)
   wgh_sp(3:5) = j0_norm
   !===================================
   if (nxl(1)>0) then !first contaminant layer El+Z(nsp-1)
    do ic = 4, 5
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      xpt(i, ic) = xfsh + lpx(1)*uu
      wghpt(i, ic) = np1*wgh_sp(ic) !pp weights using mp_per_cell(5:6)
     end do
    end do
    xfsh = xfsh + lpx(1)
    !=========== Distributes on x-MPI tasks
    do ic = 4, 5
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    !========================
    !========================
    p = imodx
    l = imody
    ip = imodz
    ! Counts particles (e+Z3 ions)

    nps_loc(1) = nps_loc(1) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip, &
      4)
    nps_loc(nsp) = nps_loc(nsp) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax &
      (ip, 5)

   end if
   !====================
   ! Electrons and (Z1+Z2)_ions : central layer with lpx(2) preplasma
   ! x distribution
   !====================
   np1_loc = 0.005
   if (np1 > 0.0) np1_loc = np1
   l_inv = log(1./np1_loc)
   do ic = 1, 3
    n_peak = nptx_loc(ic) !central target
    do i = 1, n_peak
     uu = (real(i,dp)-0.5)/real(n_peak, dp)
     xpt(i, ic) = xfsh + (lpx(2)+lpx(3))*uu
     wghpt(i, ic) = wgh_sp(ic) !weights usig mp_per_cell(1:3)  (El, Z1, Z2) ions
    end do
    if (nxl(2)>0) then
     n_peak = nxl(2)*np_per_xc(ic) !preplasma
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      wghpt(i, ic) = wghpt(i, ic)*np1_loc*exp(uu*l_inv)
     end do
    end if
   end do
   xfsh = xfsh + lpx(3) + lpx(2)
   !=============================
   ! Charge equilibria mp_per_cell(4)*Z1 +mp_per_cell(5)*Z3= mp_per_cell(1)
   !==========================
   !=========== Distributes on x-MPI tasks
   do ic = 1, 3
    i1 = 0
    do i = 1, nptx_loc(ic)
     if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. xpt(i,ic)<loc_xgrid( &
       imodx)%gmax) then
      i1 = i1 + 1
      loc_xpt(i1, ic) = xpt(i, ic)
      loc_wghx(i1, ic) = wghpt(i, ic)
     end if
    end do
    loc_imax(imodx, ic) = i1
   end do
   p = imodx
   l = imody
   ip = imodz

   nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
     1)
   nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
     2)
   nps_loc(3) = nps_loc(3) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip, &
     3)
   !============================
   if (lpx(5)>0.0) then
    do ic = 6, 7
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      xpt(i, ic) = xfsh + lpx(5)*(real(i,dp)-0.5)/real(n_peak, dp)
      wghpt(i, ic) = np2*wgh_sp(ic-5) !weights using mp_per_cell(5:6) as in layer lpx(1)
     end do
    end do
    xfsh = xfsh + lpx(5)
    !===============================
    do ic = 6, 7
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
      6)
    nps_loc(nsp) = nps_loc(nsp) + loc_imax(p, 7)*loc_jmax(l, 7)*loc_kmax &
      (ip, 7)

   end if
   !======================
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1,&
    mem_psize, mobilebool)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   !============
   ! The first electron-Z3 layer
   if (nxl(1)>0) then
    p = 0
    i2 = loc_imax(imodx, 4)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 4, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(nsp), t0_pl(nsp), unit_charge(nsp), p,&
      5, i2, ip_pr)
    !Z3 for nsp=4  Z3=Z2 for nsp=3
   end if
   !=========================
   ! The second solid electron-Z1-Z2 layer
   p = ip_el
   i2 = loc_imax(imodx, 1)
   call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, &
    i2, ip_el)
   p = 0
   i2 = loc_imax(imodx, 2)
   call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, &
    i2, ip_ion)
   p = 0
   if (nsp==3) p = ip_pr
   i2 = loc_imax(imodx, 3)
   call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 3, &
    i2, ip_ion)
   !============
   ! The third electron-proton layer
   !=========================
   if (nxl(5)>0.0) then
    p = ip_el
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 6, &
      i2, ip)
    p = ip_pr
    if (nsp==3) p = ip_ion
    i2 = loc_imax(imodx, 7)
    call pspecies_distribute(spec_in(nsp), t0_pl(nsp), unit_charge(nsp), p, &
      7, i2, ip)
   end if

   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

   !============
  end subroutine
  !=====================
  subroutine multi_layer_threesp_target_old(spec_in, spec_aux_in, nyh_in, xf0)

   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip
   integer :: l, i, i1, i2, ic
   integer :: n_peak, nptx_loc(7)
   integer :: npmax, nps_loc(4)
   real (dp) :: uu, xp_min, xp_max, np1_loc
   real (dp) :: xfsh, l_inv, wgh_sp(7)
   integer :: nxl(6)
   integer :: ip_ion, ip_el, ip_pr
   !=================
   call set_uniform_yz_distrib(nyh_in, 7)
   !===========================
   xp_min = xmin
   xp_max = xmax
   !=============
   ! weights in central layer are wgh_sp(1:3)
   nxl = 0
   !=============================
   ! Multispecies  designed for nsp >2
   ! Particles distribution np_per_yc(1:3) and np_per_xc(1:3) central target
   ! (El+Z1+ Z2
   !                        np_per_yc(5:6) and np_per_xc(5:6)
   !                        contaminants (El+Z3) in front and rear layers
   !                        if contaminants: nsp=4(Z3/= Z2) if nps=3 Z3=Z2
   !=============================================
   !WARNING  nm_per_cell(1:3) and mp_per_cell(5:6) have to be set in a way global
   !zero charge per cell is assured
   ! In central layer:
   ! Electron number mp_per_cell(1)= Z1*mp_per_cell(2) +Z2*mp_per_cell(3)
   !=============================
   xtot = 0.0
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   xfsh = xf0 + lpx(7)
   targ_in = xfsh
   targ_end = targ_in + xtot
   ! Species x-distribution
   !=  np_per_xc(1:3) electrons, Z1 and Z2 ions in central layer lpx(2)+lpx(3)
   !=  np_per_xc(5:6) electrons and Z3-ions front and rear side contaminants (same
   !composition) If nsp=3 Z3=Z2
   !======================================
   nptx_loc(1:3) = (nxl(2)+nxl(3))*np_per_xc(1:3)
   nptx_loc(4:5) = nxl(1)*np_per_xc(5:6)
   nptx_loc(6:7) = nxl(5)*np_per_xc(5:6)
   !============ nptx(nsp)  distribution
   nptx(1) = nptx_loc(1) + nptx_loc(4) + nptx_loc(6) !electrons
   nptx(2) = nptx_loc(2) !Z1-A1 species
   nptx(3) = nptx_loc(3) !Z2-A2 species 
   nptx(4) = nptx_loc(5) + nptx_loc(7) !Z3-A3  species in nxl(1) and nxl(5) layer
   nptx_max = maxval(nptx_loc(1:7))
   !=======================
   allocate (xpt(nptx_max,7))
   allocate (wghpt(nptx_max,7))

   allocate (loc_xpt(nptx_max,7))
   allocate (loc_wghx(nptx_max,7))
   wghpt(1:nptx_max, 1:7) = 1.
   !=============================
   ! nsp ordering: electrons+ Z1 ions + Z2 ions
   ! first and last layers: electrons and Z3 (protons) ions
   loc_imax(imodx, 1:7) = nptx_loc(1:7)
   nps_loc = 0
   !==========================================
   wgh_sp(1) = j0_norm*ratio_mpc(5)
   wgh_sp(2) = j0_norm*ratio_mpc(6)/real(ion_min(nsp-1), dp)
   wgh_sp(3:5) = j0_norm
   !===================================
   if (nxl(1)>0) then !first contaminant layer El+Z(nsp-1)
    do ic = 4, 5
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      xpt(i, ic) = xfsh + lpx(1)*uu
      wghpt(i, ic) = np1*wgh_sp(ic) !pp weights using mp_per_cell(5:6)
     end do
    end do
    xfsh = xfsh + lpx(1)
    !=========== Distributes on x-MPI tasks
    do ic = 4, 5
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i,ic)>=loc_xgrid(imodx)%gmin .and. &
        xpt(i,ic)<loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    !========================
    !========================
    p = imodx
    l = imody
    ip = imodz
    ! Counts particles (e+Z3 ions)

    nps_loc(1) = nps_loc(1) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip, &
      4)
    nps_loc(nsp) = nps_loc(nsp) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax &
      (ip, 5)

   end if
   !====================
   ! Electrons and (Z1+Z2)_ions : central layer with lpx(2) preplasma
   ! x distribution
   !====================
   np1_loc = 0.005
   if (np1 > 0.0) np1_loc = np1
   l_inv = log(1./np1_loc)
   do ic = 1, 3
    n_peak = nptx_loc(ic) !central target
    do i = 1, n_peak
     uu = (real(i,dp)-0.5)/real(n_peak, dp)
     xpt(i, ic) = xfsh + (lpx(2)+lpx(3))*uu
     wghpt(i, ic) = wgh_sp(ic) !weights usig mp_per_cell(1:3)  (El, Z1, Z2) ions
    end do
    if (nxl(2)>0) then
     n_peak = nxl(2)*np_per_xc(ic) !preplasma
     do i = 1, n_peak
      uu = (real(i,dp)-0.5)/real(n_peak, dp)
      wghpt(i, ic) = wghpt(i, ic)*np1_loc*exp(uu*l_inv)
     end do
    end if
   end do
   xfsh = xfsh + lpx(3) + lpx(2)
   !=============================
   ! Charge equilibria mp_per_cell(4)*Z1 +mp_per_cell(5)*Z3= mp_per_cell(1)
   !==========================
   !=========== Distributes on x-MPI tasks
   do ic = 1, 3
    i1 = 0
    do i = 1, nptx_loc(ic)
     if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. xpt(i, ic) < loc_xgrid( &
         imodx)%gmax) then
      i1 = i1 + 1
      loc_xpt(i1, ic) = xpt(i, ic)
      loc_wghx(i1, ic) = wghpt(i, ic)
     end if
    end do
    loc_imax(imodx, ic) = i1
   end do
   p = imodx
   l = imody
   ip = imodz

   nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
                                                                    1)
   nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
     2)
   nps_loc(3) = nps_loc(3) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip, &
     3)
   !============================
   if (lpx(5)>0.0) then
    do ic = 6, 7
     n_peak = nptx_loc(ic)
     do i = 1, n_peak
      xpt(i, ic) = xfsh + lpx(5)*(real(i,dp)-0.5)/real(n_peak, dp)
      wghpt(i, ic) = np2*wgh_sp(ic-5) !weights using mp_per_cell(5:6) as in layer lpx(1)
     end do
    end do
    xfsh = xfsh + lpx(5)
    !===============================
    do ic = 6, 7
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. &
          xpt(i, ic) < loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
      6)
    nps_loc(nsp) = nps_loc(nsp) + loc_imax(p, 7)*loc_jmax(l, 7)*loc_kmax &
      (ip, 7)

   end if
   !======================
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   !============
   ! The first electron-Z3 layer
   if (nxl(1)>0) then
    p = 0
    i2 = loc_imax(imodx, 4)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 4, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(nsp), t0_pl(nsp), unit_charge(nsp), p, &
      5, i2, ip_pr)
    !Z3 for nsp=4  Z3=Z2 for nsp=3
   end if
   !=========================
   ! The second solid electron-Z1-Z2 layer
   p = ip_el
   i2 = loc_imax(imodx, 1)
   call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, i2, &
     ip_el)
   p = 0
   i2 = loc_imax(imodx, 2)
   call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, i2, &
     ip_ion)
   p = 0
   if (nsp==3) p = ip_pr
   i2 = loc_imax(imodx, 3)
   call pspecies_distribute(spec_in(3), t0_pl(3), unit_charge(3), p, 3, i2, &
     ip_ion)
   !============
   ! The third electron-proton layer
   !=========================
   if (nxl(5) > 0.0) then
    p = ip_el
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 6, &
      i2, ip)
    p = ip_pr
    if (nsp==3) p = ip_ion
    i2 = loc_imax(imodx, 7)
    call pspecies_distribute(spec_in(nsp), t0_pl(nsp), unit_charge(nsp), p, &
      7, i2, ip)
   end if

   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

   !============
  end subroutine
  !=====================
  subroutine one_layer_nano_wires_new(spec_in, spec_aux_in, nyh_in, xf0)

   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip
   integer :: l, i, i1, i2, ic
   integer :: np_per_zcell(6), n_peak
   integer :: nptx_loc(8)
   integer :: npty_layer(8), npyc(8), npty_ne, nptz_ne
   integer :: npmax, nps_loc(4)
   real (dp) :: uu, yy, dxip, dpy
   real (dp) :: zp_min, zp_max, yp_min, yp_max, xp_min, xp_max
   real (dp) :: xfsh, dlpy, tot_lpy, loc_ymp
   integer :: z2, nxl(6), nyl1, nlpy, nholes
   integer :: ip_ion, ip_el, ip_pr, nwires
   real (dp), allocatable :: wy(:, :), wz(:, :), wyz(:, :, :)
   logical, allocatable, dimension(:) :: mobilebool
   !=================
   !++++++++++++++++ WARNING
   ! ONLY layers (3) and (4) n_over_nc, np2*n_over_nc layer (5)
   !============================
   !=================================
   ! Array that defines if species is mobile.
   ! We should consider setting it in the input file
   allocate(mobilebool(nsp), source=.true.)
   xp_min = xmin
   xp_max = xmax
   np_per_zcell(1:6) = 1
   !============================
   nxl = 0
   z2 = ion_min(nsp-1)
   !========= gridding the transverese target size
   nyl1 = 1 + ny/2 - nyh_in/2 !=1 if nyh_in=ny
   yp_min = ymin_t
   yp_max = ymax_t

   dlpy = lpy(1) !nanowire (y,z) thickness
   tot_lpy = dlpy + lpy(2) !distance among elements (void+nanowire)`
   nwires = nint((yp_max-yp_min)/tot_lpy) !numbers of lpy elements
   nlpy = nint(dy_inv*dlpy) ! cell numbers in dlpy
   nholes = nint(dy_inv*lpy(2)) ! cell number in the lpy(2) interwire region
   if (pe0) then
    write (6, '(a18,i6)') ' Nanowires number ', nwires
    write (6, '(a23,i6)') ' Grid-points per nanow ', nlpy
   end if
   !=============================
   ! Multispecies
   !=============================
   npty = maxval(np_per_yc(1:6))
   npty = nyh_in*npty
   nptz = 1
   if (ndim==3) then
    np_per_zcell(1:6) = np_per_yc(1:6)
    zp_min = zmin_t !-Lz
    zp_max = zmax_t !+Lz
    nptz = maxval(np_per_zc(1:6))
    nptz = nyh_in*nptz
   end if
   allocate (ypt(npty+1,8))
   allocate (zpt(nptz+1,8))
   allocate (wy(npty+1,8))
   allocate (wz(nptz+1,8))
   allocate (wyz(npty+1,nptz+1,8))
   ypt = 0.
   zpt = 0.
   wy = 1.
   wz = 1.
   wyz = 1.
   !==================
   allocate (loc_jmax(0:npe_yloc-1,1:8))
   allocate (loc_kmax(0:npe_zloc-1,1:8))
   allocate (loc_imax(0:npe_xloc-1,1:8))
   !====================
   !layers in y-z transverse coordinates
   !====================================
   npyc(1:2) = np_per_yc(1:2) !layer of nano_wires electron+Z1_ion
   npyc(3:4) = np_per_yc(1:2) !layer of inter wire plasma of np1 density layer[1:4] of x-length=lpx(3)
   npyc(5:6) = np_per_yc(3:4) !bulk of electron-Z1_ion      x-length lpx(4)        
   npyc(7:8) = np_per_yc(5:6) ! coating of electron-Z2_ion      x-length lpx(5)        
   nptz_ne = 1
   if (nwires>2) then
    do ic = 1, 2
     npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
     i2 = 0
     loc_ymp = yp_min + lpy(2)
     do i1 = 1, nwires !layers of lpy=dlpy(1+rat) length
      dpy = dlpy/real(npty_ne, dp)
      do i = 1, npty_ne
       ypt(i+i2, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
      end do
      i2 = i2 + npty_ne
      loc_ymp = loc_ymp + tot_lpy
     end do
     npty_layer(ic) = i2
    end do
    do ic = 3, 4
     npty_ne = nholes*npyc(ic) !number of yp points in a lpy(2) layer
     i2 = 0
     loc_ymp = yp_min
     do i1 = 1, nwires
      dpy = lpy(2)/real(npty_ne, dp)
      do i = 1, npty_ne
       ypt(i+i2, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
      end do
      i2 = i2 + npty_ne
      loc_ymp = loc_ymp + tot_lpy
     end do
     npty_layer(ic) = i2
     !===========================
    end do
    if (lpx(4)<=0) then
     do ic = 7, 8
      npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
      i2 = 0
      loc_ymp = yp_min + lpy(2)
      do i1 = 1, nwires !layers of lpy=dlpy(1+rat) length
       dpy = dlpy/real(npty_ne, dp)
       do i = 1, npty_ne
        ypt(i+i2, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
       end do
       i2 = i2 + npty_ne
       loc_ymp = loc_ymp + tot_lpy
      end do
      npty_layer(ic) = i2
     end do
    end if
   else !two nanowires filled with n1_over_nc (el+Z1) plasma
    do ic = 1, 2
     npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
     i2 = 0
     loc_ymp = -0.5*tot_lpy
     dpy = dlpy/real(npty_ne, dp)
     do i = 1, npty_ne
      ypt(i+i2, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
     end do
     loc_ymp = loc_ymp + lpy(2) !first layer
     i2 = i2 + npty_ne
     !===========================
     do i = 1, npty_ne
      ypt(i+i2, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
     end do
     i2 = i2 + npty_ne
     !====================
     npty_layer(ic) = i2
    end do
    do ic = 3, 4
     npty_ne = nholes*npyc(ic) !number of yp points in a lpy(2) layer
     loc_ymp = -0.5*lpy(2)
     dpy = lpy(2)/real(npty_ne, dp)
     do i = 1, npty_ne
      ypt(i, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
     end do
     npty_layer(ic) = npty_ne
     !===========================
    end do
    if (lpx(4)<=0) then
     do ic = 7, 8
      npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
      i2 = 0
      loc_ymp = -0.5*tot_lpy
      dpy = dlpy/real(npty_ne, dp)
      do i = 1, npty_ne
       ypt(i+i2, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
      end do
      loc_ymp = loc_ymp + lpy(2) !first layer
      i2 = i2 + npty_ne
      !===========================
      do i = 1, npty_ne
       ypt(i+i2, ic) = loc_ymp + dpy*(real(i,dp)-0.1)
      end do
      i2 = i2 + npty_ne
      !====================
      npty_layer(ic) = i2
     end do
    end if
   end if
   !============= Uniform y-z distribution in layers [5-8]
   do ic = 5, 6
    npty_layer(ic) = nyh_in*npyc(ic)
    npty_ne = npty_layer(ic)
    dpy = (yp_max-yp_min)/real(npty_ne, dp)
    do i = 1, npty_ne
     ypt(i, ic) = yp_min + dpy*(real(i,dp)-0.5)
    end do
   end do
   if (lpx(4)>0) then
    do ic = 7, 8
     npty_layer(ic) = nyh_in*npyc(ic)
     npty_ne = npty_layer(ic)
     dpy = (yp_max-yp_min)/real(npty_ne, dp)
     do i = 1, npty_ne
      ypt(i, ic) = yp_min + dpy*(real(i,dp)-0.5)
     end do
    end do
   end if
   !========= For all (y,z) coordinates
   do ic = 1, 8
    npty_ne = npty_layer(ic)
    if (stretch) then
     yy = str_ygrid%smin
     if (yy>yp_min) then
      dpy = dyi/real(npyc(ic), dp)
      i1 = (str_ygrid%sind(1)-nyl1+1)*npyc(ic)
      i2 = npty_ne - i1
      do i = 1, i1
       dxip = dpy*(real(i-i1,dp)-0.5)
       ypt(i, ic) = str_ygrid%smin + l_s*tan(dxip)
       wy(i, ic) = 1./(cos(dxip)*cos(dxip))
      end do
      dxip = dy/real(npyc(ic), dp)
      do i = i1 + 1, i2
       ypt(i, ic) = str_ygrid%smin + dxip*(real(i-i1,dp)-0.5)
      end do
      do i = i2 + 1, npty_ne
       dxip = dpy*(real(i-i2,dp)-0.5)
       ypt(i, ic) = str_ygrid%smax + l_s*tan(dxip)
       wy(i, ic) = 1./(cos(dxip)*cos(dxip))
      end do
     end if
    end if
    !============= end stretching correction
    nptz_ne = 1
    if (ndim==3) then
     zpt(1:npty_ne, ic) = ypt(1:npty_ne, ic)
     wz(1:npty_ne, ic) = wy(1:npty_ne, ic)
     nptz_ne = npty_ne
    end if
    call set_pgrid_ind(npty_ne, nptz_ne, ic)
   end do
   !=================== y-z data on local arrays
   loc_npty(1:8) = loc_jmax(imody, 1:8)
   loc_nptz(1:8) = loc_kmax(imodz, 1:8)
   npty_ne = 1
   nptz_ne = 1
   npty_ne = maxval(loc_npty(1:8))
   nptz_ne = maxval(loc_nptz(1:8))
   !======================
   allocate (loc_wghyz(npty_ne,nptz_ne,8))
   allocate (loc_ypt(npty_ne,8))
   allocate (loc_zpt(nptz_ne,8))
   loc_wghyz = 1.
   call mpi_yz_part_distrib(8, loc_npty, loc_nptz, npty_layer, &
     npty_layer, ymin_t, zmin_t, wyz)
   !=======================
   !Longitudinal layer distribution
   !===========================
   xtot = 0.0
   lpx(1:2) = 0.0 !only layers 3-4-5
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   xfsh = xf0
   targ_in = xfsh
   targ_end = targ_in + xtot
   ! Input particles
   !====np_per_xc(1:2) electrons and Z1 ions in the nanowires target +
   !internanow-plasma => lpx(3)
   !====np_per_xc(3:4) electrons and Z1 ions in bulk layer
   !=== np_per_xc(5:6) electrons and Z2=proton in contaminant layer
   !  Particles grid ordering
   !  only nxl(3) nxl(4) and nxl(5) layers activated
   nptx_loc(1:2) = nxl(3)*np_per_xc(1:2) !inter-wire  electrons+Z1-ion plasma
   nptx_loc(3:4) = nptx_loc(1:2) !nanowires electron-Z1 ions
   nptx_loc(5:6) = nxl(4)*np_per_xc(3:4) !bulk layer electrons +Z1 ions
   nptx_loc(7:8) = nxl(5)*np_per_xc(5:6) !contaminant electrons +Z2 ions (proton)

   nptx_max = maxval(nptx_loc(1:8))
   !=======================
   allocate (xpt(nptx_max,8))
   allocate (wghpt(nptx_max,8))

   allocate (loc_xpt(nptx_max,8))
   allocate (loc_wghx(nptx_max,8))
   wghpt(1:nptx_max, 1:8) = 1.
   !=================
   !========================
   loc_imax(imodx, 1:8) = nptx_loc(1:8)
   nps_loc(1:nsp) = 0
   ! Nanowires x-layer: electrons and Z1-ions
   ! nanowires density is the reference density
   if (nxl(3)>0) then
    do ic = 1, 2
     n_peak = nptx_loc(ic)
     if (n_peak>0) then
      do i = 1, n_peak
       uu = (real(i,dp)-0.5)/real(n_peak, dp)
       xpt(i, ic) = xfsh + lpx(3)*uu
       wghpt(i, ic) = ratio_mpc(ic)*j0_norm
       xpt(i, ic+2) = xpt(i, ic)
       wghpt(i, ic+2) = np1*wghpt(i, ic) !inter-wire plasma (or vacuum)
      end do
     end if
     !========================= np1>0 a low density  interwire plasma
    end do
    xfsh = xfsh + lpx(3)
    !============= first x-layer distributed on locx mpi tasks
    do ic = 1, 2
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. &
          xpt(i, ic) < loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
       loc_xpt(i1, ic+2) = xpt(i, ic+2)
       loc_wghx(i1, ic+2) = wghpt(i, ic+2)
      end if
     end do
     loc_imax(imodx, ic) = i1
     loc_imax(imodx, ic+2) = i1
    end do
    !========================
    p = imodx
    l = imody
    ip = imodz
    nps_loc = 0
    ! Counts particles

    nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
      1)
    nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
      2)
    if (np1>0.0) then
     nps_loc(1) = nps_loc(1) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip &
       , 3)
     nps_loc(2) = nps_loc(2) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip &
       , 4)
    end if
   end if
   !------------------------------
   !  Electrons and Z1_ions: bulk layer 
   !     x distribution. Density given by the particle density mpc(3:4)
   !====================
   if (nxl(4)>0) then
    do ic = 5, 6
     n_peak = nptx_loc(ic)
     if (n_peak>0) then
      do i = 1, n_peak
       xpt(i, ic) = xfsh + lpx(4)*(real(i,dp)-0.5)/real(n_peak, dp)
       uu = j0_norm*ratio_mpc(ic-2)
       wghpt(i, ic) = uu
      end do
     end if
    end do
    xfsh = xfsh + lpx(4)
    do ic = 5, 6
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. &
          xpt(i, ic) < loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax(ip, &
      5)
    nps_loc(2) = nps_loc(2) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
      6)
   end if
   !  Electrons and Z3_ions contaminants
   !     x distribution density given by np2
   !====================
   if (nxl(5)>0) then
    do ic = 7, 8
     n_peak = nptx_loc(ic)
     if (n_peak>0) then
      do i = 1, n_peak
       xpt(i, ic) = xfsh + lpx(5)*(real(i,dp)-0.5)/real(n_peak, dp)
       uu = j0_norm*ratio_mpc(ic-2)
       wghpt(i, ic) = uu*np2
      end do
     end if
    end do
    ic = 8
    n_peak = nptx_loc(ic)
    wghpt(1:n_peak, ic) = wghpt(1:n_peak, ic)/real(ion_min(nsp-1), dp)
    xfsh = xfsh + lpx(5)
    !===============
    do ic = 7, 8
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. &
          xpt(i, ic) < loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1 - 1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 7)*loc_jmax(l, 7)*loc_kmax(ip, &
      7)
    nps_loc(nsp) = nps_loc(nsp) + loc_imax(p, 8)*loc_jmax(l, 8)*loc_kmax &
      (ip, 8)
   end if
   !==============END target x-distribution
   !==============
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, &
    mem_psize, mobilebool)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   ! The first electron-Z1-ions nanowires layer
   if (nxl(3)>0) then
    i2 = loc_imax(imodx, 1)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, &
      i2, ip_el)
    i2 = loc_imax(imodx, 2)
    call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, &
      i2, ip_ion)
    if (np1>0.0) then
     i2 = loc_imax(imodx, 3)
     call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 3, &
       i2, ip_el)
     i2 = loc_imax(imodx, 4)
     call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 4, &
       i2, ip_ion)
    end if
   end if
   !=========================
   ! The second electron-ion solid layer with Z1 A1 ion element
   if (nxl(4)>0) then
    p = ip_el
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 5, &
      i2, ip_el)
    p = ip_ion
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 6, &
      i2, ip_ion)
   end if
   !============
   ! The contaminant electron-ion solid layer Z3=proton ion element
   if (nxl(5)>0) then
    p = ip_el
    i2 = loc_imax(imodx, 7)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 7, &
      i2, ip_el)

    p = 0
    i2 = loc_imax(imodx, 8)
    call pspecies_distribute(spec_in(nsp), t0_pl(nsp), unit_charge(nsp), p,&
      8, i2, ip_ion)
   end if
   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

  end subroutine
  !============
  subroutine one_layer_nano_wires_old(spec_in, spec_aux_in, nyh_in, xf0)

   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p, ip
   integer :: l, i, i1, i2, ic
   integer :: np_per_zcell(6), n_peak
   integer :: nptx_loc(8)
   integer :: npty_layer(8), npyc(8), npty_ne, nptz_ne
   integer :: npmax, nps_loc(4)
   real(dp) :: uu, yy, dxip, dpy
   real(dp) :: zp_min, zp_max, yp_min, yp_max, xp_min, xp_max
   real(dp) :: xfsh, dlpy, tot_lpy, loc_ymp
   integer :: z2, nxl(6), nyl1, nlpy, nholes
   integer :: ip_ion, ip_el, ip_pr, nwires
   real(dp), allocatable :: wy(:, :), wz(:, :), wyz(:, :, :)
   !=================
   !++++++++++++++++ WARNING
   ! ONLY layers (3) and (4) n_over_nc, np2*n_over_nc layer (5)
   !============================
   xp_min = xmin
   xp_max = xmax
   np_per_zcell(1:6) = 1
   !============================
   nxl = 0
   z2 = ion_min(nsp - 1)
   !========= gridding the transverese target size
   nyl1 = 1 + ny/2 - nyh_in/2 !=1 if nyh_in=ny
   yp_min = ymin_t
   yp_max = ymax_t

   dlpy = lpy(1) !nanowire (y,z) thickness
   tot_lpy = dlpy + lpy(2) !distance among elements (void+nanowire)`
   nwires = nint((yp_max - yp_min)/tot_lpy) !numbers of lpy elements
   nlpy = nint(dy_inv*dlpy) ! cell numbers in dlpy
   nholes = nint(dy_inv*lpy(2)) ! cell number in the lpy(2) interwire region
   if (pe0) then
    write (6, '(a18,i6)') ' Nanowires number ', nwires
    write (6, '(a23,i6)') ' Grid-points per nanow ', nlpy
   end if
   !=============================
   ! Multispecies
   !=============================
   npty = maxval(np_per_yc(1:6))
   npty = nyh_in*npty
   nptz = 1
   if (ndim == 3) then
    np_per_zcell(1:6) = np_per_yc(1:6)
    zp_min = zmin_t !-Lz
    zp_max = zmax_t !+Lz
    nptz = maxval(np_per_zc(1:6))
    nptz = nyh_in*nptz
   end if
   allocate (ypt(npty + 1, 8))
   allocate (zpt(nptz + 1, 8))
   allocate (wy(npty + 1, 8))
   allocate (wz(nptz + 1, 8))
   allocate (wyz(npty + 1, nptz + 1, 8))
   ypt = 0.
   zpt = 0.
   wy = 1.
   wz = 1.
   wyz = 1.
   !==================
   allocate (loc_jmax(0:npe_yloc - 1, 1:8))
   allocate (loc_kmax(0:npe_zloc - 1, 1:8))
   allocate (loc_imax(0:npe_xloc - 1, 1:8))
   !====================
   !layers in y-z transverse coordinates
   !====================================
   npyc(1:2) = np_per_yc(1:2) !layer of nano_wires electron+Z1_ion
   npyc(3:4) = np_per_yc(1:2) !layer of inter wire plasma of np1 density layer[1:4] of x-length=lpx(3)
   npyc(5:6) = np_per_yc(3:4) !bulk of electron-Z1_ion      x-length lpx(4)
   npyc(7:8) = np_per_yc(5:6) ! coating of electron-Z2_ion      x-length lpx(5)
   nptz_ne = 1
   if (nwires > 2) then
    do ic = 1, 2
     npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
     i2 = 0
     loc_ymp = yp_min + lpy(2)
     do i1 = 1, nwires !layers of lpy=dlpy(1+rat) length
      dpy = dlpy/real(npty_ne, dp)
      do i = 1, npty_ne
       ypt(i + i2, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
      end do
      i2 = i2 + npty_ne
      loc_ymp = loc_ymp + tot_lpy
     end do
     npty_layer(ic) = i2
    end do
    do ic = 3, 4
     npty_ne = nholes*npyc(ic) !number of yp points in a lpy(2) layer
     i2 = 0
     loc_ymp = yp_min
     do i1 = 1, nwires
      dpy = lpy(2)/real(npty_ne, dp)
      do i = 1, npty_ne
       ypt(i + i2, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
      end do
      i2 = i2 + npty_ne
      loc_ymp = loc_ymp + tot_lpy
     end do
     npty_layer(ic) = i2
     !===========================
    end do
    if (lpx(4) <= 0) then
     do ic = 7, 8
      npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
      i2 = 0
      loc_ymp = yp_min + lpy(2)
      do i1 = 1, nwires !layers of lpy=dlpy(1+rat) length
       dpy = dlpy/real(npty_ne, dp)
       do i = 1, npty_ne
        ypt(i + i2, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
       end do
       i2 = i2 + npty_ne
       loc_ymp = loc_ymp + tot_lpy
      end do
      npty_layer(ic) = i2
     end do
    end if
   else !two nanowires filled with n1_over_nc (el+Z1) plasma
    do ic = 1, 2
     npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
     i2 = 0
     loc_ymp = -0.5*tot_lpy
     dpy = dlpy/real(npty_ne, dp)
     do i = 1, npty_ne
      ypt(i + i2, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
     end do
     loc_ymp = loc_ymp + lpy(2) !first layer
     i2 = i2 + npty_ne
     !===========================
     do i = 1, npty_ne
      ypt(i + i2, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
     end do
     i2 = i2 + npty_ne
     !====================
     npty_layer(ic) = i2
    end do
    do ic = 3, 4
     npty_ne = nholes*npyc(ic) !number of yp points in a lpy(2) layer
     loc_ymp = -0.5*lpy(2)
     dpy = lpy(2)/real(npty_ne, dp)
     do i = 1, npty_ne
      ypt(i, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
     end do
     npty_layer(ic) = npty_ne
     !===========================
    end do
    if (lpx(4) <= 0) then
     do ic = 7, 8
      npty_ne = nlpy*npyc(ic) !number of yp points in a dlpy layer
      i2 = 0
      loc_ymp = -0.5*tot_lpy
      dpy = dlpy/real(npty_ne, dp)
      do i = 1, npty_ne
       ypt(i + i2, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
      end do
      loc_ymp = loc_ymp + lpy(2) !first layer
      i2 = i2 + npty_ne
      !===========================
      do i = 1, npty_ne
       ypt(i + i2, ic) = loc_ymp + dpy*(real(i, dp) - 0.1)
      end do
      i2 = i2 + npty_ne
      !====================
      npty_layer(ic) = i2
     end do
    end if
   end if
   !============= Uniform y-z distribution in layers [5-8]
   do ic = 5, 6
    npty_layer(ic) = nyh_in*npyc(ic)
    npty_ne = npty_layer(ic)
    dpy = (yp_max - yp_min)/real(npty_ne, dp)
    do i = 1, npty_ne
     ypt(i, ic) = yp_min + dpy*(real(i, dp) - 0.5)
    end do
   end do
   if (lpx(4) > 0) then
    do ic = 7, 8
     npty_layer(ic) = nyh_in*npyc(ic)
     npty_ne = npty_layer(ic)
     dpy = (yp_max - yp_min)/real(npty_ne, dp)
     do i = 1, npty_ne
      ypt(i, ic) = yp_min + dpy*(real(i, dp) - 0.5)
     end do
    end do
   end if
   !========= For all (y,z) coordinates
   do ic = 1, 8
    npty_ne = npty_layer(ic)
    if (stretch) then
     yy = str_ygrid%smin
     if (yy > yp_min) then
      dpy = dyi/real(npyc(ic), dp)
      i1 = (str_ygrid%sind(1) - nyl1 + 1)*npyc(ic)
      i2 = npty_ne - i1
      do i = 1, i1
       dxip = dpy*(real(i - i1, dp) - 0.5)
       ypt(i, ic) = str_ygrid%smin + l_s*tan(dxip)
       wy(i, ic) = 1./(cos(dxip)*cos(dxip))
      end do
      dxip = dy/real(npyc(ic), dp)
      do i = i1 + 1, i2
       ypt(i, ic) = str_ygrid%smin + dxip*(real(i - i1, dp) - 0.5)
      end do
      do i = i2 + 1, npty_ne
       dxip = dpy*(real(i - i2, dp) - 0.5)
       ypt(i, ic) = str_ygrid%smax + l_s*tan(dxip)
       wy(i, ic) = 1./(cos(dxip)*cos(dxip))
      end do
     end if
    end if
    !============= end stretching correction
    nptz_ne = 1
    if (ndim == 3) then
     zpt(1:npty_ne, ic) = ypt(1:npty_ne, ic)
     wz(1:npty_ne, ic) = wy(1:npty_ne, ic)
     nptz_ne = npty_ne
    end if
    call set_pgrid_ind(npty_ne, nptz_ne, ic)
   end do
   !=================== y-z data on local arrays
   loc_npty(1:8) = loc_jmax(imody, 1:8)
   loc_nptz(1:8) = loc_kmax(imodz, 1:8)
   npty_ne = 1
   nptz_ne = 1
   npty_ne = maxval(loc_npty(1:8))
   nptz_ne = maxval(loc_nptz(1:8))
   !======================
   allocate (loc_wghyz(npty_ne, nptz_ne, 8))
   allocate (loc_ypt(npty_ne, 8))
   allocate (loc_zpt(nptz_ne, 8))
   loc_wghyz = 1.
   call mpi_yz_part_distrib(8, loc_npty, loc_nptz, npty_layer, &
                            npty_layer, ymin_t, zmin_t, wyz)
   !=======================
   !Longitudinal layer distribution
   !===========================
   xtot = 0.0
   lpx(1:2) = 0.0 !only layers 3-4-5
   do i = 1, 5
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   xfsh = xf0
   targ_in = xfsh
   targ_end = targ_in + xtot
   ! Input particles
   !====np_per_xc(1:2) electrons and Z1 ions in the nanowires target +
   !internanow-plasma => lpx(3)
   !====np_per_xc(3:4) electrons and Z1 ions in bulk layer
   !=== np_per_xc(5:6) electrons and Z2=proton in contaminant layer
   !  Particles grid ordering
   !  only nxl(3) nxl(4) and nxl(5) layers activated
   nptx_loc(1:2) = nxl(3)*np_per_xc(1:2) !inter-wire  electrons+Z1-ion plasma
   nptx_loc(3:4) = nptx_loc(1:2) !nanowires electron-Z1 ions
   nptx_loc(5:6) = nxl(4)*np_per_xc(3:4) !bulk layer electrons +Z1 ions
   nptx_loc(7:8) = nxl(5)*np_per_xc(5:6) !contaminant electrons +Z2 ions (proton)

   nptx_max = maxval(nptx_loc(1:8))
   !=======================
   allocate (xpt(nptx_max, 8))
   allocate (wghpt(nptx_max, 8))

   allocate (loc_xpt(nptx_max, 8))
   allocate (loc_wghx(nptx_max, 8))
   wghpt(1:nptx_max, 1:8) = 1.
   !=================
   !========================
   loc_imax(imodx, 1:8) = nptx_loc(1:8)
   nps_loc(1:nsp) = 0
   ! Nanowires x-layer: electrons and Z1-ions
   ! nanowires density is the reference density
   if (nxl(3) > 0) then
    do ic = 1, 2
     n_peak = nptx_loc(ic)
     if (n_peak > 0) then
      do i = 1, n_peak
       uu = (real(i, dp) - 0.5)/real(n_peak, dp)
       xpt(i, ic) = xfsh + lpx(3)*uu
       wghpt(i, ic) = ratio_mpc(ic)*j0_norm
       xpt(i, ic + 2) = xpt(i, ic)
       wghpt(i, ic + 2) = np1*wghpt(i, ic) !inter-wire plasma (or vacuum)
      end do
     end if
     !========================= np1>0 a low density  interwire plasma
    end do
    xfsh = xfsh + lpx(3)
    !============= first x-layer distributed on locx mpi tasks
    do ic = 1, 2
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. &
          xpt(i, ic) < loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
       loc_xpt(i1, ic + 2) = xpt(i, ic + 2)
       loc_wghx(i1, ic + 2) = wghpt(i, ic + 2)
      end if
     end do
     loc_imax(imodx, ic) = i1
     loc_imax(imodx, ic + 2) = i1
    end do
    !========================
    p = imodx
    l = imody
    ip = imodz
    nps_loc = 0
    ! Counts particles

    nps_loc(1) = nps_loc(1) + loc_imax(p, 1)*loc_jmax(l, 1)*loc_kmax(ip, &
                                                                     1)
    nps_loc(2) = nps_loc(2) + loc_imax(p, 2)*loc_jmax(l, 2)*loc_kmax(ip, &
                                                                     2)
    if (np1 > 0.0) then
     nps_loc(1) = nps_loc(1) + loc_imax(p, 3)*loc_jmax(l, 3)*loc_kmax(ip &
                                                                      , 3)
     nps_loc(2) = nps_loc(2) + loc_imax(p, 4)*loc_jmax(l, 4)*loc_kmax(ip &
                                                                      , 4)
    end if
   end if
   !------------------------------
   !  Electrons and Z1_ions: bulk layer
   !     x distribution. Density given by the particle density mpc(3:4)
   !====================
   if (nxl(4) > 0) then
    do ic = 5, 6
     n_peak = nptx_loc(ic)
     if (n_peak > 0) then
      do i = 1, n_peak
       xpt(i, ic) = xfsh + lpx(4)*(real(i, dp) - 0.5)/real(n_peak, dp)
       uu = j0_norm*ratio_mpc(ic - 2)
       wghpt(i, ic) = uu
      end do
     end if
    end do
    xfsh = xfsh + lpx(4)
    do ic = 5, 6
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. &
          xpt(i, ic) < loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 5)*loc_jmax(l, 5)*loc_kmax(ip, &
                                                                     5)
    nps_loc(2) = nps_loc(2) + loc_imax(p, 6)*loc_jmax(l, 6)*loc_kmax(ip, &
                                                                     6)
   end if
   !  Electrons and Z3_ions contaminants
   !     x distribution density given by np2
   !====================
   if (nxl(5) > 0) then
    do ic = 7, 8
     n_peak = nptx_loc(ic)
     if (n_peak > 0) then
      do i = 1, n_peak
       xpt(i, ic) = xfsh + lpx(5)*(real(i, dp) - 0.5)/real(n_peak, dp)
       uu = j0_norm*ratio_mpc(ic - 2)
       wghpt(i, ic) = uu*np2
      end do
     end if
    end do
    ic = 8
    n_peak = nptx_loc(ic)
    wghpt(1:n_peak, ic) = wghpt(1:n_peak, ic)/real(ion_min(nsp - 1), dp)
    xfsh = xfsh + lpx(5)
    !===============
    do ic = 7, 8
     i1 = 0
     do i = 1, nptx_loc(ic)
      if (xpt(i, ic) >= loc_xgrid(imodx)%gmin .and. &
          xpt(i, ic) < loc_xgrid(imodx)%gmax) then
       i1 = i1 + 1
       loc_xpt(i1, ic) = xpt(i, ic)
       loc_wghx(i1, ic) = wghpt(i, ic)
      end if
     end do
     loc_imax(imodx, ic) = i1 - 1
    end do
    p = imodx
    l = imody
    ip = imodz

    nps_loc(1) = nps_loc(1) + loc_imax(p, 7)*loc_jmax(l, 7)*loc_kmax(ip, &
                                                                     7)
    nps_loc(nsp) = nps_loc(nsp) + loc_imax(p, 8)*loc_jmax(l, 8)*loc_kmax &
                   (ip, 8)
   end if
   !==============END target x-distribution
   !==============
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
   !===========================
   ip_el = 0
   ip_pr = 0
   ip_ion = 0
   ! The first electron-Z1-ions nanowires layer
   if (nxl(3) > 0) then
    p = 0
    i2 = loc_imax(imodx, 1)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 1, &
      i2, ip_el)
    p = 0
    i2 = loc_imax(imodx, 2)
    call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 2, &
      i2, ip_ion)
    if (np1>0.0) then
     p = ip_el
     i2 = loc_imax(imodx, 3)
     call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 3, &
       i2, ip_el)
     p = ip_ion
     i2 = loc_imax(imodx, 4)
     call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 4, &
       i2, ip_ion)
    end if
   end if
   !=========================
   ! The second electron-ion solid layer with Z1 A1 ion element
   if (nxl(4) > 0) then
    p = ip_el
    i2 = loc_imax(imodx, 5)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 5, &
      i2, ip_el)
    p = ip_ion
    i2 = loc_imax(imodx, 6)
    call pspecies_distribute(spec_in(2), t0_pl(2), unit_charge(2), p, 6, &
      i2, ip_ion)
   end if
   !============
   ! The contaminant electron-ion solid layer Z3=proton ion element
   if (nxl(5) > 0) then
    p = ip_el
    i2 = loc_imax(imodx, 7)
    call pspecies_distribute(spec_in(1), t0_pl(1), unit_charge(1), p, 7, &
      i2, ip_el)

    p = 0
    i2 = loc_imax(imodx, 8)
    call pspecies_distribute(spec_in(nsp), t0_pl(nsp), unit_charge(nsp), p, &
      8, i2, ip_ion)
   end if
   do ic = 1, nsp
    loc_npart(imody, imodz, imodx, ic) = nps_loc(ic)
   end do

  end subroutine
  !============
  subroutine one_layer_nano_tubes_new(spec_in, spec_aux_in, nyh_in, xf0)

   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   logical, allocatable, dimension(:) :: mobilebool
   write(6, *) 'Warning: one_layer_nano_tubes still not adapted to new species'
   ! integer :: p
   ! integer :: i, j, i1, i2, ic, k1, k2
   ! integer :: np_per_zcell(2), n_peak, ntubes
   ! integer :: npty_ne, nptz_ne
   ! integer :: npmax, nps_loc(2)
   ! real (dp) :: uu, dpy, dlpy, rat
   ! real (dp) :: zp_min, zp_max, yp_min, yp_max, xp_min, xp_max
   ! real (dp) :: loc_ym, loc_ymx, loc_zm, loc_zmx
   ! real (dp) :: xfsh, r_int, r_ext, ffactor
   ! integer :: nxl(5), npt_nano(4)
   ! integer :: nlpy
   ! integer :: npty_layer(2), nptz_layer(2)
   ! real (dp), allocatable :: wy(:, :), wz(:, :), wyz(:, :, :)
   ! real (dp), allocatable :: yc(:), ypt_nano(:, :), zpt_nano(:, :)
   ! real (dp), allocatable :: locy_nano(:, :), locz_nano(:, :)
   ! !=================
   ! xp_min = xmin
   ! xp_max = xmax
   ! np_per_zcell(1:2) = 1
   ! !============================
   ! nxl = 0
   ! !========= gridding the transverese target size
   ! yp_min = ymin_t
   ! yp_max = ymax_t
   ! !===============================
   ! ! Geometry |--s/2--|======v=====|---s/2--|
   ! ! total size L=s+v=s*(1+v/s)=s(1+rat)
   ! ! Filling factor f=(1-(v/L)^2)
   ! !===========================
   ! ! Two-species Electrons + Z ions
   ! !=============================

   ! npty = maxval(np_per_yc(1:2))
   ! npty = nyh_in*npty !particles number in 3 nlpy slabs
   ! nptz = 1
   ! if (ndim==3) then
   !  np_per_zcell(1:2) = np_per_zc(1:2)
   !  zp_min = yp_min !-Lz
   !  zp_max = yp_max !+Lz
   !  nptz = maxval(np_per_zc(1:6))
   !  nptz = nyh_in*nptz
   ! end if
   ! allocate (ypt(npty,2))
   ! allocate (wy(npty,2))
   ! allocate (zpt(nptz,2))
   ! allocate (wz(nptz,2))
   ! wy = 1.
   ! wz = 1.
   ! !==================
   ! allocate (loc_jmax(0:npe_yloc-1,1:2))
   ! allocate (loc_kmax(0:npe_zloc-1,1:2))
   ! !====================
   ! ! Uniform yp grid of size npty_ne
   ! do ic = 1, 2
   !  npty_ne = nyh_in*np_per_yc(ic) !number of yp points in 2*ymax size
   !  dpy = (yp_max-yp_min)/real(npty_ne, dp)
   !  do i = 1, npty_ne
   !   ypt(i, ic) = yp_min + dpy*(real(i,dp)-0.5)
   !  end do
   !  npty_layer(ic) = npty_ne
   !  nptz_layer(ic) = 1
   !  if (ndim==3) then
   !   i2 = npty_ne
   !   zpt(1:i2, ic) = ypt(1:i2, ic)
   !   wz(1:i2, ic) = wy(1:i2, ic)
   !   nptz_layer(ic) = i2
   !  end if
   !  call set_pgrid_ind(npty_layer(ic), nptz_layer(ic), ic)
   ! end do
   ! !===========================
   ! ! Layer lpx(3) for nanotubes lpx(4) for target
   ! xtot = 0.0
   ! do i = 3, 4
   !  nxl(i) = nint(dx_inv*lpx(i))
   !  lpx(i) = nxl(i)*dx
   !  xtot = xtot + lpx(i)
   ! end do
   ! xfsh = xf0 + lpx(7)
   ! targ_in = xfsh
   ! targ_end = targ_in + xtot
   ! !=======================
   ! ! Only layers 3 and 4 in x
   ! loc_nptx(1:2) = nxl(3)*np_per_xc(1:2)
   ! loc_nptx(3:4) = nxl(4)*np_per_xc(3:4)
   ! nptx_max = maxval(loc_nptx(1:4))

   ! allocate (xpt(nptx_max,nsp))
   ! allocate (loc_xpt(nptx_max,nsp))
   ! allocate (wghpt(nptx_max,nsp))
   ! !======================================
   ! ! Uses the yp,zp=yp for ic=1,2 uniform p-grid
   ! ! to select a three-layer array of circular nanotubes
   ! !===============
   ! dlpy = lpy(1) !2*dr  lpy(2)=2*r_int
   ! nlpy = nint(dy_inv*dlpy) ! cell numbers in [dlpy layer]
   ! rat = lpy(2)/dlpy
   ! r_int = 0.5*lpy(2)
   ! r_ext = r_int + 0.5*dlpy

   ! uu = (yp_max-yp_min-0.5*dlpy)/(lpy(2)+1.5*dlpy)
   ! ntubes = nint(uu)
   ! allocate (yc(ntubes))
   ! !=========================
   ! yc(1) = yp_min + 0.5*dlpy + r_ext
   ! do ic = 2, ntubes
   !  yc(ic) = yc(ic-1) + lpy(2) + 1.5*dlpy
   ! end do
   ! !========= filling factor
   ! ffactor = acos(-1.)*(r_ext*r_ext-r_int*r_int)/(lpy(2)+1.5*dlpy)**2
   ! !=============================
   ! do ic = 1, 2
   !  npty_ne = nyh_in*np_per_yc(ic) !number of yp points in Ly=2*ymax size
   !  nptz_ne = nyh_in*np_per_zc(ic) !number of zp points in Lz=2*zmax size
   !  npt_nano(ic) = 0
   !  do k1 = 1, ntubes
   !   do k2 = 1, ntubes
   !    do i = 1, nptz_ne
   !     do j = 1, npty_ne
   !      dpy = sqrt((ypt(j,ic)-yc(k2))**2+(zpt(i,ic)-yc(k1))**2)
   !      if (dpy>=r_int .and. dpy<r_ext) npt_nano(ic) = npt_nano(ic) + 1
   !     end do
   !    end do
   !   end do
   !  end do
   ! end do
   ! ! npt_nano(ic) nanotubes section
   ! !====================
   ! npty_ne = npt_nano(1)
   ! allocate (ypt_nano(npty_ne,nsp), zpt_nano(npty_ne,nsp))
   ! loc_ym = loc_ygrid(imody)%gmin
   ! loc_ymx = loc_ygrid(imody)%gmax
   ! loc_zm = loc_zgrid(imodz)%gmin
   ! loc_zmx = loc_zgrid(imodz)%gmax
   ! do ic = 1, 2
   !  npty_ne = nyh_in*np_per_yc(ic) !number of yp points in 2*ymax size
   !  nptz_ne = nyh_in*np_per_zc(ic) !number of yp points in 2*ymax size
   !  i2 = 0
   !  do k1 = 1, ntubes
   !   do k2 = 1, ntubes
   !    do i = 1, nptz_ne
   !     do j = 1, npty_ne
   !      dpy = sqrt((ypt(j,ic)-yc(k2))**2+(zpt(i,ic)-yc(k1))**2)
   !      if (dpy>=r_int .and. dpy<r_ext) then
   !       i2 = i2 + 1
   !       ypt_nano(i2, ic) = ypt(j, ic)
   !       zpt_nano(i2, ic) = zpt(i, ic)
   !      end if
   !     end do
   !    end do
   !   end do
   !  end do
   !  loc_npty(ic) = 0
   !  do i = 1, i2
   !   uu = ypt_nano(i, ic)
   !   if (uu>=loc_ym .and. uu<loc_ymx) then
   !    uu = zpt_nano(i, ic)
   !    if (uu>=loc_zm .and. uu<loc_zmx) loc_npty(ic) = loc_npty(ic) + 1
   !   end if
   !  end do
   ! end do
   ! npty_ne = loc_npty(1)
   ! nptz_ne = npty_ne
   ! !==========================
   ! if (npty_ne>0) then
   !  allocate (locy_nano(npty_ne,2))
   !  allocate (locz_nano(nptz_ne,2))
   ! end if
   ! !==========================
   ! !========== Nanotubes layer
   ! do ic = 1, 2
   !  if (loc_nptx(ic)>0) then
   !   k1 = 0
   !   do i = 1, npt_nano(ic)
   !    uu = ypt_nano(i, ic)
   !    if (uu>=loc_ym .and. uu<loc_ymx) then
   !     uu = zpt_nano(i, ic)
   !     if (uu>=loc_zm .and. uu<loc_zmx) then
   !      k1 = k1 + 1
   !      locy_nano(k1, ic) = ypt_nano(i, ic)
   !      locz_nano(k1, ic) = zpt_nano(i, ic)
   !     end if
   !    end if
   !   end do
   !   loc_npty(ic) = k1
   !  end if
   ! end do
   ! !============================ Flat target
   ! !====================================
   ! loc_npty(1:nsp) = loc_jmax(imody, 1:nsp)
   ! loc_nptz(1:nsp) = loc_kmax(imodz, 1:nsp)
   ! npty_ne = 1
   ! nptz_ne = 1
   ! npty_ne = maxval(loc_npty(1:nsp))
   ! nptz_ne = maxval(loc_nptz(1:nsp))
   ! !======================
   ! allocate (loc_wghyz(npty_ne,nptz_ne,nsp))
   ! allocate (loc_ypt(npty_ne,nsp))
   ! allocate (loc_zpt(nptz_ne,nsp))
   ! loc_wghyz = 1.
   ! !============================ Uniform target
   ! call mpi_yz_part_distrib(2, loc_npty, loc_nptz, npty_layer, &
   !   nptz_layer, ymin_t, zmin_t, wyz)
   ! !==========================
   ! nptx(1:nsp) = 0
   ! !========================
   ! do ic = 1, nsp
   !  i1 = nptx(ic)
   !  n_peak = nxl(3)*np_per_xc(ic)
   !  do i = 1, n_peak
   !   i1 = i1 + 1
   !   uu = (real(i,dp)-0.5)/real(n_peak, dp)
   !   xpt(i1, ic) = xfsh + lpx(3)*uu
   !   wghpt(i1, ic) = j0_norm
   !   if (ic==2) wghpt(i1, ic) = wghpt(i1, ic)*wgh_ion
   !   loc_xpt(i1, ic) = xpt(i1, ic)
   !  end do
   !  nptx(ic) = i1
   ! end do
   ! xfsh = xfsh + lpx(3)
   ! if (nxl(4)>0) then !a bulk
   !  do ic = 1, nsp
   !   i1 = nptx(ic)
   !   n_peak = nxl(4)*np_per_xc(ic)
   !   do i = 1, n_peak
   !    i1 = i1 + 1
   !    uu = (real(i,dp)-0.5)/real(n_peak, dp)
   !    xpt(i1, ic) = xfsh + lpx(4)*uu
   !    wghpt(i1, ic) = j0_norm
   !    loc_xpt(i, ic) = xpt(i1, ic)
   !   end do
   !   nptx(ic) = i1
   !  end do
   !  xfsh = xfsh + lpx(4)
   ! end if
   ! !=================== on index ic=2 are ions with charge Z_i=npc_e/npc_i
   ! !======================
   ! do ic = 1, nsp
   !  j = nptx(ic)
   !  if (xpt(j,ic)>xmax) then
   !   p = 0
   !   do i = 1, nptx_max
   !    if (xpt(i,ic)<=xmax) p = i !inside the box xpt[1:nptx(ic)]
   !   end do
   !   nptx(ic) = p
   !  end if
   ! end do
   ! !============ count partuckes of nano-tubes
   ! do ic = 1, nsp
   !  nps_loc(ic) = 0
   !  i2 = loc_nptx(ic)
   !  do k1 = 1, loc_npty(ic)
   !   do i = 1, i2
   !    nps_loc(ic) = nps_loc(ic) + 1
   !   end do
   !  end do
   !  if (nptx(ic)>i2) then
   !   do i1 = 1, loc_kmax(imodz, ic)
   !    do k1 = 1, loc_jmax(imody, ic)
   !     do i = i2 + 1, nptx(ic)
   !      nps_loc(ic) = nps_loc(ic) + 1
   !     end do
   !    end do
   !   end do
   !  end if
   ! end do
   ! loc_npart(imody, imodz, imodx, 1:nsp) = nps_loc(1:nsp)
   ! npmax = maxval(nps_loc(1:nsp))
   ! npmax = max(npmax, 1)
   ! call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
   ! !===========================
   ! call init_random_seed(mype)
   ! !============
   ! do ic = 1, nsp
   !  i2 = loc_nptx(ic)
   !  charge = int(unit_charge(ic))
   !  p = 0
   !  call spec_in(ic)%set_component(values, component, lb=lb, ub=ub)
   !  do k1 = 1, loc_npty(ic)
   !   do i = 1, i2
   !    wgh = real(wghpt(i,ic), sp)
   !    p = p + 1
   !    spec_in(ic)%part(p, 1) = xpt(i, ic)
   !    spec_in(ic)%part(p, 2) = locy_nano(k1, ic)
   !    spec_in(ic)%part(p, 3) = locz_nano(k1, ic)
   !    call gasdev(uu)
   !    spec_in(ic)%part(p, 4) = t0_pl(ic)*uu
   !    call gasdev(uu)
   !    spec_in(ic)%part(p, 5) = t0_pl(ic)*uu
   !    call gasdev(uu)
   !    spec_in(ic)%part(p, 6) = t0_pl(ic)*uu
   !    spec_in(ic)%part(p, 7) = wgh_cmp
   !   end do
   !  end do
   !  if (nptx(ic)>loc_nptx(ic)) then
   !   i2 = nptx(ic) + 1 - loc_nptx(ic)
   !   call pspecies_distribute(spec_in(ic), t0_pl(ic), unit_charge(ic), p, &
   !     ic, i2, i1)
   !  end if
   ! end do
   ! !============
  end subroutine
  !====================================
  subroutine one_layer_nano_tubes_old(spec_in, spec_aux_in, nyh_in, xf0)

   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: nyh_in
   real (dp), intent (in) :: xf0
   integer :: p
   integer :: i, j, i1, i2, ic, k1, k2
   integer :: np_per_zcell(2), n_peak, ntubes
   integer :: npty_ne, nptz_ne
   integer :: npmax, nps_loc(2)
   real(dp) :: uu, dpy, dlpy, rat
   real(dp) :: zp_min, zp_max, yp_min, yp_max, xp_min, xp_max
   real(dp) :: loc_ym, loc_ymx, loc_zm, loc_zmx
   real(dp) :: xfsh, r_int, r_ext, ffactor
   integer :: nxl(5), npt_nano(4)
   integer :: nlpy
   integer :: npty_layer(2), nptz_layer(2)
   real(dp), allocatable :: wy(:, :), wz(:, :), wyz(:, :, :)
   real(dp), allocatable :: yc(:), ypt_nano(:, :), zpt_nano(:, :)
   real(dp), allocatable :: locy_nano(:, :), locz_nano(:, :)
   !=================
   xp_min = xmin
   xp_max = xmax
   np_per_zcell(1:2) = 1
   !============================
   nxl = 0
   !========= gridding the transverese target size
   yp_min = ymin_t
   yp_max = ymax_t
   !===============================
   ! Geometry |--s/2--|======v=====|---s/2--|
   ! total size L=s+v=s*(1+v/s)=s(1+rat)
   ! Filling factor f=(1-(v/L)^2)
   !===========================
   ! Two-species Electrons + Z ions
   !=============================

   npty = maxval(np_per_yc(1:2))
   npty = nyh_in*npty !particles number in 3 nlpy slabs
   nptz = 1
   if (ndim == 3) then
    np_per_zcell(1:2) = np_per_zc(1:2)
    zp_min = yp_min !-Lz
    zp_max = yp_max !+Lz
    nptz = maxval(np_per_zc(1:6))
    nptz = nyh_in*nptz
   end if
   allocate (ypt(npty, 2))
   allocate (wy(npty, 2))
   allocate (zpt(nptz, 2))
   allocate (wz(nptz, 2))
   wy = 1.
   wz = 1.
   !==================
   allocate (loc_jmax(0:npe_yloc - 1, 1:2))
   allocate (loc_kmax(0:npe_zloc - 1, 1:2))
   !====================
   ! Uniform yp grid of size npty_ne
   do ic = 1, 2
    npty_ne = nyh_in*np_per_yc(ic) !number of yp points in 2*ymax size
    dpy = (yp_max - yp_min)/real(npty_ne, dp)
    do i = 1, npty_ne
     ypt(i, ic) = yp_min + dpy*(real(i, dp) - 0.5)
    end do
    npty_layer(ic) = npty_ne
    nptz_layer(ic) = 1
    if (ndim == 3) then
     i2 = npty_ne
     zpt(1:i2, ic) = ypt(1:i2, ic)
     wz(1:i2, ic) = wy(1:i2, ic)
     nptz_layer(ic) = i2
    end if
    call set_pgrid_ind(npty_layer(ic), nptz_layer(ic), ic)
   end do
   !===========================
   ! Layer lpx(3) for nanotubes lpx(4) for target
   xtot = 0.0
   do i = 3, 4
    nxl(i) = nint(dx_inv*lpx(i))
    lpx(i) = nxl(i)*dx
    xtot = xtot + lpx(i)
   end do
   xfsh = xf0 + lpx(7)
   targ_in = xfsh
   targ_end = targ_in + xtot
   !=======================
   ! Only layers 3 and 4 in x
   loc_nptx(1:2) = nxl(3)*np_per_xc(1:2)
   loc_nptx(3:4) = nxl(4)*np_per_xc(3:4)
   nptx_max = maxval(loc_nptx(1:4))

   allocate (xpt(nptx_max, nsp))
   allocate (loc_xpt(nptx_max, nsp))
   allocate (wghpt(nptx_max, nsp))
   !======================================
   ! Uses the yp,zp=yp for ic=1,2 uniform p-grid
   ! to select a three-layer array of circular nanotubes
   !===============
   dlpy = lpy(1) !2*dr  lpy(2)=2*r_int
   nlpy = nint(dy_inv*dlpy) ! cell numbers in [dlpy layer]
   rat = lpy(2)/dlpy
   r_int = 0.5*lpy(2)
   r_ext = r_int + 0.5*dlpy

   uu = (yp_max - yp_min - 0.5*dlpy)/(lpy(2) + 1.5*dlpy)
   ntubes = nint(uu)
   allocate (yc(ntubes))
   !=========================
   yc(1) = yp_min + 0.5*dlpy + r_ext
   do ic = 2, ntubes
    yc(ic) = yc(ic - 1) + lpy(2) + 1.5*dlpy
   end do
   !========= filling factor
   ffactor = acos(-1.)*(r_ext*r_ext - r_int*r_int)/(lpy(2) + 1.5*dlpy)**2
   !=============================
   do ic = 1, 2
    npty_ne = nyh_in*np_per_yc(ic) !number of yp points in Ly=2*ymax size
    nptz_ne = nyh_in*np_per_zc(ic) !number of zp points in Lz=2*zmax size
    npt_nano(ic) = 0
    do k1 = 1, ntubes
     do k2 = 1, ntubes
      do i = 1, nptz_ne
       do j = 1, npty_ne
        dpy = sqrt((ypt(j, ic) - yc(k2))**2 + (zpt(i, ic) - yc(k1))**2)
        if (dpy >= r_int .and. dpy < r_ext) npt_nano(ic) = npt_nano(ic) + 1
       end do
      end do
     end do
    end do
   end do
   ! npt_nano(ic) nanotubes section
   !====================
   npty_ne = npt_nano(1)
   allocate (ypt_nano(npty_ne, nsp), zpt_nano(npty_ne, nsp))
   loc_ym = loc_ygrid(imody)%gmin
   loc_ymx = loc_ygrid(imody)%gmax
   loc_zm = loc_zgrid(imodz)%gmin
   loc_zmx = loc_zgrid(imodz)%gmax
   do ic = 1, 2
    npty_ne = nyh_in*np_per_yc(ic) !number of yp points in 2*ymax size
    nptz_ne = nyh_in*np_per_zc(ic) !number of yp points in 2*ymax size
    i2 = 0
    do k1 = 1, ntubes
     do k2 = 1, ntubes
      do i = 1, nptz_ne
       do j = 1, npty_ne
        dpy = sqrt((ypt(j, ic) - yc(k2))**2 + (zpt(i, ic) - yc(k1))**2)
        if (dpy >= r_int .and. dpy < r_ext) then
         i2 = i2 + 1
         ypt_nano(i2, ic) = ypt(j, ic)
         zpt_nano(i2, ic) = zpt(i, ic)
        end if
       end do
      end do
     end do
    end do
    loc_npty(ic) = 0
    do i = 1, i2
     uu = ypt_nano(i, ic)
     if (uu >= loc_ym .and. uu < loc_ymx) then
      uu = zpt_nano(i, ic)
      if (uu >= loc_zm .and. uu < loc_zmx) loc_npty(ic) = loc_npty(ic) + 1
     end if
    end do
   end do
   npty_ne = loc_npty(1)
   nptz_ne = npty_ne
   !==========================
   if (npty_ne > 0) then
    allocate (locy_nano(npty_ne, 2))
    allocate (locz_nano(nptz_ne, 2))
   end if
   !==========================
   !========== Nanotubes layer
   do ic = 1, 2
    if (loc_nptx(ic) > 0) then
     k1 = 0
     do i = 1, npt_nano(ic)
      uu = ypt_nano(i, ic)
      if (uu >= loc_ym .and. uu < loc_ymx) then
       uu = zpt_nano(i, ic)
       if (uu >= loc_zm .and. uu < loc_zmx) then
        k1 = k1 + 1
        locy_nano(k1, ic) = ypt_nano(i, ic)
        locz_nano(k1, ic) = zpt_nano(i, ic)
       end if
      end if
     end do
     loc_npty(ic) = k1
    end if
   end do
   !============================ Flat target
   !====================================
   loc_npty(1:nsp) = loc_jmax(imody, 1:nsp)
   loc_nptz(1:nsp) = loc_kmax(imodz, 1:nsp)
   npty_ne = 1
   nptz_ne = 1
   npty_ne = maxval(loc_npty(1:nsp))
   nptz_ne = maxval(loc_nptz(1:nsp))
   !======================
   allocate (loc_wghyz(npty_ne, nptz_ne, nsp))
   allocate (loc_ypt(npty_ne, nsp))
   allocate (loc_zpt(nptz_ne, nsp))
   loc_wghyz = 1.
   !============================ Uniform target
   call mpi_yz_part_distrib(2, loc_npty, loc_nptz, npty_layer, &
                            nptz_layer, ymin_t, zmin_t, wyz)
   !==========================
   nptx(1:nsp) = 0
   !========================
   do ic = 1, nsp
    i1 = nptx(ic)
    n_peak = nxl(3)*np_per_xc(ic)
    do i = 1, n_peak
     i1 = i1 + 1
     uu = (real(i, dp) - 0.5)/real(n_peak, dp)
     xpt(i1, ic) = xfsh + lpx(3)*uu
     wghpt(i1, ic) = j0_norm
     if (ic == 2) wghpt(i1, ic) = wghpt(i1, ic)*wgh_ion
     loc_xpt(i1, ic) = xpt(i1, ic)
    end do
    nptx(ic) = i1
   end do
   xfsh = xfsh + lpx(3)
   if (nxl(4) > 0) then !a bulk
    do ic = 1, nsp
     i1 = nptx(ic)
     n_peak = nxl(4)*np_per_xc(ic)
     do i = 1, n_peak
      i1 = i1 + 1
      uu = (real(i, dp) - 0.5)/real(n_peak, dp)
      xpt(i1, ic) = xfsh + lpx(4)*uu
      wghpt(i1, ic) = j0_norm
      loc_xpt(i, ic) = xpt(i1, ic)
     end do
     nptx(ic) = i1
    end do
    xfsh = xfsh + lpx(4)
   end if
   !=================== on index ic=2 are ions with charge Z_i=npc_e/npc_i
   !======================
   do ic = 1, nsp
    j = nptx(ic)
    if (xpt(j, ic) > xmax) then
     p = 0
     do i = 1, nptx_max
      if (xpt(i, ic) <= xmax) p = i !inside the box xpt[1:nptx(ic)]
     end do
     nptx(ic) = p
    end if
   end do
   !============ count partuckes of nano-tubes
   do ic = 1, nsp
    nps_loc(ic) = 0
    i2 = loc_nptx(ic)
    do k1 = 1, loc_npty(ic)
     do i = 1, i2
      nps_loc(ic) = nps_loc(ic) + 1
     end do
    end do
    if (nptx(ic) > i2) then
     do i1 = 1, loc_kmax(imodz, ic)
      do k1 = 1, loc_jmax(imody, ic)
       do i = i2 + 1, nptx(ic)
        nps_loc(ic) = nps_loc(ic) + 1
       end do
      end do
     end do
    end if
   end do
   loc_npart(imody, imodz, imodx, 1:nsp) = nps_loc(1:nsp)
   npmax = maxval(nps_loc(1:nsp))
   npmax = max(npmax, 1)
   call p_alloc(spec_in, spec_aux_in, npmax, nd2+1, nps_loc, nsp, lpf_ord, 1, 1, mem_psize)
   !===========================
   call init_random_seed(mype)
   !============
   do ic = 1, nsp
    i2 = loc_nptx(ic)
    charge = int(unit_charge(ic), hp_int)
    p = 0
    do k1 = 1, loc_npty(ic)
     do i = 1, i2
      wgh = real(wghpt(i, ic), sp)
      p = p + 1
      spec_in(ic)%part(p, 1) = xpt(i, ic)
      spec_in(ic)%part(p, 2) = locy_nano(k1, ic)
      spec_in(ic)%part(p, 3) = locz_nano(k1, ic)
      call gasdev(uu)
      spec_in(ic)%part(p, 4) = t0_pl(ic)*uu
      call gasdev(uu)
      spec_in(ic)%part(p, 5) = t0_pl(ic)*uu
      call gasdev(uu)
      spec_in(ic)%part(p, 6) = t0_pl(ic)*uu
      spec_in(ic)%part(p, 7) = wgh_cmp
     end do
    end do
    if (nptx(ic) > loc_nptx(ic)) then
     i2 = nptx(ic) + 1 - loc_nptx(ic)
     call pspecies_distribute(spec_in(ic), t0_pl(ic), unit_charge(ic), p, &
       ic, i2, i1)
    end if
   end do
   !============
  end subroutine
  !====================================
  subroutine part_distribute_new(spec_in, spec_aux_in, id, xf0)
   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (in) :: id
   real (dp), intent (in) :: xf0
   integer :: ip, pp, l, p
   integer :: tot_nploc(npe)
   !================= 
   if (wake) then
    !nps_run =1
    !if nsp > 1 ions active only for ionization
    call multi_layer_gas_target(spec_in, spec_aux_in, id, ny_targ, xf0)
    !======================
    !model id=1
    !lpx(1) first plateau np1 density
    !ramp lpx(2)+ plateau lpx(3) + downramp lpx(4)] density 1
    !lpx(5) last plateau np2 density
    !all densities normalized to n_0=n_over_nc
    !====================================
    !model id=2  target with two central plateau (for shocked gas-jet)
    !lpx(1) first ramp up to lpx(2) first plateau at density np1
    !lpx(3) downramp to
    !lpx(4) second plateau density np2 and to final downramp lpx(5)
    !n_0=n_over_nc can be an average, or n0_=n1_over_nc or n0_=n2_over_nc
    !Multispecies implementation
    ! target in models id=1 and id=2 contain (implicitely) an ion species id_sp=1 
    !as a neutralizing background. If ionization is on, nsp=2 and ionizing species
    !is loaded and activated for ionization.
    !====================================
    !model id=3 as id=2, with two ion species, one as local dopant and the other
    !as a neutralizing background:
    ! layer(1) + layer(2) only electrons and H+ with ne=n0=n_over_nc
    ! layer(3) is a plateau with an added dopant (A1,Z1) with density
    ! np1=n1_over_n/n0 (few %) 
    ! layer(4)+layer(5) as layer(1)+layer(2)
    !----------
   else
    !SOLID MULTISPECIES TARGETS
    !flag id=1,2 allowed not even implemented
    select case (id)
    case (3)
     call preplasma_multisp(spec_in, spec_aux_in, ny_targ, xf0)
     ! (e+Z1) preplasma and central target
     !+ (e+Z2)coating
    case (4)
     call multi_layer_twosp_target(spec_in, spec_aux_in, ny_targ, xf0)
     !(e+Z2) foam
     !(e+Z1) central layer
     !(e+Z2)coating
     !============warning exponential ramp (in layer 2) always using (e+Z1) species
    case (5)
     call multi_layer_threesp_target(spec_in, spec_aux_in, ny_targ, xf0)
     !(e+Z3) coating
     !(e+Z1+Z2) central multispecies layer with lpx(2) preplasma
     !+ (e+Z3)coating
    case (6)
     call one_layer_nano_wires(spec_in, spec_aux_in, ny_targ, xf0)
     !e+Z1 wires, e+Z2 bulk. interwire low density (e+Z1) plasma allowed
    case (7)
     call one_layer_nano_tubes(spec_in, spec_aux_in, ny_targ, xf0)
    end select
   end if
   !===================Data for all models===============
   tot_nploc = 0
   pp = 0
   do p = 0, npe_xloc - 1
    do ip = 0, npe_zloc - 1
     do l = 0, npe_yloc - 1
      pp = pp + 1
      tot_nploc(pp) = sum(loc_npart(l,ip,p,1:nsp))
     end do
    end do
   end do
   np_max = maxval(tot_nploc(1:npe))
   np_min = minval(tot_nploc(1:npe))
   do ip = 1, npe
    if (tot_nploc(ip)==np_max) pe_npmax = ip - 1
    if (tot_nploc(ip)==np_min) pe_npmin = ip - 1
   end do
   !===============
  end subroutine
  !====================================
  subroutine part_distribute_old(spec_in, spec_aux_in, id, xf0)
   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   integer, intent (in) :: id
   real (dp), intent (in) :: xf0
   integer :: ip, pp, l, p
   integer :: tot_nploc(npe)
   !=================
   if (wake) then
    !nps_run =1
    !if nsp > 1 ions active only for ionization
    call multi_layer_gas_target(spec_in, spec_aux_in, id, ny_targ, xf0)
    !======================
    !model id=1
    !lpx(1) first plateau np1 density
    !ramp lpx(2)+ plateau lpx(3) + downramp lpx(4)] density 1
    !lpx(5) last plateau np2 density
    !all densities normalized to n_0=n_over_nc
    !====================================
    !model id=2  target with two central plateau (for shocked gas-jet)
    !lpx(1) first ramp up to lpx(2) first plateau at density np1
    !lpx(3) downramp to
    !lpx(4) second plateau density np2 and to final downramp lpx(5)
    !n_0=n_over_nc can be an average, or n0_=n1_over_nc or n0_=n2_over_nc
    !Multispecies implementation
    ! target in models id=1 and id=2 contain (implicitely) an ion species id_sp=1
    !as a neutralizing background. If ionization is on, nsp=2 and ionizing species
    !is loaded and activated for ionization.
    !====================================
    !model id=3 as id=2, with two ion species, one as local dopant and the other
    !as a neutralizing background:
    ! layer(1) + layer(2) only electrons and H+ with ne=n0=n_over_nc
    ! layer(3) is a plateau with an added dopant (A1,Z1) with density
    ! np1=n1_over_n/n0 (few %)
    ! layer(4)+layer(5) as layer(1)+layer(2)
    !----------
   else
    !SOLID MULTISPECIES TARGETS
    !flag id=1,2 allowed not even implemented
    select case (id)
    case (3)
     call preplasma_multisp(spec_in, spec_aux_in, ny_targ, xf0)
     ! (e+Z1) preplasma and central target
     !+ (e+Z2)coating
    case (4)
     call multi_layer_twosp_target(spec_in, spec_aux_in, ny_targ, xf0)
     !(e+Z2) foam
     !(e+Z1) central layer
     !(e+Z2)coating
     !============warning exponential ramp (in layer 2) always using (e+Z1) species
    case (5)
     call multi_layer_threesp_target(spec_in, spec_aux_in, ny_targ, xf0)
     !(e+Z3) coating
     !(e+Z1+Z2) central multispecies layer with lpx(2) preplasma
     !+ (e+Z3)coating
    case (6)
     call one_layer_nano_wires(spec_in, spec_aux_in, ny_targ, xf0)
     !e+Z1 wires, e+Z2 bulk. interwire low density (e+Z1) plasma allowed
    case (7)
     call one_layer_nano_tubes(spec_in, spec_aux_in, ny_targ, xf0)
    end select
   end if
   !===================Data for all models===============
   tot_nploc = 0
   pp = 0
   do p = 0, npe_xloc - 1
    do ip = 0, npe_zloc - 1
     do l = 0, npe_yloc - 1
      pp = pp + 1
      tot_nploc(pp) = sum(loc_npart(l, ip, p, 1:nsp))
     end do
    end do
   end do
   np_max = maxval(tot_nploc(1:npe))
   np_min = minval(tot_nploc(1:npe))
   do ip = 1, npe
    if (tot_nploc(ip) == np_max) pe_npmax = ip - 1
    if (tot_nploc(ip) == np_min) pe_npmin = ip - 1
   end do
   !===============
  end subroutine
  subroutine clean_field(ef, lp1, i1, j1, j2, k1, k2, nc)
   real(dp), intent(inout) :: ef(:, :, :, :)
   real(dp), intent(in) :: lp1
   integer, intent(in) :: i1, j1, j2, k1, k2, nc
   integer :: ilp, i, j, k, ic

   ilp = int(dx_inv*lp1)
   do ic = 1, nc
    do k = k1, k2
     do j = j1, j2
      do i = i1, ilp
       ef(i, j, k, ic) = 0.0
      end do
     end do
    end do
   end do
  end subroutine
  !=========================
 end module
