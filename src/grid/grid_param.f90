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

 module grid_param

  use precision_def
  use struct_def

  implicit none

  !=======================
  type(grid), allocatable :: loc_ygrid(:)
  !!Contains the local (to the MPI process) y grid informations
  type(grid), allocatable :: loc_zgrid(:)
  !!Contains the local (to the MPI process) x grid informations
  type(grid), allocatable :: loc_xgrid(:)
  !!Contains the local (to the MPI process) x grid informations

  type(sgrid) :: str_xgrid, str_ygrid, str_zgrid
  real(sp), allocatable :: wdata(:), gwdata(:)
  integer, allocatable :: nxh(:), nyh(:), nzh(:)

  !fft grid
  type(grid), allocatable :: loc_yftgrid(:)
  type(grid), allocatable :: loc_zftgrid(:)
  real(dp), allocatable :: yft(:), zft(:)
  real(dp), allocatable :: loc_yft(:, :), loc_zft(:, :)
!-------------------
  real(dp), allocatable :: akx(:, :), aky(:, :), akz(:, :), sty(:, :)
  real(dp), allocatable :: ak2x(:, :), ak2y(:, :), ak2z(:, :), kern(:), &
                           kern2(:, :)
  real(dp), allocatable :: skx(:, :), sky(:, :), skz(:, :)
  !==================
  real(dp), allocatable :: loc_yg(:, :, :), loc_zg(:, :, :), &
                           loc_xg(:, :, :)
  real(dp), allocatable :: x(:), xw(:), y(:), z(:), dx1(:), dy1(:), &
                           dz1(:)
  real(dp), allocatable :: xh(:), yh(:), zh(:), dx1h(:), dy1h(:), &
                           dz1h(:)
  integer, allocatable :: str_indx(:, :), yft_ind(:, :), zft_ind(:, :)
  real(dp), allocatable :: rpt(:), wgp(:)
  real(dp) :: xtot, xmax, xmin, ymax, ymin, zmax, zmin, xw_min, xw_max
  real(dp) :: lx_box, ly_box, lz_box
  real(dp) :: dx, dx_inv, dxi_inv, dy, dz, dy_inv, dyi_inv, dz_inv, &
              dzi_inv
  real(dp) :: aph, l_s, lx_s, dxi, dyi, dzi, sy_rat, sz_rat, sx_rat
  real(dp) :: xmn, ymn, zmn
  real(dp) :: yft_min, zft_min
  !=============================
  integer :: nxp, nyp, nzp
  integer :: loc_ygr_max, loc_zgr_max, loc_xgr_max
  integer :: ix1, ix2, jy1, jy2, kz1, kz2, n_str
  integer :: gcx, gcy, gcz
  !! Ghost cells for every axis
  integer :: nx_stretch, ny_stretch, nz_stretch
  !--------------------------
 end module

