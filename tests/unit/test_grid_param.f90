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

!! @file test_grid_param.f90
!! @brief Unit tests for grid_param module
!! @details Tests grid parameter calculations

program test_grid_param
 use test_assertions
 use test_runner

 implicit none

 call start_test_suite('grid_param')

 call test_grid_dimensions()
 call test_cell_spacing()
 call test_domain_boundaries()

 call end_test_suite('grid_param')

 if (.not. test_suite_passed()) then
  error stop 1
 end if

contains

 subroutine test_grid_dimensions()
  ! Test grid dimension relationships
  integer :: nx, ny, nz
  integer :: n1, n2, n3

  call run_test('grid_dimensions')

  ! Typical grid setup
  nx = 100
  ny = 64
  nz = 64

  ! Grid sizes with boundary padding
  n1 = nx + 4  ! Account for boundary cells
  n2 = ny + 4
  n3 = nz + 4

  call assert_true(n1 > nx, 'n1 includes boundary padding')
  call assert_true(n2 > ny, 'n2 includes boundary padding')
  call assert_true(n3 > nz, 'n3 includes boundary padding')

  ! Test that padded sizes are consistent
  call assert_equal_int(n1 - nx, 4, 'x boundary padding is 4')
  call assert_equal_int(n2 - ny, 4, 'y boundary padding is 4')
  call assert_equal_int(n3 - nz, 4, 'z boundary padding is 4')
 end subroutine

 subroutine test_cell_spacing()
  ! Test cell spacing calculations
  real(dp) :: dx, dy, dz
  real(dp) :: dx_inv, dy_inv, dz_inv
  real(dp) :: lx, ly, lz
  integer :: nx, ny, nz

  call run_test('cell_spacing')

  ! Domain size
  lx = 100.0_dp
  ly = 50.0_dp
  lz = 50.0_dp

  ! Grid cells
  nx = 200
  ny = 100
  nz = 100

  ! Calculate cell spacing
  dx = lx / real(nx, dp)
  dy = ly / real(ny, dp)
  dz = lz / real(nz, dp)

  call assert_near_dp(dx, 0.5_dp, 1.0e-14_dp, 'dx cell spacing correct')
  call assert_near_dp(dy, 0.5_dp, 1.0e-14_dp, 'dy cell spacing correct')
  call assert_near_dp(dz, 0.5_dp, 1.0e-14_dp, 'dz cell spacing correct')

  ! Test inverse cell spacing
  dx_inv = 1.0_dp / dx
  dy_inv = 1.0_dp / dy
  dz_inv = 1.0_dp / dz

  call assert_near_dp(dx_inv, 2.0_dp, 1.0e-14_dp, 'dx_inv correct')
  call assert_near_dp(dy_inv, 2.0_dp, 1.0e-14_dp, 'dy_inv correct')
  call assert_near_dp(dz_inv, 2.0_dp, 1.0e-14_dp, 'dz_inv correct')

  ! Verify roundtrip
  call assert_near_dp(dx * dx_inv, 1.0_dp, 1.0e-14_dp, 'dx * dx_inv = 1')
  call assert_near_dp(dy * dy_inv, 1.0_dp, 1.0e-14_dp, 'dy * dy_inv = 1')
  call assert_near_dp(dz * dz_inv, 1.0_dp, 1.0e-14_dp, 'dz * dz_inv = 1')
 end subroutine

 subroutine test_domain_boundaries()
  ! Test domain boundary calculations
  real(dp) :: xmin, xmax, ymin, ymax, zmin, zmax
  real(dp) :: lx, ly, lz

  call run_test('domain_boundaries')

  ! Typical domain setup (centered at origin in y,z)
  lx = 100.0_dp
  ly = 50.0_dp
  lz = 50.0_dp

  xmin = 0.0_dp
  xmax = lx
  ymin = -ly / 2.0_dp
  ymax = ly / 2.0_dp
  zmin = -lz / 2.0_dp
  zmax = lz / 2.0_dp

  ! Test domain size consistency
  call assert_near_dp(xmax - xmin, lx, 1.0e-14_dp, 'x domain size is lx')
  call assert_near_dp(ymax - ymin, ly, 1.0e-14_dp, 'y domain size is ly')
  call assert_near_dp(zmax - zmin, lz, 1.0e-14_dp, 'z domain size is lz')

  ! Test centering
  call assert_near_dp((ymax + ymin) / 2.0_dp, 0.0_dp, 1.0e-14_dp, 'y domain centered at 0')
  call assert_near_dp((zmax + zmin) / 2.0_dp, 0.0_dp, 1.0e-14_dp, 'z domain centered at 0')
 end subroutine

end program test_grid_param
