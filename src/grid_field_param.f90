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

 module grid_field_param

  use common_param
  use mpi_var
  use grid_param

  implicit none

  real(dp), allocatable :: ww1(:), ww2(:), ww0(:, :), wr(:, :), &
                           wl(:, :), var(:, :)
  real(dp) :: hord_der2, opt_der2, opt_der1, aph_der, avg_cmp, &
              cmp_coeff(2), se_coeff(2), se4_coeff(2), upw(4)
 contains
  !==========================================
  subroutine set_field_param
   real(dp) :: nu
   integer :: ndmx, ng
   !==================
   nu = cfl
   if (ndim > 1) nu = cfl*yx_rat/sqrt(yx_rat*yx_rat + float(ndim) - 1.)

   ndmx = max(nx, ny, nz)
   allocate (ww1(ndmx + 5), ww2(ndmx + 5), ww0(ndmx + 5, 5 + max(ny_loc, nz_loc)))
   allocate (wr(ndmx + 6, 10), wl(ndmx + 6, 10))
   allocate (var(ndmx + 5, 10))
   var(:, :) = 0.0
   wr(:, :) = 0.0
   wl(:, :) = 0.0
   ww1(:) = 0.0
   ww2(:) = 0.0
   ww0(:, :) = 0.0
   ng = nx
   select case (der_ord)
   case (2)
    cmp_coeff(1) = 1.
    cmp_coeff(2) = 0.
    avg_cmp = 1.0
    aph_der = cmp_coeff(2)*avg_cmp
    opt_der1 = 1.
   case (3)
    !                        nu=cfl*rat/sqrt(rat*rat+nd-1) multi-D optimized
    !                        coefficient
    !=====================================
    !For der_rder=3 opt first derivative on Yee grid
    cmp_coeff(1) = 1.+0.125*(1.-nu*nu) !rot(E) and rot(B) Modified along x-coord
    cmp_coeff(2) = (1.-cmp_coeff(1))/3. !-(1-nu*nu)/24
    opt_der2 = -(1.-nu*nu)/12.
    hord_der2 = opt_der2
    !For der_order=3 opt second derivative
    opt_der1 = (4.-nu*nu)/3.
    !For der_rder=3 opt for centered first derivative
    !=========================
    avg_cmp = 1./(cmp_coeff(1) + cmp_coeff(2))
    aph_der = cmp_coeff(2)*avg_cmp
    if (comoving) then
     cmp_coeff(1) = 5./8.
     cmp_coeff(2) = 1./8.
    end if
   case (4)
    !For forth-order first derivative on Yee grid
    cmp_coeff(1) = 1.125 !9/8(SE4)
    cmp_coeff(2) = (1.-cmp_coeff(1))/3. !-1./24
    !For forth-order second derivative
    hord_der2 = -1./12.
    !===================================
    avg_cmp = 1./(cmp_coeff(1) + cmp_coeff(2))
    aph_der = cmp_coeff(2)*avg_cmp
    se4_coeff(1) = 4./3.
    se4_coeff(2) = -1./6.
    upw(1) = 1./3.
    upw(2) = 0.5
    upw(3) = -1.
    upw(4) = -(upw(1) + upw(2) + upw(3))
    !------------------------------
   end select
   se_coeff(1:2) = cmp_coeff(1:2)

  end subroutine

 end module

