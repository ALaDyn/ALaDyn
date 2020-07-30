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

 module grid_part_lib

  use common_param
  use grid_param
  use stretched_grid

  implicit none

  real(dp), parameter :: half = 0.5, thr = 0.75, two_third = 2./3., &
                         one_sixth = 1./6.
  real(sp), parameter :: shx = 3., shy = 3., shz = 3.
  integer(kind=2) :: err_ind

 contains
  !    Templates of spl=1,2,3 order shape functions for grid-particle  connection
  !==================================================
  ! Computes
  subroutine set_int_pshape(spl, xx, ax, ind)
   integer, intent(in) :: spl
   real(dp), intent(in) :: xx
   real(dp), intent(out) :: ax(0:3)
   integer, intent(out) :: ind
   real(dp) :: sx, sx2, sx3
   !To integer grid points
   ax(0:3) = 0.0
   select case (spl)
   case (1)
    ind = int(xx)
    ax(1) = xx - real(ind, dp)
    ax(0) = 1.-ax(1)
   case (2)
    ind = int(xx + 0.5)
    sx = xx - real(ind, dp)
    sx2 = sx*sx
    ax(1) = 0.75 - sx2
    ax(2) = 0.5*(0.25 + sx2 + sx)
    ax(0) = 1.-ax(1) - ax(2)
   case (3)
    ind = int(xx)
    sx = xx - real(ind, dp)
    sx2 = sx*sx
    sx3 = sx2*sx
    ax(1) = two_third - sx2 + 0.5*sx3
    ax(2) = one_sixth + 0.5*(sx + sx2 - sx3)
    ax(3) = one_sixth*sx3
    ax(0) = 1.-ax(1) - ax(2) - ax(3)
   end select
  end subroutine
  !========================
  subroutine set_hint_pshape(spl, xx, ax, ind)
   integer, intent(in) :: spl
   real(dp), intent(in) :: xx
   integer, intent(out) :: ind
   real(dp), intent(out) :: ax(0:3)
   real(dp) :: sx, sx2, sx3
   !To half-integer grid points
   ax(0:3) = 0.0
   select case (spl)
   case (1)
    sx = xx + 0.5
    ind = int(sx)
    ax(1) = sx - real(ind, dp)
    ax(0) = 1.-ax(1)
   case (2)
    ind = int(xx)
    sx = xx - 0.5 - real(ind, dp)
    sx2 = sx*sx
    ax(1) = 0.75 - sx2
    ax(2) = 0.5*(0.25 + sx2 + sx)
    ax(0) = 1.-ax(1) - ax(2)
   case (3)
    ind = int(xx + 0.5)
    sx = xx - real(ind, dp)
    sx2 = sx*sx
    sx3 = sx2*sx
    ax(1) = two_third - sx2 + 0.5*sx3
    ax(2) = one_sixth + 0.5*(sx + sx2 - sx3)
    ax(3) = one_sixth*sx3
    ax(0) = 1.-ax(1) - ax(2) - ax(3) ! to (i-1/2,i+1/2,i+3/2,i+5/2) half-int
   end select
  end subroutine
  !=======================

  !===========================================
  !     Computes particle density charge on grid integer points
  !==============================================
  !DIR$ ATTRIBUTES INLINE :: ql_interpolate,qq_interpolate
  !====================
  subroutine qlh_2d_spline(xp, ax, axh, ay, ayh, ix, ihx, iy, ihy)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:2), axh(0:1), ay(0:2), ayh(0:1)
   integer, intent(inout) :: ix, ihx, iy, ihy
   real(dp) :: xx, sx, sx2
   !======================
   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)

   axh(1) = sx + 0.5
   axh(0) = 1.-axh(1)

   xx = shy + xp(2)
   iy = int(xx + 0.5)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   ay(1) = 0.75 - sx2
   ay(2) = 0.5*(0.25 + sx2 + sx)
   ay(0) = 1.-ay(1) - ay(2)

   ayh(1) = sx + 0.5
   ayh(0) = 1.-ayh(1)

   ix = ix - 1
   ihx = ix
   iy = iy - 1
   ihy = iy

  end subroutine
  !====================
  subroutine qqh_1d_spline(xp, ax, axh, ix, ihx)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:2), axh(0:2)
   integer, intent(inout) :: ix, ihx
   real(dp) :: xx, sx, sx2
   !======================
   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)

   ihx = int(xx)
   sx = xx - real(ihx, dp)
   sx2 = sx*sx
   axh(1) = 0.75 - sx2
   axh(2) = 0.5*(0.25 + sx2 + sx)
   axh(0) = 1.-axh(1) - axh(2)
   ix = ix - 1
   ihx = ihx - 1

  end subroutine
  !=======================
  subroutine qqh_2d_spline(xp, ax, axh, ay, ayh, ix, ihx, iy, ihy)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:2), axh(0:2), ay(0:2), ayh(0:2)
   integer, intent(inout) :: ix, ihx, iy, ihy
   real(dp) :: xx, sx, sx2
   !======================
   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)

   ihx = int(xx)
   sx = xx - real(ihx, dp) - 0.5
   sx2 = sx*sx
   axh(1) = 0.75 - sx2
   axh(2) = 0.5*(0.25 + sx2 + sx)
   axh(0) = 1.-axh(1) - axh(2)

   xx = shy + xp(2)
   iy = int(xx + 0.5)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   ay(1) = 0.75 - sx2
   ay(2) = 0.5*(0.25 + sx2 + sx)
   ay(0) = 1.-ay(1) - ay(2)

   ihy = int(xx)
   sx = xx - real(ihy, dp) - 0.5
   sx2 = sx*sx
   ayh(1) = 0.75 - sx2
   ayh(2) = 0.5*(0.25 + sx2 + sx)
   ayh(0) = 1.-ayh(1) - ayh(2)

   ix = ix - 1
   ihx = ihx - 1
   iy = iy - 1
   ihy = ihy - 1
  end subroutine
  !=======================================
  subroutine qlh_3d_spline(xp, ax, axh, ay, ayh, az, azh, ix, ihx, iy, &
                           ihy, iz, ihz)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:2), axh(0:1), ay(0:2), ayh(0:1), &
                              az(0:2), azh(0:1)
   integer, intent(inout) :: ix, ihx, iy, ihy, iz, ihz
   real(dp) :: xx, sx, sx2
   !======================
   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)

   axh(1) = sx + 0.5
   axh(0) = 1.-axh(1)

   xx = shy + xp(2)
   iy = int(xx + 0.5)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   ay(1) = 0.75 - sx2
   ay(2) = 0.5*(0.25 + sx2 + sx)
   ay(0) = 1.-ay(1) - ay(2)

   ayh(1) = sx + 0.5
   ayh(0) = 1.-ayh(1)

   xx = shz + xp(3)
   iz = int(xx + 0.5)
   sx = xx - real(iz, dp)
   sx2 = sx*sx
   az(1) = 0.75 - sx2
   az(2) = 0.5*(0.25 + sx2 + sx)
   az(0) = 1.-az(1) - az(2)

   azh(1) = sx + 0.5
   azh(0) = 1.-azh(1)

   ix = ix - 1
   ihx = ix
   iy = iy - 1
   ihy = iy
   iz = iz - 1
   ihz = iz
  end subroutine
  !=================================
  subroutine qqh_3d_spline(xp, ax, axh, ay, ayh, az, azh, ix, ihx, iy, &
                           ihy, iz, ihz)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:2), axh(0:2), ay(0:2), ayh(0:2), &
                              az(0:2), azh(0:2)
   integer, intent(inout) :: ix, ihx, iy, ihy, iz, ihz
   real(dp) :: xx, sx, sx2

   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)

   ihx = int(xx)
   sx = xx - real(ihx, dp) - 0.5
   sx2 = sx*sx
   axh(1) = 0.75 - sx2
   axh(2) = 0.5*(0.25 + sx2 + sx)
   axh(0) = 1.-axh(1) - axh(2)

   xx = shy + xp(2)
   iy = int(xx + 0.5)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   ay(1) = 0.75 - sx2
   ay(2) = 0.5*(0.25 + sx2 + sx)
   ay(0) = 1.-ay(1) - ay(2)

   ihy = int(xx)
   sx = xx - real(ihy, dp) - 0.5
   sx2 = sx*sx
   ayh(1) = 0.75 - sx2
   ayh(2) = 0.5*(0.25 + sx2 + sx)
   ayh(0) = 1.-ayh(1) - ayh(2)

   xx = shz + xp(3)
   iz = int(xx + 0.5)
   sx = xx - real(iz, dp)
   sx2 = sx*sx
   az(1) = 0.75 - sx2
   az(2) = 0.5*(0.25 + sx2 + sx)
   az(0) = 1.-az(1) - az(2)

   ihz = int(xx)
   sx = xx - real(ihz, dp) - 0.5
   sx2 = sx*sx
   azh(1) = 0.75 - sx2
   azh(2) = 0.5*(0.25 + sx2 + sx)
   azh(0) = 1.-azh(1) - azh(2)

   ix = ix - 1
   ihx = ihx - 1
   iy = iy - 1
   ihy = ihy - 1
   iz = iz - 1
   ihz = ihz - 1
  end subroutine

  subroutine qden_1d_wgh(xp, ax, ix)
   real(dp), intent(in) :: xp(:)
   integer, intent(inout) :: ix
   real(dp), intent(inout) :: ax(0:2)
   real(dp) :: xx, sx, sx2
   !======================
   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)
  end subroutine

  subroutine qden_2d_wgh(xp, ax, ay, ix, iy)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:2), ay(0:2)
   integer, intent(inout) :: ix, iy
   real(dp) :: xx, sx, sx2
   !======================
   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)

   xx = shy + xp(2)
   iy = int(xx + 0.5)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   ay(1) = 0.75 - sx2
   ay(2) = 0.5*(0.25 + sx2 + sx)
   ay(0) = 1.-ay(1) - ay(2)
  end subroutine
!======================
  subroutine cden_2d_wgh(xp, ax, ay, ix, iy)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:3), ay(0:3)
   integer, intent(inout) :: ix, iy
   real(dp) :: xx, sx, sx2, sx3
   !======================
   !cubic spline to integer index
   xx = shy + xp(1)
   ix = int(xx)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   sx3 = sx2*sx
   ax(1) = two_third - sx2 + 0.5*sx3
   ax(2) = one_sixth + 0.5*(sx + sx2 - sx3)
   ax(3) = one_sixth*sx3
   ax(0) = 1.-ax(1) - ax(2) - ax(3)

   xx = shy + xp(2)
   iy = int(xx)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   sx3 = sx2*sx
   ay(1) = two_third - sx2 + 0.5*sx3
   ay(2) = one_sixth + 0.5*(sx + sx2 - sx3)
   ay(3) = one_sixth*sx3
   ay(0) = 1.-ay(1) - ay(2) - ay(3)
  end subroutine
  !==========================
  subroutine qden_3d_wgh(xp, ax, ay, az, ix, iy, iz)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:2), ay(0:2), az(0:2)
   integer, intent(inout) :: ix, iy, iz
   real(dp) :: xx, sx, sx2
   !======================
   xx = shx + xp(1)
   ix = int(xx + 0.5)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   ax(1) = 0.75 - sx2
   ax(2) = 0.5*(0.25 + sx2 + sx)
   ax(0) = 1.-ax(1) - ax(2)

   xx = shy + xp(2)
   iy = int(xx + 0.5)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   ay(1) = 0.75 - sx2
   ay(2) = 0.5*(0.25 + sx2 + sx)
   ay(0) = 1.-ay(1) - ay(2)

   xx = shz + xp(3)
   iz = int(xx + 0.5)
   sx = xx - real(iz, dp)
   sx2 = sx*sx
   az(1) = 0.75 - sx2
   az(2) = 0.5*(0.25 + sx2 + sx)
   az(0) = 1.-az(1) - az(2)
  end subroutine
!---------------------
  subroutine cden_3d_wgh(xp, ax, ay, az, ix, iy, iz)
   real(dp), intent(in) :: xp(:)
   real(dp), intent(inout) :: ax(0:3), ay(0:3), az(0:3)
   integer, intent(inout) :: ix, iy, iz
   real(dp) :: xx, sx, sx2, sx3
   !======================
   !cubic spline to integer index
   xx = shx + xp(1)
   ix = int(xx)
   sx = xx - real(ix, dp)
   sx2 = sx*sx
   sx3 = sx2*sx
   ax(1) = two_third - sx2 + 0.5*sx3
   ax(2) = one_sixth + 0.5*(sx + sx2 - sx3)
   ax(3) = one_sixth*sx3
   ax(0) = 1.-ax(1) - ax(2) - ax(3)

   xx = shy + xp(2)
   iy = int(xx)
   sx = xx - real(iy, dp)
   sx2 = sx*sx
   sx3 = sx2*sx
   ay(1) = two_third - sx2 + 0.5*sx3
   ay(2) = one_sixth + 0.5*(sx + sx2 - sx3)
   ay(3) = one_sixth*sx3
   ay(0) = 1.-ay(1) - ay(2) - ay(3)

   xx = shz + xp(3)
   iz = int(xx)
   sx = xx - real(iz, dp)
   sx2 = sx*sx
   az(1) = two_third - sx2 + 0.5*sx3
   az(2) = one_sixth + 0.5*(sx + sx2 - sx3)
   az(3) = one_sixth*sx3
   az(0) = 1.-az(1) - az(2) - az(3)
  end subroutine
  !====================================

  !DIR$ ATTRIBUTES INLINE :: ql_interpolate
  subroutine set_local_2d_positions(pt_loc, n1, np)

   real(dp), intent(inout) :: pt_loc(:, :)
   integer, intent(in) :: n1, np
   integer :: n
   !=========================
   do n = 1, np
    pt_loc(n, 1) = dx_inv*(pt_loc(n, 1) - xmn)
   end do
   if (n1 == 0) return
   if (n_str == 0) then
    do n = 1, np
     pt_loc(n, 2) = dy_inv*(pt_loc(n, 2) - ymn)
    end do
   else
    call map2dy_part_sind(np, 2, pt_loc)
   end if
   if (n1 == 1) return

   do n = 1, np
    pt_loc(n, 3) = dx_inv*(pt_loc(n, 3) - xmn)
   end do
   if (n_str == 0) then
    do n = 1, np
     pt_loc(n, 4) = dy_inv*(pt_loc(n, 4) - ymn)
    end do
   else
    call map2dy_part_sind(np, 4, pt_loc)
   end if

  end subroutine
  !======================
  subroutine set_local_3d_positions(pt_loc, n1, np)
   real(dp), intent(inout) :: pt_loc(:, :)
   integer, intent(in) :: n1, np
   integer :: n
   !=========================
   do n = 1, np
    pt_loc(n, 1) = dx_inv*(pt_loc(n, 1) - xmn)
   end do
   if (n_str == 0) then
    do n = 1, np
     pt_loc(n, 2) = dy_inv*(pt_loc(n, 2) - ymn)
     pt_loc(n, 3) = dz_inv*(pt_loc(n, 3) - zmn)
    end do
   else
    call map3d_part_sind(pt_loc, np, 2, 3)
   end if
   if (n1 == 1) return
   do n = 1, np
    pt_loc(n, 4) = dx_inv*(pt_loc(n, 4) - xmn)
   end do
   if (n_str == 0) then
    do n = 1, np
     pt_loc(n, 5) = dy_inv*(pt_loc(n, 5) - ymn)
     pt_loc(n, 6) = dz_inv*(pt_loc(n, 6) - zmn)
    end do
   else
    call map3d_part_sind(pt_loc, np, 5, 6)
   end if

  end subroutine

 end module
