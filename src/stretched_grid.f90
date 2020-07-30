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

 module stretched_grid

  use common_param
  use grid_param
  use mpi_var, only: imody, imodz

  implicit none
  private

  type str_params
   real(dp) :: const, smin, smax, nl_stretch, xs, dli_inv, ratio, &
               dl_inv, init_cell
  end type

  type(str_params) :: y_params, z_params

  real(dp), parameter :: SYMM_CENTER = zero_dp

  public :: map2dy_part_sind, map3d_part_sind

 contains
  !============== Mapping for stretched grids==========

  pure function invert_stretched_grid(yp_in, params) result(stretched)
   real(dp), intent(in) :: yp_in
   type(str_params), intent(in) :: params
   real(dp) :: stretched
   real(dp) :: const, nl_stretch, xs, dli_inv, ratio

   const = params%const
   nl_stretch = params%nl_stretch
   xs = params%xs
   dli_inv = params%dli_inv
   ratio = params%ratio

   stretched = dli_inv*atan(ratio*(yp_in + const - xs)) + nl_stretch

  end function

  pure function invert_uniform_grid(yp_in, params) result(uniform)
   real(dp), intent(in) :: yp_in
   type(str_params), intent(in) :: params
   real(dp) :: uniform, const, dl_inv

   const = params%const
   dl_inv = params%dl_inv

   uniform = (yp_in + const)*dl_inv

  end function

  subroutine map2dy_part_sind(np, ic1, pt)
   integer, intent(in) :: np, ic1
   real(dp), intent(inout) :: pt(:, :)
   real(dp) :: yp, yp_loc
   integer :: n
   !========================
   !  enter the y=part(ic1,n) particle position in stretched grid
   !            y=y(xi)
   !  exit      xi=part(ic1,n) the  particle position in uniform grid
   !               normalized to the Dxi cell size
   !==========================================
   y_params%const = one_dp*ny*dy/2
   y_params%smin = str_ygrid%smin
   y_params%smax = str_ygrid%smax
   y_params%xs = ny_stretch*dy
   y_params%dl_inv = dy_inv
   y_params%dli_inv = dyi_inv
   y_params%ratio = sy_rat
   y_params%nl_stretch = ny_stretch
   y_params%init_cell = loc_ygrid(imody)%min_cell

   do n = 1, np
    yp = pt(n, ic1)
    if (yp <= y_params%smin) then
     yp_loc = invert_stretched_grid(yp, y_params) - y_params%init_cell
    else if (yp >= y_params%smax) then
     yp = 2*SYMM_CENTER - yp
     yp_loc = invert_stretched_grid(yp, y_params)
     yp_loc = ny - yp_loc - y_params%init_cell
    else
     yp_loc = invert_uniform_grid(yp, y_params) - y_params%init_cell
    end if
    pt(n, ic1) = yp_loc
   end do
  end subroutine

  subroutine map2dz_part_sind(np, ic1, pt)
   integer, intent(in) :: np, ic1
   real(dp), intent(inout) :: pt(:, :)
   real(dp) :: zp, zp_loc
   integer :: n
   !========================
   !  enter the z=part(ic1,n) particle position in stretched grid
   !            z=y(xi)
   !  exit      xi=part(ic1,n) the  particle position in uniform grid
   !               normalized to the Dxi cell size
   !==========================================
   z_params%const = one_dp*nz*dz/2
   z_params%smin = str_zgrid%smin
   z_params%smax = str_zgrid%smax
   z_params%xs = nz_stretch*dz
   z_params%dl_inv = dz_inv
   z_params%dli_inv = dzi_inv
   z_params%ratio = sz_rat
   z_params%nl_stretch = nz_stretch
   z_params%init_cell = loc_zgrid(imodz)%min_cell

   do n = 1, np
    zp = pt(n, ic1)
    if (zp <= z_params%smin) then
     zp_loc = invert_stretched_grid(zp, z_params) - z_params%init_cell
    else if (zp >= z_params%smax) then
     zp = 2*SYMM_CENTER - zp
     zp_loc = invert_stretched_grid(zp, z_params)
     zp_loc = nz - zp_loc - z_params%init_cell
    else
     zp_loc = invert_uniform_grid(zp, z_params) - z_params%init_cell
    end if
    pt(n, ic1) = zp_loc
   end do
  end subroutine

  subroutine map3d_part_sind(pt, np, ic1, ic2)
   integer, intent(in) :: np, ic1, ic2
   real(dp), intent(inout) :: pt(:, :)

   !========================
   !  enter the y=part(n,ic1) z=part(n,ic2) particle positions
   !        in stretched grids    y=y(xi), z(zi)
   !  exit   xi=part(n,ic1) zi=part(n,ic2)
   !    particle positions in uniform grid
   !    normalized to the (Dxi Dzi) cell sizes
   !==========================================

   call map2dy_part_sind(np, ic1, pt)
   call map2dz_part_sind(np, ic2, pt)

  end subroutine
  !========================================
 end module
