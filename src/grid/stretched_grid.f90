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
  use mpi_var, only: imody, imodz, imodx

  implicit none
  private

  type str_params
   real(dp) :: const, smin, smax, xs, dli_inv, ratio, &
    dl_inv
   integer :: nl_stretch, init_cell
  end type

  type(str_params) :: y_params, z_params, x_params

  real(dp), parameter :: SYMM_CENTER = zero_dp
  
  public :: map2dx_part_sind, map2dy_part_sind, map2dz_part_sind, &
   map3d_part_sind, str_params, invert_stretched_grid, SYMM_CENTER
  
  interface map2dx_part_sind
   module procedure map2dx_part_sind_old
   module procedure map2dx_part_sind_new
  end interface
  interface map2dy_part_sind
   module procedure map2dy_part_sind_old
   module procedure map2dy_part_sind_new
  end interface
  interface map2dz_part_sind
   module procedure map2dz_part_sind_old
   module procedure map2dz_part_sind_new
  end interface

 contains
  !============== Mapping for stretched grids==========

 !DIR$ ATTRIBUTES INLINE :: invert_stretched_grid
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

  !DIR$ ATTRIBUTES INLINE :: invert_uniform_grid
  pure function invert_uniform_grid(yp_in, params) result(uniform)
   real(dp), intent(in) :: yp_in
   type(str_params), intent(in) :: params
   real(dp) :: uniform, const, dl_inv

   const = params%const
   dl_inv = params%dl_inv

   uniform = (yp_in + const)*dl_inv

  end function

  subroutine map2dx_part_sind_new(np, pt)
   integer, intent (in) :: np
   real (dp), intent (inout) :: pt(:)
   real (dp) :: xp, xp_loc
   integer :: n
   !========================
   !  enter the x=part(n) particle position in stretched grid
   !            x=x(xi)
   !  exit      xi=part(n) the  particle position in uniform grid
   !               normalized to the Dxi cell size
   !==========================================
   x_params%const = -loc_xgrid(imodx)%gmin
   x_params%smin = str_xgrid%smin
   x_params%smax = str_xgrid%smax
   x_params%xs = nx_stretch*dx
   x_params%dl_inv = dx_inv
   x_params%dli_inv = dxi_inv
   x_params%ratio = sx_rat
   x_params%nl_stretch = nx_stretch
   x_params%init_cell = loc_xgrid(imodx)%min_cell

   select case(x_params%nl_stretch)
   case(0)

    do n = 1, np
     xp = pt(n)
     pt(n) = invert_uniform_grid(xp, x_params)
    end do

   case default

    do n = 1, np
     xp = pt(n)
     if (xp <= x_params%smin) then
      xp_loc = invert_stretched_grid(xp, x_params) - x_params%init_cell
     else if (xp >= x_params%smax) then
      xp = 2*SYMM_CENTER - xp
      xp_loc = invert_stretched_grid(xp, x_params)
      xp_loc = nx - xp_loc - x_params%init_cell
     else
      xp_loc = invert_uniform_grid(xp, x_params)
     end if
     pt(n) = xp_loc
    end do

   end select
  end subroutine

  subroutine map2dx_part_sind_old(np, ic1, pt)
   integer, intent (in) :: np, ic1
   real (dp), intent (inout) :: pt(:, :)
   real (dp) :: xp, xp_loc
   integer :: n
   !========================
   !  enter the x=part(ic1,n) particle position in stretched grid
   !            x=x(xi)
   !  exit      xi=part(ic1,n) the  particle position in uniform grid
   !               normalized to the Dxi cell size
   !==========================================
   x_params%const = -loc_xgrid(imodx)%gmin
   x_params%smin = str_xgrid%smin
   x_params%smax = str_xgrid%smax
   x_params%xs = nx_stretch*dx
   x_params%dl_inv = dx_inv
   x_params%dli_inv = dxi_inv
   x_params%ratio = sx_rat
   x_params%nl_stretch = nx_stretch
   x_params%init_cell = loc_xgrid(imodx)%min_cell

   select case(x_params%nl_stretch)
   case(0)

    do n = 1, np
     xp = pt(n, ic1)
     pt(n, ic1) = invert_uniform_grid(xp, x_params)
    end do

   case default

    do n = 1, np
     xp = pt(n, ic1)
     if (xp <= x_params%smin) then
      xp_loc = invert_stretched_grid(xp, x_params) - x_params%init_cell
     else if (xp >= x_params%smax) then
      xp = 2*SYMM_CENTER - xp
      xp_loc = invert_stretched_grid(xp, x_params)
      xp_loc = nx - xp_loc - x_params%init_cell
     else
      xp_loc = invert_uniform_grid(xp, x_params)
     end if
     pt(n, ic1) = xp_loc
    end do

   end select
  end subroutine

  subroutine map2dy_part_sind_new(np, pt)
   integer, intent (in) :: np
   real (dp), intent (inout) :: pt(:)
   real (dp) :: yp, yp_loc
   integer :: n
   !========================
   !  enter the y=part(n) particle position in stretched grid
   !            y=y(xi)
   !  exit      xi=part(n) the  particle position in uniform grid
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

   select case(y_params%nl_stretch)
   case(0)

    do n = 1, np
     yp = pt(n)
     pt(n) = invert_uniform_grid(yp, y_params) - y_params%init_cell
    end do

   case default

    do n = 1, np
     yp = pt(n)
     if (yp <= y_params%smin) then
      yp_loc = invert_stretched_grid(yp, y_params) - y_params%init_cell
     else if (yp >= y_params%smax) then
      yp = 2*SYMM_CENTER - yp
      yp_loc = invert_stretched_grid(yp, y_params)
      yp_loc = ny - yp_loc - y_params%init_cell
     else
      yp_loc = invert_uniform_grid(yp, y_params) - y_params%init_cell
     end if
     pt(n) = yp_loc
    end do

   end select
  end subroutine

  subroutine map2dy_part_sind_old(np, ic1, pt)
   integer, intent (in) :: np, ic1
   real (dp), intent (inout) :: pt(:, :)
   real (dp) :: yp, yp_loc
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

  subroutine map2dz_part_sind_new(np, pt)
   integer, intent (in) :: np
   real (dp), intent (inout) :: pt(:)
   real (dp) :: zp, zp_loc
   integer :: n
   !========================
   !  enter the z=part(n) particle position in stretched grid
   !            z=y(xi)
   !  exit      xi=part(n) the  particle position in uniform grid
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

   select case(z_params%nl_stretch)
   case(0)

    do n = 1, np
     zp = pt(n)
     pt(n) = invert_uniform_grid(zp, z_params) - z_params%init_cell
    end do

   case default

    do n = 1, np
     zp = pt(n)
     if (zp <= z_params%smin) then
      zp_loc = invert_stretched_grid(zp, z_params) - z_params%init_cell
     else if (zp >= z_params%smax) then
      zp = 2*SYMM_CENTER - zp
      zp_loc = invert_stretched_grid(zp, z_params)
      zp_loc = nz - zp_loc - z_params%init_cell
     else
      zp_loc = invert_uniform_grid(zp, z_params) - z_params%init_cell
     end if
     pt(n) = zp_loc
    end do

   end select
  end subroutine

  subroutine map2dz_part_sind_old(np, ic1, pt)
   integer, intent (in) :: np, ic1
   real (dp), intent (inout) :: pt(:, :)
   real (dp) :: zp, zp_loc
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
