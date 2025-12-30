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

 module grid_part_util

  use pstruct_data
  use fstruct_data
  use grid_part_lib
  use memory_pool

  implicit none

  type(interp_coeff), allocatable, private :: interp
  !! Useful variable to store interpolation results
  interface set_grid_charge
   module procedure set_grid_charge_new
   module procedure set_grid_charge_old
  end interface

  interface set_grid_env_den_energy
   module procedure set_grid_env_den_energy_new
   module procedure set_grid_env_den_energy_old
  end interface

  interface set_grid_den_energy
   module procedure set_grid_den_energy_new
   module procedure set_grid_den_energy_old
  end interface
 contains

  !DIR$ ATTRIBUTES INLINE :: set_local_positions
  !================================
  subroutine set_part_gamma(pt_loc, np, nc)
   real(dp), intent(inout) :: pt_loc(:, :)
   integer, intent(in) :: np, nc
   integer :: n
   real(dp) :: gam2

   select case (nc)
   case (2)
    do n = 1, np
     gam2 = pt_loc(n, 3)*pt_loc(n, 3) + pt_loc(n, 4)*pt_loc(n, 4)
     pt_loc(n, 4) = sqrt(1.+gam2)
    end do
   case (3)
    do n = 1, np
     gam2 = pt_loc(n, 4)*pt_loc(n, 4) + pt_loc(n, 5)*pt_loc(n, 5) + &
            pt_loc(n, 6)*pt_loc(n, 6)
     pt_loc(n, 4) = sqrt(1.+gam2)
    end do
   end select
   !============exit gamma
  end subroutine
  !===================
  subroutine set_part_velocities(pt_loc, np, njc)
   real(dp), intent(inout) :: pt_loc(:, :)
   integer, intent(in) :: np, njc
   integer :: n
   real(dp) :: gam2, gam_inv

   select case (njc)
   case (2)
    do n = 1, np
     gam2 = pt_loc(n, 3)*pt_loc(n, 3) + pt_loc(n, 4)*pt_loc(n, 4)
     gam_inv = 1./sqrt(1.+gam2)
     pt_loc(n, 3) = gam_inv*pt_loc(n, 3)
     pt_loc(n, 4) = gam_inv*pt_loc(n, 4)
    end do
   case (3)
    do n = 1, np
     gam2 = pt_loc(n, 4)*pt_loc(n, 4) + pt_loc(n, 5)*pt_loc(n, 5) + &
            pt_loc(n, 6)*pt_loc(n, 6)
     gam_inv = 1./sqrt(1.+gam2)
     pt_loc(n, 4) = gam_inv*pt_loc(n, 4)
     pt_loc(n, 5) = gam_inv*pt_loc(n, 5)
     pt_loc(n, 6) = gam_inv*pt_loc(n, 6)
    end do
   end select
  end subroutine
  !==========================
  subroutine set_ho_grid_charge(sp_loc, pt, den, np, ic)

   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)
   real(dp), intent(inout) :: den(:, :, :, :)
   integer, intent(in) :: np, ic
   real(dp) :: xp(3), dvol, ax0(0:3), ay0(0:3), az0(0:3)
   integer :: i, j, k, i1, j1, k1, i2, j2, k2, n, ch, spl
   real(sp) :: wght
   !======================
   ! Computes charge density on a grid using cubic spline on uniform grid
   !=================================
   ax0(0:3) = zero_dp
   ay0(0:3) = zero_dp
   az0(0:3) = zero_dp
   spl = 3
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   case (2)
    ch = 5
    do n = 1, np
     pt(n, 1) = dx_inv*(sp_loc%part(n, 1) - xmn)
     pt(n, 2) = dy_inv*(sp_loc%part(n, 2) - ymn)
     wgh_cmp = sp_loc%part(n, 5)
     pt(n, 4) = charge*wgh
    end do
    !==========================
    do n = 1, np
     wght = real(pt(n, 4), sp)
     xp(1:2) = pt(n, 1:2)

     call cden_2d_wgh( xp, interp )

     ax0(0:3) = interp%coeff_x(0:3)
     ay0(0:3) = interp%coeff_y(0:3)

     i = interp%ix
     j = interp%iy

     ax0(0:3) = wght*ax0(0:3)
     do j1 = 0, 3
      j2 = j + j1
      do i1 = 0, 3
       i2 = i + i1
       dvol = ax0(i1)*ay0(j1)
       den(i2, j2, 1, ic) = den(i2, j2, 1, ic) + dvol
      end do
     end do
    end do
   case (3)
    ch = 7
    do n = 1, np
     pt(n, 1) = dx_inv*(sp_loc%part(n, 1) - xmn)
     pt(n, 2) = dy_inv*(sp_loc%part(n, 2) - ymn)
     pt(n, 3) = dz_inv*(sp_loc%part(n, 3) - zmn)
     wgh_cmp = sp_loc%part(n, 7)
     wght = charge*wgh
     pt(n, 4) = wght
    end do
    do n = 1, np
     xp(1:3) = pt(n, 1:3)
     wght = real(pt(n,4), sp)

     call cden_3d_wgh( xp, interp )

     ax0(0:3) = interp%coeff_x(0:3)
     ay0(0:3) = interp%coeff_y(0:3)
     az0(0:3) = interp%coeff_z(0:3)

     i = interp%ix
     j = interp%iy
     k = interp%iz

     ax0(0:3) = wght*ax0(0:3)
     do k1 = 0, spl
      k2 = k + k1
      do j1 = 0, spl
       j2 = j + j1
       dvol = az0(k1)*ay0(j1)
       do i1 = 0, spl
        i2 = i + i1
        den(i2, j2, k2, ic) = den(i2, j2, k2, ic) + ax0(i1)*dvol
       end do
      end do
     end do
    end do
    ! charge density on den(i,j,k,ic)
   end select
  end subroutine
  !==========================
  subroutine set_charge_on_ftgrid(sp_loc, pt, den, np, ic)

   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)
   real(dp), intent(inout) :: den(:, :, :, :)
   integer, intent(in) :: np, ic
   real(dp) :: xp(3), dvol, ax0(0:2), ay0(0:2), az0(0:2)
   integer :: i, j, k, i1, j1, k1, i2, j2, k2, n, ch, spl
   real(sp) :: wght
   real(dp) :: x1_loc, y1_loc, z1_loc
   !======================
   ! Computes charge density on the superposed uniform ftgrid
   !=================================
   ax0(0:2) = zero_dp
   ay0(0:2) = zero_dp
   az0(0:2) = zero_dp
   spl = 2
   x1_loc=xmn
   y1_loc=yft_min
   z1_loc=zft_min
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   case (2)
    ch = 5
    do n = 1, np
     pt(n, 1) = dx_inv*(sp_loc%part(n, 1) - x1_loc)
     pt(n, 2) = dy_inv*(sp_loc%part(n, 2) - y1_loc)
     wgh_cmp = sp_loc%part(n, 5)
     pt(n, 4) = charge*wgh
    end do
    !==========================
    do n = 1, np
     wght = real(pt(n, 4), sp)
     xp(1:2) = pt(n, 1:2)

     call qden_2d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)

     i = interp%ix
     j = interp%iy

     ax0(0:2) = wght*ax0(0:2)
     do j1 = 0, 2
      j2 = j + j1
      do i1 = 0, 2
       i2 = i + i1
       dvol = ax0(i1)*ay0(j1)
       den(i2, j2, 1, ic) = den(i2, j2, 1, ic) + dvol
      end do
     end do
    end do
   case (3)
    ch = 7
    do n = 1, np
     pt(n, 1) = dx_inv*(sp_loc%part(n, 1) - x1_loc)
     pt(n, 2) = dy_inv*(sp_loc%part(n, 2) - y1_loc)
     pt(n, 3) = dz_inv*(sp_loc%part(n, 3) - z1_loc)
     wgh_cmp = sp_loc%part(n, 7)
     wght = charge*wgh
     pt(n, 4) = wght
    end do
    do n = 1, np
     xp(1:3) = pt(n, 1:3)
     wght = real(pt(n,4), sp)

     call qden_3d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)
     az0(0:2) = interp%coeff_z(0:2)

     i = interp%ix
     j = interp%iy
     k = interp%iz

     ax0(0:2) = wght*ax0(0:2)
     do k1 = 0, spl
      k2 = k + k1
      do j1 = 0, spl
       j2 = j + j1
       dvol = az0(k1)*ay0(j1)
       do i1 = 0, spl
        i2 = i + i1
        den(i2, j2, k2, ic) = den(i2, j2, k2, ic) + ax0(i1)*dvol
       end do
      end do
     end do
    end do
    ! charge density on den(ic)
   end select
  end subroutine
!=================================
  subroutine set_grid_charge_new(sp_loc, den, np, ic, mempool)

   type(species_new), intent (in) :: sp_loc
   real (dp), intent (inout) :: den(:, :, :, :)
   integer, intent (in) :: np, ic
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp), pointer, contiguous, dimension(:, :) :: xx => null()
   real(dp), pointer, contiguous, dimension(:) :: weight => null()
   real (dp) :: dvol
   integer :: i1, j1, k1, i2, j2, k2, n, spline
   !======================
   ! Computes charge density of species ic on a grid
   !=================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   call interp_realloc(interp, np, sp_loc%pick_dimensions())
   !================================
   allocate( weight(np) )

   spline = 2
   select case (ndim)
   case (2)
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    xx => mempool%mp_xx_2d_A

    weight(1:np) = sp_loc%pick_charge()*sp_loc%call_component( W_COMP, lb=1, ub=np)

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    !==========================

    call qden_2d_wgh( xx(1:np, 1:2), interp, mempool )

    associate( ax0 => interp%coeff_x_rank2, &
               ay0 => interp%coeff_y_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2 )

    !==========================
    ! Warning: this call must be after qden_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    weight => mempool%mp_xx_1d_A
    weight(1:np) = sp_loc%pick_charge()*sp_loc%call_component( W_COMP, lb=1, ub=np)

    do n = 0, 2
     ax0(n, 1:np) = weight(1:np)*ax0(n, 1:np)
    end do

    do n = 1, np
     do j1 = 0, spline
      j2 = j(n) + j1
      do i1 = 0, spline
       i2 = i(n) + i1
       dvol = ax0(i1, n)*ay0(j1, n)
       den(i2, j2, 1, ic) = den(i2, j2, 1, ic) + dvol
      end do
     end do
    end do

    end associate

   case (3)
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    xx => mempool%mp_xx_2d_A
    weight(1:np) = sp_loc%pick_charge()*sp_loc%call_component( W_COMP, lb=1, ub=np)

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
    !==========================

    call qden_3d_wgh( xx(1:np, 1:3), interp, mempool )

    associate( ax0 => interp%coeff_x_rank2, &
               ay0 => interp%coeff_y_rank2, &
               az0 => interp%coeff_z_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               k => interp%iz_rank2 )

    !==========================
    ! Warning: this call must be after qden_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    weight => mempool%mp_xx_1d_A
    weight(1:np) = sp_loc%pick_charge()*sp_loc%call_component( W_COMP, lb=1, ub=np)

    do n = 0, 2
     ax0(n, 1:np) = weight(1:np)*ax0(n, 1:np)
    end do

    do n = 1, np
     do k1 = 0, spline
      k2 = k(n) + k1
      do j1 = 0, spline
       j2 = j(n) + j1
       dvol = az0(k1, n)*ay0(j1, n)
       do i1 = 0, spline
        i2 = i(n) + i1
        den(i2, j2, k2, ic) = den(i2, j2, k2, ic) + ax0(i1, n)*dvol
       end do
      end do
     end do
    end do
    ! charge density on den(ic)
    end associate
   end select
  end subroutine
  !==========================
!=================================
  subroutine set_grid_charge_old(sp_loc, pt, den, np, ic)

   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)
   real(dp), intent(inout) :: den(:, :, :, :)
   integer, intent(in) :: np, ic
   real(dp) :: xp(3), dvol, ax0(0:2), ay0(0:2), az0(0:2)
   integer :: i, j, k, i1, j1, k1, i2, j2, k2, n, ch, spl
   real(sp) :: wght
   !======================
   ! Computes charge density of species ic on a grid
   !=================================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   ax0(0:2) = zero_dp
   ay0(0:2) = zero_dp
   az0(0:2) = zero_dp
   spl = 2
   select case (ndim)
   case (1)
    j2 = 1
    do n = 1, np
     pt(n, 1) = sp_loc%part(n, 1)
    end do
    call set_local_2d_positions(pt, 0, np)
    do n = 1, np
     xp(1) = pt(n, 1)
     wgh_cmp = sp_loc%part(n, 5)
     wght = charge*wgh

     call qden_1d_wgh( xp, interp )

     ax0(0:2) = wght*interp%coeff_x(0:2)
     i = interp%ix

     do i1 = 0, 2
      i2 = i + i1
      den(i2, j2, 1, ic) = den(i2, j2, 1, ic) + ax0(i1)
     end do
    end do
   case (2)
    ch = 5
    do n = 1, np
     pt(n, 1:2) = sp_loc%part(n, 1:2)
     wgh_cmp = sp_loc%part(n, 5)
     pt(n, 4) = charge*wgh
    end do
    call set_local_2d_positions(pt, 1, np)
    !==========================
    do n = 1, np
     wght = real(pt(n, 4), sp)
     xp(1:2) = pt(n, 1:2)

     call qden_2d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)

     i = interp%ix
     j = interp%iy

     ax0(0:2) = wght*ax0(0:2)
     do j1 = 0, 2
      j2 = j + j1
      do i1 = 0, 2
       i2 = i + i1
       dvol = ax0(i1)*ay0(j1)
       den(i2, j2, 1, ic) = den(i2, j2, 1, ic) + dvol
      end do
     end do
    end do
   case (3)
    ch = 7
    do n = 1, np
     pt(n, 1:3) = sp_loc%part(n, 1:3)
     pt(n, 4) = sp_loc%part(n, ch)
     wgh_cmp = pt(n, 4)
     wght = charge*wgh
     pt(n, 4) = wght
    end do
    call set_local_3d_positions(pt, 1, np)
    do n = 1, np
     xp(1:3) = pt(n, 1:3)
     wght = real(pt(n,4), sp)

     call qden_3d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)
     az0(0:2) = interp%coeff_z(0:2)

     i = interp%ix
     j = interp%iy
     k = interp%iz

     ax0(0:2) = wght*ax0(0:2)
     do k1 = 0, spl
      k2 = k + k1
      do j1 = 0, spl
       j2 = j + j1
       dvol = az0(k1)*ay0(j1)
       do i1 = 0, spl
        i2 = i + i1
        den(i2, j2, k2, ic) = den(i2, j2, k2, ic) + ax0(i1)*dvol
       end do
      end do
     end do
    end do
    ! charge density on den(ic)
   end select
  end subroutine
  !==========================
  !==========================
  subroutine set_grid_env_den_energy_new(sp_loc, eden, np, icp, mempool)

   type (species_new), intent (in) :: sp_loc
   real (dp), intent (inout) :: eden(:, :, :, :)
   integer, intent (in) :: np, icp
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp), pointer, contiguous, dimension(:, :) :: xx => null()
   real (dp), pointer, contiguous, dimension(:) :: gam => null()
   real (dp) :: dvol
   integer :: i1, j1, k1, i2, j2, k2, n, spline
   !======================
   !   Computes eden(grid,1)= n/n_0 and eden(grid,2)=<gam-1}n>/n_0
   !================================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !================================
   call interp_realloc(interp, np, sp_loc%pick_dimensions())
   !================================

   spline = 2
   select case (ndim)
   case (1)
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 1, mempool)
    xx => mempool%mp_xx_2d_A

    call qden_1d_wgh( xx(1:np, 1:1), interp, mempool )

    !==========================
    ! Warning: this call must be after qden_1d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    gam => mempool%mp_xx_1d_A
    gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np)

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    
    j2 = 1

    associate( ax0 => interp%coeff_x_rank2, &
               i => interp%ix_rank2, &
               weight => sp_loc%call_component( W_COMP, lb=1, ub=np) )

    do n = 1, np
     do i1 = 0, spline
      i2 = i(n) + i1
      dvol = ax0(i1, n)
      gam(n) = gam(n) + dvol*eden(i2, j2, 1, icp)
     end do
    end do
    gam(1:np) = sqrt( one_dp + gam(1:np))
    do i1 = 0, spline
     ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
    end do
    do n = 1, np
     do i1 = 0, spline
      i2 = i(n) + i1
      dvol = ax0(i1, n)
      eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*sp_loc%pick_charge()
      eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam(n)-1)*dvol
     end do
    end do

   end associate

   case (2)

    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    xx => mempool%mp_xx_2d_A


    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    
    call qden_2d_wgh( xx(1:np, 1:2), interp, mempool )

    associate( ax0 => interp%coeff_x_rank2, &
               ay0 => interp%coeff_y_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               weight => sp_loc%call_component( W_COMP, lb=1, ub=np) )

    !==========================
    ! Warning: this call must be after qden_1d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    gam => mempool%mp_xx_1d_A

    if (curr_ndim==2) then

     gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PY_COMP, lb=1, ub=np)*sp_loc%call_component( PY_COMP, lb=1, ub=np)

     do n = 1, np
      do j1 = 0, spline
       j2 = j(n) + j1
       do i1 = 0, spline
        i2 = i(n) + i1
        dvol = ax0(i1, n)*ay0(j1, n)
        gam(n) = gam(n) + dvol*eden(i2, j2, 1, icp)
       end do
      end do
     end do
     gam(1:np) = sqrt(one_dp + gam(1:np))
     do i1 = 0, spline
      ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
     end do
     do n = 1, np
      do j1 = 0, spline
       j2 = j(n) + j1
       do i1 = 0, spline
        i2 = i(n) + i1
        dvol = ax0(i1, n)*ay0(j1, n)
        eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*sp_loc%pick_charge()
        eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam(n)-1.)*dvol
       end do
      end do
     end do
    end if
    if (curr_ndim==3) then

     gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PY_COMP, lb=1, ub=np)*sp_loc%call_component( PY_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PZ_COMP, lb=1, ub=np)*sp_loc%call_component( PZ_COMP, lb=1, ub=np)

     do n = 1, np
      !============ adds iparticle assigned [A^2/2]_p contribution
      do j1 = 0, spline
       j2 = j(n) + j1
       do i1 = 0, spline
        i2 = i(n) + i1
        dvol = ax0(i1, n)*ay0(j1, n)
        gam(n) = gam(n) + dvol*eden(i2, j2, 1, icp)
       end do
      end do
     end do
     gam(1:np) = sqrt(one_dp + gam(1:np))
     do i1 = 0, spline
      ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
     end do
     do n = 1, np
      do j1 = 0, spline
       j2 = j(n) + j1
       do i1 = 0, spline
        i2 = i(n) + i1
        dvol = ax0(i1, n)*ay0(j1, n)
        eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*sp_loc%pick_charge()
        eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam(n) - 1.)*dvol
       end do
      end do
     end do
    end if

    end associate

   case (3)

    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
    
    call qden_3d_wgh( xx(1:np, 1:3), interp, mempool )

    associate( ax0 => interp%coeff_x_rank2, &
               ay0 => interp%coeff_y_rank2, &
               az0 => interp%coeff_z_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               k => interp%iz_rank2, &
               weight => sp_loc%call_component( W_COMP, lb=1, ub=np) )

    !==========================
    ! Warning: this call must be after qden_1d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    gam => mempool%mp_xx_1d_A
           
    gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PY_COMP, lb=1, ub=np)*sp_loc%call_component( PY_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PZ_COMP, lb=1, ub=np)*sp_loc%call_component( PZ_COMP, lb=1, ub=np)

    do n = 1, np
     do k1 = 0, spline
      k2 = k(n) + k1
      do j1 = 0, spline
       j2 = j(n) + j1
       dvol = az0(k1, n)*ay0(j1, n)
       do i1 = 0, spline
        i2 = i(n) + i1
        gam(n) = gam(n) + ax0(i1, n)*dvol*eden(i2, j2, k2, icp)
       end do
      end do
     end do
    end do
    gam(1:np) = sqrt(one_dp + gam(1:np))
    do i1 = 0, spline
     ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
    end do
    do n = 1, np
     do k1 = 0, spline
      k2 = k(n) + k1
      do j1 = 0, spline
       j2 = j(n) + j1
       dvol = az0(k1, n)*ay0(j1, n)
       do i1 = 0, spline
        i2 = i(n) + i1
        eden(i2, j2, k2, 1) = eden(i2, j2, k2, 1) + ax0(n, i1)*dvol*sp_loc%pick_charge()
        eden(i2, j2, k2, 2) = eden(i2, j2, k2, 2) + &
          (gam(n) - 1.)*ax0(i1, n)*dvol
       end do
      end do
     end do
    end do

    end associate
   end select
   !===========================
  end subroutine
  !==========================
  subroutine set_grid_env_den_energy_old(sp_loc, pt, eden, np, icp)

   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)
   real(dp), intent(inout) :: eden(:, :, :, :)
   integer, intent(in) :: np, icp
   real(dp) :: dvol, gam2, gam_p
   real(dp) :: ax0(0:2), ay0(0:2), az0(0:2), xp(3), pp(3)
   integer :: i, j, k, i1, j1, k1, i2, j2, k2, n, ch, spl
   !======================
   !   Computes eden(grid,1)= n/n_0 and eden(grid,2)=<gam-1}n>/n_0
   !================================================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   ax0(0:2) = 0.0
   ay0(0:2) = 0.0
   az0(0:2) = 0.0
   spl = 2
   if (np < 1) return
   select case (ndim)
   case (1)
    ch = 5
    j2 = 1
    do n = 1, np
     pt(n, 1:ch) = sp_loc%part(n, 1:ch)
    end do
    call set_local_2d_positions(pt, 0, np)
    do n = 1, np
     xp(1) = pt(n, 1)
     pp(1:2) = sp_loc%part(n, 3:4)
     gam2 = pp(1)*pp(1) + pp(2)*pp(2)
     wgh_cmp = sp_loc%part(n, ch)

     call qden_1d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     i = interp%ix

     do i1 = 0, 2
      i2 = i + i1
      dvol = ax0(i1)
      gam2 = gam2 + dvol*eden(i2, j2, 1, icp)
     end do
     gam_p = sqrt(1.+gam2)
     ax0(0:2) = wgh*ax0(0:2)
     do i1 = 0, 2
      i2 = i + i1
      dvol = ax0(i1)
      eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*charge
      eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam_p - 1)*dvol
     end do
    end do
   case (2)
    ch = size(sp_loc%part, 2)
    do n = 1, np
     pt(n, 1:ch) = sp_loc%part(n, 1:ch)
    end do
    call set_local_2d_positions(pt, 1, np)
    if (curr_ndim == 2) then
     do n = 1, np
      pp(1:2) = sp_loc%part(n, 3:4)
      gam2 = pp(1)*pp(1) + pp(2)*pp(2)
      xp(1:2) = pt(n, 1:2)
      wgh_cmp = pt(n, ch)

      call qden_2d_wgh( xp, interp )

      ax0(0:2) = interp%coeff_x(0:2)
      ay0(0:2) = interp%coeff_y(0:2)
 
      i = interp%ix
      j = interp%iy

      do j1 = 0, 2
       j2 = j + j1
       do i1 = 0, 2
        i2 = i + i1
        dvol = ax0(i1)*ay0(j1)
        gam2 = gam2 + dvol*eden(i2, j2, 1, icp)
       end do
      end do
      gam_p = sqrt(1.+gam2)
      ax0(0:2) = wgh*ax0(0:2) !weights are inside
      do j1 = 0, 2
       j2 = j + j1
       do i1 = 0, 2
        i2 = i + i1
        dvol = ax0(i1)*ay0(j1)
        eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*charge
        eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam_p - 1.)*dvol
       end do
      end do
     end do
    end if
    if (curr_ndim == 3) then
     do n = 1, np
      pp(1:3) = sp_loc%part(n, 4:6)
      gam2 = pp(1)*pp(1) + pp(2)*pp(2) + pp(3)*pp(3)
      xp(1:2) = pt(n, 1:2)
      wgh_cmp = pt(n, ch)

      call qden_2d_wgh( xp, interp )

      ax0(0:2) = interp%coeff_x(0:2)
      ay0(0:2) = interp%coeff_y(0:2)
 
      i = interp%ix
      j = interp%iy

      !============ adds iparticle assigned [A^2/2]_p contribution
      do j1 = 0, 2
       j2 = j + j1
       do i1 = 0, 2
        i2 = i + i1
        dvol = ax0(i1)*ay0(j1)
        gam2 = gam2 + dvol*eden(i2, j2, 1, icp)
       end do
      end do
      gam_p = sqrt(1.+gam2)
      ax0(0:2) = wgh*ax0(0:2)
      do j1 = 0, 2
       j2 = j + j1
       do i1 = 0, 2
        i2 = i + i1
        dvol = ax0(i1)*ay0(j1)
        eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*charge
        eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam_p - 1.)*dvol
       end do
      end do
     end do
    end if
   case (3)
    ch = 7
    do n = 1, np
     pt(n, 1:ch) = sp_loc%part(n, 1:ch)
    end do
    call set_local_3d_positions(pt, 1, np)
    do n = 1, np
     pp(1:3) = sp_loc%part(n, 4:6)
     gam2 = pp(1)*pp(1) + pp(2)*pp(2) + pp(3)*pp(3)
     xp(1:3) = pt(n, 1:3)
     wgh_cmp = pt(n, ch)

     call qden_3d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)
     az0(0:2) = interp%coeff_z(0:2)

     i = interp%ix
     j = interp%iy
     k = interp%iz

     do k1 = 0, spl
      k2 = k + k1
      do j1 = 0, spl
       j2 = j + j1
       dvol = az0(k1)*ay0(j1)
       do i1 = 0, spl
        i2 = i + i1
        gam2 = gam2 + ax0(i1)*dvol*eden(i2, j2, k2, icp)
       end do
      end do
     end do
     gam_p = sqrt(1.+gam2)
     ax0(0:2) = wgh*ax0(0:2)
     do k1 = 0, spl
      k2 = k + k1
      do j1 = 0, spl
       j2 = j + j1
       dvol = az0(k1)*ay0(j1)
       do i1 = 0, spl
        i2 = i + i1
        eden(i2, j2, k2, 1) = eden(i2, j2, k2, 1) + ax0(i1)*dvol*charge
        eden(i2, j2, k2, 2) = eden(i2, j2, k2, 2) + &
                              (gam_p - 1.)*ax0(i1)*dvol
       end do
      end do
     end do
    end do
   end select
   !===========================
  end subroutine
  !=================================================
  subroutine set_grid_den_energy_new(sp_loc, eden, np, mempool)

   type (species_new), intent (in) :: sp_loc
   real (dp), intent (inout) :: eden(:, :, :, :)
   integer, intent (in) :: np
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp), pointer, contiguous, dimension(:, :) :: xx => null()
   real (dp), pointer, contiguous, dimension(:) :: gam => null()
   real (dp) :: dvol
   integer :: i1, j1, k1, i2, j2, k2, n, spline
   !================================================
   !   Computes eden(grid,1)= n/n_0 and eden(grid,2)=<gam-1}n>/n_0
   !================================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !================================
   call interp_realloc(interp, np, sp_loc%pick_dimensions())
   !================================
   allocate( gam(np) )

   spline = 2
   select case (ndim)
   case (1)
    j2 = 1

    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 1, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )

    call qden_1d_wgh( xx(1:np, 1:1), interp, mempool )

    associate( ax0 => interp%coeff_x_rank2, &
               i => interp%ix_rank2, &
               weight => sp_loc%call_component( W_COMP, lb=1, ub=np) )

     !==========================
     ! Warning: this call must be after qden_1d_spline since
     ! in that routine the 1d arrays are used
     call array_realloc_1d(mempool%mp_xx_1d_A, np)
     gam => mempool%mp_xx_1d_A

     gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np)
     gam(1:np) = sqrt( one_dp + gam(1:np))
     do i1 = 0, spline
      ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
     end do
     do n = 1, np
      do i1 = 0, spline
       i2 = i(n) + i1
       dvol = ax0(i1, n)
       eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*sp_loc%pick_charge()
       eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam(n)-1)*dvol
      end do
     end do
           
    end associate
   case (2)

    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    
    call qden_2d_wgh( xx(1:np, 1:2), interp, mempool )

    associate( ax0 => interp%coeff_x_rank2, &
               ay0 => interp%coeff_y_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               weight => sp_loc%call_component( W_COMP, lb=1, ub=np) )

     !==========================
     ! Warning: this call must be after qden_1d_spline since
     ! in that routine the 1d arrays are used
     call array_realloc_1d(mempool%mp_xx_1d_A, np)
     gam => mempool%mp_xx_1d_A

    if (curr_ndim==2) then

     gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PY_COMP, lb=1, ub=np)*sp_loc%call_component( PY_COMP, lb=1, ub=np)

     gam(1:np) = sqrt(one_dp + gam(1:np))
     do i1 = 0, spline
      ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
     end do
     do n = 1, np
      do j1 = 0, spline
       j2 = j(n) + j1
       do i1 = 0, spline
        i2 = i(n) + i1
        dvol = ax0(i1, n)*ay0(j1, n)
        eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*sp_loc%pick_charge()
        eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam(n)-1.)*dvol
       end do
      end do
     end do
    end if
    if (curr_ndim==3) then

     gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PY_COMP, lb=1, ub=np)*sp_loc%call_component( PY_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PZ_COMP, lb=1, ub=np)*sp_loc%call_component( PZ_COMP, lb=1, ub=np)

     gam(1:np) = sqrt(one_dp + gam(1:np))
     do i1 = 0, spline
      ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
     end do
     do n = 1, np
      do j1 = 0, spline
       j2 = j(n) + j1
       do i1 = 0, spline
        i2 = i(n) + i1
        dvol = ax0(i1, n)*ay0(j1, n)
        eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*sp_loc%pick_charge()
        eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam(n) - 1.)*dvol
       end do
      end do
     end do
    end if

    end associate

   case (3)

    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
    
    call qden_3d_wgh( xx(1:np, 1:3), interp, mempool )

    associate( ax0 => interp%coeff_x_rank2, &
               ay0 => interp%coeff_y_rank2, &
               az0 => interp%coeff_z_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               k => interp%iz_rank2, &
               weight => sp_loc%call_component( W_COMP, lb=1, ub=np) )

    !==========================
    ! Warning: this call must be after qden_1d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    gam => mempool%mp_xx_1d_A

    gam(1:np) = sp_loc%call_component( PX_COMP, lb=1, ub=np)*sp_loc%call_component( PX_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PY_COMP, lb=1, ub=np)*sp_loc%call_component( PY_COMP, lb=1, ub=np) + &
     sp_loc%call_component( PZ_COMP, lb=1, ub=np)*sp_loc%call_component( PZ_COMP, lb=1, ub=np)

    gam(1:np) = sqrt(one_dp + gam(1:np))
    do i1 = 0, spline
     ax0(i1, 1:np) = weight(1:np)*ax0(i1, 1:np)
    end do
    do n = 1, np
     do k1 = 0, spline
      k2 = k(n) + k1
      do j1 = 0, spline
       j2 = j(n) + j1
       dvol = az0(k1, n)*ay0(j1, n)
       do i1 = 0, spline
        i2 = i(n) + i1
        eden(i2, j2, k2, 1) = eden(i2, j2, k2, 1) + ax0(i1, n)*dvol*sp_loc%pick_charge()
        eden(i2, j2, k2, 2) = eden(i2, j2, k2, 2) + &
          (gam(n) - 1.)*ax0(i1, n)*dvol
       end do
      end do
     end do
    end do

    end associate
   end select
   !============================
  end subroutine
  !=================================================
  subroutine set_grid_den_energy_old(sp_loc, pt, eden, np)

   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)
   real(dp), intent(inout) :: eden(:, :, :, :)
   integer, intent(in) :: np
   real(dp) :: dvol, gam
   real(dp) :: ax0(0:2), ay0(0:2), az0(0:2), xp(3), pp(3)
   integer :: i, j, k, i1, j1, k1, i2, j2, k2, n, ch, spl
   !======================
   !   Computes eden(grid,1)= n/n_0 and eden(grid,2)=<gam-1}n>/n_0
   !================================================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   ax0(0:2) = zero_dp
   ay0(0:2) = zero_dp
   az0(0:2) = zero_dp
   spl = 2
   ch = size(sp_loc%part, 2)
   if (np < 1) return
   select case (ndim)
   case (1)
    j2 = 1
    do n = 1, np
     xp(1) = dx_inv*(sp_loc%part(n, 1) - xmn)
     pp(1:2) = sp_loc%part(n, 3:4)
     gam = sqrt(pp(1)*pp(1) + pp(2)*pp(2) + 1.)
     wgh_cmp = sp_loc%part(n, ch)

     call qden_1d_wgh( xp, interp )

     ax0(0:2) = wgh*interp%coeff_x(0:2)
     i = interp%ix

     do i1 = 0, 2
      i2 = i + i1
      dvol = ax0(i1)
      eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*charge
      eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam - 1.)*dvol
     end do
    end do
   case (2)
    do n = 1, np
     pt(n, 1:ch) = sp_loc%part(n, 1:ch)
    end do
    call set_local_2d_positions(pt, 1, np)

    call set_part_gamma(pt, np, 2)

    do n = 1, np
     xp(1:2) = pt(n, 1:2)
     gam = pt(n, 4)
     wgh_cmp = pt(n, ch)

     call qden_2d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)

     i = interp%ix
     j = interp%iy

     ax0(0:2) = wgh*ax0(0:2)
     do j1 = 0, 2
      j2 = j + j1
      do i1 = 0, 2
       i2 = i + i1
       dvol = ax0(i1)*ay0(j1)
       eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol*charge
       eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + (gam - 1.)*dvol
      end do
     end do
    end do
   case (3)
    ch = 7
    do n = 1, np
     pt(n, 1:ch) = sp_loc%part(n, 1:ch)
    end do
    call set_local_3d_positions(pt, 1, np)

    call set_part_gamma(pt, np, 3)
    do n = 1, np
     xp(1:3) = pt(n, 1:3)
     wgh_cmp = pt(n, ch)
     gam = pt(n, 4)

     call qden_3d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)
     az0(0:2) = interp%coeff_z(0:2)

     i = interp%ix
     j = interp%iy
     k = interp%iz

     ax0(0:2) = wgh*ax0(0:2)
     do k1 = 0, spl
      k2 = k + k1
      do j1 = 0, spl
       j2 = j + j1
       dvol = az0(k1)*ay0(j1)
       do i1 = 0, spl
        i2 = i + i1
        eden(i2, j2, k2, 1) = eden(i2, j2, k2, 1) + ax0(i1)*dvol*charge
        eden(i2, j2, k2, 2) = eden(i2, j2, k2, 2) + &
                              (gam - 1.)*ax0(i1)*dvol
       end do
      end do
     end do
    end do
   end select
   !============================
  end subroutine
  !==============================================
  subroutine set_grid_charge_and_jx(sp_loc, pt, eden, np)

   type(species), intent(in) :: sp_loc
   real(dp), intent(inout) :: pt(:, :)
   real(dp), intent(inout) :: eden(:, :, :, :)
   integer, intent(in) :: np
   real(dp) :: dvol, gam
   real(dp) :: ax0(0:2), ay0(0:2), az0(0:2), vx, xp(3)
   integer :: i, j, k, i1, j1, k1, i2, j2, k2, n, ch, spl
   real(sp) :: wght
   !======================
   !   Computes charge density and Jx current density at t^n current time
   !================================================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   ax0(0:2) = zero_dp
   ay0(0:2) = zero_dp
   az0(0:2) = zero_dp
   spl = 2
   ch = size(sp_loc%part, 2)
   if (np < 1) return
   select case (ndim)
   case (2)
    do n = 1, np
     pt(n, 1:ch) = sp_loc%part(n, 1:ch)
    end do
    call set_local_2d_positions(pt, 1, np)
    do n = 1, np
     xp(1:2) = pt(n, 1:2)
     wgh_cmp = pt(n, ch)
     gam = sqrt(1.+pt(n, 3)*pt(n, 3) + pt(n, 4)*pt(n, 4))
     vx = pt(n, 3)/gam
     wght = charge*wgh

     call qden_2d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)

     i = interp%ix
     j = interp%iy

     ax0(0:2) = wght*ax0(0:2)
     do j1 = 0, 2
      j2 = j + j1
      do i1 = 0, 2
       i2 = i + i1
       dvol = ax0(i1)*ay0(j1)
       eden(i2, j2, 1, 1) = eden(i2, j2, 1, 1) + dvol
       eden(i2, j2, 1, 2) = eden(i2, j2, 1, 2) + vx*dvol
      end do
     end do
    end do
   case (3)
    ch = 7
    do n = 1, np
     pt(n, 1:ch) = sp_loc%part(n, 1:ch)
    end do
    call set_local_3d_positions(pt, 1, np)

    do n = 1, np
     xp(1:3) = pt(n, 1:3)
     wgh_cmp = pt(n, ch)
     gam = sqrt(1.+pt(n, 4)*pt(n, 4) + pt(n, 5)*pt(n, 5) + pt(n, 6)*pt(n, 6))
     vx = pt(n, 4)/gam
     wght = charge*wgh

     call qden_3d_wgh( xp, interp )

     ax0(0:2) = interp%coeff_x(0:2)
     ay0(0:2) = interp%coeff_y(0:2)
     az0(0:2) = interp%coeff_z(0:2)

     i = interp%ix
     j = interp%iy
     k = interp%iz

     ax0(0:2) = wght*ax0(0:2)
     do k1 = 0, spl
      k2 = k + k1
      do j1 = 0, spl
       j2 = j + j1
       dvol = az0(k1)*ay0(j1)
       do i1 = 0, spl
        i2 = i + i1
        eden(i2, j2, k2, 1) = eden(i2, j2, k2, 1) + ax0(i1)*dvol
        eden(i2, j2, k2, 2) = eden(i2, j2, k2, 2) + vx*ax0(i1)*dvol
       end do
      end do
     end do
    end do
   end select
  end subroutine

 end module
