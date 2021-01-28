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

 module grid_part_connect

  use fstruct_data
  use grid_part_lib
  use memory_pool
  use particles_def
  use pstruct_data
#if !defined(OLD_SPECIES)
  use grid_tracking_part_connect
#endif

  implicit none
  interface set_part1d_acc
   module procedure set_part1d_acc_new
   module procedure set_part1d_acc_old
  end interface

  interface set_part2d_hcell_acc
   module procedure set_part2d_hcell_acc_new
   module procedure set_part2d_hcell_acc_old
  end interface

  interface set_part3d_hcell_acc
   module procedure set_part3d_hcell_acc_new
   module procedure set_part3d_hcell_acc_old
  end interface

  interface set_ion_efield
   module procedure set_ion_efield_new
   module procedure set_ion_efield_old
  end interface

  interface set_env_acc
   module procedure set_env_acc_new
   module procedure set_env_acc_old
  end interface

  interface set_ion_env_field
   module procedure set_ion_env_field_new
   module procedure set_ion_env_field_old
  end interface

  interface set_env_grad_interp
   module procedure set_env_grad_interp_new
   module procedure set_env_grad_interp_old
  end interface

  interface set_env_density
   module procedure set_env_density_new
   module procedure set_env_density_old
  end interface

  interface esirkepov_2d_curr
   module procedure esirkepov_2d_curr_new
   module procedure esirkepov_2d_curr_old
  end interface

  interface esirkepov_3d_curr
   module procedure esirkepov_3d_curr_new
   module procedure esirkepov_3d_curr_old
  end interface

  interface ncdef_2d_curr
   module procedure ncdef_2d_curr_new
   module procedure ncdef_2d_curr_old
  end interface

  interface ncdef_3d_curr
   module procedure ncdef_3d_curr_new
   module procedure ncdef_3d_curr_old
  end interface

  !========= SECTION FOR FIELDS ASSIGNEMENT
 contains

  subroutine set_part1d_acc_new(ef, sp_loc, pt, np, ndf, mempool)
   !To be checked, actually never used
   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: pt
   integer, intent (in) :: np, ndf
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null(), ap => null()
   type(interp_coeff), pointer :: interp => null()
   integer :: spl_cell, spl_h_cell
   integer :: i1, i2, j2, n
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   call mp_xx_realloc(mempool%mp_xx_2d_A, np, 1, mempool)
   xx => mempool%mp_xx_2d_A
   !================================
   spl_cell = 2
   spl_h_cell = 2
   select case (ndf)
   case (3)

    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 3, mempool)
    ap => mempool%mp_xx_2d_B
    ap(1:np, 1:3) = zero_dp
    j2 = 1
    xx(1:np, 1) = sp_loc%x(1:np)
    
    call qqh_1d_spline( xx(1:np, 1:1), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               axh => interp%h_coeff_x_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2 )

    do i1 = 0, spl_cell
     do n = 1, np
      i2 = i1 + ih(n)
      ap(n, 1) = ap(n, 1) + axh(n, i1)*ef(i2, j2, 1, 1) !Ex(i+1/2)
      ap(n, 3) = ap(n, 3) + axh(n, i1)*ef(i2, j2, 1, 3) !Bz(i+1/2)
      i2 = i1 + i(n)
      ap(n, 2) = ap(n, 2) + ax1(n, i1)*ef(i2, j2, 1, 2) !Ey(i)
     end do
    end do

    end associate
    call pt%set_component( ap(1:np, 1), EX_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 2), EY_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 3), BZ_COMP, lb=1, ub=np)
   !========================
   case (6)
    j2 = 1
    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 6, mempool)
    ap => mempool%mp_xx_2d_B
    ap(1:np, 1:6) = zero_dp
    xx(1:np, 1) = sp_loc%x(1:np) !the current particle positions
    
    call qqh_1d_spline( xx(1:np, 1:1), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               axh => interp%h_coeff_x_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2 )

    do i1 = 0, spl_cell
     do n = 1, np
      i2 = i1 + ih(n)
      ap(n, 1) = ap(n, 1) + axh(n, i1)*ef(i2, j2, 1, 1) !Ex(i+1/2)
      ap(n, 5) = ap(n, 5) + axh(n, i1)*ef(i2, j2, 1, 5) !By(i+1/2)
      ap(n, 6) = ap(n, 6) + axh(n, i1)*ef(i2, j2, 1, 6) !Bz(i+1/2)
      i2 = i1 + i(n)
      ap(n, 2) = ap(n, 2) + ax1(n, i1)*ef(i2, j2, 1, 2) !Ey(i)
      ap(n, 3) = ap(n, 3) + ax1(n, i1)*ef(i2, j2, 1, 3) !Ez(i)
      ap(n, 4) = ap(n, 4) + ax1(n, i1)*ef(i2, j2, 1, 4) !Bx(i)
     end do
    end do

    end associate

    call pt%set_component( ap(1:np, 1), EX_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 2), EY_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 3), EZ_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 4), BX_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 5), BY_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 6), BZ_COMP, lb=1, ub=np)
   end select
  end subroutine

  !===========================
  subroutine set_part1d_acc_old(ef, sp_loc, pt, np, ndf)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np, ndf
   type(interp_coeff), allocatable :: interp
   real (dp) :: xp1(3), ap(6)
   real (dp) :: axh(0:2), ax1(0:2)
   integer :: i, ih, i1, i2, j2, n
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndf)
   case (3)
    j2 = 1
    do n = 1, np
     ap(1:3) = zero_dp
     xp1(1) = sp_loc%part(n, 1) !the current particle positions

     call qqh_1d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     axh(0:2) = interp%h_coeff_x(0:2)

     i = interp%ix
     ih = interp%ihx

     do i1 = 0, 2
      i2 = i1 + ih
      ap(1) = ap(1) + axh(i1)*ef(i2, j2, 1, 1) !Ex(i+1/2)
      ap(3) = ap(3) + axh(i1)*ef(i2, j2, 1, 3) !Bz(i+1/2)
      i2 = i + i1
      ap(2) = ap(2) + ax1(i1)*ef(i2, j2, 1, 2) !Ey(i)
     end do
     pt(n, 1:3) = ap(1:3)
    end do
   !========================
   case (6)
    j2 = 1
    do n = 1, np
     ap(1:6) = zero_dp
     xp1(1) = sp_loc%part(n, 1) !the current particle positions

     call qqh_1d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     axh(0:2) = interp%h_coeff_x(0:2)

     i = interp%ix
     ih = interp%ihx

     do i1 = 0, 2
      i2 = i1 + ih
      ap(1) = ap(1) + axh(i1)*ef(i2, j2, 1, 1) !Ex
      ap(5) = ap(5) + axh(i1)*ef(i2, j2, 1, 5) !By
      ap(6) = ap(6) + axh(i1)*ef(i2, j2, 1, 6) !Bz
     end do
     do i1 = 0, 2
      i2 = i + i1
      ap(2) = ap(2) + ax1(i1)*ef(i2, j2, 1, 2) !Ey
      ap(3) = ap(3) + ax1(i1)*ef(i2, j2, 1, 3) !Ez
      ap(4) = ap(4) + ax1(i1)*ef(i2, j2, 1, 4) !Bx
     end do
     pt(n, 1:6) = ap(1:6)
    end do
   end select
  end subroutine
  !===========================
  subroutine set_part2d_hcell_acc_new(ef, sp_loc, pt, np, ndf, mempool)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: pt
   integer, intent (in) :: np, ndf
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null(), ap => null()
   type(interp_coeff), pointer :: interp => null()
   real (dp) :: dvol, dvol1
   integer :: i1, j1, i2, j2, n, spl_cell, spl_h_cell
   !================================
   ! Uses quadratic or linear shapes depending on staggering
   ! ndf is the number of field component

   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=====================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
   xx => mempool%mp_xx_2d_A
   !================================
   spl_cell = 2
   spl_h_cell = 2
   select case (ndf) !Field components
   case (3)
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, 3, mempool)
    ap => mempool%mp_xx_2d_B
    ap(1:np, 1:3) = zero_dp
    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )

    call qqh_2d_spline( xx(1:np, 1:2), interp, mempool )
    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               axh => interp%h_coeff_x_rank2, &
               ayh => interp%h_coeff_y_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2 )
 
    do n = 1, np
     do j1 = 0, spl_cell
      j2 = j(n) + j1
      dvol = ay1(j1, n)
      do i1 = 0, spl_h_cell
       i2 = i1 + ih(n)
       dvol1 = axh(i1, n)*dvol
       ap(n, 1) = ap(n, 1) + dvol1*ef(i2, j2, 1, 1) !Ex(i+1/2,j)
      end do
     end do
     do j1 = 0, spl_h_cell
      j2 = jh(n) + j1
      dvol = ayh(j1, n)
      do i1 = 0, spl_cell
       i2 = i(n) + i1
       dvol1 = ax1(i1, n)*dvol
       ap(n, 2) = ap(n, 2) + dvol1*ef(i2, j2, 1, 2) !Ey(i,j+1/2)
      end do
      do i1 = 0, spl_h_cell
       i2 = i1 + ih(n)
       dvol1 = axh(i1, n)*dvol
       ap(n, 3) = ap(n, 3) + dvol1*ef(i2, j2, 1, 3) !Bz(i+1/2,j+1/2)
      end do
     end do
    end do

    end associate

    call pt%set_component( ap(1:np, 1), EX_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 2), EY_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 3), BZ_COMP, lb=1, ub=np)
    !==============
   case (6)
    !=====================
    call mp_xx_realloc( mempool%mp_xx_2d_B, np, 6, mempool)
    ap => mempool%mp_xx_2d_B
    ap(1:np, 1:6) = zero_dp
    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )

    call qqh_2d_spline( xx(1:np, 1:2), interp, mempool )
    
    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               axh => interp%h_coeff_x_rank2, &
               ayh => interp%h_coeff_y_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2 )

    do n = 1, np
     do j1 = 0, spl_cell
      j2 = j(n) + j1
      dvol = ay1(j1, n)
      do i1 = 0, spl_h_cell
       i2 = i1 + ih(n)
       dvol1 = axh(i1, n)*dvol
       ap(n, 1) = ap(n, 1) + dvol1*ef(i2, j2, 1, 1) !Ex(i+1/2,j)
       ap(n, 5) = ap(n, 5) + dvol1*ef(i2, j2, 1, 5) !By(i+1/2,j)
      end do
      do i1 = 0, spl_cell
       i2 = i1 + i(n)
       dvol1 = ax1(i1, n)*dvol
       ap(n, 3) = ap(n, 3) + dvol1*ef(i2, j2, 1, 3) !Ez(i,j,k+1/2)
      end do
     end do
     do j1 = 0, spl_h_cell
      j2 = jh(n) + j1
      dvol = ayh(j1, n)
      do i1 = 0, spl_cell
       i2 = i(n) + i1
       dvol1 = ax1(i1, n)*dvol
       ap(n, 2) = ap(n, 2) + dvol1*ef(i2, j2, 1, 2) !Ey(i,j+1/2)
       ap(n, 4) = ap(n, 4) + dvol1*ef(i2, j2, 1, 4) !Bx(i,j+1/2)
      end do
      do i1 = 0, spl_h_cell
       i2 = i1 + ih(n)
       dvol1 = axh(i1, n)*dvol
       ap(n, 6) = ap(n, 6) + dvol1*ef(i2, j2, 1, 6) !Bz(i+1/2,j+1/2)
      end do
     end do
    end do

    end associate

    call pt%set_component( ap(1:np, 1), EX_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 2), EY_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 3), EZ_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 4), BX_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 5), BY_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 6), BZ_COMP, lb=1, ub=np)
   end select
   !=====================
  end subroutine
  !====================================
  subroutine set_part2d_hcell_acc_old(ef, sp_loc, pt, np, ndf)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np, ndf
   type(interp_coeff), allocatable :: interp
   real (dp) :: dvol, dvol1
   real (dp) :: xp1(3), ap(6)
   real (dp) :: axh(0:2), ax1(0:2)
   real (dp) :: ayh(0:2), ay1(0:2)

   integer :: i, ih, j, jh, i1, j1, i2, j2, n
   !================================
   ! Uses quadratic or linear shapes depending on staggering
   ! ndf is the number of field component
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   xp1 = zero_dp
   do n = 1, np
    pt(n, 1:3) = sp_loc%part(n, 1:3)
   end do
   call set_local_2d_positions(pt, 1, np)

   select case (ndf) !Field components
   case (3)
    do n = 1, np
     ap(1:3) = zero_dp
     xp1(1:2) = pt(n, 1:2)

     call qqh_2d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     axh(0:2) = interp%h_coeff_x(0:2)
     ayh(0:2) = interp%h_coeff_y(0:2)

     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy

     do j1 = 0, 2
      j2 = j + j1
      dvol = ay1(j1)
      do i1 = 0, 2
       i2 = i1 + ih
       dvol1 = axh(i1)*dvol
       ap(1) = ap(1) + dvol1*ef(i2, j2, 1, 1) !Ex(i+1/2,j)
      end do
     end do
     do j1 = 0, 2
      j2 = jh + j1
      dvol = ayh(j1)
      do i1 = 0, 2
       i2 = i + i1
       dvol1 = ax1(i1)*dvol
       ap(2) = ap(2) + dvol1*ef(i2, j2, 1, 2) !Ey(i,j+1/2)
      end do
      do i1 = 0, 2
       i2 = i1 + ih
       dvol1 = axh(i1)*dvol
       ap(3) = ap(3) + dvol1*ef(i2, j2, 1, 3) !Bz(i+1/2,j+1/2)
      end do
     end do
     pt(n, 1:3) = ap(1:3)
    end do
    !==============
   case (6)
    !=====================
    do n = 1, np
     ap(1:6) = zero_dp
     xp1(1:2) = pt(n, 1:2)

     call qqh_2d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     axh(0:2) = interp%h_coeff_x(0:2)
     ayh(0:2) = interp%h_coeff_y(0:2)

     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy

     do j1 = 0, 2
      j2 = j + j1
      dvol = ay1(j1)
      do i1 = 0, 2
       i2 = i1 + ih
       dvol1 = axh(i1)*dvol
       ap(1) = ap(1) + dvol1*ef(i2, j2, 1, 1) !Ex(i+1/2,j)
       ap(5) = ap(5) + dvol1*ef(i2, j2, 1, 5) !By(i+1/2,j)
      end do
      do i1 = 0, 2
       i2 = i1 + i
       dvol1 = ax1(i1)*dvol
       ap(3) = ap(3) + dvol1*ef(i2, j2, 1, 3) !Ez(i,j,k+1/2)
      end do
     end do
     do j1 = 0, 2
      j2 = jh + j1
      dvol = ayh(j1)
      do i1 = 0, 2
       i2 = i + i1
       dvol1 = ax1(i1)*dvol
       ap(2) = ap(2) + dvol1*ef(i2, j2, 1, 2) !Ey(i,j+1/2)
       ap(4) = ap(4) + dvol1*ef(i2, j2, 1, 4) !Bx(i,j+1/2)
      end do
      do i1 = 0, 2
       i2 = i1 + ih
       dvol1 = axh(i1)*dvol
       ap(6) = ap(6) + dvol1*ef(i2, j2, 1, 6) !Bz(i+1/2,j+1/2)
      end do
     end do
     pt(n, 1:6) = ap(1:6)
    end do
   end select
   !=====================
  end subroutine
  !====================================
  subroutine set_part3d_hcell_acc_new(ef, sp_loc, pt, np, mempool)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: pt
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null(), ap => null()
   type(interp_coeff), pointer :: interp => null()
   integer, intent (in) :: np
   real (dp) :: dvol
   integer :: i1, j1, i2, j2, k1, k2, n, spl_cell, spl_h_cell

   !===============================================
   !Staggered shapes
   ! Linear shape at half-index
   ! Quadratic shape at integer index
   !====================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=============================================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
   !================================
   call mp_xx_realloc(mempool%mp_xx_2d_B, np, 6, mempool)
   xx => mempool%mp_xx_2d_A
   ap => mempool%mp_xx_2d_B
   ap(1:np, 1:6) = zero_dp
   xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
   xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
   xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )

   spl_cell = 2
   spl_h_cell = 2
   call qqh_3d_spline( xx(1:np, 1:3), interp, mempool )
   !==========================

   associate( ax1 => interp%coeff_x_rank2, &
              ay1 => interp%coeff_y_rank2, &
              az1 => interp%coeff_z_rank2, &
              axh => interp%h_coeff_x_rank2, &
              ayh => interp%h_coeff_y_rank2, &
              azh => interp%h_coeff_z_rank2, &
              i => interp%ix_rank2, &
              ih => interp%ihx_rank2, &
              j => interp%iy_rank2, &
              jh => interp%ihy_rank2, &
              k => interp%iz_rank2, &
              kh => interp%ihz_rank2 )
    
   do n = 1, np
    ! Ex(i+1/2,j,k)
    !==============
    !==============
    ! Ey(i,j+1/2,k)
    !==============
    !==============
    ! Bz(i+1/2,j+1/2,k)
    !==============
    do k1 = 0, spl_cell
     k2 = k(n) + k1
     do j1 = 0, spl_cell
      j2 = j(n) + j1
      dvol = ay1(j1, n)*az1(k1, n)
      do i1 = 0, spl_h_cell
       i2 = i1 + ih(n)
       ap(n, 1) = ap(n, 1) + axh(i1, n)*dvol*ef(i2, j2, k2, 1)
      end do
     end do
     do j1 = 0, spl_h_cell
      j2 = jh(n) + j1
      dvol = ayh(j1, n)*az1(k1, n)
      do i1 = 0, spl_cell
       i2 = i(n) + i1
       ap(n, 2) = ap(n, 2) + ax1(i1, n)*dvol*ef(i2, j2, k2, 2)
      end do
      do i1 = 0, spl_h_cell
       i2 = i1 + ih(n)
       ap(n, 6) = ap(n, 6) + axh(i1, n)*dvol*ef(i2, j2, k2, 6)
      end do
     end do
    end do
    !==============
    ! Bx(i,j+1/2,k+1/2)
    !==============
    !==============
    ! By(i+1/2,j,k+1/2)
    !==============
    !==============
    ! Ez(i,j,k+1/2)
    !==============

    do k1 = 0, spl_h_cell
     k2 = kh(n) + k1
     do j1 = 0, spl_h_cell
      j2 = jh(n) + j1
      dvol = ayh(j1, n)*azh(k1, n)
      do i1 = 0, spl_cell
       i2 = i1 + i(n)
       ap(n, 4) = ap(n, 4) + ax1(i1, n)*dvol*ef(i2, j2, k2, 4)
      end do
     end do
     do j1 = 0, spl_cell
      j2 = j(n) + j1
      dvol = ay1(j1, n)*azh(k1, n)
      do i1 = 0, spl_h_cell
       i2 = ih(n) + i1
       ap(n, 5) = ap(n, 5) + axh(i1, n)*dvol*ef(i2, j2, k2, 5)
      end do
      do i1 = 0, spl_cell
       i2 = i1 + i(n)
       ap(n, 3) = ap(n, 3) + ax1(i1, n)*dvol*ef(i2, j2, k2, 3)
      end do
     end do
    end do
   end do

   end associate
   call pt%set_component( ap(1:np, 1), EX_COMP, lb=1, ub=np)
   call pt%set_component( ap(1:np, 2), EY_COMP, lb=1, ub=np)
   call pt%set_component( ap(1:np, 3), EZ_COMP, lb=1, ub=np)
   call pt%set_component( ap(1:np, 4), BX_COMP, lb=1, ub=np)
   call pt%set_component( ap(1:np, 5), BY_COMP, lb=1, ub=np)
   call pt%set_component( ap(1:np, 6), BZ_COMP, lb=1, ub=np)

  end subroutine
  !======================================
  !====================================
  subroutine set_part3d_hcell_acc_old(ef, sp_loc, pt, np)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np
   type(interp_coeff), allocatable :: interp
   real (dp) :: dvol, ap(6), xp1(3)
   real (dp) :: axh(0:2), ax1(0:2)
   real (dp) :: ayh(0:2), ay1(0:2)
   real (dp) :: azh(0:2), az1(0:2)
   integer :: i, ih, j, jh, i1, j1, i2, j2, k, kh, k1, k2, n

   !===============================================
   !Staggered shapes
   ! Linear shape at half-index
   ! Quadratic shape at integer index
   !====================================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   do n = 1, np
    pt(n, 1:3) = sp_loc%part(n, 1:3)
   end do
   call set_local_3d_positions(pt, 1, np)
   !==========================
   do n = 1, np
    ap(1:6) = zero_dp
    xp1(1:3) = pt(n, 1:3)

    call qqh_3d_spline( xp1, interp )

    ax1(0:2) = interp%coeff_x(0:2)
    ay1(0:2) = interp%coeff_y(0:2)
    az1(0:2) = interp%coeff_z(0:2)
    axh(0:2) = interp%h_coeff_x(0:2)
    ayh(0:2) = interp%h_coeff_y(0:2)
    azh(0:2) = interp%h_coeff_z(0:2)

    i = interp%ix
    ih = interp%ihx
    j = interp%iy
    jh = interp%ihy
    k = interp%iz
    kh = interp%ihz
    ! Ex(i+1/2,j,k)
    !==============
    !==============
    ! Ey(i,j+1/2,k)
    !==============
    !==============
    ! Bz(i+1/2,j+1/2,k)
    !==============
    do k1 = 0, 2
     k2 = k + k1
     do j1 = 0, 2
      j2 = j + j1
      dvol = ay1(j1)*az1(k1)
      do i1 = 0, 2
       i2 = i1 + ih
       ap(1) = ap(1) + axh(i1)*dvol*ef(i2, j2, k2, 1)
      end do
     end do
     do j1 = 0, 2
      j2 = jh + j1
      dvol = ayh(j1)*az1(k1)
      do i1 = 0, 2
       i2 = i + i1
       ap(2) = ap(2) + ax1(i1)*dvol*ef(i2, j2, k2, 2)
      end do
      do i1 = 0, 2
       i2 = i1 + ih
       ap(6) = ap(6) + axh(i1)*dvol*ef(i2, j2, k2, 6)
      end do
     end do
    end do
    !==============
    ! Bx(i,j+1/2,k+1/2)
    !==============
    !==============
    ! By(i+1/2,j,k+1/2)
    !==============
    !==============
    ! Ez(i,j,k+1/2)
    !==============

    do k1 = 0, 2
     k2 = kh + k1
     do j1 = 0, 2
      j2 = jh + j1
      dvol = ayh(j1)*azh(k1)
      do i1 = 0, 2
       i2 = i1 + i
       ap(4) = ap(4) + ax1(i1)*dvol*ef(i2, j2, k2, 4)
      end do
     end do
     do j1 = 0, 2
      j2 = j + j1
      dvol = ay1(j1)*azh(k1)
      do i1 = 0, 2
       i2 = ih + i1
       ap(5) = ap(5) + axh(i1)*dvol*ef(i2, j2, k2, 5)
      end do
      do i1 = 0, 2
       i2 = i1 + i
       ap(3) = ap(3) + ax1(i1)*dvol*ef(i2, j2, k2, 3)
      end do
     end do
    end do
    pt(n, 1:6) = ap(1:6)
   end do

  end subroutine
  !======================================

  subroutine set_ion_efield_new(ef, sp_loc, pt, np, mempool)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: pt
   integer, intent (in) :: np
   type(memory_pool_t), pointer, intent(in) :: mempool
   type(interp_coeff), pointer :: interp => null()
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null()
   real(dp), pointer, contiguous, dimension(:) :: ef_sqr => null()

   real (dp) :: dvol, ex, ey, ez
   integer :: n, ip1, jp1, kp1, ip2, jp2, kp2

   !===============================================
   ! qlh_spline()      Linear shape at half-index quadratic shape at integer index
   ! qqh_spline()      quadratic shape at half-index and at integer index
   !                 For field assignements
   !====================================
   ! fields are at t^n

   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=============================================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   !================================
   
   select case (ndim)
   case (2)
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    xx => mempool%mp_xx_2d_A
    kp2 = 1
    !==========================
    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    call qqh_2d_spline( xx(1:np, 1:2), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               axh => interp%h_coeff_x_rank2, &
               ayh => interp%h_coeff_y_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2 )
    !==========================
    ! Warning: this call must be after qqh_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    ef_sqr => mempool%mp_xx_1d_A
    ef_sqr(1:np) = zero_dp
    do n = 1, np
     ! Ex(i+1/2,j,k)
     !==============
     !==============
     ! Ey(i,j+1/2,k)
     !==============
     do jp1 = 0, 2
      jp2 = j(n) + jp1
      dvol = ay1(jp1, n)
      do ip1 = 0, 2
       ip2 = ih(n) + ip1
       ex = ef(ip2, jp2, kp2, 1)
       ef_sqr(n) = ef_sqr(n) + axh(ip1, n)*dvol*ex*ex
      end do
     end do
     do jp1 = 0, 2
      jp2 = jh(n) + jp1
      dvol = ayh(jp1, n)
      do ip1 = 0, 2
       ip2 = i(n) + ip1
       ey = ef(ip2, jp2, kp2, 2)
       ef_sqr(n) = ef_sqr(n) + ax1(ip1, n)*dvol*ey*ey
      end do
     end do
     !==============
    end do

    end associate
    call pt%set_component( ef_sqr(1:np), &
     E_SQUARED, lb=1, ub=np) !Ex(p)^2 + Ey(p)^2
    !=======================================

   case (3)
    !==========================
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    xx => mempool%mp_xx_2d_A
    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )

    call qqh_3d_spline( xx(1:np, 1:3), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               az1 => interp%coeff_z_rank2, &
               axh => interp%h_coeff_x_rank2, &
               ayh => interp%h_coeff_y_rank2, &
               azh => interp%h_coeff_z_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2, &
               k => interp%iz_rank2, &
               kh => interp%ihz_rank2 )
    !==========================
    ! Warning: this call must be after qqh_3d_spline since
    ! in that routine the 1d arrays are used
     call array_realloc_1d(mempool%mp_xx_1d_A, np)
     ef_sqr => mempool%mp_xx_1d_A
     ef_sqr(1:np) = zero_dp
    !==========================
    ! Here Quadratic shapes are used
    do n = 1, np
     ! Ex(i+1/2,j,k)
     !==============
     !==============
     ! Ey(i,j+1/2,k)
     !==============
     do kp1 = 0, 2
      kp2 = k(n) + kp1
      do jp1 = 0, 2
       jp2 = j(n) + jp1
       dvol = ay1(jp1, n)*az1(kp1, n)
       do ip1 = 0, 2
        ip2 = ip1 + ih(n)
        ex = ef(ip2, jp2, kp2, 1)
        ef_sqr(n) = ef_sqr(n) + axh(ip1, n)*dvol*ex*ex
       end do
      end do
      do jp1 = 0, 2
       jp2 = jh(n) + jp1
       dvol = ayh(jp1, n)*az1(kp1, n)
       do ip1 = 0, 2
        ip2 = i(n) + ip1
        ey = ef(ip2, jp2, kp2, 2)
        ef_sqr(n) = ef_sqr(n) + ax1(ip1, n)*dvol*ey*ey
       end do
      end do
     end do
     !==============
     ! Ez(i,j,k+1/2)
     !==============
     do kp1 = 0, 2
      kp2 = kh(n) + kp1
      do jp1 = 0, 2
       jp2 = j(n) + jp1
       dvol = ay1(jp1, n)*azh(kp1, n)
       do ip1 = 0, 2
        ip2 = ip1 + i(n)
        ez = ef(ip2, jp2, kp2, 3)
        ef_sqr(n) = ef_sqr(n) + ax1(ip1, n)*dvol*ez*ez
       end do
      end do
     end do
    end do

    end associate
    call pt%set_component( ef_sqr(1:np), &
    E_SQUARED, lb=1, ub=np) !Ex(p)^2 + Ey(p)^2 + Ez(p)^2

   end select
   !================================
  end subroutine

  subroutine set_ion_efield_old(ef, sp_loc, pt, np)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np
   type(interp_coeff), allocatable :: interp
   real (dp) :: ef_sqr, dvol, ex, ey, ez
   real (dp) :: axh(0:2), ax1(0:2)
   real (dp) :: ayh(0:2), ay1(0:2)
   real (dp) :: azh(0:2), az1(0:2)
   real (dp) :: xp1(3)
   integer :: i, ih, j, jh, k, kh
   integer :: n, ip1, jp1, kp1, ip2, jp2, kp2

   !===============================================
   ! qlh_spline()      Linear shape at half-index quadratic shape at integer index
   ! qqh_spline()      quadratic shape at half-index and at integer index
   !                 For field assignements
   !====================================
   ! fields are at t^n
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   case (2)
    kp2 = 1
    do n = 1, np
     pt(n, 1:2) = sp_loc%part(n, 1:2)
    end do
    call set_local_2d_positions(pt, 1, np)
    !==========================
    do n = 1, np
     ef_sqr = zero_dp
     xp1(1:2) = pt(n, 1:2)

     call qqh_2d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     axh(0:2) = interp%h_coeff_x(0:2)
     ayh(0:2) = interp%h_coeff_y(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy

     ! Ex(i+1/2,j,k)
     !==============
     !==============
     ! Ey(i,j+1/2,k)
     !==============
     do jp1 = 0, 2
      jp2 = j + jp1
      dvol = ay1(jp1)
      do ip1 = 0, 2
       ip2 = ih + ip1
       ex = ef(ip2, jp2, kp2, 1)
       ef_sqr = ef_sqr + axh(ip1)*dvol*ex*ex
      end do
     end do
     do jp1 = 0, 2
      jp2 = jh + jp1
      dvol = ayh(jp1)
      do ip1 = 0, 2
       ip2 = i + ip1
       ey = ef(ip2, jp2, kp2, 2)
       ef_sqr = ef_sqr + ax1(ip1)*dvol*ey*ey
      end do
     end do
     !==============
     pt(n, 5) = ef_sqr !Ex(p)^2 + Ey(p)^2
    end do
    !=======================================

   case (3)
    do n = 1, np
     pt(n, 1:3) = sp_loc%part(n, 1:3)
    end do
    call set_local_3d_positions(pt, 1, np)
    !==========================
    ! Here Quadratic shapes are used
    do n = 1, np
     ef_sqr = zero_dp
     xp1(1:3) = pt(n, 1:3)

     call qqh_3d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     az1(0:2) = interp%coeff_z(0:2)
     axh(0:2) = interp%h_coeff_x(0:2)
     ayh(0:2) = interp%h_coeff_y(0:2)
     azh(0:2) = interp%h_coeff_z(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy
     k = interp%iz
     kh = interp%ihz

     ! Ex(i+1/2,j,k)
     !==============
     !==============
     ! Ey(i,j+1/2,k)
     !==============
     do kp1 = 0, 2
      kp2 = k + kp1
      do jp1 = 0, 2
       jp2 = j + jp1
       dvol = ay1(jp1)*az1(kp1)
       do ip1 = 0, 2
        ip2 = ip1 + ih
        ex = ef(ip2, jp2, kp2, 1)
        ef_sqr = ef_sqr + axh(ip1)*dvol*ex*ex
       end do
      end do
      do jp1 = 0, 2
       jp2 = jh + jp1
       dvol = ayh(jp1)*az1(kp1)
       do ip1 = 0, 2
        ip2 = i + ip1
        ey = ef(ip2, jp2, kp2, 2)
        ef_sqr = ef_sqr + ax1(ip1)*dvol*ey*ey
       end do
      end do
     end do
     !==============
     ! Ez(i,j,k+1/2)
     !==============
     do kp1 = 0, 2
      kp2 = kh + kp1
      do jp1 = 0, 2
       jp2 = j + jp1
       dvol = ay1(jp1)*azh(kp1)
       do ip1 = 0, 2
        ip2 = ip1 + i
        ez = ef(ip2, jp2, kp2, 3)
        ef_sqr = ef_sqr + ax1(ip1)*dvol*ez*ez
       end do
      end do
     end do
     pt(n, 7) = ef_sqr
    end do
   end select
   !================================
  end subroutine

  !===================================
  ! ENV field assignement section
  !===========================
  
  subroutine set_env_acc_new(ef, av, sp_loc, pt, np, dt_step, mempool)

   real (dp), intent (in) :: ef(:, :, :, :), av(:, :, :, :)
   type (species_new), intent (inout) :: sp_loc
   type (species_aux), intent (inout) :: pt
   integer, intent (in) :: np
   real (dp), intent (in) :: dt_step
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp), pointer, contiguous, dimension(:, :) :: xx => null(), ap => null()
   real (dp), pointer, contiguous, dimension(:) :: inv_gam => null(), aa1 => null()
   real (dp), pointer, contiguous, dimension(:) :: b1 => null(), dgam => null()
   type(interp_coeff), pointer :: interp => null()
   real (dp) :: dvol, dvol1
   real (dp) :: dth, ch
   integer :: i1, j1, k1, i2, j2, k2
   integer (kind=2), parameter :: stl = 2
   integer :: n
   !===============================================
   !===============================================
   ! Uses quadratic shape functions at integer and half-integer grid points
   !====================================
   !===================================================
   ! enter ef(1:6) wake fields
   ! enters av(1)=F=|a|^2/2 envelope at integer grid nodes
   ! and av(2:4)=grad[F] at staggered points
   !  COMPUTES
   !(E,B), F, grad[F] assignements to particle positions 
   ! => ap(1:6)  in 2D 
   ! => ap(1:10) in 3D
   ! approximated gamma function:
   ! gam_new= gam +0.25*charge*Dt(gam*E+0.5*grad[F]).p^{n-1/2}/gam^2
   ! EXIT
   ! (E+ 0.5grad[F]/gam_new) B/gam_new, F   and wgh/gam_new  
   ! pt(1:5)  in 2D
   ! pt(1:7)  in 3D
   !========================================
   dth = 0.5*dt_step
   ch = sp_loc%pick_charge()
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !======================================
   ! Do not execute for immobile particles
   !======================================
   if ( .not. sp_loc%ismobile() ) return
   !========================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   !========================================
   select case (ndim)
   case (2)
    !==========================
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 6, mempool)
    xx => mempool%mp_xx_2d_A
    ap => mempool%mp_xx_2d_B
    !==========================
    ap(1:np, 1:6) = zero_dp
    k2 = 1
    if ( ANY(sp_loc%y(1:np) < ymin ) .or. ANY(sp_loc%y(1:np) > ymax ) ) then
      write(6, *) 'Waning, out of Y'
    end if

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    call qqh_2d_spline( xx(1:np, 1:2), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               axh1 => interp%h_coeff_x_rank2, &
               ayh1 => interp%h_coeff_y_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2 )

    !     upart(1:2) = sp_loc%part(n, 3:4) !the current particle  momenta
    !     wgh_cmp = sp_loc%part(n, 5) !the current particle (weight,charge)
    !==========================
    ! Warning: this call must be after qqh_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    call array_realloc_1d(mempool%mp_xx_1d_B, np)
    call array_realloc_1d(mempool%mp_xx_1d_C, np)
    call array_realloc_1d(mempool%mp_xx_1d_D, np)
    dgam => mempool%mp_xx_1d_A
    inv_gam => mempool%mp_xx_1d_B
    aa1 => mempool%mp_xx_1d_C
    b1 => mempool%mp_xx_1d_D
    aa1(1:np) = zero_dp
    b1(1:np) = zero_dp
    !==========================
    do n = 1, np
     do j1 = 0, stl
      j2 = j(n) + j1
      dvol = ay1(j1, n)
      do i1 = 0, stl
       i2 = i(n) + i1
       ap(n, 6) = ap(n, 6) + ax1(i1, n)*dvol*av(i2, j2, k2, 1) !t^n p-assigned F=a^2/2 field
      end do
      do i1 = 0, stl
       i2 = ih(n) + i1
       dvol1 = dvol*axh1(i1, n)
       ap(n, 1) = ap(n, 1) + dvol1*ef(i2, j2, k2, 1) !Ex and Dx[F] (i+1/2,j,k))
       ap(n, 4) = ap(n, 4) + dvol1*av(i2, j2, k2, 2)
       !ap(4)=ap(4)+dvol1*dx_inv*(av(i2+1,j2,k2,1)-av(i2,j2,k2,1))
      end do
     end do
     do j1 = 0, stl
      j2 = jh(n) + j1
      dvol = ayh1(j1, n)
      do i1 = 0, stl
       i2 = i(n) + i1
       dvol1 = dvol*ax1(i1, n)
       ap(n, 2) = ap(n, 2) + dvol1*ef(i2, j2, k2, 2) !Ey and Dy[F] (i,j+1/2,k)
       ap(n, 5) = ap(n, 5) + dvol1*av(i2, j2, k2, 3)
       !ap(5)=ap(5)+dvol1*dy_inv*(av(i2,j2+1,k2,1)-av(i2,j2,k2,1))
      end do
      do i1 = 0, stl
       i2 = ih(n) + i1
       ap(n, 3) = ap(n, 3) + axh1(i1, n)*dvol*ef(i2, j2, k2, 3) !Bz(i+1/2,j+1/2,k)
      end do
     end do
    end do

    end associate
    !=========================
    call sp_loc%compute_gamma(pond_pot=ap(1:np, 6))
    inv_gam(1:np) = sp_loc%gamma_inv(1:np) !1/gamma^{n-1/2}
    ap(1:np, 1:3) = ch*ap(1:np, 1:3)
    ap(1:np, 4:5) = 0.5*ch*ch*ap(1:np, 4:5)
    xx(1:np, 1) = sp_loc%px(1:np)
    xx(1:np, 2) = sp_loc%py(1:np)
    !  ap(1:2)=q(Ex,Ey)   ap(3)=q*Bz,ap(4:5)=q*q*[Dx,Dy]F/2
    do n = 1, 2
     aa1(1:np) = aa1(1:np) + ap(1:np, n)*xx(1:np, n) !Dt*(qE_ip_i)/2 ==> a
     b1(1:np) = b1(1:np) + ap(1:np, n + 3)*xx(1:np, n) !Dt*(qD_iFp_i)/4 ===> c
    end do
    dgam(1:np) = dth*inv_gam(1:np)*(aa1(1:np)-b1(1:np)*inv_gam(1:np))
    inv_gam(1:np) = (one_dp - dgam(1:np)*inv_gam(1:np))*inv_gam(1:np)
    !ap(3)=q*B/gamp, ap(4:5)= q*Grad[F]/2*gamp
    do n = 3, 5
     ap(1:np, n) = ap(1:np, n)*inv_gam(1:np)
    end do
    call pt%set_component(ap(1:np, 1) - ap(1:np, 4), FX_COMP, lb=1, ub=np)
    call pt%set_component(ap(1:np, 2) - ap(1:np, 5), FY_COMP, lb=1, ub=np)
    ! Lorentz force already multiplied by q    
    call pt%set_component(ap(1:np, 3), BZ_COMP, lb=1, ub=np)
    call pt%set_component(inv_gam(1:np)*sp_loc%weight(1:np), &
     W_COMP, lb=1, ub=np) !weight/gamp
    !=============================

   case (3)
    !==========================
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 10, mempool)
    xx => mempool%mp_xx_2d_A
    ap => mempool%mp_xx_2d_B
    !==========================
    ap(1:np, 1:10) = zero_dp

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
    call qqh_3d_spline( xx(1:np, 1:3), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               az1 => interp%coeff_z_rank2, &
               axh1 => interp%h_coeff_x_rank2, &
               ayh1 => interp%h_coeff_y_rank2, &
               azh1 => interp%h_coeff_z_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2, &
               k => interp%iz_rank2, &
               kh => interp%ihz_rank2 )
    !=====================================================
    ! Warning: this call must be after qqh_3d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    call array_realloc_1d(mempool%mp_xx_1d_B, np)
    call array_realloc_1d(mempool%mp_xx_1d_C, np)
    call array_realloc_1d(mempool%mp_xx_1d_D, np)
    dgam => mempool%mp_xx_1d_A
    inv_gam => mempool%mp_xx_1d_B
    aa1 => mempool%mp_xx_1d_C
    b1 => mempool%mp_xx_1d_D
    aa1(1:np) = zero_dp
    b1(1:np) = zero_dp
    do n = 1, np
     do k1 = 0, stl
      k2 = k(n) + k1
      do j1 = 0, stl
       j2 = j(n) + j1
       dvol = ay1(j1, n)*az1(k1, n)
       do i1 = 0, stl
        i2 = i1 + i(n)
        ap(n, 10) = ap(n, 10) + ax1(i1, n)*dvol*av(i2, j2, k2, 1) !t^n p-assigned Phi=a^2/2 field
       end do
       do i1 = 0, stl
        i2 = i1 + ih(n)
        dvol1 = dvol*axh1(i1, n)
        ap(n, 1) = ap(n, 1) + dvol1*ef(i2, j2, k2, 1) !Ex and Dx[F] (i+1/2,j,k))
        ap(n, 7) = ap(n, 7) + dvol1*av(i2, j2, k2, 2)
       end do
      end do
      do j1 = 0, stl
       j2 = jh(n) + j1
       dvol = ayh1(j1, n)*az1(k1, n)
       do i1 = 0, 2
        i2 = i(n) + i1
        dvol1 = dvol*ax1(i1, n)
        ap(n, 2) = ap(n, 2) + dvol1*ef(i2, j2, k2, 2) !Ey and Dy[F] (i,j+1/2,k)
        ap(n, 8) = ap(n, 8) + dvol1*av(i2, j2, k2, 3)
       end do
       do i1 = 0, stl
        i2 = i1 + ih(n)
        ap(n, 6) = ap(n, 6) + axh1(i1, n)*dvol*ef(i2, j2, k2, 6) !Bz(i+1/2,j+1/2,k)
       end do
      end do
     end do
     !=========================
     do k1 = 0, stl
      k2 = kh(n) + k1
      do j1 = 0, stl
       j2 = jh(n) + j1
       dvol = ayh1(j1, n)*azh1(k1, n)
       do i1 = 0, stl
        i2 = i1 + i(n)
        ap(n, 4) = ap(n, 4) + ax1(i1, n)*dvol*ef(i2, j2, k2, 4) !Bx(i,j+1/2,k+1/2)
       end do
      end do
      do j1 = 0, stl
       j2 = j(n) + j1
       dvol = ay1(j1, n)*azh1(k1, n)
       do i1 = 0, stl
        i2 = ih(n) + i1
        ap(n, 5) = ap(n, 5) + axh1(i1, n)*dvol*ef(i2, j2, k2, 5) !By(i+1/2,j,k+1/2)
       end do
       do i1 = 0, stl
        i2 = i1 + i(n)
        dvol1 = dvol*ax1(i1, n)
        ap(n, 3) = ap(n, 3) + dvol1*ef(i2, j2, k2, 3) !Ez and Dz[F] (i,j,k+1/2)
        ap(n, 9) = ap(n, 9) + dvol1*av(i2, j2, k2, 4)
       end do
      end do
     end do
    end do

    end associate
    !=========================
    call sp_loc%compute_gamma(pond_pot=ap(1:np, 10)) ! Check if needed, probably can be computed in lpf_env_positions
    inv_gam(1:np) = sp_loc%gamma_inv(1:np) !1/gamma^{n-1/2}
    ap(1:np, 1:6) = ch*ap(1:np, 1:6)
    ap(1:np, 7:9) = 0.5*ch*ch*ap(1:np, 7:9)
    xx(1:np, 1) = sp_loc%px(1:np)
    xx(1:np, 2) = sp_loc%py(1:np)
    xx(1:np, 3) = sp_loc%pz(1:np)
    !  ap(1:2)=q(Ex,Ey)   ap(3)=q*Bz,ap(4:5)=q*q*[Dx,Dy]F/2
    do n = 1, 3
     aa1(1:np) = aa1(1:np) + ap(1:np, n)*xx(1:np, n) !Dt*(qE_ip_i)/2 ==> a
     b1(1:np) = b1(1:np) + ap(1:np, n + 6)*xx(1:np, n) !Dt*(qD_iFp_i)/4 ===> c
    end do
    dgam(1:np) = dth*inv_gam(1:np)*(aa1(1:np)-b1(1:np)*inv_gam(1:np))
    inv_gam(1:np) = (one_dp-dgam(1:np)*inv_gam(1:np))*inv_gam(1:np)
    !  ap(1:3)=q(Ex,Ey,Ez)   ap(4:6)=q(Bx,By,Bz),ap(7:9)=q[Dx,Dy,Dz]F/2
    do n = 4, 9
     ap(1:np, n) = ap(1:np, n)*inv_gam(1:np)
    end do
    ! Lorentz force already multiplied by q    
    call pt%set_component(ap(1:np, 1) - ap(1:np, 7), FX_COMP, lb=1, ub=np)
    call pt%set_component(ap(1:np, 2) - ap(1:np, 8), FY_COMP, lb=1, ub=np)
    call pt%set_component(ap(1:np, 3) - ap(1:np, 9), FZ_COMP, lb=1, ub=np)
    call pt%set_component(ap(1:np, 4), BX_COMP, lb=1, ub=np)
    call pt%set_component(ap(1:np, 5), BY_COMP, lb=1, ub=np)
    call pt%set_component(ap(1:np, 6), BZ_COMP, lb=1, ub=np)
    call pt%set_component(inv_gam(1:np)*sp_loc%weight(1:np), &
     W_COMP, lb=1, ub=np) !weight/gamp
    !=============================
   end select
  end subroutine
  !=======================================
  
  subroutine set_env_acc_old(ef, av, sp_loc, pt, np, dt_step)

   real (dp), intent (in) :: ef(:, :, :, :), av(:, :, :, :)
   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np
   real (dp), intent (in) :: dt_step
   type(interp_coeff), allocatable :: interp
   real (dp) :: dvol, dvol1
   real (dp) :: xp1(3), upart(3), ap(12)
   real (dp) :: aa1, b1, dgam, gam_inv, gam, gam2, dth
   real (dp) :: axh1(0:2), ax1(0:2)
   real (dp) :: ayh1(0:2), ay1(0:2)
   real (dp) :: azh1(0:2), az1(0:2)
   integer :: i, ih, j, jh, i2, j2, k, kh, k2, n
   integer :: i1, j1, k1
   integer (kind=2), parameter :: stl = 2
   !===============================================
   !===============================================
   ! Uses quadratic shape functions at integer and half-integer grid points
   !====================================
   !===================================================
   ! enter ef(1:6) wake fields
   ! enters av(1)=F=|a|^2/2 envelope at integer grid nodes
   ! and av(2:4)=grad[F] at staggered points
   !  COMPUTES
   !(E,B), F, grad[F] assignements to particle positions 
   ! => ap(1:6)  in 2D 
   ! => ap(1:10) in 3D
   ! approximated gamma function:
   ! gam_new= gam +0.25*charge*Dt(gam*E+0.5*grad[F]).p^{n-1/2}/gam^2
   ! EXIT
   ! (E+ 0.5grad[F]/gam_new) B/gam_new, F   and wgh/gam_new  
   ! pt(1:5)  in 2D
   ! pt(1:7)  in 3D
   !========================================
   dth = 0.5*dt_step
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   !==========================
   case (2)
    ax1(0:2) = zero_dp
    ay1(0:2) = zero_dp
    axh1(0:2) = zero_dp
    ayh1(0:2) = zero_dp
    k2 = 1
    do n = 1, np
     pt(n, 1:2) = sp_loc%part(n, 1:2)
    end do
    call set_local_2d_positions(pt, 1, np)
    do n = 1, np
     ap(1:6) = 0.0
     xp1(1:2) = pt(n, 1:2) !the current particle positions
     upart(1:2) = sp_loc%part(n, 3:4) !the current particle  momenta
     wgh_cmp = sp_loc%part(n, 5) !the current particle (weight,charge)

     call qqh_2d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     axh1(0:2) = interp%h_coeff_x(0:2)
     ayh1(0:2) = interp%h_coeff_y(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy

     !==========================
     do j1 = 0, stl
      j2 = j + j1
      dvol = ay1(j1)
      do i1 = 0, stl
       i2 = i + i1
       ap(6) = ap(6) + ax1(i1)*dvol*av(i2, j2, k2, 1) !t^n p-assigned F=a^2/2 field
      end do
      do i1 = 0, stl
       i2 = ih + i1
       dvol1 = dvol*axh1(i1)
       ap(1) = ap(1) + dvol1*ef(i2, j2, k2, 1) !Ex and Dx[F] (i+1/2,j,k))
       ap(4) = ap(4) + dvol1*av(i2, j2, k2, 2)
       !ap(4)=ap(4)+dvol1*dx_inv*(av(i2+1,j2,k2,1)-av(i2,j2,k2,1))
      end do
     end do
     do j1 = 0, stl
      j2 = jh + j1
      dvol = ayh1(j1)
      do i1 = 0, stl
       i2 = i + i1
       dvol1 = dvol*ax1(i1)
       ap(2) = ap(2) + dvol1*ef(i2, j2, k2, 2) !Ey and Dy[F] (i,j+1/2,k)
       ap(5) = ap(5) + dvol1*av(i2, j2, k2, 3)
       !ap(5)=ap(5)+dvol1*dy_inv*(av(i2,j2+1,k2,1)-av(i2,j2,k2,1))
      end do
      do i1 = 0, stl
       i2 = ih + i1
       ap(3) = ap(3) + axh1(i1)*dvol*ef(i2, j2, k2, 3) !Bz(i+1/2,j+1/2,k)
      end do
     end do
     !=========================
     gam2 = 1. + upart(1)*upart(1) + upart(2)*upart(2) + ap(6) !gamma^{n-1/2}
     ap(1:3) = charge*ap(1:3)
     ap(4:5) = 0.5*charge*charge*ap(4:5)
     !  ap(1:2)=q(Ex,Ey)   ap(3)=q*Bz,ap(4:5)=q*q*[Dx,Dy]F/2
     aa1 = dth*dot_product(ap(1:2), upart(1:2)) !Dt*(qE_ip_i)/2 ==> a
     b1 = dth*dot_product(ap(4:5), upart(1:2)) !Dt*(qD_iFp_i)/4 ===> c
     gam = sqrt(gam2)
     dgam = (aa1*gam-b1)/gam2
     gam_inv = (gam-dgam)/gam2
     ap(3:5) = ap(3:5)*gam_inv !ap(3)=q*B/gamp, ap(4:5)= q*Grad[F]/2*gamp

     pt(n, 1:2) = ap(1:2) - ap(4:5) ! Lorentz force already multiplied by q    
     pt(n, 3) = ap(3)
     pt(n, 5) = wgh*gam_inv !weight/gamp
    end do
    !=============================
   case (3)
    ax1(0:2) = zero_dp
    ay1(0:2) = zero_dp
    az1(0:2) = zero_dp
    azh1(0:2) = zero_dp
    axh1(0:2) = zero_dp
    ayh1(0:2) = zero_dp
    do n = 1, np
     pt(n, 1:3) = sp_loc%part(n, 1:3)
    end do
    call set_local_3d_positions(pt, 1, np)
    do n = 1, np
     ap = zero_dp
     xp1(1:3) = pt(n, 1:3)
     upart(1:3) = sp_loc%part(n, 4:6) !the current particle  momenta
     wgh_cmp = sp_loc%part(n, 7) !the current particle (weight,charge)

     call qqh_3d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     az1(0:2) = interp%coeff_z(0:2)
     axh1(0:2) = interp%h_coeff_x(0:2)
     ayh1(0:2) = interp%h_coeff_y(0:2)
     azh1(0:2) = interp%h_coeff_z(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy
     k = interp%iz
     kh = interp%ihz

     !==========================
     do k1 = 0, stl
      k2 = k + k1
      do j1 = 0, stl
       j2 = j + j1
       dvol = ay1(j1)*az1(k1)
       do i1 = 0, stl
        i2 = i1 + i
        ap(10) = ap(10) + ax1(i1)*dvol*av(i2, j2, k2, 1) !t^n p-assigned Phi=a^2/2 field
       end do
       do i1 = 0, stl
        i2 = i1 + ih
        dvol1 = dvol*axh1(i1)
        ap(1) = ap(1) + dvol1*ef(i2, j2, k2, 1) !Ex and Dx[F] (i+1/2,j,k))
        ap(7) = ap(7) + dvol1*av(i2, j2, k2, 2)
       end do
      end do
      do j1 = 0, stl
       j2 = jh + j1
       dvol = ayh1(j1)*az1(k1)
       do i1 = 0, 2
        i2 = i + i1
        dvol1 = dvol*ax1(i1)
        ap(2) = ap(2) + dvol1*ef(i2, j2, k2, 2) !Ey and Dy[F] (i,j+1/2,k)
        ap(8) = ap(8) + dvol1*av(i2, j2, k2, 3)
       end do
       do i1 = 0, stl
        i2 = i1 + ih
        ap(6) = ap(6) + axh1(i1)*dvol*ef(i2, j2, k2, 6) !Bz(i+1/2,j+1/2,k)
       end do
      end do
     end do
     !=========================
     do k1 = 0, stl
      k2 = kh + k1
      do j1 = 0, stl
       j2 = jh + j1
       dvol = ayh1(j1)*azh1(k1)
       do i1 = 0, stl
        i2 = i1 + i
        ap(4) = ap(4) + ax1(i1)*dvol*ef(i2, j2, k2, 4) !Bx(i,j+1/2,k+1/2)
       end do
      end do
      do j1 = 0, stl
       j2 = j + j1
       dvol = ay1(j1)*azh1(k1)
       do i1 = 0, stl
        i2 = ih + i1
        ap(5) = ap(5) + axh1(i1)*dvol*ef(i2, j2, k2, 5) !By(i+1/2,j,k+1/2)
       end do
       do i1 = 0, stl
        i2 = i1 + i
        dvol1 = dvol*ax1(i1)
        ap(3) = ap(3) + dvol1*ef(i2, j2, k2, 3) !Ez and Dz[F] (i,j,k+1/2)
        ap(9) = ap(9) + dvol1*av(i2, j2, k2, 4)
       end do
      end do
     end do
     !=================================
     gam2 = 1. + upart(1)*upart(1) + upart(2)*upart(2) + &
      upart(3)*upart(3) + ap(10) !gamma^{n-1/2}
     ap(1:6) = charge*ap(1:6)
     ap(7:9) = 0.5*charge*charge*ap(7:9)
     !  ap(1:3)=q(Ex,Ey,Ez)   ap(4:6)=q(Bx,By,Bz),ap(7:9)=q[Dx,Dy,Dz]F/2
     aa1 = dth*dot_product(ap(1:3), upart(1:3))
     b1 = dth*dot_product(ap(7:9), upart(1:3))
     gam = sqrt(gam2)
     dgam = (aa1*gam-b1)/gam2
     gam_inv = (gam-dgam)/gam2

     ap(4:9) = ap(4:9)*gam_inv !ap(4:6)=B/gamp, ap(7:9)= Grad[F]/2*gamp

     pt(n, 1:3) = ap(1:3) - ap(7:9)
     pt(n, 4:6) = ap(4:6)
     pt(n, 7) = wgh*gam_inv !weight/gamp
    end do
   end select
  end subroutine
  !=======================================

  subroutine set_ion_env_field_new(ef, sp_loc, pt, np, om0, mempool)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: pt
   integer, intent (in) :: np
   real (dp), intent (in) :: om0
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null(), ap => null()
   type(interp_coeff), pointer :: interp => null()
   real (dp) :: dvol, ddx, ddy
   integer :: i1, j1, i2, j2, k1, k2, n
   !==============================
   ! Enter ef(1:2)<=  A=(A_R,A_I)
   ! Exit pt=|E|^2= |E_y|^2 + |E_x|^2 assigned to each ion particle
   !===========================
   !  Up to O(epsilon)^2:
   ! |E_y|^2= k_0^2*|A|^2+2*k_0*[A_R*Dx(A_I)-A_I*Dx(A_R)] +(Dx[A_R])^2 +Dx[A_I}^2)
   ! |E_x|^2= (Dy[A_R])^2 +Dy[A_I]^2)
   !===============================================
   !===============================================
   ! Quadratic shape functions
   !====================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   ddx = dx_inv
   ddy = dy_inv
   !========================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   !========================================
   !===== enter species positions at t^{n+1} level========
   ! fields are at t^n
   select case (ndim)
   case (2)
    !==========================
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 6, mempool)
    xx => mempool%mp_xx_2d_A
    ap => mempool%mp_xx_2d_B
    !==========================
    ap(1:np, 1:6) = zero_dp
    k2 = 1

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    call qqh_2d_spline( xx(1:np, 1:2), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               axh1 => interp%h_coeff_x_rank2, &
               ayh1 => interp%h_coeff_y_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2 )
    !==========================
    do n = 1, np
     do j1 = 0, 2
      j2 = j(n) + j1
      dvol = ay1(j1, n)
      do i1 = 0, 2
       i2 = i1 + i(n)
       ap(n, 1) = ap(n, 1) + ax1(i1, n)*dvol*ef(i2, j2, k2, 1) !A_R
       ap(n, 2) = ap(n, 2) + ax1(i1, n)*dvol*ef(i2, j2, k2, 2) !A_I
      end do
      do i1 = 0, 2
       i2 = i1 + ih(n)
       ap(n, 3) = ap(n, 3) + axh1(i1, n)*dvol*(ef(i2+1,j2,k2,1)-ef(i2,j2,k2,1)) !DxA_R
       ap(n, 4) = ap(n, 4) + axh1(i1, n)*dvol*(ef(i2+1,j2,k2,2)-ef(i2,j2,k2,2)) !DxA_I
      end do
     end do
     do j1 = 0, 2
      j2 = jh(n) + j1
      dvol = ayh1(j1, n)
      do i1 = 0, 2
       i2 = i(n) + i1
       ap(n, 5) = ap(n, 5) + ax1(i1, n)*dvol*(ef(i2,j2+1,k2,1)-ef(i2,j2,k2,1)) !DyA_R
       ap(n, 6) = ap(n, 6) + ax1(i1, n)*dvol*(ef(i2,j2+1,k2,2)-ef(i2,j2,k2,2)) !DyA_I
      end do
     end do
    end do
    !==================
    end associate
    call pt%set_component( sqrt(ap(1:np, 1)*ap(1:np, 1)+ap(1:np, 2)*ap(1:np, 2)), &
     POND_COMP, lb=1, ub=np) !The interpolated |A| potential
    ap(1:np, 1) = om0*ap(1:np, 1) 
    ap(1:np, 2) = om0*ap(1:np, 2)
    ap(1:np, 3) = ddx*ap(1:np, 3)
    ap(1:np, 4) = ddx*ap(1:np, 4)
    ap(1:np, 5) = ddy*ap(1:np, 5)
    ap(1:np, 6) = ddy*ap(1:np, 6)

    associate( aux => ap(1:np, 1)*ap(1:np, 1) + ap(1:np, 2)*ap(1:np, 2) + &
               ap(1:np, 3)*ap(1:np, 3) + ap(1:np, 4)*ap(1:np, 4) + &
               ap(1:np, 5)*ap(1:np, 5) + ap(1:np, 6)*ap(1:np, 6) + &
               2*one_dp*(ap(1:np, 1)*ap(1:np, 4) - ap(1:np, 2)*ap(1:np, 3)))
     call pt%set_component( aux, E_SQUARED, lb=1, ub=np )
    end associate

    !==========================
   case (3)
    !==========================
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 6, mempool)
    xx => mempool%mp_xx_2d_A
    ap => mempool%mp_xx_2d_B
    !==========================
    ap(1:np, 1:6) = zero_dp

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
    call qqh_3d_spline( xx(1:np, 1:3), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               az1 => interp%coeff_z_rank2, &
               axh1 => interp%h_coeff_x_rank2, &
               ayh1 => interp%h_coeff_y_rank2, &
               azh1 => interp%h_coeff_z_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2, &
               k => interp%iz_rank2, &
               kh => interp%ihz_rank2 )
    !==========================
    do n = 1, np
     do k1 = 0, 2
      k2 = k(n) + k1
      do j1 = 0, 2
       j2 = j(n) + j1
       dvol = ay1(j1, n)*az1(k1, n)
       do i1 = 0, 2
        i2 = i1 + i(n)
        ap(n, 1) = ap(n, 1) + ax1(i1, n)*dvol*ef(i2, j2, k2, 1) !A_R
        ap(n, 2) = ap(n, 2) + ax1(i1, n)*dvol*ef(i2, j2, k2, 2) !A_I
       end do
       do i1 = 0, 2
        i2 = i1 + ih(n)
        ap(n, 3) = ap(n, 3) + axh1(i1, n)*dvol*(ef(i2+1,j2,k2,1)-ef(i2,j2,k2,1)) !DxA_R
        ap(n, 4) = ap(n, 4) + axh1(i1, n)*dvol*(ef(i2+1,j2,k2,2)-ef(i2,j2,k2,2)) !DxA_I
       end do
      end do
      do j1 = 0, 2
       j2 = jh(n) + j1
       dvol = ayh1(j1, n)*az1(k1, n)
       do i1 = 0, 2
        i2 = i(n) + i1
        ap(n, 5) = ap(n, 5) + ax1(i1, n)*dvol*(ef(i2,j2+1,k2,1)-ef(i2,j2,k2,1)) !DyA_R
        ap(n, 6) = ap(n, 6) + ax1(i1, n)*dvol*(ef(i2,j2+1,k2,2)-ef(i2,j2,k2,2)) !DyA_I
       end do
      end do
     end do
    end do
    end associate
    call pt%set_component( sqrt(ap(1:np, 1)*ap(1:np, 1)+ap(1:np, 2)*ap(1:np, 2)), &
    POND_COMP, lb=1, ub=np) !The interpolated |A| potential
    ap(1:np, 1) = om0*ap(1:np, 1) 
    ap(1:np, 2) = om0*ap(1:np, 2)
    ap(1:np, 3) = ddx*ap(1:np, 3)
    ap(1:np, 4) = ddx*ap(1:np, 4)
    ap(1:np, 5) = ddy*ap(1:np, 5)
    ap(1:np, 6) = ddy*ap(1:np, 6)
    
    associate( aux => ap(1:np, 1)*ap(1:np, 1) + ap(1:np, 2)*ap(1:np, 2) + &
     ap(1:np, 3)*ap(1:np, 3) + ap(1:np, 4)*ap(1:np, 4) + &
     ap(1:np, 5)*ap(1:np, 5) + ap(1:np, 6)*ap(1:np, 6) + &
     2*one_dp*(ap(1:np, 1)*ap(1:np, 4) - ap(1:np, 2)*ap(1:np, 3)))
     call pt%set_component( aux, E_SQUARED, lb=1, ub=np )
    end associate

   end select
   !================================
  end subroutine

  subroutine set_ion_env_field_old(ef, sp_loc, pt, np, om0)

   real (dp), intent (in) :: ef(:, :, :, :)
   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np
   real (dp), intent (in) :: om0
   type(interp_coeff), allocatable :: interp
   real (dp) :: axh1(0:2), ax1(0:2)
   real (dp) :: ayh1(0:2), ay1(0:2)
   real (dp) :: azh1(0:2), az1(0:2)
   real (dp) :: dvol, ddx, ddy
   real (dp) :: xp1(3), ap(6)
   integer :: i, ih, j, jh, i1, j1, i2, j2, k, kh, k1, k2, n
   !==============================
   ! Enter ef(1:2)<=  A=(A_R,A_I)
   ! Exit pt=|E|^2= |E_y|^2 + |E_x|^2 assigned to each ion particle
   !===========================
   !  Up to O(epsilon)^2:
   ! |E_y|^2= k_0^2*|A|^2+2*k_0*[A_R*Dx(A_I)-A_I*Dx(A_R)] +(Dx[A_R])^2 +Dx[A_I}^2)
   ! |E_x|^2= (Dy[A_R])^2 +Dy[A_I]^2)
   !===============================================
   !===============================================
   ! Quadratic shape functions
   !====================================
   ddx = dx_inv
   ddy = dy_inv
   !===== enter species positions at t^{n+1} level========
   ! fields are at t^n
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   case (2)
    ax1(0:2) = zero_dp
    ay1(0:2) = zero_dp
    axh1(0:2) = zero_dp
    ayh1(0:2) = zero_dp
    k2 = 1
    do n = 1, np
     pt(n, 1:2) = sp_loc%part(n, 1:2)
    end do
    call set_local_2d_positions(pt, 1, np)
    !==========================
    do n = 1, np
     ap(1:6) = zero_dp
     xp1(1:2) = pt(n, 1:2)

     call qqh_2d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     axh1(0:2) = interp%h_coeff_x(0:2)
     ayh1(0:2) = interp%h_coeff_y(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy

     do j1 = 0, 2
      j2 = j + j1
      dvol = ay1(j1)
      do i1 = 0, 2
       i2 = i1 + i
       ap(1) = ap(1) + ax1(i1)*dvol*ef(i2, j2, k2, 1) !A_R
       ap(2) = ap(2) + ax1(i1)*dvol*ef(i2, j2, k2, 2) !A_I
      end do
      do i1 = 0, 2
       i2 = i1 + ih
       ap(3) = ap(3) + axh1(i1)*dvol*(ef(i2+1,j2,k2,1)-ef(i2,j2,k2,1)) !DxA_R
       ap(4) = ap(4) + axh1(i1)*dvol*(ef(i2+1,j2,k2,2)-ef(i2,j2,k2,2)) !DxA_I
      end do
     end do
     do j1 = 0, 2
      j2 = jh + j1
      dvol = ayh1(j1)
      do i1 = 0, 2
       i2 = i + i1
       ap(5) = ap(5) + ax1(i1)*dvol*(ef(i2,j2+1,k2,1)-ef(i2,j2,k2,1)) !DyA_R
       ap(6) = ap(6) + ax1(i1)*dvol*(ef(i2,j2+1,k2,2)-ef(i2,j2,k2,2)) !DyA_I
      end do
     end do
     !==================
     pt(n, 4) = sqrt(ap(1)*ap(1)+ap(2)*ap(2)) !The interpolated |A| potential
     ap(1) = om0*ap(1)
     ap(2) = om0*ap(2)
     ap(3) = ddx*ap(3)
     ap(4) = ddx*ap(4)
     ap(5) = ddy*ap(5)
     ap(6) = ddy*ap(6)
     pt(n, 5) = ap(1)*ap(1) + ap(2)*ap(2) + ap(3)*ap(3) + ap(4)*ap(4) + &
       ap(5)*ap(5) + ap(6)*ap(6)
     pt(n, 5) = pt(n, 5) + 2.*(ap(1)*ap(4)-ap(2)*ap(3))
    end do
    !==========================
   case (3)
    ax1(0:2) = zero_dp
    ay1(0:2) = zero_dp
    axh1(0:2) = zero_dp
    ayh1(0:2) = zero_dp
    az1(0:2) = zero_dp
    azh1(0:2) = zero_dp

    do n = 1, np
     pt(n, 1:3) = sp_loc%part(n, 1:3)
    end do
    call set_local_3d_positions(pt, 1, np)
    do n = 1, np
     ap(1:6) = zero_dp
     xp1(1:3) = pt(n, 1:3)

     call qqh_3d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     az1(0:2) = interp%coeff_z(0:2)
     axh1(0:2) = interp%h_coeff_x(0:2)
     ayh1(0:2) = interp%h_coeff_y(0:2)
     azh1(0:2) = interp%h_coeff_z(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy
     k = interp%iz
     kh = interp%ihz
     !=============== Quadratic/linear assignements
     do k1 = 0, 2
      k2 = k + k1
      do j1 = 0, 2
       j2 = j + j1
       dvol = ay1(j1)*az1(k1)
       do i1 = 0, 2
        i2 = i1 + i
        ap(1) = ap(1) + ax1(i1)*dvol*ef(i2, j2, k2, 1) !A_R
        ap(2) = ap(2) + ax1(i1)*dvol*ef(i2, j2, k2, 2) !A_I
       end do
       do i1 = 0, 2
        i2 = i1 + ih
        ap(3) = ap(3) + axh1(i1)*dvol*(ef(i2+1,j2,k2,1)-ef(i2,j2,k2,1)) !DxA_R
        ap(4) = ap(4) + axh1(i1)*dvol*(ef(i2+1,j2,k2,2)-ef(i2,j2,k2,2)) !DxA_I
       end do
      end do
      do j1 = 0, 2
       j2 = jh + j1
       dvol = ayh1(j1)*az1(k1)
       do i1 = 0, 2
        i2 = i + i1
        ap(5) = ap(5) + ax1(i1)*dvol*(ef(i2,j2+1,k2,1)-ef(i2,j2,k2,1)) !DyA_R
        ap(6) = ap(6) + ax1(i1)*dvol*(ef(i2,j2+1,k2,2)-ef(i2,j2,k2,2)) !DyA_I
       end do
      end do
     end do
     pt(n, 6) = sqrt(ap(1)*ap(1)+ap(2)*ap(2)) !The interpolated |A| potential
     ap(1) = om0*ap(1)
     ap(2) = om0*ap(2)
     ap(3) = ddx*ap(3)
     ap(4) = ddx*ap(4)
     ap(5) = ddy*ap(5)
     ap(6) = ddy*ap(6)
     pt(n, 7) = ap(1)*ap(1) + ap(2)*ap(2) + ap(3)*ap(3) + ap(4)*ap(4) + &
       ap(5)*ap(5) + ap(6)*ap(6)
     pt(n, 7) = pt(n, 7) + 2.*(ap(1)*ap(4)-ap(2)*ap(3))
    end do
   end select
   !================================
  end subroutine
  !================================
  subroutine set_env_grad_interp_new(av, sp_loc, pt, np, ndm, mempool)

   real (dp), intent (in) :: av(:, :, :, :)
   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (inout) :: pt
   integer, intent (in) :: np, ndm
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null(), ap => null()
   type(interp_coeff), pointer :: interp => null()
   real (dp) :: dvol, dvol1
   integer ::i1, j1, i2, j2, k1, k2, n

   !===============================================
   ! enters av(1)=|a|^2/2 envelope at integer grid nodes
   ! and av(2:4)=[Grad |a|2/2] at staggered grid points
   ! exit in pt(1:4) grad[|a|^2]/2 and |a|^2/2 at the particle positions
   ! On output => Reverse ordering of field variables is used
   !=========================
   ! Particle positions assigned using quadratic splines
   !  F=|a|^2/2
   !  ap(1)= [D_x(F)](i+1/2,j,k)
   !  ap(2)= [D_y(F)](i,j+1/2,k)
   !  ap(3)= [D_z(F)](i,j,k+1/2)
   !  ap(4)= [Phi](i,j,k)
   !===========================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !========================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   !========================================
   select case (ndim)
   case (2)
    !==========================
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 3, mempool)
    xx => mempool%mp_xx_2d_A
    ap => mempool%mp_xx_2d_B
    !==========================
    ap(1:np, 1:3) = zero_dp
    k2 = 1

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    call qqh_2d_spline( xx(1:np, 1:2), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               axh1 => interp%h_coeff_x_rank2, &
               ayh1 => interp%h_coeff_y_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2 )
    !==========================
    do n = 1, np
     do j1 = 0, 2
      j2 = j(n) + j1
      dvol = ay1(j1, n)
      do i1 = 0, 2
       i2 = i1 + ih(n)
       dvol1 = dvol*axh1(i1, n)
       ap(n, 1) = ap(n, 1) + dvol1*av(i2, j2, k2, 2) !Dx[Phi]
       i2 = i1 + i(n)
       ap(n, 3) = ap(n, 3) + ax1(i1, n)*dvol*av(i2, j2, k2, 1) ![Phi]
      end do
     end do
     do j1 = 0, 2
      j2 = jh(n) + j1
      dvol = ayh1(j1, n)
      do i1 = 0, 2
       i2 = i(n) + i1
       dvol1 = dvol*ax1(i1, n)
       ap(n, 2) = ap(n, 2) + dvol1*av(i2, j2, k2, 3) !Dy[Phi]
      end do
     end do
    end do

    end associate
    !assigned grad[Phi] and Phi
    call pt%set_component( ap(1:np, 1), GRADF_X_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 2), GRADF_Y_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 3), POND_COMP, lb=1, ub=np)
    !=================================
   case (3)
    !==========================
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    call mp_xx_realloc(mempool%mp_xx_2d_B, np, 4, mempool)
    xx => mempool%mp_xx_2d_A
    ap => mempool%mp_xx_2d_B
    !==========================
    ap(1:np, 1:4) = zero_dp

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
    call qqh_3d_spline( xx(1:np, 1:3), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               az1 => interp%coeff_z_rank2, &
               axh1 => interp%h_coeff_x_rank2, &
               ayh1 => interp%h_coeff_y_rank2, &
               azh1 => interp%h_coeff_z_rank2, &
               i => interp%ix_rank2, &
               ih => interp%ihx_rank2, &
               j => interp%iy_rank2, &
               jh => interp%ihy_rank2, &
               k => interp%iz_rank2, &
               kh => interp%ihz_rank2 )
    !==========================
    do n = 1, np
     do k1 = 0, 2
      k2 = k(n) + k1
      do j1 = 0, 2
       j2 = j(n) + j1
       dvol = ay1(j1, n)*az1(k1, n)
       do i1 = 0, 2
        i2 = i1 + ih(n)
        dvol1 = dvol*axh1(i1, n)
        ap(n, 1) = ap(n, 1) + dvol1*av(i2, j2, k2, 2) !Dx[F]
        i2 = i1 + i(n)
        ap(n, 4) = ap(n, 4) + ax1(i1, n)*dvol*av(i2, j2, k2, 1) !Phi
       end do
      end do
      do j1 = 0, 2
       j2 = jh(n) + j1
       dvol = ayh1(j1, n)*az1(k1, n)
       do i1 = 0, 2
        i2 = i(n) + i1
        dvol1 = dvol*ax1(i1, n)
        ap(n, 2) = ap(n, 2) + dvol1*av(i2, j2, k2, 3) !Dy[F]
       end do
      end do
      k2 = kh(n) + k1
      do j1 = 0, 2
       j2 = j(n) + j1
       dvol = ay1(j1, n)*azh1(k1, n)
       do i1 = 0, 2
        i2 = i(n) + i1
        dvol1 = dvol*ax1(i1, n)
        ap(n, 3) = ap(n, 3) + dvol1*av(i2, j2, k2, 4) !Dz[F]
       end do
      end do
     end do
    end do
    end associate
    !assigned grad[Phi] and Phi
    call pt%set_component( ap(1:np, 1), GRADF_X_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 2), GRADF_Y_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 3), GRADF_Z_COMP, lb=1, ub=np)
    call pt%set_component( ap(1:np, 4), POND_COMP, lb=1, ub=np)
    !=================================
   end select
  end subroutine
  !===========================
  subroutine set_env_grad_interp_old(av, sp_loc, pt, np, ndm)

   type (species), intent (in) :: sp_loc
   real (dp), intent (in) :: av(:, :, :, :)
   real (dp), intent (inout) :: pt(:, :)
   integer, intent (in) :: np, ndm
   type(interp_coeff), allocatable :: interp
   real (dp) :: axh1(0:2), ax1(0:2)
   real (dp) :: ayh1(0:2), ay1(0:2)
   real (dp) :: azh1(0:2), az1(0:2)
   real (dp) :: dvol, dvol1, dxe, dye, dze
   real (dp) :: xp1(3), ap(4)
   integer :: i, ih, j, jh, i1, j1, i2, j2, k, kh, k1, k2, n

   !===============================================
   ! enters av(1)=|a|^2/2 envelope at integer grid nodes
   ! and av(2:4)=[Grad |a|2/2] at staggered grid points
   ! exit in pt(1:4) grad[|a|^2]/2 and |a|^2/2 at the particle positions
   ! On output => Reverse ordering of field variables is used
   !=========================
   ! Particle positions assigned using quadratic splines
   !  F=|a|^2/2
   !  ap(1)= [D_x(F)](i+1/2,j,k)
   !  ap(2)= [D_y(F)](i,j+1/2,k)
   !  ap(3)= [D_z(F)](i,j,k+1/2)
   !  ap(4)= [Phi](i,j,k)
   !===========================================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   case (2)
    dxe = dx_inv
    dye = dy_inv
    k2 = 1
    do n = 1, np
     pt(n, 1:2) = sp_loc%part(n, 1:2) !
    end do
    call set_local_2d_positions(pt, 1, np)
    do n = 1, np
     ap = 0.0
     xp1(1:2) = pt(n, 1:2)

     call qqh_2d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     axh1(0:2) = interp%h_coeff_x(0:2)
     ayh1(0:2) = interp%h_coeff_y(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy
     !==========================
     do j1 = 0, 2
      j2 = j + j1
      dvol = ay1(j1)
      do i1 = 0, 2
       i2 = i1 + ih
       dvol1 = dvol*axh1(i1)
       ap(1) = ap(1) + dvol1*av(i2, j2, k2, 2) !Dx[Phi]
       i2 = i1 + i
       ap(3) = ap(3) + ax1(i1)*dvol*av(i2, j2, k2, 1) ![Phi]
      end do
     end do
     do j1 = 0, 2
      j2 = jh + j1
      dvol = ayh1(j1)
      do i1 = 0, 2
       i2 = i + i1
       dvol1 = dvol*ax1(i1)
       ap(2) = ap(2) + dvol1*av(i2, j2, k2, 3) !Dy[Phi]
      end do
     end do
     !pt(n,1)=dxe*ap(1)    !assigned grad[A^2/2]
     !pt(n,2)=dye*ap(2)
     pt(n, 1:3) = ap(1:3) !assigned grad[Phi] and Phi
    end do
    !=================================
   case (3)
    dxe = dx_inv
    dye = dy_inv
    dze = dz_inv
    do n = 1, np
     pt(n, 1:3) = sp_loc%part(n, 1:3)
    end do
    call set_local_3d_positions(pt, 1, np)
    do n = 1, np
     ap = 0.0
     xp1(1:3) = pt(n, 1:3)

     call qqh_3d_spline( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     az1(0:2) = interp%coeff_z(0:2)
     axh1(0:2) = interp%h_coeff_x(0:2)
     ayh1(0:2) = interp%h_coeff_y(0:2)
     azh1(0:2) = interp%h_coeff_z(0:2)
 
     i = interp%ix
     ih = interp%ihx
     j = interp%iy
     jh = interp%ihy
     k = interp%iz
     kh = interp%ihz

     !==========================
     ap = 0.0
     do k1 = 0, 2
      k2 = k + k1
      do j1 = 0, 2
       j2 = j + j1
       dvol = ay1(j1)*az1(k1)
       do i1 = 0, 2
        i2 = i1 + ih
        dvol1 = dvol*axh1(i1)
        ap(1) = ap(1) + dvol1*av(i2, j2, k2, 2) !Dx[F]
        i2 = i1 + i
        ap(4) = ap(4) + ax1(i1)*dvol*av(i2, j2, k2, 1) !Phi
       end do
      end do
      do j1 = 0, 2
       j2 = jh + j1
       dvol = ayh1(j1)*az1(k1)
       do i1 = 0, 2
        i2 = i + i1
        dvol1 = dvol*ax1(i1)
        ap(2) = ap(2) + dvol1*av(i2, j2, k2, 3) !Dy[F]
       end do
      end do
      k2 = kh + k1
      do j1 = 0, 2
       j2 = j + j1
       dvol = ay1(j1)*azh1(k1)
       do i1 = 0, 2
        i2 = i + i1
        dvol1 = dvol*ax1(i1)
        ap(3) = ap(3) + dvol1*av(i2, j2, k2, 4) !Dz[F]
       end do
      end do
     end do
     pt(n, 1:4) = ap(1:4) !Exit grad[Phi] and Phi
     !=================================
    end do
   end select
  end subroutine
  !===========================
  subroutine set_env_density_new(sp_loc, av, np, ic, mempool)

   type (species_aux), intent (inout) :: sp_loc
   real (dp), intent (inout) :: av(:, :, :, :)
   integer, intent (in) :: np, ic
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp), pointer, contiguous, dimension(:, :) :: xx => null()
   real (dp), pointer, contiguous, dimension(:) :: weight => null()
   type(interp_coeff), pointer :: interp => null()
   real (dp) :: dvol, dvol1
   integer :: i1, j1, i2, j2, k1, k2, n
   !===============================================
   ! Enter in sp_loc positions and efp(5) wgh/gamp at time level n
   ! exit av(:,:,:,ic) the den source in envelope equation :  <n*wgh/gamp> > 0
   ! exit efp(1:3) relative positions at time level n
   !=========================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=============================================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   !================================
   select case (ndim)
   case (2)
    k2 = 1
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )

    call qden_2d_wgh( xx(1:np, 1:2), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2 )
    !==========================
    ! Warning: this call must be after qqh_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    weight => mempool%mp_xx_1d_A
    weight(1:np) = sp_loc%weight(1:np)
     !==========================
    do n = 1, np
     do j1 = 0, 2
      j2 = j(n) + j1
      dvol = ay1(j1, n)*weight(n)
      do i1 = 0, 2
       i2 = i1 + i(n)
       dvol1 = dvol*ax1(i1, n)
       av(i2, j2, k2, ic) = av(i2, j2, k2, ic) + dvol1
      end do
     end do
    end do
    !========================
    end associate
   case (3)
    call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
    xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
    xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
    call qden_3d_wgh( xx(1:np, 1:3), interp, mempool )

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               az1 => interp%coeff_z_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               k => interp%iz_rank2 )
    !==========================
    ! Warning: this call must be after qqh_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    weight => mempool%mp_xx_1d_A
    weight(1:np) = sp_loc%weight(1:np)
    do n = 1, np
     do k1 = 0, 2
      k2 = k(n) + k1
      do j1 = 0, 2
       j2 = j(n) + j1
       dvol = ay1(j1, n)*az1(k1, n)*weight(n)
       do i1 = 0, 2
        i2 = i1 + i(n)
        dvol1 = dvol*ax1(i1, n)
        av(i2, j2, k2, ic) = av(i2, j2, k2, ic) + dvol1
       end do
      end do
     end do
    end do
    end associate
   end select
   !In ebfp(1:3) exit relative (x,y,z) positions at current t^n level
   !In av(ic)  exit particle density
   !================================
  end subroutine
  !==========================
  subroutine set_env_density_old(efp, av, np, ic)

   real (dp), intent (inout) :: efp(:, :)
   real (dp), intent (inout) :: av(:, :, :, :)
   integer, intent (in) :: np, ic
   type(interp_coeff), allocatable :: interp
   real (dp) :: dvol, dvol1, wghp
   real (dp) :: ax1(0:2), ay1(0:2), az1(0:2), xp1(3)
   integer :: i, j, i1, j1, i2, j2, k, k1, k2, n
   !===============================================
   ! 2D enter efp(1:2) positions and efp(5) wgh/gamp at time level n
   ! 3D enter efp(1:3) positions and efp(7) wgh/gamp at time level n
   ! exit av(:,:,:,ic) the den source in envelope equation :  <n*wgh/gamp> > 0
   ! exit efp(1:3) relative positions at time level n
   !=========================
   ax1(0:2) = zero_dp
   ay1(0:2) = zero_dp
   az1(0:2) = zero_dp

   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   case (2)
    k2 = 1
    call set_local_2d_positions(efp, 1, np)
    do n = 1, np
     xp1(1:2) = efp(n, 1:2)
     wghp = efp(n, 5) !the particle  wgh/gamp at current time

     call qden_2d_wgh( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)

     i = interp%ix
     j = interp%iy
     !==========================
     do j1 = 0, 2
      j2 = j + j1
      dvol = ay1(j1)*wghp
      do i1 = 0, 2
       i2 = i1 + i
       dvol1 = dvol*ax1(i1)
       av(i2, j2, k2, ic) = av(i2, j2, k2, ic) + dvol1
      end do
     end do
    end do
    !========================
   case (3)
    call set_local_3d_positions(efp, 1, np)
    do n = 1, np
     xp1(1:3) = efp(n, 1:3) ! local x-y-z
     wghp = efp(n, 7) !the particle  wgh/gamp at current time

     call qden_3d_wgh( xp1, interp )

     ax1(0:2) = interp%coeff_x(0:2)
     ay1(0:2) = interp%coeff_y(0:2)
     az1(0:2) = interp%coeff_z(0:2)

     i = interp%ix
     j = interp%iy
     k = interp%iz

     do k1 = 0, 2
      k2 = k + k1
      do j1 = 0, 2
       j2 = j + j1
       dvol = ay1(j1)*az1(k1)*wghp
       do i1 = 0, 2
        i2 = i1 + i
        dvol1 = dvol*ax1(i1)
        av(i2, j2, k2, ic) = av(i2, j2, k2, ic) + dvol1
       end do
      end do
     end do
    end do
   end select
   !In ebfp(1:3) exit relative (x,y,z) positions at current t^n level
   !In av(ic)  exit particle density
   !================================
  end subroutine
  !====================================================
  !========= PARTICLE ASSIGNEMENT TO GRID FOR CURRENT DENSITY
  !=============================
  subroutine esirkepov_2d_curr_new(sp_loc, pt, jcurr, np, mempool)

   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (in) :: pt
   real (dp), intent (inout) :: jcurr(:, :, :, :)
   integer, intent (in) :: np
   type(memory_pool_t), pointer, intent(in) :: mempool
   real (dp), pointer, contiguous, dimension(:, :) :: xx => null()
   type(interp_coeff), pointer :: interp => null(), interp_old => null()
   real (dp), dimension(0:4) :: axh, ayh, currx, curry, axh0
   real (dp) :: dvol, dvolh
   integer :: i1, j1, i2, j2, j20, n, ih, jh
   integer :: x0, x1, y0, y1
   !==========================
   ! Iform=0 or 1 IMPLEMENTS the ESIRKEPOV SCHEME for LINEAR-QUADRATIC SHAPE
   !==============================
   ! Only new and old positions needed
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=================================
   ! Do not execute for test particles
   !=================================
   if ( sp_loc%istest() ) return
   !=================================
   ! Do not execute for immobile particles
   !=================================
   if ( .not. sp_loc%ismobile() ) return
   !=============================================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   call interp_realloc(mempool%interp_old, np, sp_loc%pick_dimensions())
   interp_old => mempool%interp_old
   !=============================================================
   call mp_xx_realloc(mempool%mp_xx_2d_B, np, 4, mempool)
   xx => mempool%mp_xx_2d_B
   
   ! Interpolation on new positions
   xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
   xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
   call qden_2d_wgh( xx(1:np, 1:2), interp, mempool )
   
   ! Interpolation on old positions
   xx(1:np, 1) = set_local_positions( pt, X_COMP )
   xx(1:np, 2) = set_local_positions( pt, Y_COMP )
   
   call qden_2d_wgh( xx(1:np, 1:2), interp_old, mempool )

   if (curr_ndim==2) then !Two current components
    
    ! Recycling xx since it's not needed anymore
    xx(1:np, 2) = sp_loc%pick_charge()*sp_loc%weight(1:np)

    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               ax0 => interp_old%coeff_x_rank2, &
               ay0 => interp_old%coeff_y_rank2, &
               ii0 => interp_old%ix_rank2, &
               jj0 => interp_old%iy_rank2 )

     do n = 1, np

      axh(0:4) = zero_dp
      ayh(0:4) = zero_dp

      ih = i(n) - ii0(n) + 1
      !========================================
      do i1 = 0, 2
       axh(ih + i1) = ax1(i1, n)
      end do

      currx(0) = -axh(0)
      do i1 = 1, 3
       currx(i1) = currx(i1-1) + ax0(i1-1, n) - axh(i1)
      end do
      currx(4) = currx(3) - axh(4)

      do i1 = 1, 3
       axh(i1) = axh(i1) + ax0(i1-1, n)
      end do
     
      ! Current times weight
      do i1 = 0, 4
       currx(i1) = xx(n, 2)*currx(i1)
      end do

      x0 = min(ih, 1)
      x1 = max(ih + 2, 3)

      !========================================
      jh = j(n) - jj0(n) + 1

      do i1 = 0, 2
       ayh(jh + i1) = ay1(i1, n)
      end do

      curry(0) = -ayh(0)
      do i1 = 1, 3
       curry(i1) = curry(i1-1) + ay0(i1-1, n) - ayh(i1)
      end do
      curry(4) = curry(3) - ayh(4)
      ! Current times weight
      do i1 = 0, 4
       curry(i1) = xx(n, 2)*curry(i1)
      end do

      do i1 = 1, 3
       ayh(i1) = ayh(i1) + ay0(i1-1, n)
      end do
      y0 = min(jh, 1)
      y1 = max(jh + 2, 3)

      !================dt*J_x

      jh = jj0(n) - 1
      ih = ii0(n) - 1

      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + ayh(j1)*currx(i1)
       end do
      end do
      !================dt*J_y
      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + axh(i1)*curry(j1)
       end do
      end do
     end do
    end associate
   end if
    !========================================
   if (curr_ndim==3) then !Three currents conditions in 2D grid
    
    !========================================
    ! Computing velocity along z
    xx(1:np, 1) = set_local_positions( sp_loc, Z_COMP ) ! z new
    xx(1:np, 2) = set_local_positions( pt, Z_COMP ) ! z old
    
    ! Storing z_new - z_old in xx(1:np, 1)
    xx(1:np, 1) = (xx(1:np, 1) - xx(1:np, 2))/3.
    
    ! Recycling xx since it's not needed anymore
    xx(1:np, 2) = sp_loc%pick_charge()*sp_loc%weight(1:np)

    ! Multiplying by the particle weight
    xx(1:np, 1) = xx(1:np, 2)*xx(1:np, 1)
    
    associate( ax1 => interp%coeff_x_rank2, &
               ay1 => interp%coeff_y_rank2, &
               i => interp%ix_rank2, &
               j => interp%iy_rank2, &
               ax0 => interp_old%coeff_x_rank2, &
               ay0 => interp_old%coeff_y_rank2, &
               ii0 => interp_old%ix_rank2, &
               jj0 => interp_old%iy_rank2 )

     do n = 1, np

      axh(0:4) = zero_dp
      ayh(0:4) = zero_dp
      axh0(0:4) = zero_dp

      ih = i(n) - ii0(n) + 1

      do i1 = 0, 2
       axh(ih + i1) = ax1(i1, n)
      end do

      currx(0) = -axh(0)
      do i1 = 1, 3
       currx(i1) = currx(i1-1) + ax0(i1-1, n) - axh(i1)
      end do
      currx(4) = currx(3) - axh(4)

      do i1 = 1, 3
       axh0(i1) = axh(i1) + ax0(i1-1, n)
      end do
     
      do i1 = 0, 4
       currx(i1) = xx(n, 2)*currx(i1)
      end do

      x0 = min(ih, 1)
      x1 = max(ih + 2, 3)

      !========================================
      jh = j(n) - jj0(n) + 1
      do i1 = 0, 2
       ayh(jh + i1) = ay1(i1, n)
      end do
      curry(0) = -ayh(0)
      do i1 = 1, 3
       curry(i1) = curry(i1-1) + ay0(i1-1, n) - ayh(i1)
      end do
      curry(4) = curry(3) - ayh(4)
      do i1 = 0, 4
       curry(i1) = xx(n, 2)*curry(i1)
      end do
      do i1 = 1, 3
       ayh(i1) = ayh(i1) + ay0(i1-1, n)
      end do
      y0 = min(jh, 1)
      y1 = max(jh + 2, 3)

      !================dt*J_x= currx*(Wy^0+Wy^1) to be multiplied by dx/2

      jh = jj0(n) - 1
      ih = ii0(n) - 1

      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + ayh(j1)*currx(i1)
       end do
      end do
      !================dt*J_y= curry*(Wx^0+Wx^1)
      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + axh0(i1)*curry(j1)
       end do
      end do

      ! Here we recycle the currx and curry arrays that are not needed anymore
      currx(0:4) = 0.5*axh(0:4)
      curry(0:4) = axh(0:4)
      do i1 = 1, 3
       currx(i1) = currx(i1) + ax0(i1-1, n)
       curry(i1) = curry(i1) + 0.5*ax0(i1-1, n)
      end do
      !========== dt*J_z Vz*[Wy^0(Wx^0+0.5*Wx^1)+Wy^1*(Wx^1+0.5*Wx^0)]
      do j1 = 0, 2
       j20 = jj0(n) + j1
       j2 = j(n) + j1
       dvol = ay0(j1, n)*xx(n, 1)
       dvolh = ay1(j1, n)*xx(n, 1)
       do i1 = x0, x1
        i2 = i1 + ih
        jcurr(i2, j20, 1, 3) = jcurr(i2, j20, 1, 3) + currx(i1)*dvol
        jcurr(i2, j2, 1, 3) = jcurr(i2, j2, 1, 3) + curry(i1)*dvolh
       end do
      end do
     end do 
    end associate
   end if
   !===================================
  end subroutine
  subroutine esirkepov_2d_curr_old(sp_loc, pt, jcurr, np)

   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :), jcurr(:, :, :, :)
   integer, intent (in) :: np
   real (dp) :: dvol
   type(interp_coeff), allocatable :: interp
   real (dp) :: ax0(0:3), ay0(0:3), xp1(3), xp0(3)
   real (dp) :: ax1(0:3), ay1(0:3), vp(3)
   real (dp) :: axh(0:4), axh0(0:4), axh1(0:4), ayh(0:4)
   real (dp) :: currx(0:4), curry(0:4)
   real (sp) :: wght
   integer :: i, j, ii0, jj0, i1, j1, i2, j2, n
   integer :: ih, jh, x0, x1, y0, y1
   !==========================
   !Iform=0 or 1 IMPLEMENTS the ESIRKEPOV SCHEME for LINEAR-QUADRATIC SHAPE
   ! ==============================Only new and old positions needed
   ax1 = zero_dp
   ay1 = zero_dp
   ax0 = zero_dp
   ay0 = zero_dp
   !======================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   select case (ndim)
   case (2)
    if (curr_ndim==2) then !Two current components
     do n = 1, np
      pt(n, 1:2) = sp_loc%part(n, 1:2) !x-y-new  t^(n+1)
      wgh_cmp = sp_loc%part(n, 5)
      wght = charge*wgh
      pt(n, 5) = wght
     end do
     call set_local_2d_positions(pt, 2, np)
     !========================
     ii0 = 0
     jj0 = 0
     i = 0
     j = 0

     do n = 1, np
      xp1(1:2) = pt(n, 1:2) !x-y  -new
      xp0(1:2) = pt(n, 3:4) !x-y  -old
      wght = real(pt(n,5), sp) !w*q
      !=====================
      call qden_2d_wgh( xp0, interp )

      ax0(0:2) = interp%coeff_x(0:2)
      ay0(0:2) = interp%coeff_y(0:2)
 
      ii0 = interp%ix
      jj0 = interp%iy


      call qden_2d_wgh( xp1, interp )

      ax1(0:2) = interp%coeff_x(0:2)
      ay1(0:2) = interp%coeff_y(0:2)
 
      i = interp%ix
      j = interp%iy

      axh(0:4) = zero_dp
      ih = i - ii0 + 1
      do i1 = 0, 2
       axh(ih+i1) = ax1(i1)
      end do
      currx(0) = -axh(0)
      do i1 = 1, 3
       currx(i1) = currx(i1-1) + ax0(i1-1) - axh(i1)
      end do
      currx(4) = currx(3) - axh(4)
      do i1 = 1, 3
       axh(i1) = axh(i1) + ax0(i1-1)
      end do
      currx(0:4) = wght*currx(0:4)
      x0 = min(ih, 1)
      x1 = max(ih+2, 3)
      !-------
      jh = j - jj0 + 1
      ayh(0:4) = zero_dp
      do i1 = 0, 2
       ayh(jh+i1) = ay1(i1)
      end do
      curry(0) = -ayh(0)
      do i1 = 1, 3
       curry(i1) = curry(i1-1) + ay0(i1-1) - ayh(i1)
      end do
      curry(4) = curry(3) - ayh(4)
      curry(0:4) = wght*curry(0:4)
      !========================================
      do i1 = 1, 3
       ayh(i1) = ayh(i1) + ay0(i1-1)
      end do
      y0 = min(jh, 1)
      y1 = max(jh+2, 3)
      !================dt*J_x

      jh = jj0 - 1

      ih = ii0 - 1

      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + ayh(j1)*currx(i1)
       end do
      end do
      !================dt*J_y
      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + axh(i1)*curry(j1)
       end do
      end do
     end do
    end if
    if (curr_ndim==3) then !Three currents conditions in 2D grid
     do n = 1, np
      pt(n, 1:3) = sp_loc%part(n, 1:3) !x-y-z -new  t^(n+1)
      wgh_cmp = sp_loc%part(n, 7)
      wght = charge*wgh
      pt(n, 7) = wght
     end do
     call set_local_2d_positions(pt, 2, np)
     !==============================
     do n = 1, np
      xp1(1:3) = pt(n, 1:3) !increments xyz-new
      xp0(1:3) = pt(n, 4:6) !increments xyz z-old
      wght = real(pt(n,7), sp)
      vp(3) = xp1(3) - xp0(3) !dt*v_z(n+1/2)
      vp(3) = wght*vp(3)/3. !dt*q*w*vz/3
      !=====================

      call qden_2d_wgh( xp0, interp )

      ax0(0:2) = interp%coeff_x(0:2)
      ay0(0:2) = interp%coeff_y(0:2)
 
      ii0 = interp%ix
      jj0 = interp%iy


      call qden_2d_wgh( xp1, interp )

      ax1(0:2) = interp%coeff_x(0:2)
      ay1(0:2) = interp%coeff_y(0:2)
 
      i = interp%ix
      j = interp%iy

      axh(0:4) = zero_dp
      ih = i - ii0 + 1
      x0 = min(ih, 1)
      x1 = max(ih+2, 3)
      do i1 = 0, 2
       axh(ih+i1) = ax1(i1)
      end do
      currx(0) = -axh(0)
      do i1 = 1, 3
       currx(i1) = currx(i1-1) + ax0(i1-1) - axh(i1)
      end do
      currx(4) = currx(3) - axh(4)
      do i1 = 1, 3
       axh(i1) = axh(i1) + ax0(i1-1)
      end do
      currx(0:4) = wght*currx(0:4)

      axh0(0:4) = 0.5*axh(0:4)
      axh1(0:4) = axh(0:4)
      do i1 = 1, 3
       axh0(i1) = axh0(i1) + ax0(i1-1)
       axh1(i1) = axh1(i1) + 0.5*ax0(i1-1)
       axh(i1) = axh(i1) + ax0(i1-1) !Wx^0+Wx^1)
      end do

      jh = j - jj0 + 1
      y0 = min(jh, 1)
      y1 = max(jh+2, 3)

      ayh(0:4) = zero_dp
      do i1 = 0, 2
       ayh(jh+i1) = ay1(i1)
      end do
      curry(0) = -ayh(0)
      do i1 = 1, 3
       curry(i1) = curry(i1-1) + ay0(i1-1) - ayh(i1)
      end do
      curry(4) = curry(3) - ayh(4)
      curry(0:4) = wght*curry(0:4)
      do i1 = 1, 3
       ayh(i1) = ayh(i1) + ay0(i1-1)
      end do
      !================dt*J_x= currx*(Wy^0+Wy^1) to be multiplied by dx/2
      ih = ii0 - 1
      jh = jj0 - 1
      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + ayh(j1)*currx(i1)
       end do
      end do
      !================dt*J_y= curry*(Wx^0+Wx^1)
      do j1 = y0, y1
       j2 = jh + j1
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + axh(i1)*curry(j1)
       end do
      end do
      !========== dt*J_z Vz*[Wy^0(Wx^0+0.5*Wx^1)+Wy^1*(Wx^1+0.5*Wx^0)]
      do j1 = 0, 2
       j2 = jj0 + j1
       dvol = ay0(j1)*vp(3)
       do i1 = x0, x1
        i2 = i1 + ih
        jcurr(i2, j2, 1, 3) = jcurr(i2, j2, 1, 3) + axh0(i1)*dvol
       end do
       j2 = j + j1
       dvol = ay1(j1)*vp(3)
       do i1 = x0, x1
        i2 = i1 + ih
        jcurr(i2, j2, 1, 3) = jcurr(i2, j2, 1, 3) + axh1(i1)*dvol
       end do
      end do
     end do
    end if
   end select
   !-----------------------
  end subroutine
  !==========================================
  !=============3D=================
  subroutine esirkepov_3d_curr_new(sp_loc, pt, jcurr, np, mempool)

   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (in) :: pt
   real (dp), intent (inout) :: jcurr(:, :, :, :)
   integer, intent (in) :: np
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null()
   type(interp_coeff), pointer :: interp => null(), interp_old => null()
   real (dp), dimension(0:4) :: axh0, axh1, ayh0, ayh1
   real (dp), dimension(0:4) :: axh, ayh, azh
   real (dp), dimension(0:4) :: currx, curry, currz
   real (dp) :: dvol, dvolh
   integer :: ih, jh, kh
   integer :: x0, x1, y0, y1, z0, z1
   integer :: i1, j1, i2, j2, k1, k2, i20, j20, k20, n
   !==========================
   !Iform=0 or 1 IMPLEMENTS the ESIRKEPOV SCHEME for LINEAR-QUADRATIC SHAPE
   ! ==============================Only new and old positions needed
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=================================
   ! Do not execute for test particles
   !=================================
   if ( sp_loc%istest() ) return
   !=================================
   ! Do not execute for immobile particles
   !=================================
   if ( .not. sp_loc%ismobile() ) return
   !=============================================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   call interp_realloc(mempool%interp_old, np, sp_loc%pick_dimensions())
   interp_old => mempool%interp_old
   call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
    xx => mempool%mp_xx_2d_A
   !=============================================================

   ! Interpolation on new positions
   xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
   xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
   xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
   
   call qden_3d_wgh( xx(1:np, 1:3), interp, mempool )
   
   ! Interpolation on old positions
   xx(1:np, 1) = set_local_positions( pt, X_COMP )
   xx(1:np, 2) = set_local_positions( pt, Y_COMP )
   xx(1:np, 3) = set_local_positions( pt, Z_COMP )
   
   call qden_3d_wgh( xx(1:np, 1:3), interp_old, mempool )

   ! Recycling xx since it's not needed anymore
   xx(1:np, 1) = sp_loc%pick_charge()*sp_loc%weight(1:np)

   associate( ax1 => interp%coeff_x_rank2, &
              ay1 => interp%coeff_y_rank2, &
              az1 => interp%coeff_z_rank2, &
              i => interp%ix_rank2, &
              j => interp%iy_rank2, &
              k => interp%iz_rank2, &
              ax0 => interp_old%coeff_x_rank2, &
              ay0 => interp_old%coeff_y_rank2, &
              az0 => interp_old%coeff_z_rank2, &
              ii0 => interp_old%ix_rank2, &
              jj0 => interp_old%iy_rank2, &
              kk0 => interp_old%iz_rank2 )

    do n = 1, np

     axh(0:4) = zero_dp
     ayh(0:4) = zero_dp
     azh(0:4) = zero_dp

     ih = i(n) - ii0(n) + 1
     !========== direct Jx-inversion
     do i1 = 0, 2
      axh(ih + i1) = ax1(n, i1)
     end do
     currx(0) = -axh(0)
     do i1 = 1, 3
      currx(i1) = currx(i1-1) + ax0(i1-1, n) - axh(i1)
     end do
     currx(4) = currx(3) - axh(4)
     do i1 = 0, 4
      currx(i1) = xx(n, 1)*currx(i1)
     end do
     !=======================
     axh0(0:4) = 0.5*axh(0:4)
     axh1(0:4) = axh(0:4)
     do i1 = 1, 3
      axh0(i1) = axh0(i1) + ax0(i1-1, n)
      axh1(i1) = axh1(i1) + 0.5*ax0(i1-1, n)
     end do

     x0 = min(ih, 1)
     x1 = max(ih + 2, 3)

     !========== direct Jy-inversion
     jh = j(n) - jj0(n) + 1 !=[0,1,2]
     do i1 = 0, 2
      ayh(jh + i1) = ay1(i1, n)
     end do
     curry(0) = -ayh(0)
     do i1 = 1, 3
      curry(i1) = curry(i1-1) + ay0(i1-1, n) - ayh(i1)
     end do
     curry(4) = curry(3) - ayh(4)
     do i1 = 0, 4
      curry(i1) = xx(n, 1)*curry(i1)
     end do
     !=====================================
     !                                 Jx =>    Wz^0(0.5*wy^1+Wy^0)=Wz^0*ayh0
     !                                          Wz^1(wy^1+0.5*Wy^0)=Wz^1*ayh1
     !==============================
     ayh0(0:4) = 0.5*ayh(0:4)
     ayh1(0:4) = ayh(0:4)
     do i1 = 1, 3
      ayh0(i1) = ayh0(i1) + ay0(i1-1, n)
      ayh1(i1) = ayh1(i1) + 0.5*ay0 (i1-1, n)
     end do 
     y0 = min(jh, 1) ![0,1]
     y1 = max(jh + 2, 3) ![3,4]

     !============= Direct Jz inversion
     kh = k(n) - kk0(n) + 1
     do i1 = 0, 2
      azh(kh + i1) = az1(i1, n)
     end do
     currz(0) = -azh(0)
     do i1 = 1, 3
      currz(i1) = currz(i1-1) + az0(i1-1, n) - azh(i1)
     end do
     currz(4) = currz(3) - azh(4)
     do i1 = 0, 4
      currz(i1) = xx(n, 1)*currz(i1)
     end do
     z0 = min(kh, 1)
     z1 = max(kh + 2, 3)
     !================Jx=DT*drho_x to be inverted==================
     jh = jj0(n) - 1
     !====================
     ih = ii0(n) - 1
     !========================================
     do k1 = 0, 2
      k20 = kk0(n) + k1
      k2 = k(n) + k1
      do j1 = y0, y1
       j2 = jh + j1
       dvol = ayh0(j1)*az0(k1, n)
       dvolh = ayh1(j1)*az1(k1, n)
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, k20, 1) = jcurr(i2, j2, k20, 1) + &
          dvol*currx(i1)
        jcurr(i2, j2, k2, 1) = jcurr(i2, j2, k2, 1) + &
         dvolh*currx(i1)
       end do
      end do
     end do
     !================Jy
     do k1 = 0, 2
      k20 = kk0(n) + k1
      k2 = k(n) + k1
      do j1 = y0, y1
       j2 = jh + j1
       dvol = curry(j1)*az0(k1, n)
       dvolh = curry(j1)*az1(k1, n)
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j2, k20, 2) = jcurr(i2, j2, k20, 2) + &
         axh0(i1)*dvol
        jcurr(i2, j2, k2, 2) = jcurr(i2, j2, k2, 2) + &
         axh1(i1)*dvolh
       end do
      end do
     end do
     !================Jz
     kh = kk0(n) - 1
     do k1 = z0, z1
      k2 = kh + k1
      do j1 = 0, 2
       j20 = jj0(n) + j1
       j2 = j(n) + j1
       dvol = ay0(j1, n)*currz(k1)
       dvolh = ay1(j1, n)*currz(k1)
       do i1 = x0, x1
        i2 = ih + i1
        jcurr(i2, j20, k2, 3) = jcurr(i2, j20, k2, 3) + &
         axh0(i1)*dvol
        jcurr(i2, j2, k2, 3) = jcurr(i2, j2, k2, 3) + &
         axh1(i1)*dvolh
       end do
      end do
     end do
    end do
   !============= Curr data on [1:n+4] extended range
   end associate

  end subroutine

  subroutine esirkepov_3d_curr_old(sp_loc, pt, jcurr, np)

   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :), jcurr(:, :, :, :)
   integer, intent (in) :: np
   real (dp) :: dvol, dvolh
   real (dp) :: ax0(0:2), ay0(0:2), az0(0:2), xp0(1:3)
   real (dp) :: ax1(0:2), ay1(0:2), az1(0:2), xp1(1:3)
   real (dp) :: axh(0:4), ayh(0:4), azh(0:4)
   real (dp) :: axh0(0:4), axh1(0:4), ayh0(0:4), ayh1(0:4)
   real (dp) :: currx(0:4), curry(0:4), currz(0:4)
   type(interp_coeff), allocatable :: interp
   real (sp) :: wght
   integer :: i, j, k, ii0, jj0, kk0, i1, j1, k1, i2, j2, k2, n
   integer :: x0, x1, y0, y1, z0, z1, ih, jh, kh
   !=======================
   !Enter pt(4:6) old positions sp_loc(1:3) new positions

   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   ax1(0:2) = zero_dp
   ay1(0:2) = zero_dp
   az1(0:2) = zero_dp
   az0(0:2) = zero_dp
   ax0(0:2) = zero_dp
   ay0(0:2) = zero_dp
   axh(0:4) = zero_dp
   ayh(0:4) = zero_dp
   azh(0:4) = zero_dp
   currx(0:4) = zero_dp
   curry(0:4) = zero_dp
   currz(0:4) = zero_dp
   axh0(0:4) = zero_dp
   ayh0(0:4) = zero_dp
   axh1(0:4) = zero_dp
   ayh1(0:4) = zero_dp
   ! ==============================Only new and old positions needed
   do n = 1, np
    pt(n, 1:3) = sp_loc%part(n, 1:3) !x-y-z -new  t^(n+1)
    wgh_cmp = sp_loc%part(n, 7)
    wght = charge*wgh
    pt(n, 7) = wght
   end do
   call set_local_3d_positions(pt, 2, np)
   do n = 1, np
    wght = real(pt(n,7), sp)
    xp1(1:3) = pt(n, 1:3) !increments of the new positions
    xp0(1:3) = pt(n, 4:6) !increments of old positions

    call qden_3d_wgh( xp0, interp )

    ax0(0:2) = interp%coeff_x(0:2)
    ay0(0:2) = interp%coeff_y(0:2)
    az0(0:2) = interp%coeff_z(0:2)

    ii0 = interp%ix
    jj0 = interp%iy
    kk0 = interp%iz

    call qden_3d_wgh( xp1, interp )

    ax1(0:2) = interp%coeff_x(0:2)
    ay1(0:2) = interp%coeff_y(0:2)
    az1(0:2) = interp%coeff_z(0:2)

    i = interp%ix
    j = interp%iy
    k = interp%iz

    axh(0:4) = zero_dp
    ih = i - ii0 + 1
    !========== direct Jx-inversion
    do i1 = 0, 2
     axh(ih+i1) = ax1(i1)
    end do
    currx(0) = -axh(0)
    do i1 = 1, 3
     currx(i1) = currx(i1-1) + ax0(i1-1) - axh(i1)
    end do
    currx(4) = currx(3) - axh(4)
    currx(0:4) = wght*currx(0:4)
    !=======================
    axh0(0:4) = 0.5*axh(0:4)
    axh1(0:4) = axh(0:4)
    do i1 = 1, 3
     axh0(i1) = axh0(i1) + ax0(i1-1)
     axh1(i1) = axh1(i1) + 0.5*ax0(i1-1)
    end do

    x0 = min(ih, 1)
    x1 = max(ih+2, 3)

    !========== direct Jy-inversion
    jh = j - jj0 + 1 !=[0,1,2]
    ayh(0:4) = zero_dp
    do i1 = 0, 2
     ayh(jh+i1) = ay1(i1)
    end do
    curry(0) = -ayh(0)
    do i1 = 1, 3
     curry(i1) = curry(i1-1) + ay0(i1-1) - ayh(i1)
    end do
    curry(4) = curry(3) - ayh(4)
    curry(0:4) = wght*curry(0:4)
    !=====================================
    !                                 Jx =>    Wz^0(0.5*wy^1+Wy^0)=Wz^0*ayh0
    !                                          Wz^1(wy^1+0.5*Wy^0)=Wz^1*ayh1
    !==============================
    ayh0(0:4) = 0.5*ayh(0:4)
    ayh1(0:4) = ayh(0:4)
    do i1 = 1, 3
     ayh0(i1) = ayh0(i1) + ay0(i1-1)
     ayh1(i1) = ayh1(i1) + 0.5*ay0(i1-1)
    end do
    y0 = min(jh, 1) ![0,1]
    y1 = max(jh+2, 3) ![3,4]

    ! Direct Jz inversion
    kh = k - kk0 + 1
    azh(0:4) = zero_dp
    do i1 = 0, 2
     azh(kh+i1) = az1(i1)
    end do
    currz(0) = -azh(0)
    do i1 = 1, 3
     currz(i1) = currz(i1-1) + az0(i1-1) - azh(i1)
    end do
    currz(4) = currz(3) - azh(4)
    currz(0:4) = wght*currz(0:4)
    z0 = min(kh, 1)
    z1 = max(kh+2, 3)
    !================Jx=DT*drho_x to be inverted==================
    jh = jj0 - 1
    !====================
    ih = ii0 - 1
    do k1 = 0, 2
     do j1 = y0, y1
      j2 = jh + j1
      dvol = ayh0(j1)*az0(k1)
      dvolh = ayh1(j1)*az1(k1)
      do i1 = x0, x1
       i2 = ih + i1
       jcurr(i2, j2, kk0+k1, 1) = jcurr(i2, j2, kk0+k1, 1) + &
         dvol*currx(i1)
       jcurr(i2, j2, k+k1, 1) = jcurr(i2, j2, k+k1, 1) + dvolh*currx(i1)
      end do
     end do
    end do
    !================Jy
    do k1 = 0, 2
     do j1 = y0, y1
      j2 = jh + j1
      dvol = curry(j1)*az0(k1)
      dvolh = curry(j1)*az1(k1)
      do i1 = x0, x1
       i2 = ih + i1
       jcurr(i2, j2, kk0+k1, 2) = jcurr(i2, j2, kk0+k1, 2) + axh0(i1)*dvol
       jcurr(i2, j2, k+k1, 2) = jcurr(i2, j2, k+k1, 2) + axh1(i1)*dvolh
      end do
     end do
    end do
    !================Jz
    kh = kk0 - 1

    do k1 = z0, z1
     k2 = kh + k1
     do j1 = 0, 2
      dvol = ay0(j1)*currz(k1)
      dvolh = ay1(j1)*currz(k1)
      do i1 = x0, x1
       i2 = ih + i1
       jcurr(i2, jj0+j1, k2, 3) = jcurr(i2, jj0+j1, k2, 3) + axh0(i1)*dvol
       jcurr(i2, j+j1, k2, 3) = jcurr(i2, j+j1, k2, 3) + axh1(i1)*dvolh
      end do
     end do
    end do
   end do
   !============= Curr data on [1:n+4] extended range
  end subroutine
  !===============================
  ! NO CHARGE PRESERVING CURRENT DENSITY
  !=========================
  subroutine ncdef_2d_curr_new(sp_loc, pt, jcurr, np, mempool)

   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (in) :: pt
   real (dp), intent (inout) :: jcurr(:, :, :, :)
   integer, intent (in) :: np
   type(memory_pool_t), pointer, intent(in) :: mempool

   real (dp), pointer, contiguous, dimension(:, :) :: xx => null(), vp => null()
   real (dp), pointer, contiguous, dimension(:) :: weight => null()
   type(interp_coeff), pointer :: interp => null(), interp_old => null()
   real (dp) ::dvol(3)
   integer :: i1, j1, i2, j2, n
   !=======================
   !Enter pt(3:4) old x-y positions
   !====================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=================================
   ! Do not execute for test particles
   !=================================
   if ( sp_loc%istest() ) return
   !=================================
   ! Do not execute for immobile particles
   !=================================
   if ( .not. sp_loc%ismobile() ) return
   !=============================================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   call interp_realloc(mempool%interp_old, np, sp_loc%pick_dimensions())
   interp_old => mempool%interp_old
   call mp_xx_realloc(mempool%mp_xx_2d_A, np, 2, mempool)
   call mp_xx_realloc(mempool%mp_xx_2d_B, np, 2, mempool)

   xx => mempool%mp_xx_2d_A
   vp => mempool%mp_xx_2d_B

   !=============================================================
   ! Interpolation on new positions
   xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
   xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )

   call qlh_2d_spline( xx(1:np, 1:2), interp, mempool )

   ! Interpolation on old positions
   xx(1:np, 1) = set_local_positions( pt, X_COMP )
   xx(1:np, 2) = set_local_positions( pt, Y_COMP )

   call qlh_2d_spline( xx(1:np, 1:2), interp_old, mempool )
   
   associate( ax1 => interp%coeff_x_rank2, &
              ay1 => interp%coeff_y_rank2, &
              axh1 => interp%h_coeff_x_rank2, &
              ayh1 => interp%h_coeff_y_rank2, &
              i => interp%ix_rank2, &
              ih => interp%ihx_rank2, &
              j => interp%iy_rank2, &
              jh => interp%ihy_rank2, &
              ax0 => interp_old%coeff_x_rank2, &
              ay0 => interp_old%coeff_y_rank2, &
              axh0 => interp_old%h_coeff_x_rank2, &
              ayh0 => interp_old%h_coeff_y_rank2, &
              ii0 => interp_old%ix_rank2, &
              ih0 => interp_old%ihx_rank2, &
              jj0 => interp_old%iy_rank2, &
              jh0 => interp_old%ihy_rank2 )

    !==========================
    ! Warning: this call must be after qqh_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    weight => mempool%mp_xx_1d_A

    weight(1:np) = sp_loc%pick_charge()*sp_loc%weight(1:np)

    !=== Make sure on pt%call_comp( INV_GAMMA ) the actual stored factor
    ! is dt/gam and not just 1/gam ===

    vp(1:np, 1) = 0.5*weight(1:np) * pt%gamma_inv(1:np) * sp_loc%px(1:np)
    vp(1:np, 2) = 0.5*weight(1:np) * pt%gamma_inv(1:np) * sp_loc%py(1:np)

    do n = 1, np
     !===============Jx ========
     do j1 = 0, 2
      j2 = j(n) + j1
      dvol(1) = vp(n, 1)*ay1(n, j1)
      do i1 = 0, 1
       i2 = ih(n) + i1
       jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + dvol(1)*axh1(n, i1)
      end do
      j2 = jj0(n) + j1
      dvol(1) = vp(n, 1)*ay0(n, j1)
      do i1 = 0, 1
       i2 = ih0(n) + i1
       jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + dvol(1)*axh0(n, i1)
      end do
     end do
     !=========== Jy             
     do j1 = 0, 1
      j2 = jh0(n) + j1
      dvol(2) = vp(n, 2)*ayh0(n, j1)
      do i1 = 0, 2
       i2 = ii0(n) + i1
       jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + dvol(2)*ax0(n, i1)
      end do
      j2 = jh(n) + j1
      dvol(2) = vp(n, 2)*ayh1(n, j1)
      do i1 = 0, 2
       i2 = i(n) + i1
       jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + dvol(2)*ax1(n, i1)
      end do
     end do
    end do
   end associate
  end subroutine
  !========================
  subroutine ncdef_2d_curr_old(sp_loc, pt, jcurr, np)

   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :), jcurr(:, :, :, :)
   integer, intent (in) :: np
   real (dp) :: axh0(0:2), ayh0(0:2)
   real (dp) :: axh1(0:2), ayh1(0:2)
   real (dp) :: ax0(0:2), ay0(0:2), xp0(1:2)
   real (dp) :: ax1(0:2), ay1(0:2), xp1(1:2)
   real (dp) :: vp(3), dvol(3)
   type(interp_coeff), allocatable :: interp
   real (sp) :: wght
   integer :: i, j, ii0, jj0, i1, j1, i2, j2, n
   integer :: jh0, jh, ih0, ih
   !=======================
   !Enter pt(3:4) old x-y positions
   !=====================================
   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   do n = 1, np
    pt(n, 1:2) = sp_loc%part(n, 1:2) !(x,y,z) new
   end do
   call set_local_2d_positions(pt, 2, np)
   !========== pt(n,5) = dt/gam
   do n = 1, np
    wgh_cmp = sp_loc%part(n, 5)
    wght = charge*wgh !w*q for  q=e, ion_charge
    vp(1:2) = wght*pt(n, 5)*sp_loc%part(n, 3:4) !dt*q*wgh*P/gam at t^{n+1/2}
    vp(1:2) = 0.5*vp(1:2) !1/2 * V*q*wgh*dt

    xp1(1:2) = pt(n, 1:2)
    xp0(1:2) = pt(n, 3:4)

    call qlh_2d_spline( xp0, interp )

    ax0(0:2) = interp%coeff_x(0:2)
    ay0(0:2) = interp%coeff_y(0:2)
    axh0(0:1) = interp%h_coeff_x(0:1)
    ayh0(0:1) = interp%h_coeff_y(0:1)

    ii0 = interp%ix
    ih0 = interp%ihx
    jj0 = interp%iy
    jh0 = interp%ihy

    !====================

    call qlh_2d_spline( xp1, interp )

    ax1(0:2) = interp%coeff_x(0:2)
    ay1(0:2) = interp%coeff_y(0:2)
    axh1(0:1) = interp%h_coeff_x(0:1)
    ayh1(0:1) = interp%h_coeff_y(0:1)

    i = interp%ix
    ih = interp%ihx
    j = interp%iy
    jh = interp%ihy
    
    !===============Jx ========
    do j1 = 0, 2
     j2 = j + j1
     dvol(1) = vp(1)*ay1(j1)
     do i1 = 0, 1
      i2 = ih + i1
      jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + dvol(1)*axh1(i1)
     end do
     j2 = jj0 + j1
     dvol(1) = vp(1)*ay0(j1)
     do i1 = 0, 1
      i2 = ih0 + i1
      jcurr(i2, j2, 1, 1) = jcurr(i2, j2, 1, 1) + dvol(1)*axh0(i1)
     end do
    end do
    !=========== Jy             
    do j1 = 0, 1
     j2 = jh0 + j1
     dvol(2) = vp(2)*ayh0(j1)
     do i1 = 0, 2
      i2 = ii0 + i1
      jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + dvol(2)*ax0(i1)
     end do
     j2 = jh + j1
     dvol(2) = vp(2)*ayh1(j1)
     do i1 = 0, 2
      i2 = i + i1
      jcurr(i2, j2, 1, 2) = jcurr(i2, j2, 1, 2) + dvol(2)*ax1(i1)
     end do
    end do
   end do
  end subroutine
  !========================
  subroutine ncdef_3d_curr_new(sp_loc, pt, jcurr, np, mempool)

   type (species_new), intent (in) :: sp_loc
   type (species_aux), intent (in) :: pt
   real (dp), intent (inout) :: jcurr(:, :, :, :)
   integer, intent (in) :: np
   type(memory_pool_t), pointer, intent(in) :: mempool

   real (dp), pointer, contiguous, dimension(:, :) :: xx => null(), vp => null()
   real (dp), pointer, contiguous, dimension(:) :: weight => null()
   type(interp_coeff), pointer :: interp => null(), interp_old => null()
   real (dp) ::dvol(3)
   integer :: i1, j1, k1, i2, j2, k2, n
   !=======================
   ! Current densities defined by alternating order (quadratic/linear) shapes
   ! Enter pt(4:6)=old positions sp_loc(1:3)=new positions 
   !WARNING : to be used ONLY within the one cycle partcle integration scheme
   !==========================================
   ! Exit in jcurr(1:3) =[Drho,J_y,J_z]   !Drho= rho^{new}-rho^{old}
   ! Component J_x recovered by enforcing the continuity equation on a grid
   !=============================================
   !=================================
   ! Do not execute without particles
   !=================================
   if ( sp_loc%empty ) return
   !=================================
   ! Do not execute for test particles
   !=================================
   if ( sp_loc%istest() ) return
   !=================================
   ! Do not execute for immobile particles
   !=================================
   if ( .not. sp_loc%ismobile() ) return
   !=============================================================
   call interp_realloc(mempool%interp, np, sp_loc%pick_dimensions())
   interp => mempool%interp
   call interp_realloc(mempool%interp_old, np, sp_loc%pick_dimensions())
   interp_old => mempool%interp_old
   call mp_xx_realloc(mempool%mp_xx_2d_A, np, 3, mempool)
   call mp_xx_realloc(mempool%mp_xx_2d_B, np, 3, mempool)

   xx => mempool%mp_xx_2d_A
   vp => mempool%mp_xx_2d_B

   !=============================================================
   ! Interpolation on new positions
   xx(1:np, 1) = set_local_positions( sp_loc, X_COMP )
   xx(1:np, 2) = set_local_positions( sp_loc, Y_COMP )
   xx(1:np, 3) = set_local_positions( sp_loc, Z_COMP )
 
   call qlh_3d_spline( xx(1:np, 1:3), interp, mempool )
 
    ! Interpolation on old positions
   xx(1:np, 1) = set_local_positions( pt, X_COMP )
   xx(1:np, 2) = set_local_positions( pt, Y_COMP )
   xx(1:np, 3) = set_local_positions( pt, Z_COMP )
 
   call qlh_3d_spline( xx(1:np, 1:3), interp_old, mempool )

   associate( ax1 => interp%coeff_x_rank2, &
              ay1 => interp%coeff_y_rank2, &
              az1 => interp%coeff_z_rank2, &
              axh1 => interp%h_coeff_x_rank2, &
              ayh1 => interp%h_coeff_y_rank2, &
              azh1 => interp%h_coeff_z_rank2, &
              i => interp%ix_rank2, &
              ih => interp%ihx_rank2, &
              j => interp%iy_rank2, &
              jh => interp%ihy_rank2, &
              k => interp%iz_rank2, &
              kh => interp%ihz_rank2, &
              ax0 => interp_old%coeff_x_rank2, &
              ay0 => interp_old%coeff_y_rank2, &
              az0 => interp_old%coeff_z_rank2, &
              axh0 => interp_old%h_coeff_x_rank2, &
              ayh0 => interp_old%h_coeff_y_rank2, &
              azh0 => interp_old%h_coeff_z_rank2, &
              ii0 => interp_old%ix_rank2, &
              ih0 => interp_old%ihx_rank2, &
              jj0 => interp_old%iy_rank2, &
              jh0 => interp_old%ihy_rank2, &
              kk0 => interp_old%iz_rank2, &
              kh0 => interp_old%ihz_rank2 )

              
    !==========================
    ! Warning: this call must be after qqh_2d_spline since
    ! in that routine the 1d arrays are used
    call array_realloc_1d(mempool%mp_xx_1d_A, np)
    weight => mempool%mp_xx_1d_A

    weight(1:np) = sp_loc%pick_charge()*sp_loc%weight(1:np)

    !=== Make sure on pt%call_comp( INV_GAMMA ) the actual stored factor
    ! is dt/gam and not just 1/gam ===

    vp(1:np, 1) = 0.5*weight(1:np) * pt%gamma_inv(1:np) * sp_loc%px(1:np)
    vp(1:np, 2) = 0.5*weight(1:np) * pt%gamma_inv(1:np) * sp_loc%py(1:np)
    vp(1:np, 3) = 0.5*weight(1:np) * pt%gamma_inv(1:np) * sp_loc%pz(1:np)


    do n = 1, np
     !======================   Jx
     do k1 = 0, 2
      k2 = k(n) + k1
      do j1 = 0, 2
       j2 = j(n) + j1
       dvol(1) = vp(n, 1)*ay1(n, j1)*az1(n, k1)
       do i1 = 0, 1
        i2 = ih(n) + i1
        jcurr(i2, j2, k2, 1) = jcurr(i2, j2, k2, 1) + dvol(1)*axh1(n, i1)
       end do
      end do
      k2 = kk0(n) + k1
      do j1 = 0, 2
       j2 = jj0(n) + j1
       dvol(1) = vp(n, 1)*ay0(n, j1)*az0(n, k1)
       do i1 = 0, 1
        i2 = ih0(n) + i1
        jcurr(i2, j2, k2, 1) = jcurr(i2, j2, k2, 1) + dvol(1)*axh0(n, i1)
       end do
      end do
     end do
     !================Jy-Jz=============
     do k1 = 0, 2
      k2 = kk0(n) + k1
      do j1 = 0, 1
       j2 = jh0(n) + j1
       dvol(2) = vp(n, 2)*ayh0(n, j1)*az0(n, k1)
       do i1 = 0, 2
        i2 = ii0(n) + i1
        jcurr(i2, j2, k2, 2) = jcurr(i2, j2, k2, 2) + dvol(2)*ax0(n, i1)
       end do
      end do
      k2 = k(n) + k1
      do j1 = 0, 1
       j2 = jh(n) + j1
       dvol(2) = vp(n, 2)*ayh1(n, j1)*az1(n, k1)
       do i1 = 0, 2
        i2 = i(n) + i1
        jcurr(i2, j2, k2, 2) = jcurr(i2, j2, k2, 2) + dvol(2)*ax1(n, i1)
       end do
      end do
     end do
     do k1 = 0, 1
      k2 = kh0(n) + k1
      do j1 = 0, 2
       j2 = jj0(n) + j1
       dvol(3) = vp(n, 3)*ay0(n, j1)*azh0(n, k1)
       do i1 = 0, 2
        i2 = ii0(n) + i1
        jcurr(i2, j2, k2, 3) = jcurr(i2, j2, k2, 3) + dvol(3)*ax0(n, i1)
       end do
      end do
      k2 = kh(n) + k1
      do j1 = 0, 2
       j2 = j(n) + j1
       dvol(3) = vp(n, 3)*ay1(n, j1)*azh1(n, k1)
       do i1 = 0, 2
        i2 = i(n) + i1
        jcurr(i2, j2, k2, 3) = jcurr(i2, j2, k2, 3) + dvol(3)*ax1(n, i1)
       end do
      end do
     end do
    end do
   end associate
   !============= Curr and density data on [0:n+3] extended range
  end subroutine
  !==========================
  subroutine ncdef_3d_curr_old(sp_loc, pt, jcurr, np)

   type (species), intent (in) :: sp_loc
   real (dp), intent (inout) :: pt(:, :), jcurr(:, :, :, :)
   integer, intent (in) :: np
   real (dp) :: dvol(3), gam_inv
   real (dp) :: xp0(3), xp1(3)
   real (dp) :: ax0(0:2), ay0(0:2), az0(0:2)
   real (dp) :: ax1(0:2), ay1(0:2), az1(0:2)
   real (dp) :: axh0(0:1), ayh0(0:1), azh0(0:1)
   real (dp) :: axh1(0:1), ayh1(0:1), azh1(0:1)
   real (dp) :: vp(3)
   type(interp_coeff), allocatable :: interp
   real (sp) :: wght
   integer :: i, j, k, ii0, jj0, kk0, i1, j1, k1, i2, j2, k2, n
   integer :: ih, jh, kh, ih0, jh0, kh0
   !=======================
   ! Current densities defined by alternating order (quadratic/linear) shapes
   ! Enter pt(4:6)=old positions sp_loc(1:3)=new positions 
   !WARNING : to be used ONLY within the one cycle partcle integration scheme
   !==========================================
   ! Exit in jcurr(1:3) =[Drho,J_y,J_z]   !Drho= rho^{new}-rho^{old}
   ! Component J_x recovered by enforcing the continuity equation on a grid
   !=============================================

   !================================
   call interp_realloc(interp, 1, 3)
   !================================
   do n = 1, np
    pt(n, 1:3) = sp_loc%part(n, 1:3) !(x,y,z) new
   end do
   call set_local_3d_positions(pt, 2, np)

   do n = 1, np
    vp(1:3) = sp_loc%part(n, 4:6) !Momenta at t^{n+1/2}
    wgh_cmp = sp_loc%part(n, 7)
    wght = real(charge*wgh, sp) !w*q for  q=charge
    gam_inv = wght*pt(n, 7) !q*wgh*dt/gam               
    vp(1:3) = 0.5*gam_inv*vp(1:3) !wgh*q*dt*V factor 1/2 from density average

    xp1(1:3) = pt(n, 1:3) !new relative coordinates
    xp0(1:3) = pt(n, 4:6) !old relative coordinates

    call qlh_3d_spline( xp0, interp )

    ax0(0:2) = interp%coeff_x(0:2)
    ay0(0:2) = interp%coeff_y(0:2)
    az0(0:2) = interp%coeff_z(0:2)
    axh0(0:1) = interp%h_coeff_x(0:1)
    ayh0(0:1) = interp%h_coeff_y(0:1)
    azh0(0:1) = interp%h_coeff_z(0:1)

    ii0 = interp%ix
    ih0 = interp%ihx
    jj0 = interp%iy
    jh0 = interp%ihy
    kk0 = interp%iz
    kh0 = interp%ihz

    !====================
    call qlh_3d_spline( xp1, interp )

    ax1(0:2) = interp%coeff_x(0:2)
    ay1(0:2) = interp%coeff_y(0:2)
    az1(0:2) = interp%coeff_z(0:2)
    axh1(0:1) = interp%h_coeff_x(0:1)
    ayh1(0:1) = interp%h_coeff_y(0:1)
    azh1(0:1) = interp%h_coeff_z(0:1)

    i = interp%ix
    ih = interp%ihx
    j = interp%iy
    jh = interp%ihy
    k = interp%iz
    kh = interp%ihz
    !======================   Jx
    do k1 = 0, 2
     k2 = k + k1
     do j1 = 0, 2
      j2 = j + j1
      dvol(1) = vp(1)*ay1(j1)*az1(k1)
      do i1 = 0, 1
       i2 = ih + i1
       jcurr(i2, j2, k2, 1) = jcurr(i2, j2, k2, 1) + dvol(1)*axh1(i1)
      end do
     end do
     k2 = kk0 + k1
     do j1 = 0, 2
      j2 = jj0 + j1
      dvol(1) = vp(1)*ay0(j1)*az0(k1)
      do i1 = 0, 1
       i2 = ih0 + i1
       jcurr(i2, j2, k2, 1) = jcurr(i2, j2, k2, 1) + dvol(1)*axh0(i1)
      end do
     end do
    end do
    !================Jy-Jz=============
    do k1 = 0, 2
     k2 = kk0 + k1
     do j1 = 0, 1
      j2 = jh0 + j1
      dvol(2) = vp(2)*ayh0(j1)*az0(k1)
      do i1 = 0, 2
       i2 = ii0 + i1
       jcurr(i2, j2, k2, 2) = jcurr(i2, j2, k2, 2) + dvol(2)*ax0(i1)
      end do
     end do
     k2 = k + k1
     do j1 = 0, 1
      j2 = jh + j1
      dvol(2) = vp(2)*ayh1(j1)*az1(k1)
      do i1 = 0, 2
       i2 = i + i1
       jcurr(i2, j2, k2, 2) = jcurr(i2, j2, k2, 2) + dvol(2)*ax1(i1)
      end do
     end do
    end do
    do k1 = 0, 1
     k2 = kh0 + k1
     do j1 = 0, 2
      j2 = jj0 + j1
      dvol(3) = vp(3)*ay0(j1)*azh0(k1)
      do i1 = 0, 2
       i2 = ii0 + i1
       jcurr(i2, j2, k2, 3) = jcurr(i2, j2, k2, 3) + dvol(3)*ax0(i1)
      end do
     end do
     k2 = kh + k1
     do j1 = 0, 2
      j2 = j + j1
      dvol(3) = vp(3)*ay1(j1)*azh1(k1)
      do i1 = 0, 2
       i2 = i + i1
       jcurr(i2, j2, k2, 3) = jcurr(i2, j2, k2, 3) + dvol(3)*ax1(i1)
      end do
     end do
    end do
   end do
   !============= Curr and density data on [0:n+3] extended range
  end subroutine
  !==========================
 end module
