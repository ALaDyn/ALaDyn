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
  use particles_def
  use interpolation_lib
  use memory_pool

  implicit none

  real (dp), parameter :: half = 0.5
  real (sp) :: shx, shy, shz
  integer (kind=2) :: err_ind

  interface qqh_1d_spline
   module procedure qqh_1d_spline_real
   module procedure qqh_1d_spline_vector
  end interface
  
  interface qden_1d_wgh
   module procedure qden_1d_wgh_real
   module procedure qden_1d_wgh_vector
  end interface
  
  interface qlh_2d_spline
   module procedure qlh_2d_spline_real
   module procedure qlh_2d_spline_vector
  end interface
    
  interface qqh_2d_spline
   module procedure qqh_2d_spline_real
   module procedure qqh_2d_spline_vector
  end interface
    
    
  interface qden_2d_wgh
   module procedure qden_2d_wgh_real
   module procedure qden_2d_wgh_vector
  end interface
  
  interface cden_2d_wgh
   module procedure cden_2d_wgh_real
   module procedure cden_2d_wgh_vector
  end interface

  interface qlh_3d_spline
   module procedure qlh_3d_spline_real
   module procedure qlh_3d_spline_vector
  end interface

  interface qqh_3d_spline
   module procedure qqh_3d_spline_real
   module procedure qqh_3d_spline_vector
  end interface

  interface qden_3d_wgh
   module procedure qden_3d_wgh_real
   module procedure qden_3d_wgh_vector
  end interface

  interface cden_3d_wgh
   module procedure cden_3d_wgh_real
   module procedure cden_3d_wgh_vector
  end interface

  interface set_local_positions
   module procedure set_local_positions_sa
   module procedure set_local_positions_sn
  end interface

  contains

  !===========================================
  !     Computes particle density charge on grid integer points
  !==============================================

   !=======================
   !  1D
   !=======================
  !DIR$ ATTRIBUTES INLINE :: qqh_1d_spline_real
  subroutine qqh_1d_spline_real( xp, interp_in )
    !! Quadratic interpolation at both integer and half-integer
    !! cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   real (dp) :: xx, sx
   integer :: ord, h_ord
   !======================
   ord = 2
   h_ord = 2
   shx = gcx

   xx = shx + xp(1)
   interp_in%ix = int(xx+half)
   sx = xx - real(interp_in%ix, dp)
   call second_order( sx, interp_in%coeff_x(0:ord) )

   interp_in%ihx = int(xx)
   sx = xx - real(interp_in%ihx, dp)
   call second_order( sx, interp_in%h_coeff_x(0:h_ord) )

   interp_in%ix = interp_in%ix - 1
   interp_in%ihx = interp_in%ihx - 1

  end subroutine
  !=======================

  !DIR$ ATTRIBUTES INLINE :: qqh_1d_spline_vector
  subroutine qqh_1d_spline_vector( xp, interp_in, mempool )
    !! Quadratic interpolation at both integer and half-integer
    !! cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================

   length = SIZE( xp, DIM=1 )
   call array_realloc_1d(mempool%mp_xx_1d_A, length)
   call array_realloc_1d(mempool%mp_xx_1d_B, length)
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int( xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )

   interp_in%ihx_rank2(1:length) = int(xx(1:length))
   sx(1:length) = xx(1:length) - &
   real(interp_in%ihx_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%h_coeff_x_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%ihx_rank2(1:length) = interp_in%ihx_rank2(1:length) - 1

  end subroutine
  !=======================

  !DIR$ ATTRIBUTES INLINE :: qden_1d_wgh_real
  subroutine qden_1d_wgh_real( xp, interp_in )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix
   real (dp) :: xx, sx
   integer :: ord
   !======================
   ord = 2
   shx = gcx

   xx = shx + xp(1)
   ix = int(xx+half)
   sx = xx - real(ix, dp)

   call second_order( sx, interp_in%coeff_x(0:ord))

   interp_in%ix = ix - 1
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: qden_1d_wgh_vector
  subroutine qden_1d_wgh_vector( xp, interp_in, mempool )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================
   length = SIZE( xp, DIM=1 )

   call array_realloc_1d(mempool%mp_xx_1d_A, length)
   call array_realloc_1d(mempool%mp_xx_1d_B, length)
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int( xx(1:length) + half )
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
  end subroutine

  !=======================
  !  2D
  !=======================

  !DIR$ ATTRIBUTES INLINE :: qlh_2d_spline_real
  subroutine qlh_2d_spline_real( xp, interp_in )
  !! Quadratic interpolation at integer cells, linear at
  !! half-integer cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, iy
   real (dp) :: xx, sx
   integer :: ord, h_ord
   !======================
   ord = 2
   h_ord = 2
   shx = gcx
   shy = gcy

   xx = shx + xp(1)
   ix = int(xx+half)
   sx = xx - real(ix, dp)

   call second_order( sx, interp_in%coeff_x(0:ord) )
   call first_order( sx + half, interp_in%h_coeff_x(0:h_ord) )

   xx = shy + xp(2)
   iy = int(xx+half)
   sx = xx - real(iy, dp)

   call second_order( sx, interp_in%coeff_y(0:ord) )
   call first_order( sx + half, interp_in%h_coeff_y(0:h_ord) )


   interp_in%ix = ix - 1
   interp_in%ihx = ix
   interp_in%iy = iy - 1
   interp_in%ihy = iy
   
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: qlh_2d_spline_vector
  subroutine qlh_2d_spline_vector( xp, interp_in, mempool )
  !! Quadratic interpolation at integer cells, linear at
  !! half-integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================

   length = SIZE( xp, DIM=1 )
   call array_realloc_1d(mempool%mp_xx_1d_A, length)
   call array_realloc_1d(mempool%mp_xx_1d_B, length)
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx
   shy = gcy

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )
   call first_order( sx(1:length) + half, interp_in%h_coeff_x_rank2 )

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_y_rank2 )
   call first_order( sx(1:length) + half, interp_in%h_coeff_y_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%ihx_rank2(1:length) = interp_in%ix_rank2(1:length)
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1
   interp_in%ihy_rank2(1:length) = interp_in%iy_rank2(1:length)
   
  end subroutine
  !====================

  !DIR$ ATTRIBUTES INLINE :: qqh_2d_spline_real
  subroutine qqh_2d_spline_real( xp, interp_in )
   !! Quadratic interpolation at both integer and half-integer
   !! cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, ihx, iy, ihy
   real (dp) :: xx, sx
   !======================
   shx = gcx
   shy = gcy

   xx = shx + xp(1)
   ix = int(xx+half)
   sx = xx - real(ix, dp)

   call second_order( sx, interp_in%coeff_x )

   ihx = int(xx)
   sx = xx - real(ihx, dp) - half

   call second_order( sx, interp_in%h_coeff_x )

   xx = shy + xp(2)
   iy = int(xx+half)
   sx = xx - real(iy, dp)

   call second_order( sx, interp_in%coeff_y )

   ihy = int(xx)
   sx = xx - real(ihy, dp) - half

   call second_order( sx, interp_in%h_coeff_y )

   interp_in%ix = ix - 1
   interp_in%ihx = ihx - 1
   interp_in%iy = iy - 1
   interp_in%ihy = ihy - 1
  end subroutine
  !=======================================

  !DIR$ ATTRIBUTES INLINE :: qqh_2d_spline_vector
  subroutine qqh_2d_spline_vector( xp, interp_in, mempool )
  !! Quadratic interpolation at integer cells, linear at
  !! half-integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================
   shx = gcx
   shy = gcy

   length = SIZE( xp, DIM=1 )
   call array_realloc_1d(mempool%mp_xx_1d_A, length)
   call array_realloc_1d(mempool%mp_xx_1d_B, length)
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )

   interp_in%ihx_rank2(1:length) = int(xx(1:length))
   sx(1:length) = xx(1:length) - &
    real(interp_in%ihx_rank2(1:length), dp) - half

   call second_order( sx(1:length), interp_in%h_coeff_x_rank2 )

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_y_rank2 )

   interp_in%ihy_rank2(1:length) = int(xx(1:length))
   sx(1:length) = xx(1:length) - &
    real(interp_in%ihy_rank2(1:length), dp) - half

   call second_order( sx(1:length), interp_in%h_coeff_y_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%ihx_rank2(1:length) = interp_in%ihx_rank2(1:length) - 1
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1
   interp_in%ihy_rank2(1:length) = interp_in%ihy_rank2(1:length) - 1
   
  end subroutine
  !====================

  !DIR$ ATTRIBUTES INLINE :: qden_2d_wgh_real
  subroutine qden_2d_wgh_real( xp, interp_in )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, iy
   real (dp) :: xx, sx
   !======================
   shx = gcx
   shy = gcy

   xx = shx + xp(1)
   ix = int(xx+half)
   sx = xx - real(ix, dp)

   call second_order( sx, interp_in%coeff_x )

   xx = shy + xp(2)
   iy = int(xx+half)
   sx = xx - real(iy, dp)

   call second_order( sx, interp_in%coeff_y )

   interp_in%ix = ix - 1
   interp_in%iy = iy - 1
  end subroutine
  !======================

  !DIR$ ATTRIBUTES INLINE :: qden_2d_wgh_vector
  subroutine qden_2d_wgh_vector( xp, interp_in, mempool )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================
   length = SIZE( xp, DIM=1 )
   call array_realloc_1d( mempool%mp_xx_1d_A, length )
   call array_realloc_1d( mempool%mp_xx_1d_B, length )
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx
   shy = gcy

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int( xx(1:length) + half )
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int( xx(1:length) + half )
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_y_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1

  end subroutine

  !DIR$ ATTRIBUTES INLINE :: cden_2d_wgh_real
  subroutine cden_2d_wgh_real( xp, interp_in )
   !! Cubic interpolation at integer cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, iy
   real (dp) :: xx, sx
   !======================
   shx = gcx
   shy = gcy

   xx = shx + xp(1)
   ix = int(xx)
   sx = xx - real(ix, dp)

   call third_order( sx, interp_in%coeff_x )
   
   xx = shy + xp(2)
   iy = int(xx)
   sx = xx - real(iy, dp)
   
   call third_order( sx, interp_in%coeff_y )

   interp_in%ix = ix - 1
   interp_in%iy = iy - 1
  end subroutine
  !==========================

  !DIR$ ATTRIBUTES INLINE :: cden_2d_wgh_vector
  subroutine cden_2d_wgh_vector( xp, interp_in, mempool )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================
   length = SIZE( xp, DIM=1 )
   call array_realloc_1d( mempool%mp_xx_1d_A, length )
   call array_realloc_1d( mempool%mp_xx_1d_B, length )
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx
   shy = gcy

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int( xx(1:length) )
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call third_order( sx(1:length), interp_in%coeff_x_rank2 )

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int( xx(1:length) )
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call third_order( sx(1:length), interp_in%coeff_y_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1

  end subroutine
  !=======================
  !  3D
  !=======================

  !DIR$ ATTRIBUTES INLINE :: qlh_3d_spline_real
  subroutine qlh_3d_spline_real( xp, interp_in )
   !! Quadratic interpolation at integer cells, linear at
   !! half-integer cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, iy, iz
   real (dp) :: xx, sx
   !======================
   shx = gcx
   shy = gcy
   shz = gcz

   xx = shx + xp(1)
   ix = int(xx+half)
   sx = xx - real(ix, dp)

   call second_order( sx, interp_in%coeff_x )
   call first_order( sx + half, interp_in%h_coeff_x )

   xx = shy + xp(2)
   iy = int(xx+half)
   sx = xx - real(iy, dp)

   call second_order( sx, interp_in%coeff_y )
   call first_order( sx + half, interp_in%h_coeff_y )

   xx = shz + xp(3)
   iz = int(xx+half)
   sx = xx - real(iz, dp)

   call second_order( sx, interp_in%coeff_z )
   call first_order( sx + half, interp_in%h_coeff_z )

   interp_in%ix = ix - 1
   interp_in%ihx = ix
   interp_in%iy = iy - 1
   interp_in%ihy = iy
   interp_in%iz = iz - 1
   interp_in%ihz = iz
  end subroutine
  !=================================

  !DIR$ ATTRIBUTES INLINE :: qlh_3d_spline_vector
  subroutine qlh_3d_spline_vector( xp, interp_in, mempool )
   !! Quadratic interpolation at integer cells, linear at
   !! half-integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================
   length = SIZE( xp, DIM=1 )
   call array_realloc_1d( mempool%mp_xx_1d_A, length )
   call array_realloc_1d( mempool%mp_xx_1d_B, length )
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx
   shy = gcy
   shz = gcz

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )
   call first_order( sx(1:length), interp_in%h_coeff_x_rank2 )

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_y_rank2 )
   call first_order( sx(1:length), interp_in%h_coeff_y_rank2 )

   xx(1:length) = shz + xp(1:length, 3)
   interp_in%iz_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%iz_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_z_rank2 )
   call first_order( sx(1:length), interp_in%h_coeff_z_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%ihx_rank2(1:length) = interp_in%ix_rank2(1:length)
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1
   interp_in%ihy_rank2(1:length) = interp_in%iy_rank2(1:length)
   interp_in%iz_rank2(1:length) = interp_in%iz_rank2(1:length) - 1
   interp_in%ihz_rank2(1:length) = interp_in%iz_rank2(1:length)
  end subroutine
  !=================================

  !DIR$ ATTRIBUTES INLINE :: qqh_3d_spline_real
  subroutine qqh_3d_spline_real( xp, interp_in )
   !! Quadratic interpolation at both integer and half-integer
   !! cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, ihx, iy, ihy, iz, ihz
   real (dp) :: xx, sx
   !=====================
   shx = gcx
   shy = gcy
   shz = gcz

   xx = shx + xp(1)
   ix = int(xx+half)
   sx = xx - real(ix, dp)

   call second_order( sx, interp_in%coeff_x )

   ihx = int(xx)
   sx = xx - real(ihx, dp) - half

   call second_order( sx, interp_in%h_coeff_x )

   xx = shy + xp(2)
   iy = int(xx+half)
   sx = xx - real(iy, dp)

   call second_order( sx, interp_in%coeff_y )

   ihy = int(xx)
   sx = xx - real(ihy, dp) - half

   call second_order( sx, interp_in%h_coeff_y )

   xx = shz + xp(3)
   iz = int(xx+half)
   sx = xx - real(iz, dp)

   call second_order( sx, interp_in%coeff_z )

   ihz = int(xx)
   sx = xx - real(ihz, dp) - half

   call second_order( sx, interp_in%h_coeff_z )

   interp_in%ix = ix - 1
   interp_in%ihx = ihx - 1
   interp_in%iy = iy - 1
   interp_in%ihy = ihy - 1
   interp_in%iz = iz - 1
   interp_in%ihz = ihz - 1
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: qqh_3d_spline_vector
  subroutine qqh_3d_spline_vector( xp, interp_in, mempool )
  !! Quadratic interpolation at integer cells, linear at
  !! half-integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================

   length = SIZE( xp, DIM=1 )
   call array_realloc_1d( mempool%mp_xx_1d_A, length )
   call array_realloc_1d( mempool%mp_xx_1d_B, length )
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx
   shy = gcy
   shz = gcz

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )

   interp_in%ihx_rank2(1:length) = int(xx(1:length))
   sx(1:length) = xx(1:length) - &
    real(interp_in%ihx_rank2(1:length), dp) - half

   call second_order( sx(1:length), interp_in%h_coeff_x_rank2 ) 

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_y_rank2 )

   interp_in%ihy_rank2(1:length) = int(xx(1:length))
   sx(1:length) = xx(1:length) - &
    real(interp_in%ihy_rank2(1:length), dp) - half

   call second_order( sx(1:length), interp_in%h_coeff_y_rank2 )

   xx(1:length) = shz + xp(1:length, 3)
   interp_in%iz_rank2(1:length) = int(xx(1:length) + half)
   sx(1:length) = xx(1:length) - &
    real(interp_in%iz_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_z_rank2 )

   interp_in%ihz_rank2(1:length) = int(xx(1:length))
   sx(1:length) = xx(1:length) - &
    real(interp_in%ihz_rank2(1:length), dp) - half

    call second_order( sx(1:length), interp_in%h_coeff_z_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%ihx_rank2(1:length) = interp_in%ihx_rank2(1:length) - 1
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1
   interp_in%ihy_rank2(1:length) = interp_in%ihy_rank2(1:length) - 1
   interp_in%iz_rank2(1:length) = interp_in%iz_rank2(1:length) - 1
   interp_in%ihz_rank2(1:length) = interp_in%ihz_rank2(1:length) - 1
   
  end subroutine
  !====================

  !DIR$ ATTRIBUTES INLINE :: qden_3d_wgh_real
  subroutine qden_3d_wgh_real( xp, interp_in )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, iy, iz
   real (dp) :: xx, sx
   !======================
   shx = gcx
   shy = gcy
   shz = gcz

   xx = shx + xp(1)
   ix = int(xx+half)
   sx = xx - real(ix, dp)

   call second_order( sx, interp_in%coeff_x )

   xx = shy + xp(2)
   iy = int(xx+half)
   sx = xx - real(iy, dp)

   call second_order( sx, interp_in%coeff_y )

   xx = shz + xp(3)
   iz = int(xx+half)
   sx = xx - real(iz, dp)

   call second_order( sx, interp_in%coeff_z )

   interp_in%ix = ix - 1
   interp_in%iy = iy - 1
   interp_in%iz = iz - 1
  end subroutine
  !================================

  !DIR$ ATTRIBUTES INLINE :: qden_3d_wgh_vector
  subroutine qden_3d_wgh_vector( xp, interp_in, mempool )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================
   length = SIZE( xp, DIM=1 )
   call array_realloc_1d( mempool%mp_xx_1d_A, length )
   call array_realloc_1d( mempool%mp_xx_1d_B, length )
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx
   shy = gcy
   shz = gcz

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int( xx(1:length) + half )
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_x_rank2 )

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int( xx(1:length) + half )
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call second_order( sx(1:length), interp_in%coeff_y_rank2 )

   xx(1:length) = shz + xp(1:length, 3)
   interp_in%iz_rank2(1:length) = int( xx(1:length) + half )
   sx(1:length) = xx(1:length) - &
    real(interp_in%iz_rank2(1:length), dp)

    call second_order( sx(1:length), interp_in%coeff_z_rank2 )

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1
   interp_in%iz_rank2(1:length) = interp_in%iz_rank2(1:length) - 1

  end subroutine

  !DIR$ ATTRIBUTES INLINE :: cden_3d_wgh_real
  subroutine cden_3d_wgh_real( xp, interp_in )
   !! Cubic interpolation at integer cells
   real (dp), intent (in) :: xp(:)
   type(interp_coeff), intent(inout) :: interp_in
   integer :: ix, iy, iz
   real (dp) :: xx, sx
   !======================
   !cubic spline to integer index
   shx = gcx
   shy = gcy
   shz = gcz

   xx = shx + xp(1)
   ix = int(xx)
   sx = xx - real(ix, dp)

   call third_order( sx, interp_in%coeff_x )

   xx = shy + xp(2)
   iy = int(xx)
   sx = xx - real(iy, dp)

   call third_order( sx, interp_in%coeff_y )

   xx = shz + xp(3)
   iz = int(xx)
   sx = xx - real(iz, dp)

   call third_order( sx, interp_in%coeff_z )

   interp_in%ix = ix - 1
   interp_in%iy = iy - 1
   interp_in%iz = iz - 1

  end subroutine

  !DIR$ ATTRIBUTES INLINE :: cden_3d_wgh_vector
  subroutine cden_3d_wgh_vector( xp, interp_in, mempool )
   !! Quadratic interpolation at integer cells
   real (dp), intent (in), dimension(:, :) :: xp
   type(interp_coeff), intent(inout) :: interp_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   real(dp), pointer, contiguous, dimension(:) :: xx => null(), sx => null()
   integer :: length
   !======================
   length = SIZE( xp, DIM=1 )
   call array_realloc_1d( mempool%mp_xx_1d_A, length )
   call array_realloc_1d( mempool%mp_xx_1d_B, length )
   xx => mempool%mp_xx_1d_A
   sx => mempool%mp_xx_1d_B
   shx = gcx
   shy = gcy
   shz = gcz

   xx(1:length) = shx + xp(1:length, 1)
   interp_in%ix_rank2(1:length) = int( xx(1:length) )
   sx(1:length) = xx(1:length) - &
    real(interp_in%ix_rank2(1:length), dp)

   call third_order( sx(1:length), interp_in%coeff_x_rank2)

   xx(1:length) = shy + xp(1:length, 2)
   interp_in%iy_rank2(1:length) = int( xx(1:length) )
   sx(1:length) = xx(1:length) - &
    real(interp_in%iy_rank2(1:length), dp)

   call third_order( sx(1:length), interp_in%coeff_y_rank2)

   xx(1:length) = shz + xp(1:length, 3)
   interp_in%iz_rank2(1:length) = int( xx(1:length) )
   sx(1:length) = xx(1:length) - &
    real(interp_in%iz_rank2(1:length), dp)

   call third_order( sx(1:length), interp_in%coeff_y_rank2)

   interp_in%ix_rank2(1:length) = interp_in%ix_rank2(1:length) - 1
   interp_in%iy_rank2(1:length) = interp_in%iy_rank2(1:length) - 1
   interp_in%iz_rank2(1:length) = interp_in%iz_rank2(1:length) - 1

  end subroutine
  !====================================

  !DIR$ ATTRIBUTES INLINE :: set_local_positions_sn
  function set_local_positions_sn( pt_array, component, mask_in, tracking ) result(position)
   type (species_new), intent(in) :: pt_array
   integer, intent(in) :: component
   logical, dimension(:), intent(in), optional :: mask_in
   logical, intent(in), optional :: tracking
   integer :: np, npt
   real(dp), allocatable, dimension(:) :: position
   logical :: track_flag

   track_flag = .false.

   if ( PRESENT(tracking) ) then
    track_flag = tracking
   end if

   if (track_flag) then
     np = pt_array%how_many()
     npt = pt_array%pick_tot_tracked_parts()
     allocate( position(npt) )
     position(1:npt) = pt_array%call_tracked_component( component, lb=1, ub=np )
     np = npt
   else
     np = pt_array%how_many()
     allocate( position(np) )
     position(1:np) = pt_array%call_component( component, lb=1, ub=np )
   end if

   if ( PRESENT( mask_in ) ) then
    position = PACK( position, mask_in )
    np = COUNT( mask_in )
   end if
   
   select case(component)
   case(X_COMP)
    call map2dx_part_sind( np, position )
   case(Y_COMP)
    call map2dy_part_sind( np, position )
   case(Z_COMP)
    call map2dz_part_sind( np, position )
   end select

  end function

  !DIR$ ATTRIBUTES INLINE :: set_local_positions_sa
  function set_local_positions_sa( pt_array, component, mask_in, tracking ) result(position)
   type (species_aux), intent(in) :: pt_array
   integer, intent(in) :: component
   logical, dimension(:), intent(in), optional :: mask_in
   logical, intent(in), optional :: tracking
   integer :: np
   real(dp), allocatable, dimension(:) :: position
   logical :: track_flag

   track_flag = .false.

   if ( PRESENT(tracking) ) then
    track_flag = tracking
   end if
   if (track_flag) then
     np = pt_array%pick_tot_tracked_parts()
     allocate( position(np) )
     position(1:np) = pt_array%call_tracked_component( component, lb=1, ub=np )
   else
     np = pt_array%how_many()
     allocate( position(np) )
     position(1:np) = pt_array%call_component( component, lb=1, ub=np )
   end if

   if ( PRESENT( mask_in ) ) then
    position = PACK( position, mask_in )
    np = COUNT( mask_in )
   end if

   select case(component)
   case(X_COMP)
    call map2dx_part_sind( np, position )
   case(Y_COMP)
    call map2dy_part_sind( np, position )
   case(Z_COMP)
    call map2dz_part_sind( np, position )
   end select

  end function

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
