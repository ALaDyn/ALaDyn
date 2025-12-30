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

 module interpolation_lib

  use precision_def

  implicit none

  real (dp), parameter, private :: PP1 = 0.25, PP2 = 0.5, &
   PP3 = 0.75, PP4 = 2./3., PP5 = 1./6.
  integer, parameter :: max_order = 2
  integer, parameter :: max_h_order = 2
  type interp_coeff
  !! Type containing particle-grid relations during
  !! interpolation on grid
   integer :: order
   !! Interpolation order on integer cells
   integer :: h_order
   !! Interpolation order on half-integer cells
   real(dp), allocatable, dimension(:) :: coeff_x
   !! Interpolation coefficients along x (integer cells)
   real(dp), allocatable, dimension(:) :: coeff_y
   !! Interpolation coefficients along y (integer cells)
   real(dp), allocatable, dimension(:) :: coeff_z
   !! Interpolation coefficients along z (integer cells)
   real(dp), allocatable, dimension(:) :: h_coeff_x
   !! Interpolation coefficients along x (half-integer cells)
   real(dp), allocatable, dimension(:) :: h_coeff_y
   !! Interpolation coefficients along y (half-integer cells)
   real(dp), allocatable, dimension(:) :: h_coeff_z
   !! Interpolation coefficients along z (half-integer cells)
   real(dp), allocatable, dimension(:, :) :: coeff_x_rank2
   !! Interpolation coefficients along x for vectors (integer cells)
   real(dp), allocatable, dimension(:, :) :: coeff_y_rank2
   !! Interpolation coefficients along y for vectors (integer cells)
   real(dp), allocatable, dimension(:, :) :: coeff_z_rank2
   !! Interpolation coefficients along z for vectors (integer cells)
   real(dp), allocatable, dimension(:, :) :: h_coeff_x_rank2
   !! Interpolation coefficients along x for vectors (half-integer cells)
   real(dp), allocatable, dimension(:, :) :: h_coeff_y_rank2
   !! Interpolation coefficients along y for vectors (half-integer cells)
   real(dp), allocatable, dimension(:, :) :: h_coeff_z_rank2
   !! Interpolation coefficients along z for vectors (half-integer cells)
   integer :: ix
   !! Cell index along x
   integer :: ihx
   !! Half-cell index along x
   integer :: iy
   !! Cell index along y
   integer :: ihy
   !! Half-cell index along y
   integer :: iz
   !! Cell index along z
   integer :: ihz
   !! Half-cell index along z
   integer, allocatable, dimension(:) :: ix_rank2
   !! Cell index along x for vectors
   integer, allocatable, dimension(:) :: ihx_rank2
   !! Half-cell index along x for vectors
   integer, allocatable, dimension(:) :: iy_rank2
   !! Cell index along y for vectors
   integer, allocatable, dimension(:) :: ihy_rank2
   !! Half-cell index along y for vectors
   integer, allocatable, dimension(:) :: iz_rank2
   !! Cell index along z for vectors
   integer, allocatable, dimension(:) :: ihz_rank2
   !! Half-cell index along z for vectors
   integer :: n_parts
   contains
   procedure, pass, public :: new_interp
   final :: sweep
  end type

  interface zeroth_order
   module procedure zeroth_order_real
   module procedure zeroth_order_vector
  end interface

  interface first_order
   module procedure first_order_real
   module procedure first_order_vector
  end interface

  interface second_order
   module procedure second_order_real
   module procedure second_order_vector
  end interface

  interface third_order
   module procedure third_order_real
   module procedure third_order_vector
  end interface

  contains

  subroutine new_interp(this, n_parts, order, h_order, dimensions)
   class(interp_coeff), intent(inout) :: this
   integer, intent(in) :: n_parts
   integer, intent(in) :: order, h_order, dimensions

   this%n_parts = n_parts
   this%order = order
   this%h_order = h_order

   select case(dimensions)
   case(1)
    allocate( this%coeff_x(0:order) )
    allocate( this%coeff_x_rank2(0:order, n_parts) )
    allocate( this%h_coeff_x(0:h_order) )
    allocate( this%h_coeff_x_rank2(0:h_order, n_parts) )
    allocate( this%ix_rank2(n_parts) )
    allocate( this%ihx_rank2(n_parts) )
   case(2)
    allocate( this%coeff_x(0:order) )
    allocate( this%coeff_x_rank2(0:order, n_parts) )
    allocate( this%h_coeff_x(0:h_order) )
    allocate( this%h_coeff_x_rank2(0:h_order, n_parts) )
    allocate( this%ix_rank2(n_parts) )
    allocate( this%ihx_rank2(n_parts) )

    allocate( this%coeff_y(0:order) )
    allocate( this%coeff_y_rank2(0:order, n_parts) )
    allocate( this%h_coeff_y(0:h_order) )
    allocate( this%h_coeff_y_rank2(0:h_order, n_parts) )
    allocate( this%iy_rank2(n_parts) )
    allocate( this%ihy_rank2(n_parts) )
   case(3)
    allocate( this%coeff_x(0:order) )
    allocate( this%coeff_x_rank2(0:order, n_parts) )
    allocate( this%h_coeff_x(0:h_order) )
    allocate( this%h_coeff_x_rank2(0:h_order, n_parts) )
    allocate( this%ix_rank2(n_parts) )
    allocate( this%ihx_rank2(n_parts) )

    allocate( this%coeff_y(0:order) )
    allocate( this%coeff_y_rank2(0:order, n_parts) )
    allocate( this%h_coeff_y(0:h_order) )
    allocate( this%h_coeff_y_rank2(0:h_order, n_parts) )
    allocate( this%iy_rank2(n_parts) )
    allocate( this%ihy_rank2(n_parts) )

    allocate( this%coeff_z(0:order) )
    allocate( this%coeff_z_rank2(0:order, n_parts) )
    allocate( this%h_coeff_z(0:h_order) )
    allocate( this%h_coeff_z_rank2(0:h_order, n_parts) )
    allocate( this%iz_rank2(n_parts) )
    allocate( this%ihz_rank2(n_parts) )
   end select
  end subroutine

  subroutine sweep( this )
   type(interp_coeff), intent(inout) :: this

   ! write(6, *) 'Called interp destroyer'
   if ( allocated(this%coeff_x) ) then
    deallocate(this%coeff_x)
   end if
   if ( allocated(this%coeff_x_rank2) ) then
    deallocate(this%coeff_x_rank2)
   end if
   if ( allocated(this%h_coeff_x) ) then
    deallocate(this%h_coeff_x)
   end if
   if ( allocated(this%h_coeff_x_rank2) ) then
    deallocate(this%h_coeff_x_rank2)
   end if
   if ( allocated(this%ix_rank2) ) then
    deallocate(this%ix_rank2)
   end if
   if ( allocated(this%ihx_rank2) ) then
    deallocate(this%ihx_rank2)
   end if
   
   if ( allocated(this%coeff_y) ) then
    deallocate(this%coeff_y)
   end if
   if ( allocated(this%coeff_y_rank2) ) then
    deallocate(this%coeff_y_rank2)
   end if
   if ( allocated(this%h_coeff_y) ) then
    deallocate(this%h_coeff_y)
   end if
   if ( allocated(this%h_coeff_y_rank2) ) then
    deallocate(this%h_coeff_y_rank2)
   end if
   if ( allocated(this%iy_rank2) ) then
    deallocate(this%iy_rank2)
   end if
   if ( allocated(this%ihy_rank2) ) then
    deallocate(this%ihy_rank2)
   end if
   
   if ( allocated(this%coeff_z) ) then
    deallocate(this%coeff_z)
   end if
   if ( allocated(this%coeff_z_rank2) ) then
    deallocate(this%coeff_z_rank2)
   end if
   if ( allocated(this%h_coeff_z) ) then
    deallocate(this%h_coeff_z)
   end if
   if ( allocated(this%h_coeff_z_rank2) ) then
    deallocate(this%h_coeff_z_rank2)
   end if
   if ( allocated(this%iz_rank2) ) then
    deallocate(this%iz_rank2)
   end if
   if ( allocated(this%ihz_rank2) ) then
    deallocate(this%ihz_rank2)
   end if
   
  end subroutine
  ! =======================================================
  !    Templates of spl=1,2,3 order shape functions for grid-particle  connection
  !==================================================
  ! Computes 
  subroutine set_int_pshape(spl, xx, ax, ind)
   integer, intent (in) :: spl
   real (dp), intent (in) :: xx
   real (dp), intent (out) :: ax(0:3)
   integer, intent (out) :: ind
   real (dp) :: sx, sx2, sx3
   !To integer grid points
   ax(0:3) = 0.0
   select case (spl)
   case (1)
    ind = int(xx)
    ax(1) = xx - real(ind, dp)
    ax(0) = one_dp - ax(1)
   case (2)
    ind = int(xx+0.5)
    sx = xx - real(ind, dp)
    sx2 = sx*sx
    ax(1) = PP3 - sx2
    ax(2) = PP2*(PP1+sx2+sx)
    ax(0) = one_dp - ax(1) - ax(2)
   case (3)
    ind = int(xx)
    sx = xx - real(ind, dp)
    sx2 = sx*sx
    sx3 = sx2*sx
    ax(1) = PP4 - sx2 + PP2*sx3
    ax(2) = PP5 + PP2*(sx+sx2-sx3)
    ax(3) = PP5*sx3
    ax(0) = one_dp - ax(1) - ax(2) - ax(3)
   end select
  end subroutine
  !========================
  subroutine set_hint_pshape(spl, xx, ax, ind)
   integer, intent (in) :: spl
   real (dp), intent (in) :: xx
   integer, intent (out) :: ind
   real (dp), intent (out) :: ax(0:3)
   real (dp) :: sx, sx2, sx3
   !To half-integer grid points
   ax(0:3) = 0.0
   select case (spl)
   case (1)
    sx = xx + 0.5
    ind = int(sx)
    ax(1) = sx - real(ind, dp)
    ax(0) = one_dp - ax(1)
   case (2)
    ind = int(xx)
    sx = xx - PP2 - real(ind, dp)
    sx2 = sx*sx
    ax(1) = PP3 - sx2
    ax(2) = PP2*(PP1+sx2+sx)
    ax(0) = one_dp - ax(1) - ax(2)
   case (3)
    ind = int(xx+0.5)
    sx = xx - real(ind, dp)
    sx2 = sx*sx
    sx3 = sx2*sx
    ax(1) = PP4 - sx2 + PP2*sx3
    ax(2) = PP5 + PP2*(sx+sx2-sx3)
    ax(3) = PP5*sx3
    ax(0) = one_dp - ax(1) - ax(2) - ax(3) ! to (i-1/2,i+1/2,i+3/2,i+5/2) half-int
   end select
  end subroutine
  !=======================
  ! End templates
  ! =======================================================

  !DIR$ ATTRIBUTES INLINE :: zeroth_order_real
  subroutine zeroth_order_real(deltax, ax0)
   real(dp), intent(inout) :: deltax
   real(dp), intent(out) :: ax0
   ! Here dx should be computed as
   ! dx = x - int(x + 0.5)
   deltax = zero_dp
   ax0 = one_dp
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: zeroth_order_vector
  subroutine zeroth_order_vector(deltax, ax0)
   real(dp), intent(in), dimension(:) :: deltax
   real(dp), intent(inout), dimension(:, :) :: ax0
   integer :: length
   ! Here dx should be computed as
   ! dx = x - int(x + 0.5)
   length = SIZE( deltax )
   ax0(1, 1:length) = one_dp
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: first_order_real
  subroutine first_order_real(deltax, ax1)
   real(dp), intent(in) :: deltax
   real(dp), dimension(:), intent(inout) :: ax1
   ! Here dx should be computed as
   ! dx = x - int(x)
   ax1(2) = deltax
   ax1(1) = one_dp - ax1(2)
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: first_order_vector
  subroutine first_order_vector(deltax, ax1)
   real(dp), dimension(:), intent(in) :: deltax
   real(dp), dimension(:, :), intent(inout) :: ax1
   integer :: length, i
   ! Here dx should be computed as
   ! dx = x - int(x)
   length = SIZE( deltax )
   do i = 1, length
    ax1(2, i) = deltax(i)
    ax1(1, i) = one_dp - ax1(2, i)
   end do
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: second_order_real
  subroutine second_order_real(deltax, ax2)
   real(dp), intent(in) :: deltax
   real(dp), dimension(:), intent(inout) :: ax2
   real(dp) :: dx2_r
   ! Here dx should be computed as
   ! dx = x - int(x + 0.5)
   dx2_r = deltax*deltax
   ax2(2) = PP3 - dx2_r
   ax2(3) = PP2*(PP1 + deltax + dx2_r)
   ax2(1) = one_dp - ax2(2) - ax2(3)
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: second_order_vector
  subroutine second_order_vector(deltax, ax2)
   real(dp), dimension(:), intent(in) :: deltax
   real(dp), dimension(:, :), intent(inout) :: ax2
   real(dp) :: dx2
   integer :: length, i
   ! Here dx should be computed as
   ! dx = x - int(x)
   length = SIZE( deltax )
   do i = 1, length
    dx2 = deltax(i)*deltax(i)
    ax2(2, i) = PP3 - dx2
    ax2(3, i) = PP2*(PP1 + deltax(i) + dx2)
    ax2(1, i) = one_dp - ax2(2, i) - ax2(3, i)
   end do
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: third_order_real
  subroutine third_order_real(deltax, ax3)
   real(dp), intent(in) :: deltax
   real(dp), dimension(:), intent(inout) :: ax3
   real(dp) :: dx2_r, dx3_r
   ! Here dx should be computed as
   ! dx = x - int(x)
   dx2_r = deltax*deltax
   dx3_r = dx2_r*deltax

   ax3(2) = PP4 - dx2_r + PP2*dx3_r
   ax3(3) = PP5 + PP2*(deltax + dx2_r - dx3_r)
   ax3(4) = PP5*dx3_r
   ax3(1) = one_dp - ax3(2) - ax3(3) - ax3(4)
  end subroutine

  !DIR$ ATTRIBUTES INLINE :: third_order_vector
  subroutine third_order_vector(deltax, ax3)
   real(dp), dimension(:), intent(in) :: deltax
   real(dp), dimension(:, :), intent(inout) :: ax3
   real(dp) :: dx2, dx3
   integer :: length, i
   ! Here dx should be computed as
   ! dx = x - int(x)
   length = SIZE( deltax )
   do i = 1, length
    dx2 = deltax(i)*deltax(i)
    dx3 = dx2*deltax(i)

    ax3(2, i) = PP4 - dx2 + PP2*dx3
    ax3(3, i) = PP5 + PP2*(deltax(i) + dx2 - dx3)
    ax3(4, i) = PP5*dx3
    ax3(1, i) = one_dp - ax3(2, i) - ax3(3, i) - ax3(4, i)
   end do
  end subroutine

 end module
