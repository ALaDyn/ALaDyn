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

 module grid_tracking_part_connect

  use fstruct_data
  use grid_part_lib
  use memory_pool
  use mpi_var
  use particles_def
  use pstruct_data

  implicit none
  type(interp_coeff), private, allocatable, save :: interp
  integer, parameter :: Y_POLARIZATION = 1
  integer, parameter :: Z_POLARIZATION = 2

  contains

  subroutine a_on_tracking_particles( field, spec_in, spec_aux_in, np, order, polarization, mempool, mask_in )
   !! Subroutine that interpolates vector potential A on the tracking particles.

   real (dp), intent (in) :: field(:, :, :)
   type (species_new), intent (inout) :: spec_in
   type (species_aux), intent (in) :: spec_aux_in
   integer, intent(in) :: np, order, polarization
   type(memory_pool_t), pointer, intent(in) :: mempool
   logical, dimension(:), intent(in), target, contiguous, optional :: mask_in
   real(dp) :: dvol
   integer :: npt, i1, i2, j1, j2, k1, k2, n, cc
   real(dp), pointer, contiguous, dimension(:, :) :: xx => null()
   logical, pointer, contiguous, dimension(:) :: track_mask => null()
   real(dp), pointer, contiguous, dimension(:) :: interpolated_field => null()
   real(dp), pointer, contiguous, dimension(:) :: ap => null()

   npt = spec_in%pick_tot_tracked_parts()

   if (npt == 0) return
   call interp_realloc(interp, np, spec_in%pick_dimensions())

   if ( PRESENT( mask_in ) ) then
    track_mask => mask_in(1:np)
   else
    associate( inds => spec_in%call_component(INDEX_COMP, lb=1, ub=np) )
     call array_realloc_1d( mempool%mp_log_1d, np )
     track_mask => mempool%mp_log_1d
     track_mask(1:np) = (int(inds) > 0)

    end associate
   end if

   cc = COUNT(track_mask(1:np) )
   if (npt /= cc ) then
    call write_warning(text='Error in counting tracked particles', task=mype)
   end if
   
   select case (ndim)
   case(2)

    k2 = 1
    call mp_xx_realloc(mempool%mp_xx_2d_A, npt, 2, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:npt, 1) = set_local_positions( spec_aux_in, X_COMP, mask_in=track_mask(1:np) )
    xx(1:npt, 2) = set_local_positions( spec_aux_in, Y_COMP, mask_in=track_mask(1:np) )

    select case (order)

    case(0)
     ! Envelope case
     call qden_2d_wgh( xx(1:npt, 1:2), interp, mempool )

     call array_realloc_1d( mempool%mp_xx_1d_A, npt )
     ap => mempool%mp_xx_1d_A

     associate( ax1 => interp%coeff_x_rank2, &
                ay1 => interp%coeff_y_rank2, &
                i => interp%ix_rank2, &
                j => interp%iy_rank2 )

                do n = 1, npt
                 do j1 = 0, 2
                  j2 = j(n) + j1
                  dvol = ay1( j1, n )
                  do i1 = 0, 2
                   i2 = i(n) + i1
                   ap(n) = ap(n) + ax1(i1, n)*dvol*field(i2, j2, k2)
                  end do
                 end do
                end do

     end associate

    case(1:2)
     ! Full Pic case

     call qqh_2d_spline( xx(1:npt, 1:2), interp, mempool )

     call array_realloc_1d( mempool%mp_xx_1d_A, npt )
     ap => mempool%mp_xx_1d_A

     select case(polarization)
     !Integration order used for computing a from Ey
     case(Y_POLARIZATION)


      associate( ax1 => interp%coeff_x_rank2, &
                 ayh1 => interp%h_coeff_y_rank2, &
                 i => interp%ix_rank2, &
                 jh => interp%ihy_rank2 )

       do n = 1, npt
        do j1 = 0, 2
         j2 = jh(n) + j1
         dvol = ayh1( j1, n )
         do i1 = 0, 2
          i2 = i(n) + i1
          ap(n) = ap(n) + ax1(i1, n)*dvol*field(i2, j2, k2)
         end do
        end do
       end do

      end associate

     case(Z_POLARIZATION)

      associate( ax1 => interp%coeff_x_rank2, &
                 ay1 => interp%coeff_y_rank2, &
                 i => interp%ix_rank2, &
                 j => interp%iy_rank2 )

       do n = 1, npt
        do j1 = 0, 2
         j2 = j(n) + j1
         dvol = ay1( j1, n )
         do i1 = 0, 2
          i2 = i(n) + i1
          ap(n) = ap(n) + ax1(i1, n)*dvol*field(i2, j2, k2)
         end do
        end do
       end do

      end associate

     end select
    end select

   case(3)

    call mp_xx_realloc(mempool%mp_xx_2d_A, npt, 3, mempool)
    xx => mempool%mp_xx_2d_A

    xx(1:npt, 1) = set_local_positions( spec_aux_in, X_COMP, mask_in=track_mask(1:np) )
    xx(1:npt, 2) = set_local_positions( spec_aux_in, Y_COMP, mask_in=track_mask(1:np) )
    xx(1:npt, 3) = set_local_positions( spec_aux_in, Z_COMP, mask_in=track_mask(1:np) )

    select case(order)
    case(0)
    ! Envelope case
     call qden_3d_wgh( xx(1:npt, 1:3), interp, mempool )

     call array_realloc_1d( mempool%mp_xx_1d_A, npt )
     ap => mempool%mp_xx_1d_A

     associate( ax1 => interp%coeff_x_rank2, &
                ay1 => interp%coeff_y_rank2, &
                az1 => interp%coeff_z_rank2, &
                i => interp%ix_rank2, &
                j => interp%iy_rank2, &
                k => interp%iz_rank2 )

                do n = 1, npt
                 do k1 = 0, 2
                  k2 = k(n) + k1
                  dvol = az1( k1, n )
                  do j1 = 0, 2
                   j2 = j(n) + j1
                   dvol = dvol*ay1( j1, n )
                   do i1 = 0, 2
                    i2 = i(n) + i1
                    ap(n) = ap(n) + ax1(i1, n)*dvol*field(i2, j2, k2)
                   end do
                  end do
                 end do
                end do

     end associate

    case(1)
     !Full Pic
     call qqh_3d_spline( xx(1:npt, 1:3), interp, mempool )

     call array_realloc_1d( mempool%mp_xx_1d_A, npt )
     ap => mempool%mp_xx_1d_A

     select case(polarization)
     !Integration order used for computing a from Ey
     case(Y_POLARIZATION)


      associate( ax1 => interp%coeff_x_rank2, &
                 ayh1 => interp%h_coeff_y_rank2, &
                 az1 => interp%coeff_z_rank2, &
                 i => interp%ix_rank2, &
                 jh => interp%ihy_rank2, &
                 k => interp%iz_rank2 )

                 do n = 1, npt
                  do k1 = 0, 2
                   k2 = k(n) + k1
                   dvol = az1( k1, n )
                   do j1 = 0, 2
                    j2 = jh(n) + j1
                    dvol = dvol*ayh1( j1, n )
                    do i1 = 0, 2
                     i2 = i(n) + i1
                     ap(n) = ap(n) + ax1(i1, n)*dvol*field(i2, j2, k2)
                    end do
                   end do
                  end do
                 end do

      end associate

     case(Z_POLARIZATION)

      associate( ax1 => interp%h_coeff_x_rank2, &
                 ay1 => interp%coeff_y_rank2, &
                 azh1 => interp%h_coeff_z_rank2, &
                 i => interp%ihx_rank2, &
                 j => interp%iy_rank2, &
                 kh => interp%ihz_rank2 )

                 do n = 1, npt
                  do k1 = 0, 2
                   k2 = kh(n) + k1
                   dvol = azh1( k1, n )
                   do j1 = 0, 2
                    j2 = j(n) + j1
                    dvol = dvol*ay1( j1, n )
                    do i1 = 0, 2
                     i2 = i(n) + i1
                     ap(n) = ap(n) + ax1(i1, n)*dvol*field(i2, j2, k2)
                    end do
                   end do
                  end do
                 end do

      end associate
     end select
    end select
   end select
   
   call array_realloc_1d( mempool%mp_xx_1d_B, np )
   interpolated_field => mempool%mp_xx_1d_B

   ! Field assignment on tracked particles
   interpolated_field(1:np) = UNPACK( ap(1:npt), track_mask(1:np), interpolated_field(1:np) )
   call spec_in%set_component( interpolated_field(1:np), A_PARTICLE, lb=1, ub=np)

  end subroutine

 end module
