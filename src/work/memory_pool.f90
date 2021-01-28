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

 module memory_pool
  !! Module that serves as container for all auxiliary arrays to avoid
  !! unnecessary allocations and multiple copies.

  use precision_def
  use base_species
  use particles_def
  use particles_aux_def
  use interpolation_lib

  implicit none
  public
  
  interface array_realloc_1d
   module procedure array_realloc_1d_dp
   module procedure array_realloc_1d_sp
   module procedure array_realloc_1d_logical
  end interface

  type memory_pool_t

  integer :: reset_count = 1
  !! Is reset to 1 when an array is allocated.
  !! Keeps track of how many times the arrays are not reallocated
  integer :: max_np = 0
  !! Keeps track on the max number of particles,
  !! which usually corresponds to the largest size of the allocated arrays.
  integer :: iter_since_last_cleaning = 0
  !! Iteration since the memory pool was last cleaned
  integer(dp_int) :: used_memory = 0
  real(dp), allocatable, dimension(:) :: mp_xx_1d_A
  real(dp), allocatable, dimension(:) :: mp_xx_1d_B
  real(dp), allocatable, dimension(:) :: mp_xx_1d_C
  real(dp), allocatable, dimension(:) :: mp_xx_1d_D
  real(dp), allocatable, dimension(:, :) :: mp_xx_2d_A
  real(dp), allocatable, dimension(:, :) :: mp_xx_2d_B
  real(dp), allocatable, dimension(:, :, :) :: mp_xx_3d
  logical, allocatable, dimension(:) :: mp_log_1d
  type(species_new), allocatable :: mp_species_new
  type(interp_coeff), allocatable :: interp
  type(interp_coeff), allocatable :: interp_old

  contains

  procedure :: clean
  procedure :: memory_usage
  final :: finalize_pool

  end type

  integer, parameter :: EVERY_CLEAN = 200
  integer, parameter :: RESET_CLEAN = 100
  !! Number of iterations after which the cleaning occurs

  contains

  subroutine clean( this, np )
   class(memory_pool_t), intent(inout) :: this
   integer, intent(in) :: np

   this%iter_since_last_cleaning = min(this%iter_since_last_cleaning + 1, EVERY_CLEAN)

   if ( np > this%max_np ) then
    this%max_np = np
    this%reset_count = 1
   else
    this%reset_count = min(this%reset_count + 1, RESET_CLEAN)
   end if

   !write(6, *) 'ITER_SINCE_LAST_CLEANING', this%iter_since_last_cleaning
   !write(6, *) 'RESET_COUNT', this%reset_count
   if( mod(this%reset_count, RESET_CLEAN) == 0 .and. &
       mod(this%iter_since_last_cleaning, EVERY_CLEAN) == 0 ) then
    !write(6, *) 'Called cleaning memory pool'

    call finalize_pool( this )

    this%reset_count = 1
    this%used_memory = 0
    this%iter_since_last_cleaning = 0
    this%max_np = 0
   end if

  end subroutine

  subroutine create_memory_pool( this )
   type(memory_pool_t), pointer, intent(inout) :: this

   if ( .not. ASSOCIATED(this) ) then
    !write(6, *) 'Constructing the memory pool'
    ALLOCATE(this)
   end if

  end subroutine

  subroutine finalize_pool( this )
   type(memory_pool_t), intent(inout) :: this

   !write(6, *) 'Called memory pool destructor'
   if ( ALLOCATED(this%mp_log_1d) ) then
    deallocate( this%mp_log_1d )
   end if

   if ( ALLOCATED(this%mp_xx_1d_A) ) then
    deallocate( this%mp_xx_1d_A )
   end if
   if ( ALLOCATED(this%mp_xx_1d_B) ) then
    deallocate( this%mp_xx_1d_B )
   end if
   if ( ALLOCATED(this%mp_xx_1d_C) ) then
    deallocate( this%mp_xx_1d_C )
   end if
   if ( ALLOCATED(this%mp_xx_1d_D) ) then
    deallocate( this%mp_xx_1d_D )
   end if

   if ( ALLOCATED(this%mp_xx_2d_A) ) then
    deallocate( this%mp_xx_2d_A )
   end if
   if ( ALLOCATED(this%mp_xx_2d_B) ) then
    deallocate( this%mp_xx_2d_B )
   end if

   if ( ALLOCATED(this%mp_xx_3d) ) then
    deallocate( this%mp_xx_3d )
   end if

   if ( ALLOCATED(this%mp_species_new) ) then
    deallocate( this%mp_species_new )
   end if

   if ( ALLOCATED(this%interp) ) then
    deallocate( this%interp )
   end if
   if ( ALLOCATED(this%interp_old) ) then
    deallocate( this%interp_old )
   end if

  end subroutine

  subroutine destroy_memory_pool( this )
   type(memory_pool_t), pointer, intent(inout) :: this

   if ( ASSOCIATED(this) ) then
    deallocate(this)
   end if

  end subroutine

  subroutine memory_usage( this )
   class(memory_pool_t), intent(inout) :: this
   integer(dp_int) :: um

   um = 0
   if ( ALLOCATED(this%mp_log_1d) ) then
    um = um + size(this%mp_log_1d)*kind(this%mp_log_1d)
   end if

   if ( ALLOCATED(this%mp_xx_1d_A) ) then
    um = um + size(this%mp_xx_1d_A)*kind(this%mp_xx_1d_A)
   end if
   if ( ALLOCATED(this%mp_xx_1d_B) ) then
    um = um + size(this%mp_xx_1d_B)*kind(this%mp_xx_1d_B)
   end if
   if ( ALLOCATED(this%mp_xx_1d_C) ) then
    um = um + size(this%mp_xx_1d_C)*kind(this%mp_xx_1d_C)
   end if
   if ( ALLOCATED(this%mp_xx_1d_D) ) then
    um = um + size(this%mp_xx_1d_D)*kind(this%mp_xx_1d_D)
   end if

   if ( ALLOCATED(this%mp_xx_2d_A) ) then
    um = um + size(this%mp_xx_2d_A)*kind(this%mp_xx_2d_A)
   end if
   if ( ALLOCATED(this%mp_xx_2d_B) ) then
    um = um + size(this%mp_xx_2d_B)*kind(this%mp_xx_2d_B)
   end if

   if ( ALLOCATED(this%mp_xx_3d) ) then
    um = um + size(this%mp_xx_3d)*kind(this%mp_xx_3d)
   end if

   if ( ALLOCATED(this%mp_species_new) ) then
    um = um + this%mp_species_new%array_size()*this%mp_species_new%total_size()*sizeof(one_dp)
   end if

   this%used_memory = um

  end subroutine
  !==============================================
  ! Subroutines for allocation
  !==============================================
  !DIR$ ATTRIBUTES INLINE :: array_realloc_1d_dp
  subroutine array_realloc_1d_dp(vdata, npt_new)
   real (dp), allocatable, intent (inout) :: vdata(:)
   integer, intent (in) :: npt_new
   integer :: allocstatus, deallocstatus

   if (allocated(vdata)) then
    if (size(vdata,1)<npt_new) then
     deallocate (vdata, stat=deallocstatus)
     allocate (vdata(1:npt_new), stat=allocstatus)
    end if
   else
    allocate (vdata(1:npt_new), stat=allocstatus)
   end if
  end subroutine
  !===========================
  !DIR$ ATTRIBUTES INLINE :: array_realloc_1d_sp
  subroutine array_realloc_1d_sp(vdata, npt_new)
   real (sp), allocatable, intent (inout) :: vdata(:)
   integer, intent (in) :: npt_new
   integer :: allocstatus, deallocstatus

   if (allocated(vdata)) then
    if (size(vdata,1)<npt_new) then
     deallocate (vdata, stat=deallocstatus)
     allocate (vdata(1:npt_new), stat=allocstatus)
    end if
   else
    allocate (vdata(1:npt_new), stat=allocstatus)
   end if
  end subroutine
  !===========================
  !DIR$ ATTRIBUTES INLINE :: array_realloc_1d_logical
  subroutine array_realloc_1d_logical(vdata, npt_new)
   logical, allocatable, intent (inout) :: vdata(:)
   integer, intent (in) :: npt_new
   integer :: allocstatus, deallocstatus

   if (allocated(vdata)) then
    if (size(vdata,1)<npt_new) then
     deallocate (vdata, stat=deallocstatus)
     allocate (vdata(1:npt_new), stat=allocstatus)
    end if
   else
    allocate (vdata(1:npt_new), stat=allocstatus)
   end if
  end subroutine
  !==========================================
  !DIR$ ATTRIBUTES INLINE :: mp_xx_realloc
  subroutine mp_xx_realloc( xx_in, npart_new, dimensions, mempool )
   real(dp), allocatable, dimension(:, :), intent(inout) :: xx_in
   integer, intent(in) :: npart_new, dimensions
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: allocstatus, deallocstatus

   if ( ALLOCATED(xx_in) ) then   
    if (SIZE(xx_in, DIM=1) < npart_new .or. &
     SIZE(xx_in, DIM=2) < dimensions ) then
     deallocate(xx_in, stat=deallocstatus)
     allocate(xx_in(npart_new, dimensions), stat=allocstatus)
    end if
   else
    allocate(xx_in(npart_new, dimensions), stat=allocstatus)
   end if
  end subroutine

  !==========================================
  !DIR$ ATTRIBUTES INLINE :: interp_realloc
  subroutine interp_realloc(interp_in, npart_new, dimensions)
   type(interp_coeff), allocatable, intent(inout) :: interp_in
   integer, intent(in) :: npart_new, dimensions

   if ( allocated(interp_in) ) then   
    if (interp_in%n_parts < npart_new ) then
     call sweep( interp_in )
     call interp_in%new_interp(npart_new, max_order, &
      max_h_order, dimensions)
    end if
   else
    allocate( interp_in )
    call interp_in%new_interp(npart_new, max_order, &
      max_h_order, dimensions)
   end if

  end subroutine
 end module