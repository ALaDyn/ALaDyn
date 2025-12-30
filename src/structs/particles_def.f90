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

module particles_def

 use base_species
 implicit none

 type species
  real (dp), allocatable :: part(:, :)
 end type
 
 type, extends(base_species_T) :: species_new
  contains
   procedure, public :: append => append_spec
   procedure, public :: call_component => call_component_spec
   procedure, public :: call_tracked_component => call_tracked_component_spec
   procedure, public :: extend => extend_spec
   procedure, public :: new_species => new_species_spec
   procedure, public :: set_component_real
   procedure, public :: set_component_integer
   procedure, pass :: sel_particles_bounds => sel_particles_bounds_spec
   procedure, pass :: sel_particles_index => sel_particles_index_spec
   procedure, pass :: sweep => sweep_spec
 end type

 type(species_new), save :: pd_temp_spec

 contains

 !========================================
 ! CONSTRUCTOR
 !========================================
 
 subroutine new_species_spec( this, n_particles, curr_ndims, tracked, mobile, extra_outputs )
  !! Constructor for the `species_new` type
  class(species_new), intent(inout) :: this
  integer, intent(in) :: n_particles, curr_ndims
  logical, intent(in), optional :: tracked
  logical, intent(in), optional :: mobile
  integer, intent(in), optional :: extra_outputs
  integer :: allocstatus
 
  this%allocated_x = .false.
  this%allocated_y = .false.
  this%allocated_z = .false.
  this%allocated_px = .false.
  this%allocated_py = .false.
  this%allocated_pz = .false.
  this%allocated_gamma = .false.
  this%allocated_weight = .false.
  this%allocated_index = .false.
  this%allocated_data_out = .false.

  call this%set_name( 'electron' )
  if (n_particles < 0) then
   return
  end if
  if ( this%initialized) then
    call write_warning('WARNING: Trying to allocate an already initialized spec_new object')
  end if
  this%initialized = .true.
  call this%set_part_number(n_particles)
  call this%set_dimensions(curr_ndims)
  if ( present(tracked) ) then
   call this%track( tracked , allocate_now=.false.)
  else
   call this%track( .false. )
  end if

  if ( present(mobile) ) then
   call this%set_mobile(mobile)
  else
   call this%set_mobile( .false. )
  end if

  if ( PRESENT(extra_outputs) ) then
   call this%set_extra_outputs( extra_outputs, n_particles )
  else
   call this%set_extra_outputs( 0, n_particles )
  end if

  if (n_particles == 0) then
   this%empty = .true.
   return
  end if
  this%empty = .false.
 
  select case(curr_ndims)
  
  case(1)
  
   allocate( this%x(n_particles), stat=allocstatus)
   this%allocated_x = .true.
   allocate( this%px(n_particles), stat=allocstatus)
   this%allocated_px = .true.
   allocate( this%gamma_inv(n_particles), stat=allocstatus)
   this%allocated_gamma = .true.
   allocate( this%weight(n_particles), stat=allocstatus)
   this%allocated_weight = .true.
   if (this%istracked()) then
    allocate( this%part_index(n_particles), stat=allocstatus, source=0)
    this%allocated_index = .true.
   end if
  case(2)
  
   allocate( this%x(n_particles), stat=allocstatus)
   this%allocated_x = .true.
   allocate( this%px(n_particles), stat=allocstatus)
   this%allocated_px = .true.
   allocate( this%y(n_particles), stat=allocstatus)
   this%allocated_y = .true.
   allocate( this%py(n_particles), stat=allocstatus)
   this%allocated_py = .true.
   allocate( this%gamma_inv(n_particles), stat=allocstatus)
   this%allocated_gamma = .true.
   allocate( this%weight(n_particles), stat=allocstatus)
   this%allocated_weight = .true.
   if (this%istracked()) then
    allocate( this%part_index(n_particles), stat=allocstatus, source=0)
    this%allocated_index = .true.
   end if
  
  case(3)
  
   allocate( this%x(n_particles), stat=allocstatus)
   this%allocated_x = .true.
   allocate( this%px(n_particles), stat=allocstatus)
   this%allocated_px = .true.
   allocate( this%y(n_particles), stat=allocstatus)
   this%allocated_y = .true.
   allocate( this%py(n_particles), stat=allocstatus)
   this%allocated_py = .true.
   allocate( this%z(n_particles), stat=allocstatus)
   this%allocated_z = .true.
   allocate( this%pz(n_particles), stat=allocstatus)
   this%allocated_pz = .true.
   allocate( this%gamma_inv(n_particles), stat=allocstatus)
   this%allocated_gamma = .true.
   allocate( this%weight(n_particles), stat=allocstatus)
   this%allocated_weight = .true.
   if (this%istracked()) then
    allocate( this%part_index(n_particles), stat=allocstatus, source=0)
    this%allocated_index = .true.
   end if
  end select

 end subroutine

 !========================================
 ! CLEANING PROCEDURES
 !========================================
 
 subroutine sweep_spec( this )
  !! Method that resets all the species elements to default and deallocates
  !! the arrays
  class(species_new), intent(inout) :: this

  if ( .not. this%initialized ) then
   return
  else
   this%initialized = .false.
  end if

  call this%properties%sweep()

  if ( this%allocated_x ) then
   this%allocated_x = .false.
   deallocate( this%x )
  end if
  if ( this%allocated_y ) then
   this%allocated_y = .false.
   deallocate( this%y )
  end if
  if ( this%allocated_z ) then
   this%allocated_z = .false.
   deallocate( this%z )
  end if
  if ( this%allocated_px ) then
   this%allocated_px = .false.
   deallocate( this%px )
  end if
  if ( this%allocated_py ) then
   this%allocated_py = .false.
   deallocate( this%py )
  end if
  if ( this%allocated_pz ) then
   this%allocated_pz = .false.
   deallocate( this%pz )
  end if
  if ( this%allocated_weight ) then
   this%allocated_weight = .false.
   deallocate( this%weight )
  end if
  if ( this%allocated_gamma ) then
   this%allocated_gamma = .false.
   deallocate( this%gamma_inv )
  end if
  if ( this%allocated_index ) then
   this%allocated_index = .false.
   deallocate( this%part_index )
  end if
  if ( this%allocated_data_out ) then
   this%allocated_data_out = .false.
   deallocate( this%data_output )
  end if

 end subroutine
 !========================================
 ! TYPE BOUND PROCEDURES
 !========================================

 subroutine append_spec( this, other )
  class(species_new), intent(inout) :: this
  class(base_species_T), intent(inout) :: other
  integer :: tot_size, np1, np2

  ! Other is always already initialized (check in any case)
  if ( (.not. this%initialized) .or. this%empty ) then
    call this%copy(other, 1, other%how_many())
    return
  end if
  if ( other%empty ) then
    return
  end if
  
  np1 = this%how_many()
  np2 = other%how_many()
  tot_size = np1 + np2

  if (this%array_size() < tot_size ) then
   call pd_temp_spec%copy( this, 1, np1 )
   call this%reallocate( tot_size, this%pick_properties() )
   call this%copy( pd_temp_spec, 1, np1 )
  end if
  
  if(other%allocated_x) then
   call assign(this%x, other%x(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_y) then
   call assign(this%y, other%y(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_z) then
   call assign(this%z, other%z(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_px) then
   call assign(this%px, other%px(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_py) then
   call assign(this%py, other%py(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_pz) then
   call assign(this%pz, other%pz(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_gamma) then
   call assign(this%gamma_inv, other%gamma_inv(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_weight) then
   call assign(this%weight, other%weight(1:np2), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_index) then
   call assign(this%part_index, int(other%part_index(1:np2)), &
    np1 + 1, tot_size)
  end if
  if(other%allocated_data_out) then
   call assign(this%data_output, other%data_output(1:np2), &
    np1 + 1, tot_size)
  end if

  call this%set_part_number(tot_size)

 end subroutine

 pure function call_component_spec( this, component, lb, ub ) result(comp)
 !! Function that hides the underlying array and calls the
 !! corresponding component from the particle structure.
 !! @warning
 !! This function gives back always an array of reals!
 !! When using for weights and particle indexes remember to
 !! cast it again to the right type.
 !! @endwarning

  class(species_new), intent(in) :: this
  integer, intent(in) :: component
  integer, intent(in), optional :: lb, ub
  real(dp), allocatable, dimension(:) :: comp
  integer :: lowb, upb, n_parts

  n_parts = this%how_many()

  if (this%empty) then
   allocate( comp(0) )
   return
  end if

  if ( present(lb) ) then
   lowb = lb
  else
   lowb = 1
  end if

  if ( present(ub) ) then
   upb = ub
  else
   upb = n_parts
  end if

  ! WARNING: allocation status should be checked
  select case(component)
  case(X_COMP)
   comp = this%x(lowb:upb)
  case(Y_COMP)
   comp = this%y(lowb:upb)
  case(Z_COMP)
   comp = this%z(lowb:upb)
  case(PX_COMP)
   comp = this%px(lowb:upb)
  case(PY_COMP)
   comp = this%py(lowb:upb)
  case(PZ_COMP)
   comp = this%pz(lowb:upb)
  case(INV_GAMMA_COMP)
   comp = this%gamma_inv(lowb:upb)
  case(W_COMP)
   comp = real( this%weight(lowb:upb), dp )
  case(INDEX_COMP)
   comp = real( this%part_index(lowb:upb), dp )
  end select

 end function

 pure function call_tracked_component_spec( this, component, lb, ub ) result(comp)
 !! Function that hides the underlying array and calls the
 !! corresponding component from the particle structure.
 !! @warning
 !! This function gives back always an array of reals!
 !! When using for weights and particle indexes remember to
 !! cast it again to the right type.
 !! @endwarning

  class(species_new), intent(in) :: this
  integer, intent(in) :: component
  integer, intent(in), optional :: lb, ub
  real(dp), allocatable, dimension(:) :: comp
  integer :: lowb, upb, n_parts
  logical :: tracked
  logical, allocatable, dimension(:) :: track_mask

  n_parts = this%how_many()
  tracked = this%istracked()

  if ( .not. tracked ) return

  allocate( track_mask(n_parts) )

  if ( present(lb) ) then
   lowb = lb
  else
   lowb = 1
  end if

  if ( present(ub) ) then
   upb = ub
  else
   upb = n_parts
  end if

  associate (xx => this%call_component(component, lb=lowb, ub=upb))

   track_mask(1:n_parts) = ( this%part_index(lowb:upb) > 0 )
   comp = PACK(xx(1:n_parts), track_mask(1:n_parts))
  end associate

 end function

 subroutine extend_spec( this, new_number )
  class(species_new), intent(inout) :: this
  integer, intent(in) :: new_number
  integer :: n_size


  if ( .not. this%initialized ) then
   write (6, *) 'Warning, cannot extend uninitialized species'
   return
  end if

  n_size = this%array_size()
  if ( n_size >= new_number ) then
   call this%set_part_number(new_number)
   return
  end if
  call pd_temp_spec%copy(this)
  call this%reallocate(new_number, this%pick_properties())
  call this%copy(pd_temp_spec)
  call this%set_part_number(new_number)

 end subroutine

 subroutine sel_particles_bounds_spec( this, out_sp, lower_bound, upper_bound )
 !! Function that selects particles with respect to the given array boundaries
 !! (Memory position, NOT a particle index)
  class(species_new), intent(in) :: this
  class(base_species_T), intent(inout) :: out_sp
  integer, intent(in) :: lower_bound, upper_bound
  integer :: tot_len

  tot_len = upper_bound - lower_bound

  if ( out_sp%initialized ) then
   call out_sp%sweep()
  end if

  call out_sp%new_species( tot_len, this%pick_dimensions(), tracked=this%istracked(), &
  extra_outputs=this%pick_extra_outputs() )
  call out_sp%set_charge(this%pick_charge())

  if( this%allocated_x ) then
   out_sp%x = this%x(lower_bound:upper_bound)
  end if
  if( this%allocated_y ) then
   out_sp%y = this%y(lower_bound:upper_bound)
  end if
  if( this%allocated_z ) then
   out_sp%z = this%z(lower_bound:upper_bound)
  end if
  if( this%allocated_px ) then
   out_sp%px = this%px(lower_bound:upper_bound)
  end if
  if( this%allocated_py ) then
   out_sp%py = this%py(lower_bound:upper_bound)
  end if
  if( this%allocated_pz ) then
   out_sp%pz = this%pz(lower_bound:upper_bound)
  end if
  if( this%allocated_gamma ) then
   out_sp%gamma_inv = this%gamma_inv(lower_bound:upper_bound)
  end if
  if( this%allocated_weight ) then
   out_sp%weight = this%weight(lower_bound:upper_bound)
  end if
  if( this%allocated_index ) then
   out_sp%part_index = this%part_index(lower_bound:upper_bound)
  end if
  if( this%allocated_data_out ) then
   out_sp%data_output = this%data_output(lower_bound:upper_bound)
  end if

  call out_sp%set_part_number(out_sp%array_size())

 end subroutine

 subroutine sel_particles_index_spec( this, out_sp, index_array_in )
  class(species_new), intent(in) :: this
  class(base_species_T), intent(inout) :: out_sp
  integer, dimension(:), intent(in) :: index_array_in
  integer :: i, tot_len, n

  tot_len = SIZE(index_array_in, DIM=1)

  if ( out_sp%initialized ) then
   call out_sp%sweep()
  end if

  call out_sp%new_species( tot_len, this%pick_dimensions(), tracked=this%istracked(), &
  extra_outputs=this%pick_extra_outputs() )
  call out_sp%set_charge(this%pick_charge())

  if( this%allocated_x ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%x(i) = this%x(n)
   end do
  end if
  if( this%allocated_y ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%y(i) = this%y(n)
   end do
  end if
  if( this%allocated_z ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%z(i) = this%z(n)
   end do
  end if

  if( this%allocated_px ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%px(i) = this%px(n)
   end do
  end if
  if( this%allocated_py ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%py(i) = this%py(n)
   end do
  end if
  if( this%allocated_pz ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%pz(i) = this%pz(n)
   end do
  end if
  if( this%allocated_gamma ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%gamma_inv(i) = this%gamma_inv(n)
   end do
  end if

  if( this%allocated_weight ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%weight(i) = this%weight(n)
   end do
  end if
  if( this%allocated_index ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%part_index(i) = this%part_index(n)
   end do
  end if
  if( this%allocated_data_out ) then
   do i = 1, tot_len
    n = index_array_in(i)
    out_sp%data_output(i) = this%data_output(n)
   end do
  end if

 end subroutine

 subroutine set_component_real( this, values, component, lb, ub )
  !! Assigns an array of real values to a given `species_new` component
  class(species_new), intent(inout) :: this
  real (dp), intent(in) :: values(:)
  integer, intent(in) :: component
  integer, intent(in), optional :: lb, ub
  integer :: lowb, upb, np

  np = this%how_many()
  if ( present(lb) ) then
   lowb = lb
  else
   lowb = 1
  end if

  if ( present(ub) ) then
   upb = ub
  else
   upb = SIZE(values, DIM=1)
  end if

  select case(component)
  case(X_COMP)
   call assign(this%x, values, lowb, upb, np)
   this%allocated_x = .true.
  case(Y_COMP)
   call assign(this%y, values, lowb, upb, np)
   this%allocated_y = .true.
  case(Z_COMP)
   call assign(this%z, values, lowb, upb, np)
   this%allocated_z = .true.
  case(PX_COMP)
   call assign(this%px, values, lowb, upb, np)
   this%allocated_px = .true.
  case(PY_COMP)
   call assign(this%py, values, lowb, upb, np)
   this%allocated_py = .true.
  case(PZ_COMP)
   call assign(this%pz, values, lowb, upb, np)
   this%allocated_pz = .true.
  case(INV_GAMMA_COMP)
   call assign(this%gamma_inv, values, lowb, upb, np)
   this%allocated_gamma = .true.
  case(W_COMP)
   call assign(this%weight, values, lowb, upb, np)
   this%allocated_weight = .true.
  case(A_PARTICLE)
   call assign(this%data_output, values, lowb, upb, np)
   this%allocated_data_out = .true.
  case default
   call write_warning('Other components in set_component not allowed')
  end select

 end subroutine

 subroutine set_component_integer( this, values, component, lb, ub )
  !! Assigns an array of integer values to a given `species_new` component
  class(species_new), intent(inout) :: this
  integer, intent(in) :: values(:)
  integer, intent(in) :: component
  integer, intent(in), optional :: lb, ub
  integer :: lowb, upb, np

  np = this%how_many()
  if ( present(lb) ) then
   lowb = lb
  else
   lowb = 1
  end if

  if ( present(ub) ) then
   upb = ub
  else
   upb = SIZE(values, DIM=1)
  end if

  select case(component)
  case(X_COMP)
   call assign(this%x, values, lowb, upb, np)
   this%allocated_x = .true.
  case(Y_COMP)
   call assign(this%y, values, lowb, upb, np)
   this%allocated_y = .true.
  case(Z_COMP)
   call assign(this%z, values, lowb, upb, np)
   this%allocated_z = .true.
  case(PX_COMP)
   call assign(this%px, values, lowb, upb, np)
   this%allocated_px = .true.
  case(PY_COMP)
   call assign(this%py, values, lowb, upb, np)
   this%allocated_py = .true.
  case(PZ_COMP)
   call assign(this%pz, values, lowb, upb, np)
   this%allocated_pz = .true.
  case(INV_GAMMA_COMP)
   call assign(this%gamma_inv, values, lowb, upb, np)
   this%allocated_gamma = .true.
  case(W_COMP)
   call assign(this%weight, values, lowb, upb, np)
   this%allocated_weight = .true.
  case(INDEX_COMP)
   call assign(this%part_index, values, lowb, upb, np)
   this%allocated_index = .true.
  case(A_PARTICLE)
   call assign(this%data_output, values, lowb, upb, np)
   this%allocated_data_out = .true.
  case default
   call write_warning('Other components in set_component not allowed')
  end select

 end subroutine
 !========================================
 ! NOT TYPE BOUND PROCEDURES
 !========================================

end module