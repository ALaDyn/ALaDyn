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

module particles_aux_def

 use base_species
 implicit none
 public

 integer, parameter :: EX_COMP = 33
 integer, parameter :: EY_COMP = 34
 integer, parameter :: EZ_COMP = 35
 integer, parameter :: BX_COMP = 36
 integer, parameter :: BY_COMP = 37
 integer, parameter :: BZ_COMP = 38

 integer, parameter :: AUX1_COMP = 39
 integer, parameter :: AUX2_COMP = 40
 integer, parameter :: AUX3_COMP = 41
 integer, parameter :: AUX4_COMP = 42
 integer, parameter :: AUX5_COMP = 43
 integer, parameter :: AUX6_COMP = 44
 integer, parameter :: AUX7_COMP = 45
 integer, parameter :: AUX8_COMP = 46

 integer, parameter :: OLD_X_COMP = 14
 integer, parameter :: OLD_Y_COMP = 15
 integer, parameter :: OLD_Z_COMP = 16
 integer, parameter :: OLD_PX_COMP = 17
 integer, parameter :: OLD_PY_COMP = 18
 integer, parameter :: OLD_PZ_COMP = 19
 integer, parameter :: OLD_GAMMA_COMP = 20

 integer, parameter :: VX_COMP = 21
 integer, parameter :: VY_COMP = 22
 integer, parameter :: VZ_COMP = 23

 integer, parameter :: POND_COMP = 24
 integer, parameter :: GRADF_X_COMP = 25
 integer, parameter :: GRADF_Y_COMP = 26
 integer, parameter :: GRADF_Z_COMP = 27

 integer, parameter :: E_SQUARED = 28
 integer, parameter :: B_SQUARED = 29

 integer, parameter :: FX_COMP = 30
 integer, parameter :: FY_COMP = 31
 integer, parameter :: FZ_COMP = 32

 type, extends(base_species_T) :: species_aux
  !! Auxiliary species for operations on species type

  real(dp), allocatable :: aux1(:)
  logical :: allocated_aux1
  !! True if aux1 array is allocated

  real(dp), allocatable :: aux2(:)
  logical :: allocated_aux2
  !! True if aux2 array is allocated

  real(dp), allocatable :: aux3(:)
  logical :: allocated_aux3
  !! True if aux3 array is allocated

  real(dp), allocatable :: aux4(:)
  logical :: allocated_aux4
  !! True if aux4 array is allocated

  real(dp), allocatable :: aux5(:)
  logical :: allocated_aux5
  !! True if aux5 array is allocated

  real(dp), allocatable :: aux6(:)
  logical :: allocated_aux6
  !! True if aux6 array is allocated

  real(dp), allocatable :: aux7(:)
  logical :: allocated_aux7
  !! True if aux7 array is allocated

  real(dp), allocatable :: aux8(:)
  logical :: allocated_aux8
  !! True if aux8 array is allocated

  contains
   procedure, public :: append => append_aux
   procedure, public :: call_component => call_component_aux
   procedure, public :: extend => extend_aux
   procedure, pass :: new_species => new_species_aux
   procedure, pass :: sel_particles_bounds => sel_particles_bounds_aux
   procedure, pass :: sel_particles_index => sel_particles_index_aux
   procedure, public :: set_component_real => set_component_aux_real
   procedure, public :: set_component_integer => set_component_aux_integer
   procedure, pass :: sweep => sweep_aux

 end type species_aux

 interface operator(*)
  module procedure :: multiply_number_aux
 end interface
 contains

 !========================================
 ! CONSTRUCTOR
 !========================================
 subroutine new_species_aux( this, n_particles, curr_ndims, tracked )
  !! Constructor for the `species_new` type
  class(species_aux), intent(inout) :: this
  integer, intent(in) :: n_particles, curr_ndims
  logical, intent(in), optional :: tracked
  integer :: allocstatus
 
  if (n_particles < 0) then
   return
  end if
  if ( .not. allocated(this%initialized)) then
   allocate(this%initialized)
  end if
  this%initialized = .true.
  call this%set_part_number(n_particles)
  call this%set_dimensions(curr_ndims)
  if ( present(tracked) ) then
   call this%track( tracked )
  else
   call this%track( .false. )
  end if
  this%allocated_x = .false.
  this%allocated_y = .false.
  this%allocated_z = .false.
  this%allocated_px = .false.
  this%allocated_py = .false.
  this%allocated_pz = .false.
  this%allocated_gamma = .false.
  this%allocated_weight = .false.
  this%allocated_index = .false.
  
  this%allocated_aux1 = .false.
  this%allocated_aux2 = .false.
  this%allocated_aux3 = .false.
  this%allocated_aux4 = .false.
  this%allocated_aux5 = .false.
  this%allocated_aux6 = .false.
  this%allocated_aux7 = .false.
  this%allocated_aux8 = .false.
  
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
    allocate( this%part_index(n_particles), stat=allocstatus)
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
    allocate( this%part_index(n_particles), stat=allocstatus)
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
    allocate( this%part_index(n_particles), stat=allocstatus)
    this%allocated_index = .true.
   end if
  case default

  end select
 end subroutine

 !========================================
 ! CLEANING PROCEDURES
 !========================================
 
 subroutine sweep_aux( this )
  class(species_aux), intent(inout) :: this

  if ( .not. allocated(this%initialized) ) then
   return
  else
   deallocate( this%initialized )
  end if

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
  if ( this%allocated_aux1 ) then
   this%allocated_aux1 = .false.
   deallocate( this%aux1 )
  end if
  if ( this%allocated_aux2 ) then
   this%allocated_aux2 = .false.
   deallocate( this%aux2 )
  end if
  if ( this%allocated_aux3 ) then
   this%allocated_aux3 = .false.
   deallocate( this%aux3 )
  end if
  if ( this%allocated_aux4 ) then
   this%allocated_aux4 = .false.
   deallocate( this%aux4 )
  end if
  if ( this%allocated_aux5 ) then
   this%allocated_aux5 = .false.
   deallocate( this%aux5 )
  end if
  if ( this%allocated_aux6 ) then
   this%allocated_aux6 = .false.
   deallocate( this%aux6 )
  end if
  if ( this%allocated_aux7 ) then
   this%allocated_aux7 = .false.
   deallocate( this%aux7 )
  end if
  if ( this%allocated_aux8 ) then
   this%allocated_aux8 = .false.
   deallocate( this%aux8 )
  end if

 end subroutine

 !========================================
 ! TYPE BOUND PROCEDURES
 !========================================

 function append_aux( this, other ) result(spec)
  class(species_aux), intent(inout) :: this
  class(base_species_T), intent(inout) :: other
  type(species_aux) :: spec
  integer :: tot_size

  if ( .not. allocated(this%initialized) .and. .not. allocated(other%initialized) ) then
   return
  else if ( .not. allocated(this%initialized) ) then
   call this%new_species(0, other%pick_dimensions(), tracked=other%istracked())
  else if ( .not. allocated(other%initialized) ) then
   call other%new_species(0, this%pick_dimensions(), tracked=this%istracked())
  end if
  
  if ( allocated(this%initialized) .and. allocated(other%initialized) ) then
   if ( this%empty .and. .not. other%empty ) then
    call spec%copy(other, 1, other%how_many())
    return
   else if (other%empty .and. .not. this%empty) then
    call spec%copy(this, 1, this%how_many())
    return
   else if (other%empty .and. this%empty) then
    return
   end if
  end if
  
  tot_size = this%how_many()+other%how_many()
  call spec%new_species(tot_size, this%pick_dimensions(), tracked=this%istracked())
  
  if(other%allocated_x) then
   call assign(spec%x, [this%call_component(X_COMP), other%call_component(X_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_y) then
   call assign(spec%y, [this%call_component(Y_COMP), other%call_component(Y_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_z) then
   call assign(spec%z, [this%call_component(Z_COMP), other%call_component(Z_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_px) then
   call assign(spec%px, [this%call_component(PX_COMP), other%call_component(PX_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_py) then
   call assign(spec%py, [this%call_component(PY_COMP), other%call_component(PY_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_pz) then
   call assign(spec%pz, [this%call_component(PZ_COMP), other%call_component(PZ_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_gamma) then
   call assign(spec%gamma_inv, [this%call_component(INV_GAMMA_COMP), other%call_component(INV_GAMMA_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_weight) then
   call assign(spec%weight, [this%call_component(W_COMP), other%call_component(W_COMP)], &
    1, tot_size, tot_size)
  end if
  if(other%allocated_index) then
   call assign(spec%part_index, [this%call_component(INDEX_COMP), other%call_component(INDEX_COMP)], &
    1, tot_size, tot_size)
  end if
 end function

 pure function call_component_aux( this, component, lb, ub ) result(comp)
 !! Function that hides the underlying array and calls the
 !! corresponding component from the particle structure.

  class(species_aux), intent(in) :: this
  integer, intent(in) :: component
  integer, intent(in), optional :: lb, ub
  real(dp), allocatable, dimension(:) :: comp
  integer :: lowb, upb, n_parts

  n_parts = this%how_many()

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

  case(EX_COMP)
   comp = this%aux1(lowb:upb)
  case(EY_COMP)
   comp = this%aux2(lowb:upb)
  case(EZ_COMP)
   comp = this%aux3(lowb:upb)
  case(BX_COMP)
   comp = this%aux4(lowb:upb)
  case(BY_COMP)
   comp = this%aux5(lowb:upb)
  case(BZ_COMP)
   comp = this%aux6(lowb:upb)

  case(AUX1_COMP)
   comp = this%aux1(lowb:upb)
  case(AUX2_COMP)
   comp = this%aux2(lowb:upb)
  case(AUX3_COMP)
   comp = this%aux3(lowb:upb)
  case(AUX4_COMP)
   comp = this%aux4(lowb:upb)
  case(AUX5_COMP)
   comp = this%aux5(lowb:upb)
  case(AUX6_COMP)
   comp = this%aux6(lowb:upb)
  case(AUX7_COMP)
   comp = this%aux7(lowb:upb)
  case(AUX8_COMP)
   comp = this%aux8(lowb:upb)

  case(OLD_X_COMP)
   comp = this%x(lowb:upb)
  case(OLD_Y_COMP)
   comp = this%y(lowb:upb)
  case(OLD_Z_COMP)
   comp = this%z(lowb:upb)
  case(OLD_PX_COMP)
   comp = this%px(lowb:upb)
  case(OLD_PY_COMP)
   comp = this%py(lowb:upb)
  case(OLD_PZ_COMP)
   comp = this%pz(lowb:upb)
  case(OLD_GAMMA_COMP)
   comp = this%gamma_inv(lowb:upb)

  case(VX_COMP)
   comp = this%px(lowb:upb)
  case(VY_COMP)
   comp = this%py(lowb:upb)
  case(VZ_COMP)
   comp = this%pz(lowb:upb)

  case(POND_COMP)
   comp = this%aux4(lowb:upb)
  case(GRADF_X_COMP)
   comp = this%aux1(lowb:upb)
  case(GRADF_Y_COMP)
   comp = this%aux2(lowb:upb)
  case(GRADF_Z_COMP)
   comp = this%aux3(lowb:upb)

  case(E_SQUARED)
   comp = this%aux8(lowb:upb)

  case(FX_COMP)
   comp = this%aux1(lowb:upb)
  case(FY_COMP)
   comp = this%aux2(lowb:upb)
  case(FZ_COMP)
   comp = this%aux3(lowb:upb)
  
  end select

 end function

 
 subroutine extend_aux( this, new_number )
  class(species_aux), intent(inout) :: this
  integer, intent(in) :: new_number
  type(species_aux) :: temp
  integer :: n_parts

  if ( .not. allocated(this%initialized) ) then
   write (6, *) 'Warning, cannot extend uninitialized species'
   return
  end if

  n_parts = this%how_many()
  if ( n_parts >= new_number ) then
   return
  end if

  call temp%copy(this)
  call this%sweep()
  call this%new_species(new_number, this%pick_dimensions(), tracked=this%istracked())
  call this%copy(temp)

  end subroutine

 subroutine sel_particles_bounds_aux( this, out_sp, lower_bound, upper_bound )
  !! Function that selects particles with respect to the given array boundaries
  !! (Memory position, NOT a particle index)
   class(species_aux), intent(in) :: this
   class(base_species_T), intent(inout) :: out_sp
   integer, intent(in) :: lower_bound, upper_bound
   integer :: tot_len
 
   tot_len = upper_bound - lower_bound

   if ( allocated(out_sp%initialized) ) then
    call out_sp%sweep()
   end if

   call out_sp%new_species( tot_len, this%pick_dimensions(), tracked=this%istracked())
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
 
   call out_sp%set_part_number(out_sp%array_size())
 
  end subroutine
 
  subroutine sel_particles_index_aux( this, out_sp, index_array )
   class(species_aux), intent(in) :: this
   class(base_species_T), intent(inout) :: out_sp
   integer, dimension(:), intent(in) :: index_array
   integer :: i, tot_len, n
 
   tot_len = SIZE(index_array, DIM=1)

   if ( allocated(out_sp%initialized) ) then
    call out_sp%sweep()
   end if

   call out_sp%new_species( tot_len, this%pick_dimensions(), tracked=this%istracked())
   call out_sp%set_charge(this%pick_charge())
 
   if( this%allocated_x ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%x(i) = this%x(n)
    end do
   end if
   if( this%allocated_y ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%y(i) = this%y(n)
    end do
   end if
   if( this%allocated_z ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%z(i) = this%z(n)
    end do
   end if
 
   if( this%allocated_px ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%px(i) = this%px(n)
    end do
   end if
   if( this%allocated_py ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%py(i) = this%py(n)
    end do
   end if
   if( this%allocated_pz ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%pz(i) = this%pz(n)
    end do
   end if
   if( this%allocated_gamma ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%gamma_inv(i) = this%gamma_inv(n)
    end do
   end if
 
   if( this%allocated_weight ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%weight(i) = this%weight(n)
    end do
   end if
   if( this%allocated_index ) then
    do i = 1, tot_len
     n = index_array(i)
     out_sp%part_index(i) = this%part_index(n)
    end do
   end if
 
  end subroutine

 subroutine set_component_aux_real( this, values, component, lb, ub )
  !! Assigns an array of real values to a given `species_new` component
  class(species_aux), intent(inout) :: this
  real (dp), intent(in) :: values(:)
  integer, intent(in) :: component
  integer, intent(in), optional :: lb, ub
  integer :: lowb, upb

  if ( present(lb) ) then
   lowb = lb
  else
   lowb = lbound(this%call_component( component ), 1)
  end if

  if ( present(ub) ) then
   upb = ub
  else
   upb = ubound(this%call_component( component ), 1)
  end if

  select case(component)
  case(X_COMP)
   call assign(this%x, values, lowb, upb, this%how_many())
   this%allocated_x = .true.
  case(Y_COMP)
   call assign(this%y, values, lowb, upb, this%how_many())
   this%allocated_y = .true.
  case(Z_COMP)
   call assign(this%z, values, lowb, upb, this%how_many())
   this%allocated_z = .true.
  case(PX_COMP)
   call assign(this%px, values, lowb, upb, this%how_many())
   this%allocated_px = .true.
  case(PY_COMP)
   call assign(this%py, values, lowb, upb, this%how_many())
   this%allocated_py = .true.
  case(PZ_COMP)
   call assign(this%pz, values, lowb, upb, this%how_many())
   this%allocated_pz = .true.
  case(INV_GAMMA_COMP)
   call assign(this%gamma_inv, values, lowb, upb, this%how_many())
   this%allocated_gamma = .true.
  case(W_COMP)
   call assign(this%weight, values, lowb, upb, this%how_many())
   this%allocated_weight = .true.

  case(EX_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(EY_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(EZ_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.
  case(BX_COMP)
   call check_array_1d(this%aux4, upb - lowb + 1)
   call assign(this%aux4, values, lowb, upb, this%how_many())
   this%allocated_aux4 = .true.
  case(BY_COMP)
   call check_array_1d(this%aux5, upb - lowb + 1)
   call assign(this%aux5, values, lowb, upb, this%how_many())
   this%allocated_aux5 = .true.
  case(BZ_COMP)
   call check_array_1d(this%aux6, upb - lowb + 1)
   call assign(this%aux6, values, lowb, upb, this%how_many())
   this%allocated_aux6 = .true.

  case(AUX1_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(AUX2_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(AUX3_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.
  case(AUX4_COMP)
   call check_array_1d(this%aux4, upb - lowb + 1)
   call assign(this%aux4, values, lowb, upb, this%how_many())
   this%allocated_aux4 = .true.
  case(AUX5_COMP)
   call check_array_1d(this%aux5, upb - lowb + 1)
   call assign(this%aux5, values, lowb, upb, this%how_many())
   this%allocated_aux5 = .true.
  case(AUX6_COMP)
   call check_array_1d(this%aux6, upb - lowb + 1)
   call assign(this%aux6, values, lowb, upb, this%how_many())
   this%allocated_aux6 = .true.
  case(AUX7_COMP)
   call check_array_1d(this%aux7, upb - lowb + 1)
   call assign(this%aux7, values, lowb, upb, this%how_many())
   this%allocated_aux7 = .true.
  case(AUX8_COMP)
   call check_array_1d(this%aux8, upb - lowb + 1)
   call assign(this%aux8, values, lowb, upb, this%how_many())
   this%allocated_aux8 = .true.

  case(OLD_X_COMP)
   call assign(this%x, values, lowb, upb, this%how_many())
   this%allocated_x = .true.
  case(OLD_Y_COMP)
   call assign(this%y, values, lowb, upb, this%how_many())
   this%allocated_y = .true.
  case(OLD_Z_COMP)
   call assign(this%z, values, lowb, upb, this%how_many())
   this%allocated_z = .true.
  case(OLD_PX_COMP)
   call assign(this%px, values, lowb, upb, this%how_many())
   this%allocated_px = .true.
  case(OLD_PY_COMP)
   call assign(this%py, values, lowb, upb, this%how_many())
   this%allocated_py = .true.
  case(OLD_PZ_COMP)
   call assign(this%pz, values, lowb, upb, this%how_many())
   this%allocated_pz = .true.
  case(OLD_GAMMA_COMP)
   call assign(this%gamma_inv, values, lowb, upb, this%how_many())
   this%allocated_gamma = .true.

  case(VX_COMP)
   call assign(this%px, values, lowb, upb, this%how_many())
   this%allocated_px = .true.
  case(VY_COMP)
   call assign(this%py, values, lowb, upb, this%how_many())
   this%allocated_py = .true.
  case(VZ_COMP)
   call assign(this%pz, values, lowb, upb, this%how_many())
   this%allocated_pz = .true.

  case(POND_COMP)
   call check_array_1d(this%aux4, upb - lowb + 1)
   call assign(this%aux4, values, lowb, upb, this%how_many())
   this%allocated_aux4 = .true.
  case(GRADF_X_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(GRADF_Y_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(GRADF_Z_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.

  case(E_SQUARED)
   call check_array_1d(this%aux8, upb - lowb + 1)
   call assign(this%aux8, values, lowb, upb, this%how_many())
   this%allocated_aux8 = .true.

  case(FX_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(FY_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(FZ_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.
  end select

 end subroutine

 subroutine set_component_aux_integer( this, values, component, lb, ub )
  !! Assigns an array of integer values to a given `species_new` component
  class(species_aux), intent(inout) :: this
  integer, intent(in) :: values(:)
  integer, intent(in) :: component
  integer, intent(in), optional :: lb, ub
  integer :: lowb, upb

  if ( present(lb) ) then
   lowb = lb
  else
   lowb = lbound(this%call_component( component ), 1)
  end if

  if ( present(ub) ) then
   upb = ub
  else
   upb = ubound(this%call_component( component ), 1)
  end if

  select case(component)
  case(X_COMP)
   call assign(this%x, values, lowb, upb, this%how_many())
   this%allocated_x = .true.
  case(Y_COMP)
   call assign(this%y, values, lowb, upb, this%how_many())
   this%allocated_y = .true.
  case(Z_COMP)
   call assign(this%z, values, lowb, upb, this%how_many())
   this%allocated_z = .true.
  case(PX_COMP)
   call assign(this%px, values, lowb, upb, this%how_many())
   this%allocated_px = .true.
  case(PY_COMP)
   call assign(this%py, values, lowb, upb, this%how_many())
   this%allocated_py = .true.
  case(PZ_COMP)
   call assign(this%pz, values, lowb, upb, this%how_many())
   this%allocated_pz = .true.
  case(INV_GAMMA_COMP)
   call assign(this%gamma_inv, values, lowb, upb, this%how_many())
   this%allocated_gamma = .true.
  case(W_COMP)
   call assign(this%weight, values, lowb, upb, this%how_many())
   this%allocated_weight = .true.
  case(INDEX_COMP)
   call assign(this%part_index, values, lowb, upb, this%how_many())
   this%allocated_index = .true.
   call this%track( .true. )

  case(EX_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(EY_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(EZ_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.
  case(BX_COMP)
   call check_array_1d(this%aux4, upb - lowb + 1)
   call assign(this%aux4, values, lowb, upb, this%how_many())
   this%allocated_aux4 = .true.
  case(BY_COMP)
   call check_array_1d(this%aux5, upb - lowb + 1)
   call assign(this%aux5, values, lowb, upb, this%how_many())
   this%allocated_aux5 = .true.
  case(BZ_COMP)
   call check_array_1d(this%aux6, upb - lowb + 1)
   call assign(this%aux6, values, lowb, upb, this%how_many())
   this%allocated_aux6 = .true.

  case(AUX1_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(AUX2_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(AUX3_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.
  case(AUX4_COMP)
   call check_array_1d(this%aux4, upb - lowb + 1)
   call assign(this%aux4, values, lowb, upb, this%how_many())
   this%allocated_aux4 = .true.
  case(AUX5_COMP)
   call check_array_1d(this%aux5, upb - lowb + 1)
   call assign(this%aux5, values, lowb, upb, this%how_many())
   this%allocated_aux5 = .true.
  case(AUX6_COMP)
   call check_array_1d(this%aux6, upb - lowb + 1)
   call assign(this%aux6, values, lowb, upb, this%how_many())
   this%allocated_aux6 = .true.
  case(AUX7_COMP)
   call check_array_1d(this%aux7, upb - lowb + 1)
   call assign(this%aux7, values, lowb, upb, this%how_many())
   this%allocated_aux7 = .true.
  case(AUX8_COMP)
   call check_array_1d(this%aux8, upb - lowb + 1)
   call assign(this%aux8, values, lowb, upb, this%how_many())
   this%allocated_aux8 = .true.

  case(OLD_X_COMP)
   call assign(this%x, values, lowb, upb, this%how_many())
   this%allocated_x = .true.
  case(OLD_Y_COMP)
   call assign(this%y, values, lowb, upb, this%how_many())
   this%allocated_y = .true.
  case(OLD_Z_COMP)
   call assign(this%z, values, lowb, upb, this%how_many())
   this%allocated_z = .true.
  case(OLD_PX_COMP)
   call assign(this%px, values, lowb, upb, this%how_many())
   this%allocated_px = .true.
  case(OLD_PY_COMP)
   call assign(this%py, values, lowb, upb, this%how_many())
   this%allocated_py = .true.
  case(OLD_PZ_COMP)
   call assign(this%pz, values, lowb, upb, this%how_many())
   this%allocated_pz = .true.
  case(OLD_GAMMA_COMP)
   call assign(this%gamma_inv, values, lowb, upb, this%how_many())
   this%allocated_gamma = .true.

  case(VX_COMP)
   call assign(this%px, values, lowb, upb, this%how_many())
   this%allocated_px = .true.
  case(VY_COMP)
   call assign(this%py, values, lowb, upb, this%how_many())
   this%allocated_py = .true.
  case(VZ_COMP)
   call assign(this%pz, values, lowb, upb, this%how_many())
   this%allocated_pz = .true.

  case(POND_COMP)
   call check_array_1d(this%aux4, upb - lowb + 1)
   call assign(this%aux4, values, lowb, upb, this%how_many())
   this%allocated_aux4 = .true.
  case(GRADF_X_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(GRADF_Y_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(GRADF_Z_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.

  case(E_SQUARED)
   call check_array_1d(this%aux8, upb - lowb + 1)
   call assign(this%aux8, values, lowb, upb, this%how_many())
   this%allocated_aux8 = .true.

  case(FX_COMP)
   call check_array_1d(this%aux1, upb - lowb + 1)
   call assign(this%aux1, values, lowb, upb, this%how_many())
   this%allocated_aux1 = .true.
  case(FY_COMP)
   call check_array_1d(this%aux2, upb - lowb + 1)
   call assign(this%aux2, values, lowb, upb, this%how_many())
   this%allocated_aux2 = .true.
  case(FZ_COMP)
   call check_array_1d(this%aux3, upb - lowb + 1)
   call assign(this%aux3, values, lowb, upb, this%how_many())
   this%allocated_aux3 = .true.
  end select

 end subroutine

 !========================================
 ! NOT TYPE BOUND PROCEDURES
 !========================================

 function multiply_number_aux( this, number ) result(dot)
  type(species_aux), intent(in) :: this
  real(dp), intent(in) :: number
  type(species_aux) :: dot
  integer :: np

  if ( .not. allocated(this%initialized) ) then
   write(6, *) 'Error, particle aux vector not initialized'
  end if

  if ( this%empty ) then
   call dot%new_species(this%how_many(), this%pick_dimensions())
  else
   np = this%how_many()
   call dot%new_species(this%how_many(), this%pick_dimensions())
   call dot%set_charge(this%pick_charge())
   if( this%allocated_x ) then
    call assign(dot%x, number*this%call_component(X_COMP), 1, np)
   end if
   if( this%allocated_y ) then
    call assign(dot%y, number*this%call_component(Y_COMP), 1, np)
   end if
   if( this%allocated_z ) then
    call assign(dot%z, number*this%call_component(Z_COMP), 1, np)
   end if
   if( this%allocated_px ) then
    call assign(dot%px, number*this%call_component(PX_COMP), 1, np)
   end if
   if( this%allocated_py ) then
    call assign(dot%py, number*this%call_component(PY_COMP), 1, np)
   end if
   if( this%allocated_pz ) then
    call assign(dot%pz, number*this%call_component(PZ_COMP), 1, np)
   end if
   if( this%allocated_gamma ) then
    call assign(dot%gamma_inv, number*this%call_component(INV_GAMMA_COMP), 1, np)
   end if
   if( this%allocated_weight ) then
    call assign(dot%weight, number*this%call_component(W_COMP), 1, np)
   end if
   if( this%allocated_index ) then
    call assign(dot%part_index, number*this%call_component(INDEX_COMP), 1, np)
   end if

   if( this%allocated_aux1 ) then
    call assign(dot%aux1, number*this%call_component(AUX1_COMP), 1, np)
   end if
   if( this%allocated_aux2 ) then
    call assign(dot%aux2, number*this%call_component(AUX2_COMP), 1, np)
   end if
   if( this%allocated_aux3 ) then
    call assign(dot%aux3, number*this%call_component(AUX3_COMP), 1, np)
   end if
   if( this%allocated_aux4 ) then
    call assign(dot%aux4, number*this%call_component(AUX4_COMP), 1, np)
   end if
   if( this%allocated_aux5 ) then
    call assign(dot%aux5, number*this%call_component(AUX5_COMP), 1, np)
   end if
   if( this%allocated_aux6 ) then
    call assign(dot%aux6, number*this%call_component(AUX6_COMP), 1, np)
   end if
   if( this%allocated_aux7 ) then
    call assign(dot%aux7, number*this%call_component(AUX7_COMP), 1, np)
   end if
   if( this%allocated_aux8 ) then
    call assign(dot%aux8, number*this%call_component(AUX8_COMP), 1, np)
   end if
  end if
  
 end function
end module