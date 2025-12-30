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

module base_species

 use precision_def
 use index_array_module
 use util, only: gasdev
 use warnings, only: write_warning
 implicit none
 public

 integer, parameter :: X_COMP = 1
 integer, parameter :: Y_COMP = 2
 integer, parameter :: Z_COMP = 3
 integer, parameter :: PX_COMP = 4
 integer, parameter :: PY_COMP = 5
 integer, parameter :: PZ_COMP = 6
 integer, parameter :: INV_GAMMA_COMP = 7
 integer, parameter :: W_COMP = 8
 integer, parameter :: INDEX_COMP = -1

 integer, parameter :: A_PARTICLE = -2
 integer, parameter :: E_X_PARTICLE = -3
 integer, parameter :: E_Y_PARTICLE = -4
 integer, parameter :: E_Z_PARTICLE = -5
 integer, parameter :: B_X_PARTICLE = -6
 integer, parameter :: B_Y_PARTICLE = -7
 integer, parameter :: B_Z_PARTICLE = -8

 real(dp), allocatable, dimension(:), save :: bs_temp_1d

 type track_data_t
  logical, public :: tracked = .false.
  !! Flag to track the particles
  integer, public :: n_tracked = 0
  !! Number of tracked particles
  real(dp), allocatable, public :: xmin
  !! Minimum x position of the tracked particles
  real(dp), allocatable, public :: xmax
  !! Maximum x position of the tracked particles
  real(dp), allocatable, public :: ymin
  !! Minimum y position of the tracked particles
  real(dp), allocatable, public :: ymax
  !! Maximum y position of the tracked particles
  real(dp), allocatable, public :: zmin
  !! Minimum z position of the tracked particles
  real(dp), allocatable, public :: zmax
  !! Maximum z position of the tracked particles
  integer, public :: jump = 1
  !! Jump parameter in particles selection
  integer, public :: highest_index = 0
  !! Highest particle index available
  integer, public :: extra_outputs = 0
  !! Number of extra outputs (*i.e.* not included in the particles dynamics)
  contains
   procedure, public, pass :: sweep => sweep_track_data
 end type

 type scalars
  character(:), allocatable :: name
  !! Species name
  real(dp), private :: charge = -1
  !! Particle charge
  integer, private :: n_part = 0
  !! Number of particles
  integer, private :: dimensions = 2
  !! Number of dimensions in which particles live
  real, private :: temperature = 0
  !! Initial temperature given to the species
  type(track_data_t), public :: track_data
  !! Type containing all the tracking datas
  logical :: test = .false.
  !! Indicates if the species is test (generates current)
  logical :: mobile = .false.
  !! Indicates either if the species moves or it's frozen
 contains
  procedure, public, pass :: istracked => istracked_scalars
  procedure, public, pass :: how_many => how_many_scalars
  procedure, public, pass :: pick_extra_outputs => pick_extra_outputs_scalars
  procedure, public, pass :: pick_charge => pick_charge_scalars
  procedure, public, pass :: pick_dimensions => pick_dimensions_scalars
  procedure, public, pass :: pick_name => pick_name_scalars
  procedure, public, pass :: pick_properties => pick_properties_scalars
  procedure, public, pass :: pick_temperature => pick_temperature_scalars
  procedure, public, pass :: set_charge => set_charge_scalars
  procedure, public, pass :: set_dimensions => set_dimensions_scalars
  procedure, public, pass :: set_name => set_name_scalars
  procedure, public, pass :: set_extra_outputs => set_extra_outputs_scalars
  procedure, public, pass :: set_part_number => set_part_number_scalars
  procedure, public, pass :: set_temperature => set_temperature_scalars
  procedure, public, pass :: sweep => sweep_scalars
  procedure, public, pass :: track => track_scalars
 end type scalars

 type, abstract :: base_species_T

  logical :: initialized = .false.
  !! Flag that states if the species has been initialized
  logical :: empty = .true.
  !! Flag that states if there are particles

  type(scalars) :: properties
  !! Contains the informations of the species

  real(dp), allocatable :: x(:)
  !! Array containig the x particle positions
  logical :: allocated_x = .false.
  !! True if x array is allocated

  real(dp), allocatable :: y(:)
  !! Array containig the y particle positions
  logical :: allocated_y = .false.
  !! True if y array is allocated

  real(dp), allocatable :: z(:)
  !! Array containig the z particle positions
  logical :: allocated_z = .false.
  !! True if z array is allocated

  real(dp), allocatable :: px(:)
  !! Array containig the x particle momenta
  logical :: allocated_px = .false.
  !! True if px array is allocated

  real(dp), allocatable :: py(:)
  !! Array containig the y particle momenta
  logical :: allocated_py = .false.
  !! True if py array is allocated

  real(dp), allocatable :: pz(:)
  !! Array containig the z particle momenta
  logical :: allocated_pz = .false.
  !! True if pz array is allocated
  
  real(dp), allocatable :: gamma_inv(:)
  !! Array containig the inverse of Lorentz gamma factor
  logical :: allocated_gamma = .false.
  !! True if gamma array is allocated
  
  real (dp), allocatable :: weight(:)
  !! Array containig the particle weights
  logical :: allocated_weight = .false.
  !! True if weight array is allocated
  
  integer, allocatable :: part_index(:)
  !! Array containig the particle index
  logical :: allocated_index = .false.
  !! True if index array is allocated
  
  real(dp), allocatable :: data_output(:)
  !! Array used to pass any information about particles
  !! not relevant to the dynamics that has to be returned in the outputs
  logical :: allocated_data_out = .false.
  !! True if data_output is allocated

  contains
   procedure, public, pass :: add_data
   procedure, public, pass :: allocate_data_output
   procedure, pass :: array_component
   procedure, pass :: array_size
   procedure, private, pass :: call_particle_bounds
   procedure, private, pass :: call_particle_index_array
   procedure, private, pass :: call_particle_single
   procedure, public, pass :: check_tracking
   procedure, pass :: compute_gamma
   procedure, pass, private :: copy_all
   procedure, pass, private :: copy_boundaries
   procedure, pass, public :: count_tracked_particles
   procedure, public, pass :: deallocate_data_output
   procedure, public, pass :: dump_properties_to_array
   procedure, public, pass :: dump_tracking_to_array
   procedure, public, pass :: flatten_into
   procedure, pass :: how_many
   procedure, pass :: initialize_data
   procedure, public, pass :: ismobile
   procedure, public, pass :: istest
   procedure, public, pass :: istracked
   procedure, pass :: new_species => new_species_abstract
   procedure, public :: pack_into => pack_into_array
   procedure, public, pass :: pick_charge
   procedure, public, pass :: pick_dimensions
   procedure, public, pass :: pick_extra_outputs
   procedure, public, pass :: pick_name
   procedure, public, pass :: pick_properties
   procedure, public, pass :: pick_temperature
   procedure, public, pass :: pick_tot_tracked_parts
   procedure, public, pass :: pick_track_params
   procedure, public, nopass :: properties_array_size
   procedure, public, pass :: read_properties_from_array
   procedure, public, pass :: read_tracking_from_array
   procedure, public, pass :: reallocate
   procedure, public, pass :: redistribute
   procedure, public, pass :: return_gamma
   procedure, pass :: set_charge_int
   procedure, pass :: set_charge_real
   procedure, pass :: set_dimensions
   procedure, pass :: set_extra_outputs
   procedure, public, pass :: set_highest_track_index
   procedure, public, pass :: set_mobile
   procedure, public, pass :: set_name
   procedure, pass :: set_part_number
   procedure, pass :: set_properties
   procedure, pass :: set_temperature
   procedure, public, pass :: set_tot_tracked_parts
   procedure, public, pass :: set_track_params
   procedure, public, pass :: track
   procedure, public, pass :: total_size
   procedure(call_component_abstract), deferred, pass :: call_component
   procedure(call_tracked_component_abstract), deferred, pass :: call_tracked_component
   procedure(extend_abstract), deferred, pass :: extend
   procedure(sel_particles_bounds_abstract), deferred, pass :: sel_particles_bounds
   procedure(sel_particles_index_abstract), deferred, pass :: sel_particles_index
   procedure(set_component_abstract_real), deferred, pass :: set_component_real
   procedure(set_component_abstract_integer), deferred, pass :: set_component_integer
   procedure(sweep_abstract), deferred, pass :: sweep
   generic :: call_particle => call_particle_single, call_particle_bounds, call_particle_index_array
   generic :: copy => copy_all, copy_boundaries
   generic :: sel_particles => sel_particles_bounds, sel_particles_index
   generic :: set_component => set_component_real, set_component_integer
   generic :: set_charge => set_charge_int, set_charge_real
  end type

  abstract interface
   pure function call_component_abstract( this, component, lb, ub ) result(comp)
    import :: base_species_T, dp
    implicit none
    class(base_species_T), intent(in) :: this
    integer, intent(in) :: component
    integer, intent(in), optional :: lb, ub
    real(dp), allocatable, dimension(:) :: comp
   end function
  end interface

  abstract interface
   pure function call_tracked_component_abstract( this, component, lb, ub ) result(comp)
    import :: base_species_T, dp
    implicit none
    class(base_species_T), intent(in) :: this
    integer, intent(in) :: component
    integer, intent(in), optional :: lb, ub
    real(dp), allocatable, dimension(:) :: comp
   end function
  end interface

  abstract interface
   subroutine extend_abstract( this, new_number )
    import base_species_T, dp
    class(base_species_T), intent(inout) :: this
    integer, intent(in) :: new_number
   end subroutine
  end interface

  abstract interface
   subroutine set_component_abstract_real( this, values, component, lb, ub )
    import :: base_species_T, dp
    implicit none
    class(base_species_T), intent(inout) :: this
    real(dp), intent(in), dimension(:) :: values
    integer, intent(in) :: component
    integer, intent(in), optional :: lb, ub
   end subroutine

   subroutine set_component_abstract_integer( this, values, component, lb, ub )
    import :: base_species_T, dp
    implicit none
    class(base_species_T), intent(inout) :: this
    integer, intent(in), dimension(:) :: values
    integer, intent(in) :: component
    integer, intent(in), optional :: lb, ub
   end subroutine
  end interface

  abstract interface
   subroutine sel_particles_index_abstract( this, out_sp, index_array_in )
    import base_species_T, dp
    implicit none
    class(base_species_T), intent(in) :: this
    class(base_species_T), intent(inout) :: out_sp
    integer, dimension(:), intent(in) :: index_array_in
   end subroutine
   
   subroutine sel_particles_bounds_abstract( this, out_sp, lower_bound, upper_bound )
    import base_species_T, dp
    implicit none
    class(base_species_T), intent(in) :: this
    class(base_species_T), intent(inout) :: out_sp
    integer, intent(in) :: lower_bound, upper_bound
   end subroutine

  end interface

  abstract interface

   subroutine sweep_abstract( this )
    import base_species_T, dp
    implicit none
    class(base_species_T), intent(inout) :: this
   end subroutine

  end interface

  interface assign
   module procedure assign_real_realdp
   module procedure assign_real_realsp
   module procedure assign_realsp_realsp
   module procedure assign_real_integer
   module procedure assign_integer_realdp
   module procedure assign_integer_realsp
   module procedure assign_integer_integer
  end interface
  contains
  
  !==== Constructor ===
  subroutine new_species_abstract( this, n_particles, curr_ndims, tracked, mobile, extra_outputs )
   !! Constructor for the `species_new` type
   class(base_species_T), intent(inout) :: this
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
   if ( this%initialized ) then
    call write_warning('WARNING: Trying to allocate an already initialized spec object')
   end if
   this%initialized = .true.
   call this%set_part_number(n_particles)
   call this%set_dimensions(curr_ndims)
   if ( present(tracked) ) then
    call this%track( tracked , allocate_now=.false. )
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

!=== Type bound procedures

!DIR$ ATTRIBUTES INLINE:: add_data
  subroutine add_data( this, x_arr, y_arr, z_arr, &
   weightx_arr, weightyz_arr, loc_x0, loc_x, loc_y, loc_z, np_old)
   class( base_species_T), intent(inout) :: this
   real(dp), dimension(:), intent(in) :: x_arr, y_arr, z_arr
   real(dp), dimension(:), intent(in) :: weightx_arr
   real(dp), dimension(:, :), intent(in) :: weightyz_arr
   integer, intent(in) :: loc_x0, loc_x, loc_y, loc_z, np_old
   integer :: p
   real(dp) :: u, t_x
   real(dp) :: whz
   integer :: dim, i, j, k
  ! When this function is called in the window.f90 add_particles routine, species has already
  ! been extended
   t_x = this%pick_temperature()
   dim = this%pick_dimensions()
   p = np_old

   select case(dim)
   case(2)
    do k = 1, 1
     do j = 1, loc_y
      do i = loc_x0, loc_x
       p = p + 1
       this%x(p) = x_arr(i)
       this%y(p) = y_arr(j)
       call gasdev(u)
       this%px(p) = t_x*u
       call gasdev(u)
       this%py(p) = t_x*u
       this%weight(p) = weightx_arr(i)*weightyz_arr(j, k)
      end do
     end do
    end do
   case(3)
    do k = 1, loc_z
     do j = 1, loc_y
      do i = loc_x0, loc_x
       p = p + 1
       this%x(p) = x_arr(i)
       this%y(p) = y_arr(j)
       this%z(p) = z_arr(k)
       call gasdev(u)
       this%px(p) = t_x*u
       call gasdev(u)
       this%py(p) = t_x*u
       call gasdev(u)
       this%pz(p) = t_x*u
       whz = weightx_arr(i)*weightyz_arr(j, k)
       this%weight(p) = whz
      end do
     end do
    end do
   end select
    

  end subroutine

  pure function array_component( this, component ) result(ac)
   class(base_species_T), intent(in) :: this
   integer, intent(in) :: component
   integer :: ac

   select case(this%pick_dimensions())
   case(1)
    select case(component)
    case(X_COMP)
     ac = 1
    case(PX_COMP)
     ac = 2
    case(W_COMP)
     ac = 3
    case(INV_GAMMA_COMP)
     ac = 4
    case(INDEX_COMP)
     ac = 5
    case(A_PARTICLE)
     ac = 6
    case default
     ac = -1
    end select
   case(2)
    select case(component)
    case(X_COMP)
     ac = 1
    case(Y_COMP)
     ac = 2
    case(PX_COMP)
     ac = 3
    case(PY_COMP)
     ac = 4
    case(W_COMP)
     ac = 5
    case(INV_GAMMA_COMP)
     ac = 6
    case(INDEX_COMP)
     ac = 7
    case(A_PARTICLE)
      ac = 8
    case default
     ac = -1
    end select
   case(3)
    select case(component)
    case(X_COMP)
     ac = 1
    case(Y_COMP)
     ac = 2
    case(Z_COMP)
     ac = 3
    case(PX_COMP)
     ac = 4
    case(PY_COMP)
     ac = 5
    case(PZ_COMP)
     ac = 6
    case(W_COMP)
     ac = 7
    case(INV_GAMMA_COMP)
     ac = 8
    case(INDEX_COMP)
     ac = 9
    case(A_PARTICLE)
      ac = 10
    case default
     ac = -1
    end select
   end select
  end function

  subroutine allocate_data_output( this, size_array )
   class(base_species_T), intent(inout) :: this
   integer :: size_array

   call realloc_temp_1d( this%data_output, size_array )
   this%data_output = zero_dp
   this%allocated_data_out = .true.

   end subroutine

  subroutine deallocate_data_output( this )
   class(base_species_T), intent(inout) :: this

   if (this%allocated_data_out) then
    deallocate( this%data_output )
    this%allocated_data_out = .false.
   end if
   end subroutine

!DIR$ ATTRIBUTES INLINE:: call_particle_single
  subroutine call_particle_single( this, particles, index_in, tracking )
   class(base_species_T), intent(in) :: this
   real(dp), dimension(:), intent(inout) :: particles
   integer, intent(in) :: index_in
   logical, intent(in), optional :: tracking
   logical :: track_out
   integer :: i

   track_out = .false.
   if ( present(tracking) ) then
    track_out = tracking
   end if
   i=1

   select case(this%pick_dimensions())
   case(1)
    particles(i) = this%x(index_in)
    i = i + 1
    particles(i) = this%px(index_in)
    i = i + 1
    particles(i) = this%weight(index_in)
    i = i + 1
    particles(i) = this%gamma_inv(index_in)
    i = i + 1
    if ( track_out ) then
     particles(i) = this%part_index(index_in)
     i = i + 1
     if ( this%pick_extra_outputs() > 0 ) then
      particles(i) = this%data_output(index_in)
     end if
    end if
   case(2)
    particles(i) = this%x(index_in)
    i = i + 1
    particles(i) = this%y(index_in)
    i = i + 1
    particles(i) = this%px(index_in)
    i = i + 1
    particles(i) = this%py(index_in)
    i = i + 1
    particles(i) = this%weight(index_in)
    i = i + 1
    particles(i) = this%gamma_inv(index_in)
    i = i + 1
    if ( track_out ) then
     particles(i) = this%part_index(index_in)
     i = i + 1
     if ( this%pick_extra_outputs() > 0 ) then
      particles(i) = this%data_output(index_in)
     end if
    end if
   case(3)
    particles(i) = this%x(index_in)
    i = i + 1
    particles(i) = this%y(index_in)
    i = i + 1
    particles(i) = this%z(index_in)
    i = i + 1
    particles(i) = this%px(index_in)
    i = i + 1
    particles(i) = this%py(index_in)
    i = i + 1
    particles(i) = this%pz(index_in)
    i = i + 1
    particles(i) = this%weight(index_in)
    i = i + 1
    particles(i) = this%gamma_inv(index_in)
    i = i + 1
    if ( track_out ) then
     particles(i) = this%part_index(index_in)
     i = i + 1
     if ( this%pick_extra_outputs() > 0 ) then
      particles(i) = this%data_output(index_in)
     end if
    end if
   end select
  end subroutine

!DIR$ ATTRIBUTES INLINE:: call_particle_bounds
  subroutine call_particle_bounds( this, particles, lb, ub, tracking)
   class(base_species_T), intent(in) :: this
   real(dp), dimension(:, :), intent(inout) :: particles
   integer, intent(in) :: lb, ub
   logical, intent(in), optional :: tracking
   logical :: track_out
   integer :: i

   track_out = .false.
   if ( present(tracking) ) then
    track_out = tracking
   end if
   i = 1

   select case(this%pick_dimensions())
   case(1)
    particles(1:(ub-lb+1), i) = this%x(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%px(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%weight(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%gamma_inv(lb:ub)
    i = i + 1
    if ( track_out ) then
     particles(1:(ub-lb+1), i) = this%part_index(lb:ub)
     i = i + 1
     if ( this%pick_extra_outputs() > 0 ) then
      particles(1:(ub-lb+1), i) = this%data_output(lb:ub)
     end if
    end if
   case(2)
    particles(1:(ub-lb+1), i) = this%x(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%y(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%px(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%py(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%weight(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%gamma_inv(lb:ub)
    i = i + 1
    if ( track_out ) then
     particles(1:(ub-lb+1), i) = this%part_index(lb:ub)
     i = i + 1
     if ( this%pick_extra_outputs() > 0 ) then
      particles(1:(ub-lb+1), i) = this%data_output(lb:ub)
     end if
    end if
   case(3)
    particles(1:(ub-lb+1), i) = this%x(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%y(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%z(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%px(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%py(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%pz(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%weight(lb:ub)
    i = i + 1
    particles(1:(ub-lb+1), i) = this%gamma_inv(lb:ub)
    i = i + 1
    if ( track_out ) then
     particles(1:(ub-lb+1), i) = this%part_index(lb:ub)
     i = i + 1
     if ( this%pick_extra_outputs() > 0 ) then
      particles(1:(ub-lb+1), i) = this%data_output(lb:ub)
     end if
    end if
   end select
  end subroutine

!DIR$ ATTRIBUTES INLINE:: call_particle_index_array
  subroutine call_particle_index_array( this, particles, index_in, tracking)
   class(base_species_T), intent(in) :: this
   real(dp), dimension(:, :), intent(inout) :: particles
   integer, dimension(:), intent(in) :: index_in
   logical, intent(in), optional :: tracking
   integer :: n, size_ind, idx, k
   logical :: track_out

   track_out = .false.
   if ( present(tracking) ) then
    track_out = tracking
   end if

   size_ind = SIZE(index_in, DIM=1)
   k = 1
   if ( track_out ) then
    if ( this%pick_extra_outputs() > 0 ) then
     select case(this%pick_dimensions())
     case(1)
      do n = 1, size_ind
       idx = index_in(n)
       particles(k, 1) = this%x(idx)
       particles(k, 2) = this%px(idx)
       particles(k, 3) = this%weight(idx)
       particles(k, 4) = this%gamma_inv(idx)
       particles(k, 5) = this%part_index(idx)
       particles(k, 6) = this%data_output(idx)
       k = k + 1
      end do
     case(2)
      do n = 1, size_ind
       idx = index_in(n)
       particles(k, 1) = this%x(idx)
       particles(k, 2) = this%y(idx)
       particles(k, 3) = this%px(idx)
       particles(k, 4) = this%py(idx)
       particles(k, 5) = this%weight(idx)
       particles(k, 6) = this%gamma_inv(idx)
       particles(k, 7) = this%part_index(idx)
       particles(k, 8) = this%data_output(idx)
       k = k + 1 
      end do
     case(3)
      do n = 1, size_ind
       idx = index_in(n)
       particles(k, 1) = this%x(idx)
       particles(k, 2) = this%y(idx)
       particles(k, 3) = this%z(idx)
       particles(k, 4) = this%px(idx)
       particles(k, 5) = this%py(idx)
       particles(k, 6) = this%pz(idx)
       particles(k, 7) = this%weight(idx)
       particles(k, 8) = this%gamma_inv(idx)
       particles(k, 9) = this%part_index(idx)
       particles(k, 10) = this%data_output(idx)
       k = k + 1
      end do
     end select
    else
     select case(this%pick_dimensions())
     case(1)
      do n = 1, size_ind
       idx = index_in(n)
       particles(k, 1) = this%x(idx)
       particles(k, 2) = this%px(idx)
       particles(k, 3) = this%weight(idx)
       particles(k, 4) = this%gamma_inv(idx)
       particles(k, 5) = this%part_index(idx)
       k = k + 1
      end do
     case(2)
      do n = 1, size_ind
       idx = index_in(n)
       particles(k, 1) = this%x(idx)
       particles(k, 2) = this%y(idx)
       particles(k, 3) = this%px(idx)
       particles(k, 4) = this%py(idx)
       particles(k, 5) = this%weight(idx)
       particles(k, 6) = this%gamma_inv(idx)
       particles(k, 7) = this%part_index(idx)
       k = k + 1 
      end do
     case(3)
      do n = 1, size_ind
       idx = index_in(n)
       particles(k, 1) = this%x(idx)
       particles(k, 2) = this%y(idx)
       particles(k, 3) = this%z(idx)
       particles(k, 4) = this%px(idx)
       particles(k, 5) = this%py(idx)
       particles(k, 6) = this%pz(idx)
       particles(k, 7) = this%weight(idx)
       particles(k, 8) = this%gamma_inv(idx)
       particles(k, 9) = this%part_index(idx)
       k = k + 1
      end do
     end select
    end if
   else
    select case(this%pick_dimensions())
    case(1)
     do n = 1, size_ind
      idx = index_in(n)
      particles(k, 1) = this%x(idx)
      particles(k, 2) = this%px(idx)
      particles(k, 3) = this%weight(idx)
      particles(k, 4) = this%gamma_inv(idx)
      k = k + 1
     end do
    case(2)
     do n = 1, size_ind
      idx = index_in(n)
      particles(k, 1) = this%x(idx)
      particles(k, 2) = this%y(idx)
      particles(k, 3) = this%px(idx)
      particles(k, 4) = this%py(idx)
      particles(k, 5) = this%weight(idx)
      particles(k, 6) = this%gamma_inv(idx)
      k = k + 1 
     end do
    case(3)
     do n = 1, size_ind
      idx = index_in(n)
      particles(k, 1) = this%x(idx)
      particles(k, 2) = this%y(idx)
      particles(k, 3) = this%z(idx)
      particles(k, 4) = this%px(idx)
      particles(k, 5) = this%py(idx)
      particles(k, 6) = this%pz(idx)
      particles(k, 7) = this%weight(idx)
      particles(k, 8) = this%gamma_inv(idx)
      k = k + 1
     end do
    end select
   end if
  end subroutine

  subroutine compute_gamma( this, pond_pot )
   class(base_species_T), intent(inout) :: this
   real(dp), intent(in), optional :: pond_pot(:)
   integer :: np

   np = this%how_many()
   if ( this%empty ) return

   call realloc_temp_1d( bs_temp_1d, np )
   bs_temp_1d(1:np) = zero_dp

   if ( this%allocated_px ) then
    bs_temp_1d(1:np) = bs_temp_1d(1:np) + this%px(1:np)*this%px(1:np)
   end if

   if ( this%allocated_py ) then
    bs_temp_1d(1:np) = bs_temp_1d(1:np) + this%py(1:np)*this%py(1:np)
   end if

   if ( this%allocated_pz ) then
    bs_temp_1d(1:np) = bs_temp_1d(1:np) + this%pz(1:np)*this%pz(1:np)
   end if

   if( present( pond_pot ) ) then
    bs_temp_1d(1:np) = bs_temp_1d(1:np) + pond_pot(1:np)
   end if

   bs_temp_1d(1:np) = one_dp/sqrt(one_dp + bs_temp_1d(1:np))
   call assign(this%gamma_inv, bs_temp_1d(1:np), 1, np, np)

  end subroutine

  pure function array_size( this ) result( number )
   class(base_species_T), intent(in) :: this
   integer :: number

   if ( this%allocated_x ) then
    number = SIZE( this%x(:) )
   else if ( this%allocated_y ) then
    number = SIZE( this%y(:) )
   else if ( this%allocated_z ) then
    number = SIZE( this%z(:) )
   else if ( this%allocated_px ) then
    number = SIZE( this%px(:) )
   else if ( this%allocated_py ) then
    number = SIZE( this%py(:) )
   else if ( this%allocated_pz ) then
    number = SIZE( this%pz(:) )
   else if ( this%allocated_gamma ) then
    number = SIZE( this%gamma_inv(:) )
   else if ( this%allocated_index ) then
    number = SIZE( this%part_index(:) )
   else if ( this%allocated_weight ) then
    number = SIZE( this%weight(:) )
   else
    number = 0
   end if

  end function

!DIR$ ATTRIBUTES INLINE:: copy_all
  subroutine copy_all( this, other )
   class(base_species_T), intent(inout) :: this
   class(base_species_T), intent(in) :: other
   integer :: tot

   tot = other%how_many()

   call this%reallocate(tot, other%pick_properties())
   call this%set_properties(other%pick_properties())
   call this%set_part_number(tot)

   if (other%allocated_x) then
    call assign(this%x, other%x(1:tot), 1, tot)
   end if
   if (other%allocated_y) then
    call assign(this%y, other%y(1:tot), 1, tot)
   end if
   if (other%allocated_z) then
    call assign(this%z, other%z(1:tot), 1, tot)
   end if
   if (other%allocated_px) then
    call assign(this%px, other%px(1:tot), 1, tot)
   end if
   if (other%allocated_py) then
    call assign(this%py, other%py(1:tot), 1, tot)
   end if
   if (other%allocated_pz) then
    call assign(this%pz, other%pz(1:tot), 1, tot)
   end if
   if (other%allocated_gamma) then
    call assign(this%gamma_inv, other%gamma_inv(1:tot), 1, tot)
   end if
   if (other%allocated_weight) then
    call assign(this%weight, other%weight(1:tot), 1, tot)
   end if
   if (other%allocated_index) then
    call assign(this%part_index, other%part_index(1:tot), 1, tot)
   end if
   if (other%allocated_data_out) then
    call assign(this%data_output, other%data_output(1:tot), 1, tot)
   end if
  end subroutine

!DIR$ ATTRIBUTES INLINE:: copy_boundaries
  subroutine copy_boundaries( this, other, lower_bound, upper_bound )
   class(base_species_T), intent(inout) :: this
   class(base_species_T), intent(in) :: other
   integer, intent(in) :: lower_bound, upper_bound
   integer :: tot

   tot = upper_bound - lower_bound + 1

   call this%reallocate(tot, other%pick_properties())
   call this%set_properties(other%pick_properties())
   call this%set_part_number(tot)

   if (other%allocated_x) then
    call assign(this%x, other%x(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_y) then
    call assign(this%y, other%y(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_z) then
    call assign(this%z, other%z(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_px) then
    call assign(this%px, other%px(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_py) then
    call assign(this%py, other%py(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_pz) then
    call assign(this%pz, other%pz(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_gamma) then
    call assign(this%gamma_inv, other%gamma_inv(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_weight) then
    call assign(this%weight, other%weight(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_index) then
    call assign(this%part_index, other%part_index(lower_bound:upper_bound), 1, tot)
   end if
   if (other%allocated_data_out) then
    call assign(this%data_output, other%data_output(lower_bound:upper_bound), 1, tot)
   end if
  end subroutine

  pure function how_many_scalars( this ) result(n_parts)
   class(scalars), intent(in) :: this
   integer :: n_parts

   n_parts = this%n_part
  
  end function

!DIR$ ATTRIBUTES INLINE:: how_many
  pure function how_many( this ) result(n_parts)
   !! Number of particles in the species
   class(base_species_T), intent(in) :: this
   integer :: n_parts

   n_parts = this%properties%n_part
  
  end function

!DIR$ ATTRIBUTES INLINE:: flatten_into
  subroutine flatten_into( this, flat_array)
   class(base_species_T), intent(in) :: this
   real(dp), allocatable, dimension(:), intent(inout) :: flat_array
   integer :: array_sz, num_comps, pack_size, lb

   array_sz = this%how_many()
   num_comps = this%total_size()
   pack_size = array_sz*num_comps
   call realloc_temp_1d( flat_array, pack_size)

   lb = 1
   if( this%allocated_x ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%x(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_y ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%y(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_z ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%z(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_px ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%px(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_py ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%py(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_pz ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%pz(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_gamma ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%gamma_inv(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_weight ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%weight(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_index ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%part_index(1:array_sz)
    lb = lb + array_sz
   end if
   if( this%allocated_data_out ) then
    flat_array( lb:(lb + array_sz - 1) ) = this%data_output(1:array_sz)
    lb = lb + array_sz
   end if

  end subroutine

  subroutine initialize_data( this, x_arr, y_arr, z_arr, &
   weightx_arr, weightyz_arr, loc_x, loc_y, loc_z)
   class( base_species_T ), intent(inout) :: this
   real(dp), dimension(:), intent(in) :: x_arr, y_arr, z_arr
   real(dp), dimension(:), intent(in) :: weightx_arr
   real(dp), dimension(:, :), intent(in) :: weightyz_arr
   integer, intent(in) :: loc_x, loc_y, loc_z

   call this%add_data( x_arr, y_arr, z_arr, &
   weightx_arr, weightyz_arr, 1, loc_x, loc_y, loc_z, 0)

  end subroutine

!DIR$ ATTRIBUTES INLINE:: pack_into_arrays
  subroutine pack_into_array( this, packed, mask, new_np )
   class(base_species_T), intent(in) :: this
   class(base_species_T), intent(inout) :: packed
   logical, intent(in) :: mask(:)
   integer, intent(in) :: new_np
   integer :: np
 
   np = this%how_many()
   call packed%reallocate(new_np, this%pick_properties())
   call packed%set_properties(this%pick_properties())
 
   if( this%allocated_x ) then
    packed%x(1:new_np) = PACK( this%x(1:np), mask(1:np) )
   end if
   if( this%allocated_y ) then
    packed%y(1:new_np) = PACK( this%y(1:np), mask(1:np) )
   end if
   if( this%allocated_z ) then
    packed%z(1:new_np) = PACK( this%z(1:np), mask(1:np) )
   end if
   if( this%allocated_px ) then
    packed%px(1:new_np) = PACK( this%px(1:np), mask(1:np) )
   end if
   if( this%allocated_py ) then
    packed%py(1:new_np) = PACK( this%py(1:np), mask(1:np) )
   end if
   if( this%allocated_pz ) then
    packed%pz(1:new_np) = PACK( this%pz(1:np), mask(1:np) )
   end if
   if( this%allocated_gamma ) then
    packed%gamma_inv(1:new_np) = PACK( this%gamma_inv(1:np), mask(1:np) )
   end if
   if( this%allocated_weight ) then
    packed%weight(1:new_np) = PACK( this%weight(1:np), mask(1:np) )
   end if
   if( this%allocated_index ) then
    packed%part_index(1:new_np) = PACK( this%part_index(1:np), mask(1:np) )
   end if
   if( this%allocated_data_out ) then
    packed%data_output(1:new_np) = PACK( this%data_output(1:np), mask(1:np) )
   end if

   call packed%set_part_number(new_np)

  end subroutine

!DIR$ ATTRIBUTES INLINE:: redistribute
  subroutine redistribute( this, flat_array, num_particles, properties_in, aux_in )
   class(base_species_T), intent(inout) :: this
   real(dp), intent(in), dimension(:) :: flat_array
   integer, intent(in) :: num_particles
   type(scalars), intent(in) :: properties_in
   logical, intent(in), optional :: aux_in
   integer :: i
   logical :: aux

   i = 0
   call this%reallocate(num_particles, properties_in%pick_properties())
   call this%set_properties( properties_in )
   call this%set_part_number( num_particles )
   if ( num_particles == 0 ) then
    return
   end if

   aux = .false.
   if ( PRESENT(aux_in) ) then
    aux = aux_in
   end if
   if( this%allocated_x ) then
    this%x(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_y ) then
    this%y(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_z ) then
    this%z(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_px ) then
    this%px(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_py ) then
    this%py(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_pz ) then
    this%pz(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_gamma ) then
    this%gamma_inv(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_weight ) then
    this%weight(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
   if( this%allocated_index ) then
    this%part_index(1:num_particles) = int(flat_array((i + 1): (i + num_particles)))
    i = i + num_particles
   end if
   if( this%allocated_data_out ) then
    this%data_output(1:num_particles) = flat_array((i + 1): (i + num_particles))
    i = i + num_particles
   end if
 
   if ( aux ) then
    call write_warning("Called aux routine for non aux species")
   end if
  end subroutine

  subroutine reallocate(this, n_parts, properties_in)
   class(base_species_T), intent(inout) :: this
   integer, intent(in) :: n_parts
   type (scalars), intent(in) :: properties_in
   type (scalars) :: old_properties

   if ( this%initialized ) then
    if (this%array_size() < n_parts ) then
     old_properties = this%pick_properties()
     call this%sweep()
     call this%new_species(n_parts, old_properties%pick_dimensions(), &
      tracked=old_properties%istracked(), extra_outputs=old_properties%pick_extra_outputs() )
     call this%set_properties(old_properties)
    end if
   else
    call this%new_species(n_parts, properties_in%pick_dimensions(), &
     tracked=properties_in%istracked(), extra_outputs=properties_in%pick_extra_outputs() )
   end if

  end subroutine

  pure function return_gamma( this, lb, ub ) result(gam)
   class(base_species_T), intent(in) :: this
   integer, intent(in), optional :: lb, ub
   real(dp), allocatable, dimension(:) :: gam
   integer :: lowb, upb

   lowb = 1
   if ( present(lb) ) then
    lowb = lb
   end if
   upb = this%how_many()
   if ( present(ub) ) then
    upb = ub
   end if

   gam = one_dp/this%gamma_inv(lowb:upb)

  end function

  pure function pick_charge( this ) result(ch)
   class(base_species_T), intent(in) :: this
   real(dp) :: ch

   ch = this%properties%charge

  end function

  pure function pick_extra_outputs( this ) result(n_outputs)
   class(base_species_T), intent(in) :: this
   integer :: n_outputs

   n_outputs = this%properties%track_data%extra_outputs

  end function
  
  pure function pick_temperature( this ) result(tem)
   class(base_species_T), intent(in) :: this
   real(dp) :: tem

   tem = this%properties%temperature

  end function

  pure function pick_dimensions( this ) result(dims)
   class(base_species_T), intent(in) :: this
   integer :: dims

   dims = this%properties%dimensions

  end function

  pure function pick_name( this ) result(name)
   class(base_species_T), intent(in) :: this
   character(len=100) :: name

   name = this%properties%name

  end function
  
  function pick_properties( this ) result(properties)
   class(base_species_T), intent(in) :: this
   type(scalars) :: properties

   call properties%set_charge(this%pick_charge())
   call properties%set_temperature(this%pick_temperature())
   call properties%set_dimensions(this%pick_dimensions())
   call properties%track(this%istracked())
   call properties%set_extra_outputs(this%pick_extra_outputs())

  end function
  
  pure function pick_tot_tracked_parts( this ) result(n_tracked)
   class(base_species_T), intent(in) :: this
   type(track_data_t) :: track_data
   integer :: n_tracked

   track_data = this%pick_track_params()
   n_tracked = track_data%n_tracked

  end function

  subroutine set_properties( this, properties_in )
   class(base_species_T), intent(inout) :: this
   type(scalars), intent(in) :: properties_in
   integer :: size_array

   call this%set_charge(properties_in%pick_charge())
   call this%set_temperature(properties_in%pick_temperature())
   call this%set_dimensions(properties_in%pick_dimensions())
   call this%track(properties_in%istracked())
   size_array = this%array_size()
   call this%set_extra_outputs(properties_in%pick_extra_outputs(), size_array)

  end subroutine

  subroutine set_charge_int( this, ch)
   class(base_species_T), intent(inout) :: this
   integer, intent(in) :: ch
   
   this%properties%charge = real(ch, dp)

  end subroutine
  
  subroutine set_charge_real( this, ch)
   class(base_species_T), intent(inout) :: this
   real(dp), intent(in) :: ch
   
   this%properties%charge = ch

  end subroutine
  
  subroutine set_dimensions( this, dimens)
   class(base_species_T), intent(inout) :: this
   integer, intent(in) :: dimens
   
   this%properties%dimensions = dimens

  end subroutine

  subroutine set_extra_outputs( this, n_outputs, size_array )
   class(base_species_T), intent(inout) :: this
   integer, intent(in) :: n_outputs, size_array
   integer :: n_outputs_old

   n_outputs_old = this%properties%track_data%extra_outputs

   this%properties%track_data%extra_outputs = n_outputs

   if ( (this%properties%track_data%extra_outputs /= n_outputs_old) .and. &
        (size_array > 0) ) then
    call this%allocate_data_output( size_array )
   end if
   if ( (this%properties%track_data%extra_outputs > 0) .and. &
    (.not. this%allocated_data_out) .and. (size_array > 0) ) then
    call this%allocate_data_output( size_array )
   end if

  end subroutine

  subroutine set_name( this, name)
   class(base_species_T), intent(inout) :: this
   character(len=*), intent(in) :: name
   
   this%properties%name = name

  end subroutine

  subroutine set_part_number( this, n_parts)
   class(base_species_T), intent(inout) :: this
   integer, intent(in) :: n_parts

   this%properties%n_part = n_parts
   if ( n_parts > 0 ) then
    this%empty = .false.
   else if (n_parts == 0) then
    this%empty = .true. 
   else
    write(6, *) 'Error in part number'
   end if
  end subroutine

  subroutine set_temperature( this, temperature)
   class(base_species_T), intent(inout) :: this
   real(dp), intent(in) :: temperature

   this%properties%temperature = real(temperature, sp)
  end subroutine

  subroutine track( this, track_flag, allocate_now )
   class(base_species_T), intent(inout) :: this
   logical, intent(in) :: track_flag
   logical, intent(in), optional :: allocate_now
   logical :: alloc

   alloc = .false.
   if ( present(allocate_now) ) then
    alloc = allocate_now
   end if

   this%properties%track_data%tracked = track_flag
   if (track_flag .and. (.not. this%allocated_index) .and. alloc ) then
    allocate( this%part_index(this%array_size()), source=0 )
    this%allocated_index = .true.
   end if

  end subroutine

  pure function ismobile( this ) result(mobile)
   !! True if particles move
   class(base_species_T), intent(in) :: this
   logical :: mobile

   mobile = this%properties%mobile

  end function

  pure function istest( this ) result(test)
   !! True if particles are test particles
   class(base_species_T), intent(in) :: this
   logical :: test
  
   test = this%properties%test
  
  end function
 
  pure function istracked( this ) result(tracked)
   !! True if particles are tracked
   class(base_species_T), intent(in) :: this
   logical :: tracked

   tracked = this%properties%track_data%tracked

  end function

  subroutine set_track_params( this, xmn, xmx, ymn, ymx, zmn, zmx, jmp)
   class(base_species_T), intent(inout) :: this
   real(dp), intent(in), optional :: xmn, xmx, ymn, ymx, zmn, zmx
   integer, intent(in), optional :: jmp

   if ( present(xmn) ) then
    allocate( this%properties%track_data%xmin, source=xmn)
   end if
   if ( present(xmx) ) then
    allocate( this%properties%track_data%xmax, source=xmx)
   end if
   if ( present(ymn) ) then
    allocate( this%properties%track_data%ymin, source=ymn)
   end if
   if ( present(ymx) ) then
    allocate( this%properties%track_data%ymax, source=ymx)
   end if
   if ( present(zmn) ) then
    allocate( this%properties%track_data%zmin, source=zmn)
   end if
   if ( present(zmx) ) then
    allocate( this%properties%track_data%zmax, source=zmx)
   end if
   if ( present(jmp) ) then
    this%properties%track_data%jump = jmp
   else
    this%properties%track_data%jump = 1
   end if

  end subroutine

  pure function pick_track_params( this ) result(track_out)
   class(base_species_T), intent(in) :: this
   type(track_data_t) :: track_out

   track_out%n_tracked = this%properties%track_data%n_tracked
   if ( allocated(this%properties%track_data%xmin) ) then
    track_out%xmin = this%properties%track_data%xmin
   end if
   if ( allocated(this%properties%track_data%xmax) ) then
    track_out%xmax = this%properties%track_data%xmax
   end if
   if ( allocated(this%properties%track_data%ymin) ) then
    track_out%ymin = this%properties%track_data%ymin
   end if
   if ( allocated(this%properties%track_data%ymax) ) then
    track_out%ymax = this%properties%track_data%ymax
   end if
   if ( allocated(this%properties%track_data%zmin) ) then
    track_out%zmin = this%properties%track_data%zmin
   end if
   if ( allocated(this%properties%track_data%zmax) ) then
    track_out%zmax = this%properties%track_data%zmax
   end if
   track_out%jump = this%properties%track_data%jump
   track_out%highest_index = this%properties%track_data%highest_index
   track_out%extra_outputs = this%properties%track_data%extra_outputs

  end function

  pure function properties_array_size( ) result(psize)
  !! This function returns the size of the array (number of doubles) generated when calling the
  !! properties dump to array. It MUST be updated if properties are changed
   integer :: psize

   psize = 22

  end function

  subroutine set_mobile( this, mobile )
    class(base_species_T), intent(inout) :: this
    logical, intent(in) :: mobile

    this%properties%mobile = mobile
  end

  subroutine set_tot_tracked_parts( this, n_tracked )
   class(base_species_T), intent(inout) :: this
   integer, intent(in) :: n_tracked

   this%properties%track_data%n_tracked = n_tracked

  end subroutine

  pure function count_tracked_particles( this ) result(n_tracked)
   class(base_species_T), intent(in) :: this
   integer :: n_tracked
   integer :: np

   n_tracked = 0
   if ( this%empty .or. ( .not. this%istracked() ) ) return
   np = this%how_many()
   n_tracked = COUNT( int(this%call_component(INDEX_COMP, lb=1, ub=np)) > 0 )

  end function

  subroutine set_highest_track_index( this, max_index )
   class(base_species_T), intent(inout) :: this
   integer, intent(in) :: max_index

   this%properties%track_data%highest_index = max_index

  end subroutine

  subroutine check_tracking( this )
   class(base_species_T), intent(inout) :: this
   integer :: np_track
   integer :: max_index

   if ( .not. this%istracked() ) return

   if ( this%empty ) then
    call this%set_tot_tracked_parts(0)
    return
   end if

   np_track = this%count_tracked_particles()
   if ( np_track == 0 ) then
    call this%set_highest_track_index(0)
    call this%set_tot_tracked_parts(0)
   else
    max_index = MAXVAL( int(this%call_component(INDEX_COMP)) )
    call this%set_highest_track_index(max_index)
    call this%set_tot_tracked_parts(np_track)
   end if

  end subroutine

  pure function total_size( this ) result(size)
   class(base_species_T), intent(in) :: this
   integer :: size

   if (this%istracked()) then
    if ( .not. this%pick_extra_outputs() > 0 ) then
     select case(this%pick_dimensions())
     case(1)
      size = 5
     case(2)
      size = 7
     case(3)
      size = 9
     case default
      size = -1
     end select
    else
     select case(this%pick_dimensions())
     case(1)
      size = 6
     case(2)
      size = 8
     case(3)
      size = 10
     case default
      size = -1
     end select
    end if
   else
    select case(this%pick_dimensions())
    case(1)
     size = 4
    case(2)
     size = 6
    case(3)
     size = 8
    case default
     size = -1
    end select
   end if

  end function

!==== Procedures for the track_data type ====
  !===== Sweep track_data type =======

  subroutine sweep_track_data( this )
   class(track_data_t), intent(inout) :: this

   this%tracked = .false.
   this%extra_outputs = 0

  end subroutine
    
  !==== Type bound procedures ====

  subroutine dump_tracking_to_array(this, array_out)
   class(base_species_T), intent(in) :: this
   real(dp), dimension(:), allocatable, intent(inout) :: array_out
   integer :: i, len_array
   integer, parameter :: len_max = 30
   real(dp) :: temp_array(len_max)
   ! We store the bool true components as real one_dp
   ! and false as zero_dp

   i = 1
   if (this%properties%track_data%tracked) then
    temp_array(i) = one_dp
    i = i + 1
   else
    temp_array(i) = zero_dp
    i = i + 1
   end if
   
   temp_array(i) = real(this%properties%track_data%n_tracked, dp)
   i = i + 1
   
   if ( allocated(this%properties%track_data%xmin) ) then
    temp_array(i) = one_dp
    i = i + 1
    temp_array(i) = this%properties%track_data%xmin
    i = i + 1
   else
    temp_array(i) = zero_dp
    i = i + 1
    temp_array(i) = zero_dp
    i = i + 1
   end if
   
   if ( allocated(this%properties%track_data%xmax) ) then
    temp_array(i) = one_dp
    i = i + 1
    temp_array(i) = this%properties%track_data%xmax
    i = i + 1
   else
    temp_array(i) = zero_dp
    i = i + 1
    temp_array(i) = zero_dp
    i = i + 1
   end if
   
   if ( allocated(this%properties%track_data%ymin) ) then
    temp_array(i) = one_dp
    i = i + 1
    temp_array(i) = this%properties%track_data%ymin
    i = i + 1
   else
    temp_array(i) = zero_dp
    i = i + 1
    temp_array(i) = zero_dp
    i = i + 1
   end if
   
   if ( allocated(this%properties%track_data%ymax) ) then
    temp_array(i) = one_dp
    i = i + 1
    temp_array(i) = this%properties%track_data%ymax
    i = i + 1
   else
    temp_array(i) = zero_dp
    i = i + 1
    temp_array(i) = zero_dp
    i = i + 1
   end if
   
   if ( allocated(this%properties%track_data%zmin) ) then
    temp_array(i) = one_dp
    i = i + 1
    temp_array(i) = this%properties%track_data%zmin
    i = i + 1
   else
    temp_array(i) = zero_dp
    i = i + 1
    temp_array(i) = zero_dp
    i = i + 1
   end if
   
   if ( allocated(this%properties%track_data%zmax) ) then
    temp_array(i) = one_dp
    i = i + 1
    temp_array(i) = this%properties%track_data%zmax
    i = i + 1
   else
    temp_array(i) = zero_dp
    i = i + 1
    temp_array(i) = zero_dp
    i = i + 1
   end if

   temp_array(i) = real(this%properties%track_data%jump, dp)
   i = i + 1

   temp_array(i) = real(this%properties%track_data%highest_index, dp)
   i = i + 1

   temp_array(i) = real(this%properties%track_data%extra_outputs, dp)
   
   len_array = i

   if ( allocated(array_out) ) then
    deallocate(array_out)
   end if
   allocate( array_out(len_array) )
   array_out(1:len_array) = temp_array(1:len_array)

  end subroutine

  subroutine read_tracking_from_array(this, array_in, np)
   class(base_species_T), intent(inout) :: this
   real(dp), dimension(:), intent(in) :: array_in
   integer, intent(in) :: np
   integer :: i

   i = 1

   if ( int(array_in(i)) == 0 ) then
    call this%track(.false., allocate_now=.false.)
   else
    call this%track(.true., allocate_now=.true.)
   end if
   i = i + 1

   this%properties%track_data%n_tracked = int(array_in(i))
   i = i + 1

   ! Allocation and assignment of xmin - xmax
   if ( int(array_in(i)) == 0 ) then
    if ( allocated(this%properties%track_data%xmin) ) then
     deallocate(this%properties%track_data%xmin)
    end if
    i = i + 2
   else
    i = i + 1
    if ( .not. allocated(this%properties%track_data%xmin) ) then
     allocate( this%properties%track_data%xmin )
    end if
    this%properties%track_data%xmin = array_in(i)
    i = i + 1
   end if

   if ( int(array_in(i)) == 0 ) then
    if ( allocated(this%properties%track_data%xmax) ) then
     deallocate(this%properties%track_data%xmax)
    end if
    i = i + 2
   else
    i = i + 1
    if ( .not. allocated(this%properties%track_data%xmax) ) then
     allocate( this%properties%track_data%xmax )
    end if
    this%properties%track_data%xmax = array_in(i)
    i = i + 1
   end if

   ! Allocation and assignment of ymin - ymax
   if ( int(array_in(i)) == 0 ) then
    if ( allocated(this%properties%track_data%ymin) ) then
     deallocate(this%properties%track_data%ymin)
    end if
    i = i + 2
   else
    i = i + 1
    if ( .not. allocated(this%properties%track_data%ymin) ) then
     allocate( this%properties%track_data%ymin )
    end if
    this%properties%track_data%ymin = array_in(i)
    i = i + 1
   end if

   if ( int(array_in(i)) == 0 ) then
    if ( allocated(this%properties%track_data%ymax) ) then
     deallocate(this%properties%track_data%ymax)
    end if
    i = i + 2
   else
    i = i + 1
    if ( .not. allocated(this%properties%track_data%ymax) ) then
     allocate( this%properties%track_data%ymax )
    end if
    this%properties%track_data%ymax = array_in(i)
    i = i + 1
   end if

   ! Allocation and assignment of zmin - zmax
   if ( int(array_in(i)) == 0 ) then
    if ( allocated(this%properties%track_data%zmin) ) then
     deallocate(this%properties%track_data%zmin)
    end if
    i = i + 2
   else
    i = i + 1
    if ( .not. allocated(this%properties%track_data%zmin) ) then
     allocate( this%properties%track_data%zmin )
    end if
    this%properties%track_data%zmin = array_in(i)
    i = i + 1
   end if

   if ( int(array_in(i)) == 0 ) then
    if ( allocated(this%properties%track_data%zmax) ) then
     deallocate(this%properties%track_data%zmax)
    end if
    i = i + 2
   else
    i = i + 1
    if ( .not. allocated(this%properties%track_data%zmax) ) then
     allocate( this%properties%track_data%zmax )
    end if
    this%properties%track_data%zmax = array_in(i)
    i = i + 1
   end if

   this%properties%track_data%jump = int(array_in(i))
   i = i + 1

   this%properties%track_data%highest_index = int(array_in(i))
   i = i + 1

   call this%set_extra_outputs( int(array_in(i)), np)

  end subroutine
!==== Procedures for the scalar type ====

  !===== Sweep scalar type =======
  subroutine sweep_scalars( this )
   class(scalars), intent(inout) :: this

   this%charge = -1
   this%dimensions = 2
   this%n_part = 0
   this%temperature = 0
   call this%track_data%sweep()

  end subroutine

  !==== Type bound procedures ====
    
  subroutine dump_properties_to_array(this, array_out)
   class(base_species_T), intent(in) :: this
   real(dp), allocatable, dimension(:), intent(inout) :: array_out
   integer :: i, size_track
   integer, parameter :: len_max = 30
   real(dp) :: temp_array(len_max)
   real(dp), allocatable, dimension(:) :: track_data_array

   i = 1

   temp_array(i) = this%properties%charge
   i = i + 1

   temp_array(i) = real(this%properties%n_part, dp)
   i = i + 1

   temp_array(i) = real(this%properties%dimensions, dp)
   i = i + 1

   temp_array(i) = real(this%properties%temperature, dp)
   i = i + 1

   if (this%properties%mobile) then
    temp_array(i) = one_dp
   else
    temp_array(i) = zero_dp
   end if
 
   call this%dump_tracking_to_array(track_data_array)

   size_track = SIZE(track_data_array)
   if ( allocated(array_out) ) then
    deallocate(array_out)
   end if
   allocate(array_out(i + size_track))
   array_out(1:i) = temp_array(1:i)
   array_out(i+1:i+size_track) = track_data_array(1:size_track)

  end subroutine

  subroutine read_properties_from_array( this, array_in )
   class(base_species_T), intent(inout) :: this
   real(dp), dimension(:), intent(in) :: array_in
   integer i, ub

   i = 1
   ub = UBOUND(array_in, DIM=1)

   call this%set_charge(array_in(i))
   i = i + 1

   call this%set_part_number(int(array_in(i)))
   i = i + 1

   call this%set_dimensions(int(array_in(i)))
   i = i + 1

   call this%set_temperature(array_in(i))
   i = i + 1
   
   if (array_in(i) > 0) then
    call this%set_mobile(.true.)
   else
    call this%set_mobile(.false.)
   end if
   i = i + 1

   call this%read_tracking_from_array( array_in( i:ub ), this%how_many() )

  end subroutine

  pure function istracked_scalars( this ) result(tracked)
   class(scalars), intent(in) :: this
   logical :: tracked

   tracked = this%track_data%tracked

  end function

  pure function pick_charge_scalars( this ) result(ch)
   class(scalars), intent(in) :: this
   real(dp) :: ch

   ch = this%charge

  end function

  pure function pick_dimensions_scalars( this ) result(dims)
   class(scalars), intent(in) :: this
   integer :: dims

   dims = this%dimensions

  end function

  pure function pick_extra_outputs_scalars( this ) result(n_outputs)
   class(scalars), intent(in) :: this
   integer :: n_outputs

   n_outputs = this%track_data%extra_outputs

  end function

  pure function pick_name_scalars( this ) result(name)
   class(scalars), intent(in) :: this
   character(len=100) :: name

   name = this%name

  end function
  
  function pick_properties_scalars( this ) result(properties)
   class(scalars), intent(in) :: this
   type(scalars) :: properties

   call properties%set_charge(this%pick_charge())
   call properties%set_temperature(this%pick_temperature())
   call properties%set_dimensions(this%pick_dimensions())
   call properties%track(this%istracked())
   call properties%set_extra_outputs(this%pick_extra_outputs())

  end function

  pure function pick_temperature_scalars( this ) result(tem)
   class(scalars), intent(in) :: this
   real(dp) :: tem

   tem = this%temperature

  end function

  subroutine set_charge_scalars( this, ch)
   class(scalars), intent(inout) :: this
   real(dp), intent(in) :: ch
   
   this%charge = ch

  end subroutine
  
  subroutine set_dimensions_scalars( this, dimens)
   class(scalars), intent(inout) :: this
   integer, intent(in) :: dimens
   
   this%dimensions = dimens

  end subroutine

  subroutine set_extra_outputs_scalars( this, n_outputs )
   class(scalars), intent(inout) :: this
   integer, intent(in) :: n_outputs

   this%track_data%extra_outputs = n_outputs

  end subroutine

  subroutine set_name_scalars( this, name)
   class(scalars), intent(inout) :: this
   character(len=*), intent(in) :: name
   
   this%name = name

  end subroutine

  subroutine set_part_number_scalars( this, n_parts)
   class(scalars), intent(inout) :: this
   integer, intent(in) :: n_parts

   this%n_part = n_parts

  end subroutine

  subroutine set_temperature_scalars( this, temperature)
   class(scalars), intent(inout) :: this
   real(dp), intent(in) :: temperature
   
   this%temperature = real(temperature, sp)
  end subroutine
  
  subroutine track_scalars( this, track_flag )
   class(scalars), intent(inout) :: this
   logical, intent(in) :: track_flag

   this%track_data%tracked = track_flag

  end subroutine
!==== Procedures not bound to type ======

 subroutine assign_real_realdp( array, values, lb, ub, n_parts)
  real(dp), allocatable, dimension(:), intent(inout) :: array
  real(dp), dimension(:), intent(in)  :: values
  integer, intent(in)  :: lb, ub
  integer, intent(in), optional  :: n_parts
  integer :: size_value, np

  np = 0
  if ( present(n_parts) ) then
   np = n_parts
  end if

  size_value = SIZE(values, DIM=1)
  if ( (ub - lb + 1) > size_value ) then
   write( 6, *) 'Assigning wrong value size'
  end if

  ! if ( .not. allocated(array) ) then
  !  allocate( array(np) )
  ! end if

  array(lb:ub) = values(:)

 end subroutine

 subroutine assign_integer_realdp( array, values, lb, ub, n_parts)
  real(dp), allocatable, dimension(:), intent(inout) :: array
  integer, dimension(:), intent(in)  :: values
  integer, intent(in)  :: lb, ub
  integer, intent(in), optional  :: n_parts
  integer :: size_value, np

  np = 0
  if ( present(n_parts) ) then
   np = n_parts
  end if

  size_value = SIZE(values, DIM=1)
  if ( (ub - lb + 1) > size_value ) then
   write( 6, *) 'Assigning wrong value size'
  end if

  ! if ( .not. allocated(array) ) then
  !  allocate( array(np) )
  ! end if

  array(lb:ub) = real(values(:), dp)

 end subroutine

 subroutine assign_real_realsp( array, values, lb, ub, n_parts)
  real(sp), allocatable, dimension(:), intent(inout) :: array
  real(dp), dimension(:), intent(in)  :: values
  integer, intent(in)  :: lb, ub
  integer, intent(in), optional  :: n_parts
  integer :: size_value, np

  np = 0
  if ( present(n_parts) ) then
   np = n_parts
  end if

  size_value = SIZE(values, DIM=1)
  if ( (ub - lb + 1) > size_value ) then
   write( 6, *) 'Assigning wrong value size'
  end if

  ! if ( .not. allocated(array) ) then
  !  allocate( array(np) )
  ! end if

  array(lb:ub) = real(values(:), sp)

 end subroutine

 subroutine assign_realsp_realsp( array, values, lb, ub, n_parts)
  real(sp), allocatable, dimension(:), intent(inout) :: array
  real(sp), dimension(:), intent(in)  :: values
  integer, intent(in)  :: lb, ub
  integer, intent(in), optional  :: n_parts
  integer :: size_value, np

  np = 0
  if ( present(n_parts) ) then
   np = n_parts
  end if

  size_value = SIZE(values, DIM=1)
  if ( (ub - lb + 1) > size_value ) then
   write( 6, *) 'Assigning wrong value size'
  end if

  ! if ( .not. allocated(array) ) then
  !  allocate( array(np) )
  ! end if

  array(lb:ub) = values(:)

 end subroutine

 subroutine assign_integer_realsp( array, values, lb, ub, n_parts)
  real(sp), allocatable, dimension(:), intent(inout) :: array
  integer, dimension(:), intent(in)  :: values
  integer, intent(in)  :: lb, ub
  integer, intent(in), optional  :: n_parts
  integer :: size_value, np

  np = 0
  if ( present(n_parts) ) then
   np = n_parts
  end if

  size_value = SIZE(values, DIM=1)
  if ( (ub - lb + 1) > size_value ) then
   write( 6, *) 'Assigning wrong value size'
  end if

  ! if ( .not. allocated(array) ) then
  !  allocate( array(np) )
  ! end if

  array(lb:ub) = real(values(:), sp)

 end subroutine

 subroutine assign_real_integer( array, values, lb, ub, n_parts)
  integer, allocatable, dimension(:), intent(inout) :: array
  real(dp), dimension(:), intent(in)  :: values
  integer, intent(in)  :: lb, ub
  integer, intent(in), optional  :: n_parts
  integer :: size_value, np

  np = 0
  if ( present(n_parts) ) then
   np = n_parts
  end if

  size_value = SIZE(values, DIM=1)
  if ( (ub - lb + 1) > size_value ) then
   write( 6, *) 'Assigning wrong value size'
  end if

  ! if ( .not. allocated(array) ) then
  !  allocate( array(np) )
  ! end if

  array(lb:ub) = int(values(:))

 end subroutine

 subroutine assign_integer_integer( array, values, lb, ub, n_parts)
  integer, allocatable, dimension(:), intent(inout) :: array
  integer, dimension(:), intent(in)  :: values
  integer, intent(in)  :: lb, ub
  integer, intent(in), optional  :: n_parts
  integer :: size_value, np

  np = 0
  if ( present(n_parts) ) then
   np = n_parts
  end if

  size_value = SIZE(values, DIM=1)
  if ( (ub - lb + 1) > size_value ) then
   write( 6, *) 'Assigning wrong value size'
  end if

  ! if ( .not. allocated(array) ) then
  !  allocate( array(np) )
  ! end if

  array(lb:ub) = values(:)

 end subroutine

 subroutine check_array_1d( array, size_array )
  real(dp), allocatable, dimension(:), intent(inout) :: array
  integer, intent(in) :: size_array

  if ( allocated(array) ) then
   if ( SIZE(array) < size_array ) then
    deallocate(array)
    allocate(array(size_array))
   endif
  else
   allocate(array(size_array))
  end if
  
 end subroutine

 pure function component_dictionary( component ) result(cdir)
 !! Dictionary wrapper for send_recieve routines in parallel.F90
 !! that need the cdir parameter
  integer, intent(in) :: component
  integer :: cdir
  select case(component)
  case(X_COMP)
   cdir = 3
  case(Y_COMP)
   cdir = 1
  case(Z_COMP)
   cdir = 2
  case default
   cdir = -10
  end select
 end function

 pure function link_position_momentum( component ) result (pm_comp)
 !! Dictionary that gives back the corresponding position (momentum)
 !! when a momentum (position) is given, e.g.
 !!
 !! ```
 !!     comp = X_COMP
 !!
 !!     link_position_momentum( comp ) => gives PX_COMP
 !! ```
  integer, intent(in) :: component
  integer :: pm_comp
  select case(component)
  case(X_COMP)
   pm_comp = PX_COMP
  case(Y_COMP)
   pm_comp = PY_COMP
  case(Z_COMP)
   pm_comp = PZ_COMP
  case(PX_COMP)
   pm_comp = X_COMP
  case(PY_COMP)
   pm_comp = Y_COMP
  case(PZ_COMP)
   pm_comp = Z_COMP
  case default
   pm_comp = -10
  end select
 end function

 !===========================
 !DIR$ ATTRIBUTES INLINE :: realloc_temp_1d
 subroutine realloc_temp_1d(vdata, npt_new)
  real (dp), allocatable, intent (inout) :: vdata(:)
  integer, intent (in) :: npt_new
  integer :: allocstatus, deallocstatus

  if (allocated(vdata)) then
   if (SIZE(vdata,1) < npt_new) then
    deallocate (vdata, stat=deallocstatus)
    allocate (vdata(1:npt_new), stat=allocstatus)
   end if
  else
   allocate (vdata(1:npt_new), stat=allocstatus)
  end if
 end subroutine
 !===========================

 !DIR$ ATTRIBUTES INLINE :: realloc_temp_2d
 subroutine realloc_temp_2d(vdata, npt_new, n_comp)
  real (dp), allocatable, intent (inout) :: vdata(:, :)
  integer, intent (in) :: npt_new, n_comp
  integer :: allocstatus, deallocstatus

  if (allocated(vdata)) then
   if (SIZE(vdata, 1) < npt_new .or. SIZE(vdata, 2) < n_comp) then
    deallocate (vdata, stat=deallocstatus)
    allocate (vdata(npt_new, n_comp), stat=allocstatus)
   end if
  else
   allocate (vdata(npt_new, n_comp), stat=allocstatus)
  end if
 end subroutine
end module
