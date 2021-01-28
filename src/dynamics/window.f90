
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

 module window
  use common_param
  use fstruct_data
  use grid_param
  use memory_pool
  use mpi_field_interface
  use mpi_part_interface
  use pstruct_data
  use run_data_info, only: part_numbers
  use tracking, only: set_tracked_particle_index
  use util, only: init_random_seed, gasdev

  implicit none
  !===============================
  ! MOVING WINDOW SECTION
  !=============================

  interface add_particles
   module procedure add_particles_new
   module procedure add_particles_old
  end interface

  interface particles_inject
   module procedure particles_inject_new
   module procedure particles_inject_old
  end interface

  interface lp_window_xshift
   module procedure lp_window_xshift_new
   module procedure lp_window_xshift_old
  end interface

  interface comoving_coordinate
   module procedure comoving_coordinate_old
   module procedure comoving_coordinate_new
  end interface
 contains

  subroutine add_particles_new(spec_in, spec_aux_in, np, i1, i2, ic, mempool)
   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   integer, intent (in) :: np, i1, i2, ic
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: n, j2, k2, n_parts

   n = np
   k2 = loc_nptz(ic)
   j2 = loc_npty(ic)
   n_parts = 0

   select case( spec_in(ic)%pick_dimensions() )
   case(1)
    n_parts = (i2 - i1 + 1)
   case(2)
    n_parts = (i2 - i1 + 1)*j2
   case(3)
    n_parts = (i2 - i1 + 1)*j2*k2
   end select

   call init_random_seed(mype)

   if ( pex1 ) then
    call spec_in(ic)%add_data(xpt(:, ic), loc_ypt(:, ic), loc_zpt(:, ic), &
    wghpt(:, ic), loc_wghyz(:, :, ic), i1, i2, j2, k2, np)
   end if

   call set_tracked_particle_index( spec_in, np + 1, np + n_parts, ic, mempool)
   call spec_in(ic)%check_tracking()
   if( spec_in(ic)%istracked() .and. (.not. spec_in(ic)%empty) ) then
    select case (spec_in(ic)%pick_dimensions())
    case(1)
      call spec_aux_in(ic)%set_component(spec_in(ic)%px(np + 1:np + n_parts), &
        OLD_PX_COMP, lb=np + 1, ub=np + n_parts)
    case(2)
      call spec_aux_in(ic)%set_component(spec_in(ic)%px(np + 1:np + n_parts), &
        OLD_PX_COMP, lb=np + 1, ub=np + n_parts)
      call spec_aux_in(ic)%set_component(spec_in(ic)%py(np + 1:np + n_parts), &
        OLD_PY_COMP, lb=np + 1, ub=np + n_parts)
    case(3)
      call spec_aux_in(ic)%set_component(spec_in(ic)%px(np + 1:np + n_parts), &
        OLD_PX_COMP, lb=np + 1, ub=np + n_parts)
      call spec_aux_in(ic)%set_component(spec_in(ic)%py(np + 1:np + n_parts), &
        OLD_PY_COMP, lb=np + 1, ub=np + n_parts)
      call spec_aux_in(ic)%set_component(spec_in(ic)%pz(np + 1:np + n_parts), &
        OLD_PZ_COMP, lb=np + 1, ub=np + n_parts)
    end select
   end if
  end subroutine
  !---------------------------
  subroutine add_particles_old(spec_in, np, i1, i2, ic)
   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   integer, intent (in) :: np, i1, i2, ic
   integer :: n, ix, j, k, j2, k2
   real(dp) :: u, tmp0, whz

   tmp0 = t0_pl(ic)
   n = np
   charge = int(unit_charge(ic), hp_int)
   part_ind = 0
   k2 = loc_nptz(ic)
   j2 = loc_npty(ic)
   call init_random_seed(mype)
   select case (curr_ndim)
   case(3)
    do k = 1, k2
     do j = 1, j2
      do ix = i1, i2
       whz = wghpt(ix, ic)*loc_wghyz(j, k, ic)
       wgh = real(whz, sp)
       n = n + 1
       spec_in(ic)%part(n, 1) = xpt(ix, ic)
       spec_in(ic)%part(n, 2) = loc_ypt(j, ic)
       spec_in(ic)%part(n, 3) = loc_zpt(k, ic)
       call gasdev(u)
       spec_in(ic)%part(n, 4) = tmp0*u
       call gasdev(u)
       spec_in(ic)%part(n, 5) = tmp0*u
       call gasdev(u)
       spec_in(ic)%part(n, 6) = tmp0*u
       spec_in(ic)%part(n, 7) = wgh_cmp
      end do
     end do
    end do
   case(2)
    do j = 1, j2
     do ix = i1, i2
      wgh = real(loc_wghyz(j, 1, ic)*wghpt(ix, ic), sp)
      n = n + 1
      spec_in(ic)%part(n, 1) = xpt(ix, ic)
      spec_in(ic)%part(n, 2) = loc_ypt(j, ic)
      call gasdev(u)
      spec_in(ic)%part(n, 4) = tmp0*u
      call gasdev(u)
      spec_in(ic)%part(n, 5) = tmp0*u
      spec_in(ic)%part(n, 5) = wgh_cmp
     end do
    end do
   end select
  end subroutine
  !---------------------------
  subroutine count_inject_particles( xmx, np_old, np_new, initial_index, final_index )
   real (dp), intent (in) :: xmx
   integer, allocatable, dimension(:), intent(inout) :: np_old, np_new, initial_index, final_index
   integer :: ic, ix, npt_inj(4)
   integer :: i1, i2
   integer :: j2, k2, ndv

   !========== Inject particles from the right 
   !   xmx is the box xmax grid value at current time after window move
   !   in Comoving frame xmax is fixed and particles are left advected
   !=================================
   !  nptx(ic) is the max particle index inside the computational box
   !  nptx(ic) is updated in the same way both for moving window xmax
   !  or for left-advect particles with fixed xmax
   !===============================================

   ndv = nd2 + 1
   np_new = 0
   do ic = 1, nsp
    i1 = 1 + nptx(ic)
    i2 = i1 - 1
    if ( pex1 ) then
     if ( i1 <= sptx_max(ic) ) then
     !while particle index is less then the max index
      do ix = i1, sptx_max(ic)
       if ( xpt(ix, ic) > xmx ) exit
      end do
      i2 = ix - 1
      if ( ix == sptx_max(ic) ) i2 = ix
     else
      i2 = i1 - 1
     end if
     nptx(ic) = i2
    end if
    ! end if
    !==========================
    ! Partcles to be injected have index ix [i1,i2]
    !============================
    !==========================
    npt_inj(ic) = 0
    !=========== injects particles with coordinates index i1<= ix <=i2
    select case (ndim)
    case (1)
     npt_inj(ic) = (i2 - i1 + 1)
    case (2)
     j2 = loc_npty(ic)
     npt_inj(ic) = (i2 - i1 + 1)*j2
    case (3)
     k2 = loc_nptz(ic)
     j2 = loc_npty(ic)
     npt_inj(ic) = (i2 - i1 + 1)*j2*k2
    end select
    np_old(ic) = loc_npart(imody, imodz, imodx, ic)
    np_new(ic) = max(np_old(ic) + npt_inj(ic), np_new(ic))
    !=========================
    loc_npart(imody, imodz, imodx, ic) = np_new(ic)
    initial_index(ic) = i1
    final_index(ic) = i2
   end do

  end subroutine

  subroutine particles_inject_new(spec_in, spec_aux_in, np_old, np_new, i1, i2, mempool)
   type(species_new), dimension(:), allocatable, intent(inout) :: spec_in
   type(species_aux), dimension(:), allocatable, intent(inout) :: spec_aux_in
   integer, allocatable, dimension(:), intent(in) :: np_new, np_old, i1, i2
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: ic, q

   do ic = 1, nsp
    if ( pex1 ) then
     call spec_in(ic)%extend(np_new(ic))
     call spec_aux_in(ic)%extend(np_new(ic))
    end if
    q = np_old(ic)
    call add_particles(spec_in, spec_aux_in, q, i1(ic), i2(ic), ic, mempool)
   end do
   !=======================
  end subroutine

  subroutine particles_inject_old(xmx, spec_in, spec_aux_in)
   type(species), dimension(:), allocatable, intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   real (dp), intent (in) :: xmx
   integer :: ic, ix, npt_inj(4), np_old, np_new
   integer :: i1, i2, q
   integer :: j2, k2, ndv
   integer :: j, k

   !========== inject particles from the right
   !   xmx is the box xmax grid value at current time after window move
   !   in Comoving frame xmax is fixed and particles are left advected
   !=================================
   !  nptx(ic) is the max particle index inside the computational box
   !  nptx(ic) is updated in the same way both for moving window xmax
   !  or for left-advect particles with fixed xmax
   !===============================================

   ndv = nd2 + 1
   do ic = 1, nsp
    i1 = 1 + nptx(ic)
    if (i1 <= sptx_max(ic)) then
     !while particle index is less then the max index
     do ix = i1, sptx_max(ic)
      if (xpt(ix, ic) > xmx) exit
     end do
     i2 = ix - 1
     if (ix == sptx_max(ic)) i2 = ix
    else
     i2 = i1 - 1
    end if
    nptx(ic) = i2
    ! end if
    !==========================
    ! Partcles to be injected have index ix [i1,i2]
    !============================
    if (i2 > i1) then
     !==========================
     npt_inj(ic) = 0
     !=========== injects particles with coordinates index i1<= ix <=i2
     select case (ndim)
     case (1)
      do ix = i1, i2
       npt_inj(ic) = npt_inj(ic) + 1
      end do
     case (2)
      j2 = loc_npty(ic)
      do ix = i1, i2
       do j = 1, j2
        npt_inj(ic) = npt_inj(ic) + 1
       end do
      end do
     case (3)
      k2 = loc_nptz(ic)
      j2 = loc_npty(ic)
      do ix = i1, i2
       do k = 1, k2
        do j = 1, j2
         npt_inj(ic) = npt_inj(ic) + 1
        end do
       end do
      end do
     end select
     np_new = 0
     np_old = loc_npart(imody, imodz, imodx, ic)
     np_new = max(np_old+npt_inj(ic), np_new)
     call v_realloc( spec_aux_in, np_new, ndv )
     !=========================
     if (size(spec_in(ic)%part,ic)<np_new) then
      spec_aux_in(1:np_old, 1:ndv) = spec_in(ic)%part(1:np_old, 1:ndv)
      deallocate (spec_in(ic)%part)
      allocate (spec_in(ic)%part(np_new, ndv))
      spec_in(ic)%part(1:np_old, 1:ndv) = spec_aux_in(1:np_old, 1:ndv)
     end if
     q = np_old
     call add_particles(spec_in, q, i1, i2, ic)
     loc_npart(imody, imodz, imodx, ic) = np_new
    end if
   end do
   !=======================
  end subroutine
  !=======================
  subroutine reset_loc_xgrid
   integer :: p, ip, i, ii, n_loc

   p = 0
   n_loc = loc_xgrid(p)%ng
   loc_xg(0, 1, p) = x(1) - dx
   loc_xg(0, 2, p) = xh(1) - dx
   do i = 1, n_loc + 1
    loc_xg(i, 1, p) = x(i)
    loc_xg(i, 2, p) = xh(i)
   end do
   ip = loc_xgrid(0)%ng
   if (npe_xloc > 2) then
    do p = 1, npe_xloc - 2
     n_loc = loc_xgrid(p - 1)%ng
     loc_xg(0, 1:2, p) = loc_xg(n_loc, 1:2, p - 1)
     n_loc = loc_xgrid(p)%ng
     do i = 1, n_loc + 1
      ii = i + ip
      loc_xg(i, 1, p) = x(ii)
      loc_xg(i, 2, p) = xh(ii)
     end do
     loc_xgrid(p)%gmin = loc_xgrid(p - 1)%gmax
     ip = ip + n_loc
     loc_xgrid(p)%gmax = x(ip + 1)
    end do
   end if
   p = npe_xloc - 1
   n_loc = loc_xgrid(p - 1)%ng
   loc_xg(0, 1:2, p) = loc_xg(n_loc, 1:2, p - 1)
   n_loc = loc_xgrid(p)%ng
   do i = 1, n_loc + 1
    ii = i + ip
    loc_xg(i, 1, p) = x(ii)
    loc_xg(i, 2, p) = xh(ii)
   end do
   loc_xgrid(p)%gmin = loc_xgrid(p - 1)%gmax
   ip = ip + n_loc
   loc_xgrid(p)%gmax = x(ip + 1)
  end subroutine
  !========================================
  subroutine comoving_coordinate_new(vb, w_nst, loc_it, spec_in, spec_aux_in, mempool)
   real (dp), intent (in) :: vb
   integer, intent (in) :: w_nst, loc_it
   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer, allocatable, dimension(:) :: np_new, np_old, i1, i2
   integer :: i, ic, nshx
   real (dp) :: dt_tot, dt_step
   logical, parameter :: mw = .true.
   !======================
   ! In comoving x-coordinate the 
   ! [xmin <= x <= xmax] computational box is stationaty
   ! xi= (x-vb*t) => xw is left-advected 
   ! fields are left-advected in the x-grid directely in the maxw. equations
   ! particles are left-advected:
   ! xp=xp-vb*dt inside the computational box is added in the eq. of motion and
   ! for moving coordinates at each w_nst steps
   ! xpt(ix,ic)=xpt(ix,ic)-vb*w_nst*dt outside the computational box
   ! then targ_in=targ_in -vb*w_nst*dt   targ_out=targ_out-vb*w_nst*dt
   ! 
   !==================
   if (loc_it==0) return
   dt_step = dt_loc
   dt_tot = 0.0
   do i = 1, w_nst
    dt_tot = dt_tot + dt_step
   end do
   nshx = nint(dx_inv*dt_tot*vb) !the number of grid points x-shift for each w_nst step 
   do i = 1, nx + 1
    xw(i) = xw(i) - dx*nshx !moves backwards the grid xw 
   end do
   xw_max = xw_max - dx*nshx
   xw_min = xw_min - dx*nshx
   !======================== xw(i) grid used only for diagnostics purposes
   targ_in = targ_in - vb*dt_tot
   targ_end = targ_end - vb*dt_tot
   if (.not. part) return
   !===========================
   do ic = 1, nsp !left-advects all particles of the target outside the computational box
    do i = nptx(ic) + 1, sptx_max(ic)
     xpt(i, ic) = xpt(i, ic) - vb*dt_tot
    end do
   end do
   !======================
   do ic = 1, nsp
    call cell_part_dist(mw, spec_in(ic), spec_aux_in(ic), ic) !particles are redistributes along the
   end do
   allocate(np_new(nsp), source=0)
   allocate(np_old(nsp), source=0)
   allocate(i1(nsp), source=0)
   allocate(i2(nsp), source=0)
   call count_inject_particles(xmax, np_old, np_new, i1, i2)
   if (pex1) then
    if (targ_in<=xmax .and. targ_end>xmax) then
     call particles_inject(spec_in, spec_aux_in, np_old, np_new, i1, i2, mempool)
    end if
   end if
  end subroutine
  !====================================
  !========================================
  subroutine comoving_coordinate_old(vb, w_nst, loc_it, spec_in, spec_aux_in, mempool)
   real (dp), intent (in) :: vb
   integer, intent (in) :: w_nst, loc_it
   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: i, ic, nshx
   real(dp) :: dt_tot, dt_step
   logical, parameter :: mw = .true.
   !======================
   ! In comoving x-coordinate the
   ! [xmin <= x <= xmax] computational box is stationaty
   ! xi= (x-vb*t) => xw is left-advected
   ! fields are left-advected in the x-grid directely in the maxw. equations
   ! particles are left-advected:
   ! xp=xp-vb*dt inside the computational box is added in the eq. of motion and
   ! for moving coordinates at each w_nst steps
   ! xpt(ix,ic)=xpt(ix,ic)-vb*w_nst*dt outside the computational box
   ! then targ_in=targ_in -vb*w_nst*dt   targ_out=targ_out-vb*w_nst*dt
   !
   !==================
   if (loc_it == 0) return
   dt_step = dt_loc
   dt_tot = 0.0
   do i = 1, w_nst
    dt_tot = dt_tot + dt_step
   end do
   nshx = nint(dx_inv*dt_tot*vb) !the number of grid points x-shift for each w_nst step
   do i = 1, nx + 1
    xw(i) = xw(i) - dx*nshx !moves backwards the grid xw
   end do
   xw_max = xw_max - dx*nshx
   xw_min = xw_min - dx*nshx
   !======================== xw(i) grid used only for diagnostics purposes
   targ_in = targ_in - vb*dt_tot
   targ_end = targ_end - vb*dt_tot
   if (.not. part) return
   !===========================
   do ic = 1, nsp !left-advects all particles of the target outside the computational box
    do i = nptx(ic) + 1, sptx_max(ic)
     xpt(i, ic) = xpt(i, ic) - vb*dt_tot
    end do
   end do
   !======================
   call cell_part_dist(mw, spec_in, spec_aux_in, ic) !particles are redistributes along the
   if (pex1) then
    if (targ_in<=xmax .and. targ_end>xmax) then
     call particles_inject(xmax, spec_in, spec_aux_in)
    end if
   end if
  end subroutine
  !====================================
  subroutine lp_window_xshift_new(witr, init_iter, spec_in, spec_aux_in, mempool)
   integer, intent (in) :: witr, init_iter
   type(species_new), allocatable, dimension(:), intent(inout) :: spec_in
   type(species_aux), allocatable, dimension(:), intent(inout) :: spec_aux_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer, allocatable, dimension(:) :: np_new, np_old, in_ind, fin_ind
   integer :: i1, n1p, nc_env
   integer :: ix, nshx, wi2, ic
   real (dp), save :: xlapse, dt_step
   integer, save :: wi1
   logical, parameter :: mw = .true.
   integer, parameter :: spl = 2, spr = 2

   if (init_iter==0) then
    xlapse = 0.0
    wi1 = 0
    return
   end if
   dt_step = dt_loc
   !==================
   i1 = loc_xgrid(imodx)%p_ind(1)
   n1p = loc_xgrid(imodx)%p_ind(2)
   !======================
   xlapse = xlapse + w_speed*dt_step*witr
   wi2 = nint(dx_inv*xlapse)
   nshx = wi2 - wi1
   wi1 = wi2
   do ix = 1, nx + 1
    x(ix) = x(ix) + dx*nshx
    xh(ix) = xh(ix) + dx*nshx
   end do
   xmin = xmin + dx*nshx
   xmax = xmax + dx*nshx
   xp0_out = xp0_out + dx*nshx
   xp1_out = xp1_out + dx*nshx
   loc_xgrid(imodx)%gmin = loc_xgrid(imodx)%gmin + dx*nshx
   loc_xgrid(imodx)%gmax = loc_xgrid(imodx)%gmax + dx*nshx
   xmn = xmn + dx*nshx
   wi2 = n1p - nshx
   if (wi2<=0) then
    write (6, '(a37,3i6)') 'Error in window shifting for MPI proc', &
      imody, imodz, imodx
    ier = 2
    return
   end if
   !===========================
   call fields_left_xshift(ebf, i1, wi2, 1, nfield, nshx)
   if (hybrid) then
    do ix = 1, nxf - nshx
     fluid_x_profile(ix) = fluid_x_profile(ix+nshx)
    end do
    nxf = nxf - nshx
    call fluid_left_xshift(up, fluid_x_profile, fluid_yz_profile, i1, &
      wi2, 1, nfcomp, nshx)
    call fields_left_xshift(up0, i1, wi2, 1, nfcomp, nshx)
   end if
   if (envelope) then
    nc_env = size(env, 4)
    call fields_left_xshift(env, i1, wi2, 1, nc_env, nshx)
    if (prl) then
     call fill_ebfield_yzxbdsdata(env, 1, nc_env, spr, spl)
    end if
    if (Two_color) then
     call fields_left_xshift(env1, i1, wi2, 1, nc_env, nshx)
     if (prl) then
      call fill_ebfield_yzxbdsdata(env1, 1, nc_env, spr, spl)
     end if
    end if
   end if
   !shifts fields data and inject right ebf(wi2+1:n1p) x-grid nshx new data
   !===========================
   if (part) then
    do ic = 1, nsp
     call cell_part_dist(mw, spec_in(ic), spec_aux_in(ic), ic) !particles are redistributes along the
    end do
   end if
   allocate(np_new(nsp), source=0)
   allocate(np_old(nsp), source=0)
   allocate(in_ind(nsp), source=0)
   allocate(fin_ind(nsp), source=0)
   call count_inject_particles(loc_xgrid(imodx)%gmax, np_old, np_new, in_ind, fin_ind)
    ! right-shifted x-coordinate in MPI domains
   call part_numbers
   if (part) then
    if (targ_in<=xmax) then
     call particles_inject(spec_in, spec_aux_in, np_old, np_new, in_ind, fin_ind, mempool)
    end if
    
   end if
  end subroutine
  !==============================
  !====================================
  subroutine lp_window_xshift_old(witr, init_iter, spec_in, spec_aux_in, mempool)
   integer, intent (in) :: witr, init_iter
   type(species), allocatable, dimension(:), intent(inout) :: spec_in
   real(dp), allocatable, dimension(:, :), intent(inout) :: spec_aux_in
   type(memory_pool_t), pointer, intent(in) :: mempool
   integer :: i1, n1p, nc_env
   integer :: ix, nshx, wi2, ic
   real (dp), save :: xlapse, dt_step
   integer, save :: wi1
   logical, parameter :: mw = .true.

   if (init_iter == 0) then
    xlapse = 0.0
    wi1 = 0
    return
   end if
   dt_step = dt_loc
   !==================
   i1 = loc_xgrid(imodx)%p_ind(1)
   n1p = loc_xgrid(imodx)%p_ind(2)
   !======================
   xlapse = xlapse + w_speed*dt_step*witr
   wi2 = nint(dx_inv*xlapse)
   nshx = wi2 - wi1
   wi1 = wi2
   do ix = 1, nx + 1
    x(ix) = x(ix) + dx*nshx
    xh(ix) = xh(ix) + dx*nshx
   end do
   xmin = xmin + dx*nshx
   xmax = xmax + dx*nshx
   xp0_out = xp0_out + dx*nshx
   xp1_out = xp1_out + dx*nshx
   loc_xgrid(imodx)%gmin = loc_xgrid(imodx)%gmin + dx*nshx
   loc_xgrid(imodx)%gmax = loc_xgrid(imodx)%gmax + dx*nshx
   xmn = xmn + dx*nshx
   wi2 = n1p - nshx
   if (wi2 <= 0) then
    write (6, '(a37,3i6)') 'Error in window shifting for MPI proc', &
     imody, imodz, imodx
    ier = 2
    return
   end if
   !===========================
   call fields_left_xshift(ebf, i1, wi2, 1, nfield, nshx)
   if (hybrid) then
    do ix = 1, nxf - nshx
     fluid_x_profile(ix) = fluid_x_profile(ix + nshx)
    end do
    nxf = nxf - nshx
    call fluid_left_xshift(up, fluid_x_profile, fluid_yz_profile, i1, &
                           wi2, 1, nfcomp, nshx)
    call fields_left_xshift(up0, i1, wi2, 1, nfcomp, nshx)
   end if
   if (envelope) then
    nc_env = size(env, 4)
    call fields_left_xshift(env, i1, wi2, 1, nc_env, nshx)
    if (Two_color) call fields_left_xshift(env1, i1, wi2, 1, nc_env, &
                                           nshx)
   end if
   !shifts fields data and inject right ebf(wi2+1:n1p) x-grid nshx new data
   !===========================
   if (part) then
    call cell_part_dist(mw, spec_in, spec_aux_in, ic) !particles are redistributes along the
    ! right-shifted x-coordinate in MPI domains
    if (pex1) then
     if (targ_in<=xmax) then
      if (targ_end>xmax) then
       call particles_inject(xmax, spec_in, spec_aux_in)
      end if
     end if
    end if
    call part_numbers
   end if
  end subroutine
  !==============================
 end module

