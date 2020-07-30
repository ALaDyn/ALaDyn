
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
  use util, only: init_random_seed, gasdev
  use pstruct_data
  use fstruct_data
  use common_param
  use grid_param
  use mpi_field_interface
  use mpi_part_interface
  use run_data_info, only: part_numbers

  implicit none
  !===============================
  ! MOVING WINDOW SECTION
  !=============================
 contains

  subroutine add_particles(np, i1, i2, ic)
   integer, intent(in) :: np, i1, i2, ic
   integer :: n, ix, j, k, j2, k2
   real(dp) :: u, tmp0, whz

   tmp0 = t0_pl(ic)
   n = np
   charge = int(unit_charge(ic), hp_int)
   part_ind = 0
   k2 = loc_nptz(ic)
   j2 = loc_npty(ic)
   if (curr_ndim > 2) then
    do k = 1, k2
     do j = 1, j2
      do ix = i1, i2
       whz = wghpt(ix, ic)*loc_wghyz(j, k, ic)
       wgh = real(whz, sp)
       n = n + 1
       spec(ic)%part(n, 1) = xpt(ix, ic)
       spec(ic)%part(n, 2) = loc_ypt(j, ic)
       spec(ic)%part(n, 3) = loc_zpt(k, ic)
       spec(ic)%part(n, 4:6) = zero_dp
       spec(ic)%part(n, 7) = wgh_cmp
      end do
     end do
    end do
   else
    do j = 1, j2
     do ix = i1, i2
      wgh = real(loc_wghyz(j, 1, ic)*wghpt(ix, ic), sp)
      n = n + 1
      spec(ic)%part(n, 1) = xpt(ix, ic)
      spec(ic)%part(n, 2) = loc_ypt(j, ic)
      spec(ic)%part(n, 3:4) = zero_dp
      spec(ic)%part(n, 5) = wgh_cmp
     end do
    end do
   end if
   if (tmp0 > 0.0) then
    n = np
    call init_random_seed(mype)
    if (curr_ndim > 2) then
     do k = 1, k2
      do j = 1, j2
       do ix = i1, i2
        n = n + 1
        call gasdev(u)
        spec(ic)%part(n, 4) = tmp0*u
        spec(ic)%part(n, 5) = tmp0*u
        spec(ic)%part(n, 6) = tmp0*u
       end do
      end do
     end do
    else
     do j = 1, j2
      do ix = i1, i2
       n = n + 1
       call gasdev(u)
       spec(ic)%part(n, 3) = tmp0*u
       call gasdev(u)
       spec(ic)%part(n, 4) = tmp0*u
      end do
     end do
    end if
   end if
  end subroutine
  !---------------------------
  subroutine particles_inject(xmx)
   real(dp), intent(in) :: xmx
   integer :: ic, ix, npt_inj(4), np_old, np_new
   integer :: i1, i2, n, q
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
    ! endif
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
     np_new = max(np_old + npt_inj(ic), np_new)
     call v_realloc(ebfp, np_new, ndv)
     !=========================
     if (size(spec(ic)%part, ic) < np_new) then
      ebfp(1:np_old, 1:ndv) = spec(ic)%part(1:np_old, 1:ndv)
      deallocate (spec(ic)%part)
      allocate (spec(ic)%part(np_new, ndv))
      spec(ic)%part(1:np_old, 1:ndv) = ebfp(1:np_old, 1:ndv)
     end if
     q = np_old
     call add_particles(q, i1, i2, ic)
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
  subroutine comoving_coordinate(vb, w_nst, loc_it)
   real(dp), intent(in) :: vb
   integer, intent(in) :: w_nst, loc_it
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
   call cell_part_dist(mw) !particles are redistributes along the
   if (pex1) then
    if (targ_in <= xmax .and. targ_end > xmax) then
     call particles_inject(xmax)
    end if
   end if
  end subroutine
  !====================================
  subroutine lp_window_xshift(witr, init_iter)
   integer, intent(in) :: witr, init_iter
   integer :: i1, n1p, nc_env
   integer :: ix, nshx, wi2
   real(dp), save :: xlapse, dt_step
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
    call cell_part_dist(mw) !particles are redistributes along the
    ! right-shifted x-coordinate in MPI domains
    if (pex1) then
     if (targ_in <= xmax) then
      if (targ_end > xmax) then
       call particles_inject(xmax)
      end if
     end if
    end if
    call part_numbers
   end if
  end subroutine
  !==============================
 end module

