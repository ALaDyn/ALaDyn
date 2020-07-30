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

 module mpi_part_interface

  use array_alloc
  use grid_param
  use code_util
  use parallel

  implicit none
  real(dp) :: loc_pstore(7)
  real(dp), allocatable :: sp_aux(:, :), sp1_aux(:, :)

 contains
  !=================
  subroutine traffic_size_eval(sp_loc, xl, xr, pel, per, ibd, ind, &
                               npold, nsr, npnew)

   type(species), intent(in) :: sp_loc
   real(dp), intent(in) :: xl, xr
   logical, intent(in) :: pel, per
   integer, intent(in) :: ibd, ind, npold
   integer, intent(inout) :: nsr(4)
   integer, intent(inout) :: npnew
   integer :: p, q
   integer :: nl_send, nl_recv, nr_send, nr_recv, cdir

   cdir = ind - 1
   if (ind == 1) cdir = 3
   p = 0
   q = 0
   nr_recv = 0
   nl_recv = 0
   if (npold > 0) then
    p = COUNT(sp_loc%part(1:npold, ind) > xr)
    q = COUNT(sp_loc%part(1:npold, ind) < xl)
   end if
   nr_send = p
   nl_send = q
   call sr_idata(nr_send, nl_recv, cdir, left)
   !sends to right nr_send receives from left nl_recv
   call sr_idata(nl_send, nr_recv, cdir, right)
   !sends to left nl_send  receives from right nr_recv

   if (ibd < 2) then !NOT PERIODIC BD
    if (pel) nl_recv = 0
    if (per) then
     nr_recv = 0
     if (ibd == 1) nr_send = 0
    end if
   end if
   npnew = npold + nl_recv + nr_recv - nl_send - nr_send
   !================================
   !if(pel)nl_send=0
   !if(per)nr_send=0
   nsr(1) = nl_send
   nsr(2) = nr_send
   nsr(3) = nl_recv
   nsr(4) = nr_recv
   !================================
  end subroutine
  !======================================
  subroutine part_prl_wexchange(sp_loc, xl, xr, xlmin, xrmax, &
                                pel, per, ibd, dir, ndv, old_np, n_sr, npt)

   type(species), intent(inout) :: sp_loc
   real(dp), intent(in) :: xl, xr, xlmin, xrmax
   logical, intent(in) :: pel, per
   integer, intent(in) :: ibd, dir, ndv, old_np, n_sr(4)
   integer, intent(out) :: npt
   type(index_array) :: left_pind, right_pind
   integer :: k, kk, n, p, q, ns, nr, cdir
   integer :: nl_send, nr_send, nl_recv, nr_recv, vxdir
   logical :: mask(old_np)
   !================ dir are cartesian coordinate index (x,y,z)
   !================ cdir are mpi-cartesian index (y,z,x)

   nl_send = n_sr(1)
   nr_send = n_sr(2)
   nl_recv = n_sr(3)
   nr_recv = n_sr(4)
   cdir = dir - 1
   if (dir == 1) cdir = 3 !for x-direction
   vxdir = dir + ndim
   !================== checks memory
   p = ndv*max(nl_send, nr_send)
   if (p > 0) then
    if (size(aux1) < p) then
     deallocate (aux1)
     allocate (aux1(p))
     aux1(:) = zero_dp
    end if
   end if
   q = ndv*max(nl_recv, nr_recv)
   if (q > 0) then
    if (size(aux2) < q) then
     deallocate (aux2)
     allocate (aux2(q))
     aux2(:) = zero_dp
    end if
   end if
   !==================== copy remaining part => ebfp
   right_pind = index_array(old_np)
   left_pind = index_array(old_np)

   p = 0
   q = 0
   npt = 0
   if (ibd == 1 .and. dir == 1) then !reflecting on the right
    if (per) then
     associate (xp => sp_loc%part(1:old_np, dir))
      where (xp > xr)
      xp = xr - (xp - xr)
      sp_loc%part(1:old_np, vxdir) = -sp_loc%part(1:old_np, vxdir)
      end where
     end associate
    end if
   end if
!================== copy in sp_aux particles not to be exchanged
   associate (xp => sp_loc%part(1:old_np, dir))
    call right_pind%find_index(xp > xr)
    call left_pind%find_index(xp < xl)
    mask(:) = (xp >= xl .and. xp <= xr)
    npt = COUNT(mask(:))
   end associate

   do n = 1, ndv
    sp_aux(1:npt, n) = PACK(sp_loc%part(1:old_np, n), mask(:))
   end do
   !=======================
   ns = ndv*nr_send
   nr = ndv*nl_recv
   if (ibd < 2) then !NON PERIODIC CASE
    if (per) ns = 0
    if (pel) nr = 0
   end if
   if (ns > 0) then
    kk = 0
    if (per) then
     !sends to the right only for Periodic boundary
     do k = 1, nr_send
      n = right_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      loc_pstore(dir) = loc_pstore(dir) + xlmin - xr
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
!=============== NON PERIODIC CASE
    else
     !To be checked case ibd == 1
     do k = 1, nr_send
      n = right_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
    end if
   end if

   if (max(ns, nr) > 0) call sr_pdata(aux1, aux2, ns, nr, cdir, left)
   ! sends ns data to the right
   if (nr > 0) then !receives nr data from left
    kk = 0
    p = npt
    do n = 1, nl_recv
     p = p + 1
     do q = 1, ndv
      kk = kk + 1
      sp_aux(p, q) = aux2(kk)
     end do
    end do
    npt = p
   end if
!===================
   ns = ndv*nl_send
   nr = ndv*nr_recv
   if (ibd == 0) then
    if (pel) ns = 0
    if (per) nr = 0
   end if
   if (ns > 0) then
    kk = 0
    if (pel) then !only for periodic case
     do k = 1, nl_send
      n = left_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      loc_pstore(dir) = loc_pstore(dir) + xrmax - xl
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
    else
     !============ NON PERIODIC EXCHANGE
     do k = 1, nl_send
      n = left_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
    end if
   end if   !END ns >0
   if (max(ns, nr) > 0) call sr_pdata(aux1, aux2, ns, nr, cdir, right)
   ! sends ns data to the left recieves nr data from right
   if (nr > 0) then
    p = npt
    kk = 0
    do n = 1, nr_recv
     p = p + 1
     do q = 1, ndv
      kk = kk + 1
      sp_aux(p, q) = aux2(kk)
     end do
    end do
    npt = p
   end if

!  EXIT old+new data in sp_aux(:,:)
  end subroutine
!==============================================
  subroutine part_prl_exchange(sp_loc, vstore, xl, xr, xlmin, xrmax, &
                               pel, per, ibd, dir, ndv, old_np, n_sr, npt)

   type(species), intent(inout) :: sp_loc
   real(dp), intent(in) :: vstore(:, :)
   real(dp), intent(in) :: xl, xr, xlmin, xrmax
   logical, intent(in) :: pel, per
   integer, intent(in) :: ibd, dir, ndv, old_np, n_sr(4)
   integer, intent(out) :: npt
   type(index_array) :: left_pind, right_pind
   integer :: k, kk, n, p, q, ns, nr, cdir
   integer :: nl_send, nr_send, nl_recv, nr_recv, vxdir
   logical :: mask(old_np)
   !================ dir are cartesian coordinate index (x,y,z)
   !================ cdir are mpi-cartesian index (y,z,x)

   nl_send = n_sr(1)
   nr_send = n_sr(2)
   nl_recv = n_sr(3)
   nr_recv = n_sr(4)
   cdir = dir - 1
   if (dir == 1) cdir = 3 !for x-direction
   vxdir = dir + ndim
   !================== checks memory
   p = 2*ndv*max(nl_send, nr_send)
   if (p > 0) then
    if (size(aux1) < p) then
     deallocate (aux1)
     allocate (aux1(p))
     aux1(:) = zero_dp
    end if
   end if
   q = 2*ndv*max(nl_recv, nr_recv)
   if (q > 0) then
    if (size(aux2) < q) then
     deallocate (aux2)
     allocate (aux2(q))
     aux2(:) = zero_dp
    end if
   end if
   !==================== copy remaining part => ebfp
   right_pind = index_array(old_np)
   left_pind = index_array(old_np)

   p = 0
   q = 0
   npt = 0
   if (ibd == 1 .and. dir == 1) then !reflecting on the right
    if (per) then
     associate (xp => sp_loc%part(1:old_np, dir))
      where (xp > xr)
      xp = xr - (xp - xr)
      sp_loc%part(1:old_np, vxdir) = -sp_loc%part(1:old_np, vxdir)
      end where
     end associate
    end if
   end if
!================== copy in sp_aux particles not to be exchanged
   associate (xp => sp_loc%part(1:old_np, dir))
    call right_pind%find_index(xp > xr)
    call left_pind%find_index(xp < xl)
    mask(:) = (xp >= xl .and. xp <= xr)
    npt = COUNT(mask(:))
   end associate

   do n = 1, ndv
    sp_aux(1:npt, n) = PACK(sp_loc%part(1:old_np, n), mask(:))
    sp1_aux(1:npt, n) = PACK(vstore(1:old_np, n), mask(:))
   end do
   !=======================
   ns = 2*ndv*nr_send
   nr = 2*ndv*nl_recv
   if (ibd < 2) then !NON PERIODIC CASE
    if (per) ns = 0
    if (pel) nr = 0
   end if
   if (ns > 0) then
    kk = 0
    if (per) then
     !sends to the right only for Periodic boundary
     do k = 1, nr_send
      n = right_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      loc_pstore(dir) = loc_pstore(dir) + xlmin - xr
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
     !adds vstore data
     do k = 1, nr_send
      n = right_pind%indices(k)
      loc_pstore(1:ndv) = vstore(n, 1:ndv)
      loc_pstore(dir) = loc_pstore(dir) + xlmin - xr
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
!=============== NON PERIODIC CASE
    else
     !To be checked case ibd == 1
     do k = 1, nr_send
      n = right_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
     !adds vstore data
     do k = 1, nr_send
      n = right_pind%indices(k)
      loc_pstore(1:ndv) = vstore(n, 1:ndv)
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
    end if
   end if

   if (max(ns, nr) > 0) call sr_pdata(aux1, aux2, ns, nr, cdir, left)
   ! sends ns data to the right
   if (nr > 0) then !receives nr data from left
    kk = 0
    p = npt
    do n = 1, nl_recv
     p = p + 1
     do q = 1, ndv
      kk = kk + 1
      sp_aux(p, q) = aux2(kk)
     end do
    end do
    !   adds...
    p = npt
    do n = 1, nl_recv
     p = p + 1
     do q = 1, ndv
      kk = kk + 1
      sp1_aux(p, q) = aux2(kk)
     end do
    end do
    npt = p
   end if
!===================
   ns = 2*ndv*nl_send
   nr = 2*ndv*nr_recv
   if (ibd == 0) then
    if (pel) ns = 0
    if (per) nr = 0
   end if
   if (ns > 0) then
    kk = 0
    if (pel) then !only for periodic case
     do k = 1, nl_send
      n = left_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      loc_pstore(dir) = loc_pstore(dir) + xrmax - xl
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
! adds....
     do k = 1, nl_send
      n = left_pind%indices(k)
      loc_pstore(1:ndv) = vstore(n, 1:ndv)
      loc_pstore(dir) = loc_pstore(dir) + xrmax - xl
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
    else
     !============ NON PERIODIC EXCHANGE
     do k = 1, nl_send
      n = left_pind%indices(k)
      loc_pstore(1:ndv) = sp_loc%part(n, 1:ndv)
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
     do k = 1, nl_send
      n = left_pind%indices(k)
      loc_pstore(1:ndv) = vstore(n, 1:ndv)
      do q = 1, ndv
       kk = kk + 1
       aux1(kk) = loc_pstore(q)
      end do
     end do
    end if
   end if   !END ns >0
   if (max(ns, nr) > 0) call sr_pdata(aux1, aux2, ns, nr, cdir, right)
   ! sends ns data to the left recieves nr data from right
   if (nr > 0) then
    p = npt
    kk = 0
    do n = 1, nr_recv
     p = p + 1
     do q = 1, ndv
      kk = kk + 1
      sp_aux(p, q) = aux2(kk)
     end do
    end do
    p = npt
    do n = 1, nr_recv
     p = p + 1
     do q = 1, ndv
      kk = kk + 1
      sp1_aux(p, q) = aux2(kk)
     end do
    end do
    npt = p
   end if

!  EXIT old+new data in sp_aux(:,:) and sp1_aux(:,:)
  end subroutine
  !================
  !=============================
  subroutine reset_all_part_dist(loc_sp, pstore, xl, xr, ib, np, ndv, &
                                 cin, np_new, mwin)
   type(species), intent(inout) :: loc_sp
   real(dp), intent(inout) :: pstore(:, :)
   real(dp), intent(in) :: xl, xr
   logical, intent(in) :: mwin
   integer, intent(in) :: ib, np, ndv, cin
   integer, intent(out) :: np_new
   real(dp) :: xp, dxp
   integer :: n, p, pout
   !===========================
   np_new = np
   p = 0
   pout = 0
   if (ib == 2) then
    dxp = xr - xl
    do p = 1, np
     xp = loc_sp%part(p, cin)
     if (xp < xl) loc_sp%part(p, cin) = xp + dxp
     xp = loc_sp%part(p, cin)
     if (xp > xr) loc_sp%part(p, cin) = xp - dxp
    end do
    return
   end if
   do n = 1, np
    xp = loc_sp%part(n, cin)
    if (xp <= xl) p = p + 1
    if (xp > xr) p = p + 1
   end do
   pout = p
   if (pout > 0) then
    if (mwin) then
     call v_realloc(sp_aux, np - pout, ndv)
     p = 0
     do n = 1, np
      xp = loc_sp%part(n, cin)
      if (xp > xl .and. xp <= xr) then
       p = p + 1
       sp_aux(p, 1:ndv) = loc_sp%part(n, 1:ndv)
      end if
     end do
    else
     call v_realloc(sp_aux, np - pout, ndv)
     call v_realloc(sp1_aux, np - pout, ndv)
     p = 0
     do n = 1, np
      xp = loc_sp%part(n, cin)
      if (xp > xl .and. xp <= xr) then
       p = p + 1
       sp_aux(p, 1:ndv) = loc_sp%part(n, 1:ndv)
       sp1_aux(p, 1:ndv) = pstore(n, 1:ndv)
      end if
     end do
    endif
    np_new = p
   end if
  end subroutine
  !==============
  subroutine cell_part_dist(moving_wind)
   logical, intent(in) :: moving_wind
   integer :: ic, nspx, n, np, np_new, np_new_allocate, ndv, &
              np_rs, np_out
   integer :: n_sr(4)
   real(dp) :: ymm, ymx, lbd_min, rbd_max
   real(dp) :: zmm, zmx
   real(dp) :: xmm, xmx

   ndv = nd2 + 1
   !===================================
   ! In traffic_size_eval() Counts numbers of left-right exchanges
   !nsr(1)=nl_send
   !nsr(2)=nr_send
   !nsr(3)=nl_recv
   !nsr(4)=nr_recv
   ! ==> new particle number np_new= nl_recv-nl_send+ nr_recv-nr_send
   !      In part_prl_exchange()    exchanges particle data by mpi_send_recv
   !=====================================
   !In moving window box (xmm,xmx) are right shifted
   !all species leaving the computational box at the left
   !x-boundary are removed
   !==========================================
   !=========== mowing window section
   if (moving_wind) then
    nspx = nsp
    xmm = loc_xgrid(imodx)%gmin
    xmx = loc_xgrid(imodx)%gmax
    lbd_min = loc_xgrid(0)%gmin
    rbd_max = loc_xgrid(npe_xloc - 1)%gmax
    if (prlx) then
     do ic = 1, nspx
      np = loc_npart(imody, imodz, imodx, ic)
      np_new = np
      n_sr = 0
      call traffic_size_eval(spec(ic), xmm, xmx, pex0, pex1, ibx, 1, np, &
                             n_sr, np_new)
      ! Allocate the aux array with lenght np + n_recieve
      ! because it needs to receive before to send
      np_new_allocate = np_new + SUM(n_sr(1:2))
      np_rs = maxval(n_sr(1:4))
      if (np_rs > 0) then
       call v_realloc(sp_aux, np_new_allocate, ndv)
       call part_prl_wexchange(spec(ic), xmm, xmx, lbd_min, rbd_max, &
                               pex0, pex1, ibx, 1, ndv, np, n_sr, np_out)
       if (np_out /= np_new) then
        write (6, *) 'error in x-part w-count', mype, np_out, np_new
        ier = 99
       end if
       call p_realloc(spec(ic), np_new, ndv)
       call v_realloc(ebfp, np_new, ndv)
       spec(ic)%part(1:np_new, 1:ndv) = sp_aux(1:np_new, 1:ndv)
       loc_npart(imody, imodz, imodx, ic) = np_new
      end if
     end do
    else
     do ic = 1, nspx
      np = loc_npart(imody, imodz, imodx, ic)
      if (np > 0) then
       call reset_all_part_dist(spec(ic), ebfp, xmm, xmx, ibx, np, ndv, &
                                1, np_new, moving_wind)
       if (np_new < np) then
        loc_npart(imody, imodz, imodx, ic) = np_new
        do n = 1, np_new
         spec(ic)%part(n, 1:ndv) = sp_aux(n, 1:ndv)
        end do
       end if
      end if
     end do
    end if
    if (allocated(sp_aux)) deallocate (sp_aux)
    return
   endif
!=========== not mowing window section
   nspx = nsp_run
   xmm = loc_xgrid(imodx)%gmin
   xmx = loc_xgrid(imodx)%gmax
   lbd_min = loc_xgrid(0)%gmin
   rbd_max = loc_xgrid(npe_xloc - 1)%gmax
   if (prlx) then
    do ic = 1, nspx
     np = loc_npart(imody, imodz, imodx, ic)
     np_new = np
     n_sr = 0
     call traffic_size_eval(spec(ic), xmm, xmx, pex0, pex1, ibx, 1, np, &
                            n_sr, np_new)
     ! Allocate the aux array with lenght np + n_recieve
     ! because it needs to recieve before to send
     np_new_allocate = np_new + SUM(n_sr(1:2))
     np_rs = maxval(n_sr(1:4))
     if (np_rs > 0) then
      call v_realloc(sp_aux, np_new_allocate, ndv)
      call v_realloc(sp1_aux, np_new_allocate, ndv)
      call part_prl_exchange(spec(ic), ebfp, xmm, xmx, lbd_min, rbd_max, &
                             pex0, pex1, ibx, 1, ndv, np, n_sr, np_out)
      if (np_out /= np_new) then
       write (6, *) 'error in x-part count', mype, np_out, np_new
       ier = 99
      end if
      call p_realloc(spec(ic), np_new, ndv)
      call v_realloc(ebfp, np_new, ndv)
      spec(ic)%part(1:np_new, 1:ndv) = sp_aux(1:np_new, 1:ndv)
      ebfp(1:np_new, 1:ndv) = sp1_aux(1:np_new, 1:ndv)
      loc_npart(imody, imodz, imodx, ic) = np_new
     end if
    end do
   else
    do ic = 1, nspx
     np = loc_npart(imody, imodz, imodx, ic)
     if (np > 0) then
      call reset_all_part_dist(spec(ic), ebfp, xmm, xmx, ibx, np, ndv, &
                               1, np_new, moving_wind)
      if (np_new < np) then
       loc_npart(imody, imodz, imodx, ic) = np_new
       do n = 1, np_new
        spec(ic)%part(n, 1:ndv) = sp_aux(n, 1:ndv)
        ebfp(n, 1:ndv) = sp1_aux(n, 1:ndv)
       end do
      end if
     end if
    end do
   end if
   if (allocated(sp_aux)) deallocate (sp_aux)
   if (allocated(sp1_aux)) deallocate (sp1_aux)
!==========================
   ymm = loc_ygrid(imody)%gmin
   ymx = loc_ygrid(imody)%gmax
   lbd_min = loc_ygrid(0)%gmin
   rbd_max = loc_ygrid(npe_yloc - 1)%gmax
   if (prly) then
    do ic = 1, nsp_run
     n_sr = 0
     np = loc_npart(imody, imodz, imodx, ic)
     np_new = np
     call traffic_size_eval(spec(ic), ymm, ymx, pe0y, pe1y, iby, 2, np, &
                            n_sr, np_new)
     np_new_allocate = np_new + SUM(n_sr(1:2))
     np_rs = maxval(n_sr(1:4))
     if (np_rs > 0) then
      call v_realloc(sp_aux, np_new_allocate, ndv)
      call v_realloc(sp1_aux, np_new_allocate, ndv)
      call part_prl_exchange(spec(ic), ebfp, ymm, ymx, lbd_min, &
                             rbd_max, pe0y, pe1y, iby, 2, ndv, np, n_sr, np_out)
      if (np_out /= np_new) then
       write (6, *) 'error in y-part count', mype, np_out, np_new
       ier = 99
      end if
      call p_realloc(spec(ic), np_new, ndv)
      call v_realloc(ebfp, np_new, ndv)
      do n = 1, np_new
       spec(ic)%part(n, 1:ndv) = sp_aux(n, 1:ndv)
       ebfp(n, 1:ndv) = sp1_aux(n, 1:ndv)
      end do
      loc_npart(imody, imodz, imodx, ic) = np_new
     end if
    end do
   else
    do ic = 1, nsp_run
     np = loc_npart(imody, imodz, imodx, ic)
     if (np > 0) then
      call reset_all_part_dist(spec(ic), ebfp, ymm, ymx, iby, np, ndv, &
                               2, np_new, moving_wind)
      if (np_new < np) then
       loc_npart(imody, imodz, imodx, ic) = np_new
       do n = 1, np_new
        spec(ic)%part(n, 1:ndv) = sp_aux(n, 1:ndv)
        ebfp(n, 1:ndv) = sp1_aux(n, 1:ndv)
       end do
      end if
     end if
    end do
   end if
   if (allocated(sp_aux)) deallocate (sp_aux)
   if (allocated(sp1_aux)) deallocate (sp1_aux)
   if (ndim > 2) then
    zmm = loc_zgrid(imodz)%gmin
    zmx = loc_zgrid(imodz)%gmax
    lbd_min = loc_zgrid(0)%gmin
    rbd_max = loc_zgrid(npe_zloc - 1)%gmax
    if (prlz) then
     do ic = 1, nsp_run
      np = loc_npart(imody, imodz, imodx, ic)
      np_new = np
      n_sr = 0
      call traffic_size_eval(spec(ic), zmm, zmx, pe0z, pe1z, ibz, 3, &
                             np, n_sr, np_new)
      np_new_allocate = np_new + SUM(n_sr(1:2))
      np_rs = maxval(n_sr(1:4))
      if (np_rs > 0) then
       !=====================
       call v_realloc(sp_aux, np_new_allocate, ndv)
       call v_realloc(sp1_aux, np_new_allocate, ndv)
       !================
       call part_prl_exchange(spec(ic), ebfp, zmm, zmx, lbd_min, &
                              rbd_max, pe0z, pe1z, ibz, 3, ndv, np, n_sr, np_out)
       if (np_out /= np_new) then
        write (6, *) 'error in z-part count', mype, np_out, np_new
        ier = 99
       end if
       call p_realloc(spec(ic), np_new, ndv)
       call v_realloc(ebfp, np_new, ndv)
       do n = 1, np_new
        spec(ic)%part(n, 1:ndv) = sp_aux(n, 1:ndv)
        ebfp(n, 1:ndv) = sp1_aux(n, 1:ndv)
       end do
       loc_npart(imody, imodz, imodx, ic) = np_new
      end if
     end do
    else
     do ic = 1, nsp_run
      np = loc_npart(imody, imodz, imodx, ic)
      if (np > 0) then
       call reset_all_part_dist(spec(ic), ebfp, zmm, zmx, ibz, np, ndv, &
                                3, np_new, moving_wind)
       if (np_new < np) then
        loc_npart(imody, imodz, imodx, ic) = np_new
        do n = 1, np_new
         spec(ic)%part(n, 1:ndv) = sp_aux(n, 1:ndv)
         ebfp(n, 1:ndv) = sp1_aux(n, 1:ndv)
        end do
       end if
      end if
     end do
    end if
    if (allocated(sp_aux)) deallocate (sp_aux)
    if (allocated(sp1_aux)) deallocate (sp1_aux)
   end if
   !=====================
  end subroutine
  !=========================
 end module
