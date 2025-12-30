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

 module ionize
  use ionz_data
  use common_param
  use array_alloc
  use mpi_var
  use util

  implicit none
  private
  public :: ionization_cycle, set_field_ioniz_wfunction, de_inv, &
            deb_inv
  integer, allocatable :: el_ionz_count(:)
  real(dp), allocatable :: efp_aux(:, :)
 contains

  !================================
  subroutine set_field_ioniz_wfunction(z0, zm, loc_ion, nz_lev, &
                                       nz_model, e_max, dt_in)
   integer, intent(in) :: z0, zm, loc_ion, nz_lev, nz_model
   real(dp), intent(in) :: e_max
   integer :: i, k, j, ic, z, zk, zm_loc, ll
   real(dp) :: ei, w_bsi, w_adk, ns, fact1, fact2, dt_temp
   real(dp), allocatable :: aw(:, :)
   real(dp) :: p2, efact, avg_fact
   real(dp), optional :: dt_in
   !=============================
   ! uses stored coefficients nstar(z,ic), vfact(z,ic),C_nstar(z,ic)
   ! ionz_model, ionz_lev,nsp_ionz z0=z_ion(), zmax=atn() in common
   !===============================
   wi = 0.0
   dge = e_max/real(n_ge, dp)
   d2ge = dge*dge
   de_inv = 1./dge
   deb_inv = de_inv/514. !For fields in GV/m unit
   ic = loc_ion - 1
   dt_temp = dt_loc
   if (present(dt_in)) then
    dt_temp = dt_in
   end if
   select case (nz_model)
   case (1) !Only ADK: W_DC in chen et al (2013)
    do k = z0 + 1, zm !V(k,ic) to ionize ion(ic) A(k-1)=> A(k),
     j = k - z0
     ns = 2.*nstar(k, ic) - 1.
     do i = 1, n_ge
      ei = dge*real(i, dp) !The gridded field intensity
      fact2 = (2.*vfact(k, ic)/ei) !Vfact= (V/V_H)^{3/2}
      fact1 = (fact2)**ns
      w_adk = c_nstar(k, ic)*fact1*exp(-fact2/3.)
      !=============
      if (w_adk < tiny) w_adk = 0.0
      wi(i, j, ic) = w_adk !Wi(Ei,j,ic)  for j=1,zm-z0
     end do
    end do
   case (2) ! W_AC = <W_DC> in chen et al. (2013)
    do k = z0 + 1, zm
     j = k - z0
     ns = 2.*nstar(k, ic) - 1.
     do i = 1, n_ge
      ei = dge*real(i, dp) !The field intensity on grid
      fact2 = (2.*vfact(k, ic)/ei)
      fact1 = (fact2)**ns
      fact1 = fact1*sqrt(3.*ei/(pig*vfact(k, ic)))
      w_adk = c_nstar(k, ic)*fact1*exp(-fact2/3.)
      !=============
      if (w_adk < tiny) w_adk = 0.0
      wi(i, j, ic) = w_adk
     end do
    end do
   case (3) !BSI +adk(AC) (Posthumus)
    do k = z0 + 1, zm
     j = k - z0
     ns = 2.*nstar(k, ic) - 1.
     do i = 1, n_ge
      ei = dge*real(i, dp)
      fact2 = (2.*vfact(k, ic)/ei)
      fact1 = (fact2)**ns
      fact1 = fact1*sqrt(3.*ei/(pig*vfact(k, ic)))
      w_adk = c_nstar(k, ic)*fact1*exp(-fact2/3.)
      if (w_adk < tiny) w_adk = 0.0
      wi(i, j, ic) = w_adk
      !=============
      if (ei > e_c(k, ic)) then !w_adk(Ei=Ec)
       fact2 = (2.*vfact(k, ic)/e_c(k, ic))
       fact1 = (fact2)**ns
       fact1 = fact1*sqrt(3.*e_c(k, ic)/(pig*vfact(k, ic)))
       w_adk = c_nstar(k, ic)*fact1*exp(-fact2/3.)
       w_bsi = vfact(k, ic)*(1.-e_c(k, ic)/ei)/(4.*pig*real(k, dp))
       wi(i, j, ic) = w_bsi + w_adk
      end if
     end do
    end do
   case (4) ! Epoch version Min(adk-BSI) with adk = adk<m>
    do k = z0 + 1, zm
     ll = (l_fact(k) - 1)/2
     j = k - z0
     ns = 2.*nstar(k, ic) - 1.
     do i = 1, n_ge
      ei = dge*real(i, dp) !The field intensity on grid
      fact2 = (2.*vfact(k, ic)/ei)
      fact1 = (fact2)**ns
      avg_fact = 1.0
      if (l_fact(k) > 1) then
       do ll = 1, (l_fact(k) - 1)/2
        avg_fact = avg_fact + 2./(fact2)**ll
       end do
       avg_fact = avg_fact/l_fact(k)
       fact1 = fact1*avg_fact
      end if
      fact1 = fact1*sqrt(3.*ei/(pig*vfact(k, ic)))
      w_adk = c_nstar(k, ic)*fact1*exp(-fact2/3.)
      if (w_adk < tiny) w_adk = 0.0
      !=============
      wi(i, j, ic) = w_adk
      efact = 1.-e_c(k, ic)/ei
      w_bsi = vfact(k, ic)*efact/(4.*pig*real(k, dp))
      if (w_bsi < tiny) w_bsi = 0.
      if (ei > e_c(k, ic)) wi(i, j, ic) = min(w_bsi, w_adk)
      if (ei > e_b(k, ic)) wi(i, j, ic) = w_bsi
     end do
    end do
   case (5) !cycle averaged BSI +adk (Posthumus)
    do k = z0 + 1, zm
     j = k - z0
     ns = 2.*nstar(k, ic) - 1.
     do i = 1, n_ge
      ei = dge*real(i, dp) !The gridded field intensity
      fact2 = (2.*vfact(k, ic)/ei)
      fact1 = sqrt(3.*ei/(pig*vfact(k, ic)))*(fact2)**ns
      w_adk = c_nstar(k, ic)*fact1*exp(-fact2/3.)
      if (w_adk < tiny) w_adk = 0.0
      wi(i, j, ic) = w_adk
      !=============
      if (ei > e_c(k, ic)) then
       fact2 = (2.*vfact(k, ic)/e_c(k, ic))
       fact1 = sqrt(3.*e_c(k, ic)/(pig*vfact(k, ic)))*(fact2)**ns
       w_adk = c_nstar(k, ic)*fact1*exp(-fact2/3.)
       w_bsi = vfact(k, ic)*(1.-e_c(k, ic)/ei)/(4.*pig*real(k, dp))
       wi(i, j, ic) = w_bsi + w_adk
      end if
     end do
    end do
   end select
   !=================== ordering================
   ! k=0
   !V(z0+1) for n[z0]=> n[z0+1] transition with probability W[1]
   ! k=1,2,...
   !V(z0+k+1) for n[z0+k]=> n[z0+k+1] transition with probability W(k+1)
   ! k=zmax-z0-1
   !V(zmax) for n[zmax-1]=> n[zmax] transition with probability W[zmax-z0]
   !===============================================
   ! dt in unit l0/c= 10/3. fs  dt_fs in fs unit
   dt_fs = 10.*dt_temp/3.
   wi = omega_a*wi
   zm_loc = zm - z0
   if (nz_lev == 1) then ! one level ionization
    !W_one_level(Ef,k=z0:zm,ic) P(k.k+1) ionization
    !Wi(Ef,zk=1,zm-z0,ic) Rate of ionization zk+z0-1=> zk+z0
    w_one_lev(0:n_ge, zm, ic) = zero_dp
    do z = 0, zm_loc - 1
     zk = z + 1
     k = z + z0
     !z0 is the initial ion charge status
     !k=z0,z0+1,..,zm-1  zk=k-z0+1
     do i = 1, n_ge
      w_one_lev(i, k, ic) = 1.-exp(-dt_fs*wi(i, zk, ic))
     end do
    end do
    return
   end if
   !===================
   allocate (aw(0:zm_loc, 0:zm_loc))
   do i = 1, n_ge
    do z = 0, zm_loc - 1
     zk = z + 1
     !z is the initial ion charge status
     !z=z0,z0+1,..,zmax-1
     !for fixed z0=0,   zmax-1
     !with inization potential v(1),...,V(zmax)
     aw(0, 0) = one_dp
     k = 0
     wsp(i, k, z + z0, ic) = aw(0, 0)*exp(-dt_fs*wi(i, zk, ic))
     if (wi(i, zk, ic) > 0.0) then
      if (zk < zm_loc) then
       do k = 1, zm_loc - zk
        aw(k, k) = 0.0
        if (wi(i, k + zk, ic) > 0.0) then
         p2 = 0.0
         do j = 0, k - 1
          fact1 = wi(i, k - 1 + zk, ic)/wi(i, k + zk, ic)
          fact2 = wi(i, j + zk, ic)/wi(i, k + zk, ic)
          aw(j, k) = aw(j, k - 1)*fact1/(1.-fact2)
          aw(k, k) = aw(k, k) - aw(j, k)
          p2 = p2 + aw(j, k)*exp(-dt_fs*wi(i, zk + j, ic))
         end do
         wsp(i, k, z + z0, ic) = p2 + aw(k, k)*exp(-dt_fs*wi(i, k + zk, ic))
        else
         p2 = 0.0
         if (k == 1) then
          j = 0
          aw(j, k) = -aw(j, k - 1)
          aw(k, k) = -aw(j, k)
          p2 = p2 + aw(j, k)*exp(-dt_fs*wi(i, zk + j, ic))
         end if
         if (k == 2) then
          j = 0
          aw(j, k) = -aw(j, k - 1)*wi(i, zk + k - 1, ic)/wi(i, zk + j, ic)
          aw(k, k) = -aw(j, k)
          p2 = p2 + aw(j, k)*exp(-dt_fs*wi(i, zk + j, ic))
          j = 1
          aw(j, k) = -aw(j, k - 1)
          aw(k, k) = aw(k, k) - aw(j, k)
          p2 = p2 + aw(j, k)*exp(-dt_fs*wi(i, zk + j, ic))
         end if
         wsp(i, k, z + z0, ic) = p2 + aw(k, k)
        end if
       end do
       do k = 1, zm_loc - zk
        wsp(i, k, z + z0, ic) = wsp(i, k, z + z0, ic) + &
                                wsp(i, k - 1, z + z0, ic)
       end do
      end if
     else
      do k = 1, zm_loc - zk
       wsp(i, k, z + z0, ic) = wsp(i, 0, z + z0, ic)
      end do
     end if
     wsp(i, zm_loc + 1 - zk, z + z0, ic) = 1.
    end do
   end do
   !============================
   ! EXIT the cumulative DF F_j= u_0, u_0+u_1,.., u_0+u_1+ u_zmax-1
   ! ndexed with j=1,2,1
  end subroutine

  !========================================
  subroutine ionization_electrons_inject(spec_in, ion_ch_inc, ic, np, np_el, &
    new_np_el)

   type(species), dimension(:), intent(inout) :: spec_in
   integer, intent (in) :: ion_ch_inc(:)
   integer, intent (in) :: ic, np, new_np_el
   integer, intent (inout) :: np_el
   integer (sp) :: inc, id_ch
   real (dp) :: u, temp(3)

   integer :: n, i, ii
   !========== Enter sp_field(n,1)= the rms momenta Delta*a (n) for env model
   !                 inc=ion_ch_inc(n) the number of ionization electrons
   id_ch = nd2 + 1
   ii = np_el
   temp(1) = t0_pl(1)
   temp(2:3) = temp(1)
   if (ii == 0) write (6, '(a33,2I6)') 'warning, no electrons before ionz' &
    , imody, imodz
   select case (curr_ndim)
   case (2)
    do n = 1, np
     inc = ion_ch_inc(n)
     if (inc>0) then
      wgh_cmp = spec_in(ic)%part(n, id_ch)
      charge = -one_int_hp
      part_ind = -one_int_hp
      wgh = wgh*n_mol_atoms(ic - 1)
      do i = 1, inc
       ii = ii + 1
       spec_in(1)%part(ii, 1:2) = spec_in(ic)%part(n, 1:2)
       call gasdev(u)
       spec_in(1)%part(ii, 3) = temp(1)*u
       call gasdev(u)
       spec_in(1)%part(ii, 4) = temp(2)*u
       spec_in(1)%part(ii, id_ch) = wgh_cmp
      end do
      np_el = np_el + inc
     end if
    end do
   case (3)
    do n = 1, np
     inc = ion_ch_inc(n)
     if (inc>0) then
      wgh_cmp = spec_in(ic)%part(n, id_ch)
      charge = -one_int_hp
      part_ind = -one_int_hp
      wgh = wgh*n_mol_atoms(ic - 1)
      do i = 1, inc
       ii = ii + 1
       spec_in(1)%part(ii, 1:3) = spec_in(ic)%part(n, 1:3)
       call gasdev(u)
       spec_in(1)%part(ii, 4) = temp(1)*u
       call gasdev(u)
       spec_in(1)%part(ii, 5) = temp(2)*u
       call gasdev(u)
       spec_in(1)%part(ii, 6) = temp(3)*u
       spec_in(1)%part(ii, id_ch) = wgh_cmp
      end do
      np_el = np_el + inc
     end if
    end do
   end select
   loc_npart(imody, imodz, imodx, 1) = np_el
   !============ Now create new_np_el electrons
  end subroutine
  !=======================================
  subroutine env_ionization_electrons_inject(spec_in, sp_field, ion_ch_inc, ic, &
    np, np_el, new_np_el)

   type(species), dimension(:), intent(inout) :: spec_in
   real (dp), intent (in) :: sp_field(:, :)
   integer, intent (in) :: ion_ch_inc(:)
   integer, intent (in) :: ic, np, new_np_el
   integer, intent (inout) :: np_el
   integer (sp) :: inc, id_ch
   real (dp) :: u, temp(3), a_symm
   integer :: n, i, ii
   !========== Enter sp_field(n,1)= the rms momenta Delta*a (n) for env model
   !                 inc=ion_ch_inc(n) the number of ionization electrons
   id_ch = nd2 + 1

   temp(1) = t0_pl(1)
   temp(2:3) = temp(1)
   ii = np_el
   !if(ii==0)write(6,'(a33,2I6)')'warning, no electrons before ionz',imody,imodz
   select case (curr_ndim)
   case (2)
    do n = 1, np
     inc = ion_ch_inc(n)
     if (inc>0) then
      wgh_cmp = spec_in(ic)%part(n, id_ch)
      charge = -one_int_hp
      part_ind = -one_int_hp
      wgh = wgh*n_mol_atoms(ic - 1)
      do i = 1, inc
       ii = ii + 1
       spec_in(1)%part(ii, 1:2) = spec_in(ic)%part(n, 1:2)
       call gasdev(u)
       spec_in(1)%part(ii, 3) = temp(1)*u
       call gasdev(u)
       spec_in(1)%part(ii, 4) = sp_field(n, 1)*u
       spec_in(1)%part(ii, id_ch) = wgh_cmp
      end do
      np_el = np_el + inc
     end if
    end do
   case (3)
    do n = 1, np
     inc = ion_ch_inc(n)
     if (inc>0) then
      wgh_cmp = spec_in(ic)%part(n, id_ch)
      charge = -one_int_hp
      part_ind = -one_int_hp
      wgh = wgh*n_mol_atoms(ic - 1)
      do i = 1, inc
       ii = ii + 1
       spec_in(1)%part(ii, 1:3) = spec_in(ic)%part(n, 1:3)
       call gasdev(u)
       spec_in(1)%part(ii, 4) = temp(1)*u
       call gasdev(u)
       spec_in(1)%part(ii, 5) = sp_field(n, 1)*u
       a_symm = sp_field(n, 1)*sqrt(2.0)
       spec_in(1)%part(ii, 6) = sin(2.0*pig*u)*a_symm
       spec_in(1)%part(ii, id_ch) = wgh_cmp
      end do
      np_el = np_el + inc
     end if
    end do
   end select
   loc_npart(imody, imodz, imodx, 1) = np_el
   !============ Now create new_np_el electrons
  end subroutine
  !===============================
  subroutine part_ionize(sp_loc, amp_aux, np, ic, new_np_el, ion_ch_inc)

   type(species), intent(inout) :: sp_loc
   real(dp), intent(inout) :: amp_aux(:, :)

   integer, intent(in) :: np, ic
   integer, intent(inout) :: new_np_el
   integer, intent(inout) :: ion_ch_inc(:)
   real(dp) :: p
   integer :: n, nk, kk, z0
   integer :: kf, id_ch, sp_ion
   real(dp) :: ef_ion
   !=====================
   ! Units Ef_ion is in unit mc^2/e=2 MV/m
   ! Hence E0*Ef_ion, E0=0.51 is the electric field in MV/m
   ! The reference value in ADK formula is Ea= 1a.u. 0.514 MV/m,
   ! then Ef_ion/Ea= Ef_approximates the field in code units.
   ! Vfact(z,ic)=(V/V_H)^(3/2) where V_H is the Hydrogen ionization energy
   !              V(z,ic) is the potential  to ionize ion(ic) z-1 => z
   !===============================
   ! enters nk=ion_ch_inc(n) the index of field modulus on ion n=1,np
   ! exit  ion_ch_inc(n)= the number(0,1, ionz_lev) of ionization electrons of ion n=1,np
   ! exit sp_aux(n,id_ch)=Delta*a= sqrt(1.5*Ef/Vfact(Z,ic))*a_n for envelope model
   !=======================

   !energy_norm=1./energy_unit
   id_ch = nd2 + 1
   kf = curr_ndim
   sp_ion = ic - 1
   kk = 0
   !===========================
   select case (ionz_lev)
   case (1)
    !========= Only one level ionization usining  adk model
    do n = 1, np
     nk = ion_ch_inc(n) !the ioniz field grid value on the n-th ion E_f=nk*dge
     wgh_cmp = sp_loc%part(n, id_ch)
     z0 = charge !the current ion Z charge, charge is short_int
     ion_ch_inc(n) = 0
     call random_number(p)
     if (p < w_one_lev(nk, z0, sp_ion)) then
      charge = charge + one_int_hp
      ion_ch_inc(n) = 1 !the ionization electron count
      z0 = z0 + 1
      sp_loc%part(n, id_ch) = wgh_cmp !the new ion (id,z-chargei,wgh)
      ef_ion = 1.5*amp_aux(n, 1)/vfact(z0, sp_ion)
      if (ef_ion > 0.0) amp_aux(n, 1) = sqrt(ef_ion)*amp_aux(n, 2) !Delta*|A| on ion(n,ic)
      kk = kk + 1
     end if
     !ion_ch_inc(n)=0 or 1
    end do
    new_np_el = kk
    !============= old ion charge stored in ebfp(id_ch)
   case (2)
    new_np_el = 0
    write (6, *) 'WARNING :    two-step ionization no yet activated'
    return
   end select
   !============= old ion charge stored in ebfp(id_ch)
   !================= Exit
  end subroutine
  !====================
  subroutine ionization_cycle(spec_in, sp_aux, np, ic, itloc, mom_id, &
    def_inv)
   type (species), intent (inout), dimension(:) :: spec_in
   real (dp), allocatable, intent (inout) :: sp_aux(:, :)
   integer, intent (in) :: np, ic, itloc, mom_id
   real (dp), intent (in) :: def_inv
   integer :: id_ch, old_np_el, new_np_el, new_np_alloc, n, nk
   real(dp) :: ef2_ion, ef_ion

   new_np_el = 0
   id_ch = nd2 + 1
   !==================
   ! In sp_aux(id_ch) enters the |E|^2 env field assigned  to each ion
   ! np is the number of ions
   !==================
   ! spec_in(np,1:id_ch) is the array structure of ions coordinates, charge and weight
   !==========================
   !mom_id=1  select ionization procedure for envelope
   !mom_id=0 for other models
   !==============================
   !======== First check memory for auxiliary arrays=============
   if (allocated(el_ionz_count)) then
    if (size(el_ionz_count, 1) < np) then
     deallocate (el_ionz_count)
     allocate (el_ionz_count(np + 100))
    end if
   else
    allocate (el_ionz_count(np + 100))
   end if
   el_ionz_count(:) = 0
   if (allocated(efp_aux)) then
    if (size(efp_aux, 1) < np) then
     deallocate (efp_aux)
     allocate (efp_aux(np + 100, 2))
    end if
   else
    allocate (efp_aux(np + 100, 2))
   end if
   efp_aux(:, :) = 0.0
   efp_aux(1:np, 1) = sp_aux(1:np, id_ch)
   efp_aux(1:np, 2) = sp_aux(1:np, id_ch - 1)
   !=========================
   ! In efp_aux(n,1) is the ionizing field squared |E|^2 on ions n=1,np
   ! For envelope model : in efp_aux(n,2) is the env |A| value on ions n=1,np
   !=========================

   do n = 1, np
    ef2_ion = efp_aux(n, 1) !the interpolated E^2 field
    if (ef2_ion > 0.) then
     ef_ion = sqrt(ef2_ion)
     nk = nint(def_inv*ef_ion)
     efp_aux(n, 1) = ef_ion
     el_ionz_count(n) = nk
     !for each ion index n nk(n) is the ionizing fiels grid index
    end if
   end do
   !=====================
   ! The ionizing field ef_ion=|E| discretized to a grid.
   !            Ef(n)=nk*DE=nk*dge=nk/def_inv
   ! Grid index nk stored in el_ionz_count(n)
   !====================
   call part_ionize(spec_in(ic), efp_aux, np, ic, new_np_el, el_ionz_count)
   !======= In part_ionize:
   ! The transition probality from ion charge z_0 => z_0 +1 is evaluated using
   ! the probability table W_one_lev(nk,z0,sp_ion)
   !=====================
   !EXIT in el_ionz_count(n) the numeber of ionization electrons (=0 or =1) on each ion(n)
   ! For envelope model in efp_aux(n,1)=sqrt(1.5*E_f/Vfact(z1))*|A|
   ! To modelize the rms P_y moment of ionization electron
   !==========================
   !=========== CHECK FOR MEMORYof electron struct array ================
   if (new_np_el > 0) then
    old_np_el = loc_npart(imody, imodz, imodx, 1)
    new_np_alloc = old_np_el + new_np_el
    loc_ne_ionz(imody, imodz, imodx) = loc_ne_ionz(imody, imodz, imodx) &
                                       + new_np_el
    !==========
    if (allocated(spec_in(1)%part)) then
     if (size(spec_in(1)%part, 1)<new_np_alloc) then
      do n = 1, old_np_el
       sp_aux(n, 1:id_ch) = spec_in(1)%part(n, 1:id_ch)
      end do
      deallocate (spec_in(1)%part)
      allocate (spec_in(1)%part(new_np_alloc,id_ch))
      do n = 1, old_np_el
       spec_in(1)%part(n, 1:id_ch) = sp_aux(n, 1:id_ch)
      end do
     end if
    else
     allocate (spec_in(1)%part(new_np_alloc,id_ch))
     write (6, '(a37,2I6)') &
      'warning, electron array not previously allocated', imody, imodz
    end if
    call v_realloc(sp_aux, new_np_alloc, id_ch)
    !=========== and then Inject new electrons================
    select case (mom_id)
    case (0)
     call ionization_electrons_inject(spec_in, el_ionz_count, ic, np, old_np_el, &
       new_np_el)
    case (1)
     call env_ionization_electrons_inject(spec_in, efp_aux, el_ionz_count, ic, &
       np, old_np_el, new_np_el)
    end select
   end if
   !Ionization energy to be added to the plasma particles current
  end subroutine
  !=======================================
 end module
