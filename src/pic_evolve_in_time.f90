 !*****************************************************************************************************!
 !             Copyright 2008-2016 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
 !                                 Alberto Marocchino                                                  !
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

 module pic_evolve_in_time
 use precision_def
 use pic_rutil
 use particles
 use grid_fields

 implicit none
 integer,parameter :: x_parity(6)=(/-1,1,1,-1,1,1/)
 integer,parameter :: y_parity(6)=(/1,-1,1,1,-1,1/)
 integer,parameter :: z_parity(6)=(/1,1,-1,1,1,-1/)
 !===============================
 ! MOVING WINDOW SECTION
 !=============================
 contains
 subroutine sort_particles(sp_loc,pt,&
  np,i2,j2,k2,xm,ym,zm)
 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np,i2,j2,k2
 real(dp),intent(in) :: xm,ym,zm
 integer :: j,k,n,n1,n2,n3
 integer :: ip,jp,kp,ndv,kk,nk,n12
 integer,allocatable :: p_count(:)
 !
 ndv=nd2+1
 !============== in buffer the cell index of each particle
 n1=i2-2
 n2=j2-2
 n12=n1*n2
 select case(curr_ndim)
 case(2)
  nk=n12
  allocate(p_count(nk))
  p_count=0
  pt(1:np,1:ndv)=sp_loc%part(1:np,1:ndv)
  do n=1,np
   ip=1+int(dx_inv*(pt(n,1)-xm))
   jp=1+int(dy_inv*(pt(n,2)-ym))
   kk=1+ip+(jp-1)*n1
   p_count(kk)=p_count(kk)+1
  end do
  !================= kk=2,nk p_count(1)=0
  ! converts particle_per_cell number in allocation index
  k=1
  do jp=1,nk
   j=p_count(jp)
   if( j >0)then
    p_count(jp)=k
    k=k+j
   endif
  end do
  do n=1,np
   ip=1+int(dx_inv*(pt(n,1)-xm))
   jp=1+int(dy_inv*(pt(n,2)-ym))
   kk=1+ip+(jp-1)*n1
   j=p_count(kk)
   if(j >0)then
    p_count(kk)=p_count(kk)+1
    sp_loc%part(j,1:ndv)=pt(n,1:ndv)
   endif
  end do
 case(3)
  n3=k2-2
  nk=n3*n12
  allocate(p_count(nk))
  p_count=0
  pt(1:np,1:ndv)=sp_loc%part(1:np,1:ndv)
  do n=1,np
   ip=1+int(dx_inv*(pt(n,1)-xm))
   jp=1+int(dy_inv*(pt(n,2)-ym))
   kp=1+int(dz_inv*(pt(n,3)-zm))
   kk=1+ip+(jp-1)*n1+(kp-1)*n12
   p_count(kk)=p_count(kk)+1
  end do
  ! converts particle_per_cell number in allocation index
  k=1
  do jp=1,nk
   j=p_count(jp)
   if( j >0)then
    p_count(jp)=k
    k=k+j
   endif
  end do
  do n=1,np
   ip=1+int(dx_inv*(pt(n,1)-xm))
   jp=1+int(dy_inv*(pt(n,2)-ym))
   kp=1+int(dz_inv*(pt(n,3)-zm))
   kk=1+ip+(jp-1)*n1+(kp-1)*n1*n2
   j=p_count(kk)
   if(j >0)then
    p_count(kk)=p_count(kk)+1
    sp_loc%part(j,1:ndv)=pt(n,1:ndv)
   endif
  end do
 end select
 if(allocated(p_count))deallocate(p_count)
 ! return ordered sp_loc array and p_count() location

 end subroutine sort_particles

 subroutine add_particles(np,i1,i2,ic)
 integer,intent(in) :: np,i1,i2,ic
 integer :: n,ix,j,k,j2,k2
 real(dp) :: u,tmp0,whz,wp
 real(sp) :: wch(2)
 equivalence(wch,wp)

 tmp0=t0_pl(ic)
 n=np
 wch(2)=real(unit_charge(ic),sp)
 k2=loc_nptz(ic)
 j2=loc_npty(ic)
 if(curr_ndim >2 )then
  do ix=i1,i2
   do k=1,k2
    whz=wghpt(ix,ic)*loc_wghz(k,ic)
    do j=1,j2
     wch(1)=real(whz*loc_wghy(j,ic),sp)
     n=n+1
     spec(ic)%part(n,1)=xpt(ix,ic)
     spec(ic)%part(n,2)=loc_ypt(j,ic)
     spec(ic)%part(n,3)=loc_zpt(k,ic)
     spec(ic)%part(n,4:6)=0.0
     spec(ic)%part(n,7)=wp
    end do
   end do
  enddo
 else
  do ix=i1,i2
   do j=1,j2
    wch(1)=real(loc_wghy(j,ic)*wghpt(ix,ic),sp)
    n=n+1
    spec(ic)%part(n,1)=xpt(ix,ic)
    spec(ic)%part(n,2)=loc_ypt(j,ic)
    spec(ic)%part(n,3:4)=0.0
    spec(ic)%part(n,5)=wp
   end do
  end do
 endif
 if(tmp0 >0.0)then
  n=np
  call init_random_seed(mype)
  if(curr_ndim > 2)then
   do ix=i1,i2
    do k=1,k2
     do j=1,j2
      n=n+1
      call gasdev(u)
      spec(ic)%part(n,4)=tmp0*u
      call gasdev(u)
      spec(ic)%part(n,5)=tmp0*u
      call gasdev(u)
      spec(ic)%part(n,6)=tmp0*u
     end do
    end do
   enddo
  else
   do ix=i1,i2
    do j=1,j2
     n=n+1
     call gasdev(u)
     spec(ic)%part(n,3)=tmp0*u
     call gasdev(u)
     spec(ic)%part(n,4)=tmp0*u
    end do
   end do
  endif
 endif
 end subroutine add_particles
 !---------------------------
 subroutine particles_inject(xmx)
 real(dp),intent(in) :: xmx
 integer :: ic,ix,npt_inj,np_old,np_new
 integer :: i1,i2,n,q
 integer :: j2,k2,ndv
 integer :: j,k,DeallocStatus,AllocStatus

 !========== inject particles from the right x_p >= x(nx)
 !   xmx is the box xmax
 ndv=nd2+1
 do ic=1,nsp
  i1=1+nptx(ic)
  do ix=i1,nptx_max
   if(xpt(ix,ic) > xmx)exit
  end do
  i2=ix-1
  nptx(ic)=i2
  !==========================
  npt_inj=0
  select case(ndim)
  case(1)
   do ix=i1,i2
    npt_inj=npt_inj+1
   end do
  case(2)
   j2=loc_npty(ic)
   do ix=i1,i2
    do j=1,j2
     npt_inj=npt_inj+1
    end do
   end do
  case(3)
   k2=loc_nptz(ic)
   j2=loc_npty(ic)
   do ix=i1,i2
    do k=1,k2
     do j=1,j2
      npt_inj=npt_inj+1
     end do
    end do
   end do
  end select
  np_old=loc_npart(imody,imodz,imodx,ic)
  np_new=np_old+npt_inj
  !=========================
  if(npt_inj >0)then
   if(size(spec(ic)%part,1) <np_new)then
    do n=1,np_old
     ebfp(n,1:ndv)=spec(ic)%part(n,1:ndv)
    end do
    deallocate(spec(ic)%part)
    allocate(spec(ic)%part(np_new,ndv))
    do n=1,np_old
     spec(ic)%part(n,1:ndv)=ebfp(n,1:ndv)
    end do
    if(size(ebfp,1)< np_new)then
     deallocate(ebfp)
     allocate(ebfp(np_new,ndv))
    endif
   endif
   q=np_old
   call add_particles(q,i1,i2,ic)
   loc_npart(imody,imodz,imodx,ic)=np_new
  endif
 end do
 !=======================
 end subroutine particles_inject
 !========================================
 subroutine ionization_electrons_inject(ion_ch_inc,ic,np,new_np_el)

 integer,intent(in) :: ion_ch_inc(:)
 integer,intent(in) :: ic,np,new_np_el
 integer :: np_el
 integer(sp) :: inc,id_ch,ndp
 real(sp) :: ch(2)
 real(dp) :: wgh
 equivalence(wgh,ch)
 real :: u,temp

 integer :: n,i,ii,new_np_alloc

 id_ch=nd2+1
 ndp=curr_ndim
 temp=t0_pl(1)

 np_el=loc_npart(imody,imodz,imodx,1)
 new_np_alloc=np_el+new_np_el
 if(allocated(spec(1)%part))then
  if(size(spec(1)%part,1) < new_np_alloc)then
   do n=1,np_el
    ebfp(n,1:id_ch)=spec(1)%part(n,1:id_ch)
   end do
   deallocate(spec(1)%part)
   allocate(spec(1)%part(new_np_alloc,id_ch))
   do n=1,np_el
    spec(1)%part(n,1:id_ch)=ebfp(n,1:id_ch)
   end do
  endif
 else 
  allocate(spec(1)%part(new_np_alloc,id_ch))
  write(6,'(a37,2I6)')'warning, electron array not allocated',imody,imodz
 endif
 call v_realloc(ebfp,new_np_alloc,id_ch)
 !call p_realloc(spec(1),np_el+new_np_el,id_ch)
 ii=np_el
 if(ii>0)then
  wgh=spec(1)%part(ii,id_ch)
 else
  ch(1)=j0_norm
  ch(2)=-1
  write(6,'(a33,2I6)')'warning, no electrons before ionz',imody,imodz
 endif
 select case(curr_ndim)
 case(2)
  do n=1,np
   inc=ion_ch_inc(n)
   if(inc>0)then
    do i=1,inc
     ii=ii+1
     spec(1)%part(ii,1:2)=spec(ic)%part(n,1:2)
     call random_number(u)
     u=2.*u-1.
     spec(1)%part(ii,3)=temp*u
     call random_number(u)
     u=2.*u-1.
     spec(1)%part(ii,4)=temp*u
     spec(1)%part(ii,id_ch)=wgh
    end do
    np_el=np_el+inc
   endif
  end do
 case(3)
  do n=1,np
   inc=ion_ch_inc(n)
   if(inc>0)then
    do i=1,inc
     ii=ii+1
     spec(1)%part(ii,1:3)=spec(ic)%part(n,1:3)
     call random_number(u)
     u=2.*u-1.
     spec(1)%part(ii,4)=temp*u
     call random_number(u)
     u=2.*u-1.
     spec(1)%part(ii,5)=temp*u
     call random_number(u)
     u=2.*u-1.
     spec(1)%part(ii,6)=temp*u
     spec(1)%part(ii,id_ch)=wgh
    end do
    np_el=np_el+inc
   endif
  end do
 end select
 loc_npart(imody,imodz,imodx,1)=np_el
 !============ Now create new_np_el electrons
 end subroutine ionization_electrons_inject
 !===============================
 subroutine part_ionize(sp_loc,sp_aux,&
                               np,ic,new_np_el,ion_ch_inc)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: sp_aux(:,:)

 integer,intent(in) :: np,ic
 integer,intent(inout) :: new_np_el
 integer,intent(inout) :: ion_ch_inc(:)
 real(dp),allocatable :: wpr(:)
 real(sp) :: ch(2)
 real(dp) :: ion_wch,p, p1,p2,ep(3)
 equivalence (ion_wch,ch)
 integer :: n,nk,kk
 integer :: kf,z0,loc_zmax,z1,inc,id_ch,sp_ion
 real(dp) :: energy_norm,ef2_ion
 !=====================
 ! Units Ef_ion is in unit mc^2/e=2 MV/m
 ! Hence E0*Ef_ion, E0=0.51 is the electric field in MV/m
 ! The reference value in ADK formula is Ea= 1a.u. 0.514 MV/m,
 ! then Ef_ion/Ea= Ef_ion in code units.
 ! V(i) are the ionization energies (eV) and
 ! Vfact(i)=(V/V_H)^(3/2) where V_H is the Hydrogen ionization energy
 !===============================

 energy_norm=1./energy_unit
 id_ch=7
 if(ndim < 3)id_ch=5
 kf=curr_ndim
 sp_ion=ic-1
 kk=0
 !===========================
 select case(ionz_lev)
 case(1)
  !========= Only one level ionization and adk model
  do n=1,np
   nk=ion_ch_inc(n)    !the field grid value on the n-th ion
   ion_wch=sp_loc%part(n,id_ch)
   z0=int(ch(2))     !the ion Z charge
   ion_ch_inc(n)=0
   call random_number(p)
   if(p < W_one_lev(nk,z0,sp_ion))then
    !ep(1:kf)=sp_aux(kf+1:2*kf,n)  !the ion current positions
    z1=z0+1
    ion_ch_inc(n)=1
    ch(2)=z1
    sp_loc%part(n,id_ch)=ion_wch
    kk=kk+1
    ef2_ion=V_norm(z0,sp_ion)
    sp_aux(kk,id_ch)=ch(1)*ef2_ion*energy_norm
    !sp_aux(1:kf,kk)=ep(1:kf)
    !to be multiplied by E_i/E^2 on a grid at ion position
   endif
  end do
  new_np_el=kk
  !============= old ion charge stored in ebfp(id_ch)
  !================= Exit
 case(2)
  !the field modulus interpolated at the ion particle
  loc_zmax=ion_max(sp_ion)
  do n=1,np
   nk=ion_ch_inc(n)
   ef2_ion=0.0
   !ep(1:kf)=sp_aux(kf+1:2*kf,n)
   if(nk >0)then
    ion_wch=sp_loc%part(n,id_ch)
    z0=int(ch(2))
    if(z0 < loc_zmax)then
     z1=z0
     inc=0
     call random_number(p)
     call set_ionization_rate(z0,nk,ef2_ion,z1)
     inc=z1-z0
     ion_ch_inc(n)=inc
     ch(2)=z1
     sp_loc%part(n,id_ch)=ion_wch
     new_np_el=new_np_el+inc
     if(inc >0)then
      kk=kk+1
      sp_aux(kk,id_ch)=ch(1)*ef2_ion*energy_norm
      sp_aux(kk,1:kf)=ep(1:kf)
     endif
     !to be multiplied by E_i/E^2 on a grid
    endif
   endif
  end do
 endselect
 !============= old ion charge stored in ebfp(id_ch)
 !================= Exit
 contains
 subroutine set_ionization_rate(n0,nk,E_f,new_z)
 integer(sp),intent(in) :: n0,nk
 real(dp),intent(inout) :: E_f
 integer(sp),intent(inout) :: new_z
 integer(sp) :: k
 !=========== nk is the field grid index
 !            n0 is the current charge state n0=0,1,..,zmax-1
 !            cumulative transitions n0 -> n0+k
 do k=0,loc_zmax-n0
  wpr(k)=Wsp(nk,k,n0,sp_ion)
 end do
 !===================================
 E_f=0.0
 p1=wpr(0)
 if(p > p1)then
  do k=1,loc_zmax-n0
   E_f=E_f+V_norm(k+n0,sp_ion)
   p2=wpr(k)
   if(p1 < p.and.p2 >=p)then
    new_z=n0+k
    return
   endif
   p1=p2
  enddo
  p1=wpr(loc_zmax-n0)
  if(p1 < p)then
   new_z=loc_zmax
   E_f=E_f+V_norm(loc_zmax,sp_ion)
  endif
 endif
 !===================== sets the new ion charge Z=new_z, new_z=n0+1, zmax
 ! new_z-n0 electrons have to be generated.
 end subroutine set_ionization_rate

 end subroutine part_ionize
 !
 subroutine ionization_cycle(sp_loc,sp_aux,np,ic,itloc,def_inv)
 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: sp_aux(:,:)
 integer,intent(in) :: np,ic,itloc
 real(dp),intent(in) :: def_inv
 integer :: id_ch,new_np_el,n,nk
 real(dp) :: ef2_ion

 new_np_el=0
 id_ch= nd2+1
 if(itloc==0)then
  call init_random_seed(mype)
 endif
 !==================
 ! Assigns the |E| field on each ion
 if(np >0)then
  if(size(el_ionz_count,1)< np)then
   deallocate(el_ionz_count)
   allocate(el_ionz_count(np+100))
  endif
  el_ionz_count(1:np)=0

  do n=1,np
   ef2_ion=sp_aux(n,id_ch)   !the interpolated E^2 field
   if(ef2_ion >0.)then
    nk=nint(def_inv*sqrt(ef2_ion))
    el_ionz_count(n)=nk
   endif
  end do
  call part_ionize(&
             sp_loc,sp_aux,np,ic,new_np_el,el_ionz_count)
  !if(ionz_count >0)call ionization_energy(ef,jc,sp_aux,ionz_count,ndim,xm,ym,zm)
  !===========
  if(new_np_el >0)then
   call ionization_electrons_inject(el_ionz_count,ic,np,new_np_el)
  endif
 endif
 !Ionization energy to be added to the plasma particles current
 end subroutine ionization_cycle
 !=======================================
 subroutine coordinate_xshift(vb,dt_loc)
 real(dp),intent(in) :: vb,dt_loc
 integer :: i,ndv
 integer :: ic,ix,npmax,npmin,npt_inj,old_np
 integer :: ii,j,k,i1,i2,n,q,inc
 integer :: j2,k2
 real(dp) :: xm_box
 !======================
 do i=1,nx+1
  xw(i)=xw(i)+dt_loc*vb
 end do
 if(.not.Part)return
 ndv=nd2+1
 xm_box=x(nx)
 do ic=1,nsp
  do i=nptx(ic),nptx_max
   xpt(i,ic)=xpt(i,ic)-vb*dt_loc
  end do
 end do
 inc=0
 do ic=1,nsp
  i1=nptx(ic)
  i2=i1
  do ix=i1,nptx_max
   if(xpt(ix,ic)<xm_box)i2=ix
  end do
  i1=i1+1
  inc=i2+1-i1
  nptx(ic)=i2
  ii=1
  npt_inj=0
  k2=loc_nptz(ic)
  j2=loc_npty(ic)
  do i=i1,i2
   do k=1,k2
    do j=1,j2
     npt_inj=npt_inj+1
    end do
   end do
  end do
  !=========================
  if(npt_inj >0)then
   old_np=loc_npart(imody,imodz,imodx,ic)
   if(old_np >0)then
    npmax=old_np+npt_inj
    loc_npart(imody,imodz,imodx,ic)=npmax
    if(allocated(ebfp))then
     call v_realloc(ebfp,npmax,ndv)
     do n=1,old_np
      ebfp(n,1:ndv)=spec(ic)%part(n,1:ndv)
     end do
     call p_realloc(spec(ic),npmax,ndv)
     do n=1,old_np
      spec(ic)%part(n,1:ndv)=ebfp(n,1:ndv)
     end do
     q=old_np
    else
     if(ic==1)allocate(ebfp(npt_inj,ndv))
     allocate(spec(ic)%part(npt_inj,ndv))
     q=npt_inj
    endif
    call add_particles(q,i1,i2,ic)
   endif
  endif
 end do
 !=======================
 npmax=0
 npmin=loc_npart(imody,imodz,imodx,1)
 do ic=1,nsp
  npmax=max(npmax,loc_npart(imody,imodz,imodx,ic))
  npmin=min(npmin,loc_npart(imody,imodz,imodx,ic))
 end do
 np_max=npmax
 np_min=npmin
 !=======================
 if(prl)call part_numbers

 end subroutine coordinate_xshift
 !====================================
 subroutine reset_loc_xgrid
 integer :: p,ip,i,ii,n_loc

 p=0
 n_loc=loc_xgrid(p)%ng
 loc_xg(0,1,p)=x(1)-dx
 loc_xg(0,2,p)=xh(1)-dx
 do i=1,n_loc+1
  loc_xg(i,1,p)=x(i)
  loc_xg(i,2,p)=xh(i)
 end do
 ip=loc_xgrid(0)%ng
 if(npe_xloc >2) then
  do p=1,npe_xloc-2
   n_loc=loc_xgrid(p-1)%ng
   loc_xg(0,1:2,p)=loc_xg(n_loc,1:2,p-1)
   n_loc=loc_xgrid(p)%ng
   do i=1,n_loc+1
    ii=i+ip
    loc_xg(i,1,p)=x(ii)
    loc_xg(i,2,p)=xh(ii)
   end do
   loc_xgrid(p)%gmin=loc_xgrid(p-1)%gmax
   ip=ip+n_loc
   loc_xgrid(p)%gmax=x(ip+1)
  end do
 endif
 p=npe_xloc-1
 n_loc=loc_xgrid(p-1)%ng
 loc_xg(0,1:2,p)=loc_xg(n_loc,1:2,p-1)
 n_loc=loc_xgrid(p)%ng
 do i=1,n_loc+1
  ii=i+ip
  loc_xg(i,1,p)=x(ii)
  loc_xg(i,2,p)=xh(ii)
 end do
 loc_xgrid(p)%gmin=loc_xgrid(p-1)%gmax
 ip=ip+n_loc
 loc_xgrid(p)%gmax=x(ip+1)
 end subroutine reset_loc_xgrid

 subroutine LP_window_xshift(dt_loc,witr,wt)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: witr,wt
 integer :: i1,n1p,j1,nyp,k1,nzp
 integer :: ix,shx,wi2
 real(dp),save :: xlapse
 integer,save :: wi1
 logical,parameter :: mw=.true.

 if(wt==0)then
  xlapse=0.0
  wi1=0
  return
 endif
 !==================
 i1=loc_xgrid(imodx)%p_ind(1)
 n1p=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 !======================
 xlapse=xlapse+w_speed*dt_loc*witr
 wi2=nint(dx_inv*xlapse)
 shx=wi2-wi1
 wi1=wi2
 do ix=1,nx+1
  x(ix)=x(ix)+dx*shx
  xh(ix)=xh(ix)+dx*shx
 end do
 xmin=xmin+dx*shx
 xmax=xmax+dx*shx
 xp0_out=xp0_out+dx*shx
 xp1_out=xp1_out+dx*shx
 loc_xgrid(imodx)%gmin=loc_xgrid(imodx)%gmin+dx*shx
 loc_xgrid(imodx)%gmax=loc_xgrid(imodx)%gmax+dx*shx
 wi2=n1p-shx
 if(wi2<=0)then
  write(6,'(a37,3i6)')'Error in window shifting for MPI proc',imody,imodz,imodx
  ier=2
  return
 endif
 !===========================
 call fields_left_xshift(ebf,i1,wi2,j1,nyp,k1,nzp,1,nfield,shx)
 if(ibeam==2)call fields_left_xshift(pot,i1,wi2,j1,nyp,k1,nzp,1,2,shx)
 if(Envelope)then
  call fields_left_xshift(env,i1,wi2,j1,nyp,k1,nzp,1,2,shx)
  call fields_left_xshift(env0,i1,wi2,j1,nyp,k1,nzp,1,2,shx)
 endif
 !shifts fields data and inject right ebf(wi2+1:n1p) x-grid shx new data
 !===========================
 if(Part)then
  call cell_part_dist(mw)   !particles are redistributes along the
  ! new x-coordinate in MPI domains
  if(pex1)then
   if(targ_in<=xmax.and.targ_end >xmax)then
    call particles_inject(xmax)
   endif
  endif
  if(prl)call part_numbers
 endif
 end subroutine LP_window_xshift
 !==============================

 !=======================================
 subroutine BUNCH_window_xshift(dt_loc,witr,wt)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: witr,wt
 integer :: i1,n1p,j1,nyp,k1,nzp
 integer :: ix,shx,wi2,w2f
 real(dp),save :: xlapse
 integer,save :: wi1
 logical,parameter :: mw=.true.

 if(wt==0)then
  xlapse=0.0
  wi1=0
  return
 endif
 !==================
 !i1=sh_ix;n1=nx+i1-1
 i1=loc_xgrid(imodx)%p_ind(1)
 n1p=loc_xgrid(imodx)%p_ind(2)
 !=======================
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 !========== bunch fields have enlarged stencil of (y,z)points
 xlapse=xlapse+w_speed*dt_loc*witr
 wi2=nint(dx_inv*xlapse)
 shx=wi2-wi1
 wi1=wi2
 do ix=1,nx+1
  x(ix)=x(ix)+dx*shx
  xh(ix)=xh(ix)+dx*shx
 end do
 xmin=xmin+dx*shx
 xmax=xmax+dx*shx
 loc_xgrid(imodx)%gmin=loc_xgrid(imodx)%gmin+dx*shx
 loc_xgrid(imodx)%gmax=loc_xgrid(imodx)%gmax+dx*shx
 !====================
 w2f=n1p-shx
 if(w2f<=0)then
  write(6,*)'Error in window shifting in task',imody,imodz,imodx
  ier=2
  return
 endif
 !===========================
 call fields_left_xshift(ebf,i1,w2f,j1,nyp,k1,nzp,1,nfield,shx)
 call fields_left_xshift(&
  ebf_bunch,i1,w2f,j1,nyp,k1,nzp,1,nbfield,shx)
 if(ibeam==1)call fields_left_xshift(&
  ebf1_bunch,i1,w2f,j1,nyp,k1,nzp,1,nbfield,shx)
 !========================================
 if(Part)then
  call cell_part_dist(mw)
  if(pex1)then
   if(targ_in<=xmax.and.targ_end >xmax)then
    call particles_inject(xmax)
   endif
  endif
  if(prl)call part_numbers
 endif
 !===========================
 end subroutine BUNCH_window_xshift
 !========================================
 ! END OF SECTION FOR MOVING WINDOW
 !===========================
 ! SECTION CONTAINING GRID ASSIGNEMENTS OF PARTICLE PHASE
 ! SPACE COORDINATES:
 !=> charge density, energy and momenta density, current density
 !===========================
 subroutine collect_bunch_and_plasma_density(this_bunch,isp)
 !=======================================
 !========== bunch density and particles of species isp added on jc(ic)
 !=========================================
 integer,intent(in) :: this_bunch,isp
 integer :: nyf,nzf,np,nb,i1,i2,j1,k1
 real(dp) :: xm,ym,zm,dery,derz
 integer :: ik,i,j,k,jj,kk,nst

 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyf=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzf=loc_zgrid(imodz)%p_ind(2)

 do i=1,2
  jc(:,:,:,i)=0.0
 end do

 nst=0
 if(Stretch)nst=str_indx(imody,imodz)
 np=loc_npart(imody,imodz,imodx,isp)
 if(this_bunch==0)then
  do ik=1,nsb
   nb=loc_nbpart(imody,imodz,imodx,ik)
   if(nb>0)then
    call set_grid_charge(&
     bunch(ik),ebfb,jc,nb,ndim,nst,1,xm,ym,zm)
   endif
  enddo
 else
  ik=this_bunch    !only the selected bunch density
  nb=loc_nbpart(imody,imodz,imodx,ik)
  if(nb>0)then
   call set_grid_charge(&
    bunch(ik),ebfb,jc,nb,ndim,nst,1,xm,ym,zm)
  endif
 endif
 !=========== bunch data on jc(1)
 !=====================
 if(np>0)then
  !==================== data of isp species on jc(2)
  call set_grid_charge(spec(isp),ebfp,jc,np,ndim,nst,2,xm,ym,zm)
 endif
 if(prl)then
  do i=1,2
   call fill_curr_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,i)
  end do
 endif
 do ik=1,2
  call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,ik)
 end do
 jc(i1:i2,j1:nyf,k1:nzf,1)=jc(i1:i2,j1:nyf,k1:nzf,1)+&
  jc(i1:i2,j1:nyf,k1:nzf,2)
 !============ on jc(1) bunch+ particles
 if(Stretch)then
  kk=1
  do k=k1,nzf
   derz=loc_zg(kk,3,imodz)
   jj=1
   do j=j1,nyf
    dery=loc_yg(jj,3,imody)*derz
    do i=i1,i2
     jc(i,j,k,1)=dery*jc(i,j,k,2)
     jc(i,j,k,2)=dery*jc(i,j,k,2)
    end do
    jj=jj+1
   end do
   kk=kk+1
  end do
 endif
 !=============================
 end subroutine collect_bunch_and_plasma_density

 subroutine prl_bden_energy_interp(ic)

 integer,intent(in) :: ic
 integer :: nyf,nzf,np,i1,i2,j1,k1
 real(dp) :: xm,ym,zm,dery,derz
 integer :: ik,i,j,k,jj,kk,nst

 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyf=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzf=loc_zgrid(imodz)%p_ind(2)

 if(Cyl_coord)ym=loc_rgrid(imody)%gmin
 !curr_clean
 do i=1,2
  jc(:,:,:,i)=0.0
 end do
 nst=0
 if(Stretch)nst=str_indx(imody,imodz)
 if(ic==0)then    !collects all bunch density
  do ik=1,nsb
   np=loc_nbpart(imody,imodz,imodx,ik)
   if(np>0)then
    call set_grid_den_energy(&
     bunch(ik),ebfb,jc,np,ndim,curr_ndim,nst,xm,ym,zm)
   endif
  end do
 else
  ik=ic    !only the ic-bunch density
  np=loc_nbpart(imody,imodz,imodx,ik)
  if(np>0)then
   call set_grid_den_energy(&
    bunch(ik),ebfb,jc,np,ndim,curr_ndim,nst,xm,ym,zm)
  endif
 endif
 !========= den on [i1-1:i2+2,j1-1:nyp+2,k1-1:nzp+2]
 if(prl)then
  call fill_curr_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,2)
 endif
 do ik=1,2
  call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,ik)
 end do
 jc(i1:i2,j1:nyf,k1:nzf,1)=-jc(i1:i2,j1:nyf,k1:nzf,1)  !positive for electrons
 if(Stretch)then
  kk=1
  do k=k1,nzf
   derz=loc_zg(kk,3,imodz)
   jj=1
   do j=j1,nyf
    dery=loc_yg(jj,3,imody)*derz
    do i=i1,i2
     jc(i,j,k,1)=dery*jc(i,j,k,1)
     jc(i,j,k,2)=dery*jc(i,j,k,2)
    end do
    jj=jj+1
   end do
   kk=kk+1
  end do
 endif
 if(Cyl_coord)then
  k=1
  jj=1
  do j=j1,nyf
   dery=loc_rg(jj,5,imody)
   do i=i1,i2
    jc(i,j,k,1)=dery*jc(i,j,k,1)
    jc(i,j,k,2)=dery*jc(i,j,k,2)
   end do
   jj=jj+1
  end do
 endif
 !======================
 !=============================
 end subroutine prl_bden_energy_interp
 !============================
 subroutine prl_den_energy_interp(ic)
 integer,intent(in) :: ic
 integer :: nyf,nzf,np,i1,i2,j1,k1,stl,str
 real(dp) :: xm,ym,zm,dery,derz
 integer :: i,j,k,jj,kk,n_str


 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyf=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzf=loc_zgrid(imodz)%p_ind(2)
 if(ic==0)then
  str=1
  stl=0
  call fill_ebfield_yzxbdsdata(ebf,i1,i2,j1,nyf,k1,nzf,1,curr_ndim,str,stl)
  call divE(ebf,jc,i1,i2,j1,nyf,k1,nzf,ndim,1)
  jc(i1:i2,j1:nyf,k1:nzf,1)=jc(i1:i2,j1:nyf,k1:nzf,1)/ompe
  return
 endif
 !=========== Construct grid-density
 n_str=0
 if(Stretch)n_str=str_indx(imody,imodz)
 do i=1,2
  jc(:,:,:,i)=0.0
 end do
 !curr_clean
 np=loc_npart(imody,imodz,imodx,ic)
 if(np>0)call set_grid_den_energy(&
  spec(ic),ebfp,jc,np,ndim,curr_ndim,n_str,xm,ym,zm)
 !========= den on [i1-1:i2+2,j1-1:nyp+2,k1-1:nzp+2]
 if(prl)then
  call fill_curr_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,2)
 endif
 do kk=1,2
  call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,kk)
 end do
 if(ic==1)jc(i1:i2,j1:nyf,k1:nzf,1)=-jc(i1:i2,j1:nyf,k1:nzf,1)
 jc(i1:i2,j1:nyf,k1:nzf,2)=mass(ic)*electron_mass*jc(i1:i2,j1:nyf,k1:nzf,2)
 !=========== energy density in Mev*n/n_0
 if(Stretch)then
  kk=1
  do k=k1,nzf
   derz=loc_zg(kk,3,imodz)
   jj=1
   do j=j1,nyf
    dery=loc_yg(jj,3,imody)*derz
    do i=i1,i2
     jc(i,j,k,1)=dery*jc(i,j,k,1)
     jc(i,j,k,2)=dery*jc(i,j,k,2)
    end do
    jj=jj+1
   end do
   kk=kk+1
  end do
 endif
 !======================
 end subroutine prl_den_energy_interp
 !=================
 subroutine prl_momenta_interp(ic)
 integer,intent(in) :: ic
 integer :: i,j,jj,k,kk,ik,nyf,nzf,np,i1,i2,j1,k1,nph_cmp,nst
 real(dp) :: xm,ym,zm,dery,derz

 nph_cmp=curr_ndim       !To assign (px,py,pz) on a grid
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyf=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzf=loc_zgrid(imodz)%p_ind(2)

 jc(:,:,:,:)=0.0
 nst=0
 if(Stretch)nst=str_indx(imody,imodz)
 !curr_clean
 np=loc_npart(imody,imodz,imodx,ic)
 if(np>0)call set_grid_momenta(spec(ic),ebfp,np,ndim,nph_cmp,nst,xm,ym,zm)
 !========= den on [i1-1:i2+2,j1-1:nyp+2,k1-1:nzp+2]
 if(prl)then
  call fill_curr_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,nph_cmp)
 endif
 do ik=1,nph_cmp
  call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,ik)
 end do
 !======== data on [1:nx,1:nyp,1:nzp]
 if(Beam)then
  np=loc_nbpart(imody,imodz,imodx,ic)
  if(np>0)call set_grid_momenta(bunch(ic),ebfb,np,ndim,nph_cmp,nst,xm,ym,zm)
  !========= den on [i1-1:i2+2,j1-1:nyp+2,k1-1:nzp+2]
  if(prl)then
   call fill_curr_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,nph_cmp)
  endif
  do ik=1,nph_cmp
   call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,ik)
  end do
  !======== data on [1:nx,1:nyp,1:nzp]
 endif
 if(Stretch)then
  do k=k1,nzf
   kk=k-2
   derz=loc_zg(kk,3,imodz)
   do j=j1,nyf
    jj=j-2
    dery=loc_yg(jj,3,imody)*derz
    do i=i1,i2
     jc(i,j,k,1:nph_cmp)=dery*jc(i,j,k,1:nph_cmp)
    end do
   end do
  end do
 endif
 !======================
 end subroutine prl_momenta_interp
 !============================
 subroutine field_xyzbd(ef,i01,i02,j01,j02,k01,k02,nc,stl,str)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i01,i02,j01,j02,k01,k02,nc,stl,str
 integer :: ik,iy,ix,iz
 integer :: i1,i2,j1,j2,k1,k2
 !==========================
 ! enter fields(1:n1,j01-1:j02+1,k01-1:k02+1,nc)
 ! Only for NON-PERIODIC boundaries
 j1=j01;j2=j02
 k1=k01;k2=k02
 i1=i01;i2=i02
 if(prly)then
  j1=j01-stl;j2=j02+str
 endif
 if(prlz)then
  k1=k01-stl;k2=k02+str
 endif
 i1=i01;i2=i02
 if(prlx)then
  i1=i01-stl;i2=i02+str
 endif
 if(pex0)then
  if(ibx< 2)then
   do ik=1,nc
    do iz=k1,k2
     do iy=j1,j2
      ef(i01-1,iy,iz,ik)=2.*ef(i01,iy,iz,ik)- &
       ef(i01+1,iy,iz,ik)
     end do
    end do
   end do
   i1=i01-1
  endif
 endif
 if(pex1)then
  if(ibx==0)then
   do ik=1,nc
    do iz=k1,k2
     do iy=j1,j2
      ef(i02+1,iy,iz,ik)=2.*ef(i02,iy,iz,ik)- &
       ef(i02-1,iy,iz,ik)
     end do
    end do
   end do
  endif
  if(ibx==1)then
   do ik=1,nc
    do iz=k1,k2
     do iy=j1,j2
      ef(i02+1,iy,iz,ik)=x_parity(ik)*ef(i02,iy,iz,ik)
     end do
    end do
   end do
   i2=i02+1
  endif
 endif
 if(ndim <2)return
 if(pe0y)then
  if(iby==0)then
   do ik=1,nc
    do iz=k1,k2
     do ix=i1,i2
      ef(ix,j01-1,iz,ik)=2.*ef(ix,j01,iz,ik)-ef(ix,j01+1,iz,ik)
     end do
    end do
   enddo
  endif
  if(iby==1)then
   do ik=1,nc
    do iz=k1,k2
     do ix=i1,i2
      ef(ix,j01-1,iz,ik)=y_parity(ik)*ef(ix,j01+1,iz,ik)
     end do
    end do
   end do
  endif
 endif
 if(pe1y)then
  if(iby==0 )then
   do ik=1,nc
    do iz=k1,k2
     do ix=i1,i2
      ef(ix,j02+1,iz,ik)=2.*ef(ix,j02,iz,ik)-ef(ix,j02-1,iz,ik)
     end do
    end do
   end do
  endif
  if(iby==1)then
   do ik=1,nc
    do iz=k01,k02
     do ix=i1,i2
      ef(ix,j02+1,iz,ik)=y_parity(ik)*ef(ix,j02-1,iz,ik)
     end do
    end do
   end do
  endif
 endif
 if(ndim <3)return
 if(pe0z)then
  if(ibz==0)then
   do ik=1,nc
    do iy=j1,j2
     do ix=i1,i2
      ef(ix,iy,k01-1,ik)=2*ef(ix,iy,k01,ik)-ef(ix,iy,k01+1,ik)
     end do
    end do
   end do
  endif
  if(ibz==1)then
   do ik=1,nc
    do iy=j1,j2
     do ix=i1,i2
      ef(ix,iy,k01-1,ik)=z_parity(ik)*ef(ix,iy,k01+1,ik)
     end do
    end do
   end do
  endif
 endif
 if(pe1z)then
  if(ibz==0)then
   do ik=1,nc
    do iy=j1,j2
     do ix=i1,i2
      ef(ix,iy,k02+1,ik)=2*ef(ix,iy,k02,ik)-ef(ix,iy,k02-1,ik)
     end do
    end do
   end do
  endif
  if(ibz==1)then
   do ik=1,nc
    do iy=j1,j2
     do ix=i1,i2
      ef(ix,iy,k01+1,ik)=z_parity(ik)*ef(ix,iy,k01-1,ik)
     end do
    end do
   end do
  endif
 endif
 end subroutine field_xyzbd
 !
 subroutine den_zyxbd(rho,i1,n1,j1,nyp,k1,nzp,ik)
 real(dp),intent(inout) :: rho(:,:,:,:)
 integer,intent(in) :: i1,n1,j1,nyp,k1,nzp,ik
 integer :: ix,iy,j2,k2
 ! Enter current data on extended ranges:

 !Enter data on the [0:nx+1][0:nyp+1][0:nzp+1] range
 j2=nyp
 k2=nzp
 if(ndim>2)then
  if(ibz==0)then
   if(pe0z)then
    do iy=j1,j2
     do ix=i1,n1
      rho(ix,iy,k1+1,ik)=rho(ix,iy,k1+1,ik)+ &
       rho(ix,iy,k1-1,ik)
      rho(ix,iy,k1,ik)=rho(ix,iy,k1+1,ik)
     end do
    end do
   endif
   if(pe1z)then
    if(ibz <2)then
     do iy=j1,j2
      do ix=i1,n1
       rho(ix,iy,nzp-1,ik)=rho(ix,iy,nzp-1,ik)+ &
        rho(ix,iy,nzp+1,ik)
       rho(ix,iy,nzp,ik)=rho(ix,iy,nzp-1,ik)
      end do
     end do
    endif
   endif
  endif
 endif
 !================
 if(ndim>1)then
  if(pe0y)then
   if(iby==0)then
    rho(i1:n1,j1+1,k1:k2,ik)=rho(i1:n1,j1+1,k1:k2,ik)+ &
     rho(i1:n1,j1-1,k1:k2,ik)
    rho(i1:n1,j1,k1:k2,ik)=rho(i1:n1,j1+1,k1:k2,ik)
   endif
   if(iby==1)then                     !radial grid
    rho(i1:n1,j1+1,k1:k2,ik)=rho(i1:n1,j1+1,k1:k2,ik)+ &
     rho(i1:n1,j1-1,k1:k2,ik)
    rho(i1:n1,j1-1,k1:k2,ik)=0.0
   endif
  endif
  if(pe1y)then
   if(iby <2)then
    rho(i1:n1,nyp-1,k1:k2,ik)=rho(i1:n1,nyp-1,k1:k2,ik)+ &
     rho(i1:n1,nyp+1,k1:k2,ik)
    rho(i1:n1,nyp,k1:k2,ik)=rho(i1:n1,nyp-1,k1:k2,ik)
   endif
  endif
 endif
 !============== field data on [y_loc]
 if(ibx <2 )then
  if(pex0)then
   rho(i1+1,j1:j2,k1:k2,ik)=rho(i1+1,j1:j2,k1:k2,ik)+ &
    rho(i1-1,j1:j2,k1:k2,ik)
   rho(i1,j1:j2,k1:k2,ik)=rho(i1+1,j1:j2,k1:k2,ik)
  endif
  if(pex1)then
   rho(n1-1,j1:j2,k1:k2,ik)=rho(n1-1,j1:j2,k1:k2,ik)+ &
    rho(n1+1,j1:j2,k1:k2,ik)
   rho(n1,j1:j2,k1:k2,ik)=rho(n1-1,j1:j2,k1:k2,ik)
  endif
 endif
 end subroutine den_zyxbd
 !====================
 !==============================
 subroutine curr_accumulate(sp_loc,pdata,npt0,npt,f_ch,n_st,xb,yb,zb)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pdata(:,:)
 integer,intent(in) :: npt0,npt,f_ch,n_st
 ! real(dp),intent(in) :: dtloc
 real(dp),intent(in) :: xb,yb,zb
 !=========================
 ! charge preservinga iform=0, 1
 !iform=0 => (D_xJx,D_yJy,D_zJz) inverted on each particle=> (Jx,Jy,Jz)
 !iform=1    only (D_xJx) to be inverted on a grid-- stretching allowed
 !iform=2    no charge preserving
 !=========================
 if(npt >0)then
  if(ndim==3)then
   select case(f_ch)
   case(0)
    call esirkepov_3d_curr(sp_loc,pdata,npt0,npt,n_st,xb,yb,zb)
   case(1)
    call esirkepov_3d_curr(sp_loc,pdata,npt0,npt,n_st,xb,yb,zb)
   case(2)
    call ncdef_3d_curr(sp_loc,pdata,npt0,npt,n_st,xb,yb,zb)
   end select
  else
   select case(f_ch)      !1D case also implemented
   case(0)
    call esirkepov_2d_curr(sp_loc,pdata,npt0,npt,n_st,curr_ndim,ndim,xb,yb)
   case(1)
    call esirkepov_2d_curr(sp_loc,pdata,npt0,npt,n_st,curr_ndim,ndim,xb,yb)
   case(2)
    call ncdef_2d_curr(sp_loc,pdata,npt0,npt,n_st,curr_ndim,ndim,xb,yb)
   end select
  endif
 endif
 !========================
 ! accumulates for each species currents on jc(i1:n1p,j1:n2p,k1:n3p,1:nc)
 !============================
 end subroutine curr_accumulate
 !==============================
 subroutine curr_mpi_collect(i1,n1p,j1,n2p,k1,n3p)

 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 integer :: i,j,k,jj,kk
 real(dp) :: dery,derhy,derz,derhz
 !============sums daata on ghost points
 if(prl)then
  call fill_curr_yzxbdsdata(jc,i1,n1p,j1,n2p,k1,n3p,nj_dim)
 endif
 call jc_xyzbd(i1,n1p,j1,n2p,k1,n3p,nj_dim)
 !=================
 if(iform < 2)then
  do i=1,ndim
   jc(i1:n1p,j1:n2p,k1:n3p,i)=djc(i)*jc(i1:n1p,j1:n2p,k1:n3p,i)
  end do
 endif
 if(ndim <2)return
 if(Stretch)then
  select case(nj_dim)
  case(2)
   do k=k1,n3p
    kk=max(k-2,1)
    derz=loc_zg(kk,3,imodz)
    do j=j1,n2p
     jj=j-2
     dery=loc_yg(jj,3,imody)
     derhy=loc_yg(jj,4,imody)
     do i=i1,n1p
      jc(i,j,k,1)=dery*derz*jc(i,j,k,1)
      jc(i,j,k,2)=derhy*derz*jc(i,j,k,2)
     end do
    end do
   end do
  case(3)
   do k=k1,n3p
    kk=k-2
    derz=loc_zg(kk,3,imodz)
    derhz=loc_zg(kk,4,imodz)
    do j=j1,n2p
     jj=j-2
     dery=loc_yg(jj,3,imody)
     derhy=loc_yg(jj,4,imody)
     do i=i1,n1p
      jc(i,j,k,1)=dery*derz*jc(i,j,k,1)
      jc(i,j,k,2)=derhy*derz*jc(i,j,k,2)
      jc(i,j,k,3)=dery*derhz*jc(i,j,k,3)
     end do
    end do
   end do
  end select
 endif
 end subroutine curr_mpi_collect
 !=================================
 subroutine jc_xyzbd(i1,n1,j1,n2,k1,n3,nc)
 integer,intent(in) :: i1,n1,j1,n2,k1,n3,nc
 integer :: ix,iy,iz,i0,j2,k2,ik
 ! Enter current data on extended ranges:
 !========== Only for Periodic BDs period=n1-1
 j2=n2;k2=n3
 if(ibx==0)then
  do ik=1,nc
   jc(i1,j1:j2,k1:k2,ik)=jc(i1,j1:j2,k1:k2,ik)+jc(i1-1,j1:j2,k1:k2,ik)
   jc(i1-1,j1:j2,k1:k2,ik)=0.0

   jc(n1,j1:j2,k1:k2,ik)=jc(n1,j1:j2,k1:k2,ik)+ &
    jc(n1+1,j1:j2,k1:k2,ik)+ &
    jc(n1+2,j1:j2,k1:k2,ik)
   jc(n1+1:n1+2,j1:j2,k1:k2,ik)=0.0
  end do
  i0=1
 endif
 if(ndim < 2)return
 if(pe0y)then
  if(iby==0)then
   do ik=1,nc
    do iz=k1,k2
     do ix=i1,n1
      jc(ix,j1,iz,ik)=jc(ix,j1,iz,ik)+ &
       jc(ix,j1-1,iz,ik)+jc(ix,j1-2,iz,ik)
      jc(ix,j1-2:j1-1,iz,ik)=0.0
     end do
    end do
   end do
  endif
  if(iby==1)then
   !Before norm  r*Jx(j)=rVxrho ==> rEx odd Jx(j-1)=-Jx(j+1)
   !             r*Jr=rVrrho ==> rEr even   Jr(j-1/2)=jr(j+1/2)
   do iz=k1,k2
    do ix=i1,n1
     jc(ix,j1+1,iz,1)=jc(ix,j1+1,iz,1)+ &
      jc(ix,j1-1,iz,1)
     jc(ix,j1,iz,2)=jc(ix,j1,iz,2)- &
      jc(ix,j1-1,iz,2)
    end do
   end do
  endif
 endif
 if(pe1y)then
  if(iby <2)then
   do ik=1,nc
    do iz=k1,k2
     do ix=i1,n1
      jc(ix,n2,iz,ik)=jc(ix,n2,iz,ik)+ &
       jc(ix,n2+1,iz,ik)+jc(ix,n2+2,iz,ik)
      jc(ix,n2+1:n2+2,iz,ik)=0.0
     end do
    end do
   end do
  endif
 endif
 if(ndim < 3)return
 if(ibz==0)then
  if(pe0z)then
   do ik=1,nc
    do iy=j1,j2
     do ix=i1,n1
      jc(ix,iy,k1,ik)=jc(ix,iy,k1,ik)+ &
       jc(ix,iy,k1-1,ik)
      jc(ix,iy,k1-1,ik)=0.0
     end do
    end do
   enddo
  endif
  if(pe1z)then
   if(ibz < 2)then
    do ik=1,nc
     do iy=j1,j2
      do ix=i1,n1
       jc(ix,iy,n3,ik)=jc(ix,iy,n3,ik)+ &
        jc(ix,iy,n3+1,ik)+jc(ix,iy,n3+2,ik)
       jc(ix,iy,n3+1:n3+2,ik)=0.0
      end do
     end do
    enddo
   endif
  endif
 endif
 end subroutine jc_xyzbd
 !-----------------------------------------------
 ! END SECTION ON GRID DEFINED PARTICLE VARIABLES
 !================================================
 !==========================
 subroutine pfields_prepare(ef,i1,i2,j1,j2,k1,k2,nc,spl_in,spr_in)
 real(dp),intent(inout) :: ef(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,nc,spr_in,spl_in
 integer :: spr,spl
 !===================
 ! Enter fields ef[i1:i2,j1,j2,k1,k2,1:nc)
 ! Exit fields ef[i1:i2,j1,j2,k1,k2,1:nc)
 !===========================
 spl=spl_in
 spr=spr_in
 if(spl >2)spl=2
 if(spr >2)spr=2
 if(prl)then
  call fill_ebfield_yzxbdsdata(ef,i1,i2,j1,j2,k1,k2,1,nc,spr,spl)
  !======================
  ! Adds point data => spl to the left spr to the right
  !==================
 endif
 call field_xyzbd(ef,i1,i2,j1,j2,k1,k2,nc,spl,spr)
 ! extends for one point at the box boundaries
 !==================================================
 end subroutine pfields_prepare
 !==================================
 subroutine advance_lpf_fields(ef,curr,dt_lp,v_b,&
  i1,i2,j1,j2,k1,k2,ibd)

 real(dp),intent(inout) :: ef(:,:,:,:)
 real(dp),intent(in) :: curr(:,:,:,:)
 real(dp),intent(in) :: dt_lp,v_b
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ibd
 integer :: str,stl,ik
 real(dp) :: dtx,dty,dtz,dth_lp,dthx,dthy,dthz

 dth_lp=0.5*dt_lp

 dtx=dx_inv*dt_lp
 dty=dy_inv*dt_lp
 dtz=dz_inv*dt_lp

 dthx=0.5*dtx
 dthy=0.5*dty
 dthz=0.5*dtz
 if(Comoving)then
  call field_xadvect(ef,i1,i2,j1,j2,k1,k2,1,nfield,dtx,-v_b)
 endif

 !=======================
 ! A LPF order Lpf with time-centered source term
 !=============================
 if(prl)then
  str=0
  stl=1
  call fill_ebfield_yzxbdsdata(ef,i1,i2,j1,j2,k1,k2,1,curr_ndim,str,stl)
  ! Periodic BDs are here
  ! to fill electric field data
  ! sends stl to the left,
  ! recvs stl points from right at (nyp+stl), (nzp+stl)
 endif
 !============== first substep dt/2 advance of B-field
 call ef_bds(ef,i1,i2,j1,j2,k1,k2,0.0,ibd)
 ! Uses upper BCs of E fields: ibd=0 for inflow-outflow
 !                             ibd=1 for symmetric
 !  (B,E)_n => B_{n+1/2}
 !============================
 call rotE(ef,i1,i2,j1,j2,k1,k2,dthx,dthy,dthz)
 !=============================
 !============== central tep dt advance of E-field
 if(prl)then
  str=1
  stl=0
  call fill_ebfield_yzxbdsdata(ef,i1,i2,j1,j2,k1,k2,curr_ndim+1,nfield,str,stl)
  ! sends nyp+1-str to the right
  ! recvs str points from left at (1-str)
 endif
 !======================
 call bf_bds(ef,i1,i2,j1,j2,k1,k2,dt_lp,ibd)
 !=======================
 ! Uses left BCs of B fields
 !(E,B)_{n+1/2} => E_{n+1}
 !==================
 call rotB(ef,i1,i2,j1,j2,k1,k2,dtx,dty,dtz)
 !===================
 ! adds currents
 do ik=1,curr_ndim
  ef(i1:i2,j1:j2,k1:k2,ik)=ef(i1:i2,j1:j2,k1:k2,ik)-&
   ompe*curr(i1:i2,j1:j2,k1:k2,ik)
 end do
 !============== second substep dt/2 advance of B-field
 if(prl)then
  str=0
  stl=1
  call fill_ebfield_yzxbdsdata(ef,i1,i2,j1,j2,k1,k2,1,curr_ndim,str,stl)
 endif
 ! E field gets stl points from right (nyp+stl), (nzp+stl)
 !(E_{n+1},B_{n+1/2}) => B_{n+1}
 !===================
 call ef_bds(ef,i1,i2,j1,j2,k1,k2,dt_lp,ibd)
 call rotE(ef,i1,i2,j1,j2,k1,k2,dthx,dthy,dthz)
 !==============
 !===============
 end subroutine advance_lpf_fields
 !============================
 subroutine advance_rk4_fields(v_b,dt_loc,i1,nxp,j1,nyp,k1,nzp,lp)

 real(dp),intent(in) :: v_b,dt_loc
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp,lp
 integer :: ix,iy,iz,ik
 integer :: str,stl
 real(dp) :: dtx,dty,dtz,dt_rk
 !==============
 ! In comoving xi=x-v_b*dt    v_b >0
 !===========================
 !========= for RK4================
 dt_rk=dt_loc*b_rk(lp)  !==> advances (E,B) fields u^i=u^0+dt_rk*f[u^(i-1)]
 dtx=dx_inv*dt_rk
 dty=dy_inv*dt_rk
 dtz=dz_inv*dt_rk

 if(lp==1)then
  do ik=1,nfield
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,nxp
      ebf0(ix,iy,iz,ik)=ebf(ix,iy,iz,ik)
      ebf1(ix,iy,iz,ik)=c_rk(0)*ebf(ix,iy,iz,ik)
     end do
    end do
   end do
  end do
 endif
 !=======================
 if(prl)then
  str=2
  stl=2
  call fill_ebfield_yzxbdsdata(&
   ebf,i1,nxp,j1,nyp,k1,nzp,1,nfield,str,stl)
 endif
 call ef_bds(ebf,i1,nxp,j1,nyp,k1,nzp,0.0,0)
 call bf_bds(ebf,i1,nxp,j1,nyp,k1,nzp,0.0,0)
 !=======================
 ! enters jc <=[dt*b^i]*J(x,v)^{i-1}
 !===============================
 do ik=1,curr_ndim
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
     jc(ix,iy,iz,ik)=ebf0(ix,iy,iz,ik)-ompe*jc(ix,iy,iz,ik)
    end do
   end do
  end do
 end do
 !======================
 ! advances E^i=E0+[rot(B)-ompe*jc]^{i-1}*dt_k stored in jc(1:3)
 ! advances B^i=B0-dt_rk*rot(E)^{i-1} inside
 ! E^i <= jc
 !-----------
 call rotEB_rk4(ebf,jc,ebf0,curr_ndim,i1,nxp,j1,nyp,k1,nzp,-v_b,dtx,dty,dtz)
 !====================
 do ik=1,curr_ndim
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
     ebf(ix,iy,iz,ik)=jc(ix,iy,iz,ik)
    end do
   end do
  end do
 end do
 do ik=1,nfield
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
     ebf1(ix,iy,iz,ik)=ebf1(ix,iy,iz,ik)+c_rk(lp)*ebf(ix,iy,iz,ik)
    end do
   end do
  end do
 end do
 if(lp==4)then
  do ik=1,nfield
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,nxp
      ebf(ix,iy,iz,ik)=ebf1(ix,iy,iz,ik)
     end do
    end do
   end do
  end do
 endif
 ! =================================
 end subroutine advance_rk4_fields
 ! =================================
 subroutine advance_lpf_envelope(curr,evf,evf0,dt_loc,i1,nxp,j1,nyp,k1,nzp)

 real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:),evf0(:,:,:,:)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp
 integer :: i,j,k,ic
 integer :: nst,str,stl,cind,ib
 real(dp) :: dtx,dty,dtz,ap

 dtx=dx_inv
 dty=dy_inv
 dtz=dz_inv
 ap=oml
 nst=0
 if(Stretch)nst=1
 !ord=2
 cind=1                !cind=0 FFT cind=1 grid deriv
 ib=der_ord-1          !implicit centered or explicit
 !optimized advection scheme
 if(Comoving)ib=0
 if(prl)then
  str=1
  stl=1
  call fill_ebfield_yzxbdsdata(&
                              evf,i1,nxp,j1,nyp,k1,nzp,1,2,str,stl)
 endif
 ! enters jc(1)=rho/<gamp> < 0
 do k=k1,nzp
  do j=j1,nyp
   do i=i1,nxp
    curr(i,j,k,2)=-ompe*curr(i,j,k,1)*evf(i,j,k,2)
    curr(i,j,k,1)=-ompe*curr(i,j,k,1)*evf(i,j,k,1)
    curr(i,j,k,3)=evf0(i,j,k,1)
    curr(i,j,k,4)=evf0(i,j,k,2)
   end do
  end do
 end do
 !  jc(1:2)=ompe*rho/<gamp>*env(1:2)  the source term
   call env_lpf_solve(jc,evf,ib,i1,nxp,j1,nyp,k1,nzp,ap,&
                      dtx,dty,dtz,dt_loc)
!===================
 evf0(i1:nxp,j1:nyp,k1:nzp,1:2)=curr(i1:nxp,j1:nyp,k1:nzp,3:4)
 do ic=1,2
  jc(:,:,:,ic)=0.0
 end do
 !====== exit env0(n+1/2) and env(n+1)
 !=============
 ! =================================
 end subroutine advance_lpf_envelope
 ! =================================
 subroutine advance_rk4_env_field(&
  curr,dt_loc,i1,nxp,j1,nyp,k1,nzp,lp,rko)

 real(dp),intent(inout) :: curr(:,:,:,:)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp,lp,rko
 integer :: ix,iy,iz,ik
 integer :: str,stl,nst,ib,ord,nc,cind
 real(dp) :: dtx,dty,dtz,dt_rk


 dt_rk=dt_loc*b_rk(lp)  !==> to advance env(1:2)
 dtx=dx_inv
 dty=dy_inv
 dtz=dz_inv
 nst=0
 ib=1
 if(vbeam > 0.0)ib=0
 ord=der_ord
 if(Stretch)nst=1
 nc=2
 !              enter jc(1)=0.5*ompe*rho/<gamp>*env(1)
 !                    jc(2)=0.5*ompe*rho/<gamp>*env(2) at (i-1) time level
 !                    rho=<qn_e> < 0
 !====enter the current source term in envelope equation
 if(lp==1)then
  do ik=1,nc
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,nxp
      env0(ix,iy,iz,ik)=env(ix,iy,iz,ik)
      env1(ix,iy,iz,ik)=c_rk(0)*env(ix,iy,iz,ik)
     end do
    end do
   end do
  end do
 endif
 !=======================
 if(prl)then
  str=2
  stl=2
  call fill_ebfield_yzxbdsdata(&
   env,i1,nxp,j1,nyp,k1,nzp,1,nc,str,stl)
 endif
 do iz=k1,nzp
  do iy=j1,nyp
   do ix=i1,nxp
    curr(ix,iy,iz,2)=ompe*curr(ix,iy,iz,1)*env(ix,iy,iz,2)
    curr(ix,iy,iz,1)=ompe*curr(ix,iy,iz,1)*env(ix,iy,iz,1)
   end do
  end do
 end do
 cind=1    !cind=0 FFT  =1 grid matrix inversion
 call env0_rk_field(curr,ord,ib,i1,nxp,j1,nyp,k1,nzp,oml,dtx,dty,dtz)
 !=============
 do ik=1,nc
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
     env(ix,iy,iz,ik)=env0(ix,iy,iz,ik)+dt_rk*curr(ix,iy,iz,ik)
     env1(ix,iy,iz,ik)=env1(ix,iy,iz,ik)+c_rk(lp)*env(ix,iy,iz,ik)
    end do
   end do
  end do
 end do
 if(lp==rko)then
  do ik=1,nc
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,nxp
      env(ix,iy,iz,ik)=env1(ix,iy,iz,ik)
     end do
    end do
   end do
  end do
 endif
 ! =================================
 end subroutine advance_rk4_env_field
 !===================
 subroutine advect_bunch_fields(efb,curr,&
  v_b,dt_lp,i1,i2,j1,j2,k1,k2,init_time)

 real(dp),intent(inout) :: efb(:,:,:,:),curr(:,:,:,:)
 real(dp),intent(in) :: v_b,dt_lp
 integer,intent(in) :: i1,i2,j1,j2,k1,k2
 logical,intent(in) :: init_time
 real(dp) :: dtx,dty,dtz,dthx,dthy,dthz
 integer :: ix,iy,iz

 dtx=dx_inv*dt_lp
 dty=dy_inv*dt_lp
 dtz=dz_inv*dt_lp

 dthx=0.5*dtx
 dthy=0.5*dty
 dthz=0.5*dtz
 !================================================
 call fill_ebfield_xbdsdata(&
  efb,i1,i2,j1,j2,k1,k2,1,nfield,1,1)
 if(init_time)then
  call field_xadvect(efb,i1,i2,j1,j2,k1,k2,4,4,dthx,-v_b)
  efb(i1:i2,j1:j2,k1:k2,4)=dt_lp*efb(i1:i2,j1:j2,k1:k2,4)
 endif
 call field_xadvect(efb,i1,i2,j1,j2,k1,k2,1,nfield,dtx,v_b)
 do iz=k1,k2
  do iy=j1,j2
   do ix=i1,i2
    curr(ix,iy,iz,1)=curr(ix,iy,iz,1)-efb(ix,iy,iz,4)
   end do
  end do
 end do
 end subroutine advect_bunch_fields
 !==================================

 ! END SECTION for TIME INTEGRATION OF EM fields
 !===============
 ! SECTION for Leap-frog integrators in LP regime
 !==============================
 subroutine set_lpf_acc(ef,sp_loc,apt,np,ndm,nf,nst,xmn,ymn,zmn)
 real(dp),intent(in) :: ef(:,:,:,:)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: apt(:,:)
 integer,intent(in) :: np,ndm,nf,nst
 real(dp),intent(in) :: xmn,ymn,zmn
 ! Uses alternating order quadratic or linear shapes

 select case(ndm)
 case(1)
  call set_part1d_acc(ef,sp_loc,apt,np,nf,xmn)
 case(2)
  call set_part2d_hcell_acc(ef,sp_loc,apt,np,nf,nst,xmn,ymn)
 case(3)
  call set_part3d_hcell_acc(ef,sp_loc,apt,np,nst,xmn,ymn,zmn)
 end select
 ! EXIT (E,B) fields multiplied by charge
 end subroutine set_lpf_acc
 !==========================
 subroutine init_lpf_momenta(sp_loc,pt,n0,np,dt_lp,Lfact)
 type(species),intent(inout) :: sp_loc
 real(dp),intent(in) :: pt(:,:)
 integer,intent(in) :: n0,np
 real(dp),intent(in) :: dt_lp,Lfact
 integer :: p
 real(dp) :: alp,dth_lp,pp(3),vp(3),efp(6),gam2,gam_inv
 real(dp) :: wgh
 real(sp) :: wch(2)
 equivalence(wgh,wch)

 dth_lp=0.5*dt_lp
 alp=dth_lp*Lfact     ! Lfact =1./m
 ! Fields are already multiplied by charge
 !=========================
 ! from p^n to p^{n-1/2}
 !==========================
 select case(curr_ndim)
 case(2)
  do p=n0,np
   wgh=sp_loc%part(p,5)  !weight-charge
   efp(1:3)=-wch(2)*alp*pt(p,1:3)   !-DT/2*charge*(Ex,Ey,Bz)^n
   pp(1:2)=sp_loc%part(p,3:4)  !p_{n}
   gam2=1.+dot_product(pp(1:2),pp(1:2))
   gam_inv=1./sqrt(gam2)
   vp(1:2)=pp(1:2)*gam_inv
   sp_loc%part(p,3)=sp_loc%part(p,3)+efp(1)+vp(2)*efp(3)
   sp_loc%part(p,4)=sp_loc%part(p,4)+efp(2)-vp(1)*efp(3)
  end do
 case(3)
  do p=n0,np
   pp(1:3)=sp_loc%part(p,4:6)
   wgh=sp_loc%part(p,7)  !weight-charge
   efp(1:6)=-wch(2)*alp*pt(p,1:6)
   gam2=1.+dot_product(pp(1:3),pp(1:3))
   gam_inv=1./sqrt(gam2)         !1/gamma
   vp(1:3)=gam_inv*pp(1:3)
   sp_loc%part(p,4)=sp_loc%part(p,4)+efp(1)+&
    vp(2)*efp(6)-vp(3)*efp(5)
   sp_loc%part(p,5)=sp_loc%part(p,5)+efp(2)+&
    vp(3)*efp(4)-vp(1)*efp(6)
   sp_loc%part(p,6)=sp_loc%part(p,6)+efp(3)+&
    vp(1)*efp(5)-vp(2)*efp(4)
  end do
 end select
 end subroutine init_lpf_momenta
 !======================================
 subroutine lpf_momenta_and_positions(sp_loc,pt,n0,np,dt_lp,vb,Lfact)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)

 integer,intent(in) :: n0,np
 real(dp),intent(in) :: dt_lp,vb,Lfact
 integer :: p,ch
 real(dp) :: alp,dth_lp,bb(3),pp(3),vp(3),vph(3),efp(6),b2,bv,gam02,gam2,gam
 real(dp) :: wgh
 real(sp) :: wch(2)
 equivalence(wgh,wch)
 !========================================
 ! uses exact explicit solution for
 ! p^{n}=(p^{n+1/2}+p^{n-1/2})/2 and gamma^n=sqrt( 1+p^n*p^n)
 ! v^n=p^n/gamma^n
 !========================================
 dth_lp=0.5*dt_lp
 alp=dth_lp*Lfact
 ch=5
 !==========================
 select case(curr_ndim)
 case(2)
  do p=n0,np
   pp(1:2)=sp_loc%part(p,3:4)  !p_{n-1/2}
   wgh=sp_loc%part(p,ch)
   efp(1:3)=wch(2)*alp*pt(p,1:3)         !charge*Lfact*(Ex,Ey,Bz)*Dt/2
   vp(1:2)=pp(1:2)+efp(1:2)   !u^{-} in Boris push
   vp(3)=efp(3)               !b_z
   gam02=1.+dot_product(vp(1:2),vp(1:2))  !gam in Boris push
   b2=vp(3)*vp(3)                         !b_z*b_z
   gam02=gam02-b2
   gam2=0.5*(gam02+sqrt(gam02*gam02+4.*b2))  !exact gam^2 solution
   gam= sqrt(gam2)
   !==============================
   !p_n=(gam2*vp+gam*(vp crossb)+b*bv/(gam2+b2)
   vph(1)=gam2*vp(1)+gam*vp(2)*vp(3)
   vph(2)=gam2*vp(2)-gam*vp(1)*vp(3)
   vph(1:2)=vph(1:2)/(gam2+b2)
   sp_loc%part(p,3:4)=2.*vph(1:2)-pp(1:2)
   !  the final step
   pt(p,3:4)=sp_loc%part(p,1:2)  !old positions stored
   pp(1:2)=sp_loc%part(p,3:4)
   gam2=1.+dot_product(pp(1:2),pp(1:2))
   pt(p,5)=dt_lp/sqrt(gam2)
   vp(1:2)=pt(p,5)*pp(1:2)
   sp_loc%part(p,1:2)=sp_loc%part(p,1:2)+vp(1:2) !new positions
   pt(p,1:2)=sp_loc%part(p,1:2)  !new positions stored
  end do
 case(3)
  ch=7
  do p=n0,np
   pp(1:3)=sp_loc%part(p,4:6)
   wgh=sp_loc%part(p,ch)
   efp(1:6)=wch(2)*alp*pt(p,1:6)      !charge*Lfact*(E,B) on p-th-particle
   vp(1:3)=pp(1:3)+efp(1:3)           !p^{-} in Boris push
   bb(1:3)=efp(4:6)
   gam02=1.+dot_product(vp(1:3),vp(1:3)) !the lower order gamma in Boris scheme
   !=============================
   b2=dot_product(bb(1:3),bb(1:3))
   bv=dot_product(bb(1:3),vp(1:3))
   gam02=gam02-b2
   gam2=0.5*(gam02+sqrt(gam02*gam02+4.*(b2+bv*bv)))  ! exact solution for gam2=1+p_n*p_n
   gam=sqrt(gam2)
   !============================
   vph(1:3)=gam2*vp(1:3)+bb(1:3)*bv
   vph(1)=vph(1)+gam*(vp(2)*bb(3)-vp(3)*bb(2))
   vph(2)=vph(2)+gam*(vp(3)*bb(1)-vp(1)*bb(3))
   vph(3)=vph(3)+gam*(vp(1)*bb(2)-vp(2)*bb(1))
   vph(1:3)=vph(1:3)/(b2+gam2)                  !p_n=(p_{n+1/2)+p_{n-1/2})/2
   !======== advance momenta
   sp_loc%part(p,4:6)=2.*vph(1:3)-pp(1:3)
   !==========
   pt(p,4:6)=sp_loc%part(p,1:3)  !stores old positions
   pp(1:3)=sp_loc%part(p,4:6)
   gam2=1.+dot_product(pp(1:3),pp(1:3))
   pt(p,7)= dt_lp/sqrt(gam2)
   vp(1:3)=pt(p,7)*pp(1:3)
   sp_loc%part(p,1:3)=sp_loc%part(p,1:3)+vp(1:3) !new positions
   pt(p,1:3)=sp_loc%part(p,1:3)  !stores new positions
  end do
 end select
 !====================
 if(iform <2)then
  !old charge stored for charge preserving schemes
  do p=n0,np
   pt(p,ch)=sp_loc%part(p,ch)
  end do
 endif
 if(int(vb) /= 0)then
  do p=n0,np
   sp_loc%part(p,1)=sp_loc%part(p,1)-dt_lp*vb
  end do
 endif
 end subroutine lpf_momenta_and_positions
 !=======================
 subroutine lpf2_evolve(t_loc,dt_loc,iter_loc,initial_time)
 real(dp),intent(in) :: t_loc,dt_loc
 integer,intent(in) :: iter_loc
 logical,intent(in) :: initial_time
 integer :: ic,np,i1,i2,j1,j2,k1,k2,n_st,id_ch
 real(dp) :: xm,ym,zm,Ltz,ef2_ion(1),loc_ef2_ion(1)
 !============================
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
 n_st=0
 if(Stretch)n_st=str_indx(imody,imodz)
 !====================
 jc(:,:,:,:)=0.0
 !curr_clean
 if(Part)then
  call pfields_prepare(ebf,i1,i2,j1,j2,k1,k2,nfield,2,2)
  do ic=1,nsp_run
   np=loc_npart(imody,imodz,imodx,ic)
   Ltz=Lorentz_fact(ic)
   if(np >0)then
    !==============
    !============
    call set_lpf_acc(ebf,spec(ic),ebfp,np,ndim,nfield,n_st,xm,ym,zm)
    if(initial_time)call init_lpf_momenta(spec(ic),ebfp,1,np,dt_loc,Ltz)
    call lpf_momenta_and_positions(spec(ic),ebfp,1,np,dt_loc,vbeam,Ltz)
    ! For each species :
    ! ebfp(4:7) store old positions and charge (at t^{n})
    !
    call curr_accumulate(spec(ic),ebfp,1,np,iform,n_st,xm,ym,zm)
    !================= only old ion charge saved
   endif
  enddo
  if(Ionization)then
   id_ch=nd2+1
   do ic=2,nsp_ionz
    np=loc_npart(imody,imodz,imodx,ic)
    if(np>0)then
     call set_ion_Efield(ebf,spec(ic),ebfp,np,n_st,ndim,nsp_run,dt_loc,xm,ym,zm)
     if(mod(iter_loc,100)==0)then     !refresh ionization tables, if needed
      loc_ef2_ion(1)=maxval(ebfp(1:np,id_ch))
      loc_ef2_ion(1)=sqrt(loc_ef2_ion(1))
      ef2_ion(1)=loc_ef2_ion(1)
      !if(prl)call allreduce_dpreal(MAXV,loc_ef2_ion,ef2_ion,1)
      if(ef2_ion(1) > lp_max)then
       lp_max=1.1*ef2_ion(1)
       call set_field_ioniz_wfunction(&
        ion_min(ic-1),atomic_number(ic-1),ic,ionz_lev,ionz_model,lp_max,dt_loc)
      endif
     endif
    endif
    call ionization_cycle(spec(ic),ebfp,np,ic,iter_loc,de_inv)
    !======== injects electrons and adds ionization energy
   end do
  endif
  !===================END IONIZATION MODULE============
  !    ions enter with new ionization levels and new electrons
  !                   are injected
  !=============================================
  !==========================================
  call curr_mpi_collect(i1,i2,j1,j2,k1,k2)
  !================ sums and normalize currents

 endif        !end particle section
 !=======================
 ! Inject fields at i=i1-1
 Lp_inject=.false.
 if(pex0)then
  if(lp_in < xm)then
   Lp_inject=.true.
   call wave_field_left_inject  !(Bz=Ey By=Ez are injected at i1-1 point
  endif
 endif
 call advance_lpf_fields(ebf,jc,dt_loc,vbeam,i1,i2,j1,j2,k1,k2,1)
 lp_in=lp_in+dt_loc
 !============================
 contains
 subroutine wave_field_left_inject

 real(dp) :: tnew,tau
 integer :: wmodel_id

 wmodel_id=model_id
 if(Plane_wave)wmodel_id=0

 tnew=t_loc    !Set inflow values [B_z{n}(i1-1/2) E_y{n}(i-1}
 tau=0.5*w0_x
 if(model_id<3) call inflow_lp_fields(&
  ebf,lp_amp,tnew,t0_lp,tau,w0_y,xf,wmodel_id,i1,j1,j2,k1,k2)
 if(model_id==3)call inflow_cp_fields(&
  ebf,lp_amp,tnew,t0_lp,tau,w0_y,xf,wmodel_id,i1,j1,j2,k1,k2)

 end subroutine wave_field_left_inject
 !-----------------------------
 end subroutine lpf2_evolve
 !===============
 ! END SECTION for Leap-frog integrators in LP regime
 !===============
 ! SECTION for RK4 integrators in LP regime
 !==============================
 subroutine set_rk_acc(sp_loc,apt,np,ndm,nf,nst,xmn,ymn,zmn)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: apt(:,:)
 integer,intent(in) :: np,ndm,nf,nst
 real(dp),intent(in) :: xmn,ymn,zmn

 select case(ndm)
 case(1)
  call set_part1d_acc(ebf,sp_loc,apt,np,nf,xmn)
 case(2)
  call set_part2d_acc(ebf,sp_loc,apt,np,nf,nst,xmn,ymn)
 case(3)
  call set_part3d_acc(ebf,sp_loc,apt,np,nst,xmn,ymn,zmn)
 end select

 end subroutine set_rk_acc
 !===============================
 subroutine advance_rk4_part(F_pt,F_pt0,F_pt1,sp_loc,np,dtloc,Lfact,lp)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: F_pt(:,:)
 real(dp),intent(out) :: F_pt0(:,:),F_pt1(:,:)

 integer,intent(in) :: np,lp
 real(dp),intent(in) :: dtloc,Lfact
 integer :: p,ndv
 real(dp) :: wgh,alp,afact,dt_lp,gam,vp(3),efp(3),fploc(6)
 real(sp) :: wch(2)
 equivalence(wgh,wch)

 dt_lp=dtloc*b_rk(lp)
 afact=dt_lp*Lfact
 ndv=2*curr_ndim
 !==========================
 ! Enter F_pt(1:3)=[Ex,Ey,Bz]
 if(lp==1)then
  do p=1,np
   F_pt0(p,1:ndv)=sp_loc%part(p,1:ndv)
   F_pt1(p,1:ndv)=c_rk(0)*F_pt0(p,1:ndv)
  end do
 endif
 select case(curr_ndim)
  ! Enter F_pt(1:3)=[Ex,Ey,Bz]
 case(2)
  do p=1,np
   wgh=sp_loc%part(p,5)         !stores p-weight and charge
   alp=wch(2)*afact
   fploc(1:4)=F_pt(p,1:4)

   vp(1:2)=sp_loc%part(p,3:4)
   gam=sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2))
   vp(1:2)=vp(1:2)/gam                     !p velocities

   efp(1)=alp*(fploc(1)+vp(2)*fploc(3))
   efp(2)=alp*(fploc(2)-vp(1)*fploc(3))
   F_pt(p,3:4)=dt_lp*vp(1:2)             !dt_rk*V^{k-1}

   sp_loc%part(p,3:4)=F_pt0(p,3:4)+efp(1:2)

   F_pt(p,1:2)=sp_loc%part(p,1:2)     !current positions
   sp_loc%part(p,1:2)=F_pt0(p,1:2)+F_pt(p,3:4) !advances positions
   F_pt(p,3:4)=wch(1)*wch(2)*F_pt(p,3:4)     !q*wgh*dt_rk*V^{k-1} => curr}
  end do
 case(3)
  !in F_pt(1:6) the (E,B) fields on a particle
  !==================================
  ! RK4 integration scheme p^i=p_0+Dt*b_i*F^{i-1},i=1,2,3,4
  do p=1,np
   wgh=sp_loc%part(p,7)         !stores p-charge
   alp=wch(2)*afact
   vp(1:3)=sp_loc%part(p,4:6)
   gam=sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3))
   vp(1:3)=vp(1:3)/gam
   fploc(1:6)=F_pt(p,1:6)

   efp(1)=alp*(fploc(1)+vp(2)*fploc(6)-vp(3)*fploc(5))
   efp(2)=alp*(fploc(2)+vp(3)*fploc(4)-vp(1)*fploc(6))
   efp(3)=alp*(fploc(3)+vp(1)*fploc(5)-vp(2)*fploc(4))

   F_pt(p,4:6)=dt_lp*vp(1:3)             !dt_rk*V^{k-1}
   sp_loc%part(p,4:6)=F_pt0(p,4:6)+efp(1:3)

   F_pt(p,1:3)=sp_loc%part(p,1:3)     !current positions
   sp_loc%part(p,1:3)=F_pt0(p,1:3)+F_pt(p,4:6) !advances positions
   F_pt(p,4:6)=wch(1)*wch(2)*F_pt(p,4:6)     !q*wgh*dt_rk*V^{k-1} => curr}
  end do
 end select
 do p=1,np
  F_pt1(p,1:ndv)=F_pt1(p,1:ndv)+c_rk(lp)*sp_loc%part(p,1:ndv)
 enddo
 if(lp==4)then
  do p=1,np
   sp_loc%part(p,1:ndv)=F_pt1(p,1:ndv)
  end do
 endif
 end subroutine advance_rk4_part
 !==============================
 subroutine rk_evolve(dt_loc,rk)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: rk
 integer :: np,ic,lps,nyf,nzf
 integer :: i1,j1,k1,i2,ndv,nst
 real(dp) :: xm,ym,zm,Ltz
 !============================
 ! Implements RK3 and RK4 schemes
 !===========================================
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyf=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzf=loc_zgrid(imodz)%p_ind(2)
 !====================
 ic=1                    !only for electrons
 ndv=nd2
 nst=0
 if(Stretch)nst=str_indx(imody,imodz)
 !==========Reallocates aux fields for particles data==========
 if(Part)then
  np=loc_npart(imody,imodz,imodx,ic)
  if(np>0)then
   call v_realloc(ebfp0,np,ndv)
   call v_realloc(ebfp1,np,ndv)
  endif
 endif
 !== particles number np does no change dunring rk-iterations
 !==========================
 do lps=1,rk
  jc(:,:,:,:)=0.0
  if(Part)then
   call pfields_prepare(ebf,i1,i2,j1,nyf,k1,nzf,nfield,2,2)
   do ic=1,nsp_run
    np=loc_npart(imody,imodz,imodx,ic)
    Ltz=Lorentz_fact(ic)
    if(np>0)then
     call set_rk_acc(spec(ic),ebfp,np,ndim,nfield,nst,xm,ym,zm)
     call advance_rk4_part(ebfp,ebfp0,ebfp1,spec(ic),np,dt_loc,Ltz,lps)
     ! in ebfp()[x,v*dt_k] at time (lps-1)
     call ncdef_rk_curr(ebfp,jc,nst,np,ndim,xm,ym,zm)
    endif
   end do
  endif
  call curr_mpi_collect(i1,i2,j1,nyf,k1,nzf)
  call advance_rk4_fields(&
   vbeam,dt_loc,i1,i2,j1,nyf,k1,nzf,lps)
 end do
 !-----------------------------
 end subroutine rk_evolve
 !===============
 ! END SECTION for RK4 integrators in LP regime
 !==============================
 subroutine LP_run(t_loc,dt_loc,iter_loc,t_ord)

 real(dp),intent(in) :: t_loc,dt_loc
 integer,intent(in) :: iter_loc,t_ord
 real(dp) :: ts
 logical :: init_time
 logical,parameter :: mw=.false.
 !+++++++++++++++++++++++++++++++++
 ts=t_loc
 init_time=.false.
 if(t_loc<epsilon)init_time=.true.
 if(w_speed>0.0)then ! moves the computational box with w_speed>0.
  if(iter_loc==0)call LP_window_xshift(dt_loc,w_sh,iter_loc)
  if(ts>=wi_time.and.ts< wf_time)then
   if(mod(iter_loc,w_sh)==0)then
    call LP_window_xshift(dt_loc,w_sh,iter_loc)
   endif
  endif
 endif
 if(vbeam >0.0)then
  if(iter_loc >0)call coordinate_xshift(vbeam,dt_loc)
 endif
 !==============================
 !vbeam=-w_speed
 !vbeam >0 uses the xw=(x+vbeam*t)
 !x=xi=(xw-vbeam*t) fixed
 !==============================
 !=========================
 select case(t_ord)
 case(2)
  call lpf2_evolve(t_loc,dt_loc,iter_loc,init_time)
 case(4)
  call rk_evolve(dt_loc,t_ord)
 end select
 ! WARNING: in RK3 and RK4 particles are exchanged only once
 if(Part)call cell_part_dist(mw)
 ts=ts+dt_loc
 end subroutine LP_run
 !==============================
 ! SECTION for ENVELOPE model in LP regime
 !============================
 subroutine lpf_env_momenta(sp_loc,F_pt,np,dtloc,Lz_fact)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: F_pt(:,:)

 integer,intent(in) :: np
 real(dp),intent(in) :: dtloc,Lz_fact
 integer :: p
 real(dp) :: bb(3),pp(3),vp(3),vph(3)
 real(dp) :: b2,bv,alp,dt_lp,efp(6)

 dt_lp=dtloc
 alp=0.5*dt_lp*Lz_fact
 !==========================
 select case(curr_ndim)
 case(2)
  !============  enter F_pt(1:3)=>(E+F_env/gam_p)^n  (B_z/gamp)^n
  !                    already multiplied by particle charge
  !F_pt(5)=wgh/gamp
  do p=1,np
   pp(1:2)=sp_loc%part(p,3:4)  !p_{n-1/2}
   efp(1:3)=alp*F_pt(p,1:3)         !charge/mass*(Ex,Ey,Bz)*Dt/2
   vp(1:2)=pp(1:2)+efp(1:2)   !u^{-}
   bb(1)=efp(3)
   !==============================
   b2=1.+bb(1)*bb(1)
   vph(1)=vp(1)+vp(2)*bb(1)
   vph(2)=vp(2)-vp(1)*bb(1)
   vph(1:2)=vph(1:2)/b2       !p_n=(p_{n+1/2)+p_{n-1/2})/2
   sp_loc%part(p,3:4)=2.*vph(1:2)-pp(1:2)
   F_pt(p,1:2)=sp_loc%part(p,1:2)
  end do
 case(3)
  !============  enter F_pt(1:6)=>(E+F/gam_p)^n  (B/gamp)^n
  !F_pt(7)=wgh/gamp
  do p=1,np
   pp(1:3)=sp_loc%part(p,4:6)
   efp(1:6)=alp*F_pt(p,1:6)   !charge/mass * (E+F/gamm,B/gam)*Dt/2
   vp(1:3)=efp(1:3)+pp(1:3)          !p_{n-1/2}+alp*DT/2(E+F/gamp)
   bb(1:3)=efp(4:6)
   !=============================
   ! The Boris pusher
   !=========================
   b2=1.+dot_product(bb(1:3),bb(1:3))
   bv=dot_product(bb(1:3),vp(1:3))
   vph(1)=vp(1)+vp(2)*bb(3)-vp(3)*bb(2)+bb(1)*bv
   vph(2)=vp(2)+vp(3)*bb(1)-vp(1)*bb(3)+bb(2)*bv
   vph(3)=vp(3)+vp(1)*bb(2)-vp(2)*bb(1)+bb(3)*bv
   vph(1:3)=vph(1:3)/b2       !p_n=(p_{n+1/2)+p_{n-1/2})/2
   !======== advance momenta
   sp_loc%part(p,4:6)=2.*vph(1:3)-pp(1:3)
   F_pt(p,1:3)=sp_loc%part(p,1:3) !stores old positions
  end do
 end select
 end subroutine lpf_env_momenta
 !======================
 subroutine lpf_env_positions(sp_loc,F_pt,np,dtloc,vb)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: F_pt(:,:)

 integer,intent(in) :: np
 real(dp),intent(in) :: dtloc,vb
 integer :: p,ch
 real(dp) :: pp(3),vp(3)
 real(dp) :: b2,gam2,gam_new,gam_inv,dt_lp,dth_lp

 dt_lp=dtloc
 dth_lp=0.5*dt_lp
 ch=5
 !==========================
 select case(curr_ndim)
  !============  enter F_pt(3)=F, F_pt (1:2) Grad[F] where F=|A|^2/2
  !             at time level t^{n+1/2} assigned at the x^n positions
 case(2)
  do p=1,np
   pp(1:2)=sp_loc%part(p,3:4)  !p^{n+1/2}
   vp(1:2)=F_pt(p,1:2)              !grad[F]
   !=============================
   gam2=1.+dot_product(pp(1:2),pp(1:2))+F_pt(p,3)
   b2=0.25*dot_product(pp(1:2),vp(1:2))
   !--------------------
   gam_new=sqrt(gam2)+dt_lp*b2/gam2
   gam_inv=1./gam_new
   vp(1:2)=dt_lp*gam_inv*pp(1:2)
   F_pt(p,3:4)=sp_loc%part(p,1:2) !old (z,r) positions
   F_pt(p,5)=dt_lp*gam_inv                   ! 1/gamma
   sp_loc%part(p,1:2)=sp_loc%part(p,1:2)+vp(1:2)
   F_pt(p,1:2)=sp_loc%part(p,1:2)   !(z,r) new positions
  end do
 case(3)
  !============enter F_pt(4)=F, F_pt (1:3) Grad[F] where F=|A|^2/2 at t^{n+1/2}
  ! assigned at x^n
  ch=7
  do p=1,np
   pp(1:3)=sp_loc%part(p,4:6)  !p^{n+1/2}
   vp(1:3)=F_pt(p,1:3)              !grad[F]
   !=============================
   gam2=1.+dot_product(pp(1:3),pp(1:3))+F_pt(p,4)
   b2=0.25*dot_product(pp(1:3),vp(1:3))
   !--------------------
   gam_new=sqrt(gam2)+dt_lp*b2/gam2
   gam_inv=1./gam_new
   vp(1:3)=dt_lp*gam_inv*pp(1:3)
   F_pt(p,4:6)=sp_loc%part(p,1:3) !old positions
   F_pt(p,7)=dt_lp*gam_inv             ! dt*gam_inv
   sp_loc%part(p,1:3)=sp_loc%part(p,1:3)+vp(1:3)
   F_pt(p,1:3)=sp_loc%part(p,1:3)   ! new positions
  end do
 end select
 if(iform <2)then
  do p=1,np
   F_pt(p,ch)=sp_loc%part(p,ch)
  end do
 endif
 !====================== vb < 0 in comoving x-coordinate
 if(Comoving)then
  do p=1,np
   sp_loc%part(p,1)=sp_loc%part(p,1)+dt_lp*vb
   F_pt(p,1)=sp_loc%part(p,1)   !new x-position
  end do
 endif
 end subroutine lpf_env_positions
 !=====================
 subroutine env_den_collect(eden,i1,n1p,j1,n2p,k1,n3p)

 real(dp),intent(inout) :: eden(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 integer :: i,j,k,kk,jj,ic
 real(dp) :: dery,derz

 ic=1
 if(prl)then
  call fill_curr_yzxbdsdata(eden,i1,n1p,j1,n2p,k1,n3p,ic)
 endif
 call den_zyxbd(eden,i1,n1p,j1,n2p,k1,n3p,ic)
 !=======Enters normalized <w*q*n/gam> < 0
 ! Jc(1:2)=ompe<w*q*n/gam>*env(1:2) the source in env equation
 !==================================
 if(Stretch)then
  ic=1
  do k=k1,n3p
   kk=k-2
   derz=loc_zg(kk,3,imodz)
   do j=j1,n2p
    jj=j-2
    dery=derz*loc_yg(jj,3,imody)
    do i=i1,n1p
     eden(i,j,k,ic)=dery*eden(i,j,k,ic)
    end do
   end do
  end do
 endif
 !=========================
 !exit the source terms chi() <0 for electrons of the envelope equation
 !-----------------------------------------------
 end subroutine env_den_collect
 !=========================
 subroutine env_pfields_prepare(ef,ef0,av,i1,i2,j1,j2,k1,k2,nc,spl_in,spr_in)
 real(dp),intent(in) :: ef(:,:,:,:),ef0(:,:,:,:)
 real(dp),intent(out) :: av(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,nc,spl_in,spr_in
 integer :: ix,iy,iz,spl,spr
 real(dp) :: ar,ai
 !===================
 select case(nc)
 case(1)
  do iz=k1,k2
   do iy=j1,j2
    do ix=i1,i2
     ar=0.5*(ef(ix,iy,iz,1)+ef0(ix,iy,iz,1))
     ai=0.5*(ef(ix,iy,iz,2)+ef0(ix,iy,iz,2))
     av(ix,iy,iz,1)=0.5*(ar*ar+ai*ai)
     ! |A|^2/2 at t^{n+1/2}=> gamp^{n+1/2}
    end do
   end do
  end do
 case(2)
  do iz=k1,k2
   do iy=j1,j2
    do ix=i1,i2
     av(ix,iy,iz,1)=0.5*(ef(ix,iy,iz,1)*ef(ix,iy,iz,1)+&
      ef(ix,iy,iz,2)*ef(ix,iy,iz,2))
     !|A|^2/2 at current t^n time level
    end do
   end do
  end do
 end select
 spl=spl_in
 spr=spr_in
 if(spl >2)spl=2
 if(spr >2)spr=2

 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,1,1,spr,spl)

 call env_grad(av,i1,i2,j1,j2,k1,k2,der_ord,dx_inv,dy_inv,dz_inv)
 !Staggered grad|A|^2/2 in jc(2:4) or jc(2:3)

 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,2,nj_dim,spr,spl)

 call field_xyzbd(av,i1,i2,j1,j2,k1,k2,nj_dim,spr,spl)
 !=====================
 end subroutine env_pfields_prepare
 !=======================================

 subroutine env_lpf2_evolve(dt_loc)
 real(dp),intent(in) :: dt_loc
 integer :: np,ic,nyf,nzf,n_st
 integer :: i1,j1,k1,i2
 real(dp) :: Ltz,xm,ym,zm
 !============================
 xm=loc_xgrid(imodx)%gmin
 zm=loc_zgrid(imodz)%gmin
 ym=loc_ygrid(imody)%gmin

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyf=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzf=loc_zgrid(imodz)%p_ind(2)
 n_st=0
 if(Stretch)n_st=str_indx(imody,imodz)
 !====================
 ic=1
 Ltz=Lorentz_fact(ic)
 !Lorentz force on a particle => ebfp0(1:3), velocity ebfp0(3:4) stored
 ! all the time for t^{n-1} levels ==========
 !===========================
 jc(:,:,:,:)=0.0
 np=loc_npart(imody,imodz,imodx,ic)
 if(np >0)then
  call pfields_prepare(ebf,i1,i2,j1,nyf,k1,nzf,nfield,1,1)
  call env_pfields_prepare(env,env0,jc,i1,i2,j1,nyf,k1,nzf,2,1,1)
  ! exit jc(1)=|a|^2/2 at t^n
  ! exit jc(2:4)=grad|a|^2/4 at t^n
  !======================================
   call set_env_acc(ebf,jc,spec(ic),ebfp,np,curr_ndim,dt_loc,xm,ym,zm)
   !exit ebfp(1:3)=[E+F] ebfp(4:6)=B/gamp, ebfp(7)=wgh*q/gamp at t^n
   !Fields already multiplied by particle charge
   !====================
   call lpf_env_momenta(spec(ic),ebfp,np,dt_loc,Ltz)
   ! P^{n-1/2} => P^{n+1/2}
   ! stores in ebfp(1:3)=(x,y,z)^n ebfp(7)=wgh*q/gamp
   !======================
   jc(:,:,:,1)=0.0
   call set_env_density(ebfp,jc,np,curr_ndim,1,xm,ym,zm)

  endif
  ! Jc(1:2)=ompe*<qn/gamp>*A at level t^n
  ! in the envelope equation (A^{n-1},A^n)==> (A^n,A^{n+1})
  call env_den_collect(jc,i1,i2,j1,nyf,k1,nzf)
  call advance_lpf_envelope(jc,env,env0,dt_loc,i1,i2,j1,nyf,k1,nzf)
  !(A^n, J^n) => A^{n+1}, A^{n-1}=> A^n
  call env_pfields_prepare(env,env0,jc,i1,i2,j1,nyf,k1,nzf,1,1,1)
  !exit jc(1)=|A|^2/2 at t^{n+1/2}
  if(np >0)then
   call set_env_interp(jc,spec(ic),ebfp,np,curr_ndim,xm,ym,zm)
   !=============================
   ! exit ebfp(1:3)=grad|A|^2/2 ebfp(4)=|A|^2/2 in 3D
   ! exit ebfp(1:2)=grad|A|^2/2 ebfp(3)=|A|^2/2 in 2D
   ! at time level t^{n+1/2} and positions at time t^n
   !=====================================
   call lpf_env_positions(spec(ic),ebfp,np,dt_loc,-vbeam)
   !===========================
   ! ebfp(1:6) new and old positions for curr J^{n+1/2}
   ! ebfp(7)=dt*wgh*gam_inv
   !==============================
   jc(:,:,:,:)=0.0
   call curr_accumulate(spec(ic),ebfp,1,np,iform,n_st,xm,ym,zm)
  endif
  !===========================
  call curr_mpi_collect(i1,i2,j1,nyf,k1,nzf)
  ! Jc(1:3) for curr J^{n+1/2}
  lp_in=lp_in+dt_loc
  call advance_lpf_fields(ebf,jc,dt_loc,vbeam,&
            i1,i2,j1,nyf,k1,nzf,1)
 ! (E,B) fields at time t^{n+1}
 !-----------------------------
 end subroutine env_lpf2_evolve
 !=====================================
 !=======================
 subroutine ENV_run(t_loc,dt_loc,iter_loc)

 real(dp),intent(in) :: t_loc,dt_loc
 integer,intent(in) :: iter_loc
 logical,parameter :: mw=.false.
 !+++++++++++++++++++++++++++++++++
 !for vbeam >0 uses the xw=(x+vbeam*t)
 !x=xi=(xw-vbeam*t) fixed
 if(w_speed>0.0)then ! moves the computational box with w_speed>0.
  if(iter_loc==0)call LP_window_xshift(dt_loc,w_sh,iter_loc)
  if(t_loc>=wi_time.and.t_loc < wf_time)then
   if(mod(iter_loc,w_sh)==0)then
    call LP_window_xshift(dt_loc,w_sh,iter_loc)
   endif
  endif
 endif
 if(vbeam >0.0)then
  if(iter_loc>0)call coordinate_xshift(vbeam,dt_loc)
 endif
 !=========================
 call env_lpf2_evolve(dt_loc)
 if(Part)call cell_part_dist(mw)
 !
 end subroutine ENV_run
 !============================
 ! END ENVELOPE SECTION
 !==============================
 !============================
 ! SECTION for beam-driven wakefields
 !==============================
 ! Leap-frog integrators
 !===========================
 subroutine lpf2_eb_evolve(dt_loc,iter_loc,ibmod,initial_time)

 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: iter_loc,ibmod
 logical,intent(in) :: initial_time
 integer :: np,ic,lps
 integer :: i1,i2,j1,j2,k1,k2,n_st,id_ch
 real(dp) :: xm,ym,zm,vb,Ltz,ef2_ion(1),loc_ef2_ion(1)
 !============================
 ! Fields are in ebf() (wake) and ebf_bunch() bunches
 ! particles are in spec(1)+ebfp plasma bunch(1:2)+ebfb (drive and witness)
 !==============================
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 lps=1
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
 n_st=0
 if(Stretch)n_st=str_indx(imody,imodz)
 id_ch=nd2+1
 !====================
 !Ghost cell values
 call pfields_prepare(ebf,i1,i2,j1,j2,k1,k2,nfield,1,1)
 call pfields_prepare(ebf_bunch,i1,i2,j1,j2,k1,k2,nfield,1,1)
 if(ibmod==1)call pfields_prepare(ebf1_bunch,i1,i2,j1,j2,k1,k2,nfield,1,1)
 !=================== prepares ebf and ebf_bunch for
 !                     particle acceleration
 vb=vbeam
 jc(:,:,:,:)=0.0
 !curr_clean
 do ic=1,nsp_run
  np=loc_npart(imody,imodz,imodx,ic)
  Ltz=Lorentz_fact(ic)
  if(np >0)then
   if(ibmod==0)call set_part3d_twofield_acc(ebf,ebf_bunch,spec(ic),ebfp,&
    1,np,n_st,xm,ym,zm)
   if(ibmod==1)call set_part3d_two_bfield_acc(ebf,ebf_bunch,ebf1_bunch,&
    spec(ic),ebfp,1,np,n_st,xm,ym,zm)

   !================================
   if(initial_time)call init_lpf_momenta(spec(ic),ebfp,1,np,dt_loc,Ltz)
   call lpf_momenta_and_positions(spec(ic),ebfp,1,np,dt_loc,vb,Ltz)
   if(ompe>0.0)call curr_accumulate(spec(ic),ebfp,1,np,iform,n_st,xm,ym,zm)
  endif
 end do
 if(Ionization)then
  do ic=2,nsp_ionz
   np=loc_npart(imody,imodz,imodx,ic)
   if(np>0)then
    if(ibmod==0)call set_ion_Ebfield(ebf,ebf_bunch,spec(ic),ebfp,np,&
     n_st,ndim,nsp_run,dt_loc,xm,ym,zm)
    if(ibmod==1)call set_ion_two_Ebfield(ebf,ebf_bunch,ebf1_bunch,spec(ic),&
     ebfp,np,n_st,ndim,nsp_run,dt_loc,xm,ym,zm)
    if(mod(iter_loc,100)==0)then     !refresh ionization tables
     loc_ef2_ion(1)=maxval(ebfp(1:np,id_ch))
     loc_ef2_ion(1)=sqrt(loc_ef2_ion(1))/514.
     if(prl)call allreduce_dpreal(MAXV,loc_ef2_ion,ef2_ion,1)
     if(ef2_ion(1) > eb_max)then
      eb_max=1.1*ef2_ion(1)
      call set_field_ioniz_wfunction(ion_min(ic-1),atomic_number(ic-1),ic,ionz_lev,ionz_model,eb_max,dt_loc)
     endif
    endif
   endif
   call ionization_cycle(&
                     spec(ic),ebfp,np,ic,iter_loc,deb_inv)
  end do
  !======== injects electrons and adds ionization energy
 endif
 if(ibmod==1)then
  call curr_mpi_collect(i1,i2,j1,j2,k1,k2)
  call advance_lpf_fields(ebf,jc,dt_loc,vbeam,i1,i2,j1,j2,k1,k2,1)
  jc(:,:,:,:)=0.0
  ! time step advance for plasma particles completed
 endif
 !============ advances bunches
 do ic=1,nsb
  np=loc_nbpart(imody,imodz,imodx,ic)
  Ltz=Lorentz_bfact(ic)
  if(np >0)then
   if(ibmod==0)                      call set_part3d_twofield_acc(ebf,ebf_bunch,&
    bunch(ic),ebfb,1,np,n_st,xm,ym,zm)
   if(ibmod==1.and. .not.L_Bpoloidal)call set_part3d_two_bfield_acc(ebf,ebf_bunch,&
    ebf1_bunch,bunch(ic),ebfb,1,np,n_st,xm,ym,zm)
   if(ibmod==1.and.      L_Bpoloidal)call set_part3d_three_bfield_acc(ebf,ebf_bunch,&
    ebf1_bunch,ebf0_bunch,bunch(ic),ebfb,1,np,n_st,xm,ym,zm)
   if(initial_time)call init_lpf_momenta(bunch(ic),ebfb,1,np,dt_loc,Ltz)
   call lpf_momenta_and_positions(bunch(ic),ebfb,1,np,dt_loc,vb,Ltz)
   if(ompe>0.0)call curr_accumulate(bunch(ic),ebfb,1,np,iform,n_st,xm,ym,zm)
  endif
  !============ advances bunches
 enddo
 !================ sums total currents
 call curr_mpi_collect(i1,i2,j1,j2,k1,k2)
 !======================= boundary ibx as for Maxwell equation
 call advect_bunch_fields(ebf_bunch,jc,&
  bet0,dt_loc,i1,i2,j1,j2,k1,k2,initial_time)
 select case(ibmod)
 case(0)
  call advance_lpf_fields(ebf,jc,dt_loc,vbeam,i1,i2,j1,j2,k1,k2,1)
 case(1)
  call advance_lpf_fields(ebf1_bunch,jc,dt_loc,vbeam,i1,i2,j1,j2,k1,k2,1)
 end select
 !=========================
 end subroutine lpf2_eb_evolve
 !================================
 subroutine BUNCH_run(t_loc,dt_loc,iter_loc)

 real(dp),intent(in) :: t_loc,dt_loc
 integer,intent(in) :: iter_loc
 real(dp) :: ts
 logical :: init_time
 logical,parameter :: mw=.false.
 !+++++++++++++++++++++++++++++++++
 !for vbeam >0 uses the xw=(x+vbeam*t)
 !x=xi=(xw-vbeam*t) fixed
 ts=t_loc
 init_time=.false.
 if(t_loc<epsilon)init_time=.true.
 if(w_speed>0.0)then ! moves the computational box with w_speed>0.
  if(iter_loc==0)call BUNCH_window_xshift(dt_loc,w_sh,iter_loc)
  if(ts>=wi_time.and.ts< wf_time)then
   if(mod(iter_loc,w_sh)==0)then
    call BUNCH_window_xshift(dt_loc,w_sh,iter_loc)
   endif
  endif
 endif
 if(vbeam >0.0)then
  if(iter_loc>0)call coordinate_xshift(vbeam,dt_loc)
 endif
 !=========================
 call lpf2_eb_evolve(dt_loc,iter_loc,ibeam,init_time)
 !
 call cell_part_dist(mw)
 call cell_bpart_dist(mw)
 ts=ts+dt_loc
 end subroutine BUNCH_run
 !================================
 subroutine bpart_ordering(pb_loc,pt,np,xbd,np0,np1)
 type(species),intent(inout) :: pb_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: np
 real(dp),intent(in) :: xbd
 integer,intent(out) :: np0,np1
 integer :: i1,ndv

 ndv=nd2+1
 pt(1:np,1:ndv)=pb_loc%part(1:np,1:ndv)
 np0=0
 do i1=1,np
  if(pt(i1,1) < xbd)then
   np0=np0+1
   pb_loc%part(np0,1:ndv)=pt(i1,1:ndv)
  endif
 end do
 np1=np0
 do i1=1,np
  if(pt(i1,1) >= xbd)then
   np1=np1+1
   pb_loc%part(np1,1:ndv)=pt(i1,1:ndv)
  endif
 end do
 pt(1:np,1:ndv)=0.0
 end subroutine bpart_ordering
 !=============================
 subroutine  lpf_advect_positions(pb_loc,nb0,nb,vbx)
 type(species),intent(inout) :: pb_loc
 integer,intent(in) :: nb0,nb
 real(dp),intent(in) :: vbx
 integer :: n

 do n=nb0,nb
  pb_loc%part(n,1)=pb_loc%part(n,1)+vbx
 end do
 end subroutine  lpf_advect_positions


 subroutine lpf2_pb_evolve(dt_loc,pb_mod,initial_time)

 real,intent(in) :: dt_loc
 integer,intent(in) :: pb_mod
 logical,intent(in) :: initial_time
 integer :: np,ic
 integer :: i1,i2,i1b,i2b,j1,j2,k1,k2,n_st,nbr,nbl
 real :: xm,xb,ym,zm,Ltz
 !============================
 !  Fields are in ebf() (wake) and ebf_bunch() bunches
 !  particles are in spec(1)+ebfp plasma bunch(1:2)+ebfb (drive and witness)
 !==============================
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 i2b=nx+2
 i1b=i2b/2
 xb=x(i1b)
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
 n_st=0
 !====================
 call pfields_prepare(ebf0_bunch,i1,i2,j1,j2,k1,k2,1,1,1)
 call pfields_prepare(ebf,i1,i2,j1,j2,k1,k2,nfield,1,1)
 call pfields_prepare(ebf_bunch,i1,i2b,j1,j2,k1,k2,nfield,1,1)
 !=================== prepares ebf and ebf_bunch for
 !                     particle acceleration
 jc(:,:,:,:)=0.0
 !curr_clean
 ic=1
 if(pb_mod >0)then
  np=loc_npart(imody,imodz,imodx,ic)
  Ltz=Lorentz_fact(ic)
  if(np >0)then
   call set_part3d_twofield_acc(ebf,ebf_bunch,spec(ic),ebfp,&
    1,np,n_st,xm,ym,zm)
   if(initial_time)call init_lpf_momenta(spec(ic),ebfp,1,np,dt_loc,Ltz)
   call lpf_momenta_and_positions(spec(ic),ebfp,1,np,dt_loc,vbeam,Ltz)
   call curr_accumulate(spec(ic),ebfp,1,np,iform,n_st,xm,ym,zm)
  endif
 endif
 !============ advances a uniform proton bunch
 np=loc_nbpart(imody,imodz,imodx,ic)
 Ltz=Lorentz_bfact(ic)
 if(np>0)then
  call bpart_ordering(bunch(ic),ebfb,np,xb,nbl,nbr)
  if(np /= nbr)write(6,'(a23,2i8)')'error in particle count',mype,np,nbr
  call lpf_advect_positions(bunch(ic),1,nbl,bet0*dt_loc)
  if(np > nbl)then
   !====================
   call set_part3d_threefield_acc(ebf,ebf_bunch,ebf0_bunch,bunch(ic),&
    ebfb,nbl+1,np,xm,ym,zm)
   call lpf_momenta_and_positions(bunch(ic),ebfb,nbl+1,np,dt_loc,vbeam,Ltz)
   call curr_accumulate(bunch(ic),ebfb,nbl+1,np,iform,n_st,xm,ym,zm)
  endif
 endif
 !================ sums total currents
 call curr_mpi_collect(i1,i2,j1,j2,k1,k2)
 ! current residuals are stored only for x > x(i1b)
 call advect_bunch_fields(ebf_bunch,jc,&
  bet0,dt_loc,i1,i2,j1,j2,k1,k2,initial_time)
 call advance_lpf_fields(ebf,jc,dt_loc,vbeam,i1b,i2,j1,j2,k1,k2,1)
 !=========================
 end subroutine lpf2_pb_evolve

 subroutine PBUNCH_run(t_loc,dt_loc)

 real,intent(in) :: t_loc,dt_loc
 real :: ts
 logical :: init_time
 logical,parameter :: mw=.false.
 !+++++++++++++++++++++++++++++++++
 !for vbeam >0 uses the xw=(x+vbeam*t)
 !x=xi=(xw-vbeam*t)  fixed
 ts=t_loc
 init_time=.false.
 if(t_loc<EPSILON)init_time=.true.
 !=========================
 call lpf2_pb_evolve(dt_loc,ibeam,init_time)
 !
 if(ibeam >0)call cell_part_dist(mw)
 call cell_bpart_dist(mw)
 ts=ts+dt_loc
 end subroutine PBUNCH_run

 end module pic_evolve_in_time
 !==================================
