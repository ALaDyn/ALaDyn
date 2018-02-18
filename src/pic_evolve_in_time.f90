 !*****************************************************************************************************!
 !                            Copyright 2008-2018  The ALaDyn Collaboration                            !
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
 use ionize

 implicit none
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
 real(dp) :: u,tmp0,whz

 tmp0=t0_pl(ic)
 n=np
 charge=int(unit_charge(ic),hp_int)
 part_ind=0
 k2=loc_nptz(ic)
 j2=loc_npty(ic)
 if(curr_ndim >2 )then
  do k=1,k2
   do j=1,j2
    do ix=i1,i2
     whz=wghpt(ix,ic)*loc_wghz(k,ic)
     wgh=real(whz*loc_wghy(j,ic),sp)
     n=n+1
     spec(ic)%part(n,1)=xpt(ix,ic)
     spec(ic)%part(n,2)=loc_ypt(j,ic)
     spec(ic)%part(n,3)=loc_zpt(k,ic)
     spec(ic)%part(n,4:6)=0.0
     spec(ic)%part(n,7)=wgh_cmp
    end do
   end do
  enddo
 else
  do j=1,j2
   do ix=i1,i2
    wgh=real(loc_wghy(j,ic)*wghpt(ix,ic),sp)
    n=n+1
    spec(ic)%part(n,1)=xpt(ix,ic)
    spec(ic)%part(n,2)=loc_ypt(j,ic)
    spec(ic)%part(n,3:4)=0.0
    spec(ic)%part(n,5)=wgh_cmp
   end do
  end do
 endif
 if(tmp0 >0.0)then
  n=np
  call init_random_seed(mype)
  if(curr_ndim > 2)then
   do k=1,k2
    do j=1,j2
     do ix=i1,i2
      n=n+1
      call gasdev(u)
      spec(ic)%part(n,4)=tmp0*u
      spec(ic)%part(n,5)=tmp0*u
      spec(ic)%part(n,6)=tmp0*u
     end do
    end do
   enddo
  else
   do j=1,j2
    do ix=i1,i2
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
 integer :: ic,ix,npt_inj(4),np_old,np_new
 integer :: i1,i2,n,q
 integer :: j2,k2,ndv
 integer :: j,k

 !========== inject particles from the right
 !   xmx is the box xmax grid value at current time after window move
 !   in Comoving frame xmax is fixed and particles are left advected
 !=================================
 !  nptx(ic) is the max particle index inside the computational box
 !  nptx(ic) is updated in the same way both for moving window xmax
 !  or for left-advect particles with fixed xmax
 !===============================================

 ndv=nd2+1
 do ic=1,nsp
 ! if(Comoving)then
 !  i1=1+nptx(ic)
 !  if(i1 <= sptx_max(ic))then
 !   i2=i1
 !   do ix=i1,sptx_max(ic)
 !    if(xpt(ix,ic) <= xmx)i2=i2+1
 !   end do
 !  endif
 !  nptx(ic)=i2
 ! else
   i1=1+nptx(ic)
   if(i1 <= sptx_max(ic))then
    do ix=i1,sptx_max(ic)
     if(xpt(ix,ic) > xmx)exit
    end do
    i2=ix-1
    if(ix==sptx_max(ic))i2=ix
   else
    i2=i1-1
   endif
   nptx(ic)=i2
 ! endif
!==========================
! Partcles to be injected have index ix [i1,i2]
!============================
  if(i2 > i1)then
  !==========================
   npt_inj(ic)=0
!=========== injects particles with coordinates index i1<= ix <=i2
   select case(ndim)
   case(1)
   do ix=i1,i2
    npt_inj(ic)=npt_inj(ic)+1
   end do
  case(2)
   j2=loc_npty(ic)
   do ix=i1,i2
    do j=1,j2
     npt_inj(ic)=npt_inj(ic)+1
    end do
   end do
  case(3)
   k2=loc_nptz(ic)
   j2=loc_npty(ic)
   do ix=i1,i2
    do k=1,k2
     do j=1,j2
      npt_inj(ic)=npt_inj(ic)+1
     end do
    end do
   end do
  end select
   np_new=0
  np_old=loc_npart(imody,imodz,imodx,ic)
   np_new=max(np_old+npt_inj(ic),np_new)
   if(ic==1)then
    if(size(ebfp,1)< np_new)then
     deallocate(ebfp)
     allocate(ebfp(np_new,ndv))
    endif
   endif
  !=========================
   if(size(spec(ic)%part,ic) <np_new)then
    do n=1,np_old
     ebfp(n,1:ndv)=spec(ic)%part(n,1:ndv)
    end do
    deallocate(spec(ic)%part)
    allocate(spec(ic)%part(np_new,ndv))
    do n=1,np_old
     spec(ic)%part(n,1:ndv)=ebfp(n,1:ndv)
    end do
   endif
   q=np_old
   call add_particles(q,i1,i2,ic)
   loc_npart(imody,imodz,imodx,ic)=np_new
  endif
 end do
 !=======================
 end subroutine particles_inject
 !=======================
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
 !========================================
 subroutine comoving_coordinate(vb,dt_loc,w_nst,loc_it)
 real(dp),intent(in) :: vb,dt_loc
 integer,intent(in) :: w_nst,loc_it
 integer :: i,ic,nshx
 real(dp) :: dt_tot
 logical,parameter :: mw=.true.
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
 if(loc_it==0)return
 dt_tot=0.0
 do i=1,w_nst
  dt_tot=dt_tot+dt_loc
 enddo
 nshx=nint(dx_inv*dt_tot*vb)      !the number of grid points x-shift for each w_nst step 
 do i=1,nx+1
  xw(i)=xw(i)-dx*nshx   !moves backwards the grid xw 
 end do
 xw_max=xw_max-dx*nshx
 xw_min=xw_min-dx*nshx
!======================== xw(i) grid used only for diagnostics purposes
 targ_in=targ_in-vb*dt_tot
 targ_end=targ_end-vb*dt_tot
 if(.not.Part)return
 !===========================
 do ic=1,nsp                 !left-advects all particles of the target outside the computational box
  do i=nptx(ic)+1,sptx_max(ic)
   xpt(i,ic)=xpt(i,ic)-vb*dt_tot
  end do
 end do
!======================
  call cell_part_dist(mw)   !particles are redistributes along the
  if(pex1)then
   if(targ_in<=xmax.and.targ_end >xmax)then
    call particles_inject(xmax)
   endif
  endif
  if(prl)call part_numbers
 end subroutine comoving_coordinate
 !====================================
 subroutine LP_window_xshift(dt_loc,witr,init_iter)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: witr,init_iter
 integer :: i1,n1p,j1,nyp,k1,nzp,nc_env
 integer :: ix,nshx,wi2
 real(dp),save :: xlapse
 integer,save :: wi1
 logical,parameter :: mw=.true.

 if(init_iter==0)then
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
 nshx=wi2-wi1
 wi1=wi2
 do ix=1,nx+1
  x(ix)=x(ix)+dx*nshx
  xh(ix)=xh(ix)+dx*nshx
 end do
 xmin=xmin+dx*nshx
 xmax=xmax+dx*nshx
 xp0_out=xp0_out+dx*nshx
 xp1_out=xp1_out+dx*nshx
 loc_xgrid(imodx)%gmin=loc_xgrid(imodx)%gmin+dx*nshx
 loc_xgrid(imodx)%gmax=loc_xgrid(imodx)%gmax+dx*nshx
 wi2=n1p-nshx
 if(wi2<=0)then
  write(6,'(a37,3i6)')'Error in window shifting for MPI proc',imody,imodz,imodx
  ier=2
  return
 endif
 !===========================
 call fields_left_xshift(ebf,i1,wi2,j1,nyp,k1,nzp,1,nfield,nshx)
 if(Hybrid)then
  do ix=wi2,nxf-nshx
   fluid_x_profile(ix)=fluid_x_profile(ix+nshx)
  end do
  nxf=nxf-nshx
  call fluid_left_xshift(up,fluid_x_profile,i1,wi2,j1,nyp,k1,nzp,1,nfcomp,nshx)
  call fluid_left_xshift(up0,fluid_x_profile,i1,wi2,j1,nyp,k1,nzp,1,nfcomp,nshx)
 endif
 if(Envelope)then
  nc_env=size(env,4)
  call fields_left_xshift(env,i1,wi2,j1,nyp,k1,nzp,1,nc_env,nshx)
  if(Two_color)call fields_left_xshift(env1,i1,wi2,j1,nyp,k1,nzp,1,nc_env,nshx)
 endif
 !shifts fields data and inject right ebf(wi2+1:n1p) x-grid nshx new data
 !===========================
 if(Part)then
  call cell_part_dist(mw)   !particles are redistributes along the
  ! right-shifted x-coordinate in MPI domains
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
 integer :: ix,nshx,wi2,w2f
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
 nshx=wi2-wi1
 wi1=wi2
 do ix=1,nx+1
  x(ix)=x(ix)+dx*nshx
  xh(ix)=xh(ix)+dx*nshx
 end do
 xmin=xmin+dx*nshx
 xmax=xmax+dx*nshx
 loc_xgrid(imodx)%gmin=loc_xgrid(imodx)%gmin+dx*nshx
 loc_xgrid(imodx)%gmax=loc_xgrid(imodx)%gmax+dx*nshx
 xp0_out=xp0_out+dx*nshx
 xp1_out=xp1_out+dx*nshx
 !====================
 w2f=n1p-nshx
 if(w2f<=0)then
  write(6,*)'Error in window shifting in task',imody,imodz,imodx
  ier=2
  return
 endif
 !===========================
 call fields_left_xshift(ebf,i1,w2f,j1,nyp,k1,nzp,1,nfield,nshx)
 call fields_left_xshift(&
                   ebf_bunch,i1,w2f,j1,nyp,k1,nzp,1,nbfield,nshx)
 call fields_left_xshift(&
             ebf1_bunch,i1,w2f,j1,nyp,k1,nzp,1,nbfield,nshx)
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
 !============================
 subroutine curr_accumulate(sp_loc,pdata,curr,npt0,npt,f_ch,n_st,xb,yb,zb)
 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: pdata(:,:),curr(:,:,:,:)
 integer,intent(in) :: npt0,npt,f_ch,n_st
 ! real(dp),intent(in) :: dtloc
 real(dp),intent(in) :: xb,yb,zb
 !=========================
 ! charge preservinga iform=0, 1
 !iform=0 => (D_xJx,D_yJy,D_zJz) inverted on each particle=> (Jx,Jy,Jz)
 !iform=1    as iform=0
 !iform=2    no charge preserving
 !=========================
 if(npt==0)return
  if(ndim < 3)then
   if(f_ch <2)then
    call esirkepov_2d_curr(sp_loc,pdata,curr,npt0,npt,n_st,curr_ndim,ndim,xb,yb)
  else
    call ncdef_2d_curr(sp_loc,pdata,curr,npt0,npt,n_st,curr_ndim,ndim,xb,yb)
   endif
   return
  endif
  if(f_ch <2)then
   call esirkepov_3d_curr(sp_loc,pdata,curr,npt0,npt,n_st,xb,yb,zb)
  else
   call ncdef_3d_curr(sp_loc,pdata,curr,npt0,npt,n_st,xb,yb,zb)
 endif
 !========================
 ! accumulates for each species currents on curr(i1:n1p,j1:n2p,k1:n3p,1:compnent)
 !============================
 end subroutine curr_accumulate
 !==============================
 subroutine curr_mpi_collect(curr,i1,n1p,j1,n2p,k1,n3p)

 real(dp),intent(inout) :: curr(:,:,:,:)
 integer,intent(in) :: i1,n1p,j1,n2p,k1,n3p
 integer :: i,j,k,jj,kk
 real(dp) :: dery,derhy,derz,derhz
 !============sums daata on ghost points
 if(prl)then
  call fill_curr_yzxbdsdata(curr,i1,n1p,j1,n2p,k1,n3p,nj_dim)
 endif
 call jc_xyzbd(curr,i1,n1p,j1,n2p,k1,n3p,nj_dim)
 !=================
 if(iform < 2)then   !iform=0=iform=1  use Esisrkepov scheme
  do i=1,ndim
   curr(i1:n1p,j1:n2p,k1:n3p,i)=djc(i)*curr(i1:n1p,j1:n2p,k1:n3p,i)
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
      curr(i,j,k,1)=dery*derz*curr(i,j,k,1)
      curr(i,j,k,2)=derhy*derz*curr(i,j,k,2)
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
      curr(i,j,k,1)=dery*derz*curr(i,j,k,1)
      curr(i,j,k,2)=derhy*derz*curr(i,j,k,2)
      curr(i,j,k,3)=dery*derhz*curr(i,j,k,3)
     end do
    end do
   end do
  end select
 endif
 end subroutine curr_mpi_collect
 !=================================
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
!================================
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

 !=======================
 ! A LPF order Lpf with time-centered source term
 !=============================
 if(prl)then
  str=0
  stl=1
  call fill_ebfield_yzxbdsdata(ef,i1,i2,j1,j2,k1,k2,1,curr_ndim,str,stl)
  ! to fill electric field data
  ! sends stl to the left,
  ! recvs stl points from right at (nyp+stl), (nzp+stl)
 endif
 !============== first substep dt/2 advance of B-field
 call ef_bds(ef,i1,i2,j1,j2,k1,k2,zero_dp,ibd)
 ! Uses upper BCs of E fields: ibd=0 for inflow-outflow
 !                             ibd=1 for symmetric
 if(Comoving)then
  call field_xcomov_advect(ef,i1,i2,j1,j2,k1,k2,curr_ndim+1,nfield,dthx,-v_b,0)
 ! Solves for one-half step backward advection B field explicit
 ! B^{n} => B^{n}+v_b*Dth[Dx]B^n  v_b >0
 endif
 !==================================
 ! solves B^{n+1/2}= B^n -Dth[rot E]^n
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
 ! Uses lower BCs for B fields: ibd=0 for inflow-outflow
 !                             ibd=1 for symmetric
 if(Comoving)then
  call field_xcomov_advect(ef,i1,i2,j1,j2,k1,k2,1,curr_ndim,dthx,-v_b,0)
 ! Solves for half-step backward advection E field expplicit
 ! E^{n} => E^{n}+v_b*Dth[Dx]E^n
 endif
 !=======================
 ! solves E^{n+1}= E^n +DT[rot B]^{n+1/2}- ompe*DT*J^{n+1/2}
 !==================
 call rotB(ef,i1,i2,j1,j2,k1,k2,dtx,dty,dtz)
 !===================
 ! adds currents
 do ik=1,curr_ndim
  ef(i1:i2,j1:j2,k1:k2,ik)=ef(i1:i2,j1:j2,k1:k2,ik)-&
   ompe*curr(i1:i2,j1:j2,k1:k2,ik)
 end do
 if(Comoving)then
  call field_xcomov_advect(ef,i1,i2,j1,j2,k1,k2,1,curr_ndim,dthx,-v_b,2)
 ! Solves for backward advection E field implicit
 ! E^{n+1} => E^{n}+v_b*Dth[Dx]E^{n+1}
 endif
 !============== second substep dt/2 advance of B-field
 if(prl)then
  str=0
  stl=1
  call fill_ebfield_yzxbdsdata(ef,i1,i2,j1,j2,k1,k2,1,curr_ndim,str,stl)
 endif
 ! E field gets stl points from right (nyp+stl), (nzp+stl)
 call ef_bds(ef,i1,i2,j1,j2,k1,k2,dt_lp,ibd)
 ! solves B^{n+1}= B^{n+1/2} -Dth[rot E]^{n+1}
 !===================
 call rotE(ef,i1,i2,j1,j2,k1,k2,dthx,dthy,dthz)
 !==============
 if(Comoving)then
  call field_xcomov_advect(ef,i1,i2,j1,j2,k1,k2,curr_ndim+1,nfield,dthx,-v_b,2)
 ! Solves for one-half step backward advection B field implicit
 ! B^{n+1} => B^{n+1/2}+v_b*Dth[Dx]B^{n+1}
 endif
 !===============
 end subroutine advance_lpf_fields
 !============================
 subroutine update_rk4_fields(ef,ef0,ef1,curr,v_b,dt_loc,i1,nxp,j1,nyp,k1,nzp,lp)

 real(dp),intent(inout) :: ef(:,:,:,:),ef0(:,:,:,:)
 real(dp),intent(inout) :: ef1(:,:,:,:),curr(:,:,:,:)
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
      ef0(ix,iy,iz,ik)=ef(ix,iy,iz,ik)
      ef1(ix,iy,iz,ik)=c_rk(0)*ef0(ix,iy,iz,ik)
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
                ef,i1,nxp,j1,nyp,k1,nzp,1,nfield,str,stl)
 endif
 call ef_bds(ef,i1,nxp,j1,nyp,k1,nzp,zero_dp,0)
 call bf_bds(ef,i1,nxp,j1,nyp,k1,nzp,zero_dp,0)
 !=======================
 ! enters curr() <=[dt*b^k]*J(x,v)^{k-1}
 !===============================
 ! curr() already multiplied by dt_rk
 do ik=1,curr_ndim
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
      curr(ix,iy,iz,ik)=ef0(ix,iy,iz,ik)-ompe*curr(ix,iy,iz,ik)
    end do
   end do
  end do
 end do
 ! aux acts as auxiliary array
 !======================
 ! First advances E^k=E0+[rot(B)-ompe*jc]^{k-1}*dt_k  E^k stored in aux(1:3).
 !                                                    (E,B)^k-1 not modified
 !-----------
 call rotB_rk4(ef,curr,curr_ndim,i1,nxp,j1,nyp,k1,nzp,-v_b,dtx,dty,dtz)
 ! In curr(1:3) the updated E^k field
 !====================
 !advances B^{k}= B^0 -dt_rk*rot(E^{k-1})
 call rotE_rk4(ef,ef0,curr_ndim,i1,nxp,j1,nyp,k1,nzp,-v_b,dtx,dty,dtz)
 ! curr  >  E^k
 do ik=1,curr_ndim
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
     ef(ix,iy,iz,ik)=curr(ix,iy,iz,ik) !=> E^k
    end do
   end do
  end do
 end do
 do ik=1,nfield
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
     ef1(ix,iy,iz,ik)=ef1(ix,iy,iz,ik)+c_rk(lp)*ef(ix,iy,iz,ik)
    end do
   end do
  end do
 end do
 if(lp==4)then
  do ik=1,nfield
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,nxp
      ef(ix,iy,iz,ik)=ef1(ix,iy,iz,ik)
     end do
    end do
   end do
  end do
 endif
 ! =================================
 end subroutine update_rk4_fields
 ! =================================
 subroutine advance_lpf_envelope(curr,evf,dt_loc,omg,i1,nxp,j1,nyp,k1,nzp)

 real(dp),intent(inout) :: curr(:,:,:,:),evf(:,:,:,:)
 real(dp),intent(in) :: dt_loc,omg
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp
 integer :: i,j,k,ic
 integer :: nst,str,stl,cind,ib

 !====== enter env(3:4)=A^{n-1} and env(1:2)= A^{n}
 ! enters jc(3)=<wgh*n/gamp> >0

 nst=0
 if(Stretch)nst=1
 !ord=2
 cind=1                !cind=0 FFT cind=1 grid deriv
 ib=2          !ib=1 implicit ib=2 optimazid explicit
 !optimized advection scheme
 if(Comoving)ib=0
 if(prl)then
  str=1
  stl=1
  call fill_ebfield_yzxbdsdata(&
   evf,i1,nxp,j1,nyp,k1,nzp,1,2,str,stl)
 endif
 do k=k1,nzp
  do j=j1,nyp
   do i=i1,nxp
    curr(i,j,k,1)=-ompe*curr(i,j,k,3)*evf(i,j,k,1)
    curr(i,j,k,2)=-ompe*curr(i,j,k,3)*evf(i,j,k,2)
   end do
  end do
 end do
 !  jc(1:2)=-ompe*chi*env(1:2)  the J_{env} source term
!==================================
 if(ib==0)then
  !call env_comov_maxw_solve(jc,evf,i1,nxp,j1,nyp,k1,nzp,omg,&
                    !dx_inv,dy_inv,dz_inv,dt_loc)
  call env_lpf_solve(jc,evf,ib,i1,nxp,j1,nyp,k1,nzp,omg,&
                    dx_inv,dy_inv,dz_inv,dt_loc)
 !========================================================
 else
 !=================== second order in time full wave equation
  call env_maxw_solve(jc,evf,i1,nxp,j1,nyp,k1,nzp,omg,&
                    dx_inv,dy_inv,dz_inv,dt_loc)
 endif
 ! =================================
 end subroutine advance_lpf_envelope
 ! =================================
 subroutine advance_env_rk4_field(&
  curr,envf,dt_loc,i1,nxp,j1,nyp,k1,nzp,lp,rko)

 real(dp),intent(inout) :: curr(:,:,:,:),envf(:,:,:,:)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: i1,nxp,j1,nyp,k1,nzp,lp,rko
 integer :: ix,iy,iz,ik,ik0,ik1
 integer :: str,stl,nst,ib,ord,nc,cind
 real(dp) :: dtx,dty,dtz,dt_rk


 dt_rk=dt_loc*b_rk(lp)  !==> to advance env(1:2)
 dtx=dx_inv
 dty=dy_inv
 dtz=dz_inv
 nst=0
 ib=1
 if(Comoving)ib=0
 ord=der_ord
 if(Stretch)nst=1
 nc=2
 !              enter jc(1)=0.5*ompe*rho/<gamp>*env(1)
 !                    jc(2)=0.5*ompe*rho/<gamp>*env(2) at (i-1) time level
 !                    rho=<qn_e> < 0
 !====enter the current source term in envelope equation
 if(lp==1)then
  do ik=1,nc
   ik0=ik+2
   ik1=ik0+2
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,nxp
      envf(ix,iy,iz,ik0)=envf(ix,iy,iz,ik)
      envf(ix,iy,iz,ik1)=c_rk(0)*envf(ix,iy,iz,ik)
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
   envf,i1,nxp,j1,nyp,k1,nzp,1,nc,str,stl)
 endif
 do iz=k1,nzp
  do iy=j1,nyp
   do ix=i1,nxp
    curr(ix,iy,iz,2)=-ompe*curr(ix,iy,iz,1)*envf(ix,iy,iz,2)
    curr(ix,iy,iz,1)=-ompe*curr(ix,iy,iz,1)*envf(ix,iy,iz,1)
   end do
  end do
 end do
 cind=1    !grid matrix inversion
 call env0_rk_field(curr,envf,ib,i1,nxp,j1,nyp,k1,nzp,oml,dtx,dty,dtz)
 !=============
 do ik=1,nc
  ik0=ik+2
  ik1=ik0+2
  do iz=k1,nzp
   do iy=j1,nyp
    do ix=i1,nxp
     envf(ix,iy,iz,ik)=envf(ix,iy,iz,ik0)+dt_rk*curr(ix,iy,iz,ik)
     envf(ix,iy,iz,ik1)=envf(ix,iy,iz,ik1)+c_rk(lp)*envf(ix,iy,iz,ik)
    end do
   end do
  end do
 end do
 if(lp==rko)then
  do ik=1,nc
   ik1=ik+4
   do iz=k1,nzp
    do iy=j1,nyp
     do ix=i1,nxp
      envf(ix,iy,iz,ik)=envf(ix,iy,iz,ik1)
     end do
    end do
   end do
  end do
 endif
 ! =================================
 end subroutine advance_env_rk4_field
 !===================
 subroutine advect_bunch_fields(fb,curr,&
  v_b,dt_lp,i1,i2,j1,j2,k1,k2,init_time)

 real(dp),intent(inout) :: fb(:,:,:,:),curr(:,:,:,:)
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
 !In 2D nfield=3 nbfield=4=nfield+1   in fb(4)=Jx[i+1/2,j,k] at t=0
 !================================================
 call fill_ebfield_xbdsdata(&
                     fb,i1,i2,j1,j2,k1,k2,1,nbfield,1,1)
 if(init_time)then
  call field_xadvect(fb,i1,i2,j1,j2,k1,k2,4,4,dthx,-v_b)
  fb(i1:i2,j1:j2,k1:k2,4)=dt_lp*fb(i1:i2,j1:j2,k1:k2,4)
 endif
 call field_xadvect(fb,i1,i2,j1,j2,k1,k2,1,nbfield,dtx,v_b)
 do iz=k1,k2
  do iy=j1,j2
   do ix=i1,i2
    curr(ix,iy,iz,1)=curr(ix,iy,iz,1)-fb(ix,iy,iz,4)
   end do
  end do
 end do
 ! Subtracts from the ongitudinal bunch current the advected initial bunch
 ! current
 end subroutine advect_bunch_fields
 !==================================

 ! END SECTION for TIME INTEGRATION OF EM fields
 !===============
 ! SECTION for Leap-frog integrators in LP regime
 !==============================
 subroutine field_charge_multiply(sp_loc,apt,n0,np,nf)
  type(species),intent(in) :: sp_loc
  real(dp),intent(inout) :: apt(:,:)
  integer,intent(in) :: n0,np,nf
  integer :: p,ch
  ch=size(apt,2)
 !==========================
   do p=n0,np
    wgh_cmp=sp_loc%part(p,ch)
    apt(p,1:nf)=charge*apt(p,1:nf)
   end do
 ! EXIT assigned (E,B) fields multiplied by charge
 end subroutine field_charge_multiply

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
 end subroutine set_lpf_acc
 !==========================
 subroutine init_lpf_momenta(sp_loc,pt,n0,np,dt_lp,Lfact)
 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: pt(:,:)
 integer,intent(in) :: n0,np
 real(dp),intent(in) :: dt_lp,Lfact
 integer :: p
 real(dp) :: alp,dth_lp,pp(3),vp(3),efp(6),gam2,gam_inv

 dth_lp=0.5*dt_lp
 alp=dth_lp*Lfact     ! Lfact =1./m
 ! Fields are already multiplied by charge
 !=========================
 ! from p^n to p^{n-1/2}
 !==========================
 select case(curr_ndim)
 case(2)
  do p=n0,np
   efp(1:3)=-alp*pt(p,1:3)   !-DT/2*charge*(Ex,Ey,Bz)^n
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
   efp(1:6)=-alp*pt(p,1:6)
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
 !========================================
 ! uses exact explicit solution for
 ! p^{n}=(p^{n+1/2}+p^{n-1/2})/2 and gamma^n=sqrt( 1+p^n*p^n)
 ! v^n=p^n/gamma^n
 !========================================
 !Enter Fields multiplied by charge
 dth_lp=0.5*dt_lp
 alp=dth_lp*Lfact
 select case(curr_ndim)
 case(2)
  ch=5
  do p=n0,np
   pp(1:2)=sp_loc%part(p,3:4)  !p_{n-1/2}
   efp(1:3)=alp*pt(p,1:3)         !q*Lfact*(Ex,Ey,Bz)*Dt/2
   vp(1:2)=pp(1:2)+efp(1:2)   !u^{-} in Boris push
   vp(3)=efp(3)               !b_z
   gam02=1.+dot_product(vp(1:2),vp(1:2))  !gam0 in Boris push
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
!=========== the new momenta
   !        update positions
   pt(p,3:4)=sp_loc%part(p,1:2)  !old positions stored
   pp(1:2)=sp_loc%part(p,3:4)
   gam2=1.+dot_product(pp(1:2),pp(1:2))
   pt(p,5)=dt_lp/sqrt(gam2)
   vp(1:2)=pt(p,5)*pp(1:2)   !velocities
   pt(p,1:2)=vp(1:2)                      !stores DT*V^{n+1/2}
   sp_loc%part(p,1:2)=sp_loc%part(p,1:2)+vp(1:2) !new positions
  end do
 case(3)
  ch=7
  do p=n0,np
   pp(1:3)=sp_loc%part(p,4:6)
   efp(1:6)=alp*pt(p,1:6)      !q*Lfact*(E,B) on p-th-particle
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
   pt(p,1:3)=vp(1:3)                  !stores dt*V
   sp_loc%part(p,1:3)=sp_loc%part(p,1:3)+vp(1:3) !new positions
  end do
 end select
 !====================
 if(iform <2)then
  !old charge stored for charge preserving schemes
  do p=n0,np
   pt(p,ch)=sp_loc%part(p,ch)
  end do
 endif
 if(Comoving)then
  do p=n0,np
   sp_loc%part(p,1)=sp_loc%part(p,1)-dt_lp*vb
   pt(p,1)=pt(p,1)-dt_lp*vb !
  end do
 endif
 end subroutine lpf_momenta_and_positions
!=============================
 !=======================
 subroutine lpf2_evolve(t_loc,dt_loc,iter_loc,initial_time)
 real(dp),intent(in) :: t_loc,dt_loc
 integer,intent(in) :: iter_loc
 logical,intent(in) :: initial_time
 integer :: lp,ic,np,i1,i2,j1,j2,k1,k2,n_st,id_ch
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
  call pfields_prepare(ebf,i1,i2,j1,j2,k1,k2,nfield,2,2)
 if(Ionization)then
  if(iter_loc==0)then
   call init_random_seed(mype)
   endif
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
    call ionization_cycle(spec(ic),ebfp,np,ic,iter_loc,0,de_inv)
    endif
    !======== injects new electrons.
   end do
  endif
  !===================END IONIZATION MODULE============
  !    ions enter with new ionization levels and new electrons
  !                   are injected
  !=============================================
 jc(:,:,:,:)=0.0
 !curr_clean
 if(Part)then
  do ic=1,nsp_run
   np=loc_npart(imody,imodz,imodx,ic)
   Ltz=Lorentz_fact(ic)
   if(np >0)then
    !==============
    !============
    call set_lpf_acc(ebf,spec(ic),ebfp,np,ndim,nfield,n_st,xm,ym,zm)
    call field_charge_multiply(spec(ic),ebfp,1,np,nfield)

    if(initial_time)call init_lpf_momenta(spec(ic),ebfp,1,np,dt_loc,Ltz)
    call lpf_momenta_and_positions(spec(ic),ebfp,1,np,dt_loc,vbeam,Ltz)
    ! For each species :
    ! ebfp(1:3) store (X^{n+1}-X_n)=V^{n+1/2}*dt
    ! ebfp(4:7) store old x^n positions and dt/gam at t^{n+1/2}
    !
    call curr_accumulate(spec(ic),ebfp,jc,1,np,iform,n_st,xm,ym,zm)
    !================= only old ion charge saved
   endif
  enddo
  !==========================================
  call curr_mpi_collect(jc,i1,i2,j1,j2,k1,k2)
 endif        !end particle section
  !================ sums and normalize currents
 !=======================
  ! Inject fields at i=i1-1  for inflow Lp_inject=T
  call wave_field_left_inject(xm)  !(Bz=Ey By=Ez are injected at i1-1 point
   call advance_lpf_fields(ebf,jc,dt_loc,vbeam,i1,i2,j1,j2,k1,k2,0)
 !============================
 contains
 subroutine wave_field_left_inject(x_left)
  real(dp),intent(in) :: x_left
  real(dp) :: tnew
  integer :: wmodel_id,ic

 wmodel_id=model_id
 if(Plane_wave)wmodel_id=0

 tnew=t_loc    !Set inflow values [B_z{n}(i1-1/2) E_y{n}(i-1}
  Lp_inject=.false.
  do ic=1,nb_laser
   if(lp_in(ic) < x_left.and.lp_end(ic)>= xm)then
    Lp_inject=.true.
 if(model_id<3) call inflow_lp_fields(&
              ebf,lp_amp,tnew,t0_lp,w0_x,w0_y,xf_loc(ic),oml,wmodel_id,i1,j1,j2,k1,k2)
 if(model_id==3)call inflow_cp_fields(&
              ebf,lp_amp,tnew,t0_lp,w0_x,w0_y,xf_loc(ic),wmodel_id,i1,j1,j2,k1,k2)
   endif
   lp_in(ic)=lp_in(ic)+dt_loc
   lp_end(ic)=lp_end(ic)+dt_loc
  end do
  if(Two_color)then
   if(lp_ionz_in < x_left.and.lp_ionz_end >=xm)then
    Lp_inject=.true.
    call inflow_lp_fields(&
       ebf,lp1_amp,tnew,t1_lp,w1_x,w1_y,xf1,om1,model_id,i1,j1,j2,k1,k2)
   endif
   lp_ionz_in=lp_ionz_in+dt_loc
   lp_ionz_end=lp_ionz_end+dt_loc
  endif
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
 subroutine update_rk4_part(F_pt,F_pt0,F_pt1,sp_loc,np,dtloc,Lfact,lp)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: F_pt(:,:)
 real(dp),intent(out) :: F_pt0(:,:),F_pt1(:,:)

 integer,intent(in) :: np,lp
 real(dp),intent(in) :: dtloc,Lfact
 integer :: p,ndv
 real(dp) :: alp,afact,dt_lp,gam,vp(3),efp(3),fploc(6)
 real(sp) :: wght

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
   wgh_cmp=sp_loc%part(p,5)         !stores p-weight and charge
   alp=charge*afact
   wght=charge*wgh
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
   F_pt(p,3:4)=wght*F_pt(p,3:4)     !q*wgh*dt_rk*V^{k-1} => curr}
  end do
 case(3)
  !in F_pt(1:6) the (E,B) fields on a particle at t^{k-1}
  !==================================
  ! RK4 integration scheme p^k=p_0+Dt*b_k*F^{k-1},k=1,2,3,4
  do p=1,np
   wgh_cmp=sp_loc%part(p,7)         !stores p-charge
   alp=charge*afact
   wght=charge*wgh
   vp(1:3)=sp_loc%part(p,4:6)   !P^{k-1}
   gam=sqrt(1.+vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3)) !gam^{k-1}
   vp(1:3)=vp(1:3)/gam
   fploc(1:6)=F_pt(p,1:6)         !F=(E,B)^{k-1}

   efp(1)=alp*(fploc(1)+vp(2)*fploc(6)-vp(3)*fploc(5))
   efp(2)=alp*(fploc(2)+vp(3)*fploc(4)-vp(1)*fploc(6))
   efp(3)=alp*(fploc(3)+vp(1)*fploc(5)-vp(2)*fploc(4))

   F_pt(p,4:6)=dt_lp*vp(1:3)             !dt_rk*V^{k-1}
   sp_loc%part(p,4:6)=F_pt0(p,4:6)+efp(1:3)  !=> p^k

   F_pt(p,1:3)=sp_loc%part(p,1:3)     !current positions  x^{k-1}
   sp_loc%part(p,1:3)=F_pt0(p,1:3)+F_pt(p,4:6) !advances positions x^k
   F_pt(p,4:6)=wght*F_pt(p,4:6)     !q*wgh*dt_rk*V^{k-1} => curr^{k-1}
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
 end subroutine update_rk4_part
!=============================
 subroutine update_rk4_fluid_variables(u,u0,u1,flx,curr,ef,dt_lp,i1,i2,j1,j2,k1,k2,kst)
  real(dp),intent(inout) :: u(:,:,:,:),u0(:,:,:,:),u1(:,:,:,:),curr(:,:,:,:)
  real(dp),intent(inout) :: ef(:,:,:,:),flx(:,:,:,:)
  real(dp),intent(in) :: dt_lp
  integer,intent(in) :: i1,i2,j1,j2,k1,k2,kst
  integer :: i,j,k,ic,str,stl,fdim,fldim
  real(dp) :: pp(1:3),b1p,b1m,b1pp,b1mm
  real(dp) :: dt_rk,gam2,ch,gam_inv,lzf,apx,apy,apz,dtx,dty,dtz
  real(dp) ::dth,den,ex,ey,ez,bx,by,bz,qx,qy,qz,vx,vy,vz
  real(dp),parameter :: wk1= 9./16.,wk2=-1./16.

  dt_rk=b_rk(kst)*dt_lp
  lzf=unit_charge(1)*dt_rk
  dtx=dx_inv*dt_rk
  dty=dy_inv*dt_rk
  dtz=dz_inv*dt_rk
  apx=-dtx
  apy=-dty
  apz=-dtz
  ch=dt_rk*unit_charge(1)
  fdim=curr_ndim+1     !dimension of fluid variables (ux,uy,uz,den)
  fldim=2*curr_ndim+1  !dimension of aux flx() array
!====================== CONSERVATIVE FORM for DENSITY MOMEMTA
!========== t^{k-1}=> t^k  RK cycle===========
!           ENTER u^{k-1}, (E,B)^{k-1} =>  u^k,(E,B)^k
!======================================================
! sets  flux^{k-1}=[ux,uy,uz,den,vx,vy,vz]    2*curr_ndim+1
  do ic=1,fdim
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      flx(i,j,k,ic)=u(i,j,k,ic)
     end do
    end do
   end do
  end do
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     den=u(i,j,k,fdim)
     pp(1:curr_ndim)=0.0
     if(den >0.0)then
      pp(1:curr_ndim)=u(i,j,k,1:curr_ndim)/den
      gam2=1.+dot_product(pp(1:curr_ndim),pp(1:curr_ndim))
      gam_inv=1./sqrt(gam2)
      pp(1:curr_ndim)=gam_inv*pp(1:curr_ndim)  !(vx,vy)=pp/gam
     endif
     do ic=1,curr_ndim
      flx(i,j,k,fdim+ic)=pp(ic)
     end do
    end do
   end do
  end do
!======================
  if(kst==1)then
   do ic=1,fdim
    do k=k1,k2
     do j=j1,j2
      do i=i1,i2
       u0(i,j,k,ic)=u(i,j,k,ic)               !u^0=u^n   kst=1
       u1(i,j,k,ic)=c_rk(0)*u(i,j,k,ic)
      end do
     end do
    end do
   end do
  endif
  call fill_flux_yzxbdsdata(flx,&
                   i1,i2,j1,j2,k1,k2,1,fldim,3,3)
                   !extends flx arrays to [j1-3--j2+3]
  call fill_ebfield_yzxbdsdata(&
                   ef,i1,i2,j1,j2,k1,k2,1,nfield,2,2)
  do ic=1,fdim
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      u(i,j,k,ic)=u0(i,j,k,ic)
     end do
    end do
   end do
  end do
!!!!!!!!!!!!!!!!!!!!
  call rk_fluid_density_momenta(u,flx,i1,i2,j1,j2,k1,k2,fdim,fldim,apx,apy,apz)
                        !in u() adds f(u) derivatives in yrange [j1+1,j2+1]
                        ! u=u0+f(u)   flx[u^{k-1} unmodified
  call add_rk_lorentz_force      !exit u=u+(E+vxB)^{k-1}
   do ic=1,fdim
    do k=k1,k2
     do j=j1,j2
      do i=i1,i2
       u1(i,j,k,ic)=u1(i,j,k,ic)+c_rk(kst)*u(i,j,k,ic)
      end do
     end do
    end do
   end do
   if(kst==4)then
    do ic=1,fdim
     do k=k1,k2
      do j=j1,j2
       do i=i1,i2
        u(i,j,k,ic)=u1(i,j,k,ic)   !u^{n+1}=c_rk(0)*u^n+sum_k[c_rk(k)*u^k]  final step
       end do
      end do
     end do
    end do
   endif
!==========================
  contains
!============== step=1 advances (E,B)^n => (E,B)^{n+1/2} ghost points already set
  subroutine add_rk_lorentz_force
!=================ADDS the LORETZ FORCE collocated on the (i,j,k)node points
  real(dp) :: qp,qm,qpp,qmm

  select case(curr_ndim)
   case(2)
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      ex=wk1*(ef(i,j,k,1)+ef(i-1,j,k,1))+wk2*(ef(i+1,j,k,1)+ef(i-2,j,k,1))   !Ex(i,j,k)
      ey=wk1*(ef(i,j,k,2)+ef(i,j-1,k,2))+wk2*(ef(i,j+1,k,2)+ef(i,j-2,k,2))   !Ey(i,j,k)
      b1p=wk1*(ef(i,j,k,3)+ef(i-1,j,k,3))+wk2*(ef(i+1,j,k,3)+ef(i-2,j,k,3))         !bz(i,j+1/2,k)
      b1m=wk1*(ef(i,j-1,k,3)+ef(i-1,j-1,k,3))+wk2*(ef(i+1,j-1,k,3)+ef(i-2,j-1,k,3)) !bz(i,j-1/2,k)
      b1pp=wk1*(ef(i,j+1,k,3)+ef(i-1,j+1,k,3))+wk2*(ef(i+1,j+1,k,3)+ef(i-2,j+1,k,3))!bz(i,j+3/2,k)
      b1mm=wk1*(ef(i,j-2,k,3)+ef(i-1,j-2,k,3))+wk2*(ef(i+1,j-2,k,3)+ef(i-2,j-2,k,3)) !bz(i,j-3/2,k)
      bz=wk1*(b1p+b1m) + wk2*(b1pp+b1mm)                !Bz(i,j,k)

      den=flx(i,j,k,fdim)
      vx=flx(i,j,k,fdim+1)
      vy=flx(i,j,k,fdim+2)
      u(i,j,k,1)=u(i,j,k,1)+lzf*den*(ex+vy*bz)
      u(i,j,k,2)=u(i,j,k,2)+lzf*den*(ey-vx*bz)

      qp=flx(i+1,j,k,fdim+1)*flx(i+1,j,k,fdim)
      qpp=flx(i+2,j,k,fdim+1)*flx(i+2,j,k,fdim)
      qm=flx(i,j,k,fdim+1)*flx(i,j,k,fdim)
      qmm=flx(i-1,j,k,fdim+1)*flx(i-1,j,k,fdim)
      curr(i,j,k,1)=wk1*(qp+qm)+wk2*(qpp+qmm)
      curr(i,j,k,1)=ch*curr(i,j,k,1)

      qp=flx(i,j+1,k,fdim+2)*flx(i,j+1,k,fdim)
      qpp=flx(i,j+2,k,fdim+2)*flx(i,j+2,k,fdim)
      qm=flx(i,j,k,fdim+2)*flx(i,j,k,fdim)
      qmm=flx(i,j-1,k,fdim+2)*flx(i,j-1,k,fdim)

      curr(i,j,k,2)=wk1*(qp+qm)+wk2*(qpp+qmm)
      curr(i,j,k,2)=ch*curr(i,j,k,2)
     end do
    end do
   end do
   case(3)
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      ex=0.5*(ef(i,j,k,1)+ef(i-1,j,k,1))
      ey=0.5*(ef(i,j,k,2)+ef(i,j-1,k,2))
      ez=0.5*(ef(i,j,k,3)+ef(i,j,k-1,3))
      b1p=0.5*(ef(i,j,k,4)+ef(i,j-1,k,4))
      b1m=0.5*(ef(i,j,k-1,4)+ef(i,j-1,k-1,4))
      bx=0.5*(b1p+b1m)
      b1p=0.5*(ef(i,j,k,5)+ef(i-1,j,k,5))
      b1m=0.5*(ef(i,j,k-1,5)+ef(i-1,j,k-1,5))
      by=0.5*(b1p+b1m)
      b1p=0.5*(ef(i,j,k,6)+ef(i-1,j,k,6))
      b1m=0.5*(ef(i,j-1,k,6)+ef(i-1,j-1,k,6))
      bz=0.5*(b1p+b1m)
      bx=0.5*(ef(i,j,k,4)+ef(i,j-1,k-1,4))
      by=0.5*(ef(i,j,k,5)+ef(i-1,j,k-1,5))
      bz=0.5*(ef(i,j,k,6)+ef(i-1,j-1,k,6))
!======
      den=flx(i,j,k,fdim)
      qx=den*flx(i,j,k,fdim+1)
      qy=den*flx(i,j,k,fdim+2)
      qz=den*flx(i,j,k,fdim+3)
      u(i,j,k,1)=u(i,j,k,1)+lzf*(den*ex+qy*bz-qz*by)
      u(i,j,k,2)=u(i,j,k,2)+lzf*(den*ey+qz*bx-qx*bz)
      u(i,j,k,3)=u(i,j,k,3)+lzf*(den*ez+qx*by-qy*bx)
      u(i,j,k,4)=u(i,j,k,4)
     end do
    end do
   end do
   endselect
  end subroutine add_rk_lorentz_force
 end subroutine update_rk4_fluid_variables
 !==============================
 subroutine rk_evolve(dt_loc,itr,rk)
 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: itr,rk
 integer :: np,ic,lps,nyf,nzf
 integer :: i1,j1,k1,i2,ndv,id_ch,nst
 real(dp) :: xm,ym,zm,Ltz,ef2_ion(1),loc_ef2_ion(1)
 !============================
 ! Implements PIC-RK4 schemes
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
!================= Ionization one step per rk4 cycle
 if(Ionization)then
  call pfields_prepare(ebf,i1,i2,j1,nyf,k1,nzf,nfield,2,2)
  if(itr ==0)then
   call init_random_seed(mype)
  endif
  id_ch=nd2+1
  do ic=2,nsp_ionz
   np=loc_npart(imody,imodz,imodx,ic)
   if(np>0)then
    call set_ion_Efield(ebf,spec(ic),ebfp,np,nst,ndim,nsp_run,dt_loc,xm,ym,zm)
    if(mod(itr,100)==0)then     !refresh ionization tables, if needed
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
    call ionization_cycle(spec(ic),ebfp,np,ic,itr,0,de_inv)
   endif
    !======== injects new electrons.
  end do
 endif
 !===================END IONIZATION MODULE============
 !==========Reallocates aux fields for particles data==========
 ic=1
 if(Part)then
  np=loc_npart(imody,imodz,imodx,ic)
  if(np>0)then
   call v_realloc(ebfp0,np,ndv)
   call v_realloc(ebfp1,np,ndv)
  endif
 endif
 !== particles number np does no change during rk-iterations
 !==========================
 do lps=1,rk
  jc(:,:,:,:)=0.0
  call pfields_prepare(ebf,i1,i2,j1,nyf,k1,nzf,nfield,2,2)
  if(Part)then
   do ic=1,nsp_run
    np=loc_npart(imody,imodz,imodx,ic)
    Ltz=Lorentz_fact(ic)
    if(np>0)then
     call set_rk_acc(spec(ic),ebfp,np,ndim,nfield,nst,xm,ym,zm)
     call update_rk4_part(ebfp,ebfp0,ebfp1,spec(ic),np,dt_loc,Ltz,lps)
     ! in ebfp()[x,v*dt_k] at time (lps-1)
     call ncdef_rk_curr(ebfp,jc,nst,np,ndim,xm,ym,zm)
    endif
   end do
   call curr_mpi_collect(jc,i1,i2,j1,nyf,k1,nzf)
  endif
  if(Hybrid)then
                                   !ADVANCES fluid variables  and sets currents
                                   !for Maxwell equations
   call update_rk4_fluid_variables(up,up0,up1,flux,jc,ebf,dt_loc,i1,i2,j1,nyf,k1,nzf,lps)
  endif
  !================ sums and normalize currents
  call update_rk4_fields(ebf,ebf0,ebf1,jc,&
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
 if(Comoving)then
  if(ts>=wi_time.and.ts< wf_time)then
   if(mod(iter_loc,w_sh)==0)then
    call comoving_coordinate(vbeam,dt_loc,w_sh,iter_loc)
   endif
  endif
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
  call rk_evolve(dt_loc,iter_loc,t_ord)
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
 !Enter F_pt(1:2)= charge*(E+0.5^grad[F]/gamp) and F_pt(3)=charge*B/gamp     where F=|A|^2/2
 select case(curr_ndim)
 case(2)
  !F_pt(5)=wgh/gamp
  do p=1,np
   pp(1:2)=sp_loc%part(p,3:4)  !p_{n-1/2}
   efp(1:3)=alp*F_pt(p,1:3)         !Lz_fact*Dt/2
   vp(1:2)=pp(1:2)+efp(1:2)   !u^{-}
   bb(1)=efp(3)
   !==============================
   b2=1.+bb(1)*bb(1)
   vph(1)=vp(1)+vp(2)*bb(1)
   vph(2)=vp(2)-vp(1)*bb(1)
   vph(1:2)=vph(1:2)/b2       !p_n=(p_{n+1/2)+p_{n-1/2})/2
   vph(1:2)=vp(1:2)
   sp_loc%part(p,3:4)=2.*vph(1:2)-pp(1:2)
   F_pt(p,1:2)=sp_loc%part(p,1:2)
  end do
  !F_pt(5)=wgh/gamp unchanged
 case(3)
  !F_pt(7)=wgh/gamp
  do p=1,np
   pp(1:3)=sp_loc%part(p,4:6)
   efp(1:6)=alp*F_pt(p,1:6)   !multiply by Lz_fact*Dt/2
   vp(1:3)=efp(1:3)+pp(1:3)          !p_{n-1/2}+alp*(E+0.5*F/gamp)
   bb(1:3)=efp(4:6)                  !alp*B/gamp
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
  !F_pt(7)=wgh/gamp unchanged
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
 real(dp) :: b2,gam2,gam_inv,dt_lp,dth_lp

 dt_lp=dtloc
 dth_lp=0.5*dt_lp
 ch=5
 !==========================
 select case(curr_ndim)
  !============  enter F_pt(3)=F, F_pt (1:2) Grad[F] where F=|A|^2/2
  !             at time level t^{n+1/2} assigned to the x^n positions
 case(2)
  do p=1,np
   pp(1:2)=sp_loc%part(p,3:4)  !p^{n+1/2}
   vp(1:2)=F_pt(p,1:2)              !grad[F]
   !=============================
   gam2=1.+dot_product(pp(1:2),pp(1:2))+F_pt(p,3)
   b2=0.25*dot_product(pp(1:2),vp(1:2))
   !--------------------
   gam_inv=1./sqrt(gam2)
   gam_inv=gam_inv*(1.-dt_lp*b2/gam2)
   vp(1:2)=dt_lp*gam_inv*pp(1:2)
   F_pt(p,3:4)=sp_loc%part(p,1:2) !old (x,y)^n positions
   F_pt(p,5)=dt_lp*gam_inv                   ! 1/gamma
   sp_loc%part(p,1:2)=sp_loc%part(p,1:2)+vp(1:2)
   F_pt(p,1:2)=vp(1:2) ! dt*V^{n+1/2}  velocities
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
   gam_inv=1./sqrt(gam2)
   gam_inv=gam_inv*(1.-dt_lp*b2/gam2)
   vp(1:3)=dt_lp*gam_inv*pp(1:3)
   F_pt(p,4:6)=sp_loc%part(p,1:3) !old positions
   F_pt(p,7)=dt_lp*gam_inv             ! dt*gam_inv
   sp_loc%part(p,1:3)=sp_loc%part(p,1:3)+vp(1:3)
   F_pt(p,1:3)=vp(1:3) ! dt*V^{n+1/2}  velocities
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
   sp_loc%part(p,1)=sp_loc%part(p,1)-dt_lp*vb
   F_pt(p,1)=F_pt(p,1)-dt_lp*vb   !new x-position
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
 !=======Enters normalized <w*n/gam> > 0
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
 !exit in eden(ic)  the source terms chi() >0 of the envelope equation
 !-----------------------------------------------
 end subroutine env_den_collect
 !=========================
 subroutine env_two_fields_average(evf,ev1f,av,i1,i2,j1,j2,k1,k2,spl_in,spr_in)
 real(dp),intent(in) :: evf(:,:,:,:),ev1f(:,:,:,:)
 real(dp),intent(out) :: av(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,spl_in,spr_in
 integer :: ix,iy,iz
 real(dp) :: ar,ai
 !===================
 do iz=k1,k2
  do iy=j1,j2
   do ix=i1,i2
    ar=0.5*(evf(ix,iy,iz,1)+evf(ix,iy,iz,3))   !A^{n+1/2}=(A^n+1+A^n)/2
    ai=0.5*(evf(ix,iy,iz,2)+evf(ix,iy,iz,4))
    av(ix,iy,iz,1)=0.5*(ar*ar+ai*ai)
    ar=0.5*(ev1f(ix,iy,iz,1)+ev1f(ix,iy,iz,3))   !A^{n+1/2}=(A^n+1+A^n)/2
    ai=0.5*(ev1f(ix,iy,iz,2)+ev1f(ix,iy,iz,4))
    av(ix,iy,iz,1)=av(ix,iy,iz,1)+0.5*(ar*ar+ai*ai)
    ! |A|^2/2 at t^{n+1/2}=> gamp^{n+1/2}
   end do
  end do
 end do
 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,1,1,spr_in,spl_in)
 call field_xyzbd(av,i1,i2,j1,j2,k1,k2,1,spr_in,spl_in)
 !=====================
 end subroutine env_two_fields_average
!===========================
 subroutine env_fields_average(evf,av,i1,i2,j1,j2,k1,k2,spl_in,spr_in)
 real(dp),intent(in) :: evf(:,:,:,:)
 real(dp),intent(out) :: av(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,spl_in,spr_in
 integer :: ix,iy,iz,ord
 real(dp) :: ar,ai
 !===================
 ord=2
 do iz=k1,k2
  do iy=j1,j2
   do ix=i1,i2
    ar=0.5*(evf(ix,iy,iz,1)+evf(ix,iy,iz,3))   !A^{n+1/2}=(A^n+1+A^n)/2
    ai=0.5*(evf(ix,iy,iz,2)+evf(ix,iy,iz,4))
    av(ix,iy,iz,1)=0.5*(ar*ar+ai*ai)
    ! |A|^2/2 at t^{n+1/2}=> gamp^{n+1/2}
   end do
  end do
 end do
 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,1,1,spr_in,spl_in)
 call env_grad(av,i1,i2,j1,j2,k1,k2,ord,dx_inv,dy_inv,dz_inv)
 !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3) 

 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,2,curr_ndim+1,spr_in,spl_in)
 !=====================
 end subroutine env_fields_average
 !===========================
 subroutine env_amp_prepare(envf,av,i1,i2,j1,j2,k1,k2,ord,spl_in,spr_in)
 real(dp),intent(in) :: envf(:,:,:,:)
 real(dp),intent(out) :: av(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ord,spl_in,spr_in
 integer :: ix,iy,iz,spl,spr
 !real(dp) :: ar,ai
 !===================
 do iz=k1,k2
  do iy=j1,j2
   do ix=i1,i2
    av(ix,iy,iz,1)=0.5*(envf(ix,iy,iz,1)*envf(ix,iy,iz,1)+&
     envf(ix,iy,iz,2)*envf(ix,iy,iz,2))
    !|A|^2/2 at current t^n time level
   end do
  end do
 end do
 spl=spl_in
 spr=spr_in
 if(spl >2)spl=2
 if(spr >2)spr=2

 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,1,1,spr,spl)

 call env_grad(av,i1,i2,j1,j2,k1,k2,ord,dx_inv,dy_inv,dz_inv)
 !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3)

 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,1,curr_ndim+1,spr,spl)

 call field_xyzbd(av,i1,i2,j1,j2,k1,k2,nj_dim,spr,spl)
 !=====================
 end subroutine env_amp_prepare
!=============================
 subroutine env_amp_two_fields_prepare(envf,env1f,av,i1,i2,j1,j2,k1,k2,ord,spl_in,spr_in)
 real(dp),intent(in) :: envf(:,:,:,:),env1f(:,:,:,:)
 real(dp),intent(out) :: av(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ord,spl_in,spr_in
 integer :: ix,iy,iz,spl,spr
 !real(dp) :: ar,ai
 !===================
 do iz=k1,k2
  do iy=j1,j2
   do ix=i1,i2
    av(ix,iy,iz,1)=0.5*(envf(ix,iy,iz,1)*envf(ix,iy,iz,1)+&
     envf(ix,iy,iz,2)*envf(ix,iy,iz,2))
    av(ix,iy,iz,1)=av(ix,iy,iz,1)+0.5*(env1f(ix,iy,iz,1)*env1f(ix,iy,iz,1)+&
     env1f(ix,iy,iz,2)*env1f(ix,iy,iz,2))
    !|A|^2/2 at current t^n time level
   end do
  end do
 end do
 spl=spl_in
 spr=spr_in
 if(spl >2)spl=2
 if(spr >2)spr=2

 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,1,1,spr,spl)

 call env_grad(av,i1,i2,j1,j2,k1,k2,ord,dx_inv,dy_inv,dz_inv)
 !Exit staggered grad|A|^2/2 in jc(2:4) or jc(2:3)

 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,2,nj_dim,spr,spl)

 call field_xyzbd(av,i1,i2,j1,j2,k1,k2,nj_dim,spr,spl)
 !=====================
 end subroutine env_amp_two_fields_prepare
 !=======================================
 !=============== ENV FLUID SECTION
 subroutine update_lpf2_fluid_variables(u,u0,flx,ef,dt_lp,i1,i2,j1,j2,k1,k2,lz0)
  real(dp),intent(inout) :: u(:,:,:,:),u0(:,:,:,:)
  real(dp),intent(inout) :: ef(:,:,:,:),flx(:,:,:,:)
  real(dp),intent(in) :: dt_lp,lz0
  integer,intent(in) :: i1,i2,j1,j2,k1,k2
  integer :: i,j,k,ic,ic1,str,stl,lp_step,fdim,fldim
  real(dp) :: pp(1:3),den,gam2,ch,gam_inv,lzf,apx,apy,apz,dtx,dty,dtz
  real(dp) :: dt2,ex,ey,ez,bx,by,bz,vx,vy,vz,qx,qy,qz,b1p,b1m
  real(dp),parameter :: wk1=0.5,eps=1.e-06
!===================================
! INTEGRATES by a one-step adam-bashfort (dissipative leap-frog)
!  ENTER u^n} u0=u^{n-1}
!===============================
!   NON-CONSERVATIVE FORM of RELATIVISTC COLD FLUID
!==========================================
!   D_t(p)+v*grad(p)=charge*[E +vxB+ F_env]
!   D_t(n) +div(nv) =0
!   arrays :  q(1:3)=v,  u(1:3)=p   gamm^2=1+p*p
!===============================
! enter ef=total (E,B) fields on staggered grid at t^n time level
!================================
  !dt2=2.*dt_lp
  dt2=(1.+fl_opt)*dt_lp
  lzf=lz0*unit_charge(1)*dt2
  apx=-dx_inv*dt2
  apy=-dy_inv*dt2
  apz=-dz_inv*dt2
  dtx=dx_inv*dt_lp
  dty=dy_inv*dt_lp
  dtz=dz_inv*dt_lp
  ch=dt_lp*unit_charge(1)
  fdim=curr_ndim+1
  fldim=2*curr_ndim+1
               !================== Enter
                    ! flx[Px,Py,Pz,den,vx,vy,vz]^n fldim components
                    ! ef[1:nfield] = total (E,B) fields
 !===============================================
   lp_step=1
   str=1
   stl=1
   if(prl)then
    !                                     !extends flux data to j1-2,j2+2 and k1-2,k2+2 
    call fill_ebfield_yzxbdsdata(flx,i1,i2,j1,j2,k1,k2,1,fldim,2,2)
    call fill_ebfield_yzxbdsdata(ef,i1,i2,j1,j2,k1,k2,1,nfield,str,stl)
   endif
  do ic=1,fdim
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      u0(i,j,k,ic)=(1.-fl_opt)*u(i,j,k,ic)+fl_opt*u0(i,j,k,ic) !opt_fl=0.5  for 2th adam-bashforth
     end do                                                    !opt_fl=1 for lpf
    end do
   end do
  end do
   call nc_fluid_density_momenta(flx,u0,i1,i2,j1,j2,k1,k2,fdim,apx,apy,apz)
   call add_lorentz_force  !u_0=u_0+ F(u)+q*Dt*[E+vxB+grad|A^2|/4*gam] for envelope
  do ic=1,fdim
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      u(i,j,k,ic)=u0(i,j,k,ic)              ! updated u^{n+1}
      u0(i,j,k,ic)=flx(i,j,k,ic)            ! u0= u^{n}
     end do
    end do
   end do
  end do
!==========================
  contains
!============== step=1 advances (E,B)^n => (E,B)^{n+1/2} ghost points already set
  subroutine add_lorentz_force
   real(dp) :: qp,qm
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
       den=1.
       if(flx(i,j,k,fdim)<=eps)den=0.0
       ex=wk1*(ef(i,j,k,1)+ef(i-1,j,k,1))  !Ex(i,j,k)
       ey=wk1*(ef(i,j,k,2)+ef(i,j-1,k,2))   !Ey(i,j,k)
       b1p=wk1*(ef(i,j,k,nfield)+ef(i-1,j,k,nfield))        !bz(i,j+1/2,k)
       b1m=wk1*(ef(i,j-1,k,nfield)+ef(i-1,j-1,k,nfield))    !bz(i,j-1/2,k)
       bz=wk1*(b1p+b1m)                !Bz(i,j,k)
       vx=flx(i,j,k,fdim+1)    !vx^n
       vy=flx(i,j,k,fdim+2)    !vy^n
       u0(i,j,k,1)=u0(i,j,k,1)+den*lzf*(ex+vy*bz)   !=> u^{n+1}
       u0(i,j,k,2)=u0(i,j,k,2)+den*lzf*(ey-vx*bz)
     end do
    end do
   end do
   if(curr_ndim==3)then
    do k=k1,k2
     do j=j1,j2
      do i=i1,i2
       den=1.
       if(flx(i,j,k,fdim)<=eps)den=0.0
       ez=wk1*(ef(i,j,k,3)+ef(i,j,k-1,3))  !Ez(i,j,k)
       b1p=wk1*(ef(i,j,k,5)+ef(i-1,j,k,5))        !by(i+1/2,j,k+1/2)
       b1m=wk1*(ef(i,j,k-1,5)+ef(i-1,j,k-1,5))    
       by=wk1*(b1p+b1m)                !By(i,j,k)
       b1p=wk1*(ef(i,j,k,4)+ef(i,j-1,k,4))        !bx(i,j+1/2,k+1/2)
       b1m=wk1*(ef(i,j,k-1,4)+ef(i,j-1,k-1,4))    
       bx=wk1*(b1p+b1m)                !Bx(i,j,k)
       vx=flx(i,j,k,fdim+1)    !vx^n
       vy=flx(i,j,k,fdim+2)    !vy^n
       vz=flx(i,j,k,fdim+3)    !vz^n
       u0(i,j,k,1)=u0(i,j,k,1)-den*lzf*vz*by 
       u0(i,j,k,2)=u0(i,j,k,2)+den*lzf*vz*bx
       u0(i,j,k,3)=u0(i,j,k,3)+den*lzf*(ez+vx*by-vy*bx)
                                         !=> u^{n+1}
      end do
     end do
    end do
   endif
!+++++++++++++++++++++++
  end subroutine add_lorentz_force
 end subroutine update_lpf2_fluid_variables
!===================
 subroutine env_fluid_curr_accumulate(u,u0,flx,curr,dt_lp,i1,i2,j1,j2,k1,k2)
  real(dp),intent(inout) :: u(:,:,:,:),u0(:,:,:,:)
  real(dp),intent(inout) :: flx(:,:,:,:),curr(:,:,:,:)
  real(dp),intent(in) :: dt_lp
  integer,intent(in) :: i1,i2,j1,j2,k1,k2
  integer :: i,j,k,ic,ic1,str,stl,lp_step,fdim
  real(dp) :: pp(1:3),den,gam2,ch,gam_inv
  real(dp) :: dt2,qx,qy,qz,b1p,b1m,ar,ai,av2
  real(dp),parameter :: wk1=0.5,eps=1.e-04

! Enter fluid variables at t^{n+1} and t^n and flx(1)= |a|^2/2 at t^{N+1/2}
 ch=dt_lp*wk1*unit_charge(1)
 fdim=curr_ndim+1
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     av2= flx(i,j,k,fdim)                 !time centered |A|^{n+1/2}/2
     den=0.5*(u(i,j,k,fdim)+u0(i,j,k,fdim))   !den^{n+1/2}
     pp(1:curr_ndim)=0.5*(u(i,j,k,1:curr_ndim)+u0(i,j,k,1:curr_ndim)) !p momenta at t^{n+1/2}
     gam2=1.+dot_product(pp(1:curr_ndim),pp(1:curr_ndim))
     gam2=gam2+av2
     gam_inv= 1./sqrt(gam2)
     do ic=1,curr_ndim
      flx(i,j,k,ic)=den*gam_inv*pp(ic)      !n*v= density flux at t^{n+1/2}
     end do
    end do
   end do
  end do
  call fill_ebfield_yzxbdsdata(flx,i1,i2,j1,j2,k1,k2,1,curr_ndim,1,1)
  do k=k1,k2
   do j=j1,j2
    i=i2
    flx(i+1,j,k,1)=flx(i,j,k,1)
    do i=i1,i2
     qx=ch*(flx(i,j,k,1)+flx(i+1,j,k,1))  !Dt*Jx(i+1/2,j,k)
     qy=ch*(flx(i,j,k,2)+flx(i,j+1,k,2))   !Dt*Jy(i,j+1/2,k)
     curr(i,j,k,1)=curr(i,j,k,1)+qx
     curr(i,j,k,2)=curr(i,j,k,2)+qy
    end do
   end do
  end do
  if(curr_ndim==3)then
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      qz=ch*(flx(i,j,k+1,3)+flx(i,j,k,3))  !Dt*Jz(i,j,k+1/2)
      curr(i,j,k,3)=curr(i,j,k,3)+qz
     end do
    end do
   end do
  endif
 !In curr(1:curr_ndim) exit  Dt*J^{n+1/2}
 end subroutine env_fluid_curr_accumulate

 subroutine set_env_momentum_density_flux(uv,ef,curr,eb_tot,flx,i1,i2,j1,j2,k1,k2)
  real(dp),intent(in) :: uv(:,:,:,:),ef(:,:,:,:)
  real(dp),intent(inout) :: curr(:,:,:,:)
  real(dp),intent(out) :: flx(:,:,:,:),eb_tot(:,:,:,:)
  integer,intent(in) :: i1,i2,j1,j2,k1,k2
  integer :: fdim,ic,i,j,k
  real(dp) :: den,pp(3),gam2,gam_inv
!================ set density and momenta flux
  fdim=curr_ndim+1
  flx(i1:i2,j1:j2,k1:k2,1:fdim)=uv(i1:i2,j1:j2,k1:k2,1:fdim)
  ! Enter curr(1)= |A|^2/2 and curr(2:4) grad|A|^2/2 at t^n time level
!=====================
   !NON CONSERVATIVE flx(1:4)=uv(1:4)=[Px,Py,Pz,den] flx(5:8)=[vx,vy,vz]
  do ic=1,nfield
   do k=k1,k2
    do j=j1,j2
     do i=i1,i2
      eb_tot(i,j,k,ic)=ef(i,j,k,ic)
     end do
    end do
   end do
  end do
  do k=k1,k2
   do j=j1,j2
    do i=i1,i2
     den=uv(i,j,k,fdim)
     pp(1:curr_ndim)=uv(i,j,k,1:curr_ndim)      !p momenta
     gam2=1.+dot_product(pp(1:curr_ndim),pp(1:curr_ndim))+curr(i,j,k,1)
     gam_inv= 1./sqrt(gam2)
     pp(1:curr_ndim)=gam_inv*pp(1:curr_ndim)!(vx,vy,vz)=pp/gam at time t^n
     curr(i,j,k,1)=gam_inv*den         !n/gam the sorce of envelope equation
     do ic=1,curr_ndim
      eb_tot(i,j,k,ic)=eb_tot(i,j,k,ic)+0.5*gam_inv*curr(i,j,k,ic+1)  !Envelope grad|A|^2/(4*gam_p)
      flx(i,j,k,fdim+ic)=gam_inv*uv(i,j,k,ic)  !(vx,vy,vz)
     end do
    end do
   end do
  end do
  end subroutine set_env_momentum_density_flux
!=================================
 subroutine env_lpf2_evolve(dt_loc,it_loc)

 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: it_loc
 integer :: np,ic,nyf,nzf,n_st
 integer :: lp,i1,j1,k1,i2,id_ch
 real(dp) :: xm,ym,zm,Ltz,ef2_ion,loc_ef2_ion(2)
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
 call pfields_prepare(ebf,i1,i2,j1,nyf,k1,nzf,nfield,2,2)
!======================================
 if(Ionization)then
  if(it_loc==0)then
   call init_random_seed(mype)
  endif
  id_ch=nd2+1
  call pfields_prepare(env,i1,i2,j1,nyf,k1,nzf,2,1,1)
  if(Two_color)then
   call pfields_prepare(env1,i1,i2,j1,nyf,k1,nzf,2,1,1)
   do ic=2,nsp_ionz
    np=loc_npart(imody,imodz,imodx,ic)
    if(np>0)then
     call set_ion_env_field(env1,spec(ic),ebfp,np,ndim,xm,ym,zm,om1)
     loc_ef2_ion(1)=maxval(ebfp(1:np,id_ch))
     loc_ef2_ion(1)=sqrt(loc_ef2_ion(1))
     call ionization_cycle(spec(ic),ebfp,np,ic,it_loc,1,de_inv)
!================
     call set_ion_env_field(env,spec(ic),ebfp,np,ndim,xm,ym,zm,oml)
     loc_ef2_ion(2)=maxval(ebfp(1:np,id_ch))
     loc_ef2_ion(2)=sqrt(loc_ef2_ion(2))
     ef2_ion=max(loc_ef2_ion(1),loc_ef2_ion(2))
     call ionization_cycle(spec(ic),ebfp,np,ic,it_loc,1,de_inv)
     if(ef2_ion > lp_max)then
      lp_max=1.1*ef2_ion
      call set_field_ioniz_wfunction(&
       ion_min(ic-1),atomic_number(ic-1),ic,ionz_lev,ionz_model,lp_max,dt_loc)
     endif
    endif
   end do
  else
   do ic=2,nsp_ionz
    np=loc_npart(imody,imodz,imodx,ic)
    if(np>0)then
     call set_ion_env_field(env,spec(ic),ebfp,np,ndim,xm,ym,zm,oml)
      loc_ef2_ion(1)=maxval(ebfp(1:np,id_ch))
      loc_ef2_ion(1)=sqrt(loc_ef2_ion(1))
      ef2_ion=loc_ef2_ion(1)
      if(ef2_ion > lp_max)then
       write(6,'(a22,i6,2E11.4)')'reset high ionz field ',mype,ef2_ion,lp_max
       lp_max=1.1*ef2_ion
       call set_field_ioniz_wfunction(&
       ion_min(ic-1),atomic_number(ic-1),ic,ionz_lev,ionz_model,lp_max,dt_loc)
      endif
     call ionization_cycle(spec(ic),ebfp,np,ic,it_loc,1,de_inv)
    endif
   end do
  endif
 endif
 ic=1
 Ltz=Lorentz_fact(ic)
 !Lorentz force on a particle => ebfp0(1:3), velocity ebfp0(3:4) stored
 ! all the time for t^{n-1} levels ==========
 !===========================
 jc(:,:,:,:)=0.0
 np=loc_npart(imody,imodz,imodx,ic)
 if(Two_color)then
  call env_amp_two_fields_prepare(env,env1,jc,i1,i2,j1,nyf,k1,nzf,2,2,2)
 else
  call env_amp_prepare(env,jc,i1,i2,j1,nyf,k1,nzf,2,2,2)
 endif
  !======================================
 ! exit jc(1)=|a|^2/2 at t^n
  !      jc(2:4)=grad|a|^2/2 at t^n
  ! For two-color |A|= |A_0|+|A_1|
  !======================================
  call set_env_acc(ebf,jc,spec(ic),ebfp,np,curr_ndim,dt_loc,xm,ym,zm)
                            !call field_charge_multiply(spec(ic),ebfp,1,np,nfield)
  !exit ebfp(1:3)=q*[E+F] ebfp(4:6)=q*B/gamp, ebfp(7)=wgh/gamp at t^n
  !Lorentz force already multiplied by particle charge
  !jc(1:4) not modified
 !====================
 call lpf_env_momenta(spec(ic),ebfp,np,dt_loc,Ltz)
  ! Updates particle momenta P^{n-1/2} => P^{n+1/2}
  ! stores in ebfp(1:3)=old (x,y,z)^n ebfp(7)=wgh/gamp >0
 !======================
  if(Hybrid)then
   call set_env_momentum_density_flux(up,ebf,jc,ebf0,flux,i1,i2,j1,nyf,k1,nzf)
    !exit jc(1)=q^2*n/gam, jc(2:4) ponderomotive force on a grid
    !ebf0= total fields 

   call update_lpf2_fluid_variables(up,up0,flux,ebf0,dt_loc,i1,i2,j1,nyf,k1,nzf,Ltz)
   ! In up,up0 exit updated momenta-density variables (u^{n+1}, u0^{n})

   flux(i1:i2,j1:nyf,k1:nzf,1)=jc(i1:i2,j1:nyf,k1:nzf,1)

   ! in flux(1) exit the fluid contribution of the sorce term q^2*n/gam
   ! for the envelope field solver
  endif
 jc(:,:,:,1)=0.0
 call set_env_density(ebfp,jc,np,curr_ndim,1,xm,ym,zm)

 call env_den_collect(jc,i1,i2,j1,nyf,k1,nzf)
  ! in jc(1)the particle contribution of the source term <q^2*n/gamp>
  ! to be added to the fluid contribution if (Hybrid)
  jc(i1:i2,j1:nyf,k1:nzf,3)=jc(i1:i2,j1:nyf,k1:nzf,1)
  if(Hybrid)then
   jc(i1:i2,j1:nyf,k1:nzf,3)=jc(i1:i2,j1:nyf,k1:nzf,3)+&
                             flux(i1:i2,j1:nyf,k1:nzf,1)
  endif
!===================
 ! in the envelope equation (A^{n-1},A^n)==> (A^n,A^{n+1})
 ! jc(3) = <q^2n/gam>
 ! Jc(1:2)=-ompe*jc(3)*A at level t^n
 !==================================================
 !==================================================
 call advance_lpf_envelope(jc,env,dt_loc,oml,i1,i2,j1,nyf,k1,nzf)
 !advance (A^n, J^n) => A^{n+1}, A^{n-1}=> A^n
 ! jc(3) not modified
 if(Two_color)call advance_lpf_envelope(jc,env1,dt_loc,om1,i1,i2,j1,nyf,k1,nzf)
 !advance (A_1^n, J^n) => A_1^{n+1}, A_1^{n-1}=> A_1^n
!=======================
 if(Two_color)then
  call env_two_fields_average(env,env1,jc,i1,i2,j1,nyf,k1,nzf,2,2)
 else
  call env_fields_average(env,jc,i1,i2,j1,nyf,k1,nzf,2,2)
 endif
 ! In jc(1)= F= |A|^2/2 +|A_1|/2 at t^{n+1/2}  in jc(2:4) grad[F]
 if(Hybrid)then
  flux(i1:i2,j1:nyf,k1:nzf,curr_ndim+1)=jc(i1:i2,j1:nyf,k1:nzf,1)
  !stores in flux()
 endif
  call set_env_interp(jc,spec(ic),ebfp,np,curr_ndim,xm,ym,zm)
  !=============================
  ! Exit p-interpolated field variables
  ! at time level t^{n+1/2} and positions at time t^n
  ! in ebfp(1:3)=grad|A|^2/2 ebfp(4)=|A|^2/2 in 3D
  ! in ebfp(1:2)=grad|A|^2/2 ebfp(3)=|A|^2/2 in 2D
  !=====================================
   call lpf_env_positions(spec(ic),ebfp,np,dt_loc,vbeam)
   if(ompe==0.0)return
  !===========================
  ! ebfp(1:3) dt*V^{n+1/2}  ebfp(4:6) old positions for curr J^{n+1/2}
  ! ebfp(7)=dt*gam_inv
 !==============================
 jc(:,:,:,:)=0.0
  call curr_accumulate(spec(ic),ebfp,jc,1,np,iform,n_st,xm,ym,zm)
 !===========================
 call curr_mpi_collect(jc,i1,i2,j1,nyf,k1,nzf)
 if(Hybrid)then
  call env_fluid_curr_accumulate(up,up0,flux,jc,dt_loc,i1,i2,j1,nyf,k1,nzf)
  !Computes fluid contribution => J^{n+1/2} and adds to particle contribution
 endif
 !====================
 ! Jc(1:3) for total curr Dt*J^{n+1/2}
 lp_in(1)=lp_in(1)+dt_loc
 call advance_lpf_fields(ebf,jc,dt_loc,vbeam,&
                          i1,i2,j1,nyf,k1,nzf,0)
 ! (E,B) fields at time t^{n+1}
 !-----------------------------
 end subroutine env_lpf2_evolve
!=============== END ENV PIC SECTION
!---------------------------------------------
 subroutine advance_env_rk4_part(F_pt,F_pt0,F_pt1,sp_loc,np,dtloc,Lfact,lp)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(inout) :: F_pt(:,:)
 real(dp),intent(out) :: F_pt0(:,:),F_pt1(:,:)

 integer,intent(in) :: np,lp
 real(dp),intent(in) :: dtloc,Lfact
 integer :: p,ndv
 real(dp) :: alp,afact,dt_lp,vp(3),efp(3),fploc(6)
 real(sp) :: wght

 dt_lp=dtloc*b_rk(lp)
 afact=dt_lp*Lfact
 ndv=2*curr_ndim
 !===========================
 !in F_pt(1:2) the [E+F_env],F_pt(3)=B_z/gam_p F_pt(5)= q*wgh*v fields on a particle at t^{k-1}
 if(lp==1)then
  do p=1,np
   F_pt0(p,1:ndv)=sp_loc%part(p,1:ndv)
   F_pt1(p,1:ndv)=c_rk(0)*F_pt0(p,1:ndv)
  end do
 endif
 select case(curr_ndim)
  ! Enter F_pt(1:3)=[Ex+Fx,Ey+Fy,Bz/gamp]
 case(2)
  do p=1,np
   wgh_cmp=sp_loc%part(p,5)         !stores p-weight and charge
   alp=charge*afact
   wght=charge*wgh
   fploc(1:4)=F_pt(p,1:4)

   vp(1:2)=sp_loc%part(p,3:4)

   efp(1)=alp*(fploc(1)+vp(2)*fploc(3))
   efp(2)=alp*(fploc(2)-vp(1)*fploc(3))
   F_pt(p,3:4)=dt_lp*F_pt(p,5)*vp(1:2)             !dt_rk*V^{k-1}

   sp_loc%part(p,3:4)=F_pt0(p,3:4)+efp(1:2)

   F_pt(p,1:2)=sp_loc%part(p,1:2)     !current positions
   sp_loc%part(p,1:2)=F_pt0(p,1:2)+F_pt(p,3:4) !advances positions
   F_pt(p,3:4)=wght*F_pt(p,3:4)     !q*wgh*dt_rk*V^{k-1} => curr
   F_pt(p,5)=wgh*F_pt(p,5)     !wgh/gamp => curr_env
  end do
 case(3)
  !===========================
  !in F_pt(1:3) the [E+F_env],F_pt(4:6)=B/gam_p F_pt(7)= q*wgh*v fields on a particle at t^{k-1}
  !========================
  ! RK4 integration scheme p^k=p_0+Dt*b_k*F^{k-1},k=1,2,3,4
  do p=1,np
   wgh_cmp=sp_loc%part(p,7)         !stores part(weight-charge)
   alp=charge*afact
   wght=charge*wgh
   vp(1:3)=sp_loc%part(p,4:6)   !P^{k-1}
   fploc(1:6)=F_pt(p,1:6)         !F=(E+F_env),B/gam_p at t^{k-1}

   efp(1)=alp*(fploc(1)+vp(2)*fploc(6)-vp(3)*fploc(5))
   efp(2)=alp*(fploc(2)+vp(3)*fploc(4)-vp(1)*fploc(6))
   efp(3)=alp*(fploc(3)+vp(1)*fploc(5)-vp(2)*fploc(4))

   F_pt(p,4:6)=dt_lp*F_pt(p,7)*vp(1:3)             !dt_rk*V^{k-1}
   sp_loc%part(p,4:6)=F_pt0(p,4:6)+efp(1:3)  !=> p^k

   F_pt(p,1:3)=sp_loc%part(p,1:3)     ! old positions  x^{k-1}
   sp_loc%part(p,1:3)=F_pt0(p,1:3)+F_pt(p,4:6) !advances positions x^k
   F_pt(p,4:6)=wght*F_pt(p,4:6) !q*wgh*dt_rk*V^{k-1} => curr^{k-1}
   F_pt(p,7)=wgh*F_pt(p,7)     !wgh/gamp => curr_env
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
 end subroutine advance_env_rk4_part
 !==============================
 !==============================
 subroutine env_rk_evolve(dt_loc,rk)
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
 ic=1
 Ltz=Lorentz_fact(ic)
 if(Part)then
 do lps=1,rk
  jc(:,:,:,:)=0.0
  call pfields_prepare(ebf,i1,i2,j1,nyf,k1,nzf,nfield,2,2)
 if(Two_color)then
  call env_amp_two_fields_prepare(env,env1,jc,i1,i2,j1,nyf,k1,nzf,4,2,2)
 else
  call env_amp_prepare(env,jc,i1,i2,j1,nyf,k1,nzf,4,2,2)
 endif
  ! exit jc(1)=|a|^2/2 at t^n
  ! exit jc(2:4)=grad|a|^2/2 at t^n
  ! For two-color |A|= |A_0|+|A_1|
  !======================================
  np=loc_npart(imody,imodz,imodx,ic)
  if(np>0)then
   call set_env_rk_acc(ebf,jc,spec(ic),ebfp,np,curr_ndim,xm,ym,zm)
   !exit ebfp(1:3)=[E+F] ebfp(4:6)=B/gamp, ebfp(7)=1/gamp at t^{k-1}
   call advance_env_rk4_part(ebfp,ebfp0,ebfp1,spec(ic),np,dt_loc,Ltz,lps)
   !====================
   ! stores in ebfp(1:3)=(x,y,z)^{k-1}; in ebf(4:6)=dt_rk*q*wgh*(v_x,v_y,v_z)^{k-1}; in ebfp(7)=wgh*q/gamp
   jc(:,:,:,1)=0.0    !enters ebf(7) = wgh/gam_p at t^{k-1}
   call set_env_density(ebfp,jc,np,curr_ndim,1,xm,ym,zm)
  endif
  call env_den_collect(jc,i1,i2,j1,nyf,k1,nzf)
  call advance_env_rk4_field(&
   jc,env,dt_loc,i1,i2,j1,nyf,k1,nzf,lps,rk)
  !======================
  if(np>0)then
   jc(:,:,:,:)=0.0
   ! in ebfp(1:6)[x,v*dt_k] at time (lps-1)
   call ncdef_rk_curr(ebfp,jc,nst,np,ndim,xm,ym,zm)
  endif
  call curr_mpi_collect(jc,i1,i2,j1,nyf,k1,nzf)
  call update_rk4_fields(ebf,ebf0,ebf1,jc,&
   vbeam,dt_loc,i1,i2,j1,nyf,k1,nzf,lps)
 end do
 else
 do lps=1,rk
  jc(:,:,:,1)=0.0
  call advance_env_rk4_field(&
    jc,env,dt_loc,i1,i2,j1,nyf,k1,nzf,lps,rk)
 end do
 endif
 !-----------------------------
 end subroutine env_rk_evolve
 !=======================
 subroutine ENV_run(t_loc,dt_loc,iter_loc,t_ord)

 real(dp),intent(in) :: t_loc,dt_loc
 integer,intent(in) :: iter_loc,t_ord
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
 if(Comoving)then
  if(t_loc>=wi_time.and.t_loc< wf_time)then
   if(mod(iter_loc,w_sh)==0)then
    call comoving_coordinate(vbeam,dt_loc,w_sh,iter_loc)
   endif
  endif
 endif
 select case(t_ord)
  !=========================
 case(2)
  call env_lpf2_evolve(dt_loc,iter_loc)
 case(4)
  call env_rk_evolve(dt_loc,t_ord)
 end select
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
 subroutine lpf2_eb_evolve(dt_loc,iter_loc,initial_time)

 real(dp),intent(in) :: dt_loc
 integer,intent(in) :: iter_loc
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
 !Ghost cell values for field assignement on particles
 vb=vbeam
 call pfields_prepare(ebf,i1,i2,j1,j2,k1,k2,nfield,1,1)
 call pfields_prepare(ebf_bunch,i1,i2,j1,j2,k1,k2,nfield,1,1)
 call pfields_prepare(ebf1_bunch,i1,i2,j1,j2,k1,k2,nfield,1,1)
!======== first new plasma electrons are injected by ionization
 if(Ionization)then
  if(iter_loc==0)then
   call init_random_seed(mype)
  endif
  do ic=2,nsp_ionz
   np=loc_npart(imody,imodz,imodx,ic)
   if(np>0)then
     call set_ion_two_Ebfield(ebf,ebf_bunch,ebf1_bunch,spec(ic),&
                      ebfp,np,n_st,ndim,xm,ym,zm)
    if(mod(iter_loc,50)==0)then     !refresh ionization tables
     loc_ef2_ion(1)=maxval(ebfp(1:np,id_ch))
     loc_ef2_ion(1)=sqrt(loc_ef2_ion(1))/514.   !In atomic units
     if(ef2_ion(1) > eb_max)then
      eb_max=1.1*ef2_ion(1)
      call set_field_ioniz_wfunction(ion_min(ic-1),atomic_number(ic-1),ic,&
                                     ionz_lev,ionz_model,eb_max,dt_loc)
     endif
    endif
   call ionization_cycle(&
                        spec(ic),ebfp,np,ic,iter_loc,0,deb_inv)
   endif
  end do
  !======== injects new electrons, with weights equal to ion weights
 endif
!=======================
! STEP 1
!Advances momenta and position of plasma particles
!==========================
  jc(:,:,:,:)=0.0
  do ic=1,nsp_run
   np=loc_npart(imody,imodz,imodx,ic)
   Ltz=Lorentz_fact(ic)
   if(np >0)then
    if(nfield<6)then       !2D (x,y) geometry
    call set_part2d_two_bfield_acc(ebf,ebf_bunch,ebf1_bunch,&
                    spec(ic),ebfp,np,n_st,xm,ym)
    else
     call set_part3d_two_bfield_acc(ebf,ebf_bunch,ebf1_bunch,&
                    spec(ic),ebfp,1,np,n_st,xm,ym,zm)
    endif
    call field_charge_multiply(spec(ic),ebfp,1,np,nfield)
!==================================
   !EXIT ebfp(1:6)  total=wake + bunch fields assigned to plasma particle position
!==================================
    if(initial_time)call init_lpf_momenta(spec(ic),ebfp,1,np,dt_loc,Ltz)
    call lpf_momenta_and_positions(spec(ic),ebfp,1,np,dt_loc,vb,Ltz)
!  EXIT p^{n+1/2}, v^{n+1/2}, x^{n+1}
!  in x^{n+1} are stored in ebfp(1:3) old x^n are stored in ebfp((4:6)
!  in ebfp(7) is stored dt_loc/gamma
   call curr_accumulate(spec(ic),ebfp,jc,1,np,iform,n_st,xm,ym,zm)
 endif
  end do
  call curr_mpi_collect(jc,i1,i2,j1,j2,k1,k2)
! EXIT current density jc(1:3) due to plasma particles density and velocity at
! time t^{n+1/2}
!==============================
! STEP 2
!Advances momenta and position of bunch particles
!==========================
 jb(:,:,:,:)=0.0
 do ic=1,nsb
  np=loc_nbpart(imody,imodz,imodx,ic)
  Ltz=Lorentz_bfact(ic)
  if(np >0)then
   if(nfield<6)then
    call set_part2d_two_bfield_acc(ebf,ebf_bunch,ebf1_bunch,&
                   bunch(ic),ebfb,np,n_st,xm,ym)
   else
    if(L_Bpoloidal)then
     call set_part3d_three_bfield_acc(ebf,ebf_bunch,&
                               ebf1_bunch,ebf0_bunch,bunch(ic),ebfb,1,np,n_st,xm,ym,zm)
    else
     call set_part3d_two_bfield_acc(ebf,ebf_bunch,&
    ebf1_bunch,bunch(ic),ebfb,1,np,n_st,xm,ym,zm)
    endif
   endif
   call field_charge_multiply(bunch(ic),ebfb,1,np,nfield)
!==================================
   !EXIT ebfb(1:6)  total=wake + bunch fields assigned to bunch particle position
!==================================
   if(initial_time)call init_lpf_momenta(bunch(ic),ebfb,1,np,dt_loc,Ltz)
   call lpf_momenta_and_positions(bunch(ic),ebfb,1,np,dt_loc,vb,Ltz)
   if(L_EMBunchEvolution .and. ompe>0.0) call curr_accumulate(bunch(ic),ebfb,jb,1,np,iform,n_st,xm,ym,zm)

  endif
 enddo
 call curr_mpi_collect(jb,i1,i2,j1,j2,k1,k2)
! EXIT current density jb(1:3) due to bunch particles density and velocity at
! time t^{n+1/2}
! STEP3  advances fields
!=======================
 if(L_EMBunchEvolution) call advance_lpf_fields(ebf,jc,dt_loc,vbeam,i1,i2,j1,j2,k1,k2,1)
 !======================= boundary ibx as for Maxwell equation
 if(ibeam > 0)then
  call advect_bunch_fields(ebf_bunch,jb,&
                          bet0,dt_loc,i1,i2,j1,j2,k1,k2,initial_time)
 endif
 call advance_lpf_fields(ebf1_bunch,jb,dt_loc,vbeam,i1,i2,j1,j2,k1,k2,1)  !here reflecting bds
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
 if(Comoving)then
  if(ts>=wi_time.and.ts< wf_time)then
   if(mod(iter_loc,w_sh)==0)then
    call comoving_coordinate(vbeam,dt_loc,w_sh,iter_loc)
   endif
  endif
 endif
 !=========================
  call lpf2_eb_evolve(dt_loc,iter_loc,init_time)
 !
 call cell_part_dist(mw)
 call cell_bpart_dist(mw)
 ts=ts+dt_loc
 end subroutine BUNCH_run
 !================================
 end module pic_evolve_in_time
!==============================
