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

 module pic_out_util
 use particles
 use pic_rutil
 use psolv

 implicit none
 !=====Contains functions to prepare selected output variables=======
 contains
!============================================
 subroutine loc_env_amp(envf,av)
 real(dp),intent(in) :: envf(:,:,:,:)
 real(dp),intent(out) :: av(:,:,:,:)
 integer  :: i1,i2,j1,j2,k1,k2
 integer  :: ix,iy,iz
 !real(dp) :: ar,ai
 !===================
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 j2=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 k2=loc_zgrid(imodz)%p_ind(2)
!========================
 do iz=k1,k2
  do iy=j1,j2
   do ix=i1,i2
    av(ix,iy,iz,1)=0.5*(envf(ix,iy,iz,1)*envf(ix,iy,iz,1)+&
    envf(ix,iy,iz,2)*envf(ix,iy,iz,2))
    !|A|^2/2 at current t^n time level
   end do
  end do
 end do
 if(prl)call fill_ebfield_yzxbdsdata(av,i1,i2,j1,j2,k1,k2,1,1,2,2)
 end subroutine loc_env_amp
!=============== Tracking particles============
 subroutine initial_tparticles_select(sp_loc,dt_loc,tx1,tx2,ty1,ty2,tz1,tz2)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(in):: dt_loc,tx1,tx2,ty1,ty2,tz1,tz2

 integer :: np,p,ik,ndv,ik_max
 integer(hp_int) :: plab,last_ind
 real(dp) :: xx,yy,zz

 np=size(sp_loc%part,1)
 ndv=size(sp_loc%part,2)
!========================
! Define track_tot_nstep
 p=0
 if(dt_loc > 0.0)p=nint((t_out-t_in)/dt_loc)
 track_tot_nstep=nint(real(p,dp)/real(tkjump,dp))
 ndv=size(sp_loc%part,2)
! Select particles on each mpi_task
 ik=0
 select case(ndim)
 case(2)
 do p=1,np,nkjump
  xx=sp_loc%part(p,1)
  yy=sp_loc%part(p,2)
  if(tx1 < xx.and. tx2 > xx)then
   if(ty1 < yy.and. ty2 > yy)then
   ik=ik+1
   wgh_cmp=sp_loc%part(p,5)
   part_ind=int(ik,hp_int)
   sp_loc%part(p,5)=wgh_cmp
   endif
  endif
 end do
 case(3)
 do p=1,np,nkjump
  xx=sp_loc%part(p,1)
  yy=sp_loc%part(p,2)
  zz=sp_loc%part(p,3)
  if(tx1 < xx.and. tx2 > xx)then
   if(ty1 < yy.and. ty2 > yy)then
    if(tz1 < zz.and. tz2 > zz)then
     ik=ik+1
     wgh_cmp=sp_loc%part(p,7)
     part_ind=int(ik,hp_int)
     sp_loc%part(p,7)=wgh_cmp
    endif
   endif
  endif
 end do
 end select
 loc_tpart(mype+1)=ik
 call intvec_distribute(ik,loc_tpart,npe)
 track_tot_part=sum(loc_tpart(1:npe))
 ik_max=maxval(loc_tpart(1:npe))
 last_ind=0
 if(mype==1)last_ind=int(loc_tpart(1),hp_int)
 if(mype>1)last_ind=int(sum(loc_tpart(1:mype)),hp_int)
 !if(loc_tpart(mype+1)>0)write(6,*)'last particle index',mype,last_ind
 do p=1,np
  wgh_cmp=sp_loc%part(p,ndv)
  if(part_ind >0)part_ind=part_ind+last_ind
  sp_loc%part(p,ndv)=wgh_cmp
 enddo
 allocate(track_aux(2*ndv*ik_max))
 if(pe0)then
  allocate(pdata_tracking(ndv,track_tot_part,track_tot_nstep))
  write(6,*)'==== Initial track-Particle data==========='
  write(6,'(a19,i6)')'  tot_track_steps  ',track_tot_nstep
  write(6,'(a19,i6)')'  tot_track_parts  ',track_tot_part
  write(6,'(a18,i8)')'  short_int size  ',huge(plab)
  write(6,'(a20,e11.4)')'  ptrack memory(MB) ',1.e-06*real(4*ndv*track_tot_nstep*track_tot_part,dp)
 endif
!================================
 end subroutine initial_tparticles_select
!============================================
 subroutine t_particles_collect(sp_loc,time_ind)

 type(species),intent(in) :: sp_loc
 integer,intent(in) :: time_ind

 integer :: np,ik,ik_max,ip,p,ndv,ndvp,ipe,kk,ik1,ik2,nst
 logical :: sr
 real :: xm,ym,zm

 if(time_ind > track_tot_nstep)return
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 np=loc_npart(imody,imodz,imodx,1)
 nst=0
 if(Stretch)nst=str_indx(imody,imodz)
!===================

 ndv=size(sp_loc%part,2)
 ndvp=ndv
 ik=0
 kk=0
 do p=1,np
  wgh_cmp=sp_loc%part(p,ndv)
  if(part_ind >0)ik=ik+1
 enddo
 loc_tpart(mype+1)=ik
 call intvec_distribute(ik,loc_tpart,npe)
 ik_max=maxval(loc_tpart(1:npe))
 if(ndvp*ik_max > size(track_aux))then
  deallocate(track_aux)
  allocate(track_aux(ndvp*ik_max))
 endif
 ik=0
 do p=1,np
  wgh_cmp=sp_loc%part(p,ndv)
  if(part_ind >0)then
   ik=ik+1
   do ip=1,ndv
    kk=kk+1
    track_aux(kk)=sp_loc%part(p,ip)
   enddo
  endif
 enddo
!=================
 if(pe0)then
  sr=.false.
  ik1=0
  ik2=0
  do ipe=1,npe-1
   ik=loc_tpart(ipe+1)
   if(ik >0)then
    call exchange_1d_grdata(sr,track_aux,ik*ndvp,ipe,ipe+10)
    !pe0 receives from ipe ik sp_aux data and collects on track array
    kk=0
    do p=1,ik
     ik2=ik1+p
     do ip=1,ndv-1
      kk=kk+1
      pdata_tracking(ip,ik2,time_ind)=track_aux(kk)
     enddo
     kk=kk+1
     wgh_cmp=track_aux(kk)
     pdata_tracking(ndv,ik2,time_ind)=part_ind
    enddo
    ik1=ik2
   endif
  enddo
  loc_tpart(1)=ik2
 else
  ik=loc_tpart(mype+1)
  if(ik >0)then
   sr=.true.
   call exchange_1d_grdata(sr,track_aux,ik*ndvp,0,mype+10)    !sends ik data to pe0
  endif
 endif
 end subroutine t_particles_collect
!=================================================
 subroutine fill_density_data(den,i1,i2,j1,j2,k1,k2,ic)
 real(dp),intent(inout)  :: den(:,:,:,:)
 integer,intent(in) :: i1,i2,j1,j2,k1,k2,ic
 integer :: i,j,k,iy,iz,n_loc
 n_loc=loc_ygrid(imody)%ng
 do k=k1,k2
  do j=j1,j2-1
   iy=j+imody*n_loc
   if(y(iy) > ymin_t.and.y(iy)<ymax_t)then
    do i=i1,i2-1
     if(x(i)>=targ_in)den(i,j,k,ic)=den(i,j,k,ic)+1.
    end do
   endif
  enddo
 enddo
 if(ndim <3)return
 n_loc=loc_zgrid(imodz)%ng
 do k=k1,k2-1
  iz=k+imodz*n_loc
  if(z(iz)> zmin_t.and. z(iz) < zmax_t)then
   do j=j1,j2-1
    do i=i1,i2-1
     den(i,j,k,ic)=den(i,j,k,ic)+1.
    end do
   end do
  endif
 enddo
 end subroutine fill_density_data
!=============================================
 subroutine collect_bunch_and_plasma_density(this_bunch,isp)

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
 !do ik=1,2
 ! call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,ik)
 !end do
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
 !do ik=1,2
 ! call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,ik)
 !end do
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
 !======================
 !=============================
 end subroutine prl_bden_energy_interp
 !============================
 subroutine prl_den_energy_interp(ic)
 integer,intent(in) :: ic
 integer :: nyf,nzf,np,i1,i2,j1,k1
 real(dp) :: xm,ym,zm,dery,derz,ar,ai
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
 !=========== Construct grid-density
 n_str=0
 if(Stretch)n_str=str_indx(imody,imodz)
 do i=1,2
  jc(:,:,:,i)=0.0
 end do
 !curr_clean
 np=loc_npart(imody,imodz,imodx,ic)
 if(Envelope)then
  do k=k1,nzf
   do j=j1,nyf
    do i=i1,i2
     ar=.5*(env(i,j,k,1)+env(i,j,k,3))
     ai=.5*(env(i,j,k,2)+env(i,j,k,4))
     jc(i,j,k,3)=0.5*(ar*ar+ai*ai)
    end do
   end do
  end do
  if(prl)call fill_ebfield_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,3,3,2,2)
  call set_grid_den_env_energy(&
   spec(ic),ebfp,jc,np,ndim,curr_ndim,n_str,3,xm,ym,zm)
   ! in jc(1) is the plasma density in jc(2) (gam-1)density with gamma with
   ! envelope component
 else
  call set_grid_den_energy(&
  spec(ic),ebfp,jc,np,ndim,curr_ndim,n_str,xm,ym,zm)
  ! in jc(1) is plasma norm density in jc(2) <(gam-1)density>  with
  ! kineticagamma
 endif
 !========= den on [i1-1:i2+2,j1-1:nyp+2,k1-1:nzp+2]
 if(prl)then
  call fill_curr_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,2)
 endif
 !do kk=1,2
 ! call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,kk)
 !end do
 if(ic==1)jc(i1:i2,j1:nyf,k1:nzf,1)=-jc(i1:i2,j1:nyf,k1:nzf,1)
 !if(Hybrid)jc(i1:i2,j1:nyf,k1:nzf,1)=jc(i1:i2,j1:nyf,k1:nzf,1)+up(i1:i2,j1:nyf,k1:nzf,nfcomp)
 jc(i1:i2,j1:nyf,k1:nzf,2)=mass(ic)*electron_mass*jc(i1:i2,j1:nyf,k1:nzf,2)
 !=========== energy density in Mev*n/n_0
 if(Stretch)then
  select case(ndim)
  case(2)
  k=1
  do j=j1,nyf
  jj=j-2
   dery=loc_yg(jj,3,imody)
   do i=i1,i2
    jc(i,j,k,1)=dery*jc(i,j,k,1)
    jc(i,j,k,2)=dery*jc(i,j,k,2)
   end do
  end do
  case(3)
  do k=k1,nzf
   kk=k-2
   derz=loc_zg(kk,3,imodz)
   do j=j1,nyf
    jj=j-2
    dery=loc_yg(jj,3,imody)*derz
    do i=i1,i2
     jc(i,j,k,1)=dery*jc(i,j,k,1)
     jc(i,j,k,2)=dery*jc(i,j,k,2)
    end do
   end do
  end do
  end select
 endif
 !======================
 end subroutine prl_den_energy_interp
!
 subroutine set_wake_potential

 integer :: nyf,nzf,np,i1,i2,j1,k1
 real(dp) :: xm,ym,zm
 integer :: ic,n_str,ft_mod,ft_sym


 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyf=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzf=loc_zgrid(imodz)%p_ind(2)

 !=========== Construct grid  (rho-Jx)
 n_str=0
 jc(:,:,:,1:2)=0.0
 !curr_clean
 do ic=1,nsp
  np=loc_npart(imody,imodz,imodx,ic)
  if(np>0)call set_grid_charge_and_Jx(&
                                   spec(ic),ebfp,jc,np,ndim,dt,xm,ym,zm)
 end do
 !========= jc(1)=charge density jc(2)= Jx at t^{n+1/2}
 if(prl)then
  call fill_curr_yzxbdsdata(jc,i1,i2,j1,nyf,k1,nzf,2)
 endif
 if(nsp==1)then
  call fill_density_data(jc,i1,i2,j1,nyf,k1,nzf,1)
 else
  if(dmodel_id==3)call fill_density_data(jc,i1,i2,j1,nyf,k1,nzf,1)
 endif
 jc(i1:i2,j1:nyf,k1:nzf,1)=jc(i1:i2,j1:nyf,k1:nzf,1)-jc(i1:i2,j1:nyf,k1:nzf,2)
!============== jc(1)=rho-Jx=======================

 ft_mod=2                                          !for cosine transform
 ft_sym=2
!-------------------------------------------
 call FFT_2D_Psolv(jc,ompe,nx,nx_loc,ny,ny_loc,nz,nz_loc,&
                                i1,i2,j1,nyf,k1,nzf,ft_mod,ft_sym)

 !==================================
 end subroutine set_wake_potential
 !============================
 end module pic_out_util
