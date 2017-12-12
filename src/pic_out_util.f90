 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
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
!=============== Tracking particles============
 subroutine initial_tparticles_select(sp_loc,dt_loc,tx1,tx2,ty1,ty2,tz1,tz2)

 type(species),intent(inout) :: sp_loc
 real(dp),intent(in):: dt_loc,tx1,tx2,ty1,ty2,tz1,tz2

 integer :: np,p,ik,ndv,ik_max,last_ind
 integer(hp_int) :: plab
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
  if(tx1 <= xx.and. tx2 > xx)then
   if(ty1 <= yy.and. ty2 > yy)then
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
  if(tx1 <= xx.and. tx2 > xx)then
   if(ty1 <= yy.and. ty2 > yy)then
    if(tz1 <= zz.and. tz2 > zz)then
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
 allocate(pdata_tracking(ndv,ik_max,track_tot_nstep))
 last_ind=0
 if(mype >0)last_ind=sum(loc_tpart(1:mype))
 if(loc_tpart(mype+1)>0)write(6,*)'last_ind',mype,last_ind
 do p=1,np 
  wgh_cmp=sp_loc%part(p,ndv)
  if(part_ind >0)part_ind=part_ind+int(last_ind,hp_int)
  sp_loc%part(p,ndv)=wgh_cmp
 enddo
 if(pe0)then
  write(6,'(a19,i6)')'  tot_track_steps  ',track_tot_nstep
  write(6,'(a19,i6)')'  tot_track_parts  ',track_tot_part
  write(6,'(a19,i6)')'  Max track_parts  ',ik_max
  write(6,'(a18,i8)')'  short_int size  ',huge(plab)
  write(6,'(a20,e11.4)')'  ptrack memory(MB) ',1.e-06*real(4*ndv*track_tot_nstep*ik_max,dp)
 endif
!================================

 end subroutine initial_tparticles_select
!============================================
 subroutine t_particles_collect(sp_loc,sp_aux,time_ind)

 type(species),intent(in) :: sp_loc
 real(dp),intent(inout) :: sp_aux(:,:)
 integer,intent(in) :: time_ind

 integer :: np,ik,ip,p,ndv

 if(time_ind > track_tot_nstep)return
 np=size(sp_loc%part,1)
 ndv=size(sp_loc%part,2)
 ik=0
 do p=1,np
  wgh_cmp=sp_loc%part(p,ndv)
  if(part_ind >0)then
   ik=ik+1
   do ip=1,ndv
    sp_aux(ik,ip)=sp_loc%part(p,ip)
   enddo
  endif
 enddo
 loc_tpart(mype+1)=ik
 if(ik >0)then
  !write(6,*)mype,ik,size(pdata_tracking,2),time_ind
 do p=1,ik
  do ip=1,ndv-1
   pdata_tracking(ip,p,time_ind)=real(sp_aux(p,ip),sp)
  enddo
  wgh_cmp=sp_aux(p,ndv)
  pdata_tracking(ndv,p,time_ind)=real(part_ind,sp)
 enddo
 endif
 end subroutine t_particles_collect
!=================================================
 subroutine fill_density_data(den,i1,i2,j1,j2,k1,k2,ic)
  real(dp),intent(inout)  :: den(:,:,:,:)
  integer,intent(in) :: i1,i2,j1,j2,k1,k2,ic
  integer :: i,ii,j,k,iy,iz,n_loc

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
   end do
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
 !do kk=1,2
 ! call den_zyxbd(jc,i1,i2,j1,nyf,k1,nzf,kk)
 !end do
 if(ic==1)jc(i1:i2,j1:nyf,k1:nzf,1)=-jc(i1:i2,j1:nyf,k1:nzf,1)
 !if(Hybrid)jc(i1:i2,j1:nyf,k1:nzf,1)=jc(i1:i2,j1:nyf,k1:nzf,1)+up(i1:i2,j1:nyf,k1:nzf,nfcomp)
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
!
 subroutine set_wake_potential

 integer :: nyf,nzf,np,i1,i2,j1,k1,stl,str
 real(dp) :: xm,ym,zm
 integer :: ic,i,j,k,jj,kk,n_str,ft_mod,ft_sym


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
                                   spec(ic),ebfp,jc,np,ndim,n_str,dt,xm,ym,zm)
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
