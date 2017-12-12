 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
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

 module pwfa_bunch_field_calculation

 use precision_def
 use pic_in
 use pic_evolve_in_time
 use particles
 use pic_rutil
 use fft_lib
 use grid_fields
 use pstruct_data
 use fstruct_data
 use all_param

 implicit none


 contains
 !--------------------------

 subroutine recalculate_bunch_fields
 integer :: i1,i2,i2b,j1,k1,nyp,nzp
 integer :: np,n_st,ic
 real(dp) :: gam2,gammamin,gammamax,gammadelta,beta_eff
 real(dp) :: xm,ym,zm
 real(dp) :: lxb,lyb
 real(dp), parameter :: twohundred_dp = 200.0


 !=============================
 ! The fields of moving bunches in vacuum
 ! E_x=-(DPhi/Dx)/gamma^2  , E_y=-DPhi/Dy   E_z=-DPhi/Dz
 ! Poisson eq.  D_xE_x+D_yE_y+D_zE_z=omp^2\rho (x-V_b*t_0,y,z)
 ! B_x=0   B_y=-V_b*E_z    B_z= V_b*E_y
 !=========================================
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i2b=i2
 !=======================
 xm=loc_xgrid(imodx)%gmin
 ym=loc_ygrid(imody)%gmin
 zm=loc_zgrid(imodz)%gmin
 !--- clean up old fields
 ebf_bunch=0.
 ebf1_bunch=0.
 ebf=0.
 !--- identify bunch gamma range
 gammamin=find_gamma_min_allbunches()
 gammamax=find_gamma_max_allbunches()
 gammadelta=determine_gammadelta(gammamin,gammamax)

 !!!do while(gammamin<=gammamax)
 beta_eff=sqrt(1.-1./(gammamin+gammadelta/2.)**2)
 beta_eff=sqrt(1.-1./(200.)**2)
 !---compute total bunch density
 jc_gammarange=0.0
 n_st=0
 do ic=1,nsb
  np=loc_nbpart(imody,imodz,imodx,ic)
  call set_grid_charge_gammarange(bunch(ic),gammamin,gammamax,ebfb,np,ndim,n_st,1,xm,ym,zm)
 end do

 if(prl)call fill_curr_yzxbdsdata(jc_gammarange,i1,i2b,j1,nyp,k1,nzp,1)

 !---
 !  BUNCH grid DENSITY already normalized
 !  index=3 for bunch current: a same bet0 assumed for all bunches
 !---
 ebf_bunch_gammarange(i1:i2b,j1:nyp,k1:nzp,1)=jc_gammarange(i1:i2b,j1:nyp,k1:nzp,1)
 ebf_bunch_gammarange(i1:i2b,j1:nyp,k1:nzp,2)=beta_eff*jc_gammarange(i1:i2b,j1:nyp,k1:nzp,1)

 !--- UNIFORM GRIDS  ASSUMED
 if(allocated(bpart))deallocate(bpart)
 lxb=x(nx)-x(1)
 lyb=y(ny)-y(1)
!  call FFT_Psolv(ebf_bunch_gammarange,(twohundred_dp)**2,lxb,lyb, &
!   nx,nx_loc,ny,ny_loc,nz,nz_loc,i1,i2b,j1,nyp,k1,nzp,1)
 call FFT_Psolv(ebf_bunch_gammarange,(twohundred_dp)**2,ompe,nx,nx_loc,ny,ny_loc,nz,nz_loc,&
                i1,i2b,j1,nyp,k1,nzp,2,1)
                !I have forced ft_mod=2 for cosine transform
                !I have forced ft_sym=1

 call fill_ebfield_yzxbdsdata(ebf_bunch_gammarange,i1,i2b,j1,nyp,k1,nzp,1,2,1,1)

 call initial_beam_potential_and_fields &
      (ebf_bunch_gammarange, &
      ebf1_bunch_gammarange, &
      ebf_gammarange,        &
      i1,i2b,j1,nyp,k1,nzp,dt,gam2,beta_eff)

 lp_end=xc_bunch(1)+sxb(1)
 !---sum up fields
 ebf_bunch=ebf_bunch+ebf_bunch_gammarange
 ebf1_bunch=ebf1_bunch+ebf1_bunch_gammarange
 ebf=ebf+ebf_gammarange
 ! call part_distribute(dmodel_id,lp_end)
 gammamin=gammamin+gammadelta
 !!!!!!!end do!endWhile

 end subroutine recalculate_bunch_fields


 !---gamma min for all bunches
 function find_gamma_min_allbunches()
 integer :: nbunch
 real(dp) :: find_gamma_min_allbunches

 find_gamma_min_allbunches = 1e10

 do nbunch=1,nsb
  find_gamma_min_allbunches=min(find_gamma_min_allbunches,find_gamma_min(nbunch))
 enddo
 end function find_gamma_min_allbunches

 !---gamma max for all bunches
 function find_gamma_max_allbunches()
 integer :: nbunch
 real(dp) :: find_gamma_max_allbunches

 find_gamma_max_allbunches = -1.

 do nbunch=1,nsb
  find_gamma_max_allbunches=max(find_gamma_max_allbunches,find_gamma_max(nbunch))
 enddo
 end function find_gamma_max_allbunches

 !---gamma min single bunch
 function find_gamma_min(bunch_number)
 integer, intent(in) :: bunch_number
 integer :: np_local
 real(dp) :: mu_gamma_local(1), mu_gamma(1), find_gamma_min

 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 !---
 mu_gamma_local = minval( sqrt( 1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
  bunch(bunch_number)%part(1:np_local,5)**2 + &
  bunch(bunch_number)%part(1:np_local,6)**2 ) )
 !---
 call allreduce_dpreal(-1,mu_gamma_local,mu_gamma,1)
 !---
 find_gamma_min = mu_gamma(1)
 !--- --- ---!
 end function find_gamma_min


 !---gamma max single bunch
 function find_gamma_max(bunch_number)
 integer, intent(in) :: bunch_number
 integer :: np_local
 real(dp) :: mu_gamma_local(1), mu_gamma(1), find_gamma_max

 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 !---
 mu_gamma_local = maxval( sqrt( 1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
  bunch(bunch_number)%part(1:np_local,5)**2 + &
  bunch(bunch_number)%part(1:np_local,6)**2 ) )
 !---
 call allreduce_dpreal(1,mu_gamma_local,mu_gamma,1)
 !---
 find_gamma_max = mu_gamma(1)
 !--- --- ---!
 end function find_gamma_max

 !---gamma max single bunch
 function determine_gammadelta(gmin,gmax)
 real(dp),intent(in) :: gmin,gmax
 real(dp) :: gdiff,determine_gammadelta

 gdiff=gmax-gmin

 if(gdiff<=20) then
  determine_gammadelta=(gmax+gmin)/4.
 else
  determine_gammadelta=20.
 endif
 !--- --- ---!
 end function determine_gammadelta



 !================================
 subroutine set_grid_charge_gammarange(sp_loc,gamma_min,gamma_max,part,np,ndm,n_st,ic,xmn,ymn,zmn)

 type(species),intent(in) :: sp_loc
 real(dp),intent(out) :: part(:,:)
 integer,intent(in) :: np,ndm,n_st,ic
 real(dp),intent(in) :: xmn,ymn,zmn,gamma_min,gamma_max
 real(dp) :: xx,sx,sx2,dvol,wgh
 real(dp) :: ax0(0:3),ay0(0:3),az0(0:3),xp(3),gamma_particle
 integer :: i,j,k,i1,j1,k1,i2,j2,k2,n,ch,spl
 real(sp) :: charge(2)
 equivalence(charge,wgh)
 !======================
 ax0(0:3)=0.0;ay0(0:3)=0.0
 az0(0:3)=0.0
 spl=2
 select case(ndm)
 case(1)
  j2=1
  do n=1,np
   gamma_particle=sqrt(1.0+sp_loc%part(n,4)**2+&
    sp_loc%part(n,5)**2+sp_loc%part(n,6)**2)
   if(gamma_particle.gt.gamma_min .and. gamma_particle.lt.gamma_max) then
    xp(1)=dx_inv*(sp_loc%part(n,1)-xmn)
    wgh=sp_loc%part(n,5)
    wgh=charge(1)*charge(2)
    xx=shx+xp(1)
    i=int(xx+0.5)
    sx=xx-real(i,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)
    ax0(0:2)=wgh*ax0(0:2)
    i=i-1
    do i1=0,2
     i2=i+i1
     jc_gammarange(i2,j2,1,ic)=jc_gammarange(i2,j2,1,ic)+ax0(i1)
    end do
   end if
  end do
 case(2)
  do n=1,np
   gamma_particle=sqrt(1.0+sp_loc%part(n,4)**2+&
    sp_loc%part(n,5)**2+sp_loc%part(n,6)**2)
   if(gamma_particle.gt.gamma_min .and. gamma_particle.lt.gamma_max) then
    part(1,n)=dx_inv*(sp_loc%part(n,1)-xmn)
    part(2,n)=sp_loc%part(n,2)
   end if
  end do
  if(n_st==0)then
   do n=1,np
    gamma_particle=sqrt(1.0+sp_loc%part(n,4)**2+&
     sp_loc%part(n,5)**2+sp_loc%part(n,6)**2)
    if(gamma_particle.gt.gamma_min .and. gamma_particle.lt.gamma_max) then
     xp(2)=part(2,n)
     part(2,n)=dy_inv*(xp(2)-ymn)
    end if
   end do
  else
   call map2dy_part_sind(np,n_st,2,ymn,part)
  endif
  ch=5
  do n=1,np
   gamma_particle=sqrt(1.0+sp_loc%part(n,4)**2+&
    sp_loc%part(n,5)**2+sp_loc%part(n,6)**2)
   if(gamma_particle.gt.gamma_min .and. gamma_particle.lt.gamma_max) then
    xp(1:2)=part(1:2,n)
    wgh=sp_loc%part(n,ch)
    wgh=charge(1)*charge(2)
    xx=shx+xp(1)
    i=int(xx+0.5)
    sx=xx-real(i,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)
    ax0(0:2)=wgh*ax0(0:2)
    i=i-1

    xx=shy+xp(2)
    j=int(xx+0.5)
    sx=xx-real(j,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)
    j=j-1
    do j1=0,2
     j2=j+j1
     do i1=0,2
      i2=i+i1
      dvol=ax0(i1)*ay0(j1)
      jc_gammarange(i2,j2,1,ic)=jc_gammarange(i2,j2,1,ic)+dvol
     end do
    end do
   end if
  end do
 case(3)
  ch=7
  do n=1,np
   gamma_particle=sqrt(1.0+sp_loc%part(n,4)**2+&
    sp_loc%part(n,5)**2+sp_loc%part(n,6)**2)
   if(gamma_particle.gt.gamma_min .and. gamma_particle.lt.gamma_max) then
    xp(1)=sp_loc%part(n,1)
    part(1,n)=dx_inv*(xp(1)-xmn)
    part(2:3,n)=sp_loc%part(n,2:3) ! current y-z positions
   end if
  end do
  if(n_st==0)then
   do n=1,np
    gamma_particle=sqrt(1.0+sp_loc%part(n,4)**2+&
     sp_loc%part(n,5)**2+sp_loc%part(n,6)**2)
    if(gamma_particle.gt.gamma_min .and. gamma_particle.lt.gamma_max) then
     xp(2:3)=part(2:3,n)
     part(2,n)=dy_inv*(xp(2)-ymn)
     part(3,n)=dz_inv*(xp(3)-zmn)
    end if
   end do
  else
   call map3d_part_sind(part,np,n_st,2,3,ymn,zmn)
  endif
  do n=1,np
   gamma_particle = sqrt(1.0+sp_loc%part(n,4)**2+&
    sp_loc%part(n,5)**2+sp_loc%part(n,6)**2)
   if(gamma_particle.gt.gamma_min .and. gamma_particle.lt.gamma_max) then
    xp(1:3)=part(1:3,n)
    wgh=sp_loc%part(n,ch)
    wgh=charge(1)*charge(2)
    xx=shx+xp(1)
    i=int(xx+0.5)
    sx=xx-real(i,dp)
    sx2=sx*sx
    ax0(1)=0.75-sx2
    ax0(2)=0.5*(0.25+sx2+sx)
    ax0(0)=1.-ax0(1)-ax0(2)
    ax0(0:2)=wgh*ax0(0:2)

    xx=shy+xp(2)
    j=int(xx+0.5)
    sx=xx-real(j,dp)
    sx2=sx*sx
    ay0(1)=0.75-sx2
    ay0(2)=0.5*(0.25+sx2+sx)
    ay0(0)=1.-ay0(1)-ay0(2)

    xx=shz+xp(3)
    k=int(xx+0.5)
    sx=xx-real(k,dp)
    sx2=sx*sx
    az0(1)=0.75-sx2
    az0(2)=0.5*(0.25+sx2+sx)
    az0(0)=1.-az0(1)-az0(2)
    !---------------
    i=i-1
    j=j-1
    k=k-1
    do k1=0,spl
     k2=k+k1
     do j1=0,spl
      j2=j+j1
      dvol=az0(k1)*ay0(j1)
      do i1=0,spl
       i2=i+i1
       jc_gammarange(i2,j2,k2,ic)=jc_gammarange(i2,j2,k2,ic)+ax0(i1)*dvol
      end do
     end do
    end do
   end if
  end do
  ! particles on[1:n1,1:n2,1:n3]===> data on [0:n1+1,0:n2+1,0:n3+1,1:4]
 end select
 !+++++++++++++++++++++++++++++++
 end subroutine set_grid_charge_gammarange


 !---------------------------
 end module pwfa_bunch_field_calculation
