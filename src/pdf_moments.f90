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

 module pdf_moments
 use precision_def
 use pic_rutil
 use grid_fields
 use fstruct_data
 use pstruct_data
 use fstruct_data
 use all_param

 !--- ---!
 logical, allocatable, public :: Pselection(:)
 !--- ---!


 contains

 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C calculates moments for a given pdf-distribution
 !C plus redefining bunch diagnostics
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


 SUBROUTINE bunch_diagnostics(bunch_number)
 integer, intent(in) :: bunch_number
 integer :: i,np_local
 real(dp) :: moments(2,6)

 !---!
 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 if(allocated(Pselection)) deallocate(Pselection)
 allocate(Pselection(np_local))
 Pselection=.TRUE.
 !---!

 !--- general diagnostic function
 CALL bunch_moments_diagnostic(bunch_number)
 CALL bunch_integrated_diagnostics(bunch_number,moments)
 CALL bunch_truncated_diagnostics(bunch_number,moments)
 DO i=1,4
  if(number_of_slices(i)>0) CALL bunch_sliced_diagnostics(bunch_number,moments,number_of_slices(i))
 ENDDO

 if(bunch_number.eq.1) call lineout_Ex(0.0,0.0) !lineout E-field total

 END SUBROUTINE bunch_diagnostics



 !--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---!
 !--- --- --- --- --- --- --- General functions --- --- --- --- --- --- -----!
 !--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---!
 real(dp) FUNCTION calculate_nth_central_moment_bunch(number_bunch,nth,component)
 integer, intent(in) :: nth, component, number_bunch
 integer :: np_loc,i
 real(dp) :: mu_mean_local(1)=0.0,mu_mean(1)=0.0,moment_local(1)=0.0,moment(1)=0.0, bch
 real(dp) :: tot_weights_local(1)=0.0,tot_weights(1)=0.0

 !---
 np_loc=loc_nbpart(imody,imodz,imodx,number_bunch)
 nb_tot_inv = 1.
 if (nb_tot(number_bunch)>0) nb_tot_inv = 1./real(nb_tot(number_bunch),dp)

  !--- mean calculation ---!
  mu_mean_local=0.0
  tot_weights_local=0.0
  if(component<=7) then
      DO i=1,np_loc
          if(Pselection(i)) then
              wgh_cmp=bunch(number_bunch)%part(i,7)
              mu_mean_local  = mu_mean_local+bunch(number_bunch)%part(i,component)*wgh
              tot_weights_local=tot_weights_local+wgh
          endif
      ENDDO
  endif
  if(component==8) then !--- for particle GAMMA ---!
      DO i=1,np_loc
          if(Pselection(i)) then
              wgh_cmp=bunch(number_bunch)%part(i,7)
              mu_mean_local  = mu_mean_local + &
              sqrt(1.0 + bunch(number_bunch)%part(i,4)**2+bunch(number_bunch)%part(i,5)**2+bunch(number_bunch)%part(i,6)**2)*wgh
              tot_weights_local=tot_weights_local+wgh
          endif
      ENDDO
  endif
 call allreduce_dpreal(0,mu_mean_local,mu_mean,1)
 call allreduce_dpreal(0,tot_weights_local,tot_weights,1)
 if(tot_weights(1) <= 0.0) tot_weights=1.0
 mu_mean        = mu_mean / tot_weights

 !--- moment calculation ---!
 moment_local=0.0
  if(component<=7) then
    DO i=1,np_loc
      if(Pselection(i)) then
        wgh_cmp=bunch(number_bunch)%part(i,7)
        moment_local   = moment_local+( bunch(number_bunch)%part(i,component) - mu_mean(1) )**nth * wgh
      endif
    ENDDO
  endif
  if(component==8) then !--- for particle GAMMA ---!
    DO i=1,np_loc
      if(Pselection(i)) then
        wgh_cmp=bunch(number_bunch)%part(i,7)
        moment_local   = moment_local+&
        ( sqrt(1.0+bunch(number_bunch)%part(i,4)**2+bunch(number_bunch)%part(i,5)**2+&
        bunch(number_bunch)%part(i,6)**2) &
        - mu_mean(1) )**nth * wgh
      endif
    ENDDO
  endif
 call allreduce_dpreal(0,moment_local,moment,1)
 moment         = moment / tot_weights
 !---
 calculate_nth_central_moment_bunch = moment(1)
 END FUNCTION calculate_nth_central_moment_bunch

 real(dp) FUNCTION calculate_nth_moment_bunch(number_bunch,nth,component)!,Pselection)
 integer, intent(in) :: nth, component, number_bunch
 integer :: np_loc,i
 real(dp) :: moment_local(1)=0.0,moment(1)=0.0
 real(dp) :: tot_weights_local(1)=0.0,tot_weights(1)=0.0

 !---
 np_loc=loc_nbpart(imody,imodz,imodx,number_bunch)

 !--- moment calculation ---!
 moment_local=0.0
 tot_weights_local=0.0
 if(component<=7) then
       DO i=1,np_loc
             if(Pselection(i)) then
                   wgh_cmp=bunch(number_bunch)%part(i,7)
                   moment_local   = moment_local+( bunch(number_bunch)%part(i,component) )**nth * wgh
                   tot_weights_local=tot_weights_local+wgh
             endif
       ENDDO
 endif
  if(component==8) then !for particle GAMMA ---!
        DO i=1,np_loc
              if(Pselection(i)) then
                    wgh_cmp=bunch(number_bunch)%part(i,7)
                    moment_local   = moment_local + &
                    sqrt( 1.0 + bunch(number_bunch)%part(i,4)**2 &
                    + bunch(number_bunch)%part(i,5)**2 + bunch(number_bunch)%part(i,6)**2 )**nth * wgh
                    tot_weights_local=tot_weights_local+wgh
              endif
        ENDDO
  endif
 call allreduce_dpreal(0,moment_local,moment,1)
 call allreduce_dpreal(0,tot_weights_local,tot_weights,1)
 if(tot_weights(1) <= 0.0) tot_weights=1.0
 moment         = moment / tot_weights

 !---
 calculate_nth_moment_bunch = moment(1)
 END FUNCTION calculate_nth_moment_bunch

 real(dp) FUNCTION calculate_Correlation_bunch(number_bunch,component1,component2)
 integer, intent(in) ::  component1,  component2, number_bunch
 integer :: np_loc,i
 real(dp) :: mu_mean_local_1(1),mu_mean_local_2(1),mu_mean_1(1),mu_mean_2(1)
 real(dp) :: correlation_local(1),correlation(1)
 real(dp) :: tot_weights_local(1)=0.0,tot_weights(1)=0.0
 real(dp) :: tot_weights2_local(1)=0.0,tot_weights2(1)=0.0

 !---
 np_loc=loc_nbpart(imody,imodz,imodx,number_bunch)

 !--- mean calculation ---!
 moment_local_1=0.0
 moment_local_2=0.0
 tot_weights_local=0.0
 DO i=1,np_loc
       if(Pselection(i)) then
             wgh_cmp=bunch(number_bunch)%part(i,7)
             mu_mean_local_1   = mu_mean_local_1+( bunch(number_bunch)%part(i,component1) ) * wgh
             mu_mean_local_2   = mu_mean_local_2+( bunch(number_bunch)%part(i,component2) ) * wgh
             tot_weights_local=tot_weights_local+wgh
         endif
 ENDDO
 call allreduce_dpreal(0,mu_mean_local_1,mu_mean_1,1)
 call allreduce_dpreal(0,mu_mean_local_2,mu_mean_2,1)
 call allreduce_dpreal(0,tot_weights_local,tot_weights,1)
 if(tot_weights(1) <= 0.0) tot_weights=1.0
 mu_mean_1         = mu_mean_1 / tot_weights
 mu_mean_2         = mu_mean_2 / tot_weights

!--- Correlation ---!
correlation_local=0.0
tot_weights2_local=0.0
DO i=1,np_loc
      if(Pselection(i)) then
            wgh_cmp=bunch(number_bunch)%part(i,7)
            correlation_local   = correlation_local + &
            ( bunch(number_bunch)%part(i,component1)-mu_mean_1(1) ) * &
            ( bunch(number_bunch)%part(i,component2)-mu_mean_2(1) ) * &
            wgh / tot_weights(1)
            tot_weights2_local = tot_weights2_local + wgh**2
      endif
ENDDO
call allreduce_dpreal(0,correlation_local,correlation,1)
call allreduce_dpreal(0,tot_weights2_local,tot_weights2,1)
if(tot_weights2(1) <= 0.0) tot_weights2=1.0
correlation         = correlation / (1.-tot_weights2(1))

 !---
 calculate_Correlation_bunch = correlation(1)
 END FUNCTION calculate_Correlation_bunch

 real(dp) FUNCTION calculate_Covariance_bunch(number_bunch,component1,component2)
 integer, intent(in) ::  component1,  component2, number_bunch
 integer :: np_loc,i
 real(dp) :: mu_mean_local_1(1),mu_mean_local_2(1),mu_mean_1(1),mu_mean_2(1)
 real(dp) :: covariance_local(1),covariance(1)
 real(dp) :: tot_weights_local(1)=0.0,tot_weights(1)=0.0
 real(dp) :: tot_weights2_local(1)=0.0,tot_weights2(1)=0.0

 !---
 call mpi_barrier(mpi_comm_world,error)
 np_loc=loc_nbpart(imody,imodz,imodx,number_bunch)

 !--- mean calculation ---!
 tot_weights=0.0
 correlation=0.0
 mu_mean_1=0.0
 mu_mean_2=0.0
 moment_local_1=0.0
 moment_local_2=0.0
 tot_weights_local=0.0
 DO i=1,np_loc
       if(Pselection(i)) then
             wgh_cmp=bunch(number_bunch)%part(i,7)
             mu_mean_local_1   = mu_mean_local_1+( bunch(number_bunch)%part(i,component1) ) * wgh
             mu_mean_local_2   = mu_mean_local_2+( bunch(number_bunch)%part(i,component2) ) * wgh
             tot_weights_local=tot_weights_local+wgh
         endif
 ENDDO
 call allreduce_dpreal(0,mu_mean_local_1,mu_mean_1,1)
 call allreduce_dpreal(0,mu_mean_local_2,mu_mean_2,1)
 call allreduce_dpreal(0,tot_weights_local,tot_weights,1)
 if(tot_weights(1) <= 0.0) tot_weights=1.0
 mu_mean_1         = mu_mean_1 / tot_weights
 mu_mean_2         = mu_mean_2 / tot_weights

 !--- Correlation ---!
 call mpi_barrier(mpi_comm_world,error)
 covariance_local=0.0
 covariance=0.0
 DO i=1,np_loc
      if(Pselection(i)) then
            wgh_cmp=bunch(number_bunch)%part(i,7)
            covariance_local   = covariance_local + &
            ( bunch(number_bunch)%part(i,component1)-mu_mean_1(1) ) * &
            ( bunch(number_bunch)%part(i,component2)-mu_mean_2(1) ) * &
            wgh
      endif
 ENDDO
 call allreduce_dpreal(0,covariance_local,covariance,1)
 covariance         = covariance / tot_weights(1)

 !---
 calculate_Covariance_bunch = covariance(1)
 END FUNCTION calculate_Covariance_bunch


!--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---!
!--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---!
!--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---!


 !--- --- ---!
 SUBROUTINE bunch_moments_diagnostic(bunch_number)
 integer, intent(in) :: bunch_number
 integer :: n_moment,np_local
 real(dp) :: moment(6,8) !firsts 8th moments for: 3-spatial 3-velocity
 real(dp) :: JB(6),JB_S(6),JB_K(6),vs,ek,vk,n_samples !jarque-brera normal-tests variables
 character(1) :: b2str

 !---!
 ! np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 ! if(allocated(Pselection)) deallocate(Pselection)
 ! allocate(Pselection(np_local))
 ! Pselection=.TRUE.
 !---!

 !--- 1-8th moments for -x- and -p-
 do n_moment=1,8
  moment(1,n_moment)=calculate_nth_central_moment_bunch(bunch_number,n_moment,1)
  moment(2,n_moment)=calculate_nth_central_moment_bunch(bunch_number,n_moment,2)
  moment(3,n_moment)=calculate_nth_central_moment_bunch(bunch_number,n_moment,3)
  moment(4,n_moment)=calculate_nth_central_moment_bunch(bunch_number,n_moment,4)
  moment(5,n_moment)=calculate_nth_central_moment_bunch(bunch_number,n_moment,5)
  moment(6,n_moment)=calculate_nth_central_moment_bunch(bunch_number,n_moment,6)
 enddo

 !--- Jarque-Brera (Urzula) test ---!
 JB_S  = moment(:,3)/moment(:,2)**(3./2.)
 JB_K  = moment(:,4)/moment(:,2)**2
 n_samples = real(nb_tot(bunch_number),dp)
 vs    = 6. * (n_samples-2.) / (n_samples+1.) / (n_samples+3.)
 ek    = 3. * (n_samples-1.) / (n_samples+1.)
 vk    = 24.* n_samples * (n_samples-2.) * (n_samples-3.) / (n_samples+1.)**2 / (n_samples+3.) / (n_samples+5.)
 JB    = JB_S**2/vs + (JB_K-ek)**2/vk

 !---
 if(pe0) then
  !--- moments output ---!
  write(b2str,'(I1.1)') bunch_number
  open(11,file='diagnostics/bunch_moments_'//b2str//'.dat',form='formatted', position='append')
  write(11,'(100e14.5)') tnow,moment(1,:),moment(2,:),moment(3,:),moment(4,:),moment(5,:),moment(6,:)
  close(11)
  !--- JB-U test output
  open(11,file='diagnostics/JB-U_test_'//b2str//'.dat',form='formatted', position='append')
  write(11,'(100e14.5)') tnow,JB(:)
  close(11)
 endif

 END SUBROUTINE

 SUBROUTINE bunch_integrated_diagnostics(bunch_number,moments)
 integer, intent(in) :: bunch_number
 integer :: np_local,np,i
 real(dp),intent(inout) :: moments(2,6)
 real(dp) :: mu_x, mu_y, mu_z !spatial meam
 real(dp) :: mu_px, mu_py, mu_pz !momenta mean
 real(dp) :: s_x, s_y, s_z !spatial variance
 real(dp) :: s_px, s_py, s_pz !momenta variance
 real(dp) :: mu_gamma !gamma mean
 real(dp) :: s_gamma!gamma variance
 real(dp) :: corr_y_py, corr_z_pz, corr_x_px !correlation transverse plane
 real(dp) :: emittance_y, emittance_z !emittance variables
 character(1) :: b2str

 !---!
 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 if(allocated(Pselection)) deallocate(Pselection)
 allocate(Pselection(np_local))
 Pselection=.TRUE.
 !---!

 !--- means ---!
 call mpi_barrier(mpi_comm_world,error)
 mu_x  = calculate_nth_moment_bunch(bunch_number,1,1)
 mu_y  = calculate_nth_moment_bunch(bunch_number,1,2)
 mu_z  = calculate_nth_moment_bunch(bunch_number,1,3)
 mu_px = calculate_nth_moment_bunch(bunch_number,1,4)
 mu_py = calculate_nth_moment_bunch(bunch_number,1,5)
 mu_pz = calculate_nth_moment_bunch(bunch_number,1,6)


 !--- variance calculation ---!
 call mpi_barrier(mpi_comm_world,error)
 s_x  = sqrt(calculate_nth_central_moment_bunch(bunch_number,2,1))
 s_y  = sqrt(calculate_nth_central_moment_bunch(bunch_number,2,2))
 s_z  = sqrt(calculate_nth_central_moment_bunch(bunch_number,2,3))
 s_px = sqrt(calculate_nth_central_moment_bunch(bunch_number,2,4))
 s_py = sqrt(calculate_nth_central_moment_bunch(bunch_number,2,5))
 s_pz = sqrt(calculate_nth_central_moment_bunch(bunch_number,2,6))

 moments(1,1)=mu_x
 moments(1,2)=mu_y
 moments(1,3)=mu_z
 moments(1,4)=mu_px
 moments(1,5)=mu_py
 moments(1,6)=mu_pz
 moments(2,1)=s_x
 moments(2,2)=s_y
 moments(2,3)=s_z
 moments(2,4)=s_px
 moments(2,5)=s_py
 moments(2,6)=s_pz

 !--- gamma ---!
 call mpi_barrier(mpi_comm_world,error)
 mu_gamma  = calculate_nth_moment_bunch(bunch_number,1,8)
 s_gamma=sqrt(calculate_nth_central_moment_bunch(bunch_number,2,8))

 !--- emittance calculation ---!
 call mpi_barrier(mpi_comm_world,error)
 corr_x_px  = calculate_Covariance_bunch(bunch_number,1,4)
 call mpi_barrier(mpi_comm_world,error)
 corr_y_py  = calculate_Covariance_bunch(bunch_number,2,5)
 call mpi_barrier(mpi_comm_world,error)
 corr_z_pz  = calculate_Covariance_bunch(bunch_number,3,6)

 !---
 emittance_y = sqrt( s_y**2 *s_py**2 - corr_y_py**2 )
 emittance_z = sqrt( s_z**2 *s_pz**2 - corr_z_pz**2 )


 !--- output ---!
 if(pe0) then
  write(b2str,'(I1.1)') bunch_number
  open(11,file='diagnostics/bunch_integrated_quantity_'//b2str//'.dat',form='formatted', position='append')
  !1  2   3   4   5    6    7     8      9      10     11      12      13     14    15    16     17       18       19       20
  !t,<X>,<Y>,<Z>,<Px>,<Py>,<Pz>,<rmsX>,<rmsY>,<rmsZ>,<rmsPx>,<rmsPy>,<rmsPz>,<Emy>,<Emz>,<Gam>,DGam/Gam,cov<xPx>,cov<yPy>,cov<zPz>
  write(11,'(100e14.5)') tnow,mu_x,mu_y,mu_z,mu_px,mu_py,mu_pz,s_x,s_y,s_z,s_px,s_py, &
   s_pz,emittance_y,emittance_z,mu_gamma,s_gamma,corr_x_px,corr_y_py,corr_z_pz
  close(11)
 endif

 !---lineout has been placed here---!
 !---only background
 call lineout_background_plasma(100*bunch_number+0, mu_x-4.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+1, mu_x-3.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+2, mu_x-2.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+3, mu_x-1.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+4, mu_x-0.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+5, mu_x+0.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+6, mu_x+1.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+7, mu_x+2.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+8, mu_x+3.5*s_x, mu_z)
 call lineout_background_plasma(100*bunch_number+9, mu_x+4.5*s_x, mu_z)
 !---background plus bunch density
 call lineout_bunch_and_background_plasma(100*bunch_number+0, mu_x-4.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+1, mu_x-3.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+2, mu_x-2.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+3, mu_x-1.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+4, mu_x-0.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+5, mu_x+0.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+6, mu_x+1.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+7, mu_x+2.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+8, mu_x+3.5*s_x, mu_z)
 call lineout_bunch_and_background_plasma(100*bunch_number+9, mu_x+4.5*s_x, mu_z)
 !---Ex-field on axis
 !call lineout_Ex(0.,0.)


 END SUBROUTINE bunch_integrated_diagnostics



 !--- --- ---!
 !-substituting the old function to calculate the integrated diagnostics
 SUBROUTINE bunch_integrated_diagnostics_old(bunch_number,moments)
 integer, intent(in) :: bunch_number
 integer :: np_local,np,i
 real(dp),intent(inout) :: moments(2,6)
 real(dp) :: mu_x_local(1), mu_y_local(1), mu_z_local(1) !spatial meam
 real(dp) :: mu_x(1), mu_y(1), mu_z(1) !spatial meam
 real(dp) :: mu_px_local(1), mu_py_local(1), mu_pz_local(1) !momenta mean
 real(dp) :: mu_px(1), mu_py(1), mu_pz(1) !momenta mean
 real(dp) :: s_x_local(1), s_y_local(1), s_z_local(1)!spatial variance
 real(dp) :: s_x(1), s_y(1), s_z(1) !spatial variance
 real(dp) :: s_px_local(1), s_py_local(1), s_pz_local(1) !momenta variance
 real(dp) :: s_px(1), s_py(1), s_pz(1) !momenta variance
 real(dp) :: mu_gamma_local(1), mu_gamma(1) !gamma mean
 real(dp) :: s_gamma_local(1), s_gamma(1) !gamma variance
 real(dp) :: corr_y_py_local(1), corr_z_pz_local(1), corr_x_px_local(1) !correlation transverse plane
 real(dp) :: corr_y_py(1), corr_z_pz(1), corr_x_px(1) !correlation transverse plane
 real(dp) :: emittance_y(1), emittance_z(1) !emittance variables
 real(dp) :: tot_weights(1),tot_weights_local(1)
 real(dp) :: np_inv
 character(1) :: b2str

 !---!
 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)
 if(allocated(Pselection)) deallocate(Pselection)
 allocate(Pselection(np_local))
 Pselection=.TRUE.
 !---!

 !--- mean calculation ---!
 mu_x_local=0.0
 mu_y_local=0.0
 mu_z_local=0.0
 mu_px_local=0.0
 mu_py_local=0.0
 mu_pz_local=0.0
 mu_x=0.0
 mu_y=0.0
 mu_z=0.0
 mu_px=0.0
 mu_py=0.0
 mu_pz=0.0
 tot_weights_local=0.0
 tot_weights=0.0
 DO i=1,np_local
   wgh_cmp=bunch(number_bunch)%part(i,7)
   mu_x_local   = mu_x_local+ bunch(bunch_number)%part(i,1)*wgh
   mu_y_local   = mu_y_local+ bunch(bunch_number)%part(i,2)*wgh
   mu_z_local   = mu_z_local+ bunch(bunch_number)%part(i,3)*wgh
   mu_px_local  = mu_px_local+ bunch(bunch_number)%part(i,4)*wgh
   mu_py_local  = mu_py_local+ bunch(bunch_number)%part(i,5)*wgh
   mu_pz_local  = mu_pz_local+ bunch(bunch_number)%part(i,6)*wgh
   tot_weights_local = tot_weights_local+wgh
 ENDDO
 !---
 call allreduce_dpreal(0,mu_x_local,mu_x,1)
 call allreduce_dpreal(0,mu_y_local,mu_y,1)
 call allreduce_dpreal(0,mu_z_local,mu_z,1)
 call allreduce_dpreal(0,mu_px_local,mu_px,1)
 call allreduce_dpreal(0,mu_py_local,mu_py,1)
 call allreduce_dpreal(0,mu_pz_local,mu_pz,1)
 call allreduce_dpreal(0,tot_weights_local,tot_weights,1)
 call allreduce_sint(0,np_local,np)
 !---
 mu_x  = mu_x / tot_weights
 mu_y  = mu_y / tot_weights
 mu_z  = mu_z / tot_weights
 mu_px = mu_px / tot_weights
 mu_py = mu_py / tot_weights
 mu_pz = mu_pz / tot_weights


 !--- variance calculation ---!
 s_x_local=0.0
 s_y_local=0.0
 s_z_local=0.0
 s_px_local=0.0
 s_py_local=0.0
 s_pz_local=0.0
 DO i=1,np_local
   wgh_cmp=bunch(number_bunch)%part(i,7)
   s_x_local   = s_x_local+ ( bunch(bunch_number)%part(i,1)-mu_x(1)  )**2*wgh
   s_y_local   = s_y_local+ ( bunch(bunch_number)%part(i,2)-mu_y(1)  )**2*wgh
   s_z_local   = s_z_local+ ( bunch(bunch_number)%part(i,3)-mu_z(1)  )**2*wgh
   s_px_local  = s_px_local+ ( bunch(bunch_number)%part(i,4)-mu_px(1) )**2*wgh
   s_py_local  = s_py_local+ ( bunch(bunch_number)%part(i,5)-mu_py(1) )**2*wgh
   s_pz_local  = s_pz_local+ ( bunch(bunch_number)%part(i,6)-mu_pz(1) )**2*wgh
 ENDDO
 !---
 call allreduce_dpreal(0,s_x_local,s_x,1)
 call allreduce_dpreal(0,s_y_local,s_y,1)
 call allreduce_dpreal(0,s_z_local,s_z,1)
 call allreduce_dpreal(0,s_px_local,s_px,1)
 call allreduce_dpreal(0,s_py_local,s_py,1)
 call allreduce_dpreal(0,s_pz_local,s_pz,1)
 !---
 s_x  = sqrt( s_x  / tot_weights )
 s_y  = sqrt( s_y  / tot_weights )
 s_z  = sqrt( s_z  / tot_weights )
 s_px = sqrt( s_px / tot_weights )
 s_py = sqrt( s_py / tot_weights )
 s_pz = sqrt( s_pz / tot_weights )

 moments(1,1)=mu_x(1)
 moments(1,2)=mu_y(1)
 moments(1,3)=mu_z(1)
 moments(1,4)=mu_px(1)
 moments(1,5)=mu_py(1)
 moments(1,6)=mu_pz(1)
 moments(2,1)=s_x(1)
 moments(2,2)=s_y(1)
 moments(2,3)=s_z(1)
 moments(2,4)=s_px(1)
 moments(2,5)=s_py(1)
 moments(2,6)=s_pz(1)

 !--- gamma diagnostic calculation ---!
 mu_gamma_local=0.0
 mu_gamma=0.0
 DO i=1,np_local
   wgh_cmp=bunch(number_bunch)%part(i,7)
   mu_gamma_local  = mu_gamma_local+ sqrt(   1.0 + bunch(bunch_number)%part(i,4)**2 + &
    bunch(bunch_number)%part(i,5)**2 + &
    bunch(bunch_number)%part(i,6)**2 ) * wgh
  ENDDO
 !---
 call allreduce_dpreal(0,mu_gamma_local,mu_gamma,1)
 !---
 mu_gamma  = mu_gamma / tot_weights
 !--- --- ---!
 s_gamma_local=0.0
 s_gamma=0.0
 DO i=1,np_local
   wgh_cmp=bunch(number_bunch)%part(i,7)
   s_gamma_local  = s_gamma_local+(sqrt (1.0 + bunch(bunch_number)%part(i,4)**2 + &
    bunch(bunch_number)%part(i,5)**2 + &
    bunch(bunch_number)%part(i,6)**2 )-mu_gamma)**2*wgh
  ENDDO
 !---
 call allreduce_dpreal(0,s_gamma_local,s_gamma,1)
 !---
 s_gamma  = sqrt(s_gamma / tot_weights)

 !--- emittance calculation ---!
 corr_x_px_local=0.0
 corr_x_px=0.0
 corr_y_py_local=0.0
 corr_y_py=0.0
 corr_z_pz_local=0.0
 corr_z_pz=0.0
 DO i=1,np_local
   wgh_cmp=bunch(number_bunch)%part(i,7)
   corr_x_px_local =corr_x_px_local+ (  (bunch(bunch_number)%part(i,1)-mu_x(1)) &
    * (bunch(bunch_number)%part(i,4)-mu_px(1)) )*wgh
   corr_y_py_local = corr_y_py_local+(  (bunch(bunch_number)%part(i,2)-mu_y(1)) &
    * (bunch(bunch_number)%part(i,5)-mu_py(1)) )*wgh
   corr_z_pz_local = corr_z_pz_local+(  (bunch(bunch_number)%part(i,3)-mu_z(1)) &
    * (bunch(bunch_number)%part(i,6)-mu_pz(1)) )*wgh
ENDDO
 !---
 call allreduce_dpreal(0,corr_x_px_local,corr_x_px,1)
 call allreduce_dpreal(0,corr_y_py_local,corr_y_py,1)
 call allreduce_dpreal(0,corr_z_pz_local,corr_z_pz,1)
 !---
 corr_x_px  = corr_x_px / tot_weights
 corr_y_py  = corr_y_py / tot_weights
 corr_z_pz  = corr_z_pz / tot_weights
 !---
 emittance_y = sqrt( s_y(1)**2 *s_py(1)**2 - corr_y_py(1)**2 )
 emittance_z = sqrt( s_z(1)**2 *s_pz(1)**2 - corr_z_pz(1)**2 )


 !--- output ---!
 if(pe0) then
  write(b2str,'(I1.1)') bunch_number
  open(11,file='diagnostics/bunch_integrated_quantity_'//b2str//'.dat',form='formatted', position='append')
  !1  2   3   4   5    6    7     8      9      10     11      12      13     14    15    16     17       18       19       20
  !t,<X>,<Y>,<Z>,<Px>,<Py>,<Pz>,<rmsX>,<rmsY>,<rmsZ>,<rmsPx>,<rmsPy>,<rmsPz>,<Emy>,<Emz>,<Gam>,DGam/Gam,cov<xPx>,cov<yPy>,cov<zPz>
  write(11,'(100e14.5)') tnow,mu_x,mu_y,mu_z,mu_px,mu_py,mu_pz,s_x,s_y,s_z,s_px,s_py, &
   s_pz,emittance_y,emittance_z,mu_gamma,s_gamma,corr_x_px,corr_y_py,corr_z_pz
  close(11)
 endif



 !---lineout has been placed here---!
 !---only background
 call lineout_background_plasma(100*bunch_number+0, mu_x(1)-4.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+1, mu_x(1)-3.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+2, mu_x(1)-2.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+3, mu_x(1)-1.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+4, mu_x(1)-0.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+5, mu_x(1)+0.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+6, mu_x(1)+1.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+7, mu_x(1)+2.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+8, mu_x(1)+3.5*s_x(1), mu_z(1))
 call lineout_background_plasma(100*bunch_number+9, mu_x(1)+4.5*s_x(1), mu_z(1))
 !---background plus bunch density
 call lineout_bunch_and_background_plasma(100*bunch_number+0, mu_x(1)-4.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+1, mu_x(1)-3.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+2, mu_x(1)-2.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+3, mu_x(1)-1.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+4, mu_x(1)-0.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+5, mu_x(1)+0.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+6, mu_x(1)+1.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+7, mu_x(1)+2.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+8, mu_x(1)+3.5*s_x(1), mu_z(1))
 call lineout_bunch_and_background_plasma(100*bunch_number+9, mu_x(1)+4.5*s_x(1), mu_z(1))
 !---Ex-field on axis
 !call lineout_Ex(0.,0.)

 END SUBROUTINE bunch_integrated_diagnostics_old
 !--- --- ---!


 SUBROUTINE bunch_truncated_diagnostics(bunch_number,moments)
 integer, intent(in) :: bunch_number
 integer :: np_local,np
 real(dp),intent(inout) :: moments(2,6)
 real(dp) :: mu_x_local(1), mu_y_local(1), mu_z_local(1) !spatial meam
 real(dp) :: mu_x(1), mu_y(1), mu_z(1) !spatial meam
 real(dp) :: mu_px_local(1), mu_py_local(1), mu_pz_local(1) !momenta mean
 real(dp) :: mu_px(1), mu_py(1), mu_pz(1) !momenta mean
 real(dp) :: s_x_local(1), s_y_local(1), s_z_local(1) !spatial variance
 real(dp) :: s_x(1), s_y(1), s_z(1) !spatial variance
 real(dp) :: s_px_local(1), s_py_local(1), s_pz_local(1) !momenta variance
 real(dp) :: s_px(1), s_py(1), s_pz(1) !momenta variance
 real(dp) :: mu_gamma_local(1), mu_gamma(1) !gamma mean
 real(dp) :: s_gamma_local(1), s_gamma(1) !gamma variance
 real(dp) :: corr_y_py_local(1), corr_z_pz_local(1), corr_x_px_local(1) !correlation transverse plane
 real(dp) :: corr_y_py(1), corr_z_pz(1), corr_x_px(1) !correlation transverse plane
 real(dp) :: emittance_y(1), emittance_z(1) !emittance variables
 real(dp) :: nSigmaCut
 real(dp) :: np_inv
 integer :: ip,nInside_loc
 logical, allocatable :: mask(:)
 character(1) :: b2str

 !---!
 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)


 !---- Mask Calculation ---------!
 allocate (mask(np_local))

 nSigmaCut = 5.0
 nInside_loc=0
 do ip=1,np_local
  mask(ip)=( abs(bunch(bunch_number)%part(ip,1)-moments(1,1))<nSigmaCut*moments(2,1) )&
   .and.(abs(bunch(bunch_number)%part(ip,2)-moments(1,2))<nSigmaCut*moments(2,2) )&
   .and.(abs(bunch(bunch_number)%part(ip,3)-moments(1,3))<nSigmaCut*moments(2,3) )
  if (mask(ip)) nInside_loc=nInside_loc+1
 enddo


 !--- mean calculation ---!
 !--- SUM(x, MASK=MOD(x, 2)==1)   odd elements, sum = 9 ---!

 mu_x_local   = sum( bunch(bunch_number)%part(1:np_local,1), MASK=mask(1:np_local) )
 mu_y_local   = sum( bunch(bunch_number)%part(1:np_local,2), MASK=mask(1:np_local) )
 mu_z_local   = sum( bunch(bunch_number)%part(1:np_local,3), MASK=mask(1:np_local) )
 mu_px_local  = sum( bunch(bunch_number)%part(1:np_local,4), MASK=mask(1:np_local) )
 mu_py_local  = sum( bunch(bunch_number)%part(1:np_local,5), MASK=mask(1:np_local) )
 mu_pz_local  = sum( bunch(bunch_number)%part(1:np_local,6), MASK=mask(1:np_local) )
 !---
 call allreduce_dpreal(0,mu_x_local,mu_x,1)
 call allreduce_dpreal(0,mu_y_local,mu_y,1)
 call allreduce_dpreal(0,mu_z_local,mu_z,1)
 call allreduce_dpreal(0,mu_px_local,mu_px,1)
 call allreduce_dpreal(0,mu_py_local,mu_py,1)
 call allreduce_dpreal(0,mu_pz_local,mu_pz,1)
 call allreduce_sint(0,nInside_loc,np)
 !---
 np_inv=1.
 if (np>0) np_inv=1./real(np,dp)

 mu_x  = mu_x * np_inv
 mu_y  = mu_y * np_inv
 mu_z  = mu_z * np_inv
 mu_px = mu_px * np_inv
 mu_py = mu_py * np_inv
 mu_pz = mu_pz * np_inv


 !--- variance calculation ---!
 s_x_local   = sum( ( bunch(bunch_number)%part(1:np_local,1)-mu_x(1)  )**2, MASK=mask(1:np_local) )
 s_y_local   = sum( ( bunch(bunch_number)%part(1:np_local,2)-mu_y(1)  )**2, MASK=mask(1:np_local) )
 s_z_local   = sum( ( bunch(bunch_number)%part(1:np_local,3)-mu_z(1)  )**2, MASK=mask(1:np_local) )
 s_px_local  = sum( ( bunch(bunch_number)%part(1:np_local,4)-mu_px(1) )**2, MASK=mask(1:np_local) )
 s_py_local  = sum( ( bunch(bunch_number)%part(1:np_local,5)-mu_py(1) )**2, MASK=mask(1:np_local) )
 s_pz_local  = sum( ( bunch(bunch_number)%part(1:np_local,6)-mu_pz(1) )**2, MASK=mask(1:np_local) )
 !---
 call allreduce_dpreal(0,s_x_local,s_x,1)
 call allreduce_dpreal(0,s_y_local,s_y,1)
 call allreduce_dpreal(0,s_z_local,s_z,1)
 call allreduce_dpreal(0,s_px_local,s_px,1)
 call allreduce_dpreal(0,s_py_local,s_py,1)
 call allreduce_dpreal(0,s_pz_local,s_pz,1)
 !---
 s_x  = sqrt( s_x  * np_inv )
 s_y  = sqrt( s_y  * np_inv )
 s_z  = sqrt( s_z  * np_inv )
 s_px = sqrt( s_px * np_inv )
 s_py = sqrt( s_py * np_inv )
 s_pz = sqrt( s_pz * np_inv )


 moments(1,1)=mu_x(1)
 moments(1,2)=mu_y(1)
 moments(1,3)=mu_z(1)
 moments(1,4)=mu_px(1)
 moments(1,5)=mu_py(1)
 moments(1,6)=mu_pz(1)
 moments(2,1)=s_x(1)
 moments(2,2)=s_y(1)
 moments(2,3)=s_z(1)
 moments(2,4)=s_px(1)
 moments(2,5)=s_py(1)
 moments(2,6)=s_pz(1)


 !--- gamma diagnostic calculation ---!
 mu_gamma_local  = sum(  sqrt(   1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
  bunch(bunch_number)%part(1:np_local,5)**2 + &
  bunch(bunch_number)%part(1:np_local,6)**2 ), MASK=mask(1:np_local)  )
 !---
 call allreduce_dpreal(0,mu_gamma_local,mu_gamma,1)
 !---
 mu_gamma  = mu_gamma * np_inv
 !--- --- ---!
 s_gamma_local  = sum(  (1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
  bunch(bunch_number)%part(1:np_local,5)**2 + &
  bunch(bunch_number)%part(1:np_local,6)**2 ), MASK=mask(1:np_local)  )
 !---
 call allreduce_dpreal(0,s_gamma_local,s_gamma,1)
 !---
 s_gamma  = s_gamma * np_inv
 if (mu_gamma(1) > 0. .or. mu_gamma(1) < 0.) s_gamma  = s_gamma / mu_gamma(1)**2
 if (s_gamma(1) > 1.) s_gamma  = sqrt(s_gamma-1.)



 !--- emittance calculation ---!
 corr_x_px_local = sum(  (bunch(bunch_number)%part(1:np_local,1)-mu_x(1)) &
  * (bunch(bunch_number)%part(1:np_local,4)-mu_px(1)), MASK=mask(1:np_local)  )
 corr_y_py_local = sum(  (bunch(bunch_number)%part(1:np_local,2)-mu_y(1)) &
  * (bunch(bunch_number)%part(1:np_local,5)-mu_py(1)), MASK=mask(1:np_local)  )
 corr_z_pz_local = sum(  (bunch(bunch_number)%part(1:np_local,3)-mu_z(1)) &
  * (bunch(bunch_number)%part(1:np_local,6)-mu_pz(1)), MASK=mask(1:np_local)  )
 !---
 call allreduce_dpreal(0,corr_x_px_local,corr_x_px,1)
 call allreduce_dpreal(0,corr_y_py_local,corr_y_py,1)
 call allreduce_dpreal(0,corr_z_pz_local,corr_z_pz,1)
 !---
 corr_x_px  = corr_x_px * np_inv
 corr_y_py  = corr_y_py * np_inv
 corr_z_pz  = corr_z_pz * np_inv
 !---
 emittance_y = sqrt( s_y(1)**2 *s_py(1)**2 - corr_y_py(1)**2 )
 emittance_z = sqrt( s_z(1)**2 *s_pz(1)**2 - corr_z_pz(1)**2 )


 !--- output ---!
 if(pe0) then
  write(b2str,'(I1.1)') bunch_number
  open(11,file='diagnostics/bunch_truncated_quantity_'//b2str//'.dat',form='formatted', position='append')
  !1  2   3   4   5    6    7     8      9      10     11      12      13     14    15    16     17       18       19       20
  !t,<X>,<Y>,<Z>,<Px>,<Py>,<Pz>,<rmsX>,<rmsY>,<rmsZ>,<rmsPx>,<rmsPy>,<rmsPz>,<Emy>,<Emz>,<Gam>,DGam/Gam,cov<xPx>,cov<yPy>,cov<zPz>
  write(11,'(100e14.5)') tnow,mu_x,mu_y,mu_z,mu_px,mu_py,mu_pz,s_x,s_y,s_z,s_px,s_py, &
   s_pz,emittance_y,emittance_z,mu_gamma,s_gamma,corr_x_px,corr_y_py,corr_z_pz
  close(11)
 endif


 !--- deallocate memory ----!

 deallocate (mask)

 END SUBROUTINE bunch_truncated_diagnostics

 !--- --- ---!

 SUBROUTINE bunch_sliced_diagnostics(bunch_number,moments,number_slices)
 integer, intent(in) :: bunch_number,number_slices
 integer :: np_local,np
 real(dp),intent(in) :: moments(2,6)
 real(dp) :: mu_x_local(1), mu_y_local(1), mu_z_local(1) !spatial meam
 real(dp) :: mu_x(1), mu_y(1), mu_z(1) !spatial meam
 real(dp) :: mu_px_local(1), mu_py_local(1), mu_pz_local(1) !momenta mean
 real(dp) :: mu_px(1), mu_py(1), mu_pz(1) !momenta mean
 real(dp) :: s_x_local(1), s_y_local(1), s_z_local(1) !spatial variance
 real(dp) :: s_x(1), s_y(1), s_z(1) !spatial variance
 real(dp) :: s_px_local(1), s_py_local(1), s_pz_local(1) !momenta variance
 real(dp) :: s_px(1), s_py(1), s_pz(1) !momenta variance
 real(dp) :: mu_gamma_local(1), mu_gamma(1) !gamma mean
 real(dp) :: s_gamma_local(1), s_gamma(1) !gamma variance
 real(dp) :: corr_y_py_local(1), corr_z_pz_local(1), corr_x_px_local(1) !correlation transverse plane
 real(dp) :: corr_y_py(1), corr_z_pz(1), corr_x_px(1) !correlation transverse plane
 real(dp) :: emittance_y(1), emittance_z(1) !emittance variables
 real(dp) :: nSigmaCut,delta_cut
 real(dp) :: np_inv
 integer :: ip,nInside_loc,islice
 logical, allocatable :: mask(:)
 character(1) :: b2str
 character(3) :: nslices2str,islice2str

 !---!
 np_local=loc_nbpart(imody,imodz,imodx,bunch_number)


 !---- Mask Calculation ---------!
 allocate (mask(np_local))


 !---  -5sigma   -4sigma   -3sigma   -2sigma  -1sigma     0sigma   +1sigma   +2sigma    +3sigma   +4sigma   +5sigma
 !---  | slice 0  |    1     |    2    |     3   |     4    |    5    |     6   |     7    |   8    |     9    |   ----!

 do islice=0,number_slices-1 ! change string format in output file for more than 9 slices

  nSigmaCut = 5.0
  delta_cut=2.0*nSigmaCut/number_slices
  nInside_loc=0
  do ip=1,np_local
   mask(ip)=( (bunch(bunch_number)%part(ip,1)-moments(1,1) )>( real(islice-number_slices/2)*delta_cut )*moments(2,1)  ) &
    .and.( (bunch(bunch_number)%part(ip,1)-moments(1,1) )   <( real(islice-number_slices/2+1)*delta_cut)*moments(2,1) )
   if (mask(ip)) nInside_loc=nInside_loc+1
  enddo


  !--- mean calculation ---!
  !--- SUM(x, MASK=MOD(x, 2)==1)   odd elements, sum = 9 ---!

  mu_x_local   = sum( bunch(bunch_number)%part(1:np_local,1), MASK=mask(1:np_local) )
  mu_y_local   = sum( bunch(bunch_number)%part(1:np_local,2), MASK=mask(1:np_local) )
  mu_z_local   = sum( bunch(bunch_number)%part(1:np_local,3), MASK=mask(1:np_local) )
  mu_px_local  = sum( bunch(bunch_number)%part(1:np_local,4), MASK=mask(1:np_local) )
  mu_py_local  = sum( bunch(bunch_number)%part(1:np_local,5), MASK=mask(1:np_local) )
  mu_pz_local  = sum( bunch(bunch_number)%part(1:np_local,6), MASK=mask(1:np_local) )
  !---
  call allreduce_dpreal(0,mu_x_local,mu_x,1)
  call allreduce_dpreal(0,mu_y_local,mu_y,1)
  call allreduce_dpreal(0,mu_z_local,mu_z,1)
  call allreduce_dpreal(0,mu_px_local,mu_px,1)
  call allreduce_dpreal(0,mu_py_local,mu_py,1)
  call allreduce_dpreal(0,mu_pz_local,mu_pz,1)
  call allreduce_sint(0,nInside_loc,np)
  !---
  np_inv=1.
  if (np>0) np_inv=1./real(np,dp)
  mu_x  = mu_x * np_inv
  mu_y  = mu_y * np_inv
  mu_z  = mu_z * np_inv
  mu_px = mu_px * np_inv
  mu_py = mu_py * np_inv
  mu_pz = mu_pz * np_inv


  !--- variance calculation ---!
  s_x_local   = sum( ( bunch(bunch_number)%part(1:np_local,1)-mu_x(1)  )**2, MASK=mask(1:np_local) )
  s_y_local   = sum( ( bunch(bunch_number)%part(1:np_local,2)-mu_y(1)  )**2, MASK=mask(1:np_local) )
  s_z_local   = sum( ( bunch(bunch_number)%part(1:np_local,3)-mu_z(1)  )**2, MASK=mask(1:np_local) )
  s_px_local  = sum( ( bunch(bunch_number)%part(1:np_local,4)-mu_px(1) )**2, MASK=mask(1:np_local) )
  s_py_local  = sum( ( bunch(bunch_number)%part(1:np_local,5)-mu_py(1) )**2, MASK=mask(1:np_local) )
  s_pz_local  = sum( ( bunch(bunch_number)%part(1:np_local,6)-mu_pz(1) )**2, MASK=mask(1:np_local) )
  !---
  call allreduce_dpreal(0,s_x_local,s_x,1)
  call allreduce_dpreal(0,s_y_local,s_y,1)
  call allreduce_dpreal(0,s_z_local,s_z,1)
  call allreduce_dpreal(0,s_px_local,s_px,1)
  call allreduce_dpreal(0,s_py_local,s_py,1)
  call allreduce_dpreal(0,s_pz_local,s_pz,1)
  !---
  s_x  = sqrt( s_x  * np_inv )
  s_y  = sqrt( s_y  * np_inv )
  s_z  = sqrt( s_z  * np_inv )
  s_px = sqrt( s_px * np_inv )
  s_py = sqrt( s_py * np_inv )
  s_pz = sqrt( s_pz * np_inv )



  !--- gamma diagnostic calculation ---!
  mu_gamma_local  = sum(  sqrt(   1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
   bunch(bunch_number)%part(1:np_local,5)**2 + &
   bunch(bunch_number)%part(1:np_local,6)**2 ), MASK=mask(1:np_local)  )
  !---
  call allreduce_dpreal(0,mu_gamma_local,mu_gamma,1)
  !---
  mu_gamma  = mu_gamma * np_inv
  !--- --- ---!
  s_gamma_local  = sum(  (1.0 + bunch(bunch_number)%part(1:np_local,4)**2 + &
   bunch(bunch_number)%part(1:np_local,5)**2 + &
   bunch(bunch_number)%part(1:np_local,6)**2 ), MASK=mask(1:np_local)  )
  !---
  call allreduce_dpreal(0,s_gamma_local,s_gamma,1)
  !---
  s_gamma  = s_gamma * np_inv
  if (mu_gamma(1) > 0. .or. mu_gamma(1) < 0.) s_gamma  = s_gamma / mu_gamma(1)**2
  if (s_gamma(1) > 1.) s_gamma  = sqrt(s_gamma-1.)



  !--- emittance calculation ---!
  corr_x_px_local = sum(  (bunch(bunch_number)%part(1:np_local,1)-mu_x(1)) &
   * (bunch(bunch_number)%part(1:np_local,4)-mu_px(1)), MASK=mask(1:np_local)  )
  corr_y_py_local = sum(  (bunch(bunch_number)%part(1:np_local,2)-mu_y(1)) &
   * (bunch(bunch_number)%part(1:np_local,5)-mu_py(1)), MASK=mask(1:np_local)  )
  corr_z_pz_local = sum(  (bunch(bunch_number)%part(1:np_local,3)-mu_z(1)) &
   * (bunch(bunch_number)%part(1:np_local,6)-mu_pz(1)), MASK=mask(1:np_local)  )

  !---
  call allreduce_dpreal(0,corr_x_px_local,corr_x_px,1)
  call allreduce_dpreal(0,corr_y_py_local,corr_y_py,1)
  call allreduce_dpreal(0,corr_z_pz_local,corr_z_pz,1)
  !---
  corr_x_px  = corr_x_px * np_inv
  corr_y_py  = corr_y_py * np_inv
  corr_z_pz  = corr_z_pz * np_inv
  !---
  emittance_y = sqrt( s_y(1)**2 *s_py(1)**2 - corr_y_py(1)**2 )
  emittance_z = sqrt( s_z(1)**2 *s_pz(1)**2 - corr_z_pz(1)**2 )


  !--- output ---!
  if(pe0) then
   write(b2str,'(I1.1)') bunch_number
   write(nslices2str,'(I3.3)') number_slices
   write(islice2str, '(I3.3)') islice
   open(11,file='diagnostics/bunch_sliced_quantity_'//b2str//'_'//nslices2str//'_'//islice2str//'.dat', &
    form='formatted', position='append')
   !1  2   3   4   5    6    7     8      9      10     11      12      13     14    15    16     17       18       19       20
   !t,<X>,<Y>,<Z>,<Px>,<Py>,<Pz>,<rmsX>,<rmsY>,<rmsZ>,<rmsPx>,<rmsPy>,<rmsPz>,<Emy>,<Emz>,<Gam>,DGam/Gam,cov<xPx>,cov<yPy>,cov<zPz>
   write(11,'(100e14.5)') tnow,mu_x,mu_y,mu_z,mu_px,mu_py,mu_pz,s_x,s_y,s_z,s_px,s_py, &
    s_pz,emittance_y,emittance_z,mu_gamma,s_gamma,corr_x_px,corr_y_py,corr_z_pz
   close(11)
  endif

 enddo ! end loop on slices

 !--- deallocate memory ----!
 deallocate (mask)

 END SUBROUTINE bunch_sliced_diagnostics


 !--- lineout only for the background gas ---!
 SUBROUTINE lineout_background_plasma(bunch_number,cut_x,cut_z)
 integer,intent(in) :: bunch_number
 real(dp),intent(in) :: cut_x, cut_z
 integer :: i,i1,i2,j1,k1,nyp,nzp
 integer :: i_section,k_section,iy1,iy2
 real(dp) :: x1,z1,x2,z2
 real(dp),allocatable :: rho_in(:),rho_out(:)
 character(3) :: b2str

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 x1=loc_xgrid(imodx)%gmin
 z1=loc_zgrid(imodz)%gmin
 x2=loc_xgrid(imodx)%gmax
 z2=loc_zgrid(imodz)%gmax

 allocate(rho_in(ny),rho_out(ny))
 rho_in=0.0
 rho_out=0.0

 if(cut_z> z1.and.cut_z<=z2)then
  if(cut_x> x1.and.cut_x<= x2)then
   i_section=i1+int(dx_inv*(cut_x-x1))
   k_section=k1+int(dz_inv*(cut_z-z1))
   iy1=1+imody*ny_loc
   iy2=iy1+ny_loc-1
   rho_in(iy1:iy2)=jc(i_section,j1:nyp,k_section,2)
  endif
 endif
 call allreduce_dpreal(SUMV,rho_in,rho_out,ny)

 !--- output ---!
 if(pe0) then
  write(b2str,'(I3.3)') bunch_number
  open(11,file='diagnostics/lineout_background_'//b2str//'.dat',form='formatted',position='append')
  write(11,'(1000e14.5)') (rho_out(i),i=1,ny)
  close(11)
 endif

 if( allocated(rho_in)  )  deallocate(rho_in)
 if( allocated(rho_out) )  deallocate(rho_out)
 END SUBROUTINE


 !--- lineout for: background density + bunch density ---!
 SUBROUTINE lineout_bunch_and_background_plasma(bunch_number,cut_x,cut_z)
 integer,intent(in) :: bunch_number
 real(dp),intent(in) :: cut_x, cut_z
 integer :: i,i1,i2,j1,k1,nyp,nzp
 integer :: i_section,k_section,iy1,iy2
 real(dp) :: x1,z1,x2,z2
 real(dp),allocatable :: rho_in(:),rho_out(:)
 character(3) :: b2str

 j1=loc_ygrid(imody)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 k1=loc_zgrid(imodz)%p_ind(1)
 nzp=loc_zgrid(imodz)%p_ind(2)
 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)
 x1=loc_xgrid(imodx)%gmin
 z1=loc_zgrid(imodz)%gmin
 x2=loc_xgrid(imodx)%gmax
 z2=loc_zgrid(imodz)%gmax

 allocate(rho_in(ny),rho_out(ny))
 rho_in=0.0
 rho_out=0.0

 if(cut_z> z1.and.cut_z<=z2)then
  if(cut_x> x1.and.cut_x<= x2)then
   i_section=i1+int(dx_inv*(cut_x-x1))
   k_section=k1+int(dz_inv*(cut_z-z1))
   iy1=1+imody*ny_loc
   iy2=iy1+ny_loc-1
   rho_in(iy1:iy2)=jc(i_section,j1:nyp,k_section,1)
  endif
 endif
 call allreduce_dpreal(SUMV,rho_in,rho_out,ny)

 !--- output ---!
 if(pe0) then
  write(b2str,'(I3.3)') bunch_number
  open(11,file='diagnostics/lineout_bunch_and_background_'//b2str//'.dat',form='formatted',position='append')
  write(11,'(1000e14.5)') (rho_out(i),i=1,ny)
  close(11)
 endif

 if( allocated(rho_in)  )  deallocate(rho_in)
 if( allocated(rho_out) )  deallocate(rho_out)
 END SUBROUTINE


 !--- lineout for: Ex (on axis) ---!
 SUBROUTINE lineout_Ex(cut_y,cut_z)
 real(dp),intent(in) :: cut_y, cut_z
 integer :: i,i1,i2,j1,k1,nyp,nzp
 integer :: j_section,k_section,ix1,iy1,ix2,iy2
 real(dp) :: x1,y1,z1,x2,y2,z2
 real(dp),allocatable :: rho_in(:),rho_out(:)

 j1=loc_ygrid(imody)%p_ind(1)
 k1=loc_zgrid(imodz)%p_ind(1)
 nyp=loc_ygrid(imody)%p_ind(2)
 nzp=loc_zgrid(imodz)%p_ind(2)

 i1=loc_xgrid(imodx)%p_ind(1)
 i2=loc_xgrid(imodx)%p_ind(2)

 x1=loc_xgrid(imodx)%gmin
 y1=loc_ygrid(imody)%gmin
 z1=loc_zgrid(imodz)%gmin
 x2=loc_xgrid(imodx)%gmax
 y2=loc_ygrid(imody)%gmax
 z2=loc_zgrid(imodz)%gmax

 allocate(rho_in(nx),rho_out(nx))
 rho_in=0.0
 rho_out=0.0

 if(cut_z> z1.and.cut_z<=z2)then
  if(cut_y> y1.and.cut_y<= y2)then
   j_section=j1+int(dy_inv*(cut_y-y1))
   k_section=k1+int(dz_inv*(cut_z-z1))
   iy1=1+imody*ny_loc
   iy2=iy1+ny_loc-1
   ix1=1+imodx*nx_loc
   ix2=ix1+nx_loc-1
   rho_in(ix1:ix2)=ebf_bunch(i1:i2,j_section,k_section,1) + &
    ebf(i1:i2,j_section,k_section,1)
  endif
 endif
 call allreduce_dpreal(SUMV,rho_in,rho_out,nx)

 !--- output ---!
 if(pe0) then
  open(11,file='diagnostics/lineout_Ex_onaxis.dat',form='formatted',position='append')
  write(11,'(1000e14.5)') (rho_out(i),i=1,nx)
  close(11)
 endif

 if( allocated(rho_in)  )  deallocate(rho_in)
 if( allocated(rho_out) )  deallocate(rho_out)
 END SUBROUTINE

 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 !C
 !C integrated diagnostic for the background particle with a mask-coding system
 !C
 !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 SUBROUTINE diagnostic_integrated_background
 integer :: np_local,np
 real(dp) :: mu_x_local(1), mu_y_local(1), mu_z_local(1) !spatial meam
 real(dp) :: mu_x(1), mu_y(1), mu_z(1) !spatial meam
 real(dp) :: mu_px_local(1), mu_py_local(1), mu_pz_local(1) !momenta mean
 real(dp) :: mu_px(1), mu_py(1), mu_pz(1) !momenta mean
 real(dp) :: s_x_local(1), s_y_local(1), s_z_local(1) !spatial variance
 real(dp) :: s_x(1), s_y(1), s_z(1) !spatial variance
 real(dp) :: s_px_local(1), s_py_local(1), s_pz_local(1) !momenta variance
 real(dp) :: s_px(1), s_py(1), s_pz(1) !momenta variance
 real(dp) :: mu_gamma_local(1), mu_gamma(1) !gamma mean
 real(dp) :: s_gamma_local(1), s_gamma(1) !gamma variance
 real(dp) :: corr_y_py_local(1), corr_z_pz_local(1), corr_x_px_local(1) !correlation transverse plane
 real(dp) :: corr_y_py(1), corr_z_pz(1), corr_x_px(1) !correlation transverse plane
 real(dp) :: emittance_y(1), emittance_z(1) !emittance variables
 real(dp) :: weights_local(1),weights(1),total_charge(1)
 real(dp) :: np_inv
 integer :: ip,nInside_loc
 logical, allocatable :: mask(:)

 !---!
 np_local=loc_npart(imody,imodz,imodx,1) !only electrons
 xmin_out=xp0_out
 xmax_out=xp1_out
 ymax_out=yp_out
 !---- Mask Calculation ---------!
 allocate (mask(np_local))

nInside_loc=0
 do ip=1,np_local
      mask(ip)=.true.
      !---spacial selection ---!
      if( abs(spec(1)%part(ip,2)) > ymax_out) mask(ip)=.false.
      if( abs(spec(1)%part(ip,3)) > ymax_out) mask(ip)=.false.
      if(spec(1)%part(ip,1) < xmin_out ) mask(ip)=.false.
      if(spec(1)%part(ip,1) > xmax_out) mask(ip)=.false.
      ! !--- weight selection ---!
      wgh_cmp=spec(1)%part(ip,7)
      if(wgh < weights_cut_min) mask(ip)=.false.
      if(wgh > weights_cut_max) mask(ip)=.false.
      ! !--- gamma selection ---!
      if( sqrt(1.0+SUM(spec(1)%part(ip,4:6)**2)) < gamma_cut_min) mask(ip)=.false.
      !--- particle counter ---!
      if (mask(ip)) nInside_loc=nInside_loc+1
 enddo

 !--- mean calculation ---!
 weights_local=0.0
 mu_x_local=0.0
 mu_y_local=0.0
 mu_z_local=0.0
 mu_px_local=0.0
 mu_py_local=0.0
 mu_pz_local=0.0
 do ip=1,np_local
        if(mask(ip)) then
         wgh_cmp=spec(1)%part(ip,7)
         mu_x_local   = mu_x_local+spec(1)%part(ip,1) * wgh
         mu_y_local   = mu_y_local+spec(1)%part(ip,2) * wgh
         mu_z_local   = mu_z_local+spec(1)%part(ip,3) * wgh
         mu_px_local  = mu_px_local+spec(1)%part(ip,4) * wgh
         mu_py_local  = mu_py_local+spec(1)%part(ip,5) * wgh
         mu_pz_local  = mu_pz_local+spec(1)%part(ip,6) * wgh
         weights_local = weights_local+wgh
       endif
 enddo
 !---
 call allreduce_dpreal(0,mu_x_local,mu_x,1)
 call allreduce_dpreal(0,mu_y_local,mu_y,1)
 call allreduce_dpreal(0,mu_z_local,mu_z,1)
 call allreduce_dpreal(0,mu_px_local,mu_px,1)
 call allreduce_dpreal(0,mu_py_local,mu_py,1)
 call allreduce_dpreal(0,mu_pz_local,mu_pz,1)
 call allreduce_dpreal(0,weights_local,weights,1)
 if(wgh<=0.0) weights=1.0
 !---
 mu_x  = mu_x / weights
 mu_y  = mu_y / weights
 mu_z  = mu_z / weights
 mu_px = mu_px / weights
 mu_py = mu_py / weights
 mu_pz = mu_pz / weights


 !--- variance calculation ---!
 s_x_local=0.0
 s_y_local=0.0
 s_z_local=0.0
 s_px_local=0.0
 s_py_local=0.0
 s_pz_local=0.0
 do ip=1,np_local
        if(mask(ip)) then
               wgh_cmp=spec(1)%part(ip,7)
               s_x_local   = s_x_local+( spec(1)%part(ip,1)-mu_x(1)  )**2 * wgh
               s_y_local   = s_y_local+( spec(1)%part(ip,2)-mu_y(1)  )**2 * wgh
               s_z_local   = s_z_local+( spec(1)%part(ip,3)-mu_z(1)  )**2 * wgh
               s_px_local  = s_px_local+( spec(1)%part(ip,4)-mu_px(1) )**2 * wgh
               s_py_local  = s_py_local+( spec(1)%part(ip,5)-mu_py(1) )**2 * wgh
               s_pz_local  = s_pz_local+( spec(1)%part(ip,6)-mu_pz(1) )**2 * wgh
        endif
  enddo
 !---
 call allreduce_dpreal(0,s_x_local,s_x,1)
 call allreduce_dpreal(0,s_y_local,s_y,1)
 call allreduce_dpreal(0,s_z_local,s_z,1)
 call allreduce_dpreal(0,s_px_local,s_px,1)
 call allreduce_dpreal(0,s_py_local,s_py,1)
 call allreduce_dpreal(0,s_pz_local,s_pz,1)
 !---
 s_x  = sqrt( s_x  / weights )
 s_y  = sqrt( s_y  / weights )
 s_z  = sqrt( s_z  / weights )
 s_px = sqrt( s_px / weights )
 s_py = sqrt( s_py / weights )
 s_pz = sqrt( s_pz / weights )

 !--- gamma diagnostic calculation ---!
 mu_gamma_local=0.0
 do ip=1,np_local
        if(mask(ip)) then
                wgh_cmp=spec(1)%part(ip,7)
                mu_gamma_local  = mu_gamma_local + sqrt(1.0 + spec(1)%part(ip,4)**2 + &
                                                  spec(1)%part(ip,5)**2 + &
                                                  spec(1)%part(ip,6)**2 )*wgh
        endif
  enddo
 !---
 call allreduce_dpreal(0,mu_gamma_local,mu_gamma,1)
 mu_gamma  = mu_gamma / weights
 !--- --- ---!
 s_gamma_local=0.0
 do ip=1,np_local
        if(mask(ip)) then
               wgh_cmp=spec(1)%part(ip,7)
               s_gamma_local  = s_gamma_local + (sqrt(1.0 + spec(1)%part(ip,4)**2 + &
                                             spec(1)%part(ip,5)**2 + &
                                             spec(1)%part(ip,6)**2 )-mu_gamma(1))**2 &
                                             *wgh
         endif
 enddo
 !---
 call allreduce_dpreal(0,s_gamma_local,s_gamma,1)
 s_gamma  = sqrt(s_gamma / weights)
 !---

 !--- emittance calculation ---!
 corr_x_px_local=0.0
 corr_y_py_local=0.0
 corr_z_pz_local=0.0
 do ip=1,np_local
        if(mask(ip)) then
            wgh_cmp=spec(1)%part(ip,7)
 corr_x_px_local = corr_x_px_local+ (spec(1)%part(ipl,1)-mu_x(1)) &
  * (spec(1)%part(ip,4)-mu_px(1)) * wgh
 corr_y_py_local =corr_y_py_local+ (spec(1)%part(ip,2)-mu_y(1)) &
  * (spec(1)%part(ip,5)-mu_py(1)) * wgh
 corr_z_pz_local = corr_z_pz_local+(spec(1)%part(ip,3)-mu_z(1)) &
  * (spec(1)%part(ip,6)-mu_pz(1)) * wgh
        endif
enddo
 !---
 call allreduce_dpreal(0,corr_x_px_local,corr_x_px,1)
 call allreduce_dpreal(0,corr_y_py_local,corr_y_py,1)
 call allreduce_dpreal(0,corr_z_pz_local,corr_z_pz,1)
 !---
 corr_x_px  = corr_x_px / weights
 corr_y_py  = corr_y_py / weights
 corr_z_pz  = corr_z_pz / weights
 !---
 emittance_y = sqrt( s_y(1)**2 *s_py(1)**2 - corr_y_py(1)**2 )
 emittance_z = sqrt( s_z(1)**2 *s_pz(1)**2 - corr_z_pz(1)**2 )
 !---
 total_charge = real(np_per_cell,sp) * weights !Need further checking


 !--- output ---!
 if(pe0) then
  open(11,file='diagnostics/diagnostic_integrated_background.dat',form='formatted', position='append')
  !1  2   3   4   5    6    7     8      9      10     11      12      13     14    15    16     17       18       19       20         21
  !t,<X>,<Y>,<Z>,<Px>,<Py>,<Pz>,<rmsX>,<rmsY>,<rmsZ>,<rmsPx>,<rmsPy>,<rmsPz>,<Emy>,<Emz>,<Gam>,DGam/Gam,cov<xPx>,cov<yPy>,cov<zPz>,total_charge
  write(11,'(100e14.5)') tnow,mu_x,mu_y,mu_z,mu_px,mu_py,mu_pz,s_x,s_y,s_z,s_px,s_py, &
   s_pz,emittance_y,emittance_z,mu_gamma,s_gamma,corr_x_px,corr_y_py,corr_z_pz,total_charge
  close(11)
 endif

 !--- deallocate memory ----!
 deallocate (mask)

 END SUBROUTINE diagnostic_integrated_background



 end module pdf_moments
