 !*****************************************************************************************************!
 !             Copyright 2008-2018 Pasquale Londrillo, Stefano Sinigardi, Andrea Sgattoni              !
 !                                                Alberto Marocchino                                   !
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

 module pwfa_output_addons

 use precision_def
 use pic_in
 use pic_out
 use pic_out_util
 use pic_dump
 use pic_evolve_in_time
 use read_input
 use pdf_moments
 use system_utilities

 implicit none
 contains



 !--- --- ---!



 subroutine bunch_output_struct(tdia,dtdia,tout,dtout)
 real(dp),intent(inout) :: tdia,dtdia,tout,dtout
 dump_t1=MPI_Wtime()

 !---scalar diagnostic output---!
 if (tnow>=tdia) then
  call diagnostic_integrated_output
  call diagnostic_integrated_background
  tdia=tdia+dtdia
 endif

 !--- 3D output ---!
 if(time_interval_dumps <= 0.) then !original output 3D+dumpdata
  if (tnow>=tout) then
   call create_timestep_folder(iout)
   call bunch_3D_output
   if(dump>0 .and. iter>0) call dump_data(iter,tnow)
   tout=tout+dtout
  endif
 else !--- output3D and dumpdata independently called
  if (tnow>=tout) then
   call create_timestep_folder(iout)
   call bunch_3D_output
   tout=tout+dtout
  endif
  !  if((unix_time_now - unix_time_last_dump) > time_interval_dumps) then
  !   if(pe0) write(6,*) '3D dump data being written'
  !    call dump_data(iter,tnow)
  !  endif
  !DUMP-At-TIME_INTERVAL_DUMPS
  IF(pe0 .and. dump_t1-dump_t0>time_interval_dumps) time2dump=1
  call vint_bcast(time2dump,1) !TOO-SLOW
  IF(time2dump(1)==1) then
   if(pe0) write(6,*) '3D dump data being written'
   call dump_data(iter,tnow)
   dump_t0=dump_t1
   time2dump=0
  endif
 endif
 end subroutine bunch_output_struct

 !--- --- ---!
 subroutine diagnostic_integrated_output
 integer :: i
 ienout=ienout+1
 !---gather data for lineout---!
 !---> lineout total density
 call collect_bunch_and_plasma_density(0,1)
 !---!
 do i=1,nsb
  call bunch_diagnostics(i)
 enddo
 end subroutine diagnostic_integrated_output

 !--- --- ---!
 subroutine bunch_3D_output
 integer :: i,ic

 if(nvout> 0)then
  do i=1,nvout
   if(L_force_singlefile_output) then
    call fields_out(ebf,tnow,i,i,jump)    ! second index to label field
   else
    call fields_out_new(ebf,tnow,i,i,jump)
   endif
  end do
  if(L_force_singlefile_output) then
   call fields_out(jc,tnow,1,0,jump)       !0 for Jx current
  else
   call fields_out_new(jc,tnow,1,0,jump) ! 0  to label Jx field
  endif
  do i=1,nvout
   if(L_force_singlefile_output) then
    call bfields_out(ebf_bunch,ebf1_bunch,tnow,i,jump)
   else
    call fields_out_new(ebf_bunch,tnow,i,i+6,jump)
   endif
  end do
 endif
 if(nden> 0)then
  ic=0
  call prl_bden_energy_interp(ic)
  call bden_energy_out(tnow,1,jump)
  do i=1,nsp
   call prl_den_energy_interp(i)
   ic=1
   call den_energy_out(tnow,i,ic,ic,jump)
   if(nden >1)then
     ic=2
     call den_energy_out(tnow,i,ic,ic,jump)
!      if (nden>2) then
!       call prl_momenta_interp(i)
!       do ic=1,curr_ndim
!        call den_energy_out(tnow,i,ic+2,ic,jump)
!       end do
!    endif
 endif
  enddo
 endif
 if(nbout> 0)then
  do i=1,nsb
   call part_bdata_out(tnow,i,pjump)
  end do
 endif
 if(Part)then
  if(npout> 0)then
   if(npout<=nsp)then
    call part_pdata_out(tnow,xp0_out,xp1_out,yp_out,npout,pjump)
   else
    do i=1,nsp
     call part_pdata_out(tnow,xp0_out,xp1_out,yp_out,i,pjump)
    end do
   endif
  endif
 endif
 iout=iout+1

 end subroutine bunch_3D_output

 !---------------------------
 end module pwfa_output_addons
