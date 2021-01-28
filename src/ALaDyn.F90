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
 !! author: The ALaDyn Collaboration
 !! version: v{!./docs/VERSION.md!}
 !!
 !! Program: ALaDyn. Main file that executes the dynamics.
 program aladyn

  use start_all
  use diag_part_and_fields, only: En_data, Envar
  use run_data_info
  use pic_out_util
  use pic_out
  use init_beam_part_distrib, only: beam_inject
  use pic_evolve, only: Lp_run
  use env_evolve, only: Env_run
  use util, only: write_warning

  implicit none

  call Start(mp)

  !============= info related to initial data
  if (pe0) then
   write (6, *) 'START OF RUN'
   write (6, *) 'Running on ', npe, ' cpu'
   write (6, '(a32,3i4)') ' MPI decomposition along x-y-z: ', npe_xloc, &
    npe_yloc, npe_zloc
  end if
  !====To activate/disactivate diagnostics in envar() en_data routines=========
  diag = .true.
  initial_time = .false.
  if (iene == 0) then
   diag = .false.
   iene = 1
  end if
  call Part_numbers
  if (prl) then
  call Max_pmemory_check(spec, ebfp)
  end if
  if (pe0) then
   call initial_run_info(new_sim)
  end if
  !=============================
  call CPU_TIME(unix_time_now)
  unix_time_begin = unix_time_now
  unix_time_last_dump = unix_time_begin

  tnow = tstart
  if (tnow < dt_loc) initial_time = .true.

  ! iter=last_iter
  iter = 0

  select case (mod_ord)
  case (1)
   call Lp_cycle(mp)
  case (2)
   call Env_cycle(mp)
  end select

  !call timing
  call destroy_memory_pool(mp)
  call call_barrier
  call Final_run_info
  call End_parallel
  !--------------------------

 contains

  !--------------------------
  
  subroutine Lp_cycle(mempool)
   !! Collects the Laser-plasma dynamics evolved as a standard PIC.
   type(memory_pool_t), pointer, intent(in) :: mempool

   ! ==============================================
   ! BEAM is for now deactivated with new particles
   ! ==============================================
   ! if(inject_beam)then
   !  if(tnow <= t_inject.and.tnow+dt_loc >t_inject)then
   !   call beam_inject(spec(1), ebfp)
   !   call Part_numbers
   !   if (pe0) write (6, '(a24,e11.4)') ' Injected beam at time =', tnow
   !   !call den_energy_out( 0, nden, 1 ) !data on jc(1) for beam potential at injection
   !  end if
   ! end if
   call Data_out(mempool)
   do while (tnow<tmax)
   !=======================
    call lp_run( tnow, iter, mempool )
#if !defined(OLD_SPECIES)
    call track_out( spec, ebfp, tnow, iter, mempool )
#endif
    call timing(mempool)
    call Data_out(mempool)
#if !defined(OLD_SPECIES)
    tracking_written = .false.
#endif
    if (ier /= 0) then
     call error_message
     exit
    end if
    if (tnow + dt_loc >= tmax) dt_loc = tmax - tnow
    if (initial_time) initial_time = .false.
    call mempool%clean( maxval(loc_npart(imody, imodz, imodx, 1:nsp)) )
   end do
   if (dump > 0) call dump_data(iter, tnow, spec, ebfp)

  end subroutine

  !--------------------------

  subroutine Env_cycle(mempool)
   type(memory_pool_t), pointer, intent(in) :: mempool
   !! Collects the Laser-plasma dynamics evolved as a PIC in ponderomotive
   !! approximation.

   ! ==============================================
   ! BEAM is for now deactivated with new particles
   ! ==============================================
   ! if(inject_beam)then
   !  if(tnow <= t_inject.and.tnow+dt_loc >t_inject)then
   !   call beam_inject(spec(1), ebfp)
   !   call Part_numbers
   !   if (pe0) write (6, '(a24,e11.4)') ' Injected beam at time =', tnow
   !   !call den_energy_out( 0, nden, 1 ) !data on jc(1) for beam potential at injection
   !  end if
   ! end if
   call Data_out(mempool)
   !================
   do while (tnow < tmax)
   !=================================
    call env_run( tnow, iter, mempool )
#if !defined(OLD_SPECIES)
    call track_out( spec, ebfp, tnow, iter, mempool )
#endif
    call timing(mempool) !iter=iter+1  tnow=tnow+dt_loc
    call Data_out(mempool)
#if !defined(OLD_SPECIES)
    tracking_written = .false.
#endif
    if (ier /= 0) then
     call error_message
     exit
    end if
    if (tnow + dt_loc >= tmax) dt_loc = tmax - tnow
    if (initial_time) initial_time = .false.
    call mempool%clean( maxval(loc_npart(imody, imodz, imodx, 1:nsp)) )
   end do
   if (dump > 0) call dump_data(iter, tnow, spec, ebfp)

  end subroutine
  !======================
  subroutine data_out(mempool)
   integer :: i, iic, idata
   type(memory_pool_t), pointer, intent(in) :: mempool

   idata = iout
   if (diag) then
    if (tnow >= tdia) then
     ienout = ienout + 1
     call Envar( ienout, spec )
     tdia = tdia + dtdia
     if (pe0) then
      write (6, '(a10,i3,a10,e11.4)') ' rms data ', ienout, &
       ' at time =', tnow
      write(6,*)'=========================='
     end if
    end if
   end if

   if (tnow >= tout) then
    call create_timestep_folder(iout)

    tout = tout + dtout
    if (diag) then
     if (pe0) call en_data(ienout, iter, idata)
    end if
    !==================
    if (nvout > 0) then
     if (mod_ord == 2) then
      if (L_env_modulus) then
       i = 0
       call env_fields_out(env, i)
       if (Two_color) call env_fields_out(env1, -1) !EXIT |A|
      else
       if (Two_color) then
        do i = 1, 4
         call env_two_fields_out(env, env1, i)
        end do
       else
        do i = 1, 2
         call env_fields_out(env, i) !EXIT [Ar,Ai]
        end do
       end if
      end if
     end if
     do i = 1, nvout
      if (l_force_singlefile_output) then
       call fields_out(ebf, i, i) !i to label field name
      else
       call fields_out_new(ebf, i, i)
      end if
     end do
    end if
    if(ncurr >0)then
     do i = 1, curr_ndim
      call density_flux_out(jc,i)
     end do
    end if
    if (nden>0) then
     do i = 1, nsp
#if !defined(OLD_SPECIES)
      call prl_den_energy_interp(spec(i), i, nden, mempool)
#else
      call prl_den_energy_interp(spec(i), ebfp, i, nden)
#endif
      do iic = 1, min(2, nden)
       call den_energy_out(i, iic, iic)
      end do
     end do
    end if
    if (hybrid) then
     do i = 1, nfcomp
      call fluid_den_mom_out(up, i, nfcomp)
     end do
    end if
    !if (ionization) call part_ionz_out(spec, tnow)
    !if (gam_min > 1.) call part_high_gamma_out(spec, gam_min, tnow)
    if (npout > 0) then
     iic = npout
     if (iic <= nsp) then
      call part_pdata_out(spec, tnow, xp0_out, xp1_out, yp_out, iic, pjump, mempool)
     else
      do i = 1, nsp
       call part_pdata_out(spec, tnow, xp0_out, xp1_out, yp_out, i, pjump, mempool)
      end do
     end if
    end if
    !if(tnow>0.)write(6,*)'exit pdata',mype

    call CPU_TIME(unix_time_now)

    if (pe0) then
     write (6, '(a10,i6,a10,e11.4,a10,e11.4)') 'iter = ', iter, ' t = ', &
      tnow, ' dt = ', dt_loc
     write (6, *) ' END DATA WRITE'
     write (6, '(a16,f12.3)') ' Time elapsed = ', &
      unix_time_now - unix_time_begin
    end if
    if (dump>0 .and. time_interval_dumps < 0.0) then
     if (iter>0) call dump_data(iter, tnow, spec, ebfp)
    end if
    iout = iout + 1
   end if

   call CPU_TIME(unix_time_now)

   !if((unix_time_now - unix_time_last_dump) > time_interval_dumps .and. time_interval_dumps > 0.0) then
   ! call dump_data(iter, tnow, spec)
   !end if

  end subroutine data_out
  !--------------------------
 end program
