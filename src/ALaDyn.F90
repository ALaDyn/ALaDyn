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

  implicit none

  call Start

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
  if (prl) then
   call Part_numbers
   call Max_pmemory_check()
  end if
  if (pe0) then
   call initial_run_info(new_sim)
  endif
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
   call Lp_cycle
  case (2)
   call Env_cycle
  end select

  !call timing
  call MPI_BARRIER(comm, error)
  call Final_run_info
  call End_parallel
  !--------------------------

 contains

  !--------------------------

  subroutine Lp_cycle
   !! LP_CYCLE: collects the Laser-plasma dynamics evolved as a standard PIC.
   call Data_out
   do while (tnow < tmax)
    !=======================
    call lp_run(tnow, iter)

    call timing
    call Data_out

    if (ier /= 0) then
     call error_message
     exit
    end if
    if (tnow + dt_loc >= tmax) dt_loc = tmax - tnow
    if (initial_time) initial_time = .false.
   end do
   if (dump > 0) call dump_data(iter, tnow)
  end subroutine

  !--------------------------

  subroutine Env_cycle

   if (inject_beam) then
    if (tnow <= t_inject .and. tnow + dt_loc > t_inject) then
     call beam_inject
     call Part_numbers
     if (pe0) write (6, '(a24,e11.4)') ' Injected beam at time =', tnow
    end if
    !call den_energy_out( 0, nden, 1 ) !data on jc(1) for beam potential at injection
   endif
   call Data_out
   !================
   tk_ind = 0
   do while (tnow < tmax)
    !=================================
    call env_run(tnow, iter)

    call timing !iter=iter+1  tnow=tnow+dt_loc
    call Data_out

    if (ier /= 0) then
     call error_message
     exit
    end if
    if (tnow + dt_loc >= tmax) dt_loc = tmax - tnow
    if (initial_time) initial_time = .false.
   end do
   if (dump > 0) call dump_data(iter, tnow)
  end subroutine
  !======================
  subroutine data_out
   integer :: i, iic, idata

   idata = iout
   if (diag) then
    if (tnow >= tdia) then
     ienout = ienout + 1
     call Envar(ienout)
     tdia = tdia + dtdia
     if (pe0) then
      write (6, '(a10,i3,a10,e11.4)') ' rms data ', ienout, &
       ' at time =', tnow
      write (6, *) '=========================='
     endif
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
        do i = 1, 2
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
    if (nden > 0) then
     do i = 1, nsp
      call prl_den_energy_interp(i, nden)
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
    if (ionization) call part_ionz_out(tnow)
    if (gam_min > 1.) call part_high_gamma_out(gam_min, tnow)
    if (npout > 0) then
     iic = npout
     if (iic <= nsp) then
      call part_pdata_out(tnow, xp0_out, xp1_out, yp_out, iic, pjump)
     else
      do i = 1, nsp
       call part_pdata_out(tnow, xp0_out, xp1_out, yp_out, i, pjump)
      end do
     end if
    end if

    call CPU_TIME(unix_time_now)

    if (pe0) then
     write (6, '(a10,i6,a10,e11.4,a10,e11.4)') 'iter = ', iter, ' t = ', &
      tnow, ' dt = ', dt_loc
     write (6, *) ' END DATA WRITE'
     write (6, '(a16,f12.3)') ' Time elapsed = ', &
      unix_time_now - unix_time_begin
    end if
    if (dump > 0 .and. time_interval_dumps < 0.0) then
     if (iter > 0) call dump_data(iter, tnow)
    endif
    iout = iout + 1
   end if

   call CPU_TIME(unix_time_now)

   !if((unix_time_now - unix_time_last_dump) > time_interval_dumps .and. time_interval_dumps > 0.0) then
   ! call dump_data(iter,tnow)
   !endif

  end subroutine data_out
  !--------------------------
 end program
