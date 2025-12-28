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

 module start_all

  use array_alloc
  use set_init_param
  use read_input
  use set_grid_param
  use ionize
  use pic_in
  use pic_dump
  use run_data_info
  use system_utilities

  implicit none

 contains

  !> Start subroutine. It reads the input file, initializes
  !> the variables and allocates the needed arrays before to start the
  !> simulation.
  subroutine Start

   integer :: iic, ncmp, n, i

   !enable loop to attach with gdb only if really needed
   !WARNING if enabled with no need, the program sleeps at start without doing anything!
   !To enable the flag, uncomment the corresponding line in CMakeLists.txt
#ifdef ENABLE_GDB_ATTACH
   call gdbattach
#endif
   call read_main_input

   call check_grid_size

   call set_initial_param

   !Read parameters from input.nml file

   call start_parallel(nd2, nsp, nsb)

   if (mpi_err > 0) then
    if (pe0) write (6, *) ' ERROR in mpi domain decomposition'
    call End_parallel
    stop
   end if
   if (mod(nx, nprocx) /= 0) then
    if (pe0) write (6, *) ' ==================================================='
    if (pe0) write (6, *) ' WARNING: Number of cells in the X direction is not '
    if (pe0) write (6, *) ' an integer multiple of the number of cores requested.'
    if (pe0) write (6, *) ' ==================================================='
   end if
   if (ndim > 1) then
    if (mod(ny, nprocy) /= 0) then
     if (pe0) write (6, *) ' ==================================================='
     if (pe0) write (6, *) ' WARNING: Number of cells in the Y direction is not '
     if (pe0) write (6, *) ' an integer multiple of the number of cores requested.'
     if (pe0) write (6, *) ' ==================================================='
    end if
   end if
   if (ndim > 2) then
    if (mod(nz, nprocz) /= 0) then
     if (pe0) write (6, *) ' ==================================================='
     if (pe0) write (6, *) ' WARNING: Number of cells in the Z direction is not '
     if (pe0) write (6, *) ' an integer multiple of the number of cores requested.'
     if (pe0) write (6, *) ' ==================================================='
    end if
   end if
   !sets parameters related to initial condition
   !=== Ascii art generated on http://patorjk.com/software/taag using the Star Wars font ===
   if (pe0) then
    write (6, *) '    ___   _          ______              _____  _____ '
    write (6, *) '   / _ \ | |         |  _  \            |____ ||  _  |'
    write (6, *) '  / /_\ \| |     __ _| | | |_   _ _ __      / /| |/| |'
    write (6, *) '  |  _  || |    / _  | | | | | | |  _ \     \ \|  /| |'
    write (6, *) '  | | | || |___| (_| | |/ /| |_| | | | |.___/ /\ |_/ /'
    write (6, *) '  \_| |_/\_____/\__,_|___/  \__, |_| |_|\____(_)\___/ '
    write (6, *) '                             __/ |                    '
    write (6, *) '                            |___/                     '
   end if
   if (pe0) then
    write (6, *) '======================================================'
    write (6, '(a33,i1,a1,i1,a1,i1,a17)') ' =               Code version    ', &
     major_version, '.', minor_version, '.', patch_version, '               ='
    write (6, *) '======================================================'
    call create_initial_folders
    call write_read_nml
   end if
   !call set_grid() to define global grid and grid
   !parameters
   call mpi_loc_grid(nx_loc, ny_loc, nz_loc, nprocx, nprocy, nprocz)
   call set_fyzxgrid(npe_yloc, npe_zloc, npe_xloc, sh_ix)
   if (stretch) call set_str_ind(npe_yloc, npe_zloc, ndim)
   call set_loc_grid_param
   call set_output_grid(jump, nprocx, nprocy, nprocz)

   if (inject_beam) then
    call set_ftgrid(stretch, nprocx, nprocy, nprocz)

    if (pe0) then
     if (stretch) then
      open (10, file='beam_overset_grid.dat')
      write (10, *) 'str to uniform grid', ny, n2ft, n2ft_loc, nz, n3ft, n3ft_loc
      iic = 0
      do i = 1, ny_loc
       n = yft_ind(i, iic)
       write (10, *) i, n, loc_yg(i, 1, iic), loc_yft(n, iic)
      end do
      do iic = 0, nprocy/2 - 1
       n = loc_yftgrid(iic)%ng
       write (10, *) 'pey', iic
       write (10, *) n, 4*n
       write (10, *) loc_ygrid(iic)%gmin, loc_ygrid(iic)%gmax
       write (10, *) loc_yftgrid(iic)%gmin, loc_yftgrid(iic + 3)%gmax
       write (10, *) loc_yft(1, iic), loc_yft(4*n, iic)
       write (10, *) '===================='
      end do
      iic = nprocy/2
      n = loc_yftgrid(iic)%ng
      write (10, *) 'pey', iic
      write (10, *) n, 4*n
      write (10, *) loc_ygrid(iic)%gmin, loc_ygrid(iic)%gmax
      write (10, *) loc_yftgrid(iic - 1)%gmin, loc_yftgrid(iic + 2)%gmax
      write (10, *) loc_yft(1, iic), loc_yft(4*n, iic)
      write (10, *) '===================='
      iic = nprocy/2 + 1
      n = loc_yftgrid(iic)%ng
      write (10, *) 'pey', iic
      write (10, *) n, 4*n
      write (10, *) loc_ygrid(iic)%gmin, loc_ygrid(iic)%gmax
      write (10, *) loc_yftgrid(iic - 2)%gmin, loc_yftgrid(iic + 1)%gmax
      write (10, *) loc_yft(1, iic), loc_yft(4*n, iic)
      write (10, *) '===================='
      do iic = nprocy/2 + 2, nprocy - 1
       n = loc_yftgrid(iic)%ng
       write (10, *) 'pey', iic
       write (10, *) n, 4*n
       write (10, *) loc_ygrid(iic)%gmin, loc_ygrid(iic)%gmax
       write (10, *) loc_yftgrid(iic - 3)%gmin, loc_yftgrid(iic)%gmax
       write (10, *) loc_yft(1, iic), loc_yft(4*n, iic)
       write (10, *) '===================='
      end do
      close (10)
     endif
    endif
   endif

   !Exit
   !loc_xgrid(nprocx),loc_ygrid(nprocy),loc_ygrid(nprocz) local grid data
   !local grid parameters and
   !coordinate struct  loc_xg,loc_yg,loc_zg
   !======================================
   call set_field_param !defines (nhx(nprocx), nhy(nprocy),nhz(nprocz) arrays of grid points
   mem_size = 0
   mem_psize = 0
   if (nvout > nfield) nvout = nfield
   ! for output data
   !allocates wdata() and gwdata()
   !=====================
   ncmp = nfield
   ! Allocates basic arrays, defines grid parameters, boundary index etc
   call v_alloc(nxp, nyp, nzp, nfield, nj_dim, ndim, ibeam, lpf_ord, &
                der_ord, envelope, Two_color, comoving, mem_size)
   if (hybrid) then
    call fluid_alloc(nxp, nyp, nzp, nfcomp, ndim, lpf_ord, mem_size)
    ncmp = max(ncmp, nfcomp)
   end if
   call mpi_buffer_alloc(nx_loc, ny_loc, nz_loc, ncmp)
   !local arrays and coefficients for space derivatives
   diag = .true.
   if (iene == 0) then
    diag = .false.
    iene = 1
   end if
   tpart = .false.
   inject_ind = -1
   !========================
   write_every = 100
   !============
   if (ionization) then
    do iic = 2, nsp_ionz
     call set_field_ioniz_wfunction(ion_min(iic - 1), atomic_number(iic - 1), &
                                    iic, ionz_lev, ionz_model, lp_max, dt)
    end do
    if (pe0) call ioniz_data(lp_max, ion_min, atomic_number, ionz_lev, &
                             ionz_model)
   end if
   !     Extended local grid
   select case (new_sim)
    !====== Fields and current arrays allocated on [1: N_loc+5]
   case (0)
    iout = id_new
    ienout = 0
    call init
    tstart = 0.0
    last_iter = 0
    tdia = tstart
    tout = tstart

    dt_loc = dt
    iter_max = 1
    dtout = (tmax - tstart)/nouts
    dtdia = (tmax - tstart)/iene
    if (tmax > 0.0) then
     iter_max = int(tmax/dt)
     dt_loc = tmax/float(iter_max)
    end if
    if (iter_max < 1000) write_every = nint(0.1*iter_max)
    if (pe0) write (*, *) 'write_every param =', write_every

   case (1)
    if (.not. l_first_output_on_restart) then
     iout = id_new
     ienout = 0
    else
     iout = id_new + 1
     ienout = 0
    end if
    call restart(last_iter, tstart)
    call MPI_BARRIER(comm, error)
    call set_fxgrid(npe_xloc, sh_ix)
    if (tmax > 0.0) then
     iter_max = int(tmax/dt)
     dt_loc = tmax/float(iter_max)
    end if
    if (iter_max < 1000) write_every = nint(0.1*iter_max)
    dtout = tmax/nouts
    dtdia = tmax/iene
    tmax = tmax + tstart
    if (.not. l_first_output_on_restart) then
     tdia = tstart + dtdia
     tout = tstart + dtout
    else
     tdia = tstart
     tout = tstart
    end if
    ! to count outputs in energy-data (iene+1 times)
   end select
  contains
   ! in general data (nouts+1 times)
   subroutine check_grid_size

    if (mod(nx, 2) /= 0) then
     write (6, *) ' Wrong x dimension'
     stop
    end if
    if (ny == 0) then
     write (6, *) ' Wrong y dimension'
     stop
    end if
    if (ny > 1) then
     if (mod(ny, 2) /= 0) then
      write (6, *) ' Wrong y dimension'
      stop
     end if
    end if
   end subroutine

  end subroutine
  ! reads from dump evolved data
  !=============================
 end module

