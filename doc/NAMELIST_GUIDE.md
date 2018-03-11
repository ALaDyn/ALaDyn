# ALaDyn `namelist` guide

Following here is a brief description of the new `input.nml` file required for the simulation parameters definition, through an example.

## GRID namelist block

```fortran
&GRID
 nx               = 7168,
 ny               = 3584,
 nz               = 1,
 ny_targ          = 3500,
 k0               = 100.0,
 yx_rat           = 2.0
/
```

+ `nx` is the number of grid points in the *x* direction
  + for PWFA simulations, it is required that this number contains the number of cpus of the *y* and *z* domains defined by `nprocx`, `nprocy` and `nprocz` in order to be sure that FFTs are working as expected
+ `ny` is the number of points in the *y* direction
  + in order to be sure that the simulation is working as expected, it is better to *always* use a number that contains `nprocx`, `nprocy` and `nprocz`
+ `nz` is the number of points in the *z* direction; for a 2D simulaton, use `nz = 1`
  + in order to be sure that the 3D simulation is working as expected, it is better to *always* use here a number that contains the number of cpu assigned to split the *z* domain (`mpi_ntot/pey`)
+ `ny_targ` is the transverse size of the target, expressed in number of grid cell filled with particles (if the simulation is a 3D one, the target is a square in the *xy* plane)
+ `k0` defines the resolution, being the number of points per μm along *x*, so that Δx = 1/k0 [μm]; please remember to set a value small enough to solve the skin depth
+ `yx_rat` is the ratio between the resolution along *x* and *y*: Δy = yx_rat/k0 [μm].
+ `zx_rat` is the ratio between the resolution along *x* and *z*: Δz = zx_rat/k0 [μm]; the resolution along *z* is the same as for *y* if this parameter is not defined.

With those parameters, the full box size (in μm) is: `Lx = nx / k0`, `Ly = yx_rat * ny / k0`, `Lz = zx_rat * nz / k0`

## SIMULATION namelist block

```fortran
&SIMULATION
 LPf_ord          = 2,
 der_ord          = 2,
 str_flag         = 0,
 iform            = 0,
 model_id         = 1,
 dmodel_id        = 3,
 ibx              = 0,
 iby              = 0,
 ibz              = 0,
 ibeam            = 1
/
```

+ `Lpf_ord` is  the integration scheme order. Lpf_ord=2 for standard leap-frog, Lpf_ord_ord=4 for RK4
+ `der_ord` is the order of the finite difference scheme. der_ord=2 or 3 for Lpf_ord=2, der_ord=4 for RK4
  + der_ord=3 assures optimized wave (Laser) propagation in under dense plasma.
+ `str_flag` has three possible different values:
  + `0` for uniform grid
  + `1` to enable stretching along transverse axes
  + `2` to enable stretching along all axes, but for *x* it is only on the right side of the box (need testing)
+ `iform` has two possible different values:
  + `0`:  Esirkepov's scheme for charge conservation (particle by particle) (to be preferred)
  + `1`: Esirkepov's scheme for charge conservation (inverted on grid along x) (not allowed for MPI decomposition along the x coordinate)
  + `2`: no charge preserving (better energy conservation assured)
+ `model_id` has five possible different values
  + `1` laser is p-polarized
  + `2` laser is s-polarized
  + `3` laser is circularly polarized
  + `4` laser is described by its envelope approximation model
  + `5` the driver is an electron beam. Its description must be given in a separate namelist block
  + `6` the driver is a proton bunch. Its description must be given in a separate namelist block
+ `dmodel_id` has five possible different values
  + `1` uniform - the simulation is done using  `nsp` species (electrons, Z_1,Z_2,Z_3) all distributed along the target x-profile
  + `2` empty model
  + `3` preplasma - the simulation is tested in this configuration: three `nsp=3` species (electrons, Z_1, Z_2), with a preplasma made of Z_1 + e, a bulk made of Z_1 + e and a contaminant layer made of Z_2 + e
  + `4` foam - the simulation is tested in this configuration: three `nsp=3` species (electrons, Z_1, Z_2), with a foam made of Z_2 + e, a bulk made of Z_1 + e and a contaminant layer made of Z_2 + e
  + `5` nanowires target with size parameters lpy(1),lpy(2) and species (e+Z_1) + a uniform bulk (e+Z_2)
  + `6` nanotubes target with size parameters lpy(1),lpy(2) and species (e+Z_1) + a uniform bulk (e+Z_2)
+ `ibx`, `iby`, `ibz` are the boundary conditions:
  + `0` open
  + `1` reflective
  + `2` periodic
+ `ibeam`:
  for PWFA (model_id #5/#6):
  + `0`
  + `1`
  + `2`

## TARGET_DESCRIPTION namelist block

```fortran
&TARGET_DESCRIPTION
 nsp              = 3,
 nsb              = 1,
 ion_min(1)       = 4,
 ion_max(1)       = 11,
 atomic_number(1) = 13,
 mass_number(1)   = 26.98,
 ion_min(2)       = 1,
 ion_max(2)       = 1,
 atomic_number(2) = 1,
 mass_number(2)   = 1.0,
 ion_min(3)       = 1,
 ion_max(3)       = 1,
 atomic_number(3) = 1,
 mass_number(3)   = 1.0,
 ionz_model       = 0,
 ionz_lev         = 0,
 t0_pl(1)         = 0.0003,
 t0_pl(2)         = 0.0,
 t0_pl(3)         = 0.0,
 t0_pl(4)         = 0.0,
 np_per_xc(1)     = 12,
 np_per_xc(2)     = 4,
 np_per_xc(3)     = 4,
 np_per_xc(4)     = 4,
 np_per_xc(5)     = 4,
 np_per_xc(6)     = 4,
 np_per_yc(1)     = 9,
 np_per_yc(2)     = 3,
 np_per_yc(3)     = 4,
 np_per_yc(4)     = 4,
 np_per_yc(5)     = 4,
 np_per_yc(6)     = 4,
 lpx(1)           = 0.0,
 lpx(2)           = 0.0,
 lpx(3)           = 2.0,
 lpx(4)           = 0.0,
 lpx(5)           = 0.08,
 lpx(6)           = 0.0,
 lpx(7)           = 0.01,
 lpy(1)           = 0.0,
 lpy(2)           = 0.0,
 n_over_nc        = 100.0,
 np1              = 1.0,
 np2              = 10.0
/
```

+ `nsp` is the number of species (be careful and coherent with `dmodel_id`)
+ `nsb`
+ `atomic_number(i)` are the atomic number (Z) that define each element species
+ `ion_min(i)` are the initial ionization status of the element species
+ `ion_max(i)` are the maximum ionization status of the element species
+ `mass_number(i)` are the mass number (A) that define the exact isotope of a given `atomic_number(i)`. Here following you can find the only elements known by ALaDyn

```fortran
Hydrogen  (atomic_number = 1)  - mass_number = 1.0
Helium    (atomic_number = 2)  - mass_number = 4.0
Lithium   (atomic_number = 3)  - mass_number = 6.0
Carbon    (atomic_number = 6)  - mass_number = 12.0
Nitrogen  (atomic_number = 7)  - mass_number = 14.0
Oxygen    (atomic_number = 8)  - mass_number = 16.0
Neon      (atomic_number = 10) - mass_number = 20.0
Aluminium (atomic_number = 13) - mass_number = 26.98
Silicon   (atomic_number = 14) - mass_number = 28.09
Argon     (atomic_number = 18) - mass_number = 39.948
Titanium  (atomic_number = 22) - mass_number = 47.8
Nickel    (atomic_number = 28) - mass_number = 58.7
Copper    (atomic_number = 29) - mass_number = 63.54
```

+ `t0_pl(i)`, with `i` from 1 to 4, are the initial temperatures in MeV for the different species
+ `np_per_xc(i)` **Please note that if `np_per_xc(1) =0`, it just means to ignore the plasma and simply do a laser propagation simulation.**
  + `dmodel_id=1` : i=1 indicates the number of electrons, i=2 the number of macroparticles of Z_1 species, i=3 the number of macroparticles of Z_2 species and i=4 the number of macroparticles of Z_3 species.
  + `dmodel_id=3,4` : i=1,2 are the number of electrons and ions per cell along x/y in the bulk, i=3,4 refer to the front layer and i=5,6 to the contaminants.
+ `np_per_yc(i)`: the same as `np_per_xc`, this describes the number of particles per cell along transverse directions (valid also for *z* for 3D simulations)
+ `lpx(1)` is the length [μm] of the upstream layer (foam or preplasma), having density `n1/nc`
+ `lpx(2)` is the length [μm] of the ramp (linear or exponential depending on the `mdl`) connecting the upstream layer with the central one (made with bulk particles)
+ `lpx(3)` is the length [μm] of the central layer (bulk), having density `n2/nc`
+ `lpx(4)` is the length [μm] of the ramp (linear), connecting the bulk with the contaminants (made with bulk particles)
+ `lpx(5)` is the length [μm] of the downstream layer (contaminants), having density `n3/nc`
+ `lpx(6)` is the angle *α* of incidence, between the laser axis and the target plane
+ `lpx(7)` is the offset [μm] between the end of the laser and the beginning of the target (if zero, the target starts right at the end of the laser pulse). The offset is calculated *before* laser rotation, so mind the transverse size if `lx(6) ≠ 0`, in order to avoid laser initialization *inside the target*.
+ `lpy(1)` defines the wire size [μm]
+ `lpy(2)` defines the distance [μm] between wires (interwire size)
+ `n_over_nc` is the density in the central layer (bulk)
  + *LWFA* case: density is in units of critical density
  + *PWFA* case: the density is in units of (a nominal value) nc=1e18 cm-3
+ `np1` is the density in the upstream layer (foam/preplasma)
+ `np2` is the density in the downstream layer (contaminants)
+ `ionz_lev`: if set to 0, we disable ionization; if 1, only one electron can be extracted per ion, if accessible, per timestep; if 2, it ionizes all the accessible levels in a single timestep
+ `ionz_model` describes the various ionization models:
  + 1 (pure ADK as in chen et al (2013), the best one for wake sims)
  + 2 (ADK averaged over cycles, as in chen et al (2013), `W_AC=<W_DC>`, best for envelope simulations)
  + 3 (`W_AC+BSI`, added barrier suppression ionization)
  + 4 (Minimum between ADK and BSI ionization values. Here the ADK value is computed averaging on `m`, the magnetic quantum number of the ionized electrons as in Alistair Lawrence-Douglas PhD thesis)

## LASER namelist block (**only for `ibeam=1`**)

```fortran
&LASER
 G_prof         = .true.,
 nb_laser       = 1,
 t0_lp          = 16.5,
 xc_lp          = 16.5,
 tau_fwhm       = 33.0,
 w0_y           = 6.2,
 a0             = 3.0
 lam0           = 0.8
 lp_delay       = 20.59
 lp_offset      = 0,
 t1_lp          = 200.0,
 tau1_fwhm      = 24.74,
 w1_y           = 3.5,
 a1             = 0.45,
 lam1           = 0.4,
/
```

+ `G_prof` logical flag: if true the pulse is temporally gaussian, if false it has a `cos^2` shape
+ `nb_laser` number of (identical) laser pulses injected
+ `xc_lp` is the position (in μm) along *x* of the central point of the laser envelope
+ `t0_lp` is the distance (in μm) after which the laser is focused; so the focus is at `xf = xc_lp + t0_lp`
+ `tau_fwhm` is the FWHM pulse duration (in fs)
+ `w0_y` is the transverse waist FWHM (in μm)
+ `a_0` is the laser adimensional parameter: a<sub>0</sub>=eA/(m<sub>e</sub> c<sup>2</sup>) of all the pulses injected
+ `lam0` is the laser wavelength (in μm) of all the pulses injected
+ `lp_delay`is the distance between the center of every injected laser pulse
+ `lp_offset` is the distance between the center of the last injected pulse and the center of another different pulse injected (if different from 0)
+ `t1_lp` same as `t0_lp`, but for the additional pulse injected with `lp_offset!=0`
+ `tau1_fwhm` same as `tau_fwhm`, but for the additional pulse injected with `lp_offset!=0`
+ `w1_y` same as `w0_y`, but for the additional pulse injected with `lp_offset!=0`
+ `a1` same as `a0`, but for the additional pulse injected with `lp_offset!=0`
+ `lam1` same as `lam0`, but for the additional pulse injected with `lp_offset!=0`

## MOVING_WINDOW namelist block

```fortran
&MOVING_WINDOW
 w_sh           = 20,
 wi_time        = 100.0,
 wf_time        = 150.0,
 w_speed        = 1.0
/
```

+ `w_sh` is the number of time steps after which the moving-window is called, repeatedly
+ `wi_time` is the absolute time at which the window starts moving. *nb: in order to block the MW from a certain simulation, the only tested path is to set a `wi_time` greater than the ending time of the simulation itself. `w_speed=0` should also work, but is not tested for now*
+ `wf_time` is the absolute time at which the window stops moving
+ `w_speed` is the speed over c of the moving window

## OUTPUT namelist block

```fortran
&OUTPUT
 nouts          = 10,
 iene           = 200,
 nvout          = 3,
 nden           = 1,
 npout          = 0,
 nbout          = 0,
 jump           = 1,
 pjump          = 1,
 xp0_out        = 0.0,
 xp1_out        = 100.0,
 yp_out         = 20.0,
 tmax           = 100.0,
 cfl            = 0.8,
 new_sim        = 0,
 id_new         = 0,
 dump           = 0,
 L_env_modulus  = .true.,
 P_tracking     = .true.,
/
```

+ `nouts` is the number of binary outputs during the relative time of the simulation
+ `iene` is the number of text outputs during the relative time of the simulation
+ `nvout` is the number of *fields* written. For a 2D P-polarized case, we have just 3 fields, *E<sub>x</sub>*, *E<sub>y</sub>* and *B<sub>z</sub>*; in all the other cases there are 6 fields with these IDs: 1=*E<sub>x</sub>*, 2=*E<sub>y</sub>*, 3=*E<sub>z</sub>*, 4=*B<sub>x</sub>*, 5=*B<sub>y</sub>*, 6=*B<sub>z</sub>*. At each `nouts` step, every field with `ID ≤ nvout` will be dumped.
+ `nden` can have three different values; every output is divided by species on different files:
  + `1`: writes *only* the particle density *n* on the grid
  + `2`: writes *also* the energy density *n ･ gamma* on the grid
  + `3`: writes *also* the currents *J* on the grid
+ `npout`
  + `1`: writes *only* the electron phase space
  + `2`: writes *only* the proton phase space
  + in general it writes *only* the phase space of the *n-th* species, with `n=npv`; if `n=npv>nps`, it writes the phase spaces of all the particle species
+ `nbout` (only for PWFA)
  + `1`: writes *only* the electron phase space
  + `2`: writes *only* the proton phase space
  + in general it writes *only* the phase space of the *n-th* species, with `n=npv`; if `n=npv>nps`, it writes the phase spaces of all the particle species
+ `jump`: jump on grid points; if `jump=2`, it means that each processor is going to write only one point every two. Mind that it's extremely important that `jump` is a divisor of the number of grid points *per processor*, otherwise the grid output will be corrupted
+ `pjump`: jump on particles; if `pjump=2`, it means that each processor is going to write the phase space of just one particle every two.
+ `xp0_out`, `xp1_out` and `yp_out` only particles contained inside the box defined by `xp0 < x < xp1`, `|y| < yp_out` will be printed in the output
+ `tmax` is the relative time (in μm, because it is multiplied by *c*) that the simulation is going to be evolved. Being relative, it's going to be added to the time eventually already reached before the restart. To obtain the time in fs, you have to divide it by 0.299792458 [speed of light in μm/fs]
+ `cfl` is the Courant–Friedrichs–Lewy parameter ( [Wikipedia CFL-parameter page](http://en.wikipedia.org/wiki/Courant%E2%80%93Friedrichs%E2%80%93Lewy_condition) )
+ `new_sim`: identifies if a simulation is new `new=0` (data is recreated from initial conditions) or if it is a restart `new=1` (dump are going to be read)
+ `id_new` identifies the starting number of the output files (if `new=0`) or the last one written in the previous step (if `new=1`)
+ `dump`
  + `1`: each processor will dump a binary file for every `nout` in order to enable restart
  + `0`: dumps are suppressed
+ `L_env_modulus` logical flag, only if `model_id=4`: if true the code generates the absolute value of the laser envelope amplitude, otherwise gives the real and imaginary part in two separate files
+ `P_tracking` logical flag, if true the particle tracking is enabled

## TRACKING namelist block

```fortran
&TRACKING
 nkjump           = 1,
 tkjump           = 4,
 txmin            = 55.,
 txmax            = 75.,
 tymin            = -80.,
 tymax            = 80.,
 tzmin            = -20.,
 tzmax            = 20.,
 t_in             = 0.,
 t_out            = 200.,
/
```

+ `nkjump` a tracked particle every `nkjump` is written in the output file
+ `tkjump` a snapshot of the tracked particles phase space is taken every `tkjump` timestep
+ `txmin` to select particles with initial longitudinal coordinate `x > txmin` to be tracked
+ `txmax` to select particles with initial longitudinal coordinate `x < txmax` to be tracked
+ `tymin` to select particles with initial transverse coordinate `y > tymin` to be tracked
+ `tymax` to select particles with initial transverse coordinate `y < tymax` to be tracked
+ `tzmin` to select particles with initial transverse coordinate `z > tzmin` to be tracked
+ `tzmax` to select particles with initial transverse coordinate `z < tzmax` to be tracked
+ `t_in` initial tracking time
+ `t_out` final tracking time

## MPIPARAMS namelist block

```fortran
&MPIPARAMS
 nprocx         = 1,
 nprocy         = 64,
 nprocz         = 1
/
```

+ `nprocx` defines the number of processor the simulation will be splitted along the `x` coordinate
+ `nprocy` defines the number of processor the simulation will be splitted along the `y` coordinate
+ `nprocz` defines the number of processor the simulation will be splitted along the `z` coordinate
  + mind that usually we do not split along `x` and we use the same number of processor along `y` and `z` (for 3D simulations) or we define `nprocz=1` explicitly for 2D simulations. No guarantees are given if you want to explore other configurations (that *should* work anyway).
