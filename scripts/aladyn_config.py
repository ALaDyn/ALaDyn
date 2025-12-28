#!/usr/bin/env python3
"""
ALaDyn Python Input Configuration Module

This module provides a Python-based interface for configuring ALaDyn simulations,
replacing the traditional Fortran namelist approach with a more intuitive and
flexible Python configuration system.

Features:
- Object-oriented configuration with validation
- Support for custom plasma density functions
- Type checking and parameter validation
- Easy-to-use Python syntax
- Compatible with existing Fortran namelist format
"""

from dataclasses import dataclass, field, asdict
from typing import List, Callable, Optional

# NumPy is optional - only needed for custom density functions and visualization
try:
    import numpy as np
    HAS_NUMPY = True
except ImportError:
    HAS_NUMPY = False
    np = None
    # Note: NumPy is only required for custom density functions
    # The basic configuration and namelist generation work without it


def _check_numpy_available():
    """Check if NumPy is available for custom density functions."""
    if not HAS_NUMPY:
        raise ImportError(
            "NumPy is required for custom density functions.\n"
            "The basic configuration system works without NumPy, but custom\n"
            "density functions need it for array operations.\n"
            "Install NumPy with: pip install numpy"
        )
    return True


@dataclass
class GridConfig:
    """Grid configuration parameters.
    
    Attributes:
        nx: Number of grid points in x direction
        ny: Number of grid points in y direction
        nz: Number of grid points in z direction (use 1 for 2D)
        ny_targ: Transverse size of target in grid cells
        k0: Resolution (points per μm along x)
        yx_rat: Ratio between y and x resolution
        zx_rat: Ratio between z and x resolution
    """
    nx: int
    ny: int
    nz: int = 1
    ny_targ: int = 0
    k0: float = 50.0
    yx_rat: float = 1.0
    zx_rat: float = 1.0
    
    def __post_init__(self):
        """Validate grid parameters."""
        if self.nx <= 0 or self.ny <= 0 or self.nz <= 0:
            raise ValueError("Grid dimensions must be positive")
        if self.k0 <= 0:
            raise ValueError("k0 must be positive")
        if self.yx_rat <= 0 or self.zx_rat <= 0:
            raise ValueError("Ratios must be positive")


@dataclass
class SimulationConfig:
    """Simulation configuration parameters.
    
    Attributes:
        LPf_ord: Integration scheme order (2 for leap-frog, 4 for RK4)
        der_ord: Order of finite difference scheme (2, 3, or 4)
        str_flag: Stretching flag (0=uniform, 1=light stretch, 2=strong stretch)
        iform: Charge conservation scheme (0=Esirkepov particle-by-particle, 1=grid, 2=none)
        model_id: Laser polarization (1=p-pol, 2=s-pol, 3=circular, 4=envelope)
        dmodel_id: Target model (1=uniform, 2=empty, 3=preplasma, 4=foam, 5=nanowires, 6=nanotubes)
        ibx: Boundary condition in x (0=open, 1=reflective, 2=periodic)
        iby: Boundary condition in y (0=open, 1=reflective, 2=periodic)
        ibz: Boundary condition in z (0=open, 1=reflective, 2=periodic)
        ibeam: Beam configuration (0, 1, or 2 for envelope-fluid LWFA)
    """
    LPf_ord: int = 2
    der_ord: int = 2
    str_flag: int = 0
    iform: int = 0
    model_id: int = 1
    dmodel_id: int = 1
    ibx: int = 0
    iby: int = 0
    ibz: int = 0
    ibeam: int = 0
    
    def __post_init__(self):
        """Validate simulation parameters."""
        if self.LPf_ord not in [2, 4]:
            raise ValueError("LPf_ord must be 2 or 4")
        if self.der_ord not in [2, 3, 4]:
            raise ValueError("der_ord must be 2, 3, or 4")
        if self.str_flag not in [0, 1, 2]:
            raise ValueError("str_flag must be 0, 1, or 2")
        if self.model_id not in [1, 2, 3, 4]:
            raise ValueError("model_id must be 1, 2, 3, or 4")


@dataclass
class TargetConfig:
    """Target description parameters.
    
    Attributes:
        nsp: Number of species
        nsb: Number of bunches
        ionz_lev: Ionization level
        ionz_model: Ionization model
        ion_min: Minimum ionization state per layer
        ion_max: Maximum ionization state per layer
        atomic_number: Atomic number per layer
        mass_number: Mass number per layer
        t0_pl: Temperature per layer (in units of mc^2)
        np_per_xc: Number of particles per cell in x
        np_per_yc: Number of particles per cell in y
        np_per_zc: Number of particles per cell in z
        concentration: Species concentration per layer
        lpx: Longitudinal plasma profile parameters [μm]
        lpy: Transverse plasma profile parameters [μm]
        n0_ref: Reference density (in units of critical density)
        np1: Density parameter 1
        np2: Density parameter 2
        r_c: Channel radius
        ppc: Particles per cell (alternative to np_per_xc/yc/zc)
        l_disable_rng_seed: Disable random number generator seed
    """
    nsp: int = 1
    nsb: int = 0
    ionz_lev: int = 0
    ionz_model: int = 4
    ion_min: List[int] = field(default_factory=lambda: [1, 1, 1])
    ion_max: List[int] = field(default_factory=lambda: [1, 1, 1])
    atomic_number: List[int] = field(default_factory=lambda: [1, 1, 1])
    mass_number: List[float] = field(default_factory=lambda: [1.0, 1.0, 1.0])
    t0_pl: List[float] = field(default_factory=lambda: [0.0, 0.0, 0.0, 0.0])
    np_per_xc: List[int] = field(default_factory=lambda: [2, 1, 1, 1, 1, 1])
    np_per_yc: List[int] = field(default_factory=lambda: [2, 1, 1, 1, 1, 1])
    np_per_zc: List[int] = field(default_factory=lambda: [1, 1, 1, 1, 1, 1])
    concentration: List[float] = field(default_factory=lambda: [1.0])
    lpx: List[float] = field(default_factory=lambda: [0., 50., 400., 100., 0., 0., 15.])
    lpy: List[float] = field(default_factory=lambda: [0.0, 0.0])
    n0_ref: float = 1.0
    np1: float = 0.0
    np2: float = 0.0
    r_c: float = 0.0
    ppc: List[int] = field(default_factory=lambda: [-1, -1, -1, -1, -1, -1])
    l_disable_rng_seed: bool = False


@dataclass
class LaserConfig:
    """Laser configuration parameters.
    
    Attributes:
        G_prof: Use Gaussian profile
        nb_laser: Number of lasers
        t0_lp: Laser start time
        xc_lp: Laser center position in x
        tau_fwhm: Pulse duration FWHM
        w0_y: Spot size in y
        a0: Normalized vector potential
        lam0: Wavelength [μm]
        y0_cent: Center position in y for each laser
        z0_cent: Center position in z for each laser
        incid_angle: Incidence angle
        Enable_ionization: Enable ionization for each laser
        lp_delay: Laser pulse delays
        lp_offset: Laser pulse offset
        t1_lp: Second laser start time
        tau1_fwhm: Second pulse duration FWHM
        w1_y: Second spot size in y
        a1: Second normalized vector potential
        lam1: Second wavelength [μm]
        y1_cent: Second laser center in y
        z1_cent: Second laser center in z
        Symmetrization_pulse: Enable pulse symmetrization
        a_symm_rat: Symmetrization ratio
    """
    G_prof: bool = True
    nb_laser: int = 1
    t0_lp: float = 0.0
    xc_lp: float = 50.0
    tau_fwhm: float = 40.0
    w0_y: float = 20.0
    a0: float = 2.0
    lam0: float = 0.8
    y0_cent: List[float] = field(default_factory=lambda: [0.0])
    z0_cent: List[float] = field(default_factory=lambda: [0.0])
    incid_angle: float = 0.0
    Enable_ionization: List[bool] = field(default_factory=lambda: [True, True])
    lp_delay: List[float] = field(default_factory=lambda: [0.0])
    lp_offset: float = 0.0
    t1_lp: float = 0.0
    tau1_fwhm: float = 0.0
    w1_y: float = 0.0
    a1: float = 0.0
    lam1: float = 0.0
    y1_cent: float = 0.0
    z1_cent: float = 0.0
    Symmetrization_pulse: bool = False
    a_symm_rat: float = 0.0
    
    def __post_init__(self):
        """Validate laser parameters."""
        if self.a0 < 0:
            raise ValueError("a0 must be non-negative")
        if self.lam0 <= 0:
            raise ValueError("lam0 must be positive")


@dataclass
class BeamConfig:
    """Beam injection configuration parameters.
    
    Attributes:
        nb_1: Number of beam particles (in units of 100000)
        xc_1: Beam center position in x
        gam_1: Beam gamma factor
        sxb_1: Beam size in x
        syb_1: Beam size in y
        epsy_1: Emittance in y
        epsz_1: Emittance in z
        dg_1: Energy spread
        charge_1: Beam charge
        ap1_twiss: Twiss alpha parameter
        bt1_twiss: Twiss beta parameter
        t_inject: Injection time
    """
    nb_1: float = 0.0
    xc_1: float = 0.0
    gam_1: float = 100.0
    sxb_1: float = 1.0
    syb_1: float = 1.0
    epsy_1: float = 0.0
    epsz_1: float = 0.0
    dg_1: float = 0.0
    charge_1: float = 1.0
    ap1_twiss: float = 0.0
    bt1_twiss: float = 1.0
    t_inject: float = 0.0


@dataclass
class MovingWindowConfig:
    """Moving window configuration parameters.
    
    Attributes:
        w_sh: Window shift (number of cells)
        wi_time: Initial time for window movement
        wf_time: Final time for window movement
        w_speed: Window speed (in units of c)
    """
    w_sh: int = 10
    wi_time: float = 0.0
    wf_time: float = 1000.0
    w_speed: float = 1.0


@dataclass
class OutputConfig:
    """Output configuration parameters.
    
    Attributes:
        nouts: Number of output types
        iene: Energy output interval
        nvout: Vector field output interval
        nden: Density output interval
        npout: Particle output interval
        nbout: Bunch output interval
        jump: Time step jump for output
        pjump: Particle jump for output
        gam_min: Minimum gamma for particle output
        xp0_out: Output window start in x
        xp1_out: Output window end in x
        yp_out: Output window size in y
        tmax: Maximum simulation time
        cfl: CFL condition
        new_sim: New simulation flag
        id_new: New simulation ID
        dump: Dump flag
        L_env_modulus: Use envelope modulus
        time_interval_dumps: Time interval for dumps
        L_force_singlefile_output: Force single file output
        L_first_output_on_restart: First output on restart
        L_print_j_on_grid: Print current density on grid
    """
    nouts: int = 4
    iene: int = 40
    nvout: int = 2
    nden: int = 1
    npout: int = 1
    nbout: int = 0
    jump: int = 1
    pjump: int = 1
    gam_min: float = 1.0
    xp0_out: float = 0.0
    xp1_out: float = 60.0
    yp_out: float = 50.0
    tmax: float = 45.0
    cfl: float = 0.8
    new_sim: int = 0
    id_new: int = 0
    dump: int = 0
    L_env_modulus: bool = True
    time_interval_dumps: float = -1.0
    L_force_singlefile_output: bool = True
    L_first_output_on_restart: bool = False
    L_print_j_on_grid: bool = True


@dataclass
class TrackingConfig:
    """Particle tracking configuration parameters.
    
    Attributes:
        tkjump: Tracking time jump
        nkjump: Tracking particle jump
        txmin: Tracking window minimum x
        txmax: Tracking window maximum x
        tymin: Tracking window minimum y
        tymax: Tracking window maximum y
        tzmin: Tracking window minimum z
        tzmax: Tracking window maximum z
        t_in: Tracking start time
        t_out: Tracking end time
        p_tracking: Enable tracking
    """
    tkjump: int = 1
    nkjump: int = 1
    txmin: float = 0.0
    txmax: float = 0.0
    tymin: float = 0.0
    tymax: float = 0.0
    tzmin: float = 0.0
    tzmax: float = 0.0
    t_in: float = 0.0
    t_out: float = 0.0
    p_tracking: bool = False


@dataclass
class MPIConfig:
    """MPI configuration parameters.
    
    Attributes:
        nprocx: Number of processors in x
        nprocy: Number of processors in y
        nprocz: Number of processors in z
    """
    nprocx: int = 1
    nprocy: int = 1
    nprocz: int = 1


@dataclass
class ALaDynConfig:
    """Complete ALaDyn simulation configuration.
    
    This class combines all configuration sections and provides methods
    for validation and conversion to Fortran namelist format.
    """
    grid: GridConfig
    simulation: SimulationConfig
    target: TargetConfig
    laser: LaserConfig
    moving_window: MovingWindowConfig
    output: OutputConfig
    mpi: MPIConfig
    beam: Optional[BeamConfig] = None
    tracking: Optional[TrackingConfig] = None
    
    # Optional: custom plasma density function (requires NumPy)
    custom_density_function: Optional[Callable] = None
    
    def validate(self) -> bool:
        """Validate the entire configuration.
        
        Returns:
            True if configuration is valid
            
        Raises:
            ValueError: If configuration is invalid
        """
        # Grid validation
        if self.grid.nx % self.mpi.nprocx != 0:
            print(f"Warning: nx ({self.grid.nx}) is not divisible by nprocx ({self.mpi.nprocx})")
        
        if self.grid.ny % self.mpi.nprocy != 0:
            print(f"Warning: ny ({self.grid.ny}) is not divisible by nprocy ({self.mpi.nprocy})")
        
        if self.grid.nz > 1 and self.grid.nz % self.mpi.nprocz != 0:
            print(f"Warning: nz ({self.grid.nz}) is not divisible by nprocz ({self.mpi.nprocz})")
        
        # Check CFL condition
        if self.output.cfl > 1.0:
            print(f"Warning: CFL ({self.output.cfl}) > 1.0 may cause instability")
        
        return True
    
    def summary(self) -> str:
        """Generate a human-readable summary of the configuration.
        
        Returns:
            Formatted string with configuration summary
        """
        dx = 1.0 / self.grid.k0
        dy = self.grid.yx_rat / self.grid.k0
        dz = self.grid.zx_rat / self.grid.k0
        Lx = self.grid.nx * dx
        Ly = self.grid.ny * dy
        Lz = self.grid.nz * dz
        
        summary = f"""
ALaDyn Configuration Summary
============================

Grid:
  Domain size: {Lx:.2f} × {Ly:.2f} × {Lz:.2f} μm³
  Grid points: {self.grid.nx} × {self.grid.ny} × {self.grid.nz}
  Resolution: Δx={dx:.4f}, Δy={dy:.4f}, Δz={dz:.4f} μm

Simulation:
  Model: {'LWFA' if self.simulation.dmodel_id == 1 else 'Other'}
  Laser polarization: {['', 'p-polarized', 's-polarized', 'circular', 'envelope'][self.simulation.model_id]}
  Integration: {'Leap-frog' if self.simulation.LPf_ord == 2 else 'RK4'}

Laser:
  Wavelength: {self.laser.lam0} μm
  Normalized potential a0: {self.laser.a0}
  Spot size w0: {self.laser.w0_y} μm
  Pulse duration: {self.laser.tau_fwhm} fs

Target:
  Number of species: {self.target.nsp}
  Reference density n0: {self.target.n0_ref} nc

Output:
  Maximum time: {self.output.tmax} (1/ωp)
  CFL: {self.output.cfl}

MPI:
  Processors: {self.mpi.nprocx} × {self.mpi.nprocy} × {self.mpi.nprocz} = {self.mpi.nprocx * self.mpi.nprocy * self.mpi.nprocz}
"""
        return summary
