#!/usr/bin/env python3
"""
Example LWFA (Laser Wakefield Acceleration) configuration in Python.

This configuration demonstrates a typical LWFA simulation setup,
converted from the traditional input_lwfa.nml format to Python.
"""

import sys
import os

# Add scripts directory to path
script_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'scripts')
sys.path.insert(0, script_dir)

from aladyn_config import (
    ALaDynConfig, GridConfig, SimulationConfig, TargetConfig,
    LaserConfig, MovingWindowConfig, OutputConfig, MPIConfig
)

# Define the configuration
config = ALaDynConfig(
    # Grid configuration
    grid=GridConfig(
        nx=5000,
        ny=4200,
        nz=1,           # 2D simulation
        ny_targ=3200,
        k0=50.0,        # 50 points per μm
        yx_rat=2.0,
        zx_rat=1.0,
    ),
    
    # Simulation parameters
    simulation=SimulationConfig(
        LPf_ord=2,      # Leap-frog integration
        der_ord=2,      # Standard Yee scheme
        str_flag=0,     # Uniform grid
        iform=0,        # Esirkepov charge conservation
        model_id=1,     # p-polarized laser
        dmodel_id=1,    # Uniform target
        ibx=0,          # Open boundaries in x
        iby=0,          # Open boundaries in y
        ibz=0,          # Open boundaries in z
        ibeam=1,
    ),
    
    # Target description
    target=TargetConfig(
        nsp=1,              # 1 species (electrons)
        nsb=0,              # No bunches
        ionz_lev=0,
        ionz_model=4,
        ion_min=[1, 1, 1],
        ion_max=[1, 1, 1],
        atomic_number=[1, 1, 1],  # Hydrogen
        mass_number=[1.0, 1.0, 1.0],
        t0_pl=[0.01, 0.0, 0.0, 0.0],  # Temperature
        np_per_xc=[2, 1, 1, 1, 1, 1],  # 2 particles per cell in x
        np_per_yc=[2, 1, 1, 1, 1, 1],  # 2 particles per cell in y
        concentration=[1],
        lpx=[0., 50., 400., 100., 0.0, 0.0, 15.0],  # Plasma profile in x
        lpy=[0.0, 0.0],  # Plasma profile in y
        n0_ref=1.0,      # Reference density
        np1=0.0,
        np2=0.0,
        r_c=0.0,
    ),
    
    # Laser parameters
    laser=LaserConfig(
        G_prof=True,        # Gaussian profile
        nb_laser=1,         # Single laser
        t0_lp=0.,
        xc_lp=50.,         # Laser center position
        tau_fwhm=40.,      # Pulse duration (FWHM)
        w0_y=20.0,         # Spot size
        a0=2.0,            # Normalized vector potential
        lam0=0.8,          # Wavelength (μm)
        y0_cent=[0.0],
        z0_cent=[0.0],
        incid_angle=0.0,
        Enable_ionization=[True, True],
        lp_delay=[20.59],
        lp_offset=0,
        t1_lp=200.0,
        tau1_fwhm=24.74,
        w1_y=3.5,
        a1=0.45,           # Second laser
        lam1=0.4,
        y1_cent=0.0,
        z1_cent=0.0,
        Symmetrization_pulse=False,
        a_symm_rat=1.35,
    ),
    
    # Moving window
    moving_window=MovingWindowConfig(
        w_sh=10,           # Window shift
        wi_time=0.,
        wf_time=1000.0,
        w_speed=1.0,       # Speed of light
    ),
    
    # Output configuration
    output=OutputConfig(
        nouts=4,
        iene=40,           # Energy diagnostics every 40 steps
        nvout=2,           # Vector field output
        nden=1,            # Density output
        npout=1,           # Particle output
        nbout=0,
        jump=1,
        pjump=1,
        gam_min=1.0,
        xp0_out=0.,
        xp1_out=60.0,
        yp_out=50.,
        tmax=45.,          # Maximum simulation time
        cfl=0.8,           # CFL condition
        new_sim=0,
        id_new=0,
        dump=0,
        L_env_modulus=True,
    ),
    
    # MPI configuration
    mpi=MPIConfig(
        nprocx=1,
        nprocy=40,         # 40 processors in y
        nprocz=1,
    ),
)

if __name__ == "__main__":
    # Generate the namelist file
    from config_to_namelist import config_to_namelist
    
    output_file = sys.argv[1] if len(sys.argv) > 1 else "input_lwfa.nml"
    config_to_namelist(config, output_file)
