#!/usr/bin/env python3
"""
Example PWFA (Plasma Wakefield Acceleration) configuration in Python.

This configuration demonstrates a typical PWFA simulation setup,
where a particle bunch drives a plasma wake.
"""

import sys
import os

# Add scripts directory to path
script_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'scripts')
sys.path.insert(0, script_dir)

from aladyn_config import (
    ALaDynConfig, GridConfig, SimulationConfig, TargetConfig,
    LaserConfig, BeamConfig, MovingWindowConfig, OutputConfig, MPIConfig
)

# Define the configuration
config = ALaDynConfig(
    # Grid configuration
    grid=GridConfig(
        nx=4000,
        ny=2000,
        nz=1,           # 2D simulation
        ny_targ=1800,
        k0=100.0,       # 100 points per μm
        yx_rat=1.0,
        zx_rat=1.0,
    ),
    
    # Simulation parameters
    simulation=SimulationConfig(
        LPf_ord=2,      # Leap-frog integration
        der_ord=2,      # Standard Yee scheme
        str_flag=0,     # Uniform grid
        iform=0,        # Esirkepov charge conservation
        model_id=1,     # p-polarized (not used for PWFA)
        dmodel_id=1,    # Uniform target
        ibx=0,          # Open boundaries in x
        iby=0,          # Open boundaries in y
        ibz=0,          # Open boundaries in z
        ibeam=0,
    ),
    
    # Target description
    target=TargetConfig(
        nsp=1,              # 1 species (electrons)
        nsb=1,              # 1 bunch
        ionz_lev=0,
        ionz_model=4,
        ion_min=[1, 1, 1],
        ion_max=[1, 1, 1],
        atomic_number=[1, 1, 1],  # Hydrogen
        mass_number=[1.0, 1.0, 1.0],
        t0_pl=[0.0, 0.0, 0.0, 0.0],  # Cold plasma
        np_per_xc=[4, 1, 1, 1, 1, 1],  # 4 particles per cell in x
        np_per_yc=[4, 1, 1, 1, 1, 1],  # 4 particles per cell in y
        concentration=[1],
        lpx=[0., 10., 200., 10., 0.0, 0.0, 5.0],  # Plasma profile in x
        lpy=[0.0, 0.0],
        n0_ref=0.01,     # Low density plasma (0.01 nc)
        np1=0.0,
        np2=0.0,
        r_c=0.0,
    ),
    
    # Laser parameters (minimal, not used for PWFA)
    laser=LaserConfig(
        G_prof=False,
        nb_laser=0,
        t0_lp=0.,
        xc_lp=0.,
        tau_fwhm=1.,
        w0_y=1.0,
        a0=0.0,         # No laser
        lam0=0.8,
        y0_cent=[0.0],
        z0_cent=[0.0],
    ),
    
    # Beam injection parameters
    beam=BeamConfig(
        nb_1=1.0,          # 100,000 particles
        xc_1=20.0,         # Beam center at 20 μm
        gam_1=1000.0,      # Gamma = 1000 (relativistic)
        sxb_1=2.0,         # Bunch length 2 μm
        syb_1=1.0,         # Transverse size 1 μm
        epsy_1=0.1,        # Normalized emittance in y
        epsz_1=0.1,        # Normalized emittance in z
        dg_1=10.0,         # Energy spread
        charge_1=1.0,      # Bunch charge (nC)
        ap1_twiss=0.0,     # Twiss alpha
        bt1_twiss=1.0,     # Twiss beta
        t_inject=0.0,      # Injection time
    ),
    
    # Moving window
    moving_window=MovingWindowConfig(
        w_sh=5,            # Window shift
        wi_time=0.,
        wf_time=100.0,
        w_speed=1.0,       # Speed of light
    ),
    
    # Output configuration
    output=OutputConfig(
        nouts=4,
        iene=20,           # Energy diagnostics every 20 steps
        nvout=2,           # Vector field output
        nden=1,            # Density output
        npout=1,           # Particle output
        nbout=1,           # Bunch output
        jump=1,
        pjump=1,
        gam_min=1.0,
        xp0_out=0.,
        xp1_out=40.0,
        yp_out=20.,
        tmax=20.,          # Maximum simulation time
        cfl=0.9,           # CFL condition
        new_sim=0,
        id_new=0,
        dump=0,
        L_env_modulus=False,
    ),
    
    # MPI configuration
    mpi=MPIConfig(
        nprocx=1,
        nprocy=20,         # 20 processors in y
        nprocz=1,
    ),
)

if __name__ == "__main__":
    # Generate the namelist file
    from config_to_namelist import config_to_namelist
    
    output_file = sys.argv[1] if len(sys.argv) > 1 else "input_pwfa.nml"
    config_to_namelist(config, output_file)
