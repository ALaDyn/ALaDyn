#!/usr/bin/env python3
"""
ALaDyn Python to Fortran Namelist Converter

This module converts Python-based ALaDyn configurations to Fortran namelist format,
maintaining full compatibility with the existing ALaDyn Fortran code.
"""

from typing import Any, List, Union
from aladyn_config import ALaDynConfig


def format_fortran_value(value: Any) -> str:
    """Format a Python value for Fortran namelist.
    
    Args:
        value: Python value to format
        
    Returns:
        Fortran-formatted string
    """
    if isinstance(value, bool):
        return '.true.' if value else '.false.'
    elif isinstance(value, (int, float)):
        return str(value)
    elif isinstance(value, str):
        return f"'{value}'"
    elif isinstance(value, list):
        # Format arrays for Fortran
        formatted_items = [format_fortran_value(item) for item in value]
        return ', '.join(formatted_items)
    else:
        return str(value)


def write_namelist_section(file, section_name: str, params: dict, indent: int = 2):
    """Write a namelist section to file.
    
    Args:
        file: File object to write to
        section_name: Name of the namelist section
        params: Dictionary of parameters
        indent: Number of spaces for indentation
    """
    file.write(f"&{section_name.upper()}\n")
    
    # Find maximum parameter name length for alignment
    max_len = max(len(name) for name in params.keys()) if params else 0
    
    for name, value in params.items():
        if value is None:
            continue
        
        # Special handling for arrays
        if isinstance(value, list):
            # Check if all elements are the same as default, skip if so
            if name == 'ion_min' and all(v == 1 for v in value[:3]):
                formatted_value = format_fortran_value(value[:3])
            elif name == 'ion_max' and all(v == 1 for v in value[:3]):
                formatted_value = format_fortran_value(value[:3])
            elif name == 'atomic_number' and all(v == 1 for v in value[:3]):
                formatted_value = format_fortran_value(value[:3])
            elif name == 'mass_number' and all(v == 1.0 for v in value[:3]):
                formatted_value = format_fortran_value(value[:3])
            elif name.startswith('t0_pl'):
                formatted_value = format_fortran_value(value[:4])
            elif name.startswith('np_per_'):
                formatted_value = format_fortran_value(value[:6])
            elif name == 'ppc':
                # Skip if all values are -1 (not used)
                if all(v == -1 for v in value):
                    continue
                formatted_value = format_fortran_value(value[:6])
            elif name == 'lpx':
                formatted_value = format_fortran_value(value[:7])
            elif name == 'lpy':
                formatted_value = format_fortran_value(value[:2])
            elif name == 'lp_delay':
                formatted_value = format_fortran_value(value[:1])
            elif name in ['y0_cent', 'z0_cent']:
                formatted_value = format_fortran_value(value[:1])
            elif name == 'Enable_ionization':
                formatted_value = format_fortran_value(value[:2])
            else:
                formatted_value = format_fortran_value(value)
        else:
            formatted_value = format_fortran_value(value)
        
        # Write with proper formatting
        spaces = ' ' * (max_len - len(name) + 1)
        file.write(f"  {name}{spaces}= {formatted_value},\n")
    
    file.write("/\n\n")


def config_to_namelist(config: ALaDynConfig, output_file: str = "input.nml"):
    """Convert ALaDyn Python configuration to Fortran namelist file.
    
    Args:
        config: ALaDynConfig object with simulation parameters
        output_file: Output filename for the namelist
    """
    # Validate configuration
    config.validate()
    
    with open(output_file, 'w') as f:
        # Write header comment
        f.write("!\n")
        f.write("! ALaDyn input file\n")
        f.write("! Generated from Python configuration\n")
        f.write("!\n\n")
        
        # GRID section
        grid_params = {
            'nx': config.grid.nx,
            'ny': config.grid.ny,
            'nz': config.grid.nz,
            'ny_targ': config.grid.ny_targ,
            'k0': config.grid.k0,
            'yx_rat': config.grid.yx_rat,
            'zx_rat': config.grid.zx_rat,
        }
        write_namelist_section(f, 'GRID', grid_params)
        
        # SIMULATION section
        sim_params = {
            'LPf_ord': config.simulation.LPf_ord,
            'der_ord': config.simulation.der_ord,
            'str_flag': config.simulation.str_flag,
            'iform': config.simulation.iform,
            'model_id': config.simulation.model_id,
            'dmodel_id': config.simulation.dmodel_id,
            'ibx': config.simulation.ibx,
            'iby': config.simulation.iby,
            'ibz': config.simulation.ibz,
            'ibeam': config.simulation.ibeam,
        }
        write_namelist_section(f, 'SIMULATION', sim_params)
        
        # TARGET_DESCRIPTION section
        target_params = {
            'nsp': config.target.nsp,
            'nsb': config.target.nsb,
            'ionz_lev': config.target.ionz_lev,
            'ionz_model': config.target.ionz_model,
            'ion_min': config.target.ion_min,
            'ion_max': config.target.ion_max,
            'atomic_number': config.target.atomic_number,
            'mass_number': config.target.mass_number,
            't0_pl': config.target.t0_pl,
        }
        
        # Add ppc or np_per_xc/yc/zc depending on what's used
        if any(p >= 1 for p in config.target.ppc):
            target_params['ppc'] = config.target.ppc
        else:
            target_params['np_per_xc'] = config.target.np_per_xc
            target_params['np_per_yc'] = config.target.np_per_yc
            if config.grid.nz > 1:
                target_params['np_per_zc'] = config.target.np_per_zc
        
        target_params.update({
            'concentration': config.target.concentration,
            'lpx': config.target.lpx,
            'lpy': config.target.lpy,
            'n0_ref': config.target.n0_ref,
            'np1': config.target.np1,
            'np2': config.target.np2,
            'r_c': config.target.r_c,
        })
        
        if config.target.l_disable_rng_seed:
            target_params['l_disable_rng_seed'] = config.target.l_disable_rng_seed
        
        write_namelist_section(f, 'TARGET_DESCRIPTION', target_params)
        
        # LASER section
        laser_params = {
            'G_prof': config.laser.G_prof,
            'nb_laser': config.laser.nb_laser,
            't0_lp': config.laser.t0_lp,
            'xc_lp': config.laser.xc_lp,
            'tau_fwhm': config.laser.tau_fwhm,
            'w0_y': config.laser.w0_y,
            'a0': config.laser.a0,
            'lam0': config.laser.lam0,
            'y0_cent': config.laser.y0_cent,
            'z0_cent': config.laser.z0_cent,
            'incid_angle': config.laser.incid_angle,
            'Enable_ionization': config.laser.Enable_ionization,
            'lp_delay': config.laser.lp_delay,
        }
        
        # Add second laser parameters if used
        if config.laser.a1 > 0:
            laser_params.update({
                'lp_offset': config.laser.lp_offset,
                't1_lp': config.laser.t1_lp,
                'tau1_fwhm': config.laser.tau1_fwhm,
                'w1_y': config.laser.w1_y,
                'a1': config.laser.a1,
                'lam1': config.laser.lam1,
                'y1_cent': config.laser.y1_cent,
                'z1_cent': config.laser.z1_cent,
            })
        
        if config.laser.Symmetrization_pulse:
            laser_params.update({
                'Symmetrization_pulse': config.laser.Symmetrization_pulse,
                'a_symm_rat': config.laser.a_symm_rat,
            })
        
        write_namelist_section(f, 'LASER', laser_params)
        
        # BEAM_INJECT section (if beam is configured)
        if config.beam is not None and config.target.nsb > 0:
            beam_params = {
                'nb_1': config.beam.nb_1,
                'xc_1': config.beam.xc_1,
                'gam_1': config.beam.gam_1,
                'sxb_1': config.beam.sxb_1,
                'syb_1': config.beam.syb_1,
                'epsy_1': config.beam.epsy_1,
                'epsz_1': config.beam.epsz_1,
                'dg_1': config.beam.dg_1,
                'charge_1': config.beam.charge_1,
                'ap1_twiss': config.beam.ap1_twiss,
                'bt1_twiss': config.beam.bt1_twiss,
                't_inject': config.beam.t_inject,
            }
            write_namelist_section(f, 'BEAM_INJECT', beam_params)
        
        # MOVING_WINDOW section
        window_params = {
            'w_sh': config.moving_window.w_sh,
            'wi_time': config.moving_window.wi_time,
            'wf_time': config.moving_window.wf_time,
            'w_speed': config.moving_window.w_speed,
        }
        write_namelist_section(f, 'MOVING_WINDOW', window_params)
        
        # OUTPUT section
        output_params = {
            'nouts': config.output.nouts,
            'iene': config.output.iene,
            'nvout': config.output.nvout,
            'nden': config.output.nden,
            'npout': config.output.npout,
            'nbout': config.output.nbout,
            'jump': config.output.jump,
            'pjump': config.output.pjump,
            'gam_min': config.output.gam_min,
            'xp0_out': config.output.xp0_out,
            'xp1_out': config.output.xp1_out,
            'yp_out': config.output.yp_out,
            'tmax': config.output.tmax,
            'cfl': config.output.cfl,
            'new_sim': config.output.new_sim,
            'id_new': config.output.id_new,
            'dump': config.output.dump,
            'L_env_modulus': config.output.L_env_modulus,
        }
        write_namelist_section(f, 'OUTPUT', output_params)
        
        # TRACKING section (if tracking is enabled)
        if config.tracking is not None and config.tracking.p_tracking:
            tracking_params = {
                'tkjump': config.tracking.tkjump,
                'nkjump': config.tracking.nkjump,
                'txmin': config.tracking.txmin,
                'txmax': config.tracking.txmax,
                'tymin': config.tracking.tymin,
                'tymax': config.tracking.tymax,
                'tzmin': config.tracking.tzmin,
                'tzmax': config.tracking.tzmax,
                't_in': config.tracking.t_in,
                't_out': config.tracking.t_out,
                'p_tracking': config.tracking.p_tracking,
            }
            write_namelist_section(f, 'TRACKING', tracking_params)
        
        # MPIPARAMS section
        mpi_params = {
            'nprocx': config.mpi.nprocx,
            'nprocy': config.mpi.nprocy,
            'nprocz': config.mpi.nprocz,
        }
        write_namelist_section(f, 'MPIPARAMS', mpi_params)
    
    print(f"Namelist file written to: {output_file}")
    print(config.summary())


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1:
        # Load configuration from Python file
        config_file = sys.argv[1]
        output_file = sys.argv[2] if len(sys.argv) > 2 else "input.nml"
        
        # Import the configuration
        import importlib.util
        spec = importlib.util.spec_from_file_location("user_config", config_file)
        user_config = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(user_config)
        
        # Convert to namelist
        if hasattr(user_config, 'config'):
            config_to_namelist(user_config.config, output_file)
        else:
            print("Error: Configuration file must define a 'config' variable of type ALaDynConfig")
            sys.exit(1)
    else:
        print("Usage: python config_to_namelist.py <config_file.py> [output_file.nml]")
        print("\nExample:")
        print("  python config_to_namelist.py input_lwfa.py input.nml")
        sys.exit(1)
