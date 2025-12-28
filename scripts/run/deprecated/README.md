# Deprecated Run Scripts

This folder contains legacy run scripts that have been superseded by the unified `scripts/run.sh` script.

## Migration Guide

The old platform-specific run scripts have been consolidated into a single script with automatic platform detection and job scheduler support.

### Old vs New

| Old Script | New Equivalent |
|------------|----------------|
| `cnaf_gnu_144.sh` | `../run.sh -p cnaf -t 144 --compiler gnu` |
| `cnaf_ifort_144.sh` | `../run.sh -p cnaf -t 144 --compiler intel` |
| `cnaf_scorep_64.sh` | `../run.sh -p cnaf -t 64 --scorep` |
| `marconi-68-knl.cmd` | `../run.sh -p marconi-knl -n 1 -t 68` |
| `marconi-68-knl-fftw.cmd` | `../run.sh -p marconi-knl -n 1 -t 68 --fftw` |
| `marconi-68-knl-gnu.cmd` | `../run.sh -p marconi-knl -n 1 -t 68 --compiler gnu` |

### New Features

The unified script provides:

1. **Multiple batch systems**: SLURM, LSF, and direct mpirun
2. **Auto-detection**: Platform and batch system detection
3. **Dry-run mode**: Generate scripts without submitting
4. **Local execution**: Run without a batch system
5. **Flexible options**: Nodes, tasks, walltime, queue, account

### Examples

```bash
# Run on 2 nodes with auto-detected settings
../run.sh -n 2 -a MyProject

# Generate script without submitting
../run.sh -p marconi-knl --dry-run

# Run locally with 4 MPI tasks
../run.sh --local -t 4
```

### Keeping Legacy Scripts

These scripts are kept for reference. They may be removed in a future release.
