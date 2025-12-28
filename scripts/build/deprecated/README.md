# Deprecated Build Scripts

This folder contains legacy build scripts that have been superseded by the unified `scripts/build.sh` script.

## Migration Guide

The old platform-specific scripts have been consolidated into a single script with automatic platform detection and command-line options.

### Old vs New

| Old Script | New Equivalent |
|------------|----------------|
| `cmake.generic` | `../build.sh` |
| `cmake.generic.debug` | `../build.sh -t Debug` |
| `cmake.generic.profiling` | `../build.sh -t Profiling` |
| `cmake.mac` | `../build.sh -p mac` |
| `cmake.marconi.intel` | `../build.sh -p marconi -c intel` |
| `cmake.marconi.knl` | `../build.sh -p marconi-knl -c intel` |
| `cmake.marconi.knl.fftw` | `../build.sh -p marconi-knl -c intel --fftw` |
| `cmake.marconi.knl.gnu` | `../build.sh -p marconi-knl -c gnu` |
| `cmake.cnaf.gnu` | `../build.sh -p cnaf -c gnu` |
| `cmake.cnaf.intel` | `../build.sh -p cnaf -c intel` |
| `cmake.cnaf.scorep` | `../build.sh -p cnaf --scorep` |

### Why Consolidate?

1. **Maintainability**: One script to update instead of 27
2. **Consistency**: Same options and behavior across all platforms
3. **Auto-detection**: Automatic platform and compiler detection
4. **Modern practices**: Proper error handling, colored output, help system

### Keeping Legacy Scripts

These scripts are kept for reference and backward compatibility. They may be removed in a future release.

If you have custom workflows depending on these scripts, please migrate to the new unified script.
