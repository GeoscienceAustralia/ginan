# ![Ginan Logo](https://raw.githubusercontent.com/GeoscienceAustralia/ginan/gh-pages/images/GinanLogo273-with-background.png)

# Ginan: GNSS Analysis Software Toolkit

[![Version](https://img.shields.io/badge/version-v4.1.3-blue.svg)](https://github.com/GeoscienceAustralia/ginan/releases)
[![License](https://img.shields.io/badge/license-Apache--2.0-green.svg)](LICENSE.md)
[![Platform](https://img.shields.io/badge/platform-Linux%20%7C%20macOS%20%7C%20Windows-lightgrey.svg)](#supported-platforms)
[![Docker](https://img.shields.io/badge/docker-available-blue.svg)](https://hub.docker.com/r/gnssanalysis/ginan)

**Ginan** is a powerful, open-source software toolkit for processing Global Navigation Satellite System (GNSS) observations for geodetic applications. Developed by Geoscience Australia, Ginan provides state-of-the-art capabilities for precise positioning, orbit determination, and atmospheric modeling.

## How to cite

If you use Ginan in a publication, please cite:
```
McClusky, Simon; Hammond, Aaron; Maj, Ronald; Allgeyer, Sébastien; Harima, Ken; Yeo, Mark; Du, Eugene; Riddell, Anna, "Precise Point Positioning with Ginan: Geoscience Australia’s Open-Source GNSS Analysis Centre Software," Proceedings of the ION 2024 Pacific PNT Meeting, Honolulu, Hawaii, April 2024, pp. 248-280. https://doi.org/10.33012/2024.19598
```

## Table of Contents

- [Quick Start](#quick-start)
- [Overview](#overview)
   - [How to cite](#how-to-cite)
   - [Supported GNSS Constellations](#supported-gnss-constellations)
   - [Key Features and Capabilities](#key-features-and-capabilities)
   - [Architecture](#architecture)
- [Installation](#installation)
   - [Using Ginan with Docker](#using-ginan-with-docker)
   - [Precompiled binaries](#precompiled-binaries)
   - [Installation from Source](#installation-from-source)
      - [Tested Platforms](#tested-platforms)
      - [Recommended Source Build: `vcpkg` + CMake Presets](#recommended-source-build-vcpkg--cmake-presets)
      - [Legacy: manual `cmake` + `make` instructions](#legacy-manual-cmake---make-instructions)
   - [Python Environment Setup](#python-environment-setup)
- [Getting Started with the examples](#getting-started-with-the-examples)
   - [Running Your First Example](#running-your-first-example)
   - [Adding Ginan to PATH](#adding-ginan-to-path)
- [Additional Tools and Scripts](#additional-tools-and-scripts)
- [Documentation](#documentation)
   - [User Documentation](#user-documentation)
   - [Developer Documentation](#developer-documentation)
   - [Generating Code Documentation](#generating-code-documentation)
- [Contributing](#contributing)
   - [Reporting Issues](#reporting-issues)
   - [Contributing Code](#contributing-code)
   - [Development Setup](#development-setup)
- [Support](#support)
   - [Getting Help](#getting-help)
- [License](#license)
   - [Third-Party Components](#third-party-components)
- [Acknowledgements](#acknowledgements)

## Quick Start

The fastest way to get started with Ginan is using Docker:

```bash
# Pull and run the latest Ginan container
docker run -it -v "$(pwd):/data" gnssanalysis/ginan:v4.1.3 bash

# Verify installation
pea --help

# Run a basic example
cd /ginan/exampleConfigs
pea --config ppp_example.yaml
```

## Overview

Ginan is a comprehensive processing package for GNSS observations in geodetic applications, supporting multiple satellite constellations and providing advanced analysis capabilities.



### Supported GNSS Constellations

We currently support processing of:

- **GPS** - United States' Global Positioning System
- **Galileo** - European Union's Galileo system  
- **GLONASS** - Russian GLONASS system*
- **BeiDou** - Chinese Navigation Satellite System*
- **QZSS** - Japanese Quasi-Zenith Satellite System*

*\*Development ongoing*

### Key Features and Capabilities

Ginan provides the following advanced capabilities:

- **Precise Orbit & Clock Determination** (POD) of GNSS satellites
- **Precise Point Positioning** (PPP) for stations in network and individual modes
- **Real-time corrections** generation for PPP users
- **Multi-frequency, multi-GNSS** data analysis
- **Atmospheric products** including ionosphere and troposphere models
- **Low Earth Orbiter** orbit modeling capabilities
- **Satellite Laser Ranging** processing capabilites
- Support for **wide range of users and receiver types**
- **User-friendly outputs** accessible by non-experts
- **Real-time and offline** processing capabilities
- **IGS products** generation (final, rapid, ultra-rapid, and real-time)
- **Ocean Tide Loading** (OTL) displacement modeling

### Architecture

The software consists of three main components:

- **Parameter Estimation Algorithm (PEA)** - Core processing engine incorporating Precise Orbit Determination
- **Analysis Scripts** - Tools for data preparation, solution combination and analysis
- **Visualization Tools** - Python-based plotting and comparison utilities

## Installation

Choose one of the paths below:

| Method | Best for | Notes |
|--------|----------|-------|
| Docker | Most users, tutorials, reproducible runs | Includes Ginan and its runtime dependencies. |
| Precompiled binaries | Users who want a native executable without building | Available from GitHub Releases for Linux, macOS, and Windows. |
| Source build with `vcpkg` | Developers and users who need a custom build | Recommended source-build path. |
| Legacy/manual source build | Sites with system-managed dependencies | Best-effort support; use only when `vcpkg` is not suitable. |

### Using Ginan with Docker

**Recommended for most users.** Docker is the fastest way to run Ginan with a known-good environment.

Prerequisite: install [Docker](https://docs.docker.com/get-docker/).

```bash
# Run Ginan container with data volume mounting on Linux/macOS
docker run -it -v "$(pwd):/data" gnssanalysis/ginan:v4.1.3 bash

# PowerShell equivalent on Windows
docker run -it -v "${PWD}:/data" gnssanalysis/ginan:v4.1.3 bash
```

The command mounts your current directory to `/data` in the container and opens an interactive shell with Ginan available.

**Verify installation:**

```bash
pea --help
```

### Precompiled binaries

Precompiled binaries for **Ginan** and **GinanUI** are available on the project's GitHub Releases page: https://github.com/GeoscienceAustralia/ginan/releases

We publish builds for the following platforms:

- Linux (x86_64)
- macOS (arm64 and x86_64)
- Windows (x86_64)

These artifacts are provided for convenience and have been tested on our CI runners and a subset of target systems. They may not work on every configuration — if you encounter problems please try the Docker image or build from source, and open an issue on GitHub with your OS and steps to reproduce.

Note about Windows binaries: We have observed an output file-size limitation on Windows builds where RTS/output files appear limited at about 2.1 GB (roughly equivalent to a PPP processing of two stations over one day at 30 s resolution). If you require larger RTS outputs, run the processing on Linux/macOS (or in the Docker image) or build from source on a platform without this limitation. We plan to implement a permanent solution in a future release.

### Installation from Source

**For developers and advanced users** who need to modify the source code or require specific configurations.

#### Tested Platforms

| Platform | Tested Versions | Notes |
|----------|-----------------|-------|
| **Linux** | Ubuntu 22.04, 24.04 | Primary development platform |
| **macOS** | 10.15+ (x86) | Limited testing |
| **Windows** | 10+ |  Limited testing|

#### Recommended Source Build: `vcpkg` + CMake Presets

The recommended source build uses the repository's CMake presets and `vcpkg` manifest. This keeps dependency versions close to the CI/release builds.

Prerequisites:

- CMake 3.22 or newer
- A C/C++ compiler for your platform
- Git
- `vcpkg`, installed or cloned as shown below

From the repository root:

```bash
export VCPKG_ROOT="$PWD/vcpkg"
export VCPKG_COMMIT="4c5ae6b55f3e3e39d291679f89822f496cf190ee"

git clone https://github.com/Microsoft/vcpkg.git "$VCPKG_ROOT"
git -C "$VCPKG_ROOT" fetch --depth 1 origin "$VCPKG_COMMIT"
git -C "$VCPKG_ROOT" checkout --detach "$VCPKG_COMMIT"
"$VCPKG_ROOT/bootstrap-vcpkg.sh" -disableMetrics
```

If you already have `vcpkg` installed elsewhere, set `VCPKG_ROOT` to that directory and skip the clone commands.

On Windows PowerShell:

```powershell
$env:VCPKG_ROOT = "$PWD\vcpkg"
$env:VCPKG_COMMIT = "4c5ae6b55f3e3e39d291679f89822f496cf190ee"

git clone https://github.com/Microsoft/vcpkg.git $env:VCPKG_ROOT
git -C $env:VCPKG_ROOT fetch --depth 1 origin $env:VCPKG_COMMIT
git -C $env:VCPKG_ROOT checkout --detach $env:VCPKG_COMMIT
& "$env:VCPKG_ROOT\bootstrap-vcpkg.bat" -disableMetrics
```

Then configure and build from `src` using the preset for your platform:

```bash
cd src
cmake --preset release
cmake --build --preset release
```

Common release presets:

| Platform | Configure/build preset |
|----------|------------------------|
| Linux x86_64 | `release` |
| macOS Apple silicon | `macos-arm64-release` |
| macOS Intel | `macos-x64-release` |
| Windows native | `windows-release` |
| Windows cross-compile from Linux | `windows-cross-release` |

For example, on Apple silicon:

```bash
cd src
cmake --preset macos-arm64-release
cmake --build --preset macos-arm64-release
```

Build outputs are written to the repository-level `bin/` and `lib/` directories.

Verify the build:

```bash
../bin/pea --help
```

The CMake presets use platform-specific dependency install roots under `./vcpkg_installed/`. If a configure step fails after changing triplets or branches, remove the relevant preset build directory under `src/build/` and configure again.

**OpenBLAS threading:** Ginan can use OpenMP while OpenBLAS may also use worker threads. If you see warnings such as `OpenBLAS Warning : Detect OpenMP Loop and this application may hang`, set `OPENBLAS_NUM_THREADS=1` so BLAS/LAPACK calls run single-threaded inside Ginan's OpenMP regions.

#### Legacy: manual `cmake` + `make` instructions

Use this path only if you need system-managed dependencies instead of `vcpkg`.

Pre-written installation scripts are available in `scripts/installation/` for systems where you prefer distro-specific package installation instead of `vcpkg`:

```bash
# Ubuntu 24.04
./scripts/installation/ubuntu24.sh

# Ubuntu 22.04
./scripts/installation/ubuntu22.sh

# Ubuntu 20.04
./scripts/installation/ubuntu20.sh

# Fedora 38
./scripts/installation/fedora38.sh

# Generic instructions
cat scripts/installation/generic.md
```

These scripts are maintained as best-effort and may require adjustments for your environment.

The older manual flow is still available for users who prefer it:

1. Create build directory:

```bash
mkdir -p src/build
cd src/build
```

2. Configure with CMake (legacy):

```bash
cmake ../
```

3. Compile (legacy):

```bash
# Build everything (parallel compilation recommended)
make -j$(nproc)

# Build specific components
make pea -j$(nproc)              # Core PEA executable
make make_otl_blq -j$(nproc)     # Ocean tide loading
make interpolate_loading -j$(nproc)  # Loading interpolation
```

4. Verify installation:

```bash
cd ../../exampleConfigs
../bin/pea --help
```

Expected output:
```
PEA starting... (main ginan-v4.1.3 from ...)
Options:
  -h [ --help ]     Help
  -q [ --quiet ]    Less output
  ...
```

5. Download example data:

```bash
cd ../inputData/data
./getData.sh
cd ../products  
./getProducts.sh
```

### Python Environment Setup

Ginan uses Python for automation, post-processing, and visualization:

```bash
# Create virtual environment (recommended)
python3 -m venv ginan-env
source ginan-env/bin/activate

# Install Python dependencies
pip3 install -r scripts/requirements.txt
```


## Getting Started with the examples

Congratulations! Ginan is now ready to use. The examples in `exampleConfigs/` provide a great starting point.

- **Working directory:** All examples must be run from the `exampleConfigs/` directory due to relative paths
- **MongoDB:** If MongoDB is not installed, set `mongo: enable: None` in configuration files
- **Threading:** Ginan can use OpenMP while OpenBLAS may also use its own worker threads. If you see warnings such as `OpenBLAS Warning : Detect OpenMP Loop and this application may hang`, set `OPENBLAS_NUM_THREADS=1` so BLAS/LAPACK calls run single-threaded inside Ginan's OpenMP regions. `GOTO_NUM_THREADS=1` is also recognised by OpenBLAS-compatible builds.
- **Performance tip:** For single-station PPP, limit OpenMP cores to improve performance:
  ```bash
  OPENBLAS_NUM_THREADS=1 GOTO_NUM_THREADS=1 OMP_NUM_THREADS=1 ../bin/pea --config ppp_example.yaml
  ```


### Running Your First Example

1. **Navigate to examples directory:**
   ```bash
   cd exampleConfigs
   ```

2. **Run basic PPP example:**
   ```bash
   ../bin/pea --config ppp_example.yaml
   ```

3. **Check outputs:**

   The processing will create an directory named `outputs/ppp_example/` or similar containing:
   - `*.trace` files with station processing details
   - `Network*.trace` with Kalman filter results  
   - Other auxiliary outputs as configured


### Adding Ginan to PATH

For convenience, add Ginan binaries to your system PATH:

```bash
# Add to ~/.bashrc or ~/.zshrc
export PATH="/path/to/ginan/bin:$PATH"

# Then run from anywhere
pea --config /path/to/config.yaml
```



## Additional Tools and Scripts

Beyond the core PEA executable, Ginan includes [comprehensive scripts](https://geoscienceaustralia.github.io/ginan/page.html?c=on&p=scripts.index) for:

- **Data downloading** and preprocessing
- **Output visualization** and analysis  
- **Solution comparison** and validation
- **Performance monitoring** and reporting

## Documentation

Ginan documentation is available in multiple formats:

### User Documentation

- **Online Manual:** [geoscienceaustralia.github.io/ginan](https://geoscienceaustralia.github.io/ginan/)
- **Configuration Guide:** [Detailed parameter explanations and examples](https://geoscienceaustralia.github.io/ginan/page.html?c=on&p=ginanConfiguration.md)
- **FAQ:** [Ginan FAQ](https://geoscienceaustralia.github.io/ginan/page.html?p=ginanFAQ.html)

### Developer Documentation  

- **Code Documentation:** [API Reference](https://geoscienceaustralia.github.io/ginan/codeDocs/index.html)

### Generating Code Documentation

Requirements: `doxygen` and `graphviz`

```bash
# Install dependencies (Ubuntu/Debian)
sudo apt install doxygen graphviz

# Generate documentation
cd src/build
cmake ../
make docs

# View documentation
open ../../Docs/codeDocs/index.html
```

## Contributing

We welcome contributions from the community! Here's how to get involved:

### Reporting Issues
- Use [GitHub Issues](https://github.com/GeoscienceAustralia/ginan/issues) for bug reports
- Provide detailed reproduction steps and system information
- Check existing issues before creating new ones

### Contributing Code
1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Follow our [coding standards](Docs/codingStandard.md)
4. Submit a pull request with clear description

### Development Setup
- Follow the source installation instructions above
- Review `Docs/codingStandard.md` for guidelines
- Run tests before submitting.

## Support

### Getting Help
- **Documentation:** Check the [online manual](https://geoscienceaustralia.github.io/ginan/) first
- **Issues:** Report bugs and feature requests on [GitHub](https://github.com/GeoscienceAustralia/ginan/issues)
- **Discussions:** Join community discussions on [GitHub Discussions](https://github.com/GeoscienceAustralia/ginan/discussions)

## License

Ginan is released under the **Apache License 2.0**. See [LICENSE.md](LICENSE.md) for details.

### Third-Party Components
This software incorporates components from several open-source projects. See [Acknowledgements](#acknowledgements) below for detailed attribution.
## Acknowledgements

Ginan incorporates code from several excellent open-source projects:

| Project | License | Purpose | Original Source |
|---------|---------|---------|-----------------|
| **Magic Enum** | MIT | Enhanced enum support | [github.com/Neargye/magic_enums](https://github.com/Neargye/magic_enums) |
| **EGM96** | zlib | Earth gravitational model | [github.com/emericg/EGM96](https://github.com/emericg/EGM96) |
| **IERS2010**| Public Domain | Tidal displacement computation | [github.com/xanthospap/iers2010](https://github.com/xanthospap/iers2010)
| **JPL Ephemeris** | GPL-3 | Planetary ephemeris | [github.com/Bill-Gray/jpl_eph](https://github.com/Bill-Gray/jpl_eph) |
| **NRLMSISE** | Public Domain | Atmospheric modeling | [github.com/c0d3runn3r/nrlmsise](https://github.com/c0d3runn3r/nrlmsise/tree/master) |
| **RTKLIB** | BSD-2-Clause | GNSS processing routines | [github.com/tomojitakasu/RTKLIB](https://github.com/tomojitakasu/RTKLIB) |
| **SLR** | Public Domain | SLR input file managements | [ilrs.gsfc.nasa.gov](https://ilrs.gsfc.nasa.gov/data_and_products/formats/crd.html)*
| **SOFA** | SOFA License | Astronomical computations | [iausofa.org](https://www.iausofa.org/) |

All incorporated code has been preserved with appropriate modifications in the `cpp/src/` directory structure, maintaining original licensing and attribution requirements.

---

**Developed by [Geoscience Australia](https://www.ga.gov.au/)** | **Version 4.1.3** | **[GitHub Repository](https://github.com/GeoscienceAustralia/ginan)**
