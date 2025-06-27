# ![Ginan Logo](https://raw.githubusercontent.com/GeoscienceAustralia/ginan/gh-pages/images/GinanLogo273-with-background.png)

# Ginan: GNSS Analysis Software Toolkit

[![Version](https://img.shields.io/badge/version-v3.1.0-blue.svg)](https://github.com/GeoscienceAustralia/ginan/releases)
[![License](https://img.shields.io/badge/license-Apache--2.0-green.svg)](LICENSE.md)
[![Platform](https://img.shields.io/badge/platform-Linux%20%7C%20macOS%20%7C%20Windows-lightgrey.svg)](#supported-platforms)
[![Docker](https://img.shields.io/badge/docker-available-blue.svg)](https://hub.docker.com/r/gnssanalysis/ginan)

**Ginan** is a powerful, open-source software toolkit for processing Global Navigation Satellite System (GNSS) observations for geodetic applications. Developed by Geoscience Australia, Ginan provides state-of-the-art capabilities for precise positioning, orbit determination, and atmospheric modeling.

## Table of Contents

- [Quick Start](#quick-start)
- [Overview](#overview)
- [Installation](#installation)
    - [Using Docker (Recommended)](#using-ginan-with-docker)
    - [Using AppImage](#using-ginan-with-an-appimage)
    - [From Source](#installation-from-source)
- [Getting Started](#getting-started-with-the-examples)
- [Additional Tools and Scripts](#additional-tools-and-scripts)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [Support](#support)
- [License](#license)
- [Acknowledgements](#acknowledgements)

## Quick Start

The fastest way to get started with Ginan is using Docker:

```bash
# Pull and run the latest Ginan container
docker run -it -v $(pwd):/data gnssanalysis/ginan:v3.1.0 bash

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

Choose the installation method that best fits your needs:

### Using Ginan with Docker

**Recommended for most users** - Get started quickly with a pre-configured environment:

```bash
# Run Ginan container with data volume mounting
docker run -it -v ${pwd}:/data gnssanalysis/ginan:v3.1.0 bash
```

This command:

- Mounts your current directory (`${pwd}`) to `/data` in the container
- Provides access to all Ginan tools and dependencies
- Opens an interactive bash shell

**Prerequisites:** [Docker](https://docs.docker.com/get-docker/) must be installed on your system.

**Verify installation:**
```bash
pea --help
```

### Using Ginan with an AppImage

**For Linux users** - Run Ginan without installing dependencies:

```bash
# Download the latest AppImage
git clone -b develop-weekly-appimage --depth 1 --single-branch https://github.com/GeoscienceAustralia/ginan.git

# Make executable and run
chmod +x ginan/Ginan-x86_64.AppImage
./ginan/Ginan-x86_64.AppImage
```

**For Windows users** (via WSL):
```bash
# Install Ubuntu on Windows
wsl --install -d ubuntu

# Run AppImage
./ginan/Ginan-x86_64.AppImage
```

**Troubleshooting:**
If the AppImage fails to run, install required libraries:
```bash
sudo apt install fuse libfuse2
```

**Note:** AppImage contains the core PEA executable but excludes Python scripts and example data.


### Installation from Source

**For developers and advanced users** who need to modify the source code or require specific configurations.

### Tested Platforms

| Platform | Tested Versions | Notes |
|----------|-----------------|-------|
| **Linux** | Ubuntu 18.04, 20.04, 22.04, 24.04 | Primary development platform |
| **macOS** | 10.15+ (x86) | Limited testing |
| **Windows** | 10+ | Via Docker or WSL only - Limited testing|

### Prerequisites

#### System Dependencies

**Compilers:**

- GCC/G++ (recommended, tested and supported) or equivalent C/C++ compiler

**Required Dependencies:**  

- **CMake** ≥ 3.0  

- **YAML** ≥ 0.6  

- **Boost** ≥ 1.73 (≥ 1.74 for GCC 11+)  

- **Mongo C Driver** ≥ 1.17.1  

- **Mongo C++ Driver** ≥ 3.6.0 (= 3.7.0 for GCC 11+)  

- **Eigen3** ≥ 3.4  

- **OpenBLAS** (provides BLAS and LAPACK)  

**Optional Dependencies:**

- **MongoDB** (for database features)  

- **netCDF4**  (for tidal loading computation)

- **Python** ≥ 3.9  

#### Quick Installation Scripts

Pre-written installation scripts are available in `scripts/installation/`:

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

**Note:** Scripts are maintained as best-effort and may require adjustments for your specific environment.


### Build Process

1. **Create build directory:**
   ```bash
   mkdir -p src/build
   cd src/build
   ```

2. **Configure with CMake:**
   ```bash
   cmake ../
   ```

3. **Compile (choose one):**
   ```bash
   # Build everything (parallel compilation recommended)
   make -j$(nproc)
   
   # Build specific components
   make pea -j$(nproc)              # Core PEA executable
   make make_otl_blq -j$(nproc)     # Ocean tide loading
   make interpolate_loading -j$(nproc)  # Loading interpolation
   ```

4. **Verify installation:**
   ```bash
   cd ../../exampleConfigs
   ../bin/pea --help
   ```

   Expected output:
   ```
   PEA starting... (main ginan-v3.1.0 from ...)
   Options:
     -h [ --help ]     Help
     -q [ --quiet ]    Less output
     ...
   ```

5. **Download example data:**
   ```bash
   cd ../inputData/data
   ./getData.sh
   cd ../products  
   ./getProducts.sh
   ```

#### Python Environment Setup

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
- **Performance tip:** For single-station PPP, limit cores to improve performance:
  ```bash
  OMP_NUM_THREADS=1 ../bin/pea --config ppp_example.yaml
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
| **Better Enums** | BSD-2-Clause | Enhanced enum support | [github.com/aantron/better-enums](http://github.com/aantron/better-enums) |
| **EGM96** | zlib | Earth gravitational model | [github.com/emericg/EGM96](https://github.com/emericg/EGM96) |
| **IERS2010**| Public Domain | Tidal displacement computation | [github.com/xanthospap/iers2010](https://github.com/xanthospap/iers2010)
| **JPL Ephemeris** | GPL-3 | Planetary ephemeris | [github.com/Bill-Gray/jpl_eph](https://github.com/Bill-Gray/jpl_eph) |
| **NRLMSISE** | Public Domain | Atmospheric modeling | [github.com/c0d3runn3r/nrlmsise](https://github.com/c0d3runn3r/nrlmsise/tree/master) |
| **RTKLIB** | BSD-2-Clause | GNSS processing routines | [github.com/tomojitakasu/RTKLIB](https://github.com/tomojitakasu/RTKLIB) |
| **SLR** | Public Domain | SLR input file managements | [ilrs.gsfc.nasa.gov](https://ilrs.gsfc.nasa.gov/data_and_products/formats/crd.html)*
| **SOFA** | SOFA License | Astronomical computations | [iausofa.org](https://www.iausofa.org/) |

All incorporated code has been preserved with appropriate modifications in the `cpp/src/` directory structure, maintaining original licensing and attribution requirements.

---

**Developed by [Geoscience Australia](https://www.ga.gov.au/)** | **Version 3.1.0** | **[GitHub Repository](https://github.com/GeoscienceAustralia/ginan)**
