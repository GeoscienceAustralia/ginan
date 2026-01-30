# Ginan Scripts

This directory contains a number of useful scripts that facilitate:

  - running Ginan via:
    - a graphical user interface (under `scripts/GinanUI`)
    - shell scripts for installing Ginan natively (under `scripts/installation`)
    - scripts that handle downloading necessary input files (`auto_download_PPP.py`)
  - plotting Ginan output files, including
    - POS files (`plot_pos.py`)
    - SBAS SPP files (`plot_spp.py`)
    - ZTD files (`plotting/ztd_plot.py`)
    - Network trace files (`plot_trace_res.py`)
  - exploring and debugging Ginan and it's Kalman filter via:
    - The Ginan Exploratory Data Analysis (EDA) tool (`scripts/GinanEDA`)

Each sub-directory listed above contains it's own README, which provides further details on running the various functionalities.
The rest of this README will cover the files located on the `scripts` directory, namely:
1. `auto_download_PPP.py`
2. `plot_pos.py`
3. `plot_spp.py`
4. `plot_trace_res.py`

## _**Recommended:**_
Before continuing, it is highly recommended that you create a python virtual environment if you have not already done so as suggested on the root README file:
```bash
# Create virtual environment
python3 -m venv ginan-env
source ginan-env/bin/activate
```
The above line will the virtual environment in your current working directory. Once the above is complete, you will have the virtual environment in your current working directory.

You can then install all python dependencies via a `pip` command:
```bash
# Install Python dependencies
pip3 install -r requirements.txt
```
## 1. auto_download_PPP
The `auto_download_PPP.py` script makes it easier to download the necessary high precision products and model files necessary for processing RINEX data in Ginan to produce PPP results.

Based on a few details provided by the user via arguments in the command-line interface (CLI), the script fetches the appropriate files for a given date or date range. These files includes products such as:

  - precise orbits (`.SP3`)
  - broadcast orbits (`BRDC.RNX`)
  - precise clocks (`.CLK`)
  - Earth rotation parameters (`.ERP` or IERS IAU2000 file)
  - CORS station positions and metadata (`.SNX`)
  - satellite metadata (`.SNX`)
  - code biases (`.BIA`)

The product files are mostly obtained from the NASA archive known as the Crustal Dynamics Data Information System (CDDIS). This is one of NASA's Distributed Active Archive Centers (DAACs).

To use and download from this archive, you will need to create an Earthdata Login account and provide your username and password in a `.netrc` file. This outlined below in Section 1.2.

It also includes the various model files needed:

  - planetary ephemerides (JPL Development Ephemeris `DE436.1950.2050`)
  - atmospheric loading
  - geopotential (Earth Gravitational Model `EGM2008`)
  - geomagnetic reference field (International Geomagnetic Reference Field `IGRF14`)
  - ocean loading
  - ocean pole tide coefficients
  - ocean tide potential (Finite Element Solution 2014b `FES2014b`)
  - troposphere (Global Pressure and Temperature model `GPT2.5`)

These are needed for running PPP.

### 1.1 Earthdata Login Credentials - CDDIS Downloads
To download product files from the Crustal Dynamics Data Information System (CDDIS) web archive you will need an Earthdata Login account credentials saved to your machine.

#### 1.1.1 Create New Earthdata Login Account (if you don't have one):
You can create a new Earthdata account at the following website:
https://urs.earthdata.nasa.gov/users/new

#### 1.1.2 Save Credentials to Your Machine

Once you have your username and password, these must be saved in a `.netrc` file on your home directory. Depending on your operating system, this can be achieved in different ways:

##### Unix / Linux / MacOS:
This can be done in a terminal window via:
```bash
echo "machine urs.earthdata.nasa.gov login your_username password your_password" >> ~/.netrc
```
Make sure to set appropriately restrictive file permissions as well (read / write by the current user only):
```bash
chmod 0600 ~/.netrc
```

##### Windows:
1. Open Notepad or any plain-text editor.

2. Enter the `.netrc` format shown above, replace the placeholders with your actual login and password.

3. Save the file as `_netrc` (with an underscore instead of a period) in your home directory.

4. Set file permissions:
    - Right-click _netrc file and choose Properties
    - Go to the Security tab → Click Edit
    - Remove access for all other users except your own account
    - Click Apply to save the changes.

The above Earthdata credential instructions are adapted from the following website:<br>
https://nsidc.org/data/user-resources/help-center/creating-netrc-file-earthdata-login

### 1.2 Test "auto_download_PPP" in Virtual Enviroment
Once you have your credentials set up, you are ready to automatically download all necessary product and model files via python. Before we do this though, we will test that the script is working as expected.

First, make sure you have your virtual environment activated in your current terminal. Following the way we recommended to create the environment above, this would look like:
```bash
# Activate virtual environment - ginan-env
source ginan-env/bin/activate
```

Next, test that the `auto_download_PPP` script functions correctly:
```bash
# Test auto_download_PPP script:
python auto_download_PPP.py --help
```
This will display the help page with detailed information on all possible arugments into the function itself.

### 1.3 Example Run of "auto_download_PPP"
With your virtual environment active, you can now download the product files needed for a PPP run in Ginan.

We will use the `igs-station` preset to download RINEX files for two IGS stations for two days in 2024 together with all the product and model files needed to run this in Ginan.

```bash
# Example run of auto_download_PPP:
python auto_download_PPP.py --target-dir /data/temp/products --rinex-data-dir /data/temp/data --station-list ALIC,HOB2 --start-datetime 2024-01-06_00:00:00 --end-datetime 2024-01-07_23:59:30 --preset=igs-station --dont-replace
```
Each of the arguments used above are described below:

- `--target-dir`: sets the directory where product files are downloaded into (with model files going into `target-dir/tables`). In this case `target-dir=/data/temp/products` and model files end up in `/data/temp/products/tables`
- `--rinex-data-dir`: sets the directory where observational RINEX files are downloaded into (for `ALIC` and `HOB2` in this case)
- `--station-list`: this list of stations to download RINEX files for
- `--start-datetime`: the start date and time to download files from
- `--end-datetime`: the end date and time to download files to
- `--preset=igs-station`: tells the script to download all necessary products to run this in PPP mode in Ginan: `SP3`,`CLK`, `BRDC`, `SNX`, `BIA`, satellite metadata `SNX`, IERS Earth orientation data, plus all necessary model files in `target-dir/tables`

If you are aware of which files you need to download, you can use the default `preset` mode of `manual` and just choose the various files needed based on their flags. For detailed info on all possible flags, run:
```bash
python auto_download_PPP.py --help
```

## 2. plot_pos

The `plot_pos.py` script is used to visualise the contents of a Ginan `.POS` format file.

Output plots are plotly `.html` files that can be displayed in a web browser.

```bash
Usage:
plot_pos.py [-h] --input-files INPUT_FILES [INPUT_FILES ...] [--start-datetime START_DATETIME] [--end-datetime END_DATETIME] [--horz-smoothing HORZ_SMOOTHING] [--vert-smoothing VERT_SMOOTHING] [--colour-sigma] [--max-sigma MAX_SIGMA] [--elevation] [--demean] [--map] [--heatmap] [--sigma-threshold SIGMA_THRESHOLD SIGMA_THRESHOLD SIGMA_THRESHOLD] [--down-sample DOWN_SAMPLE] [--save-prefix [SAVE_PREFIX]]
```
Plots positional data and uncertainties with optional smoothing and color coding.

### Optional arguments:

 -  `-h`, `--help`: Show this help message and exit
 -  `--input-files` INPUT_FILES ...: One or more input .POS files for plotting (**required**)
 -  `--start-datetime` START_DATETIME: Start datetime in the format YYYY-MM-DDTHH:MM:SS, optional timezone
 -  `--end-datetime` END_DATETIME: End datetime in the format YYYY-MM-DDTHH:MM:SS, optional timezone
 -  `--horz-smoothing` HORZ_SMOOTHING: Fraction of the data used for horizontal LOWESS smoothing (optional).
 -  `--vert-smoothing` VERT_SMOOTHING: Fraction of the data used for vertical (Up) LOWESS smoothing (optional).
 -  `--colour-sigma`: Colourize the timeseries using the standard deviation (sigma) values (optional).
 -  `--max-sigma` MAX_SIGMA: Set a maximum sigma threshold for the sigma colour scale (optional).
 -  `--elevation`: Plot Elevation values inplace of dU wrt the reference coord (optional).
 -  `--demean`: Remove the mean values from all time series before plotting (optional).
 -  `--map`: Create a geographic map view from the Longitude & Latitude estiamtes (optional).
 -  `--heatmap`: Create a 2D heatmap view of E/N coodrinates wrt the reference position (optional).
 -  `--sigma-threshold` THRESHOLDS: Thresholds for sE, sN, and sU to filter data.
 -  `--down-sample` DOWN_SAMPLE: Interval in seconds for down-sampling data.
 -  `--save-prefix` [SAVE_PREFIX]: Prefix for saving HTML figures, e.g., ./output/fig

### Examples

- Plot a Ginan output .POS file:

```bash
python plot_pos.py --input-files ALIC00AUS_R_20191990000_01D_30S_MO.rnx.POS
```

- Plot a Ginan output .POS file, using colours to represent uncertainties and a heatmap of horizontal positions:

```bash
python plot_pos.py --input-files ALIC00AUS_R_20191990000_01D_30S_MO.rnx.POS --colour-sigma --heatmap --elevation
```

## 3. plot_spp

The `plot_spp.py` script is used to visualise the contents of a Ginan SBAS `.SPP` format files.

Output plots are plotly `.html` files that can be displayed in a web browser.

```bash
Usage:
plot_spp.py [-h] -i INPUT [INPUT ...] [-o OUTPUT] [--title TITLE] [--map] [--pl]
```
Plot Ginan SBAS .SPP output as Plotly HTML (concatenated inputs).

### Optional arguments:

 -  `-h`, `--help`: Show this help message and exit
 -  `-i`, `--input` INPUT ...: One or more input .SPP / *_SPP.POS files (**required**)
 -  `-o`, `--output` OUTPUT: Output .html file path (default: <first_input>.html)
 -  `--title` TITLE: Main plot title (default: derived from input filenames)
 -  `--map`: Also write a separate linked-hover lat/lon + Href HTML.
 -  `--pl`: Also write a protection-level vs error log/log plot (dH vs HPL, dU vs VPL).

### Examples

- Plot a Ginan SBAS .SPP file:

```bash
python plot_spp.py -i ALIC-202602303.SPP
```

- Concatenate and plot Ginan SBAS .SPP files, and a 2D lon/lat scatter (Eref vs Nref) map of horizontal positions:

```bash
python3 plot_spp.py -i ALIC-202602303.SPP ALIC-202602304.SPP ALIC-202602305.SPP --map
```

## 4. plot_trace_res

The `plot_trace_res.py` script is used to visualise the contents of a Ginan Network `.TRACE` format file.

Extracts and plots GNSS code and phase residuals by receiver and/or satellite with optional markers for large-errors, state errors.

Output plots are plotly `.html` files that can be displayed in a web browser

```bash
Usage:
plot_trace_res.py [-h] --files FILES [FILES ...] [--residual {prefit,postfit}] [--receivers RECEIVERS] [--sat SAT] [--label-regex LABEL_REGEX] [--max-abs MAX_ABS] [--start START] [--end END] [--decimate DECIMATE] [--split-per-sat | --split-per-recv] [--out-dir OUT_DIR] [--basename BASENAME] [--webgl] [--log-level {DEBUG,INFO,WARNING,ERROR,CRITICAL}] [--out-prefix OUT_PREFIX] [--mark-large-errors] [--hover-unified] [--plot-normalised-res] [--show-stats-table] [--stats-matrix] [--stats-matrix-weighted] [--annotate-stats-matrix] [--mark-amb-resets] [--ambiguity-counts] [--ambiguity-totals] [--amb-totals-orient {h,v}] [--amb-totals-topn AMB_TOTALS_TOPN] [--use-forward-residuals]
```

Optional arguments:

-   `-h`, `--help`: Show this help message and exit
-   `--files` FILES [FILES ...]: One or more TRACE files (space or , sep ), e.g. 'A.trace B.trace,C.trace' (wildcards allowed eg. *.TRACE)
-   `--residual` {prefit,postfit}: Plot prefit or postfit residuals (default: postfit)
-   `--receivers` RECEIVERS: One or more receiver names (space or , separated), e.g. 'ABMF,CHUR ALGO'
-   `--sat` SAT, -s SAT:  Filter by satellite ID
-   `--label-regex` LABEL_REGEX: Regex to filter labels
-   `--max-abs` MAX_ABS: Max residual to plot
-   `--start` START: Start datetime or time-only
-   `--end` END:  End datetime (exclusive)
-   `--decimate` DECIMATE:
-   `--split-per-sat` :
-   `--split-per-recv` :
-   `--out-dir` OUT_DIR: Output directory for HTML files; defaults to CWD.
-   `--basename` BASENAME: Base filename prefix for outputs (no extension).
-   `--webgl`:  Use webgl graphic acceleration
-   `--log-level` {DEBUG,INFO,WARNING,ERROR,CRITICAL}: Logging verbosity
-   `--out-prefix` OUT_PREFIX: unique prefix to add to output filenames
-   `--mark-large-errors`: Mark LARGE STATE/MEAS ERROR events on plots.
-   `--hover-unified`: Use unified hover tooltips across all traces (default: closest point hover).
-   `--plot-normalised-res`: Also generate plots of normalised residuals (residual / sigma).
-   `--show-stats-table`: Add a Mean / Std / RMS table per (sat × signal) at the bottom of each plot.
-   `--stats-matrix`: Generate receiver×satellite heatmaps (Mean/Std/RMS) aggregated across signals.
-   `--stats-matrix-weighted`: Use sigma-weighted statistics in the heatmaps (weights 1/σ²).
-   `--annotate-stats-matrix`: Write the numeric value (mean/std/rms) into each stats heatmap cell. Hover still shows full details.
-   `--mark-amb-resets`: Overlay PHASE ambiguity reset events (PREPROC=green, REJECT=blue) on PHASE per-receiver plots.
-   `--ambiguity-counts`: Plot cumulative counts of ambiguity reset reasons and unique satellite resets over time.
-   `--ambiguity-totals`: Bar chart of total ambiguity reset reasons (diagnostic view of detection methods).
-   `--amb-totals-orient`: {h,v} Orientation for totals bar charts: 'h' (horizontal, default) or 'v' (vertical).
-   `--amb-totals-topn AMB_TOTALS_TOPN`: Show only the top N receivers/satellites by total resets (to avoid clutter).
-   `--use-forward-residuals`: Use residuals from forward (non-smoothed) files instead of smoothed files (default: use smoothed for more accurate residuals).
