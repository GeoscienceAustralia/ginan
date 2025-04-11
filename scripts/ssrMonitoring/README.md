### Ginan SSR Stream Quality Monitoring Toolkit

This is a collection of tools to automatically record SSR streams with Ginan and monitor the quality of their orbit and clock corrections.

## Getting started

Ensure you have created a virtual environment in the `scripts` folder and installed the `requirements.txt`. See [here](https://docs.python.org/3/library/venv.html) for more information.

```bash
cd /path/to/ginan/scripts

# Create a new virtualenv
python -m venv env

# Activate the virtualenv
source env/bin/activate

# Install requirements
pip install -r requirements.txt
```

Ensure you have `auto_download_PPP.py` ready in the `scripts` folder.

Also ensure you have set up your AWS credentials for S3 bucket with AWS CLI before running `auto_record_ssr_streams.py` or `upload_recordings.py`.

```bash
aws configure
```

## Use of the master script `auto_record_ssr_streams.py`

This script is master script that calls following individual scripts to automatically download required real-time products and start PEA instances to record and decode SSR streams, compare their orbits and clocks against IGS rapid products, and upload output files to an AWS S3 bucket.

```bash
python auto_record_ssr_streams.py --job-dir /path/to/job/folder/ --product-dir /path/to/ginan/products/ --template-config /path/to/ginan/debugConfigs/record_ssr_stream.yaml --ssr-streams 'SSRA00BKG0, SSRA00GFZ0, SSRA00WHU0, SSRA02IGS0, SSRA03IGS0' --aws-profile aws-credentials-profile --s3-bucket target-s3-bucket --s3-root-dir target/s3/prefix --cull-file-types '.rtcm, .rnx, .json'
```

## Use of individual scripts

# `kill_pids.py`

This script is to kill running processes that may conflict with the target job.

```bash
python kill_pids.py --job-dir /path/to/job/folder/
```

# `download_rt_products.py`

This script is to regularly download/update necessary product files used for recording SSR streams. It will run infinitely and attempt download periodically based on the input `--interval` argument once it is started.

```bash
python download_rt_products.py --product-dir /path/to/ginan/products/ --interval 86400
```

# `record_ssr_stream.py`

This script is to start a PEA instance to record and decode a SSR stream in real-time. The PEA instance will run infinitely once it is started.

```bash
python record_ssr_stream.py --template-config /path/to/ginan/debugConfigs/record_ssr_stream.yaml --job-dir /path/to/job/folder/ --product-dir /path/to/ginan/products/ --ssr-mountpoint SSRA00BKG0 --rotation-period 86400 --interval 1
```

# `analyse_orbit_clock.py`

This script is to compare orbits and clocks in SP3 and CLK files against corresponding reference products, such as IGS rapid products, calculate orbit/clock differences and statistics, and plot out orbit/clock differences and RMS errors for given time period. It can run in post-processing (finite) mode or real-time (infinite) mode.

Post processing mode:
```bash
python analyse_orbit_clock.py --job-dir /path/to/job/folder/ --ref-dir /path/to/ginan/products/ --start-yrdoy 2024204 --end-yrdoy 2024215 --session-len 1 --clk-norm-types 'epoch, daily' --rel-output-dir gnssanalysis
```

Real-time mode:
```bash
python analyse_orbit_clock.py --job-dir /path/to/job/folder/ --ref-dir /path/to/ginan/products/ --session-len 1 --clk-norm-types 'epoch, daily' --rel-output-dir gnssanalysis
```

# `upload_recordings.py`

This script is to upload outputs and log files in the job folder to an AWS S3 bucket. It will run infinitely and attempt upload periodically based on the input `--interval` argument once it is started.

```bash
python upload_recordings.py --job-dir /path/to/job/folder/ --aws-profile aws-credentials-profile --s3-bucket target-s3-bucket --s3-root-dir target/s3/prefix --interval 86400 --time-threshold 129600 --cull-file-types '.rtcm, .rnx, .json'
```
