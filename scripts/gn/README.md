### Ginan Python CLI Tools

This is a collection of tools to help you prepare your own data to run with Ginan.

## Getting started

Ensure you have created a virtual environment in the `scripts` folder and installed the `requirements.txt`. See [here](https://docs.python.org/3/library/venv.html) for more information.

```
cd /path/to/ginan/scripts

# Create a new virtualenv
python -m venv env

# Activate the virtualenv
source env/bin/activate

# Install requirements
pip install -r requirements.txt

# Install gn
pip install .
```

Also ensure you have run `python3 scripts/s3_filehandler.py -d -p` to populate the `inputData` folder with the constants that `pea` depends on.

## prep

The following command `prep` can be used to download the required IGS products and generate the required yaml
for Ginan to be able to determine a static position from a RINEX file.

From your ginan root directory run the following command:

```
gn prep --ppp --static --rinex-path=path/to/rinex --workspace-name=test
```

Once this has been run, a new folder called `workspace/test` will be available. This is where all of the IGS products and generated yaml config are stored.

```
cd workspace/test
pea --config test_auto_template.yaml
```

One the pea has finished running, open up the sinex file in the `workspace/test/outputs` folder to see the estimated position in the `SOLUTION/ESTIMATE` block.

**Note**: This command is a work in progress and currently only supports recent files that you would submit to AUSPOS and state government for inclusion in the national adjustment:

* Only handles RINEX v3
* Only works for RINEX files that were collected >2 weeks ago (just requires FIN products)
* Only works on RINEX files collected after 2022-11-27 (long filenames started on CDDIS)
* Only handles a single RINEX file at a time
* Only handle downloads from CDDIS
* Recommendation to use RINEX files with >6 hours of data. The generated yaml configuration has ambiguity resolution turned off, so while pea will still run with a shorter observation session, the results may be much less accurate.

The prep command will be enhanced over time to support a wider range of use cases.

### Known issues

* Sometimes downloads from the CDDIS ftp repository will time out. If this happens, try running the command again.