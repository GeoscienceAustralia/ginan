### Ginan Python CLI Tools

This is a collection of tools to help you prepare your own data to run with Ginan.

## Getting started

Ensure you have created a virtual environment in the `scripts` folder and installed the `requirements.txt`.

Also ensure you have run `python3 scripts/s3_filehandler.py -d -p` to populate the `inputData` folder with the constants that `pea` depends on.

## prep

The following command `prep` can be used to download the required IGS products and generate the required yaml
for Ginan to be able to determine a static position from a RINEX file.

From your ginan root directory run the following command:

```
gn prep --ppp --static --rinex-path=path/to/rinex --config-name=test
```

Once this has been run, a new folder called `workspace/test` will be available. This is where all of the IGS products and generated template are stored.

```
cd workspace/test
pea --config test_auto_template.yaml
```

**Note**: This command is a work in progress and currently only supports a narrow use case:

* Only handles RINEX v3
* Only works for RINEX files that were collected >2 weeks ago (just requires FIN products)
* Only works on RINEX files collected after 2022-11-27 (long filenames started on CDDIS)
* Only handles a single RINEX file at a time
* Only handle downloads from CDDIS
* Recommendation to use RINEX files with >6 hours of data. The generated yaml configuration has ambiguity resolution is turned off, so while pea will still run with a shorter observation session, the results may be well off.

The prep command will be enhanced over time to support a wider range of use cases.

