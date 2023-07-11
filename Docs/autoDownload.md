
# Auto Download Scripts

The auto download script available in the `scripts` directory is a python tool that will automatically download various inputs needed to run Ginan

The detailed feautures of each option can be found by changing to the `scripts` directory and running

    python3 auto_download_PPP.py --help

however, some of the features include: 
* the ability to download RINEX files from Geoscience Australia's `gnss-data` data repository, 
* the ability to choose between final, rapid and ultra-rapid file types 
* the ability to choose the analysis centre (apart from SNX coordinate and BIA bias files which come from IGS and COD, respectively)

To get started try the following examples:

Examples to run:

## Download necessary real-time inputs:
```
python3 auto_download_PPP.py                \
    --target-dir="/data/tmp-dwn"            \
    --preset="real-time"
```

## Download inputs for post-processed runs:

Files for post-processing runs may be downloaded using most defaults:
```
python3 auto_download_PPP.py                \
    --target-dir="/data/tmp-dwn"            \
    --preset="igs-station"                  \
    --station-list="ALIC,DARW"              \
    --start-datetime="2023-02-24_00:00:00"  \
    --end-datetime="2023-02-26_00:00:00"
```

or by choosing the solution type (ultra-rapid) and analysis centre (ESA):

```
python3 auto_download_PPP.py                \
    --target-dir="/data/tmp-dwn"            \
    --preset="igs-station"                  \
    --station-list="ALIC,DARW"              \
    --start-datetime="2023-02-24_00:00:00"  \
    --end-datetime="2023-02-26_00:00:00"    \
    --solution-type="ULT"                   \
    --analysis-center="ESA"
```


 
