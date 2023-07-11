
# s3_filehandler Scripts

The s3 file handler script available in the `scripts` directory is a python tool that will download the input data required to run the example as well as important file of solution

The detailed feautures of each option can be found by changing to the `scripts` directory and running
```
python3 s3_filehandler.py --help
```

* It is designed to be run at the root location of the ginan software but all path can be customised. 
* It has the capability to download (default) or upload data into a Amazon s3 bucket, however the upload requires special authorisation (access key)

To get started try the following examples:

Examples to run:
git
## Download input file:
```
python3 s3_filehandler.py  -p -l -d
```
will download all products, loading and data file

## Download results of some of the examples:

```
python3 s3_filehandler.py -s
```

to download them all, or by choosing the specific solution (for example ex02)

```
python3  s3_filehandler.py -s ex02
```


 
