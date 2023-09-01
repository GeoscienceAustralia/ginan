#!/usr/bin/env python3

import click
import json
import yaml
import pprint
import collections

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

pp = pprint.PrettyPrinter(indent=4)

def convertTuple(tup):
    stri = ''
    for item in tup:
        stri = stri + str(item)[1:-1] + " "
    stri = stri.replace("\\t", "\t")
    return stri

@click.command()
@click.argument("file0", nargs=1, type=str)
@click.argument("file1", nargs=1, type=str)
def compare_ginan_json(
    file0: str,
    file1: str):

    with open(file0) as json_file:
        data0 = yaml.safe_load(json_file)
    with open(file1) as json_file:
        data1 = yaml.safe_load(json_file)

    if not data0:
        print("couldnt load " + file0 + " or it is empty")
    else:
        print("Loaded " + str(len(data0)) + " epochs from " + file0)
    if not data1:
        print("couldnt load " + file1 + " or it is empty")
    else:
        print("Loaded " + str(len(data1)) + " epochs from " + file1)

    print()
    
    dict0 = collections.defaultdict(dict)
    dict1 = collections.defaultdict(dict)
         
    for entry in data0:
        dict0[entry["time"]][tuple(sorted(entry["id"].items()))] = entry["val"]

    for entry in data1:
        dict1[entry["time"]][tuple(sorted(entry["id"].items()))] = entry["val"]

    fail = False
    for time in dict0:
     for key in dict0[time]:
      for k2 in dict0[time][key]:
       v0 = dict0[time][key][k2]
       v1 = dict1[time][key][k2]
       if v0 != v1:
            fail = True
            print(time + ":    " 
                + bcolors.WARNING + k2.ljust(15) + bcolors.ENDC 
                + " is different for key: " 
                + bcolors.WARNING + convertTuple(key) + bcolors.ENDC 
                + " : " 
                + bcolors.OKBLUE + str(v0).ljust(20) + bcolors.ENDC 
                + " -> " 
                + bcolors.OKCYAN + str(v1).ljust(20) + bcolors.ENDC, end="")
            t0 = type(v0)
            t1 = type(v1)
            if (t0 == int or t0 == float) and (t0 == t1):
                print("   diff of " 
                    + bcolors.HEADER + str(v1 - v0).ljust(30)                  + bcolors.ENDC + " or " 
                    + bcolors.FAIL + str((v1-v0)/(v0) * 100).rjust(30) + "%"   + bcolors.ENDC)
            else:
                print()
    if fail:
        exit("Differences found")
    else:
        exit(None)

if __name__ == "__main__":
    compare_ginan_json()
