#!/usr/bin/env python3

import click
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

    try:
        with open(file0) as json_file:
            data0 = yaml.load(json_file, Loader=yaml.CLoader)

            if not data0:
                exit("Couldnt load " + file0 + " or it is empty")
            else:
                print("Loaded " + str(len(data0)) + " epochs from " + file0)
        with open(file1) as json_file:
            data1 = yaml.load(json_file, Loader=yaml.CLoader)

            if not data1:
                exit("Couldnt load " + file1 + " or it is empty")
            else:
                print("Loaded " + str(len(data1)) + " epochs from " + file1)
    except:
        exit("Couldnt load " + file0 + " or " + file1)

    print()
    
    dict0 = collections.defaultdict(dict)
    dict1 = collections.defaultdict(dict)
         
    for entry in data0:
        dict0[entry["Epoch"]][tuple(sorted(entry["id"].items()))] = entry["val"]

    for entry in data1:
        dict1[entry["Epoch"]][tuple(sorted(entry["id"].items()))] = entry["val"]

    fail = False    
    for time, timeDict0  in dict0       .items():
     try:   
        timeDict1 = dict1[time]
     except KeyError as k:
                                                    fail = True
                                                    print(time + ":    " 
                                                        + " is not found. ")
                                                    continue;
     for key, keyDict0   in timeDict0   .items():
      try:
        keyDict1 = timeDict1[key]
      except KeyError as k:
                                                    fail = True
                                                    print(time + ":    " 
                                                        + bcolors.WARNING + convertTuple(key) + bcolors.ENDC 
                                                        + " is not found. ")
                                                    continue;
      for k2, v0         in keyDict0    .items():
       try:
        v1 = keyDict1[k2]
       except KeyError as k:
                                                    fail = True
                                                    print(time + ":    " 
                                                        + bcolors.WARNING + convertTuple(key) + bcolors.ENDC 
                                                        + " : " 
                                                        + bcolors.WARNING + k2.ljust(15) + bcolors.ENDC
                                                        + " is not found: " )
                                                    continue;
       if v0 != v1:
            fail = True
            print(time + ":    " 
                + bcolors.WARNING + convertTuple(key) + bcolors.ENDC 
                + " : " 
                + bcolors.WARNING + k2.ljust(15) + bcolors.ENDC 
                + " is different: " 
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
