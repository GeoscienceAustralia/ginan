#!/usr/bin/env python

import re
from array import *
import numpy as np
import math
import argparse
import os

# Initiate the parser
parser = argparse.ArgumentParser(description="Compare pod output rms and out files")

parser.add_argument("-so", "--solutionout", dest='solutionout', type=str, required=False, default='pod.out', help="solution pod.out file location name")
parser.add_argument("-sr", "--solutionrms", dest='solutionrms', type=str, required=False, default='pod.rms', help="solution pod.rms file location name")
parser.add_argument("-ro", "--runout", dest='runout', type=str, required=False, default='pod.out', help="New pod run pod.out file location name")
parser.add_argument("-rr", "--runrms", dest='runrms', type=str, required=False, default='pod.rms', help="New pod run pod.rms file location name")
parser.add_argument("-em", "--errormargin", dest='errormargin', type=float, required=False, default=0.1, help="The error margin for the end comparision of the two output files")

def test(solutionrms, solutionout, runout, runrms, errormargin):
    # Get summary stats for each satellite from pod.out and solution/pod.out and save to a list 
    fail_count = 0

    solution_pod_out_log_location = solutionout
    regex = 'RMS-XYZ ITRF CMP ([A-Z]\d\d) +([0-9.0-9?]+) +([0-9.0-9?]+) +([0-9.0-9?]+)'
    
    solution_pod_out = []
    with open(solution_pod_out_log_location, "r") as file:
        for line in file:
            for match in re.finditer(regex, line, re.S):
                sat_number = match.group(1)
                sat_x = float(match.group(2))
                sat_y = float(match.group(3))
                sat_z = float(match.group(4))
                solution_pod_out.append([sat_number,sat_x,sat_y,sat_z])
    print("Solution pod.out results to compare against [Sat_Number,x,y,z]") 
    print(solution_pod_out)

    test_pod_out_log_location = runout
    regex = 'RMS-XYZ ITRF CMP ([A-Z]\d\d) +([0-9.0-9?]+) +([0-9.0-9?]+) +([0-9.0-9?]+)'
    
    test_pod_out = []
    with open(test_pod_out_log_location, "r") as file:
        for line in file:
            for match in re.finditer(regex, line, re.S):
                sat_number = match.group(1)
                sat_x = float(match.group(2))
                sat_y = float(match.group(3))
                sat_z = float(match.group(4))
                test_pod_out.append([sat_number,sat_x,sat_y,sat_z])
    
    print("Current run pod.out results [Sat_Number,x,y,z]")
    print(test_pod_out)

    # Get summart stats from pod.rms and solution/pod.rms
    
    solution_pod_rms_log_location = solutionrms
    regex = 'PRN:.(.........)...........ALL:.(.[0-9.0-9?]+).(.[0-9.0-9?]+).(.[0-9.0-9?]+).(.[0-9.0-9?]+)'
    
    solution_rms_out = []
    with open(solution_pod_rms_log_location, "r") as file:
        for line in file:
            for match in re.finditer(regex, line, re.S):
                name = match.group(1).strip()
                r = float(match.group(2))
                t = float(match.group(3))
                n = float(match.group(4))
                d = float(match.group(5))
                #print("Name = {}, R = {}, T = {}, N = {}, 3D = {}".format(name,r,t,n,d))
                solution_rms_out.append([name,r,t,n,d])
    print("Solution pod.rms results to compare against [Name,R,T,N,3D]")
    print(solution_rms_out)
    
    test_pod_rms_log_location = runrms
    regex = 'PRN:.(.........)...........ALL:.(.[0-9.0-9?]+).(.[0-9.0-9?]+).(.[0-9.0-9?]+).(.[0-9.0-9?]+)'
    
    test_rms_out = []
    with open(test_pod_rms_log_location, "r") as file:
        for line in file:
            for match in re.finditer(regex, line, re.S):
                name = match.group(1).strip()
                r = float(match.group(2))
                t = float(match.group(3))
                n = float(match.group(4))
                d = float(match.group(5))
                #print("Name = {}, R = {}, T = {}, N = {}, 3D = {}".format(name,r,t,n,d))
                test_rms_out.append([name,r,t,n,d])
    print("Currnet run pod.rms results [Name,R,T,N,3D]")
    print(test_rms_out)
    
    print("Compare magnitude different for pod.out results against solution benchmark")
    for test, solution in zip(test_pod_out, solution_pod_out):
        mag_diff = math.sqrt((test[1] - solution[1])**2 + (test[2] - solution[2])**2 + (test[3] - solution[3])**2)
        if(mag_diff > errormargin):
            print("Difference of {} found for satellite {}".format(test[0],mag_diff))
            fail_count+=1
        else:
            print("Within Error Margin, test passed")

        #    assert mag_diff < errormargin

    print("Compare difference for pod.rms results against solution benchmark")
    for test_rms, solution_rms in zip(test_rms_out, solution_rms_out):
        r_diff = test_rms[1] - solution_rms[1]
        t_diff = test_rms[2] - solution_rms[2]
        n_diff = test_rms[3] - solution_rms[3]
        d_diff = test_rms[4] - solution_rms[4]
        
        if((r_diff > errormargin) or (t_diff > errormargin) or (n_diff > errormargin) or (d_diff > errormargin)):
            print("Difference of {} found for  R = {}, T = {}, N = {}, 3D = {}".format(test_rms[0],r_diff,t_diff,n_diff,d_diff))
            fail_count += 1
        else:
            print("Within Error Margin, test passed")

        #assert r_diff < errormargin
        #assert t_diff < errormargin
        #assert n_diff < errormargin
        #assert d_diff < errormargin

    return fail_count

if __name__ == "__main__":
    
    args = parser.parse_args()
    count = test(args.solutionrms, args.solutionout, args.runout, args.runrms, args.errormargin)
    if (count == 0):
        print("Everything passed")
        exit(0)
    else:
        print(str(count)+ " tests failed")
        exit(1)
