#!/bin/bash

vg=
yaml_file="ex2.1_pod_fit_gps"
while [ "$1" != "" ]; do
    case $1 in
    --vg ) vg="valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=valgrind.errors"
	    ;;
    -y) shift
	
	;;
    esac
    shift
done

pod_opts="-y $yaml_file.yaml"


# Run the POD
$vg ../src/build/bin/pod $pod_opts &> "$yaml_file/pod.out"

# echo 'Plotting: 1] orbit fit residual time series'
# python3 ~/pod/scripts/res_plot.py     -i gag19424_igs19424_orbdiff_rtn.out -d . -c G >& /dev/null
# echo '          2] orbit fit statistics'
# python3 ~/pod/scripts/rms_bar_plot.py -i gag19424_igs19424_orbdiff_rtn.out -d . -c G >& pod.rms

# # Diff output
# diff -b pod.out ./solution/pod.out
# diff pod.rms ./solution/pod.rms
# diff gag19424.sp3 ./solution/gag19424.sp3

# # Cleanup 
# if [ -z "$yaml_file" ]; then
# \rm -r EQM0* VEQ0* ECOM1_*.in emp*.in
# fi
# \rm -r DE.430

exit 
