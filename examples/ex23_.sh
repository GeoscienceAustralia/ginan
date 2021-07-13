#!/bin/bash

vg=
yaml_file=
pod_opts="-m 2 -q 1 -k 1 -w 0 -r eopc04_IAU2000.62-now -s igs20010.sp3 -o igs20010.sp3"
while [ "$1" != "" ]; do
    case $1 in
    --vg ) vg="valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=valgrind.errors"
	    ;;
    -y) shift
	yaml_file="$1"
	pod_opts="$pod_opts -y $yaml_file"
	;;
    esac
    shift
done

# Test script for POD install

if [ ! -e ~/pod ]; then 
  echo "No sym link to the pod root directory"
  echo "In your home directory create a sym link to the pod root directory"
  echo "Eg: cd ~; ln -s /data/software/acs/pod pod"
  exit
fi

# Clean up old files
~/pod/test/ex3.clean
 
# Create necessary links
ln -s ~/pod/tables/ascp1950.430 .             >& /dev/null
ln -s ~/pod/tables/fes2004_Cnm-Snm.dat .      >& /dev/null
ln -s ~/pod/tables/goco05s.gfc .              >& /dev/null
ln -s ~/pod/tables/header.430_229 .           >& /dev/null
ln -s ~/pod/tables/igs_metadata_2063.snx .    >& /dev/null
ln -s ~/pod/tables/leap.second .              >& /dev/null
ln -s ~/pod/tables/eopc04_14_IAU2000.62-now . >& /dev/null

# Run the POD
$vg ~/pod/bin/pod $pod_opts | tee pod.out

echo 'Plotting: 1] orbit fit residual time series'
python3 ~/pod/scripts/res_plot.py     -i gag20010_igs20010_orbdiff_rtn.out -d . -c G >& /dev/null
echo '          2] orbit fit statistics'
python3 ~/pod/scripts/rms_bar_plot.py -i gag20010_igs20010_orbdiff_rtn.out -d . -c G >& pod.rms

# Diff output
diff pod.noyaml.out ./solution/pod.out
diff pod.rms ./solution/pod.rms
diff gag20010.sp3 ./solution/gag20010.sp3

# Cleanup 
if [ -z "$yaml_file" ]; then
\rm -r EQM0* VEQ0* ECOM1_*.in emp*.in
fi
\rm -r DE.430

exit 
