#!/bin/bash

TESTCOUNT=0
FAILURECOUNT=0

NAME=$1
shift

for TEST in $@
do
	let TESTCOUNT=$TESTCOUNT+1

	echo Running $TEST...
	$TEST > $TEST.out

	if [ $? -eq 0 ]; then
		echo Test passed
		rm $TEST.out
	else
		echo Failed $? and its responded with:
		cat $TEST.out
		let FAILURECOUNT=$FAILURECOUNT+1
	fi

	echo
done

echo "Ran $TESTCOUNT tests with $FAILURECOUNT failures"

mkdir -p test-results

RESULTSXML=test-results/${NAME}_results.xml

echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?><testsuites name=\"$NAME\"><testsuite name=\"$NAME.Tests\" tests=\"$TESTCOUNT\" failures=\"$FAILURECOUNT\">" > $RESULTSXML

for TEST in $@
do
	FAILURE=$TEST.out

	if ! test -f "$FAILURE"; then
		# echo $FAILURE not found, continuing
		continue
	fi

	# echo $FAILURE
	#slice up test name
	FILENAME=${FAILURE##*/}
	REMAINDER=${FILENAME%.*}
	REMAINDER=${REMAINDER%.*}

	PART1=${REMAINDER%%.*}
	# echo $PART1

	REMAINDER=${REMAINDER#*.}

	PART2=${REMAINDER%%.*}
	# echo $PART2

	REMAINDER=${REMAINDER#*.}

	PART3=${REMAINDER%%.*}
	# echo $PART3

	#output to xml
	echo "<testcase name=\"$NAME\" classname=\"$PART1.$PART2\"><error message=\"Error\">$(cat $FAILURE | sed -r 's/</{/g ; s/>/}/g' ) </error></testcase>"	| sed -r 's/\x1B\[(;?[0-9]{1,3})+[mGK]//g' >> $RESULTSXML

	rm $FAILURE
done

echo "</testsuite></testsuites>" >> $RESULTSXML

echo Results written to JUnit XML : $RESULTSXML