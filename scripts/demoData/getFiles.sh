#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "Illegal number of parameters"
    echo "This script will read a list of files from a file, download them from a website, and extract them."
    echo "usage: $0 <file.list> <server root> <destination dir>"
    echo "eg:    $0 data.list https://peanpod.s3.ap-southeast-2.amazonaws.com/aux/data ./"
    exit 1
fi

File=$1
Server=$2
Destination=$3


echo "Reading from file list: $File"
echo "Downloading from server: $Server"
Lines=$(cat $File)

cd $Destination
for line in $Lines
do
	echo "Trying to download $line" from $Server

	url=$(echo "$Server/$line.gz" | sed "s/\/.\//\//g")
	rootdir=$(pwd)
	dir=$(dirname "$line")
	mkdir -p $dir
	cd $dir
	wget $url -N
	gzip -d $(basename "$line")
	cd $rootdir
done

