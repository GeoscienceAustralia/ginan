#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
    echo ""
    echo "This script will zip and upload a piped list of files to an AWS S3 bucket"
    echo "S3 cli credentials need to be valid before running the script"
    echo ""
    echo "Usage: $0 <bucket> <root dir>"
    echo ""
    echo "eg:  find . -name '*.test' -type f > test.list"
    echo "     cat test.list | $0 peanpod aux/test"
    echo ""
    exit 1
fi

echo "Zipping and uploading piped files"

Lines=$(cat)
Bucket=$1
Destination=$2

for line in $Lines
do
  echo ""
  echo "Zipping $line"
  gzip -k -f $line

  echo "Getting checksum for $line.gz"
  md5=$(md5sum $source_file | awk '{ print $1}')
  echo $md5

  Key=$(echo "$Destination/$line.gz" | sed "s/\/.\//\//g")

  echo "Trying to upload $line.gz" to $Key

  set -x
  response=$(aws s3api put-object \
    --bucket "$Bucket" \
    --body "$line.gz" \
    --key "$Key" \
    --metadata "x-amz-meta-md5=$md5" \
  )

  if [[ ${?} -ne 0 ]]; then
    echo "ERROR: AWS reports put-object operation failed.\n$response"
    return 1
  fi

  response2=$(aws s3api put-object-acl \
    --bucket "$Bucket" \
    --key "$Key" \
    --acl public-read \
    )
  set +x

  if [[ ${?} -ne 0 ]]; then
    echo "ERROR: AWS reports put-object-acl operation failed.\n$response2"
    return 1
  fi
done

exit 1
