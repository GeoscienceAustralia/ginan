#!/bin/bash
# Specify the desired volume size in GiB as a command-line argument. If not specified, default to 250 GiB.
SIZE=${1:-250}

# Get the ID of the envrionment host Amazon EC2 instance.
INSTANCEID=$(curl http://169.254.169.254/latest/meta-data//instance-id)
EC2_AVAIL_ZONE=`curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone`
EC2_REGION="`echo \"$EC2_AVAIL_ZONE\" | sed 's/[a-z]$//'`"

# Get the ID of the Amazon EBS volume associated with the instance.
VOLUMEID=$(aws ec2 create-volume --volume-type gp2 --size $SIZE --availability-zone $EC2_AVAIL_ZONE | jq -r .VolumeId)

sleep 3

aws ec2 attach-volume --volume-id $VOLUMEID --instance-id $INSTANCEID --device /dev/sdf

sleep 5

mkdir /data

FILE=/dev/xvda

if [ -f "$FILE" ]; then
    echo "$FILE exist"
    mkfs.xfs /dev/xvdf

    mount /dev/xvdf /data
    chmod a+rw /data

    # Add a line to fstab to ensure disk is mounted on reboot
    echo "/dev/xvdf /data xfs user,rw" >> /etc/fstab
else 
    echo "$FILE does not exist"
    mkfs.xfs /dev/nvme1n1

    mount /dev/nvme1n1 /data
    chmod a+rw /data

    VAL_UUID=$(ls -l /dev/disk/by-uuid | grep nvme1n1 | awk '{print $9}')
    
    # Add a line to fstab to ensure disk is mounted on reboot
    echo "UUID=${VAL_UUID} /data xfs defaults 0 0" >> /etc/fstab
fi