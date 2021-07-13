#!/bin/bash -x
#==============================================================================
# Create a geodesy user and ssh permissions
#==============================================================================
/usr/sbin/useradd -c geodesy geodesy -p "" -m
sudo -u ec2-user /usr/local/bin/aws s3 cp s3://gnss-analysis/users/ga_moore_rsa.pub ./
sudo -u ec2-user /usr/local/bin/aws s3 cp s3://gnss-analysis/users/moore_moba_xterm.pub ./
sudo -u ec2-user /usr/local/bin/aws s3 cp s3://gnss-analysis/users/moore_thinkpad_rsa.pub ./

cd /home/geodesy
sudo -u geodesy ssh-keygen -t rsa -N "" -f /home/geodesy/.ssh/id_rsa
sudo -u geodesy touch /home/geodesy/.ssh/authorized_keys

#==============================================================================
cd /home/ec2-user
sudo -u ec2-user ssh-keygen -t rsa -N "" -f /home/ec2-user/.ssh/id_rsa
sudo -u ec2-user touch /home/ec2-user/.ssh/authorized_keys

sudo chmod a+rw *.pub
sudo ls -1 *.pub | xargs -L1 -I% cat % >> authorized_keys
cp authorized_keys geodesy_keys
chown geodesy.geodesy geodesy_keys
cp geodesy_keys /home/geodesy/.ssh/authorized_keys

sudo -u ec2-user cat /home/ec2-user/ga_moore_rsa.pub >> /home/ec2-user/.ssh/authorized_keys
sudo -u ec2-user cat /home/ec2-user/moore_moba_xterm.pub >> /home/ec2-user/.ssh/authorized_keys 
sudo -u ec2-user cat /home/ec2-user/moore_thinkpad_rsa.pub >> /home/ec2-user/.ssh/authorized_keys 

sudo -u geodesy chmod 600 /home/geodesy/.ssh/authorized_keys
sudo -u ec2-user chmod 600 /home/ec2-user/.ssh/authorized_keys

#==============================================================================
# set up ssh so it doesn;t kick you out every 10 minutes
#==============================================================================
echo '' >> /etc/ssh/sshd_config
echo 'ClientAliveInterval 60' >> /etc/ssh/sshd_config
sudo systemctl restart sshd.service
#==============================================================================
# Change the ownership of /data to the geodesy users
#==============================================================================
chown geodesy.geodesy /data
