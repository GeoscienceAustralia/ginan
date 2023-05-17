#!/bin/bash

curl --trace-ascii \
-F "payload_json='{'username': 'Clark Kent', 'content': 'Test two'}'" \
-F 'file1=@plots.pdf'	\
"https://discord.com/api/webhooks/1044032882672422942/s7MFwrbikBEIdjAGdKeX3cjBBtIgKhMqkdixnfHuiTo_5sJyy4Rx0-Ycv9hcccMfeYYR" 
