# Container image that runs your code
FROM ubuntu:18.04

# Copies your code file from your action repository to the filesystem path `/` of the container
COPY entrypoint.sh	/entrypoint.sh
COPY instally.sh	/instally.sh
COPY 0001-Patch-to-make-call-graphs-output-in-order-of-calling.patch /0001-Patch-to-make-call-graphs-output-in-order-of-calling.patch

RUN			["/instally.sh"]

# Code file to execute when the docker container starts up (`entrypoint.sh`)
ENTRYPOINT	["/entrypoint.sh"]