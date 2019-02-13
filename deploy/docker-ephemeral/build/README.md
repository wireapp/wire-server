A makefile that uses docker.io, and qemu-user-static to build dependencies for our integration tests.

Builds docker images for multiple architectures. allows for -j to build multiple images at once.

# Setup

## Docker
docker.io is only available from debian testing or unstable.
`apt-get install docker.io`

after installing docker-io, add the user you are running as to the docker group, and restart X.

once you've logged in again,
# docker login --username=$(USERNAME)

## qemu
`apt-get install qemu-user-static`
`sudo service binfmt-support start`
`cat /proc/sys/fs/binfmt_misc/qemu-arm`
make sure that you see 'flags: OCF'.

# Using

At this point, you should be able to change the account info at the top of the makefile, and type 'make'.
If you want it to go faster, but have more garbled output, type 'make -j 30'.

make by default builds and uploads the debian based images. type 'make DIST=ALPINE' to build the alpine
based images instead.

