A makefile that uses docker.io, and qemu-user-static to build dependencies for our integration tests.

Builds docker images for multiple architectures. allows for -j to build multiple images at once.

# Setup

## Docker

Follow the instructions in [our dependencies file](doc/Dependencies.md) to ensure you have docker installed, and logged in.

## qemu

### Debian
`apt-get install qemu-user-static`
`sudo service binfmt-support start`

### Fedora

'sudo dnf install -y qemu-user-static'

# Using

First, you must go to dockerhub, and create repositories under your user with the proper names. to get the list of names, type 'make names'.

At this point, you should be able to change the account info at the top of the makefile, and type 'make'.
If you want it to go faster, but have more garbled output, type 'make -j 30'.

By default this makefile builds and uploads the debian based images. Type 'make DIST=ALPINE' to build the alpine based images instead.

# Troubleshooting:
## binfmt support:

examine the following file, and ensure the 'flags:' line has an "F" flag on it:
cat /proc/sys/fs/binfmt_misc/qemu-arm | grep flags

if it doesn't, try re-starting binfmt-support on debian.

