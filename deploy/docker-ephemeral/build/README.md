A makefile that uses docker-io, and qemu-user-static to build dependencies for our integration tests.

Builds docker images for multiple architectures.


after installing docker-io, add the user you are running as to the docker group, and restart X.

# docker login --username=$(USERNAME)

# start binfmt support for debian. note that amd support is broken on i386.

