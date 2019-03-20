A makefile that uses docker.io, and qemu-user-static to build dependencies for our integration tests.

Builds and uploadsdocker images for multiple architectures. Allows for '-j' to build multiple images at once. Uploads assume the hub.docker.com docker registry.

# Setup

## Docker

Follow the instructions in [our dependencies file](doc/Dependencies.md) to ensure you have docker installed, and logged in.

## qemu

### Debian


```bash
apt-get install qemu-user-static
sudo service binfmt-support start
```

### Fedora

'sudo dnf install -y qemu-user-static'

# Using

Assuming you have docker, and have followed the above instructions, "make build-all" should work. This builds all of the images, and places them in docker on the local machine.
to build an individual image (and it's dependent images), run "make-<imagename>". to see a list of images that are buildable, run "make names".

## Using with Dockerhub

If you want to upload images to dockerhub, you must go to dockerhub, and create repositories under your user with the names of the images you want to upload. use `make names` to get the list of buildable images.

If you don't want to change the Makefile, add the DOCKER_USERNAME, DOCKER_EMAIL, and DOCKER_REALNAME environment variables.

For instance, when I want to build all debian images, and upload them to dockerhub I use:
```bash
make DIST=DEBIAN DOCKER_USERNAME=julialongtin DOCKER_EMAIL=julia.longtin@wire.com DOCKER_REALNAME='Julia Longtin' push-all
```

You can also push a single image (and it's dependent images) with "make push-<imagename>".

If you want your builds to go faster, and are okay with having interleaved output from multiple builds, use the '-j' argument to make, to parallize the builds. '-j' can take an integer argument for the number of threads you want it to run at once, or no argument for 'all of the things you can figure out how to do at once'.

By default this makefile builds and uploads the debian based images. Use the 'DIST=ALPINE' environment variable to build the alpine based images instead.

# Troubleshooting:
## binfmt support:

examine the following file, and ensure the 'flags:' line has an "F" flag on it:
cat /proc/sys/fs/binfmt_misc/qemu-arm | grep flags

if it doesn't, try re-starting binfmt-support on debian.

