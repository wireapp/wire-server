# About this document:
This document is written with the goal of explaining https://github.com/wireapp/wire-server/pull/622 well enough that someone can honestly review it. :)

In this document, we're going to rapidly bounce back and forth between GNU make, bash, GNU sed, Docker, and QEMU.

# What does this Makefile do? Why was it created?

To answer that, we're going to have to go back to Wire-Server, specifically, our integration tests. Integration tests are run locally on all of our machines, in order to ensure that changes we make to the Wire backend do not break currently existing functionality. In order to simulate the components that wire's backend depends on (s3, cassandra, redis, etc..), we use a series of docker images. These docker images are downloaded from dockerhub, are maintained (or not maintained) by outside parties, and are built by those parties.

When a docker image is built, even if the docker image is something like a java app, or a pile of perl/node/etc, the interpreters (openjdk, node, perl) are embedded into the image. Those interpreters are compiled for a specific processor architecture, and only run on that architecture (and supersets of it). For instance, an AMD64 image will run on only an AMD64 system, but a 386 image will run on AMD64 since AMD64 is a superset of 386. Neither of those images will run on an ARM, like a Raspberry pi.

This Makefile contains rules that allow our Mac users to build all of the docker images locally on their machine, with some minor improvements, which will save us about 2.5G of ram during integration tests. Additionally, it contains rules for uploading these images to dockerhub for others to use, and support for linux users to build images for arm32v5, arm32v7, 386, and AMD64, despite not being on these architectures.

It builds non-AMD64 images on linux by using QEMU, a system emulator, to allow docker to run images that are not built for the architecture the system is currently running on. This is full system emulation, like many video game engines you're probably familiar with. You know how you have to throw gobs of hardware at a machine, to play a game written for a gaming system 20 years ago? This is similarly slow. To work around this, the Makefile is written in a manner that allows us to build many docker images at once, to take advantage of the fact that most of us have many processor cores lying around doing not-all-much, while single-threaded image building is going on.

# What does this get us?

To start with, the resulting docker images allow us to tune the JVM settings on cassandra and elasticsearch, resulting in lower memory consumption, and faster integration tests that don't impact our systems as much. Additionally, it allows us more control of the docker images we're depending on, so that another leftpad incident on docker doesn't impact us. As things stand, any of the developers of these docker images can upload a new docker image that does Very Bad Things(tm), and we'll gladly download and run it many times a day. Building these images ourselves from known good GIT revisions prevents this. Additionally, whe multi-architecture approach allows us to be one step closer to running the backend on more esoteric systems, like a Raspberry pi, or an AWS A instance, both of which are built on the ARM architecture. Or, if rumour is to be believed, the next release of MacBook Pros. :)

# Breaking it down:

## Docker:

to start with, we're going to have to get a bit into some docker architecture. We all have used docker, and pretty much understand the following workflow:

I build a docker image from a Dockerfile and maybe some additions, I upload it to dockerhub, and other people can download and use the image. I can use the locally built image directly, without downloading it from dockerhub, and I can share the Dockerfile and additions via git, on github, and allow others to build the image.

While this workflow works well for working with a single architecture, we're going to have to introduce some new concepts, in order to support the multiple architecture way of building docker files.

### Manifest files.

Manifest files are a docker generated file that contains references to multiple docker images, one for each architecture a given docker image has been built for. Each image in the manifest file is tagged with the architecture that the image is built for.

Docker contains just enough built-in logic to interpret a manifest file on dockerhub, and download an image that matches the architecture that docker was built for. When using a manifest file, this is how docker determines what image to download.

### A Manifest centric Workflow:

If you're building a docker image for multiple architectures, you want a Manifest, so that docker automatically grabs the right image for the user's machine. This changes our workflow from earlier quite a bit:

I build a docker image from a Dockerfile, and i build other images from slightly different versions of this Dockerfile (more on this later). I tag these images with a suffix, so that i can tell them apart. I upload the images to dockerhub, retaining the tags that differentiate the diffenent versions from each other. I create a manifest file, referring to the images that have been pushed to DockerHub, and upload the manifest file to DockerHub. People can download and use the image from dockerhub by refering to the tag of the manifest file. I can share the Dockerfile and additions via git, on dockerhub, and others can build their own images from it.

#### What does this look like?

All of us on the team are using AMD64 based machines, so in this example, we're going to build one image for AMD64, and one for it's predecessor architecture, I386. We're going to build the SMTP server image we depend on, from https://hub.docker.com/r/namshi/smtp. We're going to use a known safe git revision, and use some minor GNU sed to generate architecture dependent Dockerfiles from the Dockerfile in git. Everyone should be able to do this on your laptops.

```bash
$ git clone https://github.com/namshi/docker-smtp.git smtp
Cloning into 'smtp'...
remote: Enumerating objects: 4, done.
remote: Counting objects: 100% (4/4), done.
remote: Compressing objects: 100% (4/4), done.
remote: Total 126 (delta 0), reused 0 (delta 0), pack-reused 122
Receiving objects: 100% (126/126), 26.57 KiB | 269.00 KiB/s, done.
Resolving deltas: 100% (61/61), done.
$ cd smtp
$ git reset --hard 8ad8b849855be2cb6a11d97d332d27ba3e47483f
HEAD is now at 8ad8b84 Merge pull request #48 from zzzsochi/master
$ cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 Julia Longtin \"julia.longtin@wire.com\"/" | sed "s=^\(FROM \)\(.*\)$=\1i386/\2=" > Dockerfile-386
$ cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 Julia Longtin \"julia.longtin@wire.com\"/" | sed "s=^\(FROM \)\(.*\)$=\1\2=" > Dockerfile-amd64
$ docker build -t julialongtin/smtp:0.0.9-amd64 -f Dockerfile-amd64
<docker build output skipped>
$ docker build -t julialongtin/smtp:0.0.9-386 -f Dockerfile-386
<docker build output skipped>
$ docker push julialongtin/smtp:0.0.9-amd64
<docker push output skipped>
$ docker push julialongtin/smtp:0.0.9-386
<docker push output skipped
$ docker manifest create julialongtin/smtp:0.0.9 julialongtin/smtp:0.0.9-386 julialongtin/smtp:0.0.9-amd64 --amend
Created manifest list docker.io/julialongtin/smtp:0.0.9
$ docker manifest annotate julialongtin/smtp:0.0.9 julialongtin/smtp:0.0.9-386 --arch 386
$ docker manifest annotate julialongtin/smtp:0.0.9 julialongtin/smtp:0.0.9-amd64 --arch amd64
$ docker manifest push julialongtin/smtp:0.0.9
sha256:1c98f4e594ae9a08c522706ee450fe5ad01caedebf22e12a173d776c2efebeb9
$
```

That wasn't so bad, was it?

##### what was that SED all about?
The sed commands used above accomplished two things. One, they changed out the MAINTAINER line in the Dockerfile, to indicate that I am the maintainer of this docker image. Two, for the 386 image, it specified that Docker was to start by using the i386 version of debian to base the image off of, not the AMD64 version. we did not need to make that change to the AMD64 version of the Dockerfile, because Docker on our local machine automatically downloads AMD64 images, since our copies of docker were built on AMD64 machines.

##### OK, what was the --amend on the docker manifest create line?
Docker creates manifest files, and stores them in your local docker. I haven't found a good way to remove them, so instead, i add --amend, so that docker changes the local file, instead of just telling you it already exists, and failing.

##### What does a manifest file look like?
to look at a manifest file (local or remote), use 'docker manifest inspect'. for example, here's the original namshi/smtp manifest.

```bash
$ docker manifest inspect namshi/smtp
{
        "schemaVersion": 2,
	"mediaType": "application/vnd.docker.distribution.manifest.v2+json",
	"config": {
	        "mediaType": "application/vnd.docker.container.image.v1+json",
	        "size": 3837,
	        "digest": "sha256:f2dffd734243f5233b3c74808868a4166e3b05c465f6343fb3c8bc30bd72af38"
	},
	"layers": [
	        {
	                "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
	                "size": 54384652,
	                "digest": "sha256:ff42297909573059acb8e981efbc12eff68641f8449f04470a4f218e53a1e80e"
	        },
	        {
	                "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
	                "size": 14757658,
	                "digest": "sha256:bb7eb4b06654d8d568eb13419e3c906c7f0b46779e1ab2a60720922e3d58eea6"
	        },
	        {
	                "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
	                "size": 1146,
	                "digest": "sha256:cf932c8b3d1a80cc099be8684668709ab2eba611d1271f5d5219abcb2213b560"
	        },
	        {
	                "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
	                "size": 424,
	                "digest": "sha256:8adeef74c927855a587f459fd3b3d03e0d75a608d81f53279358b295f2129a62"
	        },
	        {
	                "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
	                "size": 1392,
	                "digest": "sha256:52aa0a29c3081aa439aa279fe650f8d4a99b9aed799786363c926f1268a82c44"
	        }
	]
}
```

This shows us the layers for the image, but says nothing about architecture, as namshi/smtp is shipped only for AMD64. note that there is nothing indicating this is for AMD64, so docker users on other architectures will end up downloading it, trying to run it, and having horrible crashes, instead of the smtp server they are looking for.

Now, let's look at the manifest for the image we just uploaded:
```
docker manifest inspect julialongtin/smtp:0.0.9
{
   "schemaVersion": 2,
   "mediaType": "application/vnd.docker.distribution.manifest.list.v2+json",
   "manifests": [
      {
         "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
         "size": 1364,
         "digest": "sha256:5bb9d121df272f42e126647a28a43235c7d558c72ab6a8e5883095a09d34d84f",
         "platform": {
            "architecture": "386",
            "os": "linux"
         }
      },
      {
         "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
         "size": 1364,
         "digest": "sha256:dac1eb3ddfdd8c55fd403d2fdb157ead268d779a7bbb2b71cf2424e010529cdb",
         "platform": {
            "architecture": "amd64",
            "os": "linux"
         }
      }
   ]
}
```

This is very different. instead of showing layers, it has the SHASUMs of the images to use, in case you are using the architecture in question.

That's it as far as the docker parts of this. Simple, right? :)

### Limits of Manifest files:
I can't figgure out how to delete local manifest files.
I haven't figgured out how to point to local images in a manifest file. this means if we use the name of a manifest in our Docker compose configuration, docker will go out to dockerhub for the image, rather than using a new image we just built, and we have to build a manifest file AFTER the push do dockerhub has been completed.

## QEMU + BinFmt Support:

The previous section has shown us how docker handles multiple architectures, this section is going to cover abusing the BinFmt linux kernel extensions and the QEMU system emulator to allow us to build docker images for non-native architectures, like arm, arm64, ppl64le, etc.

### About QEMU:

QEMU is a full system emulator, and a userspace emulator. QEMU's system emulation means that you can start qemu with disk images, ISOs, etc, for a supported architecture, and it will emulate a whole system around it, showing it's display in a window on your system. We're not going to be using this, and instead use it's userspace emulation.

QEMU's userspace emulation allows you to download a program written for a different processor, and assuming you have the appropriate libraries, DLLs, etc... you can just run it locally. Typically this involves either having a program with no dependencies, or installing a set of system libraries on your machine for the target architecture alongside your current set of libraries.

### About BinFmt Support:
BinFmt support is a set of extensions to the linux kernel that allow you to specify an interpreter for binaries of a certain patern (magic number, ELF header, .EXE header, etc), so that when you attempt to execute them, the kerner  will launch the interpreter of your choice, passing it the path to the binary you tried to execute. On my debian machine, it is used for python by default. Many people use this support for executing windows executables on linux using the WINE package, which contains a re-implementation of the Windows system libraries.

The Linux kernel's BinFmt module can be set to look for an interpreter at run time, or to load the interpreter into memory when it is configured. The packages we're going to set up and exercise in this stage use the "load when you configure" approach. This is useful, so than when you're operating in a docker container, you don't have to place the system emulator in the docker container itsself for the kernel to find it.

### Installing QEMU with BinFmt support:

Debian's qemu-user-static package sets this all up for you.

as root:
```bash
# apt install qemu-user-static
Reading package lists... Done
Building dependency tree
Reading state information... Done
The following NEW packages will be installed:
  qemu-user-static
  0 upgraded, 1 newly installed, 0 to remove and 0 not upgraded.
  Need to get 0 B/21.1 MB of archives.
  After this operation, 211 MB of additional disk space will be used.
  Selecting previously unselected package qemu-user-static.
  (Reading database ... 111963 files and directories currently installed.)
  Preparing to unpack .../qemu-user-static_1%3a3.1+dfsg-4_amd64.deb ...
  Unpacking qemu-user-static (1:3.1+dfsg-4) ...
  Setting up qemu-user-static (1:3.1+dfsg-4) ...
  Processing triggers for man-db (2.8.5-2) ...
#
```


### Verifying it's configuration:

The linux kernel's BinFmt support is configured through a series of 'fake' files, in the /proc/sys/fs/binfmt_misc directory. by default, this directory contains the following:

```
 $ ls -la /proc/sys/fs/binfmt_misc/
total 0
drwxr-xr-x 2 root root 0 Mar  6 09:04 .
dr-xr-xr-x 1 root root 0 Mar  6 09:04 ..
-rw-r--r-- 1 root root 0 Mar  6 09:04 python2.7
-rw-r--r-- 1 root root 0 Mar  6 09:04 python3.7
--w------- 1 root root 0 Mar  6 09:04 register
-rw-r--r-- 1 root root 0 Mar  6 09:04 status
```

After installing qemu-user-static:
```bash
$ ls -la /proc/sys/fs/binfmt_misc/
total 0
drwxr-xr-x 2 root root 0 Mar  6 09:04 .
dr-xr-xr-x 1 root root 0 Mar  6 09:04 ..
-rw-r--r-- 1 root root 0 Mar  6 09:04 python2.7
-rw-r--r-- 1 root root 0 Mar  6 09:04 python3.7
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-aarch64
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-alpha
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-arm
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-armeb
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-cris
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-hppa
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-m68k
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-microblaze
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-mips
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-mips64
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-mips64el
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-mipsel
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-ppc
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-ppc64
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-ppc64abi32
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-ppc64le
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-riscv32
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-riscv64
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-s390x
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-sh4
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-sh4eb
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-sparc
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-sparc32plus
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-sparc64
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-xtensa
-rw-r--r-- 1 root root 0 Mar  6 14:12 qemu-xtensaeb
--w------- 1 root root 0 Mar  6 14:12 register
-rw-r--r-- 1 root root 0 Mar  6 14:12 status
```

This is one entry for each system emulator, along with the 'register' and 'status' files that are used to configure it.

Remember earlier, when we discussed the emulator being loaded when needed, or loaded into ram? this configuration is shown in the 'flags' field in one of these files. for example:

```bash
$ cat /proc/sys/fs/binfmt_misc/qemu-arm
enabled
interpreter /usr/bin/qemu-arm-static
flags: OCF
offset 0
magic 7f454c4601010100000000000000000002002800
mask ffffffffffffff00fffffffffffffffffeffffff
$
```

the 'F' in the flags field of that file indicates we're loading the emulators into ram. note that this file specifies the interpreter by it's full path, and a magic and mask field. these are the values the kernel looks for when executing a file, and if it finds them, launches the interpreter.

### Putting BinFmt and QEMU to work

To test all of this, let's try executing something that's not built for our machine. We're going to try to launch a copy of SASH, the Staticly linked Almquist Shell. Because this is statically linked, we are not going to need to install any system libraries to launch it. We're going to launch an ARM copy, which normally wouldn't think of running on our machines. First, we'll grab a copy with wget.

```bash
$ wget http://ftp.de.debian.org/debian/pool/main/s/sash/sash_3.8-5_armel.deb
--2019-03-06 14:27:39--  http://ftp.de.debian.org/debian/pool/main/s/sash/sash_3.8-5_armel.deb
Resolving ftp.de.debian.org (ftp.de.debian.org)... 141.76.2.4
Connecting to ftp.de.debian.org (ftp.de.debian.org)|141.76.2.4|:80... connected.
HTTP request sent, awaiting response... 200 OK
Length: 277976 (271K) [application/x-debian-package]
Saving to: ‘sash_3.8-5_armel.deb’

sash_3.8-5_armel.deb                                                  100%[========================================================================================================================================================================>] 271.46K  --.-KB/s    in 0.07s

2019-03-06 14:27:39 (3.65 MB/s) - ‘sash_3.8-5_armel.deb’ saved [277976/277976]
$ 
```

This deb will not install on our machine, so we're going to manually take it apart, to get the sash binary out of it.

```bash
$ mkdir tmp
$ cd tmp
$ ar x ../sash_3.8-5_armel.deb
$ ls
 control.tar.xz   data.tar.xz
$ tar -xf data.tar.gz
$ ls -la bin/sash
-rwxr-xr-x 1 demo demo 685348 Jun  9  2018 bin/sash
```

to verify what architecture this binary is built for, use the 'file' command.
```bash
$ file bin/sash
bin/sash: ELF 32-bit LSB executable, ARM, EABI5 version 1 (SYSV), statically linked, for GNU/Linux 3.2.0, BuildID[sha1]=20641a8ca21b2c320ea7e6079ec88b857c7cbcfb, stripped
$
```

now we can run this, and even run Arm64 programs that are on our own machine using it.
```bash
$ bin/sash
Stand-alone shell (version 3.8)
> file bin/sash
bin/sash: ELF 32-bit LSB executable, ARM, EABI5 version 1 (SYSV), statically linked, for GNU/Linux 3.2.0, BuildID[sha1]=20641a8ca21b2c320ea7e6079ec88b857c7cbcfb, stripped
> ls
bin  usr
> uname -a
Linux boxtop 4.9.0-8-amd64 #1 SMP Debian 4.9.144-3 (2019-02-02) x86_64 GNU/Linux
> whoami
demo
>
```

## QEMU, BinFmt, and Docker (Oh my!)

After following the directions in the last two sections, you've created two docker images (one for i386, one for AMD64), created a manifest referring to them, set up for linux to load qemu and use it, and launched a binary for another architecture.

Creating non-native docker images can now be done very similar to how i386 was done earlier.

Because you are using a system emulator, your docker builds for non-x86 will be slower. additionally, the emulators are not perfect, so some images won't build. finally, code is just less tested on machines that are not an AMD64 machine, so there are generally more bugs.

### Arm Complications:
The 32 bit version of arm is actually divided into versions, and not all linux distributions are available for all versions. arm32v5 and arm32v7 are supported by debian, while arm32v6 is supported by alpine. This variant must be specified during manifest construction, so to continue with our current example, these are the commands for tagging the docker images for our arm32v5 and arm32v7 builds of smtp:
```bash
$ docker manifest annotate julialongtin/smtp:0.0.9 julialongtin/smtp:0.0.9-arm32v5 --arch arm  --variant 5
$ docker manifest annotate julialongtin/smtp:0.0.9 julialongtin/smtp:0.0.9-arm32v7 --arch arm  --variant 7
```


# Into the GNU Make Abyss

Now that we've done all of the above, we should be capable of working with docker images independent of the architecture we're targeting. Now, into the rabit hole we go, automating everything with GNU Make

## Why Make?
GNU make is designed to build targets by looking at the environment it's in, and executing a number of rules depending on what it sees, and what it has been requested to do. The Makefile we're going to look through does all of the above, along with making some minor changes to the docker images. It does this in parallel, calling as many of the commands at once as possible, in order to take advantage of idle cores.

## Using the Makefile

Before we take the Makefile apart, let's go over using it.

This Makefile is meant to be used in four ways: building a set of images, pushing (and building) a set of images, building a single image, or building a set of images. It follows the manifest workflow we documented earlier.

By default, running 'make' in the same directory as the Makefile (assuming you've set all of the above up correctly) will attempt to build and push all of the docker images the makefile knows about to dockerhub. If you want this to work, you need to create a dockerhub account, use 'docker login' to log your local instance of docker in to dockerhub, then you need to create a repository for each docker image.

To get a list of the names of the docker images this Makefile knows about, run 'make names'.
```bash
$ make names
Debian based images:
airdock_fakesqs airdock_rvm airdock_base smtp dynamodb_local cassandra
Alpine based images:
elasticsearch java_maven_node_python localstack minio
$
```

The list of names is divided into two groups. one group is for images based on debian, and the other is for images based on alpine. This makefile can only build for one of these two distributions at once.

Since no-one wants to click through dockerhub to create repositories, let's just build docker images locally, for now.

Make looks at it's environment in order to decide what to do, so here are some environment variables that we're going to use. all of these variables have default values, so we're only going to provide a few of them.
'''
ARCHES: the list of architectures we're going to attempt docker builds for. Mac users should supply "386 AMD64" to this, as they have no binfmt support.
DIST: the distribution we're going to build for. this can be either DEBIAN or ALPINE.
DOCKER_USERNAME: our username on dockerhub.
DOCKER_EMAIL: Our email address, as far as dockerhub is concerned.
DOCKER_REALNAME: again, our name string that will be displayed in DockerHub.
SED: which sed binary to use. Mac users should install GSED, and pass the path to it in this variable.
'''

To build all of the debian based images locally on my machine, i run "make DIST=DEBIAN DOCKER_USERNAME=julialongtin DOCKER_EMAIL=julia.longtin@wire.com DOCKER_REALNAME='Julia Longtin' build-all -j".

What's the -j for? adding a '-j' to the command line causes make to execute in parallel. That's to say, it will try to build ALL of the images at once, taking care to build images that are dependencies of other images before building the images that depend on them.

Note that since we are building the images without pushing them to DockerHub, no manifest files are generated.

If we want to use these images in our docker compose, we can edit the docker compose file, and refer to the image we want with it's architecture suffix attached. This will make docker-compose use the local copy, instead of hitting DockerHub, grabbing the manifest, and using an image from there. for instance, to use the local cassandra image I just built, I would edit the docker-compose.yaml file in our wire-server repo, and make the cassandra section look like the following:

```
  cassandra:
    container_name: demo_wire_cassandra
    #image: cassandra:3.11.2
    image: julialongtin/cassandra:0.0.9-amd64
    ports:
      - "127.0.0.1:9042:9042"
    environment:
# what's present in the jvm.options file by default.
#      - "CS_JAVA_OPTIONS=-Xmx1024M -Xms1024M -Xmn200M"
      - "CS_JVM_OPTIONS=-Xmx128M -Xms128M -Xmn50M"
    networks:
      - demo_wire
```

To remove all of the git repositories containing the Dockerfiles we download to build these images, we can run 'make clean'. There is also the option to run 'make cleandocker' to REMOVE ALL OF THE DOCKER IMAGES ON YOUR MACHINE. careful with that one.
Note that docker makes good use of caching, so running 'make clean' and the same make command you used to build the images will complete really fast, as docker does not actually need to rebuild the images.

## Reading through the Makefile

OK, now that we have a handle on what it does, and how to use it, let's get into the Makefile itsself.

A Makefile is a series of rules for performing tasks, variables used when creating those tasks, and some minimal functions and conditional structures.  Rules are implemented as groups of bash commands, where each line is handled by a new bash interpreter. Personally, i think it 'feels functiony', only without the type systems. Like if bash tried to be functional.

### Variables

#### Overrideable Variables
the make language has multiple types of variables and variable assignments. To begin with, let's look at the variables we used in the last step.
```bash
$ cat Makefile | grep "?="
DOCKER_USERNAME ?= wireserver
DOCKER_REALNAME ?= Wire
DOCKER_EMAIL    ?= backend@wire.com
TAGNAME         ?= :0.0.9
DIST            ?= DEBIAN
LOCALARCH     ?= $(call dockerarch,$(LOCALDEBARCH))
  ARCHES ?= $(DEBARCHES)
  NAMES  ?= $(DEBNAMES)
  ARCHES ?= $(ALPINEARCHES)
  NAMES  ?= $(ALPINENAMES)
SED ?= sed
SMTP_COMMIT                ?= 8ad8b849855be2cb6a11d97d332d27ba3e47483f
DYNAMODB_COMMIT            ?= c1eabc28e6d08c91672ff3f1973791bca2e08918
ELASTICSEARCH_COMMIT       ?= 06779bd8db7ab81d6706c8ede9981d815e143ea3
AIRDOCKBASE_COMMIT         ?= 692625c9da3639129361dc6ec4eacf73f444e98d
AIRDOCKRVM_COMMIT          ?= cdc506d68b92fa4ffcc7c32a1fc7560c838b1da9
AIRDOCKFAKESQS_COMMIT      ?= 9547ca5e5b6d7c1b79af53e541f8940df09a495d
JAVAMAVENNODEPYTHON_COMMIT ?= 645af21162fffd736c93ab0047ae736dc6881959
LOCALSTACK_COMMIT          ?= 645af21162fffd736c93ab0047ae736dc6881959
MINIO_COMMIT               ?= 118270d76fc90f1e54cd9510cee9688bd717250b
CASSANDRA_COMMIT           ?= 064fb4e2682bf9c1909e4cb27225fa74862c9086
```

The '?=' assignment operator is used to provide a default value. When earlier, we ran make as "make DIST=DEBIAN DOCKER_USERNAME=julialongtin DOCKER_EMAIL=julia.longtin@wire.com DOCKER_REALNAME='Julia Longtin' build-all -j", we were overriding those values. the Make interpreter will use values provided on the command line, or values we have used 'export' to place into our shell environment.

LOCALARCH and the assignments for ARCHES and NAMES are a bit different. LOCALARCH is a function call, and the ARCHES and NAMES are emdedded in conditional statements. We'll cover those later.

Note the block of COMMIT IDs. This is in case we want to experiment with newer releases of each of the docker images we're using. Fixing what we're using to a commit ID makes it much harder for an upstream source to send us malicious code.

#### Non-Overrideable Variables
The following group of variables use a different assignment operator, that tells make not to look in the environment first.
```bash
$ cat Makefile | grep ":="
USERNAME  := $(DOCKER_USERNAME)
REALNAME  := $(DOCKER_REALNAME)
EMAIL     := $(DOCKER_EMAIL)
STRETCHARCHES := arm32v5 arm32v7 386 amd64 arm64v8 ppc64le s390x
JESSIEARCHES  := arm32v5 arm32v7 386 amd64
DEBARCHES     := arm32v5 arm32v7 386 amd64
JESSIENAMES   := airdock_fakesqs airdock_rvm airdock_base smtp
STRETCHNAMES  := dynamodb_local cassandra
DEBNAMES      :=  $(JESSIENAMES) $(STRETCHNAMES)
ALPINEARCHES  := amd64 386 arm32v6
ALPINENAMES   := elasticsearch java_maven_node_python localstack minio
PREBUILDS     := airdock_rvm-airdock_base airdock_fakesqs-airdock_rvm localstack-java_maven_node_python
NOMANIFEST    := airdock_rvm airdock_fakesqs localstack
LOCALDEBARCH  := $(shell [ ! -z `which dpkg` ] && dpkg --print-architecture)
BADARCHSIM    := localstack-arm32v6 java_maven_node_python-arm32v6 dynamodb_local-386
$
```

The first three variable assignments are referring to other variables. These basically exist as alias, to make our make rules denser later.

STRETCHARCHES and JESSIEARCHES contain the list of architectures that dockerhub's debian stretch and jessie images provide. DEBARCHES defines what architectures we're going to build, for our debian targets. STRETCHARCHES and DEBIANARCHES only exist to make it visible to readers of the Makefile which images CAN be built for which architectures.

JESSIENAMES and STRETCHNAMES are used similarly, only they are actually referred to by DEBNAMES, to provide the list of debian based images that can be built.

ALPINEARCHES and ALPINENAMES work similarly, and are used when we've provided "DIST=ALPINE". We do not divide into seperate variables quite the same way as debian, because all of our alpine images are based on alpine 3.7.

PREBUILDS contains our dependency map. essentially, this is a set of <imagename>-<imagename> pairs, where the first image mentioned depends on the second image. so, airdock_rvm depends on airdock_base, where airdock_fakesqs depends on airdock_rvm, etc.

BADARCH is similar, pairing the name of an image with the architecture it fails to build on. This is so i can blacklist things that don't work yet.

LOCALDEBARCH is a variable set by executing a small snippet of bash. the snippet checks, to make sure dpkg is installed (the debian package manager), and uses dpkg to determine what the architecture of your local machine is. As you remember from when we were building docker images by hand, docker will automatically fetch an image that is compiled for your current architecture, so we use LOCALDEBARCH later to decide what architecture we can skip specifically telling docker to fetch.

NOMANIFEST is a tricky one to explain. You know how we added the name of the architecture BEFORE the image name in the dockerfiles? well, in the case of the dependencies of the images listed here, dockerhub isn't supporting that. DockerHub is supporting that form only for 'official' docker images, like alpine, debian, etc. as a result, in order to fetch an architecture specific version of the dependencies of these images, we need to add a -<arch> suffix. like -386 -arm32v7, etc.

### Conditionals
We don't make much use of conditionals in this makefile. There are three total uses in this Makefile. let's take a look at them.

SED ABUSE:
to get our list of conditionals out of the Makefile, we're going to use some multiline sed. specifically, we're going to look for a line starting with 'ifeq', lines starting with two spaces, then the line following.

```bash
$ cat Makefile | sed -n '/ifeq/{:n;N;s/\n  /\n /;tn;p}'
ifeq ($(LOCALARCH),)
 $(error LOCALARCH is empty, you may need to supply it.)
 endif
ifeq ($(DIST),DEBIAN)
 ARCHES ?= $(DEBARCHES)
 NAMES  ?= $(DEBNAMES)
endif
ifeq ($(DIST),ALPINE)
 ARCHES ?= $(ALPINEARCHES)
 NAMES  ?= $(ALPINENAMES)
endif
$
```
There's a lot to unpack there, so let's start with the simple part, the conditionals.
The conditionals are checking for equivalence, in all cases.
First, we check to see if LOCALARCH is empty. This can happen if dpkg was unavailable, and the user did not supply a value on the make command line or in the user's bash environment. if that happens, we use make's built in error function to display an error, and break out of the Makefile.
The second and third conditionals decide on the values of ARCHES and NAMES. Earlier, we determined the default selection for DIST was DEBIAN, so this pair just allows the user to select ALPINE instead. note that the variable assignments in the conditionals are using the overrideable form, so the end user can override these on make's command line or in the user's environment. mac users will want to do this, since they don't have QEMU available in the same form, and are limited to building X86 and AMD64 architecture.

Note that conditionals are evaluated when the file is read, once. This means that we don't have the ability to use them in our rules, or in our functions, and have to abuse other operations in a manner that should be familiar to you...

Now, back to our sed abuse.
SED is a stream editor, and quite a powerful one. In this case, we're using it for a multi-line search. we're supplying the -n option, which squashes all output, except what sed is told specificly to print something with a command.
Let's look at each of the commands in that statement seperately.
```sed
# find a line that has 'ifeq' in it.
/ifeq/
# begin a block of commands. every command in the block should be seperated by a semicolon.
{
# create an anchor, that is to say, a point that can be branched to.
:n;
# Append the next line into the parameter space. so now, for the first block, the hold parameter space would include "ifeq ($(LOCALARCH),)\n  $(error LOCALARCH is empty, you may need to supply it.)".
N;
# Replace the two spaces in the parameter space with one space.
s/\n  /\n /;
# If the previous 's' command found something, and changed something, go to our label.
tn;
# print the contents of the parameter space.
p
# close the block of commands.
}
```
... Simple, right?

note that the contents above can be stored to a file, and run with sed's "-f" command, for more complicated sed scripts. Sed is turing complete, so... things like tetris have been written in it. My longest sed scripts do things like sanity check OS install procedures, or change binaryish protocols into xmlish forms.

### Functions
Make has a concept of functions, and the first two functions we use are a bit haskell inspired.

SED ABUSE:
To get a list of the functions in our makefile, we're going to use a bit more traditional sed. specifically, we're going to look for lines that start with a number of lowercase characters that are immediately followed by an '=' sign.

```bash
$ cat Makefile | sed -n '/^[a-z]*=/p'
dockerarch=$(patsubst i%,%,$(patsubst armel,arm32v5,$(patsubst armhf,arm32v7,$(patsubst arm64,arm64v8,$(1)))))
fst=$(word 1, $(subst -, ,$(1)))
snd=$(word 2, $(subst -, ,$(1)))
goodarches=$(filter-out $(call snd,$(foreach arch,$(ARCHES),$(filter $(1)-$(arch),$(BADARCHSIM)))),$(ARCHES))
nodeps=$(filter-out $(foreach target,$(NAMES),$(call snd,$(foreach dependency,$(NAMES),$(filter $(target)-$(dependency),$(PREBUILDS))))),$(NAMES))
maniarch=$(patsubst %32,%,$(call fst,$(subst v, ,$(1))))
manivariant=$(foreach variant,$(word 2, $(subst v, ,$(1))), --variant $(variant))
archpostfix=$(foreach arch,$(filter-out $(filter-out $(word 3, $(subst -, ,$(filter $(call snd,$(1))-%-$(call fst,$(1)),$(foreach prebuild,$(PREBUILDS),$(prebuild)-$(call fst,$(1)))))),$(LOCALARCH)),$(call fst,$(1))),-$(arch))
archpath=$(foreach arch,$(patsubst 386,i386,$(filter-out $(LOCALARCH),$(1))),$(arch)/)
$
```

These are going to be a bit hard to explain in order, especially since we haven't covered where they are being called from. Let's take them from simplest to hardest.

The fst and snd functions are what happens when a haskell programmer is writing make. You remember all of the pairs of values earlier, that were seperated by a single '-' character? these functions return either the first, or the second item in the pair. Let's unpack 'fst'.
fst uses the 'word' function of make to retrieve the first word from "$(subst -, ,$(1))". the 'subst' function substitutes a single dash for a single space. this seperates a <item1>-<item2> pair into a space seperated string. $(1) is the first argument passed to this function.
snd works similarly, retrieving <item2> from our pair.

The next easiest to explain function is 'maniarch'. It returns the architecture string that we use when annotating a docker image. When we refer to an architecture, we use a string like 'amd64' or 'arm32v6', but docker manifest wants just 'arm' 'amd64' or '386'.
maniarch first uses the 'patsubst' command to replace "anystring32" with "anystring". this removes the 32 from arm32. It's given the result of $(call fst,$(subst v, ,$(1)))) as a string to work with.
$(call fst,$(subst v, ,$(1)))) calls our 'fst' function, giving it the result of us substituting 'v' for ' ' in the passed in argument. in the case of arm32v6, it seperates the string into "arm32 6". Note that instead of calling fst, we could have just used 'word 1' like we did in fst. This is a mistake on my part, but it works regardless, because of the way fst is built. as before, $(1) is the argument passed into our function.

manivariant has a similar function to maniarch. It's job is to take an architecture name (amd64, arm32v5, etc...), and if it has a 'v<something>', to return the '--variant <something>' command line option for our 'docker manifest anotate'.
manivariant starts by using make's 'foreach' function. this works by breaking it's second argument into words, storing them into the variable name given in the first argument, and then generating text using the third option. this is a bit abusive, as we're really just using it as "if there is a variant, add --variant <variant>" structure.
The first argument of foreach is the name of a variable. we used 'variant' here. the second argument in this case properly uses word, and subst to return only the content after a 'v' in our passed in argument, or emptystring. the third option is ' --variant $(variant)', using the variable defined in the first parameter of foreach to create " --variant 5" if this is passed "arm32v5", for instance.

archpath is similar in structure to manivariant. In order to find a version of a docker image that is appropriate for our non-native architectures, we have to add 'archname/' to the image we're deriving from's path, in our Dockerfile. This function

To summarize the Make functions we've (ab)used in this section:
$(word)
