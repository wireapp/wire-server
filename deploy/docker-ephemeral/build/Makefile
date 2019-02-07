USERNAME ?= julialongtin
REALNAME ?= Julia Longtin
EMAIL    ?= julia.longtin@wire.com
TAGNAME  ?= :0.0.4

# the set of images we're going to build. DEBIAN, or ALPINE.
DIST     ?= DEBIAN

# these are docker archetecture names, not debian.
STRETCHARCHES := arm32v5 arm32v7 386 amd64 arm64v8 ppc64le s390x 
JESSIEARCHES  := arm32v5 arm32v7 386 amd64
# the arches that our images based on debian support.
# note that qemu's x86_64 support is not strong enough to cross-build on i386. Also note that we only care about the pi, and the 386, for now.
DEBARCHES     := arm32v5 arm32v7 386

# the names of the docker images we're building that are based on debian jessie.
JESSIENAMES   := airdock_fakesqs airdock_rvm airdock_base smtp
# the names of the docker images we're building that are based on debian stretch.
STRETCHNAMES  := dynamodb_local
# the names of the docker images that we're building that are based on debian.
DEBNAMES      :=  $(JESSIENAMES) $(STRETCHNAMES)

# the arches that we build for alpine.
ALPINEARCHES  ?= arm32v6 386
# images we build that are based on alpine.
ALPINENAMES    := elasticsearch java_maven_node_python localstack


# set the targets, depending on the distro base specified. this is so that the debian images are built for all of the debian arches, and the alpine images for it's arches.
ifeq ($(DIST),DEBIAN)
  ARCHES=$(DEBARCHES)
  NAMES=$(DEBNAMES)
endif
ifeq ($(DIST),ALPINE)
  ARCHES=$(ALPINEARCHES)
  NAMES=$(ALPINENAMES)
endif

# dependencies between docker images. <first_image>-<image_needed_to_build_first_image>
PREBUILDS := airdock_rvm-airdock_base airdock_fakesqs-airdock_rvm localstack-java_maven_node_python

# manifest files don't work for these when they are finding the image they are based on.
# use the <arch>/image:tag format needs to use image:tag-<arch> instead.
NOMANIFEST := airdock_rvm airdock_fakesqs

# turn on experimental features in docker.
export DOCKER_CLI_EXPERIMENTAL=enabled


# allow for us to (ab)use $$* in dependencies of rules.
.SECONDEXPANSION:

# disable make's default builtin rules, to make debugging output cleaner.
MAKEFLAGS += --no-builtin-rules

# empty out the default suffix list, to make debugging output cleaner.
.SUFFIXES:

# too much haskell. returns first or second from <fst>-<snd>, respectively.
fst=$(word 1, $(subst -, ,$(1)))
snd=$(word 2, $(subst -, ,$(1)))

.PHONY: manifest
manifest: $(foreach name,$(NAMES),manifest-push-$(name))

# manifest-push-<name>
manifest-push-%: $(foreach arch,$(ARCHES),manifest-annotate-$(arch)-$$*)
	docker manifest push $(USERNAME)/$*$(TAGNAME)

#manifest-annotate-<arch>-<name>
manifest-annotate-%: manifest-create-$$(call snd,%)
	docker manifest annotate $(USERNAME)/$(call snd,$*)$(TAGNAME) $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) --arch $(call fst,$*)
# special case for arm, which has bits and variant encoded in the arch name.
manifest-annotate-arm%: manifest-create-$$(word 2, $$(subst -, ,%))
	docker manifest annotate $(USERNAME)/$(call snd,$*)$(TAGNAME) $(USERNAME)/$(call snd,$*)$(TAGNAME)-arm$(call fst,$*) --arch $(patsubst %32,%,$(call fst,$(subst v, ,arm$*))) --variant $(word 2, $(subst v, ,$(call fst,$*))) 

#manifest-create-<name>
manifest-create-%: $(foreach arch,$(ARCHES),upload-$(arch)-$$*)
	docker manifest create $(USERNAME)/$*$(TAGNAME) $(patsubst %,$(USERNAME)/$*$(TAGNAME)-%,$(ARCHES)) --amend

# upload-<arch>-<name>
upload-%: build-%
	docker push $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) | cat

# build-<arch>-<name>
build-%: Dockerfile-$$(foreach target,$$(filter $$(call snd,$$*),$(NOMANIFEST)),NOMANIFEST-)$$* $$(foreach predep,$$(filter $$(call snd,%)-%,$(PREBUILDS)),manifest-push-$$(call snd,$$(predep)))
	cd $(call snd,$*) && docker build -t $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) -f Dockerfile-$(call fst,$*) . | cat

# handle cases where a manifest file is not being respected, and we have to use <name>:<tag>-<arch> format.
# Dockerfile-<arch>-<name>
Dockerfile-NOMANIFEST-386-%: %/Dockerfile
	cd $* && cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | sed "s=^\(FROM .*\)$$=\1-386=" > Dockerfile-386
# special case for arm.
Dockerfile-NOMANIFEST-arm%: $$(call snd,$$*)/Dockerfile
	cd $(call snd,$*) && cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | sed "s=^\(FROM .*\)=\1-arm$(call fst,$*)=" > Dockerfile-arm$(call fst,$*)

# handle situations where a manifest is present in upstream, and available as <arch>/<name>:<tag>
# Dockerfile-<arch>-<name>
Dockerfile-386-%: %/Dockerfile
	cd $* && cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | sed "s=^FROM =FROM i386/=" > Dockerfile-386
# special case for arm.
Dockerfile-arm%: $$(call snd,$$*)/Dockerfile
	cd $(call snd,$*) && cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | sed "s=^FROM =FROM arm$(call fst,$*)/=" > Dockerfile-arm$(call fst,$*)

# real files, finally!

smtp/Dockerfile: 
	git clone git@github.com:namshi/docker-smtp.git smtp

dynamodb_local/Dockerfile:
	git clone git@github.com:cnadiminti/docker-dynamodb-local.git dynamodb_local

elasticsearch/Dockerfile:
	git clone git@github.com:blacktop/docker-elasticsearch-alpine.git elasticsearch-all
	mv elasticsearch-all/5.6/ elasticsearch

airdock_base/Dockerfile:
	git clone git@github.com:airdock-io/docker-base.git airdock_base-all
	cp -R airdock_base-all/jessie airdock_base
	sed -i.bak "s/GOSU_VERSION=.* /GOSU_VERSION=1.11 /" $@

airdock_rvm/Dockerfile:
	git clone git@github.com:airdock-io/docker-rvm.git airdock_rvm-all
	cp -R airdock_rvm-all/jessie-rvm airdock_rvm
	sed -i.bak "s=airdock/base:jessie=$(USERNAME)/airdock_base$(TAGNAME)=" $@
# add a second key used to sign ruby to the dockerfile.
	sed -i.bak "s=\(409B6B1796C275462A1703113804BB82D39DC0E3\)=\1 7D2BAF1CF37B13E2069D6956105BD0E739499BDB=" $@

airdock_fakesqs/Dockerfile:
	git clone git@github.com:airdock-io/docker-fake-sqs.git airdock_fakesqs-all
	cp -R airdock_fakesqs-all/0.3.1 airdock_fakesqs
	sed -i.bak "s=airdock/rvm:latest=$(USERNAME)/airdock_rvm$(TAGNAME)=" $@
	sed -i.bak "s=^USER.*=USER ruby=" $@

java_maven_node_python/Dockerfile:
	git clone git@github.com:localstack/localstack.git java_maven_node_python
	cd java_maven_node_python && mv bin/Dockerfile.base Dockerfile
# disable installing docker-ce.
	sed -i.bak "/.*install Docker.*/{N;N;N;N;N;d}" $@ 
# FIXME: swap out docker-ce.

localstack/Dockerfile:
	git clone git@github.com:localstack/localstack.git localstack
	sed -i.bak "s=localstack/java-maven-node-python=$(USERNAME)/java_maven_node_python$(TAGNAME)=" $@

# cleanup.
.PHONY: clean
clean:
	rm -rf smtp/ dynamodb_local/ elasticsearch elasticsearch-all airdock_base airdock_base-all airdock_rvm airdock_rvm-all airdock_fakesqs airdock_fakesqs-all

.PHONY: cleandocker
cleandocker:
	docker rm $$(docker ps -a -q) || true
	docker rmi $$(docker images -q) --force || true


# note: you must have dbus-launch on debian, which is in package dbus-x11
# or, install pass?
# note: also, add the user you are running as to the docker group, and restart X.

# docker login --username=$(USERNAME)
# start binfmt support for debian. note that amd support is broken.

