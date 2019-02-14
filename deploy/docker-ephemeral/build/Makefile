# use DOCKER_ so we allow users to pass in values without conflicting with USERNAME, EMAIL, or somesuch already in their environments.
DOCKER_USERNAME ?= wireserver
DOCKER_REALNAME ?= Wire
DOCKER_EMAIL    ?= backend@wire.com
TAGNAME         ?= :0.0.7

# shorten the variable names above, to make the make rules below a little clearer to read.
USERNAME  := $(DOCKER_USERNAME)
REALNAME  := $(DOCKER_REALNAME)
EMAIL     := $(DOCKER_EMAIL)

# the set of images we're going to build. DEBIAN, or ALPINE.
DIST            ?= DEBIAN

# these are docker archetecture names, not debian.
STRETCHARCHES := arm32v5 arm32v7 386 amd64 arm64v8 ppc64le s390x 
JESSIEARCHES  := arm32v5 arm32v7 386 amd64
# the arches that our images based on debian support.
# note that we only care about the pi, the 386, and amd64 for now.
DEBARCHES     := arm32v5 arm32v7 386 amd64

# the names of the docker images we're building that are based on debian jessie.
JESSIENAMES   := airdock_fakesqs airdock_rvm airdock_base smtp
# the names of the docker images we're building that are based on debian stretch.
STRETCHNAMES  := dynamodb_local
# the names of the docker images that we're building that are based on debian.
DEBNAMES      :=  $(JESSIENAMES) $(STRETCHNAMES)

# the arches that we build for alpine.
ALPINEARCHES  := amd64 386 arm32v6
# images we build that are based on alpine.
ALPINENAMES   := elasticsearch java_maven_node_python localstack minio

# dependencies between docker images. <first_image>-<image_needed_to_build_first_image>
PREBUILDS     := airdock_rvm-airdock_base airdock_fakesqs-airdock_rvm localstack-java_maven_node_python

# manifest files don't work for these when they are finding the image they are based on.
# by adding the name of the docker image here, we use the image:tag-<arch> format, instead of <arch>/image:tag.
NOMANIFEST    := airdock_rvm airdock_fakesqs localstack

# convert from debian architecture string to docker architecture string. 
dockerarch=$(patsubst i%,%,$(patsubst armel,arm32v5,$(patsubst armhf,arm32v7,$(patsubst arm64,arm64v8,$(1)))))

# the local architecture, in debian format. (i386, amd64, armel, armhf, arm64, ..)
LOCALDEBARCH  := $(shell dpkg --print-architecture)
# the local architecture, in docker format. (386, amd64, arm32v5, arm32v7, arm64v8, ...)
LOCALARCH     ?= $(call dockerarch,$(LOCALDEBARCH))

ifeq ($(LOCALARCH),)
  $(error LOCALARCH is empty, you may need to supply it.)
endif

# FIXME: make this a section that depends on LOCALARCH, so we can allow these images to be built on native arm32.
# note that qemu's x86_64 support is not strong enough to cross-build most things on i386.
# these targets won't build on the system emulators for these arches. working with the qemu team to fix. they think it might be https://bugs.launchpad.net/qemu/+bug/1813398 .
BADARCHSIM    := localstack-arm32v6 java_maven_node_python-arm32v6

# set the targets, depending on the distro base specified. this is so that the debian images are built for all of the debian arches, and the alpine images for it's arches.
ifeq ($(DIST),DEBIAN)
  ARCHES=$(DEBARCHES)
  NAMES=$(DEBNAMES)
endif
ifeq ($(DIST),ALPINE)
  ARCHES=$(ALPINEARCHES)
  NAMES=$(ALPINENAMES)
endif

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

# filter the list of archetectures, removing archetectures that we know do not work for a given docker image.
goodarches=$(filter-out $(call snd,$(foreach arch,$(ARCHES),$(filter $(1)-$(arch),$(BADARCHSIM)))),$(ARCHES))

.PHONY: manifest
manifest: $(foreach name,$(NAMES),manifest-push-$(name))

# manifests use a slightly different form of architecture name than docker itsself. arm instead of arm32, and a seperate variant field.
maniarch=$(patsubst %32,%,$(call fst,$(subst v, ,$(1))))
# seperate and use the variant, if it is part of the architecture name.
manivariant=$(foreach variant,$(word 2, $(subst v, ,$(1))), --variant $(variant))

# manifest-push-<name>
manifest-push-%: $$(foreach arch,$$(call goodarches,%),manifest-annotate-$$(arch)-$$*)
	docker manifest push $(USERNAME)/$*$(TAGNAME)

#manifest-annotate-<arch>-<name>
manifest-annotate-%: manifest-create-$$(call snd,%)
	docker manifest annotate $(USERNAME)/$(call snd,$*)$(TAGNAME) $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) --arch $(call maniarch,$(call fst,$*)) $(call manivariant,$(call fst,$*))

#manifest-create-<name>
manifest-create-%: $$(foreach arch,$$(call goodarches,%),upload-$$(arch)-$$*)
	docker manifest create $(USERNAME)/$*$(TAGNAME) $(patsubst %,$(USERNAME)/$*$(TAGNAME)-%,$(call goodarches,$*)) --amend

# upload-<arch>-<name>
upload-%: build-%
	docker push $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) | cat

# build-<arch>-<name>
build-%: Dockerfile-$$(foreach target,$$(filter $$(call snd,$$*),$(NOMANIFEST)),NOMANIFEST-)$$* $$(foreach predep,$$(filter $$(call snd,%)-%,$(PREBUILDS)),manifest-push-$$(call snd,$$(predep)))
	cd $(call snd,$*) && docker build -t $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) -f Dockerfile-$(call fst,$*) . | cat


# with a broken manifest, we have to use a postfix to request docker images other than the one for our native architecture.
archpostfix=$(foreach arch,$(filter-out $(LOCALARCH),$(1)),-$(arch))
# with working manifest, we have to use a path when requesting docker images other than the one for our native architecture.
archpath=$(foreach arch,$(patsubst 386,i386,$(filter-out $(LOCALARCH),$(1))),$(arch)/)

# handle cases where a manifest file is not being respected, and we have to use <name>:<tag>-<arch> format.
# Dockerfile-NOMANIFEST-<arch>-<name>
Dockerfile-NOMANIFEST-%: $$(call snd,$$*)/Dockerfile
	cd $(call snd,$*) && cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | sed "s=^\(FROM \)\(.*\)$$=\1\2$(call archpostfix,$(call fst,$*))=" > Dockerfile-$(call fst,$*)

# handle situations where a manifest is present in upstream, and available as <arch>/<name>:<tag>
# Dockerfile-<arch>-<name>
Dockerfile-%: $$(call snd,$$*)/Dockerfile
	cd $(call snd,$*) && cat Dockerfile | sed "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | sed "s=^\(FROM \)\(.*\)$$=\1$(call archpath,$(call fst,$*))\2=" > Dockerfile-$(call fst,$*)

# real files, finally!

# define commit IDs for the versions we're using.
SMTP_COMMIT                = 8ad8b849855be2cb6a11d97d332d27ba3e47483f
DYNAMODB_COMMIT            = c1eabc28e6d08c91672ff3f1973791bca2e08918
ELASTICSEARCH_COMMIT       = 06779bd8db7ab81d6706c8ede9981d815e143ea3
AIRDOCKBASE_COMMIT         = 692625c9da3639129361dc6ec4eacf73f444e98d
AIRDOCKRVM_COMMIT          = cdc506d68b92fa4ffcc7c32a1fc7560c838b1da9
AIRDOCKFAKESQS_COMMIT      = 9547ca5e5b6d7c1b79af53e541f8940df09a495d
JAVAMAVENNODEPYTHON_COMMIT = 645af21162fffd736c93ab0047ae736dc6881959
LOCALSTACK_COMMIT          = 645af21162fffd736c93ab0047ae736dc6881959
MINIO_COMMIT               = 118270d76fc90f1e54cd9510cee9688bd717250b

smtp/Dockerfile: 
	git clone git@github.com:namshi/docker-smtp.git smtp
	cd smtp && git reset --hard $(SMTP_COMMIT)

dynamodb_local/Dockerfile:
	git clone git@github.com:cnadiminti/docker-dynamodb-local.git dynamodb_local
	cd dynamodb_local && git reset --hard $(DYNAMODB_COMMIT)

elasticsearch/Dockerfile:
	git clone git@github.com:blacktop/docker-elasticsearch-alpine.git elasticsearch-all
	cd elasticsearch-all && git reset --hard $(ELASTICSEARCH_COMMIT)
	cp -R elasticsearch-all/5.6/ elasticsearch

airdock_base/Dockerfile:
	git clone git@github.com:airdock-io/docker-base.git airdock_base-all
	cd airdock_base-all && git reset --hard $(AIRDOCKBASE_COMMIT)
	cp -R airdock_base-all/jessie airdock_base
# work around go compiler bug by using newer version of GOSU. https://bugs.launchpad.net/qemu/+bug/1696353
	sed -i.bak "s/GOSU_VERSION=.* /GOSU_VERSION=1.11 /" $@
# work around missing architecture specific binaries in earlier versions of tini.
	sed -i.bak "s/TINI_VERSION=.*/TINI_VERSION=v0.16.1/" $@
# work around the lack of architecture usage when downloading tini binaries. https://github.com/airdock-io/docker-base/issues/8
	sed -i.bak 's/tini\(.asc\|\)"/tini-\$$dpkgArch\1"/' $@

airdock_rvm/Dockerfile:
	git clone git@github.com:airdock-io/docker-rvm.git airdock_rvm-all
	cd airdock_rvm-all && git reset --hard $(AIRDOCKRVM_COMMIT)
	cp -R airdock_rvm-all/jessie-rvm airdock_rvm
	sed -i.bak "s=airdock/base:jessie=$(USERNAME)/airdock_base$(TAGNAME)=" $@
# add a second key used to sign ruby to the dockerfile. https://github.com/airdock-io/docker-rvm/issues/1
	sed -i.bak "s=\(409B6B1796C275462A1703113804BB82D39DC0E3\)=\1 7D2BAF1CF37B13E2069D6956105BD0E739499BDB=" $@

airdock_fakesqs/Dockerfile:
	git clone git@github.com:airdock-io/docker-fake-sqs.git airdock_fakesqs-all
	cd airdock_fakesqs-all && git reset --hard $(AIRDOCKFAKESQS_COMMIT)
	cp -R airdock_fakesqs-all/0.3.1 airdock_fakesqs
	sed -i.bak "s=airdock/rvm:latest=$(USERNAME)/airdock_rvm$(TAGNAME)=" $@
# add a workdir declaration to the final switch to root.
	sed -i.bak "s=^USER root=USER root\nWORKDIR /=" $@
# break directory creation into two pieces, one run by root.
	sed -i.bak "s=^USER ruby=USER root=" $@
	sed -i.bak "s=cd /srv/ruby/fake-sqs.*=chown ruby.ruby /srv/ruby/fake-sqs\nUSER ruby\nWORKDIR /srv/ruby/fake-sqs\nRUN cd /srv/ruby/fake-sqs \&\& \\\\=" $@

java_maven_node_python/Dockerfile:
	git clone git@github.com:localstack/localstack.git java_maven_node_python
	cd java_maven_node_python && git reset --hard $(JAVAMAVENNODEPYTHON_COMMIT)
	cd java_maven_node_python && mv bin/Dockerfile.base Dockerfile
# disable installing docker-ce. not available on many archetectures in binary form.
	sed -i.bak "/.*install Docker.*/{N;N;N;N;N;d}" $@

localstack/Dockerfile:
	git clone git@github.com:localstack/localstack.git localstack
	cd localstack && git reset --hard $(LOCALSTACK_COMMIT)
	sed -i.bak "s=localstack/java-maven-node-python=$(USERNAME)/java_maven_node_python$(TAGNAME)=" $@
# skip tests. they take too long.
	sed -i.bak "s=make lint.*=make lint=" localstack/Makefile
	sed -i.bak "s=\(.*lambda.*\)=#\1=" localstack/Makefile

minio/Dockerfile:
	git clone git@github.com:minio/minio.git minio
	cd minio && git reset --hard $(MINIO_COMMIT)

# cleanup. remove the directories we set up for building, as well as the git repos we download. 
.PHONY: clean
clean:
	rm -rf elasticsearch-all airdock_base-all airdock_rvm-all airdock_fakesqs-all $(DEBNAMES) $(ALPINENAMES)

.PHONY: cleandocker
cleandocker:
	docker rm $$(docker ps -a -q) || true
	docker rmi $$(docker images -q) --force || true

names:
	@echo $(DEBNAMES) $(ALPINENAMES)
