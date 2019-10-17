# use DOCKER_ so we allow users to pass in values without conflicting with USERNAME, EMAIL, or somesuch already in their environments.
DOCKER_USERNAME ?= wireserver
DOCKER_REALNAME ?= Wire
DOCKER_EMAIL    ?= backend@wire.com
TAGNAME         ?= :0.0.9

# shorten the variable names above, to make the make rules below a little clearer to read.
USERNAME  := $(DOCKER_USERNAME)
REALNAME  := $(DOCKER_REALNAME)
EMAIL     := $(DOCKER_EMAIL)

# the distribution we're going to build for. this can be either DEBIAN or ALPINE.
DIST            ?= DEBIAN

# these are docker architecture names, not debian.
STRETCHARCHES := arm32v5 arm32v7 386 amd64 arm64v8 ppc64le s390x
JESSIEARCHES  := arm32v5 arm32v7 386 amd64
# the arches that our images based on debian support.
# note that we only care about the pi, the 386, and amd64 for now.
DEBARCHES     := arm32v5 arm32v7 386 amd64

# the names of the docker images we're building that are based on debian jessie.
JESSIENAMES   := airdock_fakesqs airdock_rvm airdock_base smtp
# the names of the docker images we're building that are based on debian stretch.
STRETCHNAMES  := dynamodb_local cassandra
# the names of the docker images that we're building that are based on debian.
DEBNAMES      := $(JESSIENAMES) $(STRETCHNAMES)

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
LOCALDEBARCH  := $(shell [ ! -z `which dpkg` ] && dpkg --print-architecture)
# the local architecture, in docker format. (386, amd64, arm32v5, arm32v7, arm64v8, ...)
LOCALARCH     ?= $(call dockerarch,$(LOCALDEBARCH))

ifeq ($(LOCALARCH),)
  $(error LOCALARCH is empty, you may need to supply it.)
endif

# FIXME: make this a section that depends on LOCALARCH, so we can allow these images to be built on native arm32.
# FIXME: what's up with dynamodb?
# note that qemu's x86_64 support is not strong enough to cross-build most things on i386.
# these targets won't build on the system emulators for these arches. working with the qemu team to fix. they think it might be https://bugs.launchpad.net/qemu/+bug/1813398 .
BADARCHSIM    := localstack-arm32v6 java_maven_node_python-arm32v6 dynamodb_local-386

# set the targets, depending on the distro base specified. this is so that the debian images are built for all of the debian arches, and the alpine images for its arches.
ifeq ($(DIST),DEBIAN)
  ARCHES ?= $(DEBARCHES)
  NAMES  ?= $(DEBNAMES)
endif
ifeq ($(DIST),ALPINE)
  ARCHES ?= $(ALPINEARCHES)
  NAMES  ?= $(ALPINENAMES)
endif

# which sed to use. GNU-SED for macs.
SED ?= sed

# turn on experimental features in docker.
export DOCKER_CLI_EXPERIMENTAL=enabled

# allow for us to (ab)use $$* in dependencies of rules.
.SECONDEXPANSION:

# disable make's default builtin rules, to make debugging output cleaner.
MAKEFLAGS += --no-builtin-rules

# make sure we use bash. for proper quoting when inserting JVM_OPTIONS snippet.
SHELL = bash

# empty out the default suffix list, to make debugging output cleaner.
.SUFFIXES:

# too much haskell. returns first or second from <fst>-<snd>, respectively.
fst=$(word 1, $(subst -, ,$(1)))
snd=$(word 2, $(subst -, ,$(1)))

# filter the list of architectures, removing architectures that we know do not work for a given docker image.
goodarches=$(filter-out $(call snd,$(foreach arch,$(ARCHES),$(filter $(1)-$(arch),$(BADARCHSIM)))),$(ARCHES))
# filter the list of names, returning only names that have no pre-dependencies.
nodeps=$(filter-out $(foreach target,$(NAMES),$(call snd,$(foreach dependency,$(NAMES),$(filter $(target)-$(dependency),$(PREBUILDS))))),$(NAMES))

# the three entry points we expect users to use. all by default, to create and upload either debian or alpine images, build-<name>, to build a single image (for all arches, but without the manifest), push-<name> to build a single image, push the image, build it's manifest, and push it to dockerhub.
all: $(foreach image,$(nodeps),manifest-push-$(image))

# build-<name>
build-%: $$(foreach arch,$$(call goodarches,%),create-$$(arch)-$$*)
	@echo -n

.PHONY: build-all
build-all: $(foreach image,$(nodeps),build-$(image))

# push-<name>
push-%: manifest-push-%
	@echo -n

.PHONY:
push-all: $(foreach image,$(nodeps),manifest-push-$(image))

# manifests use a slightly different form of architecture name than docker itsself. arm instead of arm32, and a seperate variant field.
maniarch=$(patsubst %32,%,$(call fst,$(subst v, ,$(1))))
# seperate and use the variant, if it is part of the architecture name.
manivariant=$(foreach variant,$(word 2, $(subst v, ,$(1))), --variant $(variant))

# manifest-push-<name>
manifest-push-%: $$(foreach arch,$$(call goodarches,$$*), manifest-annotate-$$(arch)-$$*)
	docker manifest push $(USERNAME)/$*$(TAGNAME)

#manifest-annotate-<arch>-<name>
manifest-annotate-%: manifest-create-$$(call snd,$$*)
	docker manifest annotate $(USERNAME)/$(call snd,$*)$(TAGNAME) $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) --arch $(call maniarch,$(call fst,$*)) $(call manivariant,$(call fst,$*))

#manifest-create-<name>
manifest-create-%: $$(foreach arch,$$(call goodarches,%), upload-$$(arch)-$$*)
	docker manifest create $(USERNAME)/$*$(TAGNAME) $(patsubst %,$(USERNAME)/$*$(TAGNAME)-%,$(call goodarches,$*)) --amend

# upload-<arch>-<name>
upload-%: create-% $$(foreach predep,$$(filter $$(call snd,%)-%,$$(PREBUILDS)), dep-upload-$$(call fst,$$*)-$$(call snd,$$(predep)))
	docker push $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) | cat

dep-upload-%: create-% $$(foreach predep,$$(filter $$(call snd,%)-%,$$(PREBUILDS)), dep-subupload-$$(call fst,$$*)-$$(call snd,$$(predep)))
	docker push $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) | cat

dep-subupload-%: create-%
	docker push $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) | cat

# create-<arch>-<name>
create-%: Dockerfile-$$(foreach target,$$(filter $$(call snd,$$*),$(NOMANIFEST)),NOMANIFEST-)$$* $$(foreach predep,$$(filter $$(call snd,%)-%,$(PREBUILDS)), depend-create-$$(call fst,$$*)-$$(call snd,$$(predep)))
	cd $(call snd,$*) && docker build -t $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) -f Dockerfile-$(call fst,$*) . | cat

depend-create-%: Dockerfile-$$(foreach target,$$(filter $$(call snd,$$*),$(NOMANIFEST)),NOMANIFEST-)$$* $$(foreach predep,$$(filter $$(call snd,%)-%,$(PREBUILDS)), depend-subcreate-$$(call fst,$$*)-$$(call snd,$$(predep)))
	cd $(call snd,$*) && docker build -t $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) -f Dockerfile-$(call fst,$*) . | cat

depend-subcreate-%: Dockerfile-$$(foreach target,$$(filter $$(call snd,$$*),$(NOMANIFEST)),NOMANIFEST-)$$*
	cd $(call snd,$*) && docker build -t $(USERNAME)/$(call snd,$*)$(TAGNAME)-$(call fst,$*) -f Dockerfile-$(call fst,$*) . | cat

# with a broken manifest(our images, either docker or local), we have to use a postfix to request docker images other than the one for our native architecture.
archpostfix=$(foreach arch,$(filter-out $(filter-out $(word 3, $(subst -, ,$(filter $(call snd,$(1))-%-$(call fst,$(1)),$(foreach prebuild,$(PREBUILDS),$(prebuild)-$(call fst,$(1)))))),$(LOCALARCH)),$(call fst,$(1))),-$(arch))
# with working manifest (official images from docker built correctry), we have to use a path when requesting docker images other than the one for our native architecture.
archpath=$(foreach arch,$(patsubst 386,i386,$(filter-out $(LOCALARCH),$(1))),$(arch)/)

# handle cases where a manifest file is not being respected, and we have to use <name>:<tag>-<arch> format.
# Dockerfile-NOMANIFEST-<arch>-<name>
Dockerfile-NOMANIFEST-%: $$(call snd,%)/Dockerfile
	cd $(call snd,$*) && cat Dockerfile | ${SED} "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | ${SED} "s=^\(FROM \)\(.*\)$$=\1\2$(call archpostfix,$*)=" > Dockerfile-$(call fst,$*)

# handle situations where a manifest is present in upstream, and available as <arch>/<name>:<tag>
# Dockerfile-<arch>-<name>
Dockerfile-%: $$(call snd,%)/Dockerfile
	cd $(call snd,$*) && cat Dockerfile | ${SED} "s/^\(MAINTAINER\).*/\1 $(REALNAME) \"$(EMAIL)\"/" | ${SED} "s=^\(FROM \)\(.*\)$$=\1$(call archpath,$(call fst,$*))\2=" > Dockerfile-$(call fst,$*)

# real files, finally!

# define commit IDs for the versions we're using.
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

smtp/Dockerfile:
	git clone https://github.com/namshi/docker-smtp.git smtp
	cd smtp && git reset --hard $(SMTP_COMMIT)

dynamodb_local/Dockerfile:
	git clone https://github.com/cnadiminti/docker-dynamodb-local.git dynamodb_local
	cd dynamodb_local && git reset --hard $(DYNAMODB_COMMIT)

elasticsearch/Dockerfile:
	git clone https://github.com/blacktop/docker-elasticsearch-alpine.git elasticsearch-all
	cd elasticsearch-all && git reset --hard $(ELASTICSEARCH_COMMIT)
	cp -R elasticsearch-all/5.6/ elasticsearch
	 # add a block to the entrypoint script to interpret CS_JVM_OPTIONS, modifying the jvm.options before launching elasticsearch.
	 # first, add a marker to be replaced before the last if.
	${SED} -i.bak  -r ':a;$$!{N;ba};s/^(.*)(\n?)fi/\2\1fi\nREPLACEME/' elasticsearch/elastic-entrypoint.sh
	 # next, load our variables.
	${SED} -i.bak  's@REPLACEME@MY_APP_CONFIG="/usr/share/elasticsearch/config/"\n&@' elasticsearch/elastic-entrypoint.sh
	 # add our parser and replacer.
	${SED} -i.bak  $$'s@REPLACEME@if [ ! -z "$${JVM_OPTIONS_ES}" ]; then\\nfor x in $${JVM_OPTIONS_ES}; do { l="$${x%%=*}"; r=""; e=""; [ "$$x" != "$${x/=//}" ] \&\& e="=" \&\& r="$${x##*=}"; [ "$$x" != "$${x##-Xm?}" ] \&\& r="$${x##-Xm?}" \&\& l="$${x%%$$r}"; echo $$l $$e $$r; sed -i.bak -r \'s/^[# ]?(\'"$$l$$e"\').*/\\\\1\'"$$r"\'/\' "$$MY_APP_CONFIG/jvm.options"; diff "$$MY_APP_CONFIG/jvm.options.bak" "$$MY_APP_CONFIG/jvm.options" \&\& echo "no difference"; } done;\\nfi\\n&@' elasticsearch/elastic-entrypoint.sh
	 # remove the marker we added earlier.
	${SED} -i.bak  's@REPLACEME@@' elasticsearch/elastic-entrypoint.sh

airdock_base/Dockerfile:
	git clone https://github.com/airdock-io/docker-base.git airdock_base-all
	cd airdock_base-all && git reset --hard $(AIRDOCKBASE_COMMIT)
	cp -R airdock_base-all/jessie airdock_base
	 # work around go compiler bug by using newer version of GOSU. https://bugs.launchpad.net/qemu/+bug/1696353
	${SED} -i.bak  "s/GOSU_VERSION=.* /GOSU_VERSION=1.11 /" $@
	 # work around missing architecture specific binaries in earlier versions of tini.
	${SED} -i.bak  "s/TINI_VERSION=.*/TINI_VERSION=v0.16.1/" $@
	 # work around the lack of architecture usage when downloading tini binaries. https://github.com/airdock-io/docker-base/issues/8
	${SED} -i.bak  's/tini\(.asc\|\)"/tini-\$$dpkgArch\1"/' $@

airdock_rvm/Dockerfile:
	git clone https://github.com/airdock-io/docker-rvm.git airdock_rvm-all
	cd airdock_rvm-all && git reset --hard $(AIRDOCKRVM_COMMIT)
	cp -R airdock_rvm-all/jessie-rvm airdock_rvm
	${SED} -i.bak  "s=airdock/base:jessie=$(USERNAME)/airdock_base$(TAGNAME)=" $@
	 # add a second key used to sign ruby to the dockerfile. https://github.com/airdock-io/docker-rvm/issues/1
	${SED} -i.bak  "s=\(409B6B1796C275462A1703113804BB82D39DC0E3\)=\1 7D2BAF1CF37B13E2069D6956105BD0E739499BDB=" $@

airdock_fakesqs/Dockerfile:
	git clone https://github.com/airdock-io/docker-fake-sqs.git airdock_fakesqs-all
	cd airdock_fakesqs-all && git reset --hard $(AIRDOCKFAKESQS_COMMIT)
	cp -R airdock_fakesqs-all/0.3.1 airdock_fakesqs
	${SED} -i.bak  "s=airdock/rvm:latest=$(USERNAME)/airdock_rvm$(TAGNAME)=" $@
	 # add a workdir declaration to the final switch to root.
	${SED} -i.bak  "s=^USER root=USER root\nWORKDIR /=" $@
	 # break directory creation into two pieces, one run by root.
	${SED} -i.bak  "s=^USER ruby=USER root=" $@
	${SED} -i.bak  "s=cd /srv/ruby/fake-sqs.*=chown ruby.ruby /srv/ruby/fake-sqs\nUSER ruby\nWORKDIR /srv/ruby/fake-sqs\nRUN cd /srv/ruby/fake-sqs \&\& \\\\=" $@

java_maven_node_python/Dockerfile:
	git clone https://github.com/localstack/localstack.git java_maven_node_python
	cd java_maven_node_python && git reset --hard $(JAVAMAVENNODEPYTHON_COMMIT)
	cd java_maven_node_python && mv bin/Dockerfile.base Dockerfile
	 # disable installing docker-ce. not available on many architectures in binary form.
	${SED} -i.bak  "/.*install Docker.*/{N;N;N;N;N;d}" $@

localstack/Dockerfile:
	git clone https://github.com/localstack/localstack.git localstack
	cd localstack && git reset --hard $(LOCALSTACK_COMMIT)
	${SED} -i.bak  "s=localstack/java-maven-node-python=$(USERNAME)/java_maven_node_python$(TAGNAME)=" $@
	 # skip tests. they take too long.
	${SED} -i.bak  "s=make lint.*=make lint=" localstack/Makefile
	${SED} -i.bak  "s=\(.*lambda.*\)=#\1=" localstack/Makefile

minio/Dockerfile:
	git clone https://github.com/minio/minio.git minio
	cd minio && git reset --hard $(MINIO_COMMIT)

cassandra/Dockerfile:
	git clone https://github.com/docker-library/cassandra.git cassandra-all
	cd cassandra-all && git reset --hard $(CASSANDRA_COMMIT)
	cp -R cassandra-all/3.11 cassandra
	 # work around go compiler bug by using newer version of GOSU. https://bugs.launchpad.net/qemu/+bug/1696353
	${SED} -i.bak  "s/GOSU_VERSION .*/GOSU_VERSION 1.11/" $@
	 # add a block to the entrypoint script to interpret CS_JVM_OPTIONS, modifying the jvm.options before launching cassandra.
	 # first, add a marker to be replaced before the last if.
	${SED} -i.bak  -r ':a;$$!{N;ba};s/^(.*)(\n?)fi/\2\1REPLACEME\nfi/' cassandra/docker-entrypoint.sh
	 # next, load our variables.
	${SED} -i.bak  's/REPLACEME/\nAPP_CONFIG="$$CASSANDRA_CONFIG"\n&/' cassandra/docker-entrypoint.sh
	${SED} -i.bak  's/REPLACEME/JVM_OPTIONS="$$CS_JVM_OPTIONS"\n&/' cassandra/docker-entrypoint.sh
	 # add our parser and replacer.
	${SED} -i.bak  $$'s@REPLACEME@if [ ! -z "$${JVM_OPTIONS}" ]; then\\nfor x in $${JVM_OPTIONS}; do { l="$${x%%=*}"; r=""; e=""; [ "$$x" != "$${x/=//}" ] \&\& e="=" \&\& r="$${x##*=}"; [ "$$x" != "$${x##-Xm?}" ] \&\& r="$${x##-Xm?}" \&\& l="$${x%%$$r}"; echo $$l $$e $$r; _sed-in-place "$$APP_CONFIG/jvm.options" -r \'s/^[# ]*(\'"$$l$$e"\').*/\\\\1\'"$$r"\'/\'; } done\\nfi\\n&@' cassandra/docker-entrypoint.sh
	 # remove the marker we added earlier.
	${SED} -i.bak  's@REPLACEME@@' cassandra/docker-entrypoint.sh

# cleanup. remove the directories we set up for building, as well as the git repos we download.
.PHONY: clean
clean:
	rm -rf elasticsearch-all airdock_base-all airdock_rvm-all airdock_fakesqs-all cassandra-all $(DEBNAMES) $(ALPINENAMES)

.PHONY: cleandocker
cleandocker:
	docker rm $$(docker ps -a -q) || true
	docker rmi $$(docker images -q) --force || true

names:
	@echo Debian based images:
	@echo $(DEBNAMES)
	@echo Alpine based images:
	@echo $(ALPINENAMES)
