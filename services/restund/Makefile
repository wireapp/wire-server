LANG := en_US.UTF-8
SHELL           := /usr/bin/env bash

BUILD_NUMBER    ?= 0

RE_VERSION      := 0.4.15
RE_TARBALL      := re-$(RE_VERSION).tar.gz
RE_SRC          := src/re-$(RE_VERSION)/Makefile
RE_URL          := https://github.com/creytiv/re/archive

RESTUND_VERSION := 0.4.14
RESTUND_TARBALL := restund-$(RESTUND_VERSION).tar.gz
RESTUND_SRC     := src/restund-$(RESTUND_VERSION)/Makefile
RESTUND_URL     := https://github.com/wireapp/restund/archive

WIRE_VERSION    := 7

EXTRA_MODULES   := zrest drain

DOCKER          ?= true
DOCKER_USER     ?= quay.io/wire
IMAGE_VERSION   := $(RESTUND_VERSION)w$(WIRE_VERSION)b$(BUILD_NUMBER)
ACI             := restund-$(IMAGE_VERSION)_linux_amd64.aci

ACI_PORTS       := udp,protocol=udp,port=1024,count=64411,socketActivated=false
ACI_PORTS       += tcp,protocol=tcp,port=3478,count=1,socketActivated=false
ACI_PORTS       += tls,protocol=tcp,port=5349,count=1,socketActivated=false
ACI_PORTS       += http,protocol=tcp,port=8080,count=1,socketActivated=false

space           := $() $()


default: dist

.PHONY: dist
dist: clean $(ACI)

.PHONY: build
ifeq ($(DOCKER),true)
build: build-docker
else
build: $(RESTUND_SRC) $(RE_SRC) extra_modules
	$(MAKE) -C src/re-$(RE_VERSION) RELEASE=1
	$(MAKE) -C src/re-$(RE_VERSION) PREFIX=/usr/local install
	$(MAKE) -C src/restund-$(RESTUND_VERSION) RELEASE=1 EXTRA_MODULES='$(EXTRA_MODULES)'
	$(MAKE) -C src/restund-$(RESTUND_VERSION) RELEASE=1 EXTRA_MODULES='$(EXTRA_MODULES)' PREFIX=/usr/local install
	ldconfig
endif

.PHONY: image
image: $(ACI)

.PHONY: publish
publish: $(ACI) $(ACI).asc

.PHONY: clean
clean:
	rm -rf src
	rm -f *.docker *.aci *.asc

%.asc:
	gpg --armor --yes --output $*.asc --detach-sig $*

.PHONY: concourse
concourse: $(RESTUND_SRC) $(RE_SRC) extra_modules
	printf '{"re_version":"%s","restund_version":"%s"}\n' \
		$(RE_VERSION) $(RESTUND_VERSION) > build_args.json
	echo $(IMAGE_VERSION) > tag.txt

.PHONY: build-docker
build-docker: $(RESTUND_SRC) $(RE_SRC) extra_modules
	docker build -t $(DOCKER_USER)/restund:$(IMAGE_VERSION) \
		--build-arg re_version=$(RE_VERSION) \
		--build-arg restund_version=$(RESTUND_VERSION) \
		--build-arg extra_modules="$(EXTRA_MODULES)" \
		.

$(ACI): build-docker
	docker save -o restund-$(IMAGE_VERSION).docker \
		$(DOCKER_USER)/restund:$(IMAGE_VERSION)
	docker2aci -debug restund-$(IMAGE_VERSION).docker
	actool -debug patch-manifest -overwrite \
		--ports='$(subst $(space),:,$(ACI_PORTS))' \
		restund-$(IMAGE_VERSION).aci $(ACI)
	rm restund-$(IMAGE_VERSION).aci restund-$(IMAGE_VERSION).docker

packages:
	mkdir -p packages

packages/$(RE_TARBALL): packages
	curl -sSLf $(RE_URL)/v$(RE_VERSION).tar.gz -o packages/$(RE_TARBALL)

packages/$(RESTUND_TARBALL): packages
	curl -sSLf $(RESTUND_URL)/v$(RESTUND_VERSION).tar.gz -o packages/$(RESTUND_TARBALL)

.PHONY: tarballs
tarballs: packages/$(RE_TARBALL) packages/$(RESTUND_TARBALL)

src:
	mkdir -p src

$(RE_SRC): packages/$(RE_TARBALL) | src
	tar -C src -xmvf $(CURDIR)/packages/$(RE_TARBALL)

$(RESTUND_SRC): packages/$(RESTUND_TARBALL) | src
	tar -C src -xmvf $(CURDIR)/packages/$(RESTUND_TARBALL)

src/restund-$(RESTUND_VERSION)/modules/%: $(RESTUND_SRC)
	cp -r $(CURDIR)/modules/$* src/restund-$(RESTUND_VERSION)/modules

.PHONY: extra_modules
extra_modules: $(addprefix src/restund-$(RESTUND_VERSION)/modules/,$(EXTRA_MODULES))
