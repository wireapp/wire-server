SHELL        := /usr/bin/env bash
SRC          := Main.hs
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(CURDIR)/rex.cabal)
BUILD_NUMBER ?= 0
DOCKER       ?= true
ACI          := rex-$(VERSION)b$(BUILD_NUMBER)_linux_amd64.aci

include ../../mk/publish.mk


default: dist

.PHONY: dist
dist: clean $(ACI)

.PHONY: build
build: rex

.PHONY: publish
publish: $(ACI) $(ACI).asc s3-exists/$(ACI) s3-upload/$(ACI)

.PHONY: clean
clean:
	stack clean
	rm -f rex *.docker *.aci *.asc

$(ACI): rex
	docker build -t wire.com/rex:$(VERSION) .
	docker save -o rex-$(VERSION).docker wire.com/rex:$(VERSION)
	docker2aci -debug rex-$(VERSION).docker
	mv rex-$(VERSION).aci $(ACI)

ifeq ($(DOCKER),true)
rex: rex.cabal stack.yaml $(SRC)
	docker run --rm \
		-v $(CURDIR):/rex \
		-v ~/.stack:/root/.stack \
		quay.io/kim1/alpine-haskell-build \
		sh -c 'cd /rex && stack --system-ghc --local-bin-path . --allow-different-user install'
else
rex: rex.cabal stack.yaml $(SRC)
	stack --local-bin-path . install
endif
