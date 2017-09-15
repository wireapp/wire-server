SHELL          := /usr/bin/env bash
NAME           := nginz
NGINX_VERSION   = 1.12.1
NGINZ_VERSION  ?=
SHELL          := /usr/bin/env bash
DIST           := build
BIN            := src/objs/nginx
DEB            := $(NAME)_$(NGINZ_VERSION)_amd64.deb
TMPDIR         ?= /tmp
ifeq ($(DEBUG), 1)
WITH_DEBUG      = --with-debug
endif

DEST_PATH      ?= /opt/nginz
LOG_PATH       ?= /var/log/nginz
CONF_PATH      ?= /etc/nginz
PID_PATH       ?= /var/run

CONFIG_OPTIONS = \
	--prefix=$(DEST_PATH) \
	$(WITH_DEBUG) \
	--with-cc-opt=-std=gnu99 \
	--error-log-path=$(LOG_PATH)/error.log \
	--http-log-path=$(LOG_PATH)/access.log \
	--conf-path=$(CONF_PATH)/nginx.conf \
	--pid-path=$(PID_PATH)

ADDITIONAL_MODULES = \
    --with-http_ssl_module \
    --with-http_stub_status_module \
    --add-module=../third_party/nginx-zauth-module \
    --add-module=../third_party/headers-more-nginx-module \
    --add-module=../third_party/nginx-module-vts

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: dist

.PHONY: clean
clean:
	-rm -rf src $(DIST) .metadata

compile: $(BIN)

dist: guard-NGINZ_VERSION $(DIST)/$(DEB)

$(BIN): src
	git submodule update --init
	(cd src; ./configure $(CONFIG_OPTIONS) $(ADDITIONAL_MODULES))
	make -C src

%.deb: $(BIN) $(DIST)
	makedeb --name=$(NAME) \
	 --version=$(NGINZ_VERSION) \
	 --debian-dir=deb \
	 --architecture=amd64 \
	 --output-dir=$(DIST)

$(DIST):
	mkdir -p $(DIST)

#
# Dependencies
#

BUNDLE=nginx-$(NGINX_VERSION).tar.gz

src: $(TMPDIR)/$(BUNDLE)
	#Find keys on https://nginx.org/en/pgp_keys.html
	gpg --verify $(TMPDIR)/$(BUNDLE).asc $(TMPDIR)/$(BUNDLE)
	tar zxf $(TMPDIR)/$(BUNDLE)
	mv nginx-$(NGINX_VERSION) src

$(TMPDIR)/$(BUNDLE):
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(BUNDLE).asc)
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(BUNDLE))

.PHONY: docker
docker:
	git submodule update --init
	docker build -f ../../services/nginz/Dockerfile -t nginz ../..
