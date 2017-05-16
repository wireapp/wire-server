SHELL        := /usr/bin/env bash
NAME         := nginz
VERSION       = 1.12.0
BUILD_NUMBER ?= 0
BUILD_LABEL  ?= local
BUILD        := $(BUILD_NUMBER)$(shell [ "${BUILD_LABEL}" == "" ] && echo "" || echo ".${BUILD_LABEL}")
SHELL        := /usr/bin/env bash
DIST         := build
BIN          := src/objs/nginx
DEB          := $(NAME)_$(VERSION)+$(BUILD)_amd64.deb
TMPDIR       ?= /tmp
ifeq ($(DEBUG), 1)
WITH_DEBUG    = --with-debug
endif

DEST_PATH    ?= /opt/nginz
LOG_PATH     ?= /var/log/nginz
CONF_PATH    ?= /etc/nginz
PID_PATH     ?= /var/run

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
    --add-module=../third_party/nginx_zauth_module \
    --add-module=../third_party/nginx_headers_more

default: dist

.PHONY: clean
clean:
	-rm -rf src $(DIST) .metadata

compile: $(BIN)

dist: $(DIST)/$(DEB) .metadata

$(BIN): src
	(cd src; ./configure $(CONFIG_OPTIONS) $(ADDITIONAL_MODULES))
	make -C src

%.deb: $(BIN) $(DIST)
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD) \
	 --architecture=amd64 \
	 --output-dir=$(DIST)

.metadata:
	echo -e "NAME=$(NAME)\nVERSION=$(VERSION)\nBUILD_NUMBER=$(BUILD)" \
	 > .metadata

$(DIST):
	mkdir -p $(DIST)

#
# Dependencies
#

BUNDLE=nginx-$(VERSION).tar.gz

src: $(TMPDIR)/$(BUNDLE)
	#Find keys on https://nginx.org/en/pgp_keys.html
	gpg --verify $(TMPDIR)/$(BUNDLE).asc
	tar zxf $(TMPDIR)/$(BUNDLE)
	mv nginx-$(VERSION) src

$(TMPDIR)/$(BUNDLE):
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(BUNDLE).asc)
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(BUNDLE))
