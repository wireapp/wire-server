SHELL          := /usr/bin/env bash
NAME           := nginz
NGINX_VERSION   = 1.12.2
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
# By default, use a folder that can usually be written to avoid logging (not critical)
# errors during startup
LOG_PATH       ?= /tmp/log/nginz
CONF_PATH      ?= /etc/nginz
PID_PATH       ?= /var/run
# You may need to use this if you have some dependencies in non-standard locations
EXTRA_CC_INC ?= -I/usr/local/opt/openssl/include
EXTRA_CC_LIB ?= -L/usr/local/opt/openssl/lib

CONFIG_OPTIONS = \
	--prefix=$(DEST_PATH) \
	$(WITH_DEBUG) \
	--with-cc-opt="-std=gnu99 $(EXTRA_CC_INC)" \
	--with-ld-opt="$(EXTRA_CC_LIB)" \
	--error-log-path=$(LOG_PATH)/error.log \
	--http-log-path=$(LOG_PATH)/access.log \
	--conf-path=$(CONF_PATH)/nginx.conf \
	--pid-path=$(PID_PATH)

ADDITIONAL_MODULES = \
    --with-http_ssl_module \
    --with-http_stub_status_module \
    --with-http_realip_module \
    --with-http_gunzip_module \
    --add-module=../third_party/nginx-zauth-module \
    --add-module=../third_party/headers-more-nginx-module \
    --add-module=../third_party/nginx-module-vts

guard-%:
	@ if [ "${${*}}" = "" ]; then \
	      echo "Environment variable $* not set"; \
	    exit 1; \
	fi

default: compile

.PHONY: clean
clean:
	-rm -rf src $(DIST) .metadata conf/zwagger-ui/swagger-ui

init:
	mkdir -p $(LOG_PATH)

compile: init $(BIN)
	mkdir -p dist ../../dist
	cp src/objs/nginx ../../dist/

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
	echo $(pwd)
	tar zxf $(TMPDIR)/swagger-ui-2.0.17.tar.gz
	mkdir -p conf/zwagger-ui/swagger-ui
	cp -r swagger-ui-2.0.17/dist/* conf/zwagger-ui/swagger-ui
	-rm -rf swagger-ui-2.0.17

$(TMPDIR)/$(BUNDLE):
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(BUNDLE).asc)
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(BUNDLE))
	(cd $(TMPDIR); curl -O https://s3-eu-west-1.amazonaws.com/public.wire.com/artifacts/swagger-ui-2.0.17.tar.gz)

.PHONY: docker
docker:
	git submodule update --init
	docker build -f Dockerfile -t nginz ../..
