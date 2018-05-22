SHELL          := /usr/bin/env bash
NAME           := nginz
NGINX_VERSION   = 1.12.2
NGINZ_VERSION  ?=
SWAGGER_VERSION:= 2.0.17
SHELL          := /usr/bin/env bash
DIST           := build
BIN            := src/objs/nginx
DEB            := $(NAME)_$(NGINZ_VERSION)_amd64.deb
ifeq ($(DEBUG), 1)
WITH_DEBUG      = --with-debug
endif

DEST_PATH      ?= /opt/nginz
# Use a folder that can be written to since errors during startup do not respect
# your config and will use the `LOG_PATH` defined here
LOG_PATH       ?= /var/log/nginz
CONF_PATH      ?= /etc/nginz
PID_PATH       ?= /var/run
# You may need to use this if you have some dependencies in non-standard locations
EXTRA_CC_INC   ?=
EXTRA_CC_LIB   ?=

# Where should we look for packages, locally or globally?
EXTRA_PKG_PATH := $(shell [ -w /usr/local ] && echo /usr/local || echo "$(HOME)/.wire-dev")/lib/pkgconfig

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
	-rm -rf src $(DIST) .metadata zwagger-ui/swagger-ui

compile: $(BIN)
	mkdir -p dist ../../dist
	cp src/objs/nginx ../../dist/

dist: guard-NGINZ_VERSION $(DIST)/$(DEB)

$(BIN): src zwagger-ui/swagger-ui
	PKG_CONFIG_PATH=$(EXTRA_PKG_PATH) pkg-config --exists libzauth || { echo -e "\n\033[0;31m    The 'libzauth' library was not found\033[0m\n    pkg-config path = $(EXTRA_PKG_PATH)\n\n    Attempting to install it...\n" && $(MAKE) libzauth; }
	git submodule update --init
	(cd src; PKG_CONFIG_PATH=$(EXTRA_PKG_PATH) ./configure $(CONFIG_OPTIONS) $(ADDITIONAL_MODULES))
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

NGINX_BUNDLE=nginx-$(NGINX_VERSION).tar.gz
SWAGGER_BUNDLE=swagger-$(SWAGGER_VERSION).tar.gz

zwagger-ui/swagger-ui: $(SWAGGER_BUNDLE)
	tar zxf $(SWAGGER_BUNDLE)
	rm -rf zwagger-ui/swagger-ui
	mv -v swagger-ui-$(SWAGGER_VERSION)/dist zwagger-ui/swagger-ui
	touch zwagger-ui/swagger-ui
	rm -rf swagger-ui-$(SWAGGER_VERSION)

$(SWAGGER_BUNDLE):
	curl -L https://github.com/swagger-api/swagger-ui/archive/v$(SWAGGER_VERSION).tar.gz -o $(SWAGGER_BUNDLE)

src: $(NGINX_BUNDLE) gcc8.patch
	#Find keys on https://nginx.org/en/pgp_keys.html
	gpg --verify $(NGINX_BUNDLE).asc $(NGINX_BUNDLE)
	tar zxf $(NGINX_BUNDLE)
	# Fix compilation with GCC 8; see https://github.com/wireapp/wire-server/issues/320
	(cd nginx-$(NGINX_VERSION); patch -p1 -i ../gcc8.patch)
	rm -rf src && mv nginx-$(NGINX_VERSION) src

$(NGINX_BUNDLE):
	curl -O https://nginx.org/download/$(NGINX_BUNDLE).asc
	curl -O https://nginx.org/download/$(NGINX_BUNDLE)

.PHONY: docker
docker:
	git submodule update --init
	docker build -f Dockerfile -t nginz ../..

.PHONY: libzauth
libzauth:
	$(MAKE) -C ../../libs/libzauth install
