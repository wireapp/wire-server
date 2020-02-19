LANG           := en_US.UTF-8
SHELL          := /usr/bin/env bash
NAME           := nginz
NGINX_VERSION   = 1.16.1
NGINZ_VERSION  ?=
SWAGGER_VERSION:= 2.2.10
ARCH         := $(shell if [ -f "`which dpkg-architecture`" ]; then dpkg-architecture -qDEB_HOST_ARCH; else [ -f "`which dpkg`" ] && dpkg --print-architecture; fi )
SHELL          := /usr/bin/env bash
DIST           := build
BIN            := src/objs/nginx
DEB            := $(NAME)_$(NGINZ_VERSION)_$(ARCH).deb
ifeq ($(DEBUG), 1)
WITH_DEBUG      = --with-debug
endif
DOCKER_REGISTRY ?= quay.io
DOCKER_USER    ?= quay.io/wire
DOCKER_TAG     ?= local

DEST_PATH      ?= /opt/nginz
# Use a folder that can be written to since errors during startup do not respect
# your config and will use the `LOG_PATH` defined here
LOG_PATH       ?= /var/log/nginz
CONF_PATH      ?= /etc/nginz
PID_PATH       ?= /var/run

# You may need to use this if you have some dependencies in non-standard
# locations. For macOS, we use Brew default directories for OpenSSL (if they
# exist). These variables can be always overridden when running the
# Makefile, though.
ifeq ($(wildcard /usr/local/opt/openssl/.),)
    EXTRA_CC_INC ?=
    EXTRA_CC_LIB ?=
else
    EXTRA_CC_INC ?= -I/usr/local/opt/openssl/include
    EXTRA_CC_LIB ?= -L/usr/local/opt/openssl/lib
endif

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

.PHONY: compile
compile: $(BIN)
	mkdir -p ../../dist
	cp src/objs/nginx ../../dist/

.PHONY: dist
dist: guard-NGINZ_VERSION $(DIST)/$(DEB)

$(BIN): src zwagger-ui/swagger-ui
	PKG_CONFIG_PATH=$(EXTRA_PKG_PATH) pkg-config --exists libzauth || { echo -e "\n\033[0;31m    The 'libzauth' library was not found\033[0m\n    pkg-config path = $(EXTRA_PKG_PATH)\n\n    Attempting to install it...\n" && $(MAKE) libzauth; }
	git submodule update --init
	(cd src; PKG_CONFIG_PATH=$(EXTRA_PKG_PATH) ./configure $(CONFIG_OPTIONS) $(ADDITIONAL_MODULES))
	make -C src

%.deb: $(BIN) $(DIST) guard-ARCH
	makedeb --name=$(NAME) \
	 --version=$(NGINZ_VERSION) \
	 --debian-dir=deb \
	 --architecture=$(ARCH) \
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

src: $(NGINX_BUNDLE)
	#Find keys on https://nginx.org/en/pgp_keys.html
	gpg --verify $(NGINX_BUNDLE).asc $(NGINX_BUNDLE)
	tar zxf $(NGINX_BUNDLE)
	rm -rf src && mv nginx-$(NGINX_VERSION) src

$(NGINX_BUNDLE):
	curl -O https://nginx.org/download/$(NGINX_BUNDLE).asc
	curl -O https://nginx.org/download/$(NGINX_BUNDLE)

.PHONY: docker
docker:
	git submodule update --init
	docker build -t $(DOCKER_USER)/nginz:$(DOCKER_TAG) -f Dockerfile ../..
	docker tag $(DOCKER_USER)/nginz:$(DOCKER_TAG) $(DOCKER_USER)/nginz:latest
	if test -n "$$DOCKER_PUSH"; then docker login $(DOCKER_REGISTRY); docker push $(DOCKER_USER)/nginz:$(DOCKER_TAG); docker push $(DOCKER_USER)/nginz:latest; fi;

.PHONY: libzauth
libzauth:
	$(MAKE) -C ../../libs/libzauth install

# a target to start the locally-compiled docker image (tagged 'local')
# using the configuration in wire-server/deploy/services-demo
# can aid when updating nginx versions and configuration
.PHONY: docker-run-demo-local
docker-run-demo:
	docker run --network=host -it -v $$(pwd)/../../deploy/services-demo:/configs --entrypoint /usr/sbin/nginx quay.io/wire/nginz:local -p /configs -c /configs/conf/nginz/nginx-docker.conf
