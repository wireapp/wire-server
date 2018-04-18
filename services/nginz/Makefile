SHELL          := /usr/bin/env bash
NAME           := nginz
NGINX_VERSION   = 1.12.2
NGINZ_VERSION  ?=
SWAGGER_VERSION:= 2.0.17
SHELL          := /usr/bin/env bash
DIST           := build
BIN            := src/objs/nginx
DEB            := $(NAME)_$(NGINZ_VERSION)_amd64.deb
TMPDIR         ?= /tmp
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

compile: $(BIN)
	mkdir -p dist ../../dist
	cp src/objs/nginx ../../dist/

dist: guard-NGINZ_VERSION $(DIST)/$(DEB)

$(BIN): src conf/zwagger-ui/swagger-ui
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

NGINX_BUNDLE=nginx-$(NGINX_VERSION).tar.gz
SWAGGER_BUNDLE=swagger-$(SWAGGER_VERSION).tar.gz

conf/zwagger-ui/swagger-ui: $(TMPDIR)/$(SWAGGER_BUNDLE)
	tar zxf $(TMPDIR)/$(SWAGGER_BUNDLE)
	mv -vn swagger-ui-$(SWAGGER_VERSION)/dist conf/zwagger-ui/swagger-ui
	rm -rf swagger-ui-$(SWAGGER_VERSION)

$(TMPDIR)/$(SWAGGER_BUNDLE):
	(cd $(TMPDIR); curl -L https://github.com/swagger-api/swagger-ui/archive/v$(SWAGGER_VERSION).tar.gz -o $(SWAGGER_BUNDLE))

src: $(TMPDIR)/$(NGINX_BUNDLE)
	#Find keys on https://nginx.org/en/pgp_keys.html
	gpg --verify $(TMPDIR)/$(NGINX_BUNDLE).asc $(TMPDIR)/$(NGINX_BUNDLE)
	tar zxf $(TMPDIR)/$(NGINX_BUNDLE)
	mv -vn nginx-$(NGINX_VERSION) src
	rm -rf nginx-$(NGINX_VERSION)

$(TMPDIR)/$(NGINX_BUNDLE):
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(NGINX_BUNDLE).asc)
	(cd $(TMPDIR); curl -O https://nginx.org/download/$(NGINX_BUNDLE))

.PHONY: docker
docker:
	git submodule update --init
	docker build -f Dockerfile -t nginz ../..
