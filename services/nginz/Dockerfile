# Requires docker >= 17.05 (requires support for multi-stage builds)
FROM alpine:3.10 as libzauth-builder

# Compile libzauth
COPY libs/libzauth /src/libzauth
RUN cd /src/libzauth/libzauth-c \
    && apk add --no-cache make bash cargo libsodium-dev \
    && make install

# Nginz container
FROM alpine:3.10

# Install libzauth
COPY --from=libzauth-builder /usr/local/include/zauth.h /usr/local/include/zauth.h
COPY --from=libzauth-builder /usr/local/lib/libzauth.so /usr/local/lib/libzauth.so
COPY --from=libzauth-builder /usr/local/lib/pkgconfig/libzauth.pc /usr/local/lib/pkgconfig/libzauth.pc

COPY services/nginz/third_party /src/third_party

ENV CONFIG --prefix=/etc/nginx \
        --sbin-path=/usr/sbin/nginx \
        --modules-path=/usr/lib/nginx/modules \
        --conf-path=/etc/nginx/nginx.conf \
        --error-log-path=/var/log/nginx/error.log \
        --http-log-path=/var/log/nginx/access.log \
        --pid-path=/var/run/nginx.pid \
        --lock-path=/var/run/nginx.lock \
        --http-client-body-temp-path=/var/cache/nginx/client_temp \
        --http-proxy-temp-path=/var/cache/nginx/proxy_temp \
        --http-fastcgi-temp-path=/var/cache/nginx/fastcgi_temp \
        --http-uwsgi-temp-path=/var/cache/nginx/uwsgi_temp \
        --http-scgi-temp-path=/var/cache/nginx/scgi_temp \
        --user=nginx \
        --group=nginx \
        --with-http_ssl_module \
        --with-http_stub_status_module \
        --with-http_realip_module \
        --with-http_gunzip_module \
        --add-module=/src/third_party/nginx-zauth-module \
        --add-module=/src/third_party/headers-more-nginx-module \
        --add-module=/src/third_party/nginx-module-vts

# extra build dependencies needed for libzauth/nginx-xauth-module
RUN apk add --no-cache --virtual .build-deps \
        libsodium-dev \
        llvm-libunwind-dev

################# similar block as upstream ########################################
# see https://github.com/nginxinc/docker-nginx/blob/master/stable/alpine/Dockerfile
# This uses dockerfile logic from before 1.16
####################################################################################

ENV NGINX_VERSION 1.16.1

RUN set -x \
    && addgroup -g 101 -S nginx \
    && adduser -S -D -H -u 101 -h /var/cache/nginx -s /sbin/nologin -G nginx -g nginx nginx \
    && export GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8 \
    && apk add --no-cache --virtual .build-deps \
        libsodium-dev \
        llvm-libunwind-dev \
        gcc \
        libc-dev \
        make \
        openssl-dev \
        pcre-dev \
        zlib-dev \
        linux-headers \
        curl \
        gnupg1 \
        libxslt-dev \
        gd-dev \
        geoip-dev \
    && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz -o nginx.tar.gz \
    && curl -fSL https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz.asc  -o nginx.tar.gz.asc \
    && found=''; \
    for server in \
        ha.pool.sks-keyservers.net \
        hkp://keyserver.ubuntu.com:80 \
        hkp://p80.pool.sks-keyservers.net:80 \
        pgp.mit.edu \
    ; do \
        echo "Fetching GPG key $GPG_KEYS from $server"; \
        gpg --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$GPG_KEYS" && found=yes && break; \
    done; \
    test -z "$found" && echo >&2 "error: failed to fetch GPG key $GPG_KEYS" && exit 1; \
    gpg --batch --verify nginx.tar.gz.asc nginx.tar.gz \
    && rm -rf "$GNUPGHOME" nginx.tar.gz.asc \
    && mkdir -p /usr/src \
    && tar -zxC /usr/src -f nginx.tar.gz \
    && rm nginx.tar.gz \
    && cd /usr/src/nginx-$NGINX_VERSION \
    && ./configure $CONFIG --with-debug \
    && make -j$(getconf _NPROCESSORS_ONLN) \
    && mv objs/nginx objs/nginx-debug \
    && ./configure $CONFIG \
    && make -j$(getconf _NPROCESSORS_ONLN) \
    && make install \
    && rm -rf /etc/nginx/html/ \
    && mkdir /etc/nginx/conf.d/ \
    && mkdir -p /usr/share/nginx/html/ \
    && install -m644 html/index.html /usr/share/nginx/html/ \
    && install -m644 html/50x.html /usr/share/nginx/html/ \
    && install -m755 objs/nginx-debug /usr/sbin/nginx-debug \
    && ln -s ../../usr/lib/nginx/modules /etc/nginx/modules \
    && strip /usr/sbin/nginx* \
    && rm -rf /usr/src/nginx-$NGINX_VERSION \
    \
    # Bring in gettext so we can get `envsubst`, then throw
    # the rest away. To do this, we need to install `gettext`
    # then move `envsubst` out of the way so `gettext` can
    # be deleted completely, then move `envsubst` back.
    && apk add --no-cache --virtual .gettext gettext \
    && mv /usr/bin/envsubst /tmp/ \
    \
    && runDepsTmp="$( \
        scanelf --needed --nobanner --format '%n#p' /usr/sbin/nginx /usr/lib/nginx/modules/*.so /tmp/envsubst \
            | tr ',' '\n' \
            | sort -u \
            | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' \
    )" \
    \
    # exclude libzauth from runDeps
    && runDeps=${runDepsTmp/so:libzauth.so/''} \
    && apk add --no-cache --virtual .nginx-rundeps $runDeps \
    && apk del .build-deps \
    && apk del .gettext \
    && mv /tmp/envsubst /usr/local/bin/ \
    \
    # Bring in tzdata so users could set the timezones through the environment
    # variables
    && apk add --no-cache tzdata \
    \
    # forward request and error logs to docker log collector
    && ln -sf /dev/stdout /var/log/nginx/access.log \
    && ln -sf /dev/stderr /var/log/nginx/error.log

################# wire/nginz specific ######################

RUN apk add --no-cache inotify-tools dumb-init bash curl && \
    # add libzauth runtime dependencies back in
    apk add --no-cache libsodium llvm-libunwind libgcc

COPY services/nginz/nginz_reload.sh /usr/bin/nginz_reload.sh

ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["/usr/bin/nginz_reload.sh", "-g", "daemon off;", "-c", "/etc/wire/nginz/conf/nginx.conf"]
