FROM ubuntu:16.04

ARG re_version
ARG restund_version
ARG extra_modules="zrest drain"
ARG DEBIAN_FRONTEND=noninteractive

COPY src /build
RUN apt-get update \
    && apt-get install -y make gcc libssl-dev \
    && cd /build/re-${re_version} \
    && make RELEASE=1 \
    && make RELEASE=1 PREFIX=/usr/local install \
    && cd /build/restund-${restund_version} \
    && make RELEASE=1 EXTRA_CFLAGS="-std=gnu99" EXTRA_MODULES='${extra_modules}' \
    && make RELEASE=1 EXTRA_MODULES='${extra_modules}' PREFIX=/usr/local install \
    && ldconfig \
    && rm -rf /build \
    && apt-get remove -y make gcc \
    && apt-get autoremove -y

RUN useradd --system --shell /bin/false -U restund

USER   restund
VOLUME /usr/local/etc/restund
#EXPOSE 1024-65000
ENTRYPOINT ["/usr/local/sbin/restund", "-n", "-f", "/usr/local/etc/restund/restund.conf" ]
