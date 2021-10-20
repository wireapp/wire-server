FROM alpine:3.12.3

RUN apk add --no-cache curl bash openssl bind-tools

COPY tools/nginz_disco/nginz_disco.sh /usr/bin/nginz_disco.sh

ENTRYPOINT ["/usr/bin/nginz_disco.sh"]
