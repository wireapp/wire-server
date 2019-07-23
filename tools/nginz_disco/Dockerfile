FROM alpine:3.10

RUN apk add --no-cache curl bash openssl bind-tools

COPY nginz_disco.sh /usr/bin/nginz_disco.sh

ENTRYPOINT ["/usr/bin/nginz_disco.sh"]
