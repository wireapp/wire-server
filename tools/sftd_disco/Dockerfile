FROM alpine:3.21.3

RUN apk add --no-cache curl bash openssl bind-tools jq

COPY sftd_disco.sh /usr/bin/sftd_disco.sh

ENTRYPOINT ["/usr/bin/sftd_disco.sh"]
