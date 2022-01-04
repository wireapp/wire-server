FROM alpine:3.15.0

RUN apk add --no-cache curl bash openssl bind-tools jq

COPY tools/sftd_disco/sftd_disco.sh /usr/bin/sftd_disco.sh

ENTRYPOINT ["/usr/bin/sftd_disco.sh"]
