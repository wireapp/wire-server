#!/usr/bin/env bash

export GPG_KEYS=B0F4253373F8F6F510D42178520A9993A1C052F8
found=''
for server in \
    hkp://keyserver.ubuntu.com:80 \
    hkp://p80.pool.sks-keyservers.net:80 \
    pgp.mit.edu \
    ha.pool.sks-keyservers.net; do
    echo "Fetching GPG key $GPG_KEYS from $server"
    gpg --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$GPG_KEYS" && found=yes && break
done
test -z "$found" && echo >&2 "error: failed to fetch GPG key $GPG_KEYS" && exit 1
exit 0
