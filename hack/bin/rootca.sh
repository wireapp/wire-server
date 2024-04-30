#!/usr/bin/env bash
set -euo pipefail
set -x
CANAME=Example-RootCA
CADIR=/tmp/ca/$CANAME

mkdir -p $CADIR
cd $CADIR

## Custom CA root certificate
openssl genrsa -out $CANAME.key 4096 
openssl rsa -in $CADIR/$CANAME.key -pubout -out $CADIR/$CANAME.pub
openssl req -x509 -new -noenc -out $CADIR/$CANAME.crt -key $CANAME.key \
  -sha256 -subj '/CN=Example Root CA/C=DE/ST=Berlin/L=Berlin/O=Example'

## Intermediate certificate
openssl genrsa -out intermediate-$CANAME.key
openssl rsa -in $CADIR/intermediate-$CANAME.key -pubout -out $CADIR/intermediate-$CANAME.pub
openssl req -new -key $CADIR/intermediate-$CANAME.key -out $CADIR/intermediate-$CANAME.csr \
  -sha256 -subj '/CN=Example Root CA/C=DE/ST=Berlin/L=Berlin/O=Example'

INTDIR=$CADIR/intermediate
mkdir -p $INTDIR

openssl req -x509 -new -noenc -out $INTDIR/intermediate-$CANAME.crt -key $CADIR/intermediate-$CANAME.key \
  -sha256 -subj '/CN=Example Root CA/C=DE/ST=Berlin/L=Berlin/O=Example'
# I think this is waiting for input, but for what input? see
# https://www.openssl.org/docs/manmaster/man1/openssl-x509.html#Micro-CA-Options
strace openssl x509 -req -CA $INTDIR/intermediate-$CANAME.crt -CAkey -notext 
