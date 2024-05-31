#!/usr/bin/env bash
set -euo pipefail
set -x

## Custom CA root certificate
CANAME=Example-RootCA
CADIR=/tmp/ca/$CANAME
mkdir -p $CADIR

openssl genrsa -out $CADIR/$CANAME.key 4096 
openssl rsa -in $CADIR/$CANAME.key -pubout -out $CADIR/$CANAME.pub

openssl req -x509 -new -noenc -out $CADIR/$CANAME.crt -key $CADIR/$CANAME.key \
  -sha256 -subj '/CN=Example Root CA/C=DE/ST=Berlin/L=Berlin/O=Example'

## Intermediate certificate
INTNAME=Example-IntermediateCA
INTDIR=$CADIR/intermediate
mkdir -p $INTDIR

openssl genrsa -out $INTDIR/$INTNAME.key
openssl rsa -in $INTDIR/$INTNAME.key -pubout -out $INTDIR/$INTNAME.pub
openssl req -new -key $INTDIR/$INTNAME.key -out $INTDIR/$INTNAME.csr \
  -sha256 -subj '/CN=Example Root CA/C=DE/ST=Berlin/L=Berlin/O=Example'

openssl x509 -req -in $INTDIR/$INTNAME.csr -CA $CADIR/$CANAME.crt \
  -CAkey $CADIR/$CANAME.key -CAcreateserial -sha256 -out $INTDIR/$INTNAME.crt

## leaf certificate

LEAFNAME=Example-Leaf
LEAFDIR=$INTDIR/leaf
mkdir -p $LEAFDIR

openssl genrsa -out $LEAFDIR/$LEAFNAME.key
openssl rsa -in $LEAFDIR/$LEAFNAME.key -pubout -out $LEAFDIR/$LEAFNAME.pub
openssl req -new -key $LEAFDIR/$LEAFNAME.key -out $LEAFDIR/$LEAFNAME.csr \
  -sha256 -subj '/CN=example-leaf/C=DE/ST=Berlin/L=Berlin/O=Example'

openssl x509 -req -in $LEAFDIR/$LEAFNAME.csr -CA $INTDIR/$INTNAME.crt \
  -CAkey $INTDIR/$INTNAME.key -CAcreateserial -sha256 -out $LEAFDIR/$LEAFNAME.crt
