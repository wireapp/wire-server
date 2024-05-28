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
openssl req -x509 -new -nodes -out $CADIR/$CANAME.crt -key $CANAME.key \
  -sha256 -subj '/CN=Example Root CA/C=DE/ST=Berlin/L=Berlin/O=Example'
openssl ca -in $CADIR/$CANAME.crt -out $CADIR/$CANAME.pem -selfsign -extensions v3_intermediate_ca

## Intermediate certificate
openssl genrsa -out intermediate-$CANAME.key
openssl req -new -key $CADIR/intermediate-$CANAME.key -out $CADIR/intermediate-$CANAME.csr \
  -subj '/CN=Example Root CA\C=DE/ST=Berlin/L=Berlin/O=Example'
openssl ca -in $CADIR/intermediate-$CANAME.csr -out $CADIR/intermediate-$CANAME.pem -extensions v3_intermediate_ca
# openssl ca -keyfile $CADIR/$CANAME.key -extensions v4_intermediate_ca -notext \ 
  # -md sha256 -in $CADIR/intermediate-$CANAME.csr -out $CADIR/intermediate-$CANAME.crt

echo "Certificate and key directory:"
echo $CADIR

echo ""
echo "-----------" 
echo ""
cat $CADIR/$CANAME.key
echo ""
echo "-----------"
echo ""
cat $CADIR/$CANAME.crt
echo ""
echo "-----------"
echo ""
cat $CADIR/$CANAME.pub
