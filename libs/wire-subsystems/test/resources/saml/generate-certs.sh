#!/usr/bin/env sh

# Generate a key and certs for testing
openssl genpkey -algorithm RSA -out mykey.pem -pkeyopt rsa_keygen_bits:2048
openssl req -new -x509 -key mykey.pem -out cert1.pem -days 3650 \
  -subj "/C=US/O=ExampleOrg/OU=Dev1/CN=CertOne/emailAddress=one@example.com"
openssl req -new -x509 -key mykey.pem -out cert2.pem -days 3650 \
  -subj "/C=DE/O=ExampleOrg/OU=Dev2/CN=CertTwo/emailAddress=two@example.com"

cat cert1.pem cert2.pem > certs.store
