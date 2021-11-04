localhost.client-only.pem has been created with:

```
openssl x509 -req -in <(openssl req -nodes -newkey rsa:2048 -keyout localhost.client-only-key.pem -out /dev/stdout -subj "/") -CA unit-ca.pem -CAkey unit-ca-key.pem -out localhost.client-only.pem -set_serial 0 -extfile <(echo 'subjectAltName = DNS:*integration.example.com, DNS:localhost'; echo 'extendedKeyUsage = clientAuth')
```
