# nginx-zauth-module

This NGINX module enables verification of access-tokens.

Tokens can be send as `Authorization` header with `Bearer` scheme, e.g.

```
Authorization: Bearer eaafe0fad7947d419aea629f91679...
```

or as `Authorization` header with `AWS4-HMAC-SHA256` scheme, e.g.

```
Authorization: AWS4-HMAC-SHA256 Credential=eaafe0fad7947d419aea629f91679..., SignedHeaders=host;range;x-amz-date;,...
```

or as query parameter, e.g.

```
GET /foo?access_token=eaafe0fad7947d419aea629f91679...
```

Additionally user and connection information is set in upstream
requests if configured.

## Configuration directives and variables

### zauth_key

This directive can be in `main` or `server` sections one or more times.
Each `zauth_key` accepts a single string as argument. Together they build
the vector of keys in declaration order.

*Example*:

    server {
        listen 8080;

        zauth_key "secret1";
        zauth_key "secret2";
        zauth_key "secret3";
    }

### $zauth_user, $zauth_client, $zauth_connection

These variables are replaced with the actual user/client/connection IDs of an
access-token and set in upstream requests as headers "Z-User" / "Z-Client" /
"Z-Connection".

*Example*:

    location /upstream {
        proxy_set_header "Z-User"       $zauth_user;
        proxy_set_header "Z-Client"     $zauth_client;
        proxy_set_header "Z-Connection" $zauth_connection;
        proxy_pass       http://localhost:9000;
    }

