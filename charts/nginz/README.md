# nginz chart

Deploys [nginz - nginx with authentication module](https://github.com/wireapp/wire-server/services/nginz), along with a little sidecar container

## Configuring zauth

* Public keys must match the public/private keys as used in the `brig` chart.

TODO more documentation/links

## Configuring basic_auth

See also https://nginx.org/en/docs/http/ngx_http_auth_basic_module.html

This only needs to be done when you wish to bypass normal authentication for some specific, otherwise not-exposed endpoints (e.g. `/i/users/activation-code` - see values.yaml in this chart) in test environments for performing automated end-to-end tests.

* set `nginx_conf.env` to `staging` (and ensure this environment does not have any production traffic)
* `htpasswd -cb myfile.txt myuser mypassword && cat myfile.txt` (`htpasswd` is from `httpd-tools` or `apache-utils`) generates a hashed user:password line which you can pass to the nginz chart under `nginz.secrets.basicAuth` (see also wire-server-deploy/values/wire-server/secrets.yaml )
* generate the base64 value of the original user:password (*not* of the myfile contents): `echo '<user>:<password>' | base64`
* deploy and try a request by passing a header `Authorization: Basic <base64-encoded-value>`

## Sidecar container nginz-disco

Due to nginx not supporting DNS names for its list of upstream servers (unless you pay extra), the [nginz-disco](https://github.com/wireapp/wire-server/tree/develop/tools/nginz_disco) container is a simple bash script to do DNS lookups and write the resulting IPs to a file. Nginz reloads on changes to this file.
