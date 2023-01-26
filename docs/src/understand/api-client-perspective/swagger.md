# Swagger API documentation

Our staging system provides [Swagger /
OpenAPI](https://swagger.io/resources/open-api/) documentation of our HTTP REST
API.

The swagger docs are correct by construction (compiled from the server
code), and they are complete up to bots/services and event notification
payloads (as of 2023-01-16).

There are several ways to interpret this kind of documentation:

- Read it as a reference
- Generate client code from it
- Interactively explore the API by making requests

We are currently (as of 2021-09-29) migrating our documentation into a
new system that is automatically checked for correctness. The old
documentation still has some endpoints, but the new one is getting more and more complete. We will completely replace the old one eventually.

Please check the new docs first, and if you can't find what you're
looking for, double-check the old.

## Swagger docs (Swagger 2.0)

The Swagger documentation for public endpoints depends on the API version.
Though, the version prefix in URL paths works for internal (private) endpoints,
too, they are not versioned.

These pages show [Swagger / OpenAPI 2.0](https://swagger.io/specification/v2/)
docs:

- Version `v0`:
    - [new staging swagger page - **public**
    endpoints](https://staging-nginz-https.zinfra.io/api/swagger-ui/)
- Version `v1`:
    - [new staging swagger page - **public**
    endpoints](https://staging-nginz-https.zinfra.io/v1/api/swagger-ui/)
- Version `v2`:
    - [new staging swagger page - **public**
    endpoints](https://staging-nginz-https.zinfra.io/v2/api/swagger-ui/)
- Version `v3` (current *development* version):
    - [new staging swagger page - **public**
    endpoints](https://staging-nginz-https.zinfra.io/v3/api/swagger-ui/)
- Unversioned
    - [new staging swagger page - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/)

The first part of the URL's path is the version. No specified version means
version `v0`. In case this page gets outdated and e.g. you'll like to look at
the docs of `v4` (which does not exist at the time of writing): Just update the
first path element of an existing link.
