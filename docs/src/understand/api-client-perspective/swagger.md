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

## Swagger docs (Swagger 2.0)

The Swagger documentation for endpoints depends on the API version.

The pages below show [Swagger / OpenAPI 2.0](https://swagger.io/specification/v2/)
docs.

### Public endpoints
- Version `v0`:
    - [**public**
      endpoints](https://staging-nginz-https.zinfra.io/v0/api/swagger-ui/)
- Version `v1`:
    - [**public**
    endpoints](https://staging-nginz-https.zinfra.io/v1/api/swagger-ui/)
- Version `v2`:
    - [**public**
    endpoints](https://staging-nginz-https.zinfra.io/v2/api/swagger-ui/)
- Version `v3`:
    - [**public**
    endpoints](https://staging-nginz-https.zinfra.io/v3/api/swagger-ui/)
- Unversioned:
    - [**public**
    endpoints](https://staging-nginz-https.zinfra.io/api/swagger-ui/)

The first part of the URL's path is the version. No specified version means
Swagger docs of the *latest* API version. This differs from other API endpoints
where no version means `v0`!

New versions are added from time to time. If you
would like to look at the docs of another version (which did not exist at the
time of writing): Just update the first path element of an existing link.
The URL pattern is `https://<nginz-host>/v<version>/api/swagger-ui/`. To figure
out which versions are supported by your backend, query
`https://<nginz-host>/<version>/api-version`.

If you want to get the raw json for the swagger (ie., for compiling it
into client code in typescript, kotlin, swift, ...), replace
`swagger-ui` with `swagger.json` in the above URL pattern.

The [API versioning](../../developer/developer/api-versioning.md) article
discusses the versioning topic in detail.

#### Example

To get the versions a backend (`staging-nginz-https.zinfra.io` in this case)
supports, execute:

```sh
curl https://staging-nginz-https.zinfra.io/api-version
{"development":[3],"domain":"staging.zinfra.io","federation":false,"supported":[0,1,2]}
```

The URL to open in your browser for the development version `3` is
`https://staging-nginz-https.zinfra.io/v3/api/swagger-ui/`.

### Internal endpoints

Swagger docs for internal endpoints are served per service. I.e. there's one for
`brig`, one for `cannon`, etc.. This is because Swagger doesn't play well with
multiple actions having the same combination of HTTP method and URL path.

- Version `v3`:
    - [`brig` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/v3/api-internal/swagger-ui/brig)
    - [`cannon` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/v3/api-internal/swagger-ui/cannon)
    - [`cargohold` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/v3/api-internal/swagger-ui/cargohold)
    - [`galley` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/v3/api-internal/swagger-ui/galley)
    - [`legalhold` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/v3/api-internal/swagger-ui/legalhold)
    - [`spar` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/v3/api-internal/swagger-ui/spar)
- Unversioned:
    - [`brig` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig)
    - [`cannon` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/cannon)
    - [`cargohold` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/cargohold)
    - [`galley` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/galley)
    - [`legalhold` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/legalhold)
    - [`spar` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/spar)

The URL pattern is similar to that of public endpoints:
`https://<nginz-host>/v<version>/api-internal/swagger-ui/`. No specified version
means Swagger docs the *latest* version (as for public endpoints' Swagger docs.)

Due to technical reasons (we started to export Swagger docs for internal
endpoints in version `v3`), there are no meaningful Swagger docs for internal
endpoints for versions `v0` to `v2`.
