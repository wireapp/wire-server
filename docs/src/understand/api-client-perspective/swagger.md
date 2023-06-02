(swagger-api-docs)=

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
- ...

The first part of the URL's path is the version.

New versions are added from time to time. If you
would like to look at the docs of another version (which did not exist at the
time of writing this): Just update the first path element of an existing link.
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
{"development":[4],"domain":"staging.zinfra.io","federation":false,"supported":[0,1,2]}
```

The URL to open in your browser for the development version `4` is
`https://staging-nginz-https.zinfra.io/v4/api/swagger-ui/`.

### Internal endpoints

Swagger docs for internal endpoints are served per service. I.e. there's one for
`brig`, one for `cannon`, etc.. This is because Swagger doesn't play well with
multiple actions having the same combination of HTTP method and URL path.

Internal APIs are not under version control.

- Unversioned:
    - [`brig` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig)
    - [`cannon` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/cannon)
    - [`cargohold` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/cargohold)
    - [`galley` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/galley)
    - [`spar` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/spar)

The URL pattern is similar to that of public endpoints for latest version:
`https://<nginz-host>/api-internal/swagger-ui/<service>`.

If you want to get the raw json of the swagger:
`https://<nginz-host>/api-internal/swagger-ui/<service>-swagger.json`.

### Finding the source code for an end-point

A *route internal ID* is provided for every end-point.  See
{ref}`named-and-internal-route-ids` for details and usage.
