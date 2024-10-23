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

The [Swagger / OpenAPI 2.0](https://swagger.io/specification/v2/)
documentation for endpoints depends on the API version.  For a list of
all swagger docs for all supported API versions, [visit
https://staging-nginz-https.zinfra.io/api/swagger-ui](https://staging-nginz-https.zinfra.io/api/swagger-ui).

To learn which versions are supported, look at
`https://<nginz-host>/api-version`.  ([See
also.](../../developer/developer/api-versioning.md))

If you want to get the raw json for the swagger (ie., for compiling it
into client code in typescript, kotlin, swift, ...), replace
`swagger-ui` with `swagger.json` in the above URL pattern.

#### Example: doing it manually

To get the versions a backend (`staging-nginz-https.zinfra.io` in this case)
supports, execute:

```sh
curl https://<nginz-host>/api-version
{"development":[4],"domain":"staging.zinfra.io","federation":false,"supported":[0,1,2]}
```

The URL to open in your browser for the development version `4` is
`https://<nginz-host>/v4/api/swagger-ui/`.

### On-prem and test instances, versioning

The above is valid for the official wire.com staging environment and
includes both all released API versions and the current development
version, which changes continuously until released.

If you talk to any other backend, the development version may differ.
Try to ask the backend you're talking if it exposes its docs itself:

```
curl https://nginz-https.<custom-staging>.example.com/<version>/api/swagger-ui/
curl https://nginz-https.<custom-staging>.example.com/<version>/api/swagger.json
```

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
    - [`gundeck` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/gundeck)
    - [`spar` - **internal** (private)
    endpoints](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/spar)

The URL pattern is similar to that of public endpoints for latest version:
`https://<nginz-host>/api-internal/swagger-ui/<service>`.

If you want to get the raw json of the swagger:
`https://<nginz-host>/api-internal/swagger-ui/<service>-swagger.json`.

### Federation API

- Unversioned
  - [`brig` - Federation API](https://staging-nginz-https.zinfra.io/api-federation/swagger-ui/brig)
  - [`galley` - Federation API](https://staging-nginz-https.zinfra.io/api-federation/swagger-ui/galley)
  - [`cargohold` - Federation API](https://staging-nginz-https.zinfra.io/api-federation/swagger-ui/cargohold)

### Finding the source code for an end-point

A *route internal ID* is provided for every end-point.  See
{ref}`named-and-internal-route-ids` for details and usage.
