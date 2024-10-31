(swagger-api-docs)=

# Swagger / OpenAPI documentation

Our staging system provides [OpenAPI
3.0](https://swagger.io/resources/open-api/) documentation of our HTTP
REST API under the following URL:

[https://staging-nginz-https.zinfra.io/api/swagger-ui](https://staging-nginz-https.zinfra.io/api/swagger-ui)

There are several ways to interpret this documentation:

- Read it as a reference
- Generate client code from it
- Interactively explore the API by making requests

To find the source code of end-points mentioned in the API, a *route
internal ID* (field `operationId` in openapi) is provided for every
end-point.  See {ref}`named-and-internal-route-ids` for details and
usage.

If you find anything you don't like or understand, please let us know!

## Example

To get the versions a backend (`staging-nginz-https.zinfra.io` in this case)
supports, execute:

```sh
curl https://<nginz-host>/api-version
{"development":[4],"domain":"staging.zinfra.io","federation":false,"supported":[0,1,2]}
```

The URL to open in your browser for the development version `4` is
`https://<nginz-host>/v4/api/swagger-ui/`.
