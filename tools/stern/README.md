Stern - Backoffice facade
=========================

This tool can be used to create a very basic Backoffice tool to simplify performing operations on users and teams such as visualising their user profiles, suspending or even deleting accounts. It is used internally at Wire to provide customer support the means to respond to certain queries from our customers.

Stern provides a swagger interface that accesses multiple other services (mostly using internal endpoints) and is designed to be a simple way to create a basic backoffice functionality. The swagger interface is served at `<ip:stern_port>/stern/api-docs`

## IMPORTANT NOTES

If you want to deploy this together with the rest of the wire-server services, do _NOT_ expose this to the public internet; this will give anyone with access to it a way to look into users' metadata and other potentially sensitive information.

It is intended to be deployed in a private network and accessible only through a VPN (for instance).

Some endpoints (marked as such on the Swagger interface) depend on internal services (named galeb and ibis) that are not relevant for a generic wire server installation as they gather info from other internal systems at Wire (related to billing or other services) and as such will not work properly on installations without them.

## How to run stern together with the rest of wire-server

TODO: This section is under construction

## How to run stern locally with the `services-demo`

First, follow the instruction in [`deploy/services-demo/README.md`](../../deploy/services-demo/README.md),
running the services natively (not with Docker).

In another terminal, run stern:

```
./dist/stern -c tools/stern/stern.example.yaml
```

When you now open `localhost:8080/swagger-ui` in a browser, you can switch to the
"Back Office" tab.
