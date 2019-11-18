# Stern - Backoffice facade

This tool can be used to create a very basic Backoffice tool to simplify performing operations on users and teams such as visualising their user profiles, suspending or even deleting accounts. It is used internally at Wire to provide customer support the means to respond to certain queries from our customers.

Stern provides a swagger interface that accesses multiple other services (mostly using internal endpoints) and is designed to be a simple way to create a basic backoffice functionality. The swagger interface is served at `<ip:stern_port>/stern/api-docs`


## IMPORTANT NOTES

If you want to deploy this together with the rest of the wire-server services, do _NOT_ expose this to the public internet; this will give anyone with access to it a way to look into users' metadata and other potentially sensitive information.

It is intended to be deployed in a private network and accessible only through a VPN (for instance).

Some endpoints (marked as such on the Swagger interface) depend on internal services (named galeb and ibis) that are not relevant for a generic wire server installation as they gather info from other internal systems at Wire (related to billing or other services) and as such will not work properly on installations without them.


## Frontend (Web UI)

TODO: This section is under construction


## How to run stern together with the rest of wire-server

TODO: This section is under construction


## Adding new end-points

1. from Stern.Servant.Types, pick an end-point that looks similar to
   what you want to do.  clone it and mutate it.
2. follow the type errors.  you will be asked to:
3. add an entry to the handler table in Stern.Servant.Handler
4. add some instances for various types you've used and their behavior
   wrt. HTTP parsing and rendering as well as swagger docs generation.
5. that's probably all?
