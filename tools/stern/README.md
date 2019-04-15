Stern - Backoffice facade
=========================

Stern provides a swagger interface that accesses multiple other services (mostly using internal endpoints) and is designed to be a simple way to create a basic backoffice functionality.

It includes functionality such as displaying user profiles, team members and others. The swagger interface is served at `<ip:stern_port>/stern/api-docs`

## IMPORTANT NOTES

If you want to deploy this together with the rest of the wire-server services, do _NOT_ expose this to the public internet; this will give anyone with access to it a way to look into users' metadata and other potentially sensitive information.

It is intended to be deployed in a private network and accessible only through a VPN (for instance).
