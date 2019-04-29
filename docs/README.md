# Reference documentation

What you need to know as a user of the Wire backend: concepts, features, and API. We strive to keep these up to date.

## Users

User lifecycle:

* [User registration](reference/user/registration.md) `{#RefRegistration}`
* [User activation](reference/user/activation.md) `{#RefActivation}`

User profiles and metadata:

* [Connections between users](reference/user/connection.md) `{#RefConnection}`
* [Rich info](reference/user/rich-info.md) `{#RefRichInfo}`

TODO.

## Teams

TODO.

## Messaging

TODO.

## Single sign-on

TODO.

## SCIM provisioning

We have support for provisioning users via SCIM ([RFC 7664][], [RFC 7643][]). It's in the beta stage.

[RFC 7664]: https://tools.ietf.org/html/rfc7664
[RFC 7643]: https://tools.ietf.org/html/rfc7643

* [Using the SCIM API with curl](reference/provisioning/scim-via-curl.md) `{#RefScimViaCurl}`
* [Authentication via SCIM tokens](reference/provisioning/scim-token.md) `{#RefScimToken}`

# Developer documentation

Internal documentation detailing what you need to know as a Wire backend developer. All of these documents can and should be referenced in the code.

If you're not a member of the Wire backend team, you might still find these documents useful, but keep in mind that they are a work in progress.

* [Development setup](developer/dependencies.md) `{#DevDeps}`
* [Editor setup](developer/editor-setup.md) `{#DevEditor}`
* TODO
