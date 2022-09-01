# SCIM tokens

Reference: {#RefScimToken}

_Author: Artyom Kazak_

---

A _SCIM token_ is a bearer token used to authorize SCIM operations.

A team owner can create SCIM tokens for the team. Each of those tokens can be used to provision new members to the team, modify members' profile attributes, etc. Tokens have unlimited duration, but can be revoked.

## Using a SCIM token {#RefScimTokenUsage}

SCIM tokens are not general-purpose API tokens. They only apply to the `/scim/v2/` subtree of the API.

SCIM tokens are intended to be used by provisioning tools, such as Okta, OneLogin, Active Directory, and so on. If you have your own provisioning utility, you can use a token by adding an `Authorization` header to all `/scim/v2/` requests:

```
Authorization: Bearer <token>
```

A SCIM token identifies the team that the token belongs to, so you do not need to specify the team in the request.

## API {#RefScimTokenApi}

### Creating a token {#RefScimTokenCreate}

Creating a token requires the user to be a team owner. As an additional precaution, we also require the user to re-enter their password.

There is a reasonable limit on the number of tokens a single team can have, set in `scim.yaml` at `maxScimTokens`. For Wire the limit is 16.

Sample request and response:

```
POST /scim/auth-tokens

{
    // Token description (useful as a memory aid; non-optional but can be empty)
    "description": "Sample token",

    // User password. If the user does not have a password (e.g. they are
    // SCIM-provisioned), this field should be omitted.
    "password": "secret"
}
```

```
201 Created

{
    // Token itself; only sent once, can not be listed
    "token": "MzIgcmFuZG9tIGJ5dGVzIGluIGJhc2U2NCBmb3JtYXQ=",

    // Metadata about the token, can be listed
    "info": {
        // Token ID, can be used to revoke the token
        "id": "514613de-9fde-4aab-a704-c57af7a3366e",
        // Token description
        "description": "Sample token",
        // Team associated with the token
        "team": "78c65523-490f-4e45-881f-745e3458e280",
        // When the token was created
        "created_at": "2019-04-18T14:09:43.732Z",
        // Identity provider for users provisioned with the token
        // (optional but for now always present)
        "idp": "784edde5-29d8-4dc3-bb95-924519448f09"
    }
}
```

Note that SCIM can only be used with teams that have either no or exactly one SAML IdP ([internal issue](https://github.com/zinfra/backend-issues/issues/1377)).

### Listing existing tokens {#RefScimTokenList}

Listing tokens requires the user to be a team owner.

We don't ever send tokens themselves, only the metadata (which can be used, for instance, to decide which tokens to revoke).

Sample request and response:

```
GET /scim/auth-tokens
```

```
200 OK

{
    "tokens": [
        {
            "id": "514613de-9fde-4aab-a704-c57af7a3366e",
            "description": "Sample token",
            "team": "78c65523-490f-4e45-881f-745e3458e280",
            "created_at": "2019-04-18T14:09:43.732Z",
            "idp": "784edde5-29d8-4dc3-bb95-924519448f09"
        }
    ]
}
```

### Revoking a token {#RefScimTokenDelete}

Revoking a token requires the user to be a team owner.

To revoke a token, the user has to provide the token ID (not the token itself). The revoked token becomes unused immediately and does not show up in the results of `GET /scim/auth-tokens`.

Sample request and response:

```
DELETE /scim/auth-tokens?id=514613de-9fde-4aab-a704-c57af7a3366e
```

```
204 No Content
```
