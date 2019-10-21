# Using the SCIM API with curl {#RefScimViaCurl}

_Author: Matthias Fischmann_

---

This page shows you how to communicate with the wire backend through
the [SCIM API](http://www.simplecloud.info/) by example.  All examples
are [curl](https://curl.haxx.se/) (in bash syntax).

We support setting the handle and user name in wire (the thing with
`@` and the longer thing without `@`).  There is also support for
setting rich-info.  Group provisioning is planned, but the release
date hasn't been fixed yet.

If you want to dive into the backend code, start [reading here in our
backend](https://github.com/wireapp/wire-server/blob/develop/services/spar/src/Spar/Scim.hs)
and [our hscim library](https://github.com/wireapp/hscim).


## Creating an SCIM token

First, we need a little shell environment:

```bash
export WIRE_BACKEND=https://prod-nginz-https.wire.com
export WIRE_ADMIN=...
export WIRE_PASSWD=...
```

SCIM currently supports a variant of HTTP basic auth.  In order to
create a token in your team, you need to authenticate using your team
admin credentials.  The way this works behind the scenes in your
browser or cell phone, and in plain sight if you want to use curl, is
you need to get a wire token.

```bash
export BEARER=$(curl -X POST \
  --header 'Content-Type: application/json' \
  --header 'Accept: application/json' \
  -d '{"email":"'"$WIRE_ADMIN"'","password":"'"$WIRE_PASSWD"'"}' \
  $WIRE_BACKEND/login'?persist=false' | jq -r .access_token)
```

This token will be good for 15 minutes; after that, just repeat.
(Note that SCIM requests are authenticated with a SCIM token, see
below.  SCIM tokens do not expire, but need to be deleted explicitly.)

If you don't want to install [jq](https://stedolan.github.io/jq/), you
can just call the `curl` command and copy the access token into the
shell variable manually.

A quick test that you're logged in:

```bash
curl -X GET --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/self
```

Now you are ready to create a SCIM token:

```bash
export SCIM_TOKEN_FULL=$(curl -X POST \
  --header "Authorization: Bearer $BEARER" \
  --header 'Content-Type: application/json;charset=utf-8' \
  -d '{ "description": "test '"`date`"'", "password": "'"$WIRE_PASSWD"'" }' \
  $WIRE_BACKEND/scim/auth-tokens)
export SCIM_TOKEN=$(echo $SCIM_TOKEN_FULL | jq -r .token)
export SCIM_TOKEN_ID=$(echo $SCIM_TOKEN_FULL | jq -r .info.id)
```

...  and look it up again:

```bash
curl -X GET --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/scim/auth-tokens
```

...  and delete it:

```bash
curl -X DELETE --header "Authorization: Bearer $BEARER" \
  $WIRE_BACKEND/scim/auth-tokens?id=$SCIM_TOKEN_ID
```

## CRUD

### JSON encoding of SCIM Users

A minimal definition of a user looks like this:

```bash
export SCIM_USER='{
  "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User"],
  "externalId"  : "nick@example.com",
  "userName"    : "nick",
  "displayName" : "The Nick"
}'
```

The `externalId` is used to construct a saml identity.  Two cases are
currently supported:

1. `externalId` contains a valid email address.  The SAML `NameID` has
the form `<NameID
Format="urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress">me@example.com</NameID>`.
2. `externalId` contains anything that is *not* an email address.  The
SAML `NameID` has the form `<NameID
Format="urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified">...</NameID>`.

*NOTE: It is important to configure your SAML provider to use
`nameid-format:emailAddress` or `nameid-format:unspecified`.  Other
nameid formats are not supported at this moment*.
See also: https://github.com/wireapp/wire-server/blob/c507ed64a7d4f0af2bffe2f9c3eb4b5f89a477c0/services/spar/src/Spar/Scim/User.hs#L149-L158

We also support custom fields that are used in rich profiles in this
form [see {#RefRichInfo}](../user/rich-info.md):

```bash
export SCIM_USER='{
  "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User", "urn:wire:scim:schemas:profile:1.0"],
  "externalId"  : "rnick@example.com",
  "userName"    : "rnick",
  "displayName" : "The Rich Nick",
  "urn:wire:scim:schemas:profile:1.0": {
    "richInfo": [
      {
        "type": "Department",
        "value": "Sales & Marketing"
      },
      {
        "type": "Favorite color",
        "value": "Blue"
      }
    ]
  }
}'
```

### create user

```bash
export STORED_USER=$(curl -X POST \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  --header 'Content-Type: application/json;charset=utf-8' \
  -d "$SCIM_USER" \
  $WIRE_BACKEND/scim/v2/Users)
export STORED_USER_ID=$(echo $STORED_USER | jq -r .id)
```

### get specific user

```bash
curl -X GET \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  --header 'Content-Type: application/json;charset=utf-8' \
  $WIRE_BACKEND/scim/v2/Users/$STORED_USER_ID
```

### get all users

```bash
curl -X GET \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  --header 'Content-Type: application/json;charset=utf-8' \
  $WIRE_BACKEND/scim/v2/Users/
```

### update user

For each put request, you need to provide the full json object.  All
omitted fields will be set to `null`.  (If you do not have an
up-to-date user present, just `GET` one right before the `PUT`.)

```bash
export SCIM_USER='{
  "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User"],
  "externalId"  : "rnick@example.com",
  "userName"    : "newnick",
  "displayName" : "The New Nick"
}'

curl -X PUT \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  --header 'Content-Type: application/json;charset=utf-8' \
  -d "$SCIM_USER" \
  $WIRE_BACKEND/scim/v2/Users/$STORED_USER_ID
```

### delete user

```
curl -X DELETE \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  $WIRE_BACKEND/scim/v2/Users/$STORED_USER_ID
```

### groups

**Not implemented yet.**
