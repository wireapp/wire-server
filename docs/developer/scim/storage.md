# Storing SCIM-related data {#DevScimStorage}

_Author: Artyom Kazak_

---

## Storing user data {#DevScimStorageUsers}

SCIM user data is stored as JSON blobs in the `scim_user` table in Spar, one blob per SCIM-managed user. Those blobs conform to the SCIM standard and are returned by `GET /scim/v2/Users`.

Note that when a user is created via SCIM, the received blob is not written verbatim to the database – it is first parsed by the [hscim](https://github.com/wireapp/hscim) library, and all unknown fields are removed.

Sample blob:

```json
{
    "schemas": [
        "urn:ietf:params:scim:schemas:core:2.0:User",
        "urn:wire:scim:schemas:profile:1.0"
    ],
    "id": "ef4bafda-5be8-46e3-bed2-5bcce55cff01",
    "externalId": "lana@example.com",
    "userName": "lana_d",
    "displayName": "Lana Donohue",
    "urn:wire:scim:schemas:profile:1.0": {
        "richInfo": {
            "version": 0,
            "fields": [
                { "type": "Title", "value": "Chief Backup Officer" },
                { "type": "Favorite quote", "value": "Monads are just giant burritos" }
            ]
        }
    },
    "meta": {
        "resourceType": "User",
        "location": "https://staging-nginz-https.zinfra.io/scim/v2/Users/ef4bafda-5be8-46e3-bed2-5bcce55cff01",
        "created": "2019-04-21T04:15:12.535509602Z",
        "lastModified": "2019-04-21T04:15:18.185055531Z",
        "version": "W/\"e051bc17f7e07dec815f4b9314f76f88e2949a62b6aad8c816086cff85de4783\""
    }
}
```

### One-way sync from Spar to Brig {#DevScimOneWaySync}

A user is considered SCIM-managed if they were provisioned with SCIM (when it's the case, `userManagedBy` will be set to `ManagedByScim`). Data about SCIM-managed users is stored both in Brig and Spar, and should always be in sync.

Currently (2019-04-29) we only implement one-way sync – when a user is modified via SCIM, Spar takes care to update data in Brig. However, user data is _not_ updated on the Spar side when it is changed in Brig, and Brig does not yet prohibit changing user data via its API – it relies on clients to be well-behaved and respect `userManagedBy`.

## Storing SCIM tokens {#DevScimStorageTokens}

[SCIM tokens](../../reference/provisioning/scim-token.md) are stored in two tables in Spar:

* `team_provisioning_by_token` for `token -> token info` lookups; used to perform authentication.

* `team_provisioning_by_team` for `team -> [token info]` and `(team, token ID) -> token info` lookups; used to display tokens in team settings, and to decide which tokens should be deleted when the whole team is deleted.
