# Storing SCIM-related data

Reference: {#DevScimStorage}

_Author: Artyom Kazak, Matthias Fischmann_

---

## Storing user data {#DevScimStorageUsers}

SCIM user data is validated by the spar service and stored as brig users.  All fields that wire doesn't care about are silently dropped.  `GET /scim/v2/Users` will trigger a lookup in brig, and the data thus obtained is synthesized back into a SCIM record.

Time stamps `created_at` and `last_updated_at` for the SCIM metadata are stored in `spar.scim_user_times`.  The are kept in sync with the users that are otherwise stored in brig.  (Rationale: we briefly considered using `select writetime(*) from brig.user` for last update and `select writetime(activated) from brig.user` for creation, but this has a drawback: we don't have the time stamps when storing the record, so the `POST` handler would need to do a database write and a consecutive lookup, or an `insert if not exists`.)

Users created by SCIM set the `ManagedBy` field in brig to `ManagedByScim`.  This *should* lead to brig disallowing certain update operations (since the single source of truth should be the SCIM peer that has created and is updating the user), but we never got around to implementing that (as of Wed 15 Jul 2020 10:59:11 AM CEST).  See also {@SparBrainDump} (grep for `ManagedBy`).


## Storing SCIM tokens {#DevScimStorageTokens}

[SCIM tokens](../../reference/provisioning/scim-token.md) are stored in two tables in Spar:

* `team_provisioning_by_token` for `token -> token info` lookups; used to perform authentication.

* `team_provisioning_by_team` for `team -> [token info]` and `(team, token ID) -> token info` lookups; used to display tokens in team settings, and to decide which tokens should be deleted when the whole team is deleted.
