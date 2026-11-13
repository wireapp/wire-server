# TOTP 15 — internal admin reset and account-deletion cleanup

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 07
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §5 (internal endpoints).

## Scope

Two things that both come down to "remove a user's TOTP state": the operator escape hatch, and the automatic cleanup when an account goes away.

### Internal endpoints

In `Wire.API.Routes.Internal.Brig` (unversioned, as all `/i/` routes are):

| verb | path | purpose |
|---|---|---|
| GET | `/i/users/:uid/totp` | list credentials, metadata only — support diagnostics |
| DELETE | `/i/users/:uid/totp` | remove all credentials and recovery codes |

`DELETE` is the last-resort recovery path: a user who lost both their authenticator and their recovery codes. With `TOTP 14` shipped it should be rare, which is the point — it exists so that "rare" is not "never possible".

Requirements:

- **Audit log line** on every use: `TinyLog` at `Info` with the target user id and the request id. Removing someone's second factor must leave a trace.
- **Notify the user** by email that their second factor was removed, for the same reason as `TOTP 12`.
- `GET` returns metadata only — never a secret, never a recovery code, not even a hash. Support staff have no legitimate use for either, and an internal endpoint is still an endpoint.

### Account-deletion cleanup

Both tables have `ON DELETE CASCADE` on `wire_user(id)`, which covers the case where the row is actually deleted. Verify that brig's account deletion path deletes that row rather than only marking the account deleted — if it does not, wire `TOTP.DeleteAllCredentials` explicitly into the deletion path. Leaving a deleted user's TOTP secret and recovery-code hashes behind is a data-retention problem, and the FK will not save you if the row survives.

## Acceptance criteria

- Integration tests:
  - admin reset unlocks a user who can no longer produce a code: after the reset, login succeeds with password alone;
  - after a reset, `GET /i/users/:uid/totp` is empty;
  - the reset also removes recovery codes — assert against the store;
  - `GET` exposes no secret material;
  - the endpoints are not reachable from the public API surface.
- A test (or an explicitly documented verification) that deleting a user account removes both `totp_credential` and `totp_recovery_code` rows.
- Audit log line is emitted — assert it, do not just add it.

## Notes

- These endpoints are the ones most likely to be wrapped in a support tool later. Keeping `GET` metadata-only from the start avoids a painful conversation about what support staff can see.
- No nginz entries: `/i/` routes are not publicly exposed by design.
