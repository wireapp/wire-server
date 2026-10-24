# TOTP 22 — team-wide MFA enrolment status

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 07
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §5, D12. Consumed by the admin dashboard, and by `TOTP 23` (reset) and `TOTP 24` (grace period), which both need to show an admin who is and is not enrolled.

Milestone 1.2. Reads only; no state changes anywhere.

## Scope

One endpoint that answers "where is my team on MFA rollout?" for every member, including the members who have done nothing — which is the whole point, and the reason this cannot be derived from brig's tables alone.

### The status

```haskell
data TOTPEnrolmentStatus
  = TOTPNotEnrolled   -- "not_enrolled": no credential, or only expired pending ones
  | TOTPPending       -- "pending": a pending enrolment inside pendingEnrolmentTTL, none activated
  | TOTPEnabled       -- "enabled": at least one activated credential
```

`pending` applies the TTL against `Now.get`, in the subsystem, exactly as activation does (`TOTP 05`) — an abandoned enrolment from last week reads as `not_enrolled` whether or not the sweep job has run. An activated credential always wins over a pending one.

### The endpoint

| verb | path | `Named` |
|---|---|---|
| GET | `/teams/:tid/totp-status` | `totp-team-status` |

`From 'V19`, in `Wire.API.Routes.Public.Brig.TOTP`, served by brig: brig owns the credential rows, so the bulk of the answer is local, and brig already hosts team-scoped routes (`/teams/:tid/size`, `/teams/:tid/invitations`).

Response item: `{ user_id, status, enrolled_at }`, where `enrolled_at` is the earliest `activated_at` across the member's credentials (`null` unless `enabled`). No credential ids, no names, no secrets, no counts of attempts — this is a rollout dashboard, not an audit tool.

Named the resource `totp-status` rather than `mfa-status` because TOTP is the only second factor it reports. If another factor is added, the *item* grows a field; do not plan on a second endpoint.

### Two modes

- **`?ids=<uid>,<uid>,…`** — status for exactly those members. Cap the list at the same maximum the existing `GET /users?ids=` path uses (confirm the number in the code rather than copying one from here) and reject a longer list with `400`. Ids that are not members of `:tid` are **omitted from the response** rather than erroring: a 404 would turn this endpoint into an oracle for "is this user id real", and an admin has no legitimate need to ask about non-members.
- **No `ids`** — the whole team, paginated. Follow whichever paging convention the existing team-member listing uses, so a dashboard can page the two lists in step; do not invent a third style. Ordering must be stable across pages.

### Where the data comes from

The member list is galley's, the credentials are brig's. So: page the team members through the existing `GalleyAPIAccess` team-member lookup, then one batched read in brig for that page's user ids.

That read is a new dumb-store operation —

```haskell
GetCredentialsForUsers :: [UserId] -> TOTPStore m [StoredTOTPCredential]
```

— a batch retrieve and nothing more ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D12). The grouping, the TTL comparison (via `TOTP 05`'s shared `isExpiredPending` predicate, so activation and this endpoint cannot disagree about expiry) and the status derivation happen in the subsystem — the derivation as a pure function over `(now, rows)` so its unit tests are table-driven — behind

```haskell
GetEnrolmentStatuses :: [UserId] -> TOTP m (Map UserId TOTPEnrolmentStatus)
```

Members with no rows at all must appear as `not_enrolled`; a `Map` built only from returned rows silently drops exactly the people the dashboard exists to find.

### Authorisation

Team admins and owners only. A plain member must not be able to enumerate which colleagues lack a second factor — that is a target list for whoever is phishing the team. Use the permission that comparable team-admin read endpoints use (check how the team settings endpoints gate; do not invent a new permission), and reject a non-admin member with the same status that other admin-only team endpoints return, so this one does not become a membership oracle either.

## Acceptance criteria

- Integration tests:
  - a team with one enrolled, one pending, one never-touched member returns exactly those three statuses, and the untouched member **is present** in the response;
  - a pending enrolment older than `pendingEnrolmentTTL` reads as `not_enrolled`, with no sweep job having run;
  - `?ids=` returns only the requested members; a non-member id is silently omitted; an over-long list is `400`;
  - paging over a team larger than one page yields every member exactly once, with no duplicates or gaps across pages;
  - a non-admin member is refused; a non-member of the team is refused;
  - the response contains no secret material under any status.
- Unit tests for the status derivation: activated-wins-over-pending, expired-pending-is-not-enrolled, no-rows-is-not-enrolled.
- OpenAPI description states what each status means, including that `pending` is time-bounded.
- `changelog.d/` entry.

## Notes

- The obvious next request will be "sort by status" or "only show not_enrolled". Do not build it now; note that it is a client-side concern until a team is large enough for it not to be, and that adding a filter parameter later is additive.
- This endpoint is read-only and admin-gated, so it needs no `ReauthBody` and no second factor of its own.
