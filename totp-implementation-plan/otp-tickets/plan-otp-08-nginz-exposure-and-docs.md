# TOTP 08 — public exposure through nginz, and the docs page

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 07
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §5 "nginz", §8 (docs).

## Scope

Small but load-bearing: without it, the endpoints from `TOTP 07` exist and are tested but are unreachable by any real client.

### nginz routes

`charts/nginz/values.yaml`, alongside the existing `/self/...` entries (around line 230):

```yaml
    - path: /self/totp$
      envs:
      - all
    - path: /self/totp/([^/]*)$
      envs:
      - all
    - path: /self/totp/([^/]*)/activate$
      envs:
      - all
```

These ship as part of the chart. The API surface is defined by us, not configured per site; site admins are not expected to edit this list.

Two things to get right:

- **No `oauth_scope` key.** Second-factor management must require a real user session, not a third-party OAuth token — an OAuth app being able to enrol or remove a user's second factor would defeat the point. The neighbouring `/self$` entry has `oauth_scope: self`; do not copy it.
- Check ordering and regex anchoring against the existing `/self$` entry (an exact match, so it should not shadow these) and against anything further down the list that might match `/self/...` generically.

Add the `DELETE /self/totp/:id` path too if `TOTP 12` has already landed; otherwise that ticket adds it. The `([^/]*)$` pattern above already covers it — confirm rather than assume, since nginz matches on path, not method.

### Documentation page

A page under `docs/` covering:

- the two-step enrolment flow, end to end, with example requests and responses;
- the `otpauth://` URI format and an explicit statement that **clients render the QR code** ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §0) — this is the question client teams will ask first;
- clock-skew diagnostics: what `clientTime` is for, what the client should show the user on `totp-client-clock-skew`;
- recovery codes: shown once, what to tell users about storing them;
- the notification emails users will receive (authenticator added, removed, recovery code used) and what each one means;
- the ops runbook for a locked-out user, pointing at `TOTP 15`;
- the operational warning that the secret encryption key is unrecoverable (`TOTP 06`).

## Acceptance criteria

- At least one integration test exercises a TOTP endpoint **through nginz** rather than directly against brig. A typo in the chart is otherwise invisible to CI, and this is exactly the kind of change that gets merged broken.
- That test covers both an authenticated success and an unauthenticated 401, so the route is proven to exist *and* to be behind zauth.
- `helm lint` / chart rendering clean.
- `changelog.d/` entry noting the newly public endpoints.

## Notes

- Keep this ticket separate from `TOTP 07` deliberately: a chart or ingress problem should not block the feature work, and a route-exposure change is worth its own reviewer attention.
