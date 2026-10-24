# TOTP 23 — admin revoke / reset of MFA

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 15
- TOTP 22
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §5 (internal admin reset), §6 (`EnsureReauthenticated`).

Milestone 1.2.

## Scope

`TOTP 15` gave operators an internal `/i/` reset. This ticket gives *team admins* the same power for their own team, through the public API, so that a user who lost their phone does not need a backend operator — and gives owners a whole-team variant for the one case that genuinely needs it (a botched enforcement rollout, a key rotation gone wrong).

| verb | path | who | effect |
|---|---|---|---|
| DELETE | `/teams/:tid/members/:uid/totp` | team admin or owner | remove that member's credentials and recovery codes |
| DELETE | `/teams/:tid/totp?i_am_sure_this_is_a_good_idea=true` | team **owner** only | remove every member's |

Both `From 'V19`. Both take a `ReauthBody`: the admin proves it is still them, with their own password and their own second factor (§6). Removing other people's second factors is exactly the operation a stolen admin session would want.

### The confirmation parameter

`i_am_sure_this_is_a_good_idea=true` is **required** on the team-wide variant. Absent, misspelled, or anything other than `true` ⇒ `400` with a body that says what the parameter is and what the operation does. It is friction, not security — the security is the owner role plus re-authentication — and it should be documented as friction, so nobody later "improves" the API by making it optional for convenience.

Scope the roles deliberately: a single-member reset is routine support work and belongs to admins; wiping a whole team's second factors is not, and belongs to owners.

### Interaction with enforcement — do not skip this

If the team enforces TOTP (`TOTP 19`), resetting a member returns them to the not-enrolled state, which under enforcement means they are immediately overdue and locked out of the thing the reset was supposed to fix. So a reset **must restart that user's grace period** (`TOTP 24`), both for the single-member and the team-wide variant.

Getting this wrong turns the team-wide reset from a recovery tool into a self-inflicted outage for the entire team, which is the specific scenario the confirmation parameter is there to make people think about.

### Blast radius

A whole-team reset on a large team is a bulk write plus a bulk notification. Run it as a job (`Wire.JobSubsystem`, as in `TOTP 16`) in bounded batches and answer `202` with enough information for the admin to see it completed; do not hold a request open across tens of thousands of rows.

### Audit and notification

Per affected user, both:

- a `TinyLog` `Info` line naming the actor, the target and the request id — for the team-wide case, one line per target plus one summary line, not only a summary;
- an email to the affected user saying their second factor was removed and by whom, as in `TOTP 12` and `TOTP 15`. A silent removal of someone's second factor is indistinguishable from an attack, and the user is the only person who can tell the difference.

### Effect-level changes

`SensitiveOperation` gains `OpResetMemberTOTP` and `OpResetTeamTOTP` (`TOTP 11` defined the enumeration as complete — this ticket is the first to extend it; adding constructors is cheap, the point of the enumeration is that policy lives in one place).

Reuse `TOTP.DeleteAllCredentials` per user; no new store operation is needed beyond what `TOTP 02` provides.

## Acceptance criteria

- Integration tests:
  - admin resets a member: that member then logs in with password alone, and `GET /teams/:tid/totp-status` reports `not_enrolled` for them;
  - an admin resetting a member **without** their own valid second factor is refused;
  - a plain member cannot reset anyone, including themselves via this route (self-service disable is `DELETE /self/totp/:id`, `TOTP 12`);
  - an admin cannot reset a member of a different team;
  - an admin (non-owner) is refused the team-wide variant;
  - the team-wide variant without `i_am_sure_this_is_a_good_idea=true`, and with `=false`, is `400`, and **nothing is deleted** — assert the second half, not just the status code;
  - the team-wide variant with the parameter removes every member's credentials and recovery codes, including the owner's own;
  - under enforcement, a reset member is inside a fresh grace period and can still log in to re-enrol (`TOTP 24`).
- Audit lines and notification emails asserted, not merely implemented.
- `changelog.d/` entry, and a `docs/` runbook entry for "a user lost their authenticator" that points team admins here instead of at support.

## Notes

- The internal `/i/users/:uid/totp` reset from `TOTP 15` stays. It covers personal (teamless) users and the case where the team has no reachable owner.
- Consider whether a team-wide reset should also clear the team's `enforced` flag. Recommendation: **no** — it is a separate decision, and an owner who wants both can do both. But say so in the docs, because the surprise otherwise lands on the whole team at once.
