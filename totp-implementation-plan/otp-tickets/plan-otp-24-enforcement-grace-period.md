# TOTP 24 — enrolment grace period under team enforcement

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 19
- TOTP 22
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6 "Team-level enforcement".

Milestone 1.2. `TOTP 19` flagged the grace period as the hard part of enforcement and deferred it; this is that ticket.

## The problem

Switching `enforced = true` on a team of 500 people, 480 of whom have never heard of TOTP, must not lock 480 people out at the moment of the click. But "eventually everyone must have it" is the entire point of enforcement, so the grace cannot be open-ended either.

And there is a bootstrap trap sitting underneath: **a user who cannot log in cannot enrol.** Any design that answers "overdue" with a flat login refusal has to explain how the user gets a credential afterwards, and "ask an admin to reset something you never had" is not an answer.

## The design decision this ticket must settle first

Three ways to handle a user who is past the deadline, in decreasing order of how much we like them:

**(a) Restricted session — recommended.** Login succeeds (the password was correct, after all), but the session can reach only the TOTP enrolment endpoints and `GET /self`, until an activated credential exists. Everything else answers `403 totp-enrolment-required`. This is what Okta and Google Workspace do, it keeps the enrolment path open by construction, and the client UX is the obvious one: the app opens straight into "set up your authenticator".

The honest cost: an allowlist of reachable endpoints is the same shape as `guardSecondFactorDisabled`, the blunt instrument `TOTP 21` wants to retire. The difference must be real, not rhetorical — **one** allowlist, in **one** place, checked in a single middleware-level hook rather than sprinkled across handlers, with a test that enumerates it. If it cannot be done in one place, that is a strong argument for (b).

**(b) Flat refusal plus an enrolment token.** Login returns `403 totp-enrolment-required` with a short-lived, single-purpose token usable only against `POST /self/totp` and the activate route. Cleaner conceptually, but it is a new token kind in zauth and a new client flow, which is a lot of machinery for a transitional state.

**(c) Soft enforcement only.** Never block; return a deadline and let clients nag. Easy, and not enforcement. Acceptable only as an explicitly labelled first step, not as the end state.

Pick one, write down why, and get it signed off **before** implementing — changing this later is client-visible in every app.

## When the clock starts

Not at feature-enable time alone: people join teams, and people get promoted into scope (`TOTP 25`). A single team-wide `enforcedSince` handles the rollout and then mishandles everyone who arrives afterwards.

Use a **lazy, per-user start**: the first time the enforcement check observes a user who is in scope and not enrolled, record the timestamp; the deadline is that timestamp plus the configured grace. One small table in brig —

```sql
CREATE TABLE totp_enforcement_clock (
  user_id    uuid        NOT NULL PRIMARY KEY REFERENCES wire_user(id) ON DELETE CASCADE,
  started_at timestamptz NOT NULL
);
```

— written once per user, read on the enforcement check while the user is unenrolled, and deleted when an admin resets them (`TOTP 23`, which must restart the clock rather than leave a user instantly overdue) or, lazily, when they leave scope (`TOTP 25`).

**Not deleted on enrolment.** If enrolling cleared the row, a user could dodge enforcement indefinitely: enrol, delete the credential (`TOTP 12`), coast through a fresh grace period, re-enrol just before the deadline, repeat. Keeping the row means deleting your last credential under enforcement makes you overdue *immediately* — landing in the restricted session (or whichever design above is chosen), whose enrolment path is open by construction. The costs are one harmless stale row per enrolled user and one unfairness: someone who left the team mid-grace and rejoins much later reads as instantly overdue, which the admin reset covers.

The migration for this table lives with the others in `libs/wire-subsystems/postgres-migrations/`.

Why lazy beats computed-from-`enforcedSince`:

- it covers feature-enable, team-join and role-promotion with one rule;
- it needs nothing from galley that galley does not already expose;
- a user who never logs in never starts a clock, which is the behaviour you want anyway — their grace begins when they first show up.

The cost is a row per enforced-but-unenrolled user and the fact that an admin dashboard cannot show a deadline for someone who has not logged in since enforcement started. Show "not started" there (`TOTP 22`).

## Config

The grace period belongs in the galley feature config, not in brig's `TOTPOpts`: it is a per-team policy, not a backend-wide one.

```haskell
data TOTPConfig = TOTPConfig
  { enforced        :: Bool
  , gracePeriodDays :: Word32   -- 0 = immediate
  , enforcedFor     :: ...      -- TOTP 25
  }
```

`TOTP 19` should define all three fields when it introduces the feature, even if 24 and 25 give the latter two meaning — its own note says feature flags are cheap to add and expensive to change once a client depends on them.

## Client-visible signal

During grace, the user must be told, repeatedly and specifically. Add to the login response (and to `GET /self`, so an already-logged-in client finds out too):

```json
"second_factor_enrolment": { "required": true, "deadline": "2026-11-01T00:00:00.000Z" }
```

Absent, or `required: false`, for everyone not under enforcement — this must be additive and must not change any existing field (`TOTP 11`'s backwards compatibility rule applies unchanged).

## Acceptance criteria

- Unit tests with a settable `Now` for the deadline arithmetic: clock starts on first observation, not on feature enable; enrolling stops the chase but does **not** delete the row; deleting the last credential under enforcement → overdue immediately, no fresh grace (the evasion-loop test); an admin reset restarts it; `gracePeriodDays = 0` means overdue immediately.
- Integration tests across the matrix from `TOTP 19`: enforcing team × {enrolled, unenrolled within grace, unenrolled past grace} × {SSO, non-SSO} × {member, admin}:
  - within grace: login succeeds, the response carries the deadline, and the user can do everything they could before;
  - past grace: the chosen design's behaviour, **and in every case the user can still complete enrolment and then proceed normally** — this is the test that protects against the bootstrap trap;
  - enrolling during grace clears the signal (the clock row stays, see above);
  - disabling enforcement mid-grace releases everyone and leaves the enrolled ones enrolled.
- A test that a teamless user and a member of a non-enforcing team see no change whatsoever.
- `docs/`: the rollout runbook for an admin — enable with a grace period, watch `TOTP 22`'s dashboard, chase the stragglers, and what to do when someone is locked out anyway.
- `changelog.d/` entry.

## Notes

- The grace period is also the natural place for enrolment reminder emails. Out of scope here; note it for `TOTP 17`, which already owns notifications.
- Resist per-user grace extensions. They sound humane and they turn into a permanent exemption list that nobody ever audits. The admin reset (`TOTP 23`) already restarts the clock for genuine hardship cases, and it leaves an audit trail while doing it.
