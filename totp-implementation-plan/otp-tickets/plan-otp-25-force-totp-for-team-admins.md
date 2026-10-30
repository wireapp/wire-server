# TOTP 25 — enforcement scoped to team admins and owners

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 19
- TOTP 24
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6 "Team-level enforcement".

Milestone 1.2.

## Why this is its own ticket

The high-value accounts in a team are the ones that can add members, change permissions, read the team settings and — after `TOTP 23` — strip other people's second factors. A team that is not ready to roll TOTP out to 500 people is usually very ready to require it of the five who can do those things, and that is a rollout step worth supporting directly rather than as "enforce for everyone and hope".

## Scope

Enforcement gets a scope instead of being all-or-nothing:

```haskell
data TOTPEnforcementScope
  = EnforceOwners            -- "owners"
  | EnforceAdminsAndOwners   -- "admins"
  | EnforceAllMembers        -- "all"

data TOTPConfig = TOTPConfig
  { enforced        :: Bool
  , enforcedFor     :: TOTPEnforcementScope   -- default "all"
  , gracePeriodDays :: Word32                 -- TOTP 24
  }
```

Default `all`, so a team that just flips `enforced` gets the behaviour `TOTP 19` described and nothing changes for anyone who configured it before this ticket. The scopes nest: `admins` includes owners, `all` includes everybody.

Map the scope onto the team roles rather than onto raw permissions — the roles are what an admin sees in the UI, and "every member who holds permission X" is a set no admin can predict. External partners and guests fall under `all` only.

## The part that will break if nobody thinks about it: role changes

A member promoted to admin moves into scope the moment the promotion lands, and a demoted admin moves out.

`TOTP 24`'s lazy clock handles this with no extra machinery: the enforcement check asks "is this user in scope **now**, and enrolled?", and starts the clock the first time the answer is in-scope-and-not-enrolled. A promotion therefore grants a full grace period from the promotion, which is the humane and correct behaviour, and it needs no hook in galley's role-update path.

Demotion is the one asymmetry worth stating: a demoted admin who had already enrolled stays enrolled (we never un-enrol anyone), and a demoted admin who was mid-grace simply stops being chased. Delete their clock row lazily, on the next check, rather than reaching for a galley event.

## Interaction with the rest

- **`TOTP 22`** (status dashboard): the response should make it possible to see who is *in scope*, not just who is enrolled — otherwise an admin cannot tell whether the five people showing `not_enrolled` matter. Either add the member's role to the item, or let the dashboard join against the member list it already pages. Prefer the join; do not duplicate role data in a TOTP response.
- **`TOTP 23`** (admin reset): an owner resetting their own second factor under `owners` scope puts themselves into a fresh grace period. That is correct and it is also the fastest route to a team with no MFA-capable owner; the confirmation parameter and the audit line are what stand between an admin and that outcome.
- **The lockout endgame.** Scoping enforcement to owners raises the stakes of a lost authenticator: the people who can fix it are exactly the people affected. Document the escape hatch chain explicitly — recovery code (`TOTP 14`) → another owner's reset (`TOTP 23`) → operator reset (`TOTP 15`). A team with exactly one owner and no recovery codes depends on the operator, and that fact belongs in the docs next to the knob, not in a support ticket at 2am.

## Acceptance criteria

- Feature config round-trips with all three scopes, respects lock status and the server-wide default, and an old config without `enforcedFor` reads as `all`.
- Integration tests, for each scope × {owner, admin, member, external partner}: in-scope unenrolled users are chased per `TOTP 24`, out-of-scope users see no change at all.
- Promotion of an unenrolled member to admin under `admins` scope starts a grace period from the promotion, not from feature-enable.
- Demotion stops enforcement for that user and leaves an enrolled user enrolled.
- A test that `enforcedFor` has no effect while `enforced = false`.
- `changelog.d/` entry; `docs/` section on choosing a scope, with the staged rollout (owners → admins → all) spelled out as the recommended path.

## Notes

- Resist adding "enforce for these specific user ids". Role-scoped enforcement is a policy; an id list is an exemption register, and it will be out of date within a month.
- If product later wants "enforce for anyone with permission to read customer data", that is a different feature built on permissions, and it should replace this enum rather than extend it.
