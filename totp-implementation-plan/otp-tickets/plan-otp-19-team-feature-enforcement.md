# TOTP 19 — team-level TOTP enforcement

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 11
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6 "Team-level enforcement".

Milestone 1.2. Galley-side; the only ticket in this set that is not primarily brig work.

## Scope

Milestone 1.1 makes TOTP per-user opt-in. This ticket lets a team *require* it.


### The feature

A new galley team feature, following the existing feature machinery (`libs/wire-api/src/Wire/API/Team/Feature.hs`, `Wire/API/Team/FeatureFlags.hs`, `Galley/API/Teams/Features.hs`). Three levels, as for every other feature, and they do different jobs:

- **status** (`enabled`/`disabled`) — whether the team has the feature at all.
- **lock status** (`locked`/`unlocked`) — whether the team may change it, or whether the site admin has pinned it.
- **config** — the parameters, which is where everything team-tunable lives.

```haskell
data TOTPConfig = TOTPConfig
  { enforced        :: Bool                 -- this ticket
  , enforcedFor     :: TOTPEnforcementScope -- TOTP 25; default "all"
  , gracePeriodDays :: Word32               -- TOTP 24; 0 = immediate
  }
instance IsFeatureConfig TOTPConfig where
  type FeatureSymbol TOTPConfig = "totp"
```

Lockable, with a server-wide default in config, like every other feature.

(`TOTPConfig` here is the team-feature config; brig's subsystem config is named `TOTPSubsystemConfig` (`TOTP 05`) precisely so the two cannot be confused in brig, which imports both.)

**Define all three config fields here**, even though `TOTP 24` and `TOTP 25` are what give the latter two behaviour. This ticket's own closing note is the reason: feature flags are cheap to add and expensive to change once a client depends on them, and a config that gains fields in three consecutive releases is three client migrations.

### Enforcement semantics — the hard part

The feature flag is easy; the rollout behaviour is where this ticket earns its keep. Decide and document:

- **What happens to a member of an enforcing team who has not enrolled?** They cannot have been locked out retroactively, or enabling the feature bricks the team. The answer is a grace period, and it is involved enough — including the bootstrap trap that a user who cannot log in cannot enrol — to have its own ticket: `TOTP 24`. This ticket should land the flag and the config shape; it must not ship *enforcement that bites* before 24 settles what biting looks like.
- **SSO / SAML users.** The existing `sndFactorPasswordChallenge` enforcement exempts SAML users (`enforceVerificationCodeImpl` checks `isSamlUser`), on the grounds that the IdP owns authentication. Does the same reasoning apply? It probably does, and it probably needs to be configurable for teams whose IdP does not do MFA.
- **Interaction with `sndFactorPasswordChallenge`.** A team could enable both. Define the combination rather than letting it fall out of the code.
- **Recovery codes under enforcement.** A printable bypass may undermine an enforced second factor; some teams will want them disabled. Coordinate with `TOTP 17`.
- **Disabling the feature** does not un-enrol anyone, and should not.

### Cross-service plumbing

Brig needs to read the feature for a user's team at login. The existing route is `GalleyAPIAccess.getAllTeamFeaturesForUser` / `getVerificationCodeEnabled`; add the analogue rather than inventing a new path.

## Acceptance criteria

- Feature appears in the team features API, respects lock status and the server-wide default, and round-trips through the feature config tests.
- Integration tests for the rollout matrix: enforcing team × {enrolled user, not yet enrolled user within grace, not yet enrolled past grace} × {SSO, non-SSO}.
- A personal (teamless) user is unaffected.
- Disabling enforcement leaves enrolled users enrolled and working.
- The client-visible signal for "you must enrol now" is documented in OpenAPI.
- `changelog.d/` entry.

## Notes

- Feature flags are cheap to add and expensive to change once a client depends on them. Settle the grace-period mechanics (`TOTP 24`) and the enforcement scope (`TOTP 25`) before implementing, not during — the config shape above already reserves room for both.
- This ticket does not need recovery codes or `TOTP 18` to land first, but it reads much better after both.
- `TOTP 22` gives admins the rollout dashboard they will immediately ask for once this flag exists, and `TOTP 23` the reset button they will ask for the day after.
