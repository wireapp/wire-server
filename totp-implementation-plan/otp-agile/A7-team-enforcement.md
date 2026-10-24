# A7 — team enforcement, admin tooling, docs

**Demo:** a team admin turns on "require two-factor authentication". Members who have not enrolled are told to, with a grace period. An admin can see who has complied, and can reset a member who has lost everything.

Independent of A6. Depends only on A3. This is the largest remaining chunk and the only one that is itself worth re-cutting — treat the groups below as separate deliveries, not one step.

## Group 1 — make lockouts survivable

**Do this first, and do not ship general availability without it.** Everything else in this file is a feature; this group is the difference between a support queue and a support crisis.

- **Recovery-code regeneration and status** (`plan-otp-17`): how many codes remain, regenerate the set, and a notification when one is used. A4 deliberately shipped none of this, which left "user burns all ten codes" as an operator escalation. Fix that before the feature is general.
- **Admin reset** (`plan-otp-15`): `DELETE /i/users/:uid/totp`, for a user who has lost both their authenticator and their recovery codes. Needs an audit log line (`TinyLog` at `Info`, user id and request id) because it removes a second factor, and it must emit the `user.totp-remove` event (D17) — an ops action that silently diverges the user's UI from reality is worse than no event.
- **The `docs/` page** (`plan-otp-08`, docs half): the enrolment flow end to end, the `otpauth://` format with the explicit statement that clients render the QR, clock-skew diagnostics, what to tell users about storing recovery codes, the notification emails and what each means, and the **ops runbook for a locked-out user**. The runbook is the part that gets used.

## Group 2 — the team feature

- The galley feature `totp`, with `TOTPConfig { enforced, enforcedFor, gracePeriodDays }` (`plan-otp-19`). The full shape is settled now even though `plan-otp-24` and `25` are what give two of the fields meaning — defining it once avoids a config migration later. Note the deliberate naming split: galley's feature config is `TOTPConfig`, brig's subsystem config is `TOTPSubsystemConfig`, and brig imports both.
- **SSO members are exempt** (D15). They have no password and cannot enrol, so a team that enforces TOTP without this exemption locks out every SAML user. Say so in the feature's own documentation, not only in a ticket — "we turned on enforcement and our SSO users were locked out" is otherwise a guaranteed incident.
- Grace period (`plan-otp-24`) and the admin-specific variant (`plan-otp-25`).

## Group 3 — team-scoped surface

- `GET /teams/:tid/totp-status` (`plan-otp-22`). Must report SSO members as a **distinct third state**, not as "no second factor" — they are not non-compliant, they are out of scope. This needs the batch read `GetCredentialsForUsers` from the store.
- `DELETE /teams/:tid/members/:uid/totp` and `DELETE /teams/:tid/totp` (`plan-otp-23`). Both emit `user.totp-remove`.

All of these are `From 'V19`, documented in the commit that introduces them, and routed in `charts/nginz/values.yaml` under the same rules as the rest — no `oauth_scope`.

## The ordering argument

Group 1 is a release blocker; groups 2 and 3 are not. A backend can go to general availability with per-user opt-in TOTP, no team enforcement, and a working recovery story — that is a complete, useful feature. It cannot go to general availability with team enforcement and no recovery story, because enforcement is precisely what converts "I lost my phone" from a self-service problem into a mandatory one.

If this is re-cut further — and it should be — that is the constraint to preserve.

## Done when

Per the individual tickets. Two cross-cutting checks worth stating here because they span all three groups:

- A member of an enforcing team who is subject to the grace period, who then enrols, who then loses their authenticator, who then uses a recovery code, who then regenerates their codes, ends up in a coherent state at every step. That is one integration test and it is worth more than the sum of the per-ticket ones.
- Every path that removes a credential — user delete, admin reset, team revocation, account deletion cascade — emits the event and leaves no orphaned recovery codes.
