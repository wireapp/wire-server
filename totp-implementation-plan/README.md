otp-tickets:
 - otp-plan.md (the master plan)
 - lots of tickets that are not very well cut IMO
 - some hacks to move date between jira and local markdown files
   (working, but the direction this is taking is not very promising,
   so i'm managing the documentation in this wire-server branch for
   now)

otp-agile:
 - newly cut tickets that should lead to faster and more incremental
   results.



product questions:

- should "name" be unique?
  Arguments for enforcing per-user name uniqueness:
   - The name is the only human-readable identifier; without uniqueness the settings UI shows indistinguishable duplicate entries
   - Adds a UNIQUE (user_id, name) DB constraint + a TOTPNameConflict error, checked at both enrolment and rename
   - Consistent with how the feature is positioned (D6: "Multiple authenticators, with a configurable cap")
  Arguments against:
   - Adds friction (user must invent unique names)
   - Could be handled client-side (append "(2)", etc.)
   - The UUID is always available as the true identifier
