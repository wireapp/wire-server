<!-- Related issue: WPB-29361 -->

The meeting join-code backfill is now a one-off transactional SQL data migration (`20261019090000-meeting-join-code-backfill.sql`) that runs once per database at service startup and gives every meeting without a live join code a canonical one, so every meeting exposes a join `link`. Existing join codes and links are never rotated. The previous periodic galley background pass is removed.

Operators must run with `postgresMigration.conversationCodes: postgresql` (or `migration-to-postgresql`) and have a join-link URI configured (`settings.conversationCodeURI`, or the user's domain in the `multiIngress` map); otherwise meetings without a live join code (pre-link-feature rows) return `500 link-unavailable` on reads across all API versions (legacy V15–V18 shapes included; their response *shapes* are unchanged) until the migration has run at startup, and meeting creation fails with `500 link-unavailable` in misconfigured deployments.
