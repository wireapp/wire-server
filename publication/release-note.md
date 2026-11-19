## Overview

5.39.0 is a feature release for operators running 5.38.0. The structured release declaration of this version is [`declaration.json`](declaration.json).

Coming from 5.37.x (5.37.2 or later), also read the [5.38.0 section of `CHANGELOG.md`](https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md) first: it has the release notes of 5.38.0, including the nginz `oauth_scope` deprecation.

### Highlights

**Meetings**: every meeting has a join link, backed by a join code stored in PostgreSQL, and joining by link needs a licence. Meeting emails are now sent by brig and carry an `.ics` calendar file. Calendar integrations can manage meetings, temporary guests can join, and participants who are removed receive a `meeting.delete` event. Several of these need action, see below.

**More data on PostgreSQL**: custom backends, login budgets, DPoP nonces and connections can move from Cassandra to PostgreSQL, with the same opt-in migration pattern as the other stores.

**Federation**: `contact_status` of `POST /list-users` now covers federated users.

**Admin and account fixes**: team admins can resend the verification email of a member, and SCIM-managed accounts are protected from role and email changes.

**Mock TOTP**: mock endpoints, an optional `totp_code` field and a disabled `mfa-totp` team feature let clients prepare for authenticator-app support. No second factor is checked yet.

**`ldap-scim-bridge` chart**: it is no longer published from wire-server, see below.

## Before upgrading

If `galley.config.postgresMigration.conversationCodes` is still `cassandra` (the chart default), move it to `migration-to-postgresql` before upgrading ([#5597](https://github.com/wireapp/wire-server/pull/5597)). In dual-write mode, guest links stay on Cassandra. Meeting join codes only live in PostgreSQL. Without this change, creating a meeting or refreshing its link fails with `500 link-unavailable`, and so does reading a meeting that has no live join code, on every API version. A join-link URI must stay configured in galley: `settings.conversationCodeURI`, or the user's domain in `settings.multiIngress`.

```yaml
galley:
  config:
    postgresMigration:
      conversationCodes: migration-to-postgresql
```

If you use meeting join links, set the top-level `license` value to a licence payload that covers invitation links ([#5661](https://github.com/wireapp/wire-server/pull/5661)). Without it, `POST /meeting/{domain}/{key}/{code}/join` and `GET /meeting/{domain}/{key}/{code}/code-check` return `403 id-feature-error`.

```yaml
license: "<base64url licence payload>"
```

If you send meeting emails, their sender, transport and branding now come from brig's `emailSMS` configuration, and the keys under `galley.config.settings.meetings.email` and `galley.secrets.smtpPassword` are no longer read ([#5606](https://github.com/wireapp/wire-server/pull/5606)). Emails are sent when `emailsEnabled` is true (or the legacy galley block is present) and the licence covers meeting emails.

```yaml
galley:
  config:
    settings:
      meetings:
        emailsEnabled: true
brig:
  config:
    emailSMS:
      general:
        meeting:
          replyTo: "noreply@example.com" # optional
```

To use calendar-managed meetings, upgrade nginz first, then list the OAuth client ids of the calendar integration in `galley.config.settings.meetings.calendarOAuthClients`, and have the integration refresh its access tokens ([#5682](https://github.com/wireapp/wire-server/pull/5682)). Nothing changes until that value is set.

At startup, SQL data migrations give every existing meeting without a live join code a canonical one (existing codes and links are never rotated) and store the guest access role of existing meeting conversations. Other SQL migrations add the columns of calendar-managed meetings and create the PostgreSQL tables of the new stores; the tables stay unused until you opt in.

## API changes

- From API version 19 every meeting carries its `link`. Refreshing a meeting link reports `has_password` according to the password supplied with the refresh. ([#5597](https://github.com/wireapp/wire-server/pull/5597))
- `GET /license` (API version 19) returns the meetings licensing flags `invitation_links`, `emails_sending` and `calendars_integration`, all false without a valid licence. ([#5671](https://github.com/wireapp/wire-server/pull/5671))
- Calendar-managed meetings (API version 19): `POST /meetings` takes an optional `externally_managed_by` with `externally_calendar_event_id` and `externally_calendar_id`. Only access tokens issued to a configured OAuth client can create or change them; they send no emails through Wire. ([#5682](https://github.com/wireapp/wire-server/pull/5682))
- Meeting conversations admit temporary guests: access updates on them are rejected with `403 invalid-op`. Participants removed from a meeting receive a `meeting.delete` event. ([#5666](https://github.com/wireapp/wire-server/pull/5666), [#5694](https://github.com/wireapp/wire-server/pull/5694))
- `PUT /teams/{tid}/members/{uid}/verification-email` resends the verification email of a team member with a pending unverified email address, for team admins: `202` when an email is sent, `204` when there is nothing to verify. ([#5681](https://github.com/wireapp/wire-server/pull/5681))
- Mock endpoints for TOTP second factors, from API version 19: `POST /self/totp`, `POST /self/totp/{id}/activate`, `GET /self/totp`, `PUT /self/totp/{id}`, `DELETE /self/totp/{id}`. They return generated data and store nothing; they exist for client development. ([#5653](https://github.com/wireapp/wire-server/pull/5653))
- Seventeen endpoints accept an optional `totp_code` field (`totp:123456` or `recovery:xxxx-xxxx-xxxx-xxxx`), among them `POST /login`, `DELETE /self`, `PUT /self/password`, `POST /clients` and `POST /auth-tokens`. The format is validated, the code is not checked. ([#5669](https://github.com/wireapp/wire-server/pull/5669))
- The `mfa-totp` team feature exists, disabled and locked by default, for interface testing only. ([#5689](https://github.com/wireapp/wire-server/pull/5689))
- Federation error responses include `federation_domain` and, when an SRV target is known, `srv_target` and `srv_target_domain`. The `domain` and `path` fields are unchanged. ([#5651](https://github.com/wireapp/wire-server/pull/5651))

## Federation

- `contact_status` in `POST /list-users?include-contact-status=true` covers federated users, through the new `get-users-contact-status` call of the brig federation API (development version 5). Requests to backends on a federation version below 5 log an error at level `Warn` until that backend is upgraded. ([#5693](https://github.com/wireapp/wire-server/pull/5693))
- Federated MLS sends require federation API version 5 on both backends. ([#5712](https://github.com/wireapp/wire-server/pull/5712))
- The federator ingress of `nginx-ingress-services` only exposes paths under `/federation`. ([#5355](https://github.com/wireapp/wire-server/pull/5355))

## Fixes

- Re-joining an MLS group with an external commit could drop the client from the backend's member list, silently stopping message delivery to it. ([#5677](https://github.com/wireapp/wire-server/pull/5677))
- MLS timestamp mismatches that broke reply quotes. ([#5712](https://github.com/wireapp/wire-server/pull/5712))
- Legacy Proteus team 1:1 conversations are no longer migrated in place as MLS groups. ([#5710](https://github.com/wireapp/wire-server/pull/5710))
- Legacy team 1:1 conversations are excluded from the prevention of admin-less groups. ([#5686](https://github.com/wireapp/wire-server/pull/5686))
- Changing the permissions of a team collaborator no longer removes them from conversations, and collaborators are removed from MLS 1:1 conversations when the collaboration ends. ([#5581](https://github.com/wireapp/wire-server/pull/5581))
- SCIM-managed accounts: team admins cannot change the role of a SCIM-managed user, users cannot delete their SCIM-managed email address, and SCIM role reads and updates for pending invitations are fixed. ([#5723](https://github.com/wireapp/wire-server/pull/5723))

## Removed

The `ldap-scim-bridge` Helm chart is no longer published from wire-server ([#5680](https://github.com/wireapp/wire-server/pull/5680)). It lives in [wireapp/ldap-scim-bridge](https://github.com/wireapp/ldap-scim-bridge/tree/dev/charts/ldap-scim-bridge) and is released there with its own version (6.0.0 onwards) at `oci://quay.io/wire/charts/stable/ldap-scim-bridge`. The 5.x charts already published stay available. The `openldap` test chart moved to `examples/openldap` in that repository.

## Configuration

PostgreSQL storage for more data ([#5633](https://github.com/wireapp/wire-server/pull/5633), [#5619](https://github.com/wireapp/wire-server/pull/5619), [#5675](https://github.com/wireapp/wire-server/pull/5675), [#5626](https://github.com/wireapp/wire-server/pull/5626)) follows the migration pattern of the [configuration options documentation](https://docs.wire.com/latest/developer/reference/config-options.html): `migration-to-postgresql`, then the background-worker backfill, then `postgresql`. Login budgets have no backfill: keep `budget` on `migration-to-postgresql` for at least the longest `limitFailedLogins.timeout` before switching.

| Value | Change | Default |
|---|---|---|
| `galley.config.postgresMigration.conversationCodes` | must leave `cassandra`, see above | `cassandra` |
| `galley.config.postgresMigration.customBackend`, `.budget`, `.nonce`, `.connections` | added; mandatory in the brig, galley and background-worker configuration, rendered by the chart | `cassandra` |
| `background-worker.config.migrateCustomBackend`, `.migrateNonce`, `.migrateConnections` | added | `false` |
| `license` | added; licence payload, applied to galley and background-worker | empty |
| `galley.config.settings.meetings.emailsEnabled` | added; galley asks brig to send meeting emails | `false` |
| `brig.config.emailSMS.general.meeting.replyTo` | added; Reply-To of meeting emails | none |
| `galley.config.settings.meetings.email`, `galley.secrets.smtpPassword` | deprecated or removed, see above | none |
| `galley.config.settings.meetings.calendarOAuthClients` | added; OAuth clients of the calendar integration | `[]` |
| `galley.config.settings.featureFlags.mfa-totp` | added; defaults of the `mfa-totp` team feature | disabled, locked |

## Monitoring

- Gundeck exports new Prometheus metrics on stored pushes (`wire_push_stored_*`), notification fetches (`wire_notification_fetch*`) and websocket deliveries (`wire_push_ws_delivered_presences*`). ([#5683](https://github.com/wireapp/wire-server/pull/5683))
