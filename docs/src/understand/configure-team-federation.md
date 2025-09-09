# Federation Team Restriction

## Overview

Federation allows backends from different domains to communicate and exchange user data.
By default, federation can be configured at the **domain** level, but in some cases it is necessary to restrict communication further to specific **teams** within a remote domain.

The **team restriction** mechanism lets you define an allowlist of remote teams that are permitted to federate with your domain. Only users belonging to those teams will be discoverable or accessible across domains.

Typical deployment:

A customer runs an **on-premise instance** that needs to federate with **their team on Wire Cloud**, but not with other cloud teams or users.
The allow-list ensures that only the designated cloud team can communicate with the on-prem instance.

## Important Limitations

* If a domain is configured via the **static federation config file**, it cannot be modified in the database. That means you cannot:
  * Add or update the domain itself,
  * Add or update team restrictions for that domain.
    Changes require editing the config file and restarting the service.
* In the future, the config file will be deprecated and all configuration will live in the database.
* Private (non-team) users are not eligible for team-restricted federation. They will always be excluded if restrictions apply.
* Removing a team from the allow-list **does not break existing connections** — users already connected can still interact. Restrictions only apply to *new* searches and connection requests.

---

## API Reference

The following internal API endpoints are available for team restrictions:

### Add a Remote Team

```
POST /i/federation/remotes/:domain/teams
```

**Description:**
Allow federation with a specific team from the remote domain.

**Body:**

```json
{
  "team_id": "<TEAM_ID>"
}
```

---

### Get Allowed Remote Teams

```
GET /i/federation/remotes/:domain/teams
```

**Description:**
List all teams from the remote domain that are currently allowed to federate.

**Response Example:**

```json
[
  { "team_id": "2c41cd5a-6885-44c2-ae31-8d2bea5b59aa" },
  { "team_id": "5c78b3fa-1acc-444a-9ca9-df8a4703e603" }
]
```

---

### Delete a Remote Team

```
DELETE /i/federation/remotes/:domain/teams/:team_id
```

**Description:**
Remove a team from the allowlist. Once removed, users from that team will not be discoverable or accessible via federation.

---

## Behavior in Federated Search

The team restriction impacts **federated user search** results. Three modes are possible:

* **AllowAll** (no restriction):
  All teams from the remote domain are discoverable.

* **TeamAllowed** (allowlist):
  Only users from explicitly allowed teams are discoverable.
  You must add teams via `POST /i/federation/remotes/:domain/teams`.

**Example:**

* If `domain1` allows only `teamA` from `domain2`, then a user from `teamB` in `domain2` will **not** show up in `domain1`’s federated search.
* Exact handle search and full search both respect these restrictions.
