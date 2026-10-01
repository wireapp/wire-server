<a id="user-searchability"></a>

# User Searchability

This page explains which users a Wire user can find with the user search, and which settings control the result. The first part covers searches on the same backend. The second part covers searches on federated backends.

## Terms

- **Searcher**: the user who types the query.
- **Target**: a user who can appear in the result.
- **Exact-handle search**: the query is exactly the handle of the target. The query `mc` finds `@mc`, but not `@mccaine`. The result contains zero or one user.
- **Full-text search**: the query matches the beginning of a word in the display name or the handle of the target. The query `mar` finds `Marco C`, `Dr. Marina` and `@marek`, but not `Omar` or `@amaro`.
- **Outbound setting**: a setting that controls whom a searcher can find.
- **Inbound setting**: a setting that controls who can find a target.

Clients search with one endpoint, `GET /search/contacts`. For each query, the backend runs an exact-handle search and a full-text search, and returns the combined result. The settings on this page change which users the backend returns. Clients do not need to know the settings.

## Settings at a glance

| Setting | Direction | Applies to | Where to set it | Values |
|---|---|---|---|---|
| Team role | Who can search | One user | Team member API, SCIM `roles`, team invitation | `owner`, `admin` and `member` can search. `partner` cannot search. |
| `setSearchSameTeamOnly` | Outbound | Whole backend | brig configuration | `false` (default), `true` |
| Team search visibility | Outbound | One team | `PUT /teams/{tid}/search-visibility` | `standard` (default), `no-name-outside-team` |
| `searchVisibility` team feature | Allows a team admin to change the team search visibility | One team | Team feature API. Instance default: galley configuration key `teamSearchVisibility` | `enabled`, `disabled` (default) |
| `searchVisibilityInbound` team feature | Inbound | One team | Team feature API. Instance default: galley configuration | `disabled` (default), `enabled` |
| Per-user searchability | Inbound | One team member | `POST /users/{uid}/searchable` | `true` (default), `false` |
| `search_policy` | Inbound, federated searches only | One remote backend | brig federation configuration | `no_search`, `exact_handle_search`, `full_search` |

#### NOTE
The team search visibility and the `searchVisibility` team feature are two different settings. The team search visibility is the value that restricts the search (`standard` or `no-name-outside-team`). The `searchVisibility` team feature only decides whether a team admin is allowed to change that value.

## Who can search

A team member can search if the team role allows it. The roles `owner`, `admin` and `member` allow search. The role `partner` (External Partner) does not allow search: `GET /search/contacts` returns HTTP 403 to a partner. This applies to searches on the same backend and to federated searches. The permission to search is part of the role. To remove it from one user, change the role of that user to `partner`.

A user who is not a member of a team can always search. The results for such a user are different. Refer to [Outcomes on the same backend](#outcomes-on-the-same-backend).

## Whom a searcher can find (outbound)

### Team search visibility

A team admin or owner sets the team search visibility. The value applies to all members of the team when they search.

```default
GET /teams/{tid}/search-visibility
PUT /teams/{tid}/search-visibility

{"search_visibility": "no-name-outside-team"}
```

- `standard`: full-text search finds members of the own team, users who are not members of a team, and members of other teams that allow inbound search (refer to [`searchVisibilityInbound`](#searchvisibilityinbound-team-feature)).
- `no-name-outside-team`: full-text search finds members of the own team only.

The team search visibility does not change the exact-handle search. With both values, a searcher finds users of other teams by their exact handle.

A team admin can change the team search visibility only if the `searchVisibility` team feature is enabled for the team. If the feature is disabled, the `PUT` request fails with HTTP 403 and the error label `team-search-visibility-not-enabled`. When the feature is disabled for a team, the team search visibility of that team is reset to `standard`.

A team admin enables the feature for the team with the team feature API:

```default
PUT /teams/{tid}/features/searchVisibility

{"status": "enabled"}
```

The default of the `searchVisibility` team feature for all teams is set in the galley configuration:

```yaml
galley:
  config:
    settings:
      featureFlags:
        teamSearchVisibility: disabled-by-default # or enabled-by-default
```

This configuration key sets the default of the `searchVisibility` team feature. It does not set a default team search visibility. The default team search visibility is always `standard`.

### Backend-wide restriction: `setSearchSameTeamOnly`

If `setSearchSameTeamOnly` is `true`, each searcher on the backend finds only members of the own team. This applies to the exact-handle search and to the full-text search. A searcher who is not a member of a team finds only other users who are not members of a team.

The setting overrides the team search visibility of all teams. The backend applies it when it runs the search. It does not change the stored team search visibility of any team. If the setting is changed back to `false`, the team search visibility of each team applies again.

`setSearchSameTeamOnly` is stricter than `no-name-outside-team`. It also restricts the exact-handle search and the handle lookup (refer to [Handle lookup](#handle-lookup)).

To change the setting, edit the `values.yaml.gotmpl` file of the wire-server chart:

```yaml
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      setSearchSameTeamOnly: true
```

## Who can find a user (inbound)

<a id="searchvisibilityinbound-team-feature"></a>

### `searchVisibilityInbound` team feature

The `searchVisibilityInbound` team feature controls whether members of a team can be found by full-text search from other teams.

- `disabled` (default): only members of the same team find the members of this team with full-text search.
- `enabled`: members of other teams also find the members of this team with full-text search, if the outbound settings of the searcher allow it.

The feature does not change the exact-handle search.

A team admin sets the value for the team with the team feature API:

```default
PUT /teams/{tid}/features/searchVisibilityInbound

{"status": "enabled"}
```

The default for all teams is set in the galley configuration:

```yaml
galley:
  config:
    settings:
      featureFlags:
        searchVisibilityInbound:
          defaults:
            status: enabled # or "disabled" (default is "disabled")
```

#### NOTE
The backend stores the value with each user in the search index. A change of the default in the galley configuration does not update users that already exist. To apply a value to the existing members of a team, set the value for that team with the team feature API or the internal API. Each such request updates the search index entries of all members of the team.

An operator can read and set the value for a team with the internal galley API. Forward a local port to a galley pod:

```sh
kubectl -n wire get pods   # find the name of a galley pod
kubectl port-forward -n wire <galley-pod> 9000:8080
```

In a second terminal, read the current value:

```sh
curl -XGET http://localhost:9000/i/teams/<team-id>/features/searchVisibilityInbound
# {"lockStatus":"unlocked","status":"disabled"}
```

Set the value:

```sh
curl -XPUT -H 'Content-Type: application/json' -d '{"status": "enabled"}' \
  http://localhost:9000/i/teams/<team-id>/features/searchVisibilityInbound
```

The team ID is a UUID, for example `dcbedf9a-af2a-4f43-9fd5-525953a919e1`. The team settings app shows it.

### Per-user searchability

A team admin or owner can hide one member of the team from the search:

```default
POST /users/{uid}/searchable

{"set_searchable": false}
```

This endpoint is available from API version 12. It applies only to users who are members of a team.

If per-user searchability is `false`, `GET /search/contacts` does not return the user to any searcher. This includes members of the same team, and it applies to the exact-handle search and to the full-text search. Team admins still see the user in the team member list (`GET /teams/{tid}/search`, with the optional filter `searchable=false`).

The `stealthUsers` team feature tells clients whether to offer this option. The endpoint itself does not check the feature.

<a id="outcomes-on-the-same-backend"></a>

## Outcomes on the same backend

User `uA` searches for user `uB`. The table assumes that the role of `uA` allows search.

| Searcher `uA` | Target `uB` | `setSearchSameTeamOnly` | Team search visibility of `uA`'s team | `searchVisibilityInbound` of `uB`'s team | Exact-handle search | Full-text search |
|---|---|---|---|---|---|---|
| **Same team** | | | | | | |
| In team `tA` | In team `tA` | Irrelevant | Irrelevant | Irrelevant | Found | Found |
| **Per-user searchability `false`** | | | | | | |
| Any | Per-user searchability `false`, in any team | Irrelevant | Irrelevant | Irrelevant | Not found | Not found |
| **Target in another team** | | | | | | |
| In team `tA` | In team `tB` | `false` | `standard` | `enabled` | Found | Found |
| In team `tA` | In team `tB` | `false` | `standard` | `disabled` | Found | Not found |
| In team `tA` | In team `tB` | `false` | `no-name-outside-team` | Irrelevant | Found | Not found |
| In team `tA` | In team `tB` | `true` | Irrelevant | Irrelevant | Not found | Not found |
| **Target not in a team** | | | | | | |
| In team `tA` | Not in a team | `false` | `standard` | Not applicable | Found | Found |
| In team `tA` | Not in a team | `false` | `no-name-outside-team` | Not applicable | Found | Not found |
| In team `tA` | Not in a team | `true` | Irrelevant | Not applicable | Not found | Not found |
| **Searcher not in a team** | | | | | | |
| Not in a team | Not in a team | Irrelevant | Not applicable | Not applicable | Found | Found |
| Not in a team | In team `tB` | `false` | Not applicable | Irrelevant | Found | Not found |
| Not in a team | In team `tB` | `true` | Not applicable | Irrelevant | Not found | Not found |

If the role of `uA` is `partner`, the search fails with HTTP 403 for all targets.

<a id="handle-lookup"></a>

## Handle lookup

Two endpoints resolve a handle outside of `GET /search/contacts`.

`POST /list-users` with `qualified_handles` returns the profiles of up to four users by their handles. The profile contains the user ID. This endpoint does not check the team role of the searcher, the team search visibility, or the `searchVisibilityInbound` team feature. Only `setSearchSameTeamOnly` restricts it: if the setting is `true` and the searcher is a member of a team, the endpoint returns only members of the searcher's team.

`HEAD /handles/{handle}` only tells whether a handle is in use (HTTP 200) or free (HTTP 404). It returns no user data. None of the settings on this page apply to it.

## What these settings do not control

- **Search inside the own team.** Members of a team always find each other, unless the target has per-user searchability `false` or the searcher has the role `partner`.
- **Connections and conversations.** The settings on this page change only which users a search or a handle lookup returns. They do not prevent a connection request or a conversation with a user whose user ID is known.
- **The team member list for admins.** `GET /teams/{tid}/search` is available to team admins and lists all members of the team.

<a id="searching-users-on-another-federated-backend"></a>

## Searching users on another federated backend

User `uA` on backend A searches for user `uB` in team `tB` on backend B. Backend B decides which results to return:

- The `search_policy` that backend B has configured for backend A sets which kinds of search are allowed. An operator sets it with the internal brig API (refer to [Configure federation strategy (whom to federate with) in brig](configure-federation.md#configure-federation-strategy-in-brig)).
- The `searchVisibilityInbound` team feature of team `tB` applies to the full-text search.

The team role of `uA` applies: a searcher with the role `partner` cannot search. The outbound settings on backend A (`setSearchSameTeamOnly` and the team search visibility) do not apply to federated searches.

Two users on different backends are always in different teams, because a team cannot span more than one backend.

For a team to be found by full-text search from a federated backend, both conditions must be true:

- Backend B has set `search_policy` to `full_search` for backend A.
- Team `tB` has the `searchVisibilityInbound` team feature `enabled`.

### Table of possible outcomes

| `search_policy` of backend B for backend A | `searchVisibilityInbound` of team `tB` | Exact-handle search | Full-text search |
|---|---|---|---|
| `no_search` | Irrelevant | Not found | Not found |
| `exact_handle_search` | Irrelevant | Found | Not found |
| `full_search` | `disabled` | Found | Not found |
| `full_search` | `enabled` | Found | Found |

## Names in the code

This page uses the names of the API and of the configuration. The table maps them to the names in the source code.

| Name on this page | Name in the code |
|---|---|
| Permission to search | Hidden permission `SearchContacts`, derived from the team role |
| `setSearchSameTeamOnly` | `searchSameTeamOnly` (brig `Opts`, `UserSubsystemConfig`) |
| Team search visibility `standard`, `no-name-outside-team` | `TeamSearchVisibility`: `SearchVisibilityStandard`, `SearchVisibilityNoNameOutsideTeam` |
| `searchVisibility` team feature | `SearchVisibilityAvailableConfig`. Configuration values: `FeatureTeamSearchVisibilityAvailableByDefault`, `FeatureTeamSearchVisibilityUnavailableByDefault` |
| `searchVisibilityInbound` `disabled`, `enabled` | `SearchVisibilityInboundConfig`, stored in the search index as `SearchableByOwnTeam` (`searchable-by-own-team`), `SearchableByAllTeams` (`searchable-by-all-teams`) |
| Per-user searchability | User field `searchable`, request body `SetSearchable`, team feature `StealthUsersConfig` |
| `search_policy` | `FederatedUserSearchPolicy`: `NoSearch`, `ExactHandleSearch`, `FullSearch` |
