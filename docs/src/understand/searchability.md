(user-searchability)=

# User Searchability

You can configure how search is limited or not based on user membership in a given team.

There are two types of searches based on the direction of search:

- **Inbound** searches mean that somebody is searching for you. Configuring the inbound search visibility means that you (or some admin) can configure whether others can find you or not.
- **Out-Bound** searches mean that you are searching for somebody. Configuring the out-bound search visibility means that some admin can configure whether you can find other users or not.

There are different types of matches:

- **Exact handle** search means that the user is found only if the search query is exactly the user handle (e.g. searching for `mc` will find `@mc` but not `@mccaine`). This search returns zero or one results.
- **Full text** search means that the user is found if the search query contains some subset of the user display name and handle. (e.g. the query `mar` will find `Marco C`, `Omar`, `@amaro`)

## Searching users on the same backend

```{note}
For configuring searching accross federated backends this section is irrelevant.
```

Search visibility is controlled by three parameters on the backend:

- A team out-bound configuration flag, `TeamSearchVisibility` with possible values `SearchVisibilityStandard`, `SearchVisibilityNoNameOutsideTeam`

  - `SearchVisibilityStandard` means that the user can find other people outside of the team, if the searched-person inbound search allows it
  - `SearchVisibilityNoNameOutsideTeam` means that the user can’t find any user outside the team by full text search (but exact username search still works)

- A team inbound configuration flag, `SearchVisibilityInbound` with possible values `SearchableByOwnTeam`, `SearchableByAllTeams`

  - `SearchableByOwnTeam` means that the user can be found with full text search only by users in their own team
  - `SearchableByAllTeams` means that the user can be found with full text search by all users in any/all teams.

- A server configuration flag `searchSameTeamOnly` with possible values true, false.

  - `Note`: For the same backend, this affects inbound and out-bound searches (simply because all teams will be subject to this behavior)
  - Setting this to `true` means that all teams on that backend can only find users that belong to their team

These flag are set on the backend and the clients do not need to be aware of them.

The flags will influence the behavior of the search API endpoint; clients will only need to parse the results, that are already filtered for them by the backend.

Some configuration values supersede others. The table below clarifies how the various values interact with each other, highlighting the outcome of each search for the various combinations of values.

### Table of possible outcomes

```{eval-rst}
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Is search-er (`uA`) in team (tA)?  | Is search-ed (`uB`) in a team?  | Backend flag `searchSameTeamOnly`  | Team `tA`'s flag `TeamSearchVisibility`  | Team tB's flag `SearchVisibilityInbound`  | Result of exact search for `uB`  | Result of full-text search for `uB`  |
+====================================+=================================+====================================+==========================================+===========================================+==================================+======================================+
| **Search within the same team**                                                                                                                                                                                                                                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, the same team `tA`         | Irrelevant                         | Irrelevant                               | Irrelevant                                | Found                            | Found                                |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| **Out-Bound search unrestricted**                                                                                                                                                                                                                                          |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | false                              | `SearchVisibilityStandard`               | `SearchableByAllTeams`                    | Found                            | Found                                |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | false                              | `SearchVisibilityStandard`               | `SearchableByOwnTeam`                     | Found                            | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| **Out-Bound search restricted**                                                                                                                                                                                                                                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | true                               | Irrelevant                               | Irrelevant                                | Not found                        | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | Yes, another team tB            | false                              | `SearchVisibilityNoNameOutsideTeam`      | Irrelevant                                | Found                            | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
| Yes, `tA`                          | No                              | false                              | `SearchVisibilityNoNameOutsideTeam`      | There’s no team B                         | Found                            | Not found                            |
+------------------------------------+---------------------------------+------------------------------------+------------------------------------------+-------------------------------------------+----------------------------------+--------------------------------------+
```

### Changing the configuration on the server

To change the `searchSameTeamOnly` setting on the backend, edit the `values.yaml.gotmpl` file for the wire-server chart at this nested level of the configuration:

```yaml
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      setSearchSameTeamOnly: true
```

If `setSearchSameTeamOnly` is set to `true` then `TeamSearchVisibility` is forced be in the `SearchVisibilityNoNameOutsideTeam` setting for all teams.

### Changing the default configuration for all teams

If `setSearchSameTeamOnly` is set to `false`  (or missing from the configuration) then the default value `TeamSearchVisibility` can be configured at this level of the configuration of the `value.yaml.gotmpl` file of the wire-server chart:

```yaml
galley:
  #...
  config:
    #...
    settings:
      #...
      featureFlags:
        #...
        teamSearchVisibility: enabled-by-default
```

This default value applies to all teams for which no explicit configuration of the `TeamSearchVisibility` has been set.

(searching-users-on-another-federated-backend)=

## Searching users on another federated backend

- Setting the search policy for individual remote federated backends
  is done via a internal brig api end-points by a sysadmin (see
  {ref}`configure-federation-strategy-in-brig`}.

- The `SearchVisibilityInbound` setting applies. Since the default value for teams is `SearchableByOwnTeam` this means that for a team to be full-text searchable by users on a federating backend both

  - `FederatedUserSearchPolicy` needs to be set to to full_search for the federating backend
  - Any team that wants to be full-text searchable needs to be set to `SearchableByAllTeams`

- Out-Bound search restrictions (`searchSameTeamOnly`, `TeamSearchVisibility`) do not apply to federated searches



### Table of possible outcomes

In the following table, user `uA` on backend A is searching for user `uB` on team `tB` on backend B.

Any of the flags set for searching users on the same backend are ignored.

It’s worth nothing that if two users are on two separate backend, they are also guaranteed to be on two separate teams, as teams can not spread across backends.

| Who is searching       | Backend B flag `FederatedUserSearchPolicy` | Team `tB`'s flag `SearchVisibilityInbound` | Result of exact search for `uB` | Result of full-text search for `uB` |
| ---------------------- | ------------------------------------------ | ------------------------------------------ | ------------------------------- | ----------------------------------- |
| user `uA` on backend A | `no_search`                                | Irrelevant                                 | Not found                       | Not found                           |
| user `uA` on backend A | `exact_handle_search`                      | Irrelevant                                 | Found                           | Not found                           |
| user `uA` on backend A | `full_search`                              | SearchableByOwnTeam                        | Found                           | Not found                           |
| user `uA` on backend A | `full_search`                              | SearchableByAllTeams                       | Found                           | Found                               |

## Changing the settings for a given team

### TeamFeature searchVisibilityInbound

The team feature flag `searchVisibilityInbound` affects whether the team's users are searchable by users from other teams.

The default setting is `searchable-by-own-team` which hides users from search
results by users from other teams. If it is set to `searchable-by-all-teams`
then users of this team may be included in the results of search queries by
other users.


The default setting that applies to all teams on the instance can be defined at configuration.

```yaml
galley:
  config:
    settings:
      featureFlags:
        searchVisibilityInbound:
          defaults:
            status: enabled # or "disabled" (default is "disabled")
```

```{note}
Changing this setting in the instance configuration doesn't affect any users that have already been created. To affect these users please toggle the setting on a per-team basis (see below). Switching between "enabled" and "disabled" setting for the team causes a re-indexing of all the users of the team, thereby making the setting effective, e.g. changing to a "disabled" setting first, followed by changing to an "enabled" setting (or vice versa).
```

#### Overriding the default setting

Individual teams can overwrite the default setting with API calls:

To make API calls to set an explicit configuration for `SearchVisibilityInbound` per team, you first need to know the Team ID, which can be found in the team settings app.

It is an [UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) which has format like this  `dcbedf9a-af2a-4f43-9fd5-525953a919e1`.

In the following we will be using this Team ID as an example, please replace it with your own team id.

Next find the name of a `galley` pod by looking at the output of running this command:

```sh
kubectl -n wire get pods
```

The output will look something like this:

```
...
galley-5f4787fdc7-9l64n   ...
galley-migrate-data-lzz5j ...
...
```

Select any of the galley pods, for example we will use `galley-5f4787fdc7-9l64n`.

Next, set up a port-forwarding from your local machine's port `9000` to the galley's port `8080` by running:

```sh
kubectl port-forward -n wire galley-5f4787fdc7-9l64n 9000:8080
```

Keep this command running until the end of these instructions.

Please run the following commands in a separate terminal while keeping the terminal which establishes the port-forwarding open.

To see team's current setting run:

```sh
curl -XGET http://localhost:9000/i/teams/dcbedf9a-af2a-4f43-9fd5-525953a919e1/features/searchVisibilityInbound

# {"lockStatus":"unlocked","status":"disabled"}
```

Where `disabled` corresponds to `SearchableByOwnTeam` and enabled corresponds to `SearchableByAllTeams`.

To change the `SearchVisibilityInbound` to `SearchableByAllTeams` for the team run:

```sh
curl -XPUT -H 'Content-Type: application/json' -d "{\"status\": \"enabled\"}" http://localhost:9000/i/teams/dcbedf9a-af2a-4f43-9fd5-525953a919e1/features/searchVisibilityInbound
```

To change the `SearchVisibilityInbound` to `SearchableByOwnTeam` for the team run:

```sh
curl -XPUT -H 'Content-Type: application/json' -d "{\"status\": \"disabled\"}" http://localhost:9000/i/teams/dcbedf9a-af2a-4f43-9fd5-525953a919e1/features/searchVisibilityInbound
```

### Team searchVisibility

The team flag `searchVisibility` affects the out-bound search of user searches on the same backend. Federated searches are not affected by its setting.

If it is set to `no-name-outside-team` for a team then all users of that team will no longer be able to find users that are not part of their team when searching.

This also includes finding other users by providing their exact handle. By default it is set to `standard`, which doesn't put any additional restrictions to out-bound searches.

The setting can be changed via endpoint (for more details on how to make the API calls with `curl`, read further):

```
GET /teams/{tid}/search-visibility
  -- Shows the current TeamSearchVisibility value for the given team

PUT /teams/{tid}/search-visibility
  -- Set specific search visibility for the team

pull-down-menu "body":
  "standard"
  "no-name-outside-team"
```

The team feature flag `teamSearchVisibility` determines whether it is allowed to change the `searchVisibility` setting or not.

The default is `disabled-by-default`.

```{note}
Whenever this feature setting is disabled the `searchVisibility` will be reset to standard.
```

The default setting that applies to all teams on the instance can be defined at configuration

```yaml
settings:
  featureFlags:
    teamSearchVisibility: disabled-by-default # or enabled-by-default
```
