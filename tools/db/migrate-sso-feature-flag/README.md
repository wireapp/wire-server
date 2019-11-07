A clone of ../service-backfill/ that enables the SSO feature flag for
all teams that already have an IdP.

Connects to galley and spar databases.

Set up port-forwarding to spar and galley databases (hacky, slow, maybe dangerous), or run from a machine with access to those databases (preferred approach). Refer to ../service-backfill/ for an example. Then:

```sh
# assuming local port forwarding cassandra_galley on 2021 and cassandra_spar on 2022:
./dist/migrate-sso-feature-flag --cassandra-host-spar localhost --cassandra-port-spar 2022 --cassandra-keyspace-spar spar --cassandra-host-galley localhost --cassandra-port-galley 2021 --cassandra-keyspace-galley galley
```
