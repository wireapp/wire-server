context:
- https://github.com/wireapp/wire-server/pull/3504
- https://wearezeta.atlassian.net/browse/WPB-3888

Connects to brig database.

Set up port-forwarding to brig database (hacky, slow, maybe dangerous), or run from a machine with access to those databases (preferred approach). Refer to ../service-backfill/ for an example. Then:

```sh
# assuming local port forwarding cassandra_galley on 2021 and cassandra_spar on 2022:
./dist/repair-brig-clients-table --cassandra-host-brig localhost --cassandra-port-brig 2022 --cassandra-keyspace-brig spar
```
