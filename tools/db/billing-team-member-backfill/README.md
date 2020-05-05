## billing_team_member backfill

A tool for filling table `billing_team_member` from existing data.

### How to run this

```sh
export GALLEY_HOST=...  # ip address of galley cassandra DB node
export GALLEY_KEYSPACE=galley

ssh -v -f ubuntu@${GALLEY_HOST} -L 2021:${GALLEY_HOST}:9042 -N

./dist/billing-team-member-backfill --cassandra-host-galley=localhost --cassandra-port-galley=2021 --cassandra-keyspace-galley=${GALLEY_KEYSPACE}
```
