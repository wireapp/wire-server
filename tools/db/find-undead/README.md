## Find certain inconsistencies between ES and Cassandra user data

Context: https://github.com/zinfra/backend-issues/issues/1493

This script identifies users that are still visible on ES, but are
marked as deleted on C*.

It outputs the time at which users have been marked as deleted in C*
so that you can decide whether what you are seeing may be a race
condition (eg., big team is being deleted while you run the script,
and users will be gone from ES a moment after you log them as
inconsistencies).

### How to run this

```sh
export BRIG_HOST=...  # ip address of galley cassandra DB node
export BRIG_KEYSPACE=brig

ssh -v -f ubuntu@${BRIG_HOST} -L 2021:${BRIG_HOST}:9042 -N

./dist/find-undead --cassandra-host-brig=localhost --cassandra-port-brig=2021 --cassandra-keyspace-brig=${BRIG_KEYSPACE}
```
