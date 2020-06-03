## count-team-owners

A tool for displaying some information about team users & team owners.

### How to run this

```sh
export GALLEY_HOST=...  # ip address of galley cassandra DB node
export GALLEY_KEYSPACE=galley

ssh -v -f ubuntu@${GALLEY_HOST} -L 2021:${GALLEY_HOST}:9042 -N

./dist/count-team-owners --cassandra-host-galley=localhost --cassandra-port-galley=2021 --cassandra-keyspace-galley=${GALLEY_KEYSPACE} > results.txt

cat results.txt | grep -v '#' | sort
```
