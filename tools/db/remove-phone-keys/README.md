# Remove phone keys

This tool removes the phone keys from the `user_keys` table. Assuming `wire-server` is up and running, run the tool as laid out below.


```text
remove-phone-keys

Usage: remove-phone-keys [--brig-cassandra-host HOST]
                         [--brig-cassandra-port PORT]
                         [--brig-cassandra-keyspace STRING]

  Remove phone data from wire-server

Available options:
  -h,--help                Show this help text
  --brig-cassandra-host HOST
                           Cassandra Host for brig (default: "localhost")
  --brig-cassandra-port PORT
                           Cassandra Port for brig (default: 9042)
  --brig-cassandra-keyspace STRING
                           Cassandra Keyspace for brig (default: "brig_test")
```
