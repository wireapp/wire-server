# Client info

This program reads user UUIDs from a file and prints client information as CSV.

Example usage:

```shell
client-info --brig-cassandra-keyspace brig --input user-ids.txt
```

Filter by label/model (case-insensitive, comma-separated):

```shell
client-info --brig-cassandra-keyspace brig --input user-ids.txt --search-terms "ipad,iphone"
```

Display usage:

```shell
client-info -h
```

```text
client-info

Usage: client-info [--brig-cassandra-host HOST]
                   [--brig-cassandra-port PORT]
                   [--brig-cassandra-keyspace STRING] (-i|--input FILE)
                   [-s|--search-terms CSV]

  get client information for user ids

Available options:
  -h,--help                Show this help text
  --brig-cassandra-host HOST
                           Cassandra Host for brig (default: "localhost")
  --brig-cassandra-port PORT
                           Cassandra Port for brig (default: 9042)
  --brig-cassandra-keyspace STRING
                           Cassandra Keyspace for brig (default: "brig_test")
  -i,--input FILE          File containing user UUIDs (one per line)
  -s,--search-terms CSV    Comma-separated search terms to match label/model
                           (case-insensitive)
```
