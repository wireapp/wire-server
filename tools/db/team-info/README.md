# Team info

This program scans brig's and galley's cassandra for members of a team, their clients, and those clients' last access times.

Useful for finding out which accounts you don't want to pay license fees any more.

Example usage:

```shell
team-info \
  --brig-cassandra-port 9048 --brig-cassandra-keyspace brig \
  --galley-cassandra-port 9049 --galley-cassandra-keyspace galley \
  --team-id=904912aa-7c10-11ef-9c85-8bfd758593f6
```

Display usage:

```shell
team-info -h
```

```text
team-info

Usage: team-info [--brig-cassandra-host HOST] [--brig-cassandra-port PORT]
                 [--brig-cassandra-keyspace STRING]
                 [--galley-cassandra-host HOST] [--galley-cassandra-port PORT]
                 [--galley-cassandra-keyspace STRING] (-t|--team-id ID)

  get team info

Available options:
  -h,--help                Show this help text
  --brig-cassandra-host HOST
                           Cassandra Host for brig (default: "localhost")
  --brig-cassandra-port PORT
                           Cassandra Port for brig (default: 9042)
  --brig-cassandra-keyspace STRING
                           Cassandra Keyspace for brig (default: "brig_test")
  --galley-cassandra-host HOST
                           Cassandra Host for galley (default: "localhost")
  --galley-cassandra-port PORT
                           Cassandra Port for galley (default: 9043)
  --galley-cassandra-keyspace STRING
                           Cassandra Keyspace for galley
                           (default: "galley_test")
  -t,--team-id ID          Team ID
```
