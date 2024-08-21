# Phone users

This program scans brig's users table and determines the number of users that can only login by phone/sms.

Example usage:

```shell
team-info --brig-cassandra-keyspace brig --galley-cassandra-keyspace galley -l 100000
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
                   [--galley-cassandra-keyspace STRING] [-l|--limit INT]

  This program scans brig's users table and determines the number of users that
  can only login by phone/sms

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
  -l,--limit INT           Limit the number of users to process
```
