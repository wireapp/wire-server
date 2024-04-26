# Phone users

This program scans brig's users table and determines the number of users that can only login by phone/sms.

Example usage:

```shell
phone-users -k brig -l 1000
```

Display usage:

```shell
phone-users -h
```

```text
phone-users

Usage: phone-users [-s|--cassandra-host HOST] [-p|--cassandra-port PORT]
                   [-k|--cassandra-keyspace STRING] [-l|--limit INT]

  This program scans brig's users table and determines the number of users that
  can only login by phone/sms

Available options:
  -h,--help                Show this help text
  -s,--cassandra-host HOST Cassandra Host (default: "localhost")
  -p,--cassandra-port PORT Cassandra Port (default: 9042)
  -k,--cassandra-keyspace STRING
                           Cassandra Keyspace (default: "brig_test")
  -l,--limit INT           Limit the number of users to process
```
