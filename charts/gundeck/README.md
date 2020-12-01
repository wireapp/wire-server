Note that gundeck depends on some provisioned storage, namely:

- cassandra-all
- redis-gundeck

These are dealt with independently from this chart. Ensure the `config.redis.host` and `config.cassandra.host` point to valid dns names.
