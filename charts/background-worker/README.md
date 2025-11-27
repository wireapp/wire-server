Note that background-worker depends on some provisioned storage/services, namely:

- rabbitmq
- postgresql
- cassandra (three clusters)

PostgreSQL configuration
- Set connection parameters under `config.postgresql` (libpq keywords: `host`, `port`, `user`, `dbname`, etc.).
- Provide the password via `secrets.pgPassword`; it is mounted at `/etc/wire/background-worker/secrets/pgPassword` and referenced from the configmap.

Cassandra configuration
- Background-worker connects to three Cassandra clusters:
  - `config.cassandra` (keyspace: `gundeck`) for the dead user notification watcher.
  - `config.cassandraBrig` (keyspace: `brig`) for the user store.
  - `config.cassandraGalley` (keyspace: `galley`) for conversation-related data access.
- TLS may be configured via either a reference (`tlsCaSecretRef`) or inline CA (`tlsCa`) for each cluster. Secrets mount under:
  - `/etc/wire/background-worker/cassandra-gundeck`
  - `/etc/wire/background-worker/cassandra-brig`
  - `/etc/wire/background-worker/cassandra-galley`

These are dealt with independently from this chart.
