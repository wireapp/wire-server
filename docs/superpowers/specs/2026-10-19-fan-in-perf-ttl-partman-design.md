# fan-in-perf D: TTL garbage collection with pg_partman

Ticket: WPB-26288, experiment d. RFC: Notification fan-in, section "Event
Expiry". Previous experiments: A (produce), B (consume), C (consume-ack), see
`2026-10-08-…`, `2026-10-12-…`, `2026-10-14-…` specs and
`tools/fan-in-perf/README.md`.

## Intent

The RFC proposes TTL for notifications by partitioning the notification tables
by time and dropping whole partitions, because bulk `DELETE` plus `VACUUM` on
large tables is expected to hurt. Experiment d asks: what is the impact on C
(2000 clients pushing, consuming and acking) while TTL garbage collection
runs? If dropping partitions blocks writes or reads noticeably, the RFC's data
model is in trouble and we need to know early.

This round covers partitioned PostgreSQL using pg_partman
(<https://github.com/pgpartman/pg_partman>). Plain-PostgreSQL TTL (DELETE) is
a possible later round, not part of this work.

## Goals

- Run A, B, C's workload unchanged against partitioned notification tables while
  pg_partman creates and drops partitions every minute.
- fan-in-perf sets up everything experiment-specific itself: the dedicated
  database, pg_partman extension and role, background-worker settings, and the
  partitioned schema.
- Make partition maintenance visible in Grafana next to fan-in-perf's
  metrics, so dips in push/consume/ack rate or latency can be lined up with
  partition drops.
- Document how to run D and a plain-table baseline, and how to read the
  result.

## Non-goals

- Changing the production migration
  (`libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql`)
  or `Wire.FanInNotificationsStore`.
- New workload flags or a new workload sub-command: D is `consume-ack`.
- Handling readers lagging more than the retention. 5 minutes of lag is
  considered unrealistic; if it happens, dropped-unread rows simply show up as
  `missing` in the summary.
- Plain-PostgreSQL TTL, TimescaleDB or other alternatives.
- Production-like partition sizes (daily). Minute partitions are chosen so GC
  happens several times within one run.
- A fixed reference workload. Parameters stay flexible.

## Anti-goals

- A D result that measures something other than GC cost, e.g. because the
  baseline ran with different parameters, image or server config than D.
- Breaking the shared dev postgres for other services (brig, galley etc. use
  the same container and `backendA`).

## Decisions (by the user)

- **Partitioning:** 1-minute partitions, 5-minute retention, pg_partman
  maintenance every minute.
- **Implementation:** pg_partman.
- **Dev postgres:** the shared `deploy/dockerephemeral` postgres switches to an
  image with pg_partman, for everyone. No opt-in override; keep it simple.
- **Dedicated database:** experiments run in their own database, not
  `backendA`. (brig and galley run the fan-in migration with bookkeeping in
  `backendA` on startup; a partitioned schema there would make them fail.)
- **fan-in-perf creates the database and config** (no initdb script, no
  separate `psql` script).
- **SQL lives in `FanInNotificationsAdmin`** (`libs/wire-subsystems`): extend
  the effect and its PostgreSQL interpreter; the tool stays SQL-free.
- **pg_partman metrics:** collected by postgres-exporter (not by fan-in-perf).
- **Reader lag > retention:** ignored for now.
- **Workload parameters stay flexible.** D and its baseline must use identical
  parameters.

## Design

### Postgres image (`deploy/dockerephemeral`)

Only what needs a server restart goes here.

- Current image is `postgres:17-alpine`. Alpine has no PGDG package for
  pg_partman, so switch to a Debian-based `postgres:17` image built from a small
  Dockerfile that installs `postgresql-17-partman` from the PGDG apt repo
  (already configured in the official image). This is a base-OS change (musl
  → glibc); collations differ, which is fine for a dev DB.
- pg_partman version must be **≥ 5.5.0**: UUIDv7 support exists since 5.2.0,
  and 5.5.0 fixes SQL injection / privilege escalation CVEs in the time
  encoder/decoder path we use (CVE-2026-61781, -61817, -61818) and others.
  If PGDG doesn't ship ≥ 5.5.0 for PG 17, build from source in the
  Dockerfile at a pinned tag.
- Compose `command` adds `pg_partman_bgw` to `shared_preload_libraries` (next
  to `pg_stat_statements`). Nothing else partman-specific in compose.
- Without configured `pg_partman_bgw.dbname` the worker idles, so other
  services are unaffected until fan-in-perf configures it.

### fan-in-perf setup

New behaviour, exact CLI shape is the builder's call. Suggested:
`fan-in-perf --db $DB migrate --partitioned`, where `$DB` points at the
experiment database (default name `fan_in_perf`).

Steps, all idempotent so `migrate --partitioned` can be re-run:

1. **Database.** If the target database doesn't exist, create it.
   `CREATE DATABASE` can't run inside a transaction or against the database
   being created, so it needs a connection to another database on the same
   server (e.g. the `postgres` maintenance DB, derived from `--db` or given
   explicitly; builder's call). The dev user (`wire-server`) is superuser.
2. **Extension and role.** `CREATE EXTENSION IF NOT EXISTS pg_partman SCHEMA
partman`; create the non-superuser maintenance role for the bgw with the
   grants pg_partman's README prescribes. Do not run the bgw as superuser
   (5.5.0's default role name is `partman_maintainer`).
3. **Background worker config.** `ALTER SYSTEM SET` `pg_partman_bgw.dbname`
   (the experiment DB), `pg_partman_bgw.interval = 60`,
   `pg_partman_bgw.role`; then `SELECT pg_reload_conf()`. No restart needed.
   Builder verifies the bgw picks these up on reload; if one requires a
   restart, say so in the README.
4. **Schema.**
   - Plain `migrate` (baseline): unchanged, the production schema.
   - `migrate --partitioned` (D): all six `*_notifications` tables from the
     production migration with identical columns and primary keys, but
     `PARTITION BY RANGE (notification_id)`. Every PK already contains
     `notification_id`, which PostgreSQL requires for the partition key. Each
     registered with `partman.create_parent` as time-based partitioning on
     the UUIDv7 `notification_id` using pg*partman's UUIDv7 encoder/decoder,
     interval `1 minute`, enough premade future partitions to survive a missed
     maintenance run. Retention in `partman.part_config`: `retention = '5
minutes'`, `retention_keep_table = false` (drop, not detach). All other
     fan-in tables (`last*_`, `_\_acks`, `epoch_history`) plain, as in the
     production migration.
   - If the schema already exists in the other shape (plain vs. partitioned),
     fail with a clear message rather than silently keeping it.
5. **Switching shape** between D and baseline: drop and recreate the
   experiment database. Provide a way in the tool (e.g. `drop-db`
   sub-command, or `migrate --recreate`; builder's call). It drops the
   database named in `--db`; no name-based safety net (test DB, user
   decision).

`reset` stays as is: TRUNCATE on a partitioned parent clears its partitions,
so it works for both shapes.

### `FanInNotificationsAdmin` changes (`libs/wire-subsystems`)

Extend the effect and `Wire.FanInNotificationsAdmin.Postgres` with the
operations above (create database, set up partman extension/role/bgw config,
partitioned migrate, drop database, detect current shape). Partitioned DDL is
embedded like the existing `fanInSchema` (new SQL file next to the interpreter
or under `postgres-migrations/` but **not** picked up by `runAllMigrations`;
builder makes sure of that). Statements that can't run in a transaction
(`CREATE/DROP DATABASE`, `ALTER SYSTEM`) must run outside one.

Database and role names that end up in DDL are identifiers, not parameters:
validate them against a strict whitelist (e.g. `^[A-Za-z_][A-Za-z0-9_]{0,62}$`) and
quote them as identifiers; never interpolate unvalidated input. Unit-test the
validation (rejects quotes, semicolons, empty, too long, space, leading digit).

Retention timing: pg_partman drops a child once its _upper_ bound is older than
the retention. With 1-minute partitions and a 60 s maintenance interval, the
oldest remaining child's lower bound is roughly 6–8 min old. The first drop
happens about 6–7 min into a run.

### Metrics (postgres-exporter)

- Add custom queries to postgres-exporter for the fan-in partition sets in the
  experiment database, at least per parent table:
  - number of child partitions
  - age of the oldest child partition's lower bound (seconds)
  - `partman.part_config.last_run` / seconds since the last maintenance run
- The exporter's `DATA_SOURCE_NAME` points at `backendA` today; it must also
  (or instead) query `fan_in_perf`. Builder's call; queries must return
  nothing rather than fail while `fan_in_perf` or `partman` doesn't exist.
  Add `fan_in_perf` to the otel `postgresql` receiver's database list.
- Mechanism: postgres-exporter's custom queries file (`--extend.query-path` /
  `PG_EXPORTER_EXTEND_QUERY_PATH`). It's deprecated in recent releases and
  compose uses the unpinned `:latest` image: pin the exporter image to a
  version where it works, or use the replacement mechanism of that version.
  Builder verifies.
- Raise the `postgres-exporter` scrape interval in
  `docker/otel-collector-config.yaml` from 15 s to 1 s so drops are visible at
  fan-in-perf's resolution.
- Add panels to the "fan-in-perf" Grafana dashboard
  (`docker/grafana-dashboards/fan-in-perf.json`): partition count and oldest
  partition age per table, so a drop shows as a sawtooth step aligned with the
  push/consume/ack/latency panels.

### Running D (README section)

Add a "D: consume-ack under TTL GC" section to `tools/fan-in-perf/README.md`,
a design-spec link at the top, document the new commands/flags in "Commands
and parameters", and switch the README's `$DB` examples to the experiment
database.

1. One-time after the image change: the image declares an anonymous volume
   that `docker compose` keeps when recreating a container. Recreate with
   `docker compose rm -sfv postgres` then up (or `up -V`), and make sure the
   new image is built (`run.sh` does not pass `--build`).
2. `fan-in-perf --db $DB migrate --partitioned`.
3. `consume-ack` with any parameters and `--duration` ≥ 600 s so several drops
   happen after the first one at ~6–7 min.
4. Baseline: recreate the experiment DB, plain `migrate`, identical parameters
   and duration.
5. Compare: rates and latency percentiles in the `summary:` blocks, and in
   Grafana whether dips line up with partition drops.

Fill in "Results so far" with a D entry once run (not part of the build).

## Constraints

- AGENTS.md quality expectations: whole project type checks, no warnings,
  unit tests green (`wire-subsystems`, `fan-in-perf`), formatted, Cabal/Nix
  dependencies aligned.
- Security: no superuser bgw; pinned pg_partman ≥ 5.5.0; identifiers in DDL
  whitelisted and quoted; dev
  credentials stay the existing dev placeholders, never printed (existing
  `--db` behaviour); no secrets added.
- Other services must still start on the new postgres image.

## Left to the builder

- Exact Dockerfile (PGDG package vs. source build), image tag pinning.
- CLI shape (flag vs. sub-command), how the admin connection for `CREATE
DATABASE` is obtained.
- Exact `create_parent` arguments (premake count, encoder/decoder function
  names for the installed version), role name and grants.
- Effect constructor names and how partitioned DDL is embedded.
- Exporter version pin, query file format, single vs. second target; panel
  layout.

## Calls made by the designer (not decided by the user)

- All six `*_notifications` tables are partitioned; `last_*`, acks and
  `epoch_history` stay plain.
- bgw settings are set by fan-in-perf via `ALTER SYSTEM` + reload; compose
  only preloads the library.
- Experiment DB default name `fan_in_perf`; baseline uses the same DB,
  recreated, plain schema.
- Tool gets a way to drop/recreate the experiment DB (unguarded, per user).
- D is the existing `consume-ack` against the partitioned DB.
- Exporter scrape interval raised to 1 s.
