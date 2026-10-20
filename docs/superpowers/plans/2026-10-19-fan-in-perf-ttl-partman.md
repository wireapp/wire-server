# fan-in-perf D (TTL via pg_partman) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** fan-in-perf can set up a dedicated experiment database with pg_partman-partitioned notification tables (1-minute partitions, 5-minute retention, maintenance every 60 s), so A/B/C workloads run under TTL garbage collection, with partition metrics in Grafana.

**Architecture:** Dev postgres becomes Debian `postgres:17` + pg_partman ≥ 5.5.0 with `pg_partman_bgw` preloaded. All SQL lives in `Wire.FanInNotificationsAdmin` (effect + PostgreSQL interpreter in `libs/wire-subsystems`); fan-in-perf only orchestrates: create DB via an admin connection to the `postgres` DB, install/configure partman, migrate plain or partitioned. A second postgres-exporter, pointed at the experiment DB, exports partition metrics.

**Tech Stack:** Haskell (polysemy, hasql, optparse-applicative, hspec), PostgreSQL 17, pg_partman ≥ 5.5.0, docker compose, postgres-exporter, otel-collector, Grafana.

**Spec:** `docs/superpowers/specs/2026-10-19-fan-in-perf-ttl-partman-design.md`

## Global Constraints

- pg_partman **≥ 5.5.0** (CVE fixes in time encoder/decoder path). Partitioning: interval `1 minute`, `retention = '5 minutes'`, `retention_keep_table = false`, `pg_partman_bgw.interval = 60`.
- UUIDv7 functions: `partman.uuid7_time_encoder`, `partman.uuid7_time_decoder`.
- bgw never runs as superuser. Role name: `partman_maintainer` (constant, not user input).
- DB/role names in DDL: validated against `^[A-Za-z_][A-Za-z0-9_]{0,62}$`, then double-quoted as identifiers. Never interpolate unvalidated text.
- No name-based safety net (user decision: test DB). `migrate`/`drop-db` act on whatever dbname `--db` names, as long as it passes the identifier whitelist. Documented experiment DB: `fan_in_perf`.
- Production migration `20260729073800-fan-in-notifications.sql` and `Wire.FanInNotificationsStore` unchanged. New SQL files must **not** be under `postgres-migrations/` (picked up by `runAllMigrations` via `embedDir`).
- `--db` connection string is never printed.
- Every task ends with: `make c package=<changed pkg> test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` green, no warnings. Final task: `make format`, `make lint-all`, `make regen-local-nix-derivations` (only if deps changed), whole project `make c`.
- Do not run `*-integration` testsuites.

## Review Focus

1. `--db` without dbname, or with a dbname failing the whitelist (e.g. `x;y`, `a b`), passed to `migrate`/`drop-db` → refused with a clear message, nothing executed (Task 3 tests `experimentDb`).
2. `migrate --partitioned` on a DB already holding the plain schema (or vice versa) → fails with "exists with other shape" message, no DDL (Task 2 shape detection; Task 5 smoke step).
3. Re-running `migrate --partitioned` → succeeds, prints "already exist", role/extension/config statements idempotent (Task 2 SQL uses `IF NOT EXISTS` / guarded `DO` blocks; Task 5 smoke step).
4. `drop-db` while fan-in-perf or psql sessions are connected → still drops (`WITH (FORCE)`) (Task 2 rendered SQL test; Task 5 smoke).
5. Experiment DB missing or plain-shaped → existing `postgres-exporter`/dashboards unaffected; new exporter's partman queries return no rows instead of erroring once the DB exists (Task 4: separate exporter service; extension installed for both shapes).

---

### Task 1: `DbName` identifier type

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsAdmin.hs`
- Create: `libs/wire-subsystems/test/unit/Wire/FanInNotificationsAdminSpec.hs` (add to test-suite `other-modules` in `wire-subsystems.cabal` if modules are listed there)

**Interfaces:**
- Produces (exported from `Wire.FanInNotificationsAdmin`):
  - `newtype DbName = DbName {unDbName :: Text}` with `Eq, Show`; constructor **not** exported.
  - `mkDbName :: Text -> Either Text DbName` — Left with generic message `"invalid database name"` unless input matches `^[A-Za-z_][A-Za-z0-9_]{0,62}$`.
  - `quoteIdent :: DbName -> Text` — `"\"" <> name <> "\""`.

- [ ] **Step 1: Write failing tests** in `FanInNotificationsAdminSpec`:
  - `mkDbName "fan_in_perf"` and `mkDbName "_x1"` → Right; `quoteIdent <$> mkDbName "fan_in_perf" == Right "\"fan_in_perf\""`.
  - Right for `"backendA"`. Left for: `""`, `"a b"`, `"a;drop"`, `"a\"b"`, `"a'b"`, `"1abc"`, `"a-b"`, `T.replicate 64 "a"`. Right for `T.replicate 63 "a"`.
- [ ] **Step 2:** `cabal test wire-subsystems-tests --test-options='-m FanInNotificationsAdmin' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` → fails to compile (`mkDbName` not in scope).
- [ ] **Step 3:** Implement with `Data.Char` checks (no regex dependency).
- [ ] **Step 4:** Same command → PASS.
- [ ] **Step 5:** Commit `Add DbName identifier validation for fan-in admin`.

### Task 2: Admin effect operations and partitioned schema

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsAdmin.hs`
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsAdmin/Postgres.hs`
- Create: `libs/wire-subsystems/fan-in-sql/fan-in-notifications-partitioned.sql`
- Modify: `libs/wire-subsystems/wire-subsystems.cabal` (`extra-source-files: fan-in-sql/*.sql`)
- Test: `libs/wire-subsystems/test/unit/Wire/FanInNotificationsAdminSpec.hs`

**Interfaces:**
- Consumes: `DbName`, `quoteIdent` (Task 1).
- Produces (in `Wire.FanInNotificationsAdmin`):
  ```haskell
  data SchemaShape = Plain | Partitioned deriving (Eq, Show)
  data MigrateResult = Migrated | AlreadyMigrated | ExistsWithOtherShape SchemaShape deriving (Eq, Show)
  data FanInNotificationsAdmin m a where
    Migrate        :: SchemaShape -> FanInNotificationsAdmin m MigrateResult
    TruncateAll    :: FanInNotificationsAdmin m ()
    Ping           :: FanInNotificationsAdmin m ()
    DatabaseExists :: DbName -> FanInNotificationsAdmin m Bool   -- run on admin connection
    CreateDatabase :: DbName -> FanInNotificationsAdmin m ()     -- run on admin connection
    DropDatabase   :: DbName -> FanInNotificationsAdmin m ()     -- run on admin connection; IF EXISTS, WITH (FORCE)
    SetupPartman   :: DbName -> FanInNotificationsAdmin m ()     -- run on the experiment DB; arg = bgw dbname
  ```
  `makeSem` generates `migrate`, `truncateAll`, `ping`, `databaseExists`, `createDatabase`, `dropDatabase`, `setupPartman`.
- Produces (in `Wire.FanInNotificationsAdmin.Postgres`, exported for tests): `createDatabaseSql, dropDatabaseSql :: DbName -> Text`, `setupPartmanSql :: DbName -> [Text]`.

- [ ] **Step 1: Write failing tests** (pure rendering):
  - `createDatabaseSql n == "CREATE DATABASE \"fan_in_perf\""` for `n = fan_in_perf`.
  - `dropDatabaseSql n == "DROP DATABASE IF EXISTS \"fan_in_perf\" WITH (FORCE)"`.
  - `setupPartmanSql n` contains `"ALTER SYSTEM SET pg_partman_bgw.dbname = 'fan_in_perf'"`, `"ALTER SYSTEM SET pg_partman_bgw.interval = 60"`, `"ALTER SYSTEM SET pg_partman_bgw.role = 'partman_maintainer'"`, and its last element is `"SELECT pg_reload_conf()"`.
- [ ] **Step 2:** Run Task 1's test command → compile failure.
- [ ] **Step 3: Implement the effect + interpreter.**
  - `setupPartmanSql` order: `CREATE EXTENSION IF NOT EXISTS pg_partman SCHEMA partman` (preceded by `CREATE SCHEMA IF NOT EXISTS partman`); `DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'partman_maintainer') THEN CREATE ROLE partman_maintainer WITH LOGIN; END IF; END $$`; grants per pg_partman README (`ALL ON SCHEMA partman`, `ALL ON ALL TABLES IN SCHEMA partman`, `EXECUTE ON ALL FUNCTIONS/PROCEDURES IN SCHEMA partman`, `CREATE, USAGE ON SCHEMA public`, `TEMPORARY ON DATABASE <quoteIdent n>`); the three `ALTER SYSTEM` lines (dbname as a string literal: safe because `DbName` is whitelisted); `SELECT pg_reload_conf()`. Role has no password.
  - `CREATE/DROP DATABASE` and `ALTER SYSTEM` must run outside a transaction: execute each as its own `Session.script` (single statement → no implicit multi-statement transaction).
  - `DatabaseExists`: preparable statement `SELECT EXISTS (SELECT 1 FROM pg_database WHERE datname = $1)` with the name as a **parameter**.
  - Shape detection: `SELECT c.relkind::text FROM pg_class c WHERE c.oid = to_regclass('user_notifications')` → no row: absent; `"p"`: Partitioned; otherwise Plain.
  - `Migrate shape`: absent → run script (`Plain`: `fanInSchema`; `Partitioned`: `fanInSchema <> "\n" <> partitionedSchema` as **one** script = one implicit transaction) → `Migrated`; present with same shape → `AlreadyMigrated`; else `ExistsWithOtherShape existing`.
  - `partitionedSchema` embedded like `fanInSchema` from `fan-in-sql/fan-in-notifications-partitioned.sql`.
- [ ] **Step 4: Write `fan-in-notifications-partitioned.sql`.** Runs after the production schema in the same transaction on an empty DB:
  - `DROP TABLE` the six `*_notifications` tables; recreate each with identical columns/PK and `PARTITION BY RANGE (notification_id)`.
  - `ALTER TABLE … OWNER TO partman_maintainer` for the six parents (bgw must create/drop children; children inherit parent ownership). Designer's call.
  - Per table: `SELECT partman.create_parent(p_parent_table := 'public.<t>', p_control := 'notification_id', p_interval := '1 minute', p_premake := 10, p_time_encoder := 'partman.uuid7_time_encoder', p_time_decoder := 'partman.uuid7_time_decoder')`.
  - `UPDATE partman.part_config SET retention = '5 minutes', retention_keep_table = false, infinite_time_partitions = true WHERE parent_table IN (<six tables>)`.
  - Header comment: not a migration, used by fan-in-perf only; keep columns in sync with the production migration.
- [ ] **Step 5:** Run tests → PASS; `make c package=wire-subsystems | grep -vE '…'` → no warnings. Confirm `TruncateAll` table list still valid (unchanged names).
- [ ] **Step 6:** Commit `Add partman setup and partitioned migrate to fan-in admin`.

### Task 3: fan-in-perf CLI: `migrate [--partitioned]`, `drop-db`

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Database.hs`
- Modify: `tools/fan-in-perf/src/FanInPerf/Options.hs`, `src/FanInPerf/Run.hs`, `src/FanInPerf/Pool.hs`, `fan-in-perf.cabal`
- Test: `tools/fan-in-perf/test/FanInPerf/OptionsSpec.hs`, create `test/FanInPerf/DatabaseSpec.hs`

**Interfaces:**
- Consumes: Task 2's effect functions, `SchemaShape`, `MigrateResult`, `mkDbName`.
- Produces:
  - `FanInPerf.Options`: `data Command = Migrate SchemaShape | DropDb | Reset | Produce ProduceOptions | Consume ConsumeOptions`. `migrate` → `Migrate Plain`, `migrate --partitioned` → `Migrate Partitioned`, `drop-db` → `DropDb`.
  - `FanInPerf.Database`: `experimentDb :: ConnectionString -> Either Text DbName` (dbname present and passes `mkDbName`); `adminConnStr :: ConnectionString -> ConnectionString` = `c <> PostgresqlConnectionString.dbname "postgres"` (right-biased).
  - `FanInPerf.Pool`: `PoolRole` gains `AdminPool` (label `"admin"`).

- [ ] **Step 1: Write failing tests.**
  - OptionsSpec: replace `"parses migrate"` expectation with `Just (Migrate Plain)`; add `["--db","x","migrate","--partitioned"]` → `Just (Migrate Partitioned)`; `["--db","x","drop-db"]` → `Just DropDb`.
  - DatabaseSpec: `experimentDb` Right for `postgresql://u:p@h/fan_in_perf` and `…/my_test_db`; Left for no dbname `postgresql://u:p@h`, `…/fan_in_perf%3Bx` (decodes to `fan_in_perf;x`); Right for `…/backendA`. `toDbname (adminConnStr c) == Just "postgres"` and `toUser`/`toHosts` unchanged.
- [ ] **Step 2:** `cabal test fan-in-perf-tests | grep -vE '…'` (use the suite name from `fan-in-perf.cabal`) → compile failure.
- [ ] **Step 3: Implement.** `Run.run`:
  - `Migrate shape`: `name <- experimentDb` (abort with its message); admin pool (size 1, `AdminPool`) on `adminConnStr`; `databaseExists` → if not, `createDatabase` and print `created database <name>`; then experiment pool, `checkDatabase`, `setupPartman name` (both shapes: equal server config for D and baseline, and the exporter's partman queries never error), `migrate shape`, print per result: `created fan-in notification tables (<plain|partitioned>)`, `fan-in notification tables already exist`, or abort `fan-in notification tables exist with other shape (<shape>); run drop-db first`.
  - `DropDb`: `experimentDb`, admin pool, `dropDatabase`, print `dropped database <name>`.
  - Other commands unchanged (still `checkDatabase` first). `defaultPoolSize`: `Migrate _`, `DropDb` → 1.
  - Help text of `migrate`: mention it creates the database and configures pg_partman.
- [ ] **Step 4:** Tests PASS, no warnings.
- [ ] **Step 5:** Commit `fan-in-perf: create experiment DB, partitioned migrate, drop-db`.

### Task 4: Postgres image, exporter, otel, Grafana

**Files:**
- Create: `deploy/dockerephemeral/docker/postgres/Dockerfile`
- Modify: `deploy/dockerephemeral/docker-compose.yaml`
- Create: `deploy/dockerephemeral/docker/postgres-exporter-fan-in-perf/queries.yaml`
- Modify: `deploy/dockerephemeral/docker/otel-collector-config.yaml`
- Modify: `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json`

**Interfaces:**
- Produces: metric names used by the dashboard and README: `fan_in_partman_partitions{parent}`, `fan_in_partman_oldest_partition_age_seconds{parent}`, `fan_in_partman_seconds_since_maintenance{parent}` (final names depend on exporter's `<query>_<column>` scheme; keep these prefixes and update README with the actual ones).

- [ ] **Step 1: Dockerfile.** `FROM postgres:17` (pin a digest or minor tag), `apt-get install postgresql-17-partman` from PGDG. Verify version: `docker run --rm <image> bash -c "apt-cache policy postgresql-17-partman"` → ≥ 5.5.0; if not, build pg_partman from source at tag `v5.5.0` (or newer) instead.
- [ ] **Step 2: Compose.** `postgres` service: `build: ./docker/postgres` (drop `image: postgres:17-alpine`), command `shared_preload_libraries=pg_stat_statements,pg_partman_bgw`; nothing else partman-specific. Add service `postgres-exporter-fan-in-perf`: pinned `prometheuscommunity/postgres-exporter:<version>` whose `--extend.query-path` still works (check `--help`), DSN `…@postgres:5432/fan_in_perf?sslmode=disable`, query file mounted. Existing `postgres-exporter` untouched (designer's call: second exporter instead of re-pointing the first).
- [ ] **Step 3: `queries.yaml`.** One query `fan_in_partman`, labels `parent`, reading only catalogs + `partman.part_config` (no-row result when nothing is configured):
  - partitions: `count(*)` of `pg_inherits` children per `part_config.parent_table`, excluding the `_default` child.
  - oldest age: per parent, min over children of the lower bound in `pg_get_expr(c.relpartbound, c.oid)` (`FOR VALUES FROM ('<uuid>') TO …`); decode UUIDv7 ms: `('x' || substr(replace(<uuid>, '-', ''), 1, 12))::bit(48)::bigint / 1000.0`; age = `extract(epoch from now()) - that`.
  - seconds since maintenance: `extract(epoch from now() - maintenance_last_run)`.
- [ ] **Step 4: otel.** New scrape job `postgres-exporter-fan-in-perf`, `scrape_interval: 1s`, target `postgres-exporter-fan-in-perf:9187`; add `fan_in_perf` to the `postgresql` receiver's `databases`.
- [ ] **Step 5: Grafana.** Add a row "pg_partman" with two time series panels (partitions per parent, oldest partition age per parent), same time axis as existing panels.
- [ ] **Step 6: Verify.** `docker compose -f deploy/dockerephemeral/docker-compose.yaml build postgres` OK; `docker compose … rm -sfv postgres && docker compose … up -d`; `psql "$ADMIN" -c "SELECT default_version FROM pg_available_extensions WHERE name='pg_partman'"` ≥ 5.5.0; `psql "$ADMIN" -c "SHOW shared_preload_libraries"` contains `pg_partman_bgw`; brig/galley's DB `backendA` reachable as before.
- [ ] **Step 7:** Commit `dockerephemeral: postgres with pg_partman, fan-in partman exporter`.

### Task 5: End-to-end smoke and README

**Files:**
- Modify: `tools/fan-in-perf/README.md`

**Interfaces:**
- Consumes: Tasks 3–4 (commands, metric names).

- [ ] **Step 1: Smoke run** (`DB=postgresql://wire-server:posty-the-gres@localhost:5432/fan_in_perf`):
  - `fan-in-perf --db $DB migrate --partitioned` → `created database fan_in_perf`, `created … (partitioned)`. Re-run → `already exist` (Review Focus 3).
  - `psql $DB -c "SELECT parent_table, retention, partition_interval FROM partman.part_config"` → six rows, `5 minutes`, `00:01:00`. `psql … -c "SHOW pg_partman_bgw.dbname"` → `fan_in_perf` (if the bgw needs a restart to pick it up, record that in README).
  - `fan-in-perf --db $DB produce --targets user:1000,team:10,epoch:100 --duration 480`; during it `psql $DB -c "SELECT count(*) FROM pg_inherits WHERE inhparent='user_notifications'::regclass"` grows then stays flat; after ~7 min oldest child is dropped; postgres logs show no bgw errors. `curl -s localhost:<exporter port or via Grafana>` / Grafana shows the sawtooth.
  - `fan-in-perf --db $DB migrate` (plain) → aborts "exist with other shape (partitioned)" (Review Focus 2).
  - Keep a `psql $DB` session open, run `fan-in-perf --db $DB drop-db` → `dropped database fan_in_perf` (Review Focus 4). Then `migrate` (plain) → `created … (plain)`.
  - `fan-in-perf --db postgresql://wire-server:posty-the-gres@localhost:5432/fan_in_perf%3Bx drop-db` → refused "invalid database name", nothing dropped (Review Focus 1).
- [ ] **Step 2: README.** Per spec "Running D": spec link at top; `$DB` examples → `fan_in_perf`; commands table rows for `migrate [--partitioned]` and `drop-db` (drops the `--db` database, no confirmation); "D: consume-ack under TTL GC" section with the 5-step procedure (one-time `docker compose rm -sfv postgres` + rebuild since `run.sh` doesn't pass `--build`; `migrate --partitioned`; `consume-ack … --duration ≥ 600`; baseline = `drop-db` + plain `migrate` + identical parameters; compare `summary:` blocks and Grafana pg_partman row); retention timing note (first drop ~6–7 min in, oldest child 6–8 min old); metric names; note that readers lagging > 5 min lose rows (`missing`). Update Observability/Database schema sections for the second exporter and partitioned shape.
- [ ] **Step 3: Quality gates.** `make format`; `make lint-all`; `make c | grep -vE '…'` (whole project, no warnings); `make c package=wire-subsystems test=1 | grep -vE '…'` and `make c package=fan-in-perf test=1 | grep -vE '…'` green.
- [ ] **Step 4:** Commit `fan-in-perf: document experiment D`.
