# fan-in-perf: Notification fan-in DB performance tool — Design

Ticket: WPB-26288 (Spike: validate and benchmark notification fan-in RFC's model
and algebra). RFC: "2026-04-14 RFC Notification fan-in".

## Goal

Answer "can PostgreSQL carry the fan-in notification model?" by measuring
maximal rates against a real database, through the production access layer
(`Wire.FanInNotificationsStore`), not through ad-hoc SQL.

Experiments (ticket):

- **A** max rate of adding notifications — *this spec, built end-to-end first*
- **B** max rate of adding + consuming by ~2,000 clients — later
- **C** like B, clients acknowledge everything — later
- **D** impact of TTL garbage collection on C — later

Design must leave room for B–D (sub-commands, consume metrics, store ops) but
implements only A plus shared infrastructure.

## Non-goals

- Simulating websockets/cannon/gundeck. Writers model backend server threads
  sharing a connection pool; the tool exercises the DB, not client bookkeeping.
- Mixed-kind pushes (see "One kind per push").
- Remote-domain connection targets (local only for now).

## Package & layout

New package `tools/fan-in-perf/` (executable `fan-in-perf`), library + thin
`app/Main.hs`, following `tools/rabbitmq-consumer`. Cabal + `default.nix`
aligned via `make regen-local-nix-derivations`.

| Module | Responsibility |
|---|---|
| `FanInPerf.Options` | optparse-applicative parsers, global + sub-command options |
| `FanInPerf.Targets` | `--targets` grammar parser, stream key pre-generation, push generation (pure where possible) |
| `FanInPerf.Stats` | per-writer `MutablePrimArray` counters, latency bucketing, snapshot aggregation, rate/max/ratio/quantile math (pure core) |
| `FanInPerf.Metrics` | prometheus-client registration, custom histogram `Metric`, warp `/metrics` server |
| `FanInPerf.Terminal` | status line formatting (pure) + in-place redraw / non-TTY fallback |
| `FanInPerf.Run` | pool creation, Polysemy stack, interpreter wiring |
| `FanInPerf.Produce` | experiment A: writers, ticker, lifecycle |

## CLI

Parsed with optparse-applicative.

Global options:

| Flag | Default | Meaning |
|---|---|---|
| `--db CONNSTR` | required | PostgreSQL connection string (never logged) |
| `--pool-size N` | `--writers` (produce) / 1 (reset) | hasql pool size |
| `--metrics-port P` | 9300 | `/metrics` HTTP port, bound to `0.0.0.0` |
| `--domain D` | `example.com` | local domain (`Input (Local ())` for the store) |
| `--isolation read-committed\|serializable` | `read-committed` (RFC intent) | isolation level for push transactions |

Sub-commands:

- `reset` — truncate all fan-in tables via `FanInNotificationsAdmin`.
- `produce` — experiment A:

| Flag | Default | Meaning |
|---|---|---|
| `--writers W` | 16 | concurrent writer threads (closed loop) |
| `--targets SPEC` | required | target mix, see below |
| `--clients-per-user C` | 1 | client ids per `clients` target |
| `--payload-bytes B` | 512 | approx. JSON payload size |
| `--duration SECS` | unset = until Ctrl-C | run length |
| `--warmup SECS` | 5 | ignored for max-rate tracking |

Later: `consume` (B), `consume-ack` (C), TTL variant (D).

### `--targets` grammar

```
SPEC    = ENTRY ("," ENTRY)*
ENTRY   = KIND ":" STREAMS ("x" K)?
KIND    = user | clients | team | epoch | connections
STREAMS = positive int: distinct stream keys pre-generated for this entry
K       = positive int, K <= STREAMS, default 1: targets per push
```

| KIND | Target constructor | Stream key (table) |
|---|---|---|
| `user` | `TargetUser uid` | `user_id` (`user_notifications`) |
| `clients` | `TargetUserClients (uid, cids)` | `(user_id, client_id)` (`client_notifications`) |
| `team` | `TargetTeam tid` | `team_id` (`team_notifications`) |
| `epoch` | `TargetEpoch (gid, epoch)` | `(group_id, epoch)` (`epoch_notifications`) |
| `connections` | `TargetConnections (Qualified uid localDomain)` | `user_id` (`local_connection_notifications`) |

For `clients`, K counts users; each target carries `--clients-per-user`
client ids from a fixed per-user set.

Rejected: unknown kind, `STREAMS = 0`, `K = 0`, `K > STREAMS`, duplicate kind,
empty spec, malformed numbers.

Per push a writer: picks one entry uniformly at random → picks K distinct
stream keys from it → calls `pushViaFanIn` with those K targets.

Examples:

- `team:10` — hot rows: 10 `last_team_notifications` rows, high contention.
- `user:100000` — spread writes, low contention.
- `user:1000x20,team:10` — half "conv event to 20 of 1000 users", half "team
  event to 1 of 10 teams".

### One kind per push (decision)

Every push carries targets of exactly one constructor. Analysis of today's
push construction sites shows this holds everywhere except
`Brig.IO.Intra.notifyContacts` (self + connections + team in one push), which
under fan-in becomes up to three pushes. The store type `[Target]` still
permits mixing; the tool enforces the rule, the type is not changed for the
spike.

## Data flow (`produce`)

1. Parse options and `--targets`; fail fast on errors.
2. Parse `--db` with `PostgresqlConnectionString.parse` (generic error
   message, never echo input) and create the pool via new
   `Hasql.Pool.Extended.initPostgresPoolFromConnString`, factored out of
   `initPostgresPool` (same metrics/instrumentation).
3. Probe DB once via `FanInNotificationsAdmin.Ping`; on failure print generic
   message, exit 1.
4. Pre-generate stream keys per entry into immutable vectors (random UUIDs,
   group ids, epochs, client ids). Pre-build payload `Object` once.
5. Start metrics server, ticker thread, W writer threads (`async`).
6. On `--duration` expiry or SIGINT: cancel writers, run a final tick, print
   summary, exit 0.

Writer loop:

```
forever:
  push    <- genPush rng entries        -- pure indexing into vectors
  t0      <- getMonotonicTimeNSec
  result  <- runStore (pushViaFanIn push)
  t1      <- getMonotonicTimeNSec
  record stats (ok|error kind, K, t1 - t0)
  on error: non-blocking enqueue of message to bounded error queue (drop if full)
```

Each writer runs the Polysemy stack per push:
`Input Pool`, `Input (Local ())`, `Error UsageError`, `Embed IO` +
`interpretFanInNotificationsStoreToPostgres isolation`.

## Stats: non-blocking by construction

Requirement: metric updates and terminal output never slow writers down.

- Each writer owns a `MutablePrimArray RealWorld Int` (package `primitive`).
  Layout: `[pushes, targets, errors_by_kind…, latency_bucket_0 .. latency_bucket_N]`,
  padded to a multiple of 64 bytes so writers don't false-share cache lines.
- Owner increments with `fetchAddIntArray` (uncontended atomic, no allocation).
- Ticker reads with `atomicReadIntArray`. Cells are independent; a snapshot may
  be off by one push across fields — acceptable at 1 s granularity.
- Latency buckets: log2 of nanoseconds (bit ops), covering ~1 µs .. ~60 s.
- Only the ticker thread touches prometheus-client and stdout. A slow terminal
  delays the ticker, never writers.
- `/metrics` scrapes read ticker-published state only.
- Exception: `wire_hasql_pool_*` metrics are updated inside `Hasql.Pool.Extended`
  per session (existing behaviour, minor contention, accepted).

## Ticker (every 1 s)

Sums all writer arrays, diffs against the previous snapshot, computes:

- push rate current, push rate max (running max, ignoring first `--warmup` s)
- targets rate current
- error rate current (errors/s) and error ratio (errors / (pushes + errors)
  over the last tick; 0 when denominator is 0)
- p50 / p99 latency from merged buckets (cumulative over the run)

Publishes to prometheus metrics and redraws the status line.

## Metrics

Prefix `fanin_perf_`, label `experiment="produce"`; `kind` label where noted.

| Metric | Type |
|---|---|
| `pushes_total{kind}` | counter |
| `targets_total{kind}` | counter |
| `errors_total{kind}` | counter |
| `push_duration_seconds` | histogram (custom `Metric`, buckets from ticker snapshot) |
| `push_rate_current`, `push_rate_max` | gauge |
| `error_rate_current`, `error_ratio_current` | gauge |
| `writers` | gauge |
| `wire_hasql_pool_*` | existing pool metrics |

B/C will add `consume_*` / `ack_*` with the same pattern.

## Terminal output

One status line, redrawn in place (`\r` + `ESC[2K`, no newline):

```
t=42s push/s cur=8 312 max=9 105 | err/s cur=3 (0.04%) | targets/s cur=41 560 | p50=3.1ms p99=12.4ms
```

Error messages (drained from the bounded queue, rate-limited) and the final
summary clear the line, print, then the next tick redraws. If stdout is not a
TTY (`hIsTerminalDevice`), print one line per tick instead.

Final summary: duration, total pushes/targets/errors, error ratio, average and
max push rate, p50/p99.

## Grafana LGTM integration

Tool runs on the host; OTel collector runs in docker and scrapes it.

- `deploy/dockerephemeral/docker-compose.yaml`: `otel-collector` gets
  `extra_hosts: ["host.docker.internal:host-gateway"]`.
- `deploy/dockerephemeral/docker/otel-collector-config.yaml`: prometheus
  receiver scrape job `fan-in-perf`, target `host.docker.internal:9300`,
  interval 5 s.
- `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json`,
  mounted like `postgres-exporter.json`. Panels: push rate
  (`rate(fanin_perf_pushes_total[15s])` + tool gauges current/max), error rate
  by kind + ratio, latency p50/p99, pool in-use/ready, targets rate.

Caveat: host firewall must allow docker bridge → host port 9300. (user handles this.)

## Store changes (`libs/wire-subsystems`)

1. `interpretFanInNotificationsStoreToPostgres :: IsolationLevel -> InterpreterFor FanInNotificationsStore r`.
   Transactions keep `runTransactionWithRetry` (retries only admin errors).
   Serialization failures are retried silently inside hasql-transaction; they
   surface as latency and in postgres-exporter's `pg_stat_database_xact_rollback`.
2. `genNotificationId = Id <$> UUIDv7.genUUID`. `Data.UUID.V7.genUUID`
   (mmzk-typeid) already returns `Data.UUID.Types.UUID`; the show/parse
   roundtrip was the identity and the retry branch was dead — same semantics.
3. `group_id` as `bytea`: MLS `GroupId` wraps arbitrary bytes, but the
   migration declared `group_id text` and `pushEpochNotification` used
   `TE.decodeUtf8` (throws on non-UTF-8). Fix in the branch-local migration
   `20260729073800-fan-in-notifications.sql`: `group_id bytea` in
   `epoch_notifications`, `epoch_history`, `last_epoch_notifications`,
   `epoch_notification_acks` (matches `conversation.group_id bytea`). The
   store passes the raw `ByteString`. User re-creates the schema and
   regenerates `postgres-schema.sql` (needs docker).
4. New test/perf-only effect `Wire.FanInNotificationsAdmin`:

   ```haskell
   data FanInNotificationsAdmin m a where
     TruncateAll :: FanInNotificationsAdmin m ()
     Ping :: FanInNotificationsAdmin m ()
   ```

   Postgres interpreter `Wire.FanInNotificationsAdmin.Postgres`: `Ping` runs
   `select 1`; `TruncateAll` runs one `TRUNCATE` over all tables of migration
   `20260729073800-fan-in-notifications.sql` (notifications, `last_*`, acks,
   `epoch_history`). Comment links table list to the migration.

## Spec-adherence review of `FanInNotificationsStore`

| Item | Status |
|---|---|
| `group_id text` + `decodeUtf8` breaks on binary MLS group ids | ❌ fixed: `bytea` |
| `GREATEST` upserts on `last_*` | ✅ matches RFC |
| One UUIDv7 notification id per push, shared across targets | ✅ |
| Schema PK fixes vs RFC (`last_local_connection_notifications`, `team_notification_acks`) | ✅ sensible deviations |
| Serializable isolation vs RFC intent (Read Committed suffices for `GREATEST` upserts) | ⚠ made configurable |
| Read / ack / init-acks / expiry operations | ⚠ missing; added with B/C/D |
| `[Target]` allows mixed kinds | ⚠ accepted for spike; tool enforces one kind |
| `genNotificationId` roundtrip | ⚠ simplified (same semantics) |

RFC findings to report (out of tool scope):

- `notifyContacts` mixes user + connections + team → split into up to 3 pushes.
- MLS `propagateMessage` excludes the sender's client; `TargetEpoch` has no way
  to express that exclusion.
- `removeIfLargeFanout` counts users; no equivalent for team/connection targets.

## Error handling

- Invalid CLI / `--targets` → optparse error, exit 2.
- DB unreachable at startup → generic message, exit 1.
- Errors during the run → counted per kind, logged rate-limited, run continues.
- SIGINT → graceful stop with summary, exit 0.
- Connection string / password never printed.

## Testing

Unit test-suite `fan-in-perf-tests` (hspec + QuickCheck):

- `--targets` parser: valid specs, each rejection case.
- Push generation property: exactly one constructor kind per push, K distinct
  targets, all keys from the entry's pre-generated pool; `clients` targets carry
  `--clients-per-user` ids.
- Stats math: snapshot diffs, max with warmup, error ratio with zero
  denominator, quantiles from buckets, bucket index function bounds.
- Status line formatting.

`wire-subsystems` unit tests stay green. Store DB behaviour has no unit
coverage; acceptance check (manual, with user's consent, against the running
DB): `fan-in-perf --db … reset` then `produce --targets team:10,user:1000x20 --duration 10`
— status line updates, `/metrics` serves `fanin_perf_*`, Grafana dashboard
shows data.

## Security

- No secrets in code; password only via `--db`; never logged.
- `/metrics` exposes counters only — no payloads, ids, or connection details.
- Generic error messages to terminal; DB error details only in rate-limited
  error log lines without connection info.
- `TruncateAll` lives in a separate admin effect, never wired into services.
