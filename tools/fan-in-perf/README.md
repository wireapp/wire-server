# fan-in-perf

Load generator for the notification fan-in store proposed in the RFC
"Notification fan-in" (WPB-26288). It drives `Wire.FanInNotificationsStore`
(`libs/wire-subsystems`) against PostgreSQL and measures write throughput,
read throughput, delivery lag, ack cost and notification loss.

Design specs (background, rationale):

- `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md` (A: produce)
- `docs/superpowers/specs/2026-10-12-fan-in-perf-consume-design.md` (B: consume)
- `docs/superpowers/specs/2026-10-14-fan-in-perf-consume-ack-design.md` (C: consume-ack)

## Contents

- [Quick start](#quick-start)
- [Commands and parameters](#commands-and-parameters)
- [Observability](#observability)
- [Architecture](#architecture)
- [Database schema](#database-schema)
- [Reading the numbers](#reading-the-numbers)
- [Results so far](#results-so-far)
- [Limitations](#limitations)
- [Differences to the RFC](#differences-to-the-rfc)

## Quick start

Start PostgreSQL, the OTel collector and Grafana
(`./deploy/dockerephemeral/run.sh`), then (`fan-in-perf` = the built executable, or `cabal run fan-in-perf --`):

```sh
DB="postgresql://wire-server:posty-the-gres@localhost:5432/backendA"

fan-in-perf --db "$DB" migrate   # create tables (idempotent)
fan-in-perf --db "$DB" reset     # truncate all fan-in tables

# A: writers only
fan-in-perf --db "$DB" produce --writers 32 --targets user:100000x20,team:10,epoch:1000 --duration 60

# B: writers + 2000 readers with in-memory cursors
fan-in-perf --db "$DB" consume --targets user:2000,team:1,epoch:200 --duration 30

# C: like B, readers acknowledge every notification (RFC live path)
fan-in-perf --db "$DB" reset
fan-in-perf --db "$DB" consume-ack --targets user:1000,clients:1000,team:10,epoch:200 \
  --clients-per-user 2 --clients 2000 --duration 30 --ack-every 1
```

Global flags go before the sub-command. The tool prints a status line per
second and a summary at the end. Ctrl-C stops a run without `--duration`.

## Commands and parameters

| Command | Experiment | Does |
|---|---|---|
| `migrate` | – | Creates the tables of `libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql` unless they exist. No migration bookkeeping. |
| `reset` | – | Truncates all fan-in tables (notifications, `last_*`, acks, `epoch_history`). |
| `produce` | A | Writers push as fast as possible. |
| `consume` | B | `produce` writers plus simulated reader clients. |
| `consume-ack` | C | `consume` where every reader also acknowledges. |

### Global

| Flag | Default | Range | Meaning |
|---|---|---|---|
| `--db CONNSTR` | required | | PostgreSQL connection string. Never printed. |
| `--pool-size N` | `--writers` (1 for `migrate`/`reset`) | 1–10000 | Writer connection pool size. |
| `--metrics-port PORT` | 9400 | 1–65535 | Port of `/metrics`. |
| `--isolation LEVEL` | `read-committed` | `read-committed`, `repeatable-read`, `serializable` | Isolation level of push transactions. |

### `produce`

| Flag | Default | Range | Meaning |
|---|---|---|---|
| `--targets SPEC` | required | see below | Target mix. |
| `--writers W` | 16 | 1–10000 | Concurrent writer threads. |
| `--clients-per-user C` | 1 | 1–100000 | Client ids per `clients` target. |
| `--payload-bytes B` | 512 | 1–10⁷ | Approximate JSON payload size. |
| `--duration SECS` | until Ctrl-C | ≥ 1 | Writer phase length. |
| `--warmup SECS` | 5 | ≥ 0 | Seconds ignored by the `max` rates. |
| `--seed INT` | random, printed | any | RNG seed for stream keys and pushes. |

#### `--targets`

Comma-separated entries `KIND:STREAMS[xK]`, each kind at most once.

| Part | Meaning |
|---|---|
| `KIND` | `user`, `clients`, `team`, `epoch`, `connections` (one per `Target` constructor). |
| `STREAMS` | Number of distinct stream keys pre-generated for this kind. |
| `K` | Targets of this kind per push. Default 1, at most `STREAMS`. |

Each push picks one entry uniformly, then `K` distinct keys from its pool. A
push never mixes kinds. For `clients`, `K` counts users: a push writes
`K × --clients-per-user` rows.

Example `user:1000x20,team:10`: half of the pushes go to 20 of 1000 users,
the other half to 1 of 10 teams.

### `consume`

All `produce` flags, plus:

| Flag | Default | Range | Meaning |
|---|---|---|---|
| `--clients N` | 2000 | 1–100000 | Simulated reader clients (one thread each). |
| `--epochs-per-client E` | 3 | 1–10000 | Epoch streams per user. Only used with `epoch` in `--targets`; `E ≤` its `STREAMS`. |
| `--poll-interval MS` | 100 | 0–3600000 | Sleep after a pass in which no stream returned a full page. |
| `--fetch-limit L` | 100 | 1–10000 | Notifications per fetch. |
| `--reader-pool-size P` | 32 | 1–10000 | Reader connection pool size; also the number of concurrent reader store calls (gate). |
| `--drain-timeout SECS` | 10 | ≥ 0 | Time readers get to catch up after the writers stopped. |
| `--safety-window MS` | 0 | 0–60000 | Re-read the last `MS` ms of each stream to catch late commits. 0: plain `> lastId`. |

`connections` is rejected.

### `consume-ack`

All `consume` flags, plus:

| Flag | Default | Range | Meaning |
|---|---|---|---|
| `--ack-every N` | 100 | 1–10000 | A client acks after every N-th notification it received, counted over all its streams. |

`user` and `clients` entries must have equal `STREAMS` (one identity per
reader). Run `reset` before each run: start positions are written as acks.

## Observability

### Metrics

`/metrics` (Prometheus text format) on `0.0.0.0:<--metrics-port>`. All
families except `fanin_perf_pool_*` have an `experiment` label (`produce`,
`consume`, `consume-ack`); most counters also a `kind` label. Pool families
carry only `role`.

| Family | Experiments | Meaning |
|---|---|---|
| `fanin_perf_pushes_total`, `_errors_total`, `_targets_total` | all | Pushes, failed pushes, targets (`clients`: users, not rows). |
| `fanin_perf_push_rate_current`, `_push_rate_max`, `_error_rate_current`, `_error_ratio_current` | all | Per-tick rates. |
| `fanin_perf_push_duration_seconds` | all | Push latency histogram. |
| `fanin_perf_writers` | all | Writer threads. |
| `fanin_perf_consumed_total`, `_fetches_total`, `_empty_fetches_total`, `_fetch_errors_total` | B, C | Reader counters. |
| `fanin_perf_consume_rate_current`, `_consume_rate_max`, `_backlog`, `_clients` | B, C | Reader gauges; backlog = expected − delivered. |
| `fanin_perf_fetch_duration_seconds`, `_delivery_lag_seconds`, `_gate_wait_seconds` | B, C | Reader histograms. |
| `fanin_perf_acks_total`, `_ack_errors_total`, `_ack_rate_current`, `_ack_rate_max`, `_ack_duration_seconds` | C | Ack upserts. |
| `fanin_perf_pool_*{role="writer\|reader"}` | all | Connection pool stats (in use, idle, sessions, failures, timeouts, durations). |

### Grafana (local)

`deploy/dockerephemeral/docker-compose.yaml` runs:

- `otel-collector`: scrapes `host.docker.internal:9400` every second
  (`docker/otel-collector-config.yaml`, job `fan-in-perf`) and the
  PostgreSQL exporter. `host.docker.internal` maps to the host gateway.
- `grafana-lgtm`: Grafana on <http://localhost:3000> with the provisioned
  dashboards "fan-in-perf" (`docker/grafana-dashboards/fan-in-perf.json`)
  and "postgres-exporter".

The tool runs on the host, so the collector container must reach host port
9400. A changed `--metrics-port` also needs a changed scrape target.

### Firewall (NixOS)

The scrape arrives on the bridge of the compose network `demo_wire`, not on
`docker0`. Its name is `br-<random id>` and changes when the network is
recreated; its subnet is fixed to `172.20.2.0/24`. A rule for `docker0`
does not match. Allow port 9400 from that subnet:

```nix
# nftables backend (networking.nftables.enable = true)
networking.firewall.extraInputRules = ''
  ip saddr 172.20.2.0/24 tcp dport 9400 accept
'';

# iptables backend (default): allow on all Docker bridges
networking.firewall.interfaces."br-+".allowedTCPPorts = [ 9400 ];
```

(In nftables interface names the wildcard is `br-*`, in iptables `br-+`.)
These are examples; adapt them to your firewall setup.

Check: `curl -s localhost:9400/metrics | head` while a run is active, then
query `fanin_perf_writers` in Grafana → Explore. No data there but data on
`curl` means the scrape is blocked.

## Architecture

```mermaid
flowchart LR
  subgraph tool[fan-in-perf process]
    W["writer threads<br/>--writers"] --> WP[("writer pool<br/>--pool-size")]
    R["reader threads<br/>--clients"] --> G{{"gate: QSem<br/>--reader-pool-size"}} --> RP[("reader pool<br/>--reader-pool-size")]
    T[ticker, 1/s] --> TERM[terminal status + summary]
    T --> M["/metrics :9400"]
    W -. stats .-> T
    R -. stats .-> T
  end
  WP --> PG[(PostgreSQL)]
  RP --> PG
  M --> OC[otel-collector] --> LGTM[grafana-lgtm]
```

All database access goes through the `FanInNotificationsStore` and
`FanInNotificationsAdmin` effects with their PostgreSQL interpreters
(`FanInPerf.Store`). The tool contains no SQL.

**Writers** (`FanInPerf.Produce`) loop: generate a push (`--targets`), call
`PushViaFanIn`, record latency and outcome. Push latency covers only the
store call. A writer checks the stop flag between pushes, so every committed
push is counted.

**Readers** (`FanInPerf.Consume`, B and C) are one thread per simulated
client. A pass fetches each of the client's streams once
(`FetchNotifications stream cursor --fetch-limit`). Streams that returned a
full page are fetched again until none does; then the client sleeps
`--poll-interval`. Cursors live in memory.

**Gate.** At most `--reader-pool-size` reader store calls run at once. Without
the gate, thousands of readers wait inside the pool's STM retry and all wake
on every connection release; that saturated the CPU and stalled the ticker.
The gate wait is reported separately from fetch/ack latency. Pools use
acquisition timeout 0 (wait forever): `hasql-resource-pool` forks a sleeping
thread per acquire for the length of the timeout, which at this load means
hundreds of thousands of threads.

**Ticker.** Once per second it sums the per-thread counters, prints the
status line and publishes metrics. Threads never touch Prometheus.

### Internal data model

The tool's types fall into three groups: what writers push, what readers
read, and what everyone counts. The store's own types (`FanInPush`,
`Target`, `Stream`) come from `Wire.FanInNotificationsStore`.

#### Workload (`FanInPerf.Targets`)

```mermaid
flowchart LR
  S["--targets<br/>user:1000x20,team:10"] -->|parse| TC["TargetConfig<br/>kind, streams, perPush"]
  TC -->|mkEntries + seed| E["Entry<br/>spec + StreamPool"]
  E -->|"genPush: pick entry,<br/>perPush distinct keys"| P["FanInPush<br/>targets :: [Target]"]
```

- `TargetConfig`: one `--targets` entry.
- `StreamPool`: the pre-generated stream keys of one entry, a vector per
  kind (`UserPool [UserId]`, `TeamPool [TeamId]`, `EpochPool [(GroupId,
  Epoch)]`, ...). Generated once from the seed; writers only pick indices.
- `Entry`: config plus pool. Writers share the immutable `Vector Entry`.
- `Target` vs. `Stream`: a `Target` is what a push addresses, a `Stream` is
  what a reader fetches. They differ for `clients`: one
  `TargetUserClients (u, [c1, c2])` writes the streams `StreamClient u c1`
  and `StreamClient u c2`.

#### Readers (`FanInPerf.Population`, `FanInPerf.Consume`, `FanInPerf.Acks`)

```mermaid
flowchart TB
  POP["Population<br/>teams, personalUsers, kinds,<br/>subscribers :: Map Key Int"] --> RD["Reader<br/>team, user :: SimUser, cid"]
  RD -->|readerStreams| ST["[Stream]"]
  ST -->|start position| SUB["Subscription per stream<br/>cursor, start, seen, resumeAfter"]
  SUB -->|delivered ids| ACK["AckState<br/>Map Key Pending"]
```

- `Population`: the simulated users (`SimUser`: user id, epochs, client
  ids), grouped into teams (`SimTeam`) if `team` is in `--targets`. Built
  once, immutable.
- `Reader`: one simulated client, i.e. one `(user, client)` and its team.
  One thread per `Reader`.
- `subscribers`: number of readers per stream. Writers use it to compute
  the expected deliveries of a push (sum over the streams it writes).
- `Key`: `Stream` with an `Ord` instance (`Epoch` has none), used as map key.
- `Subscription`: per-stream reader state, owned by its thread. `cursor`:
  highest delivered id. `start`, `seen`, `resumeAfter`: only for the safety
  window (see below).
- `AckState` (C): per reader, the not yet acknowledged streams (`Pending`:
  stream, highest id, count) and their total count. At `--ack-every` it
  returns all `Pending`s to write and resets.

#### Statistics (`FanInPerf.Stats`, `FanInPerf.ConsumeStats`)

```mermaid
flowchart LR
  WT[writer thread] -->|atomicModifyIORef'| WS["WriterStats<br/>IORef Snapshot"]
  RT[reader thread] -->|atomicModifyIORef'| RS["ReaderStats<br/>IORef ReaderSnapshot"]
  WS -->|sum all| TK[ticker]
  RS -->|sum all| TK
  TK <-->|"previous snapshot,<br/>peak rates"| TS["TickState /<br/>ConsumeTickState"]
  TK --> REP["TickReport /<br/>ConsumeReport<br/>delta + total"]
  REP --> TERM[terminal]
  REP --> MET[Prometheus]
```

- `Snapshot` (writers) and `ReaderSnapshot` (readers): cumulative counters
  per `TargetKind` (pushes, errors, fetches, deliveries, acks, ...) and
  sparse histograms (`IntMap` bucket index → count). Writers also sum
  `expected`.
- Each thread owns one `IORef`, so threads never contend on counters.
- Every second the ticker sums all snapshots, subtracts the previous sum
  (kept in `TickState`/`ConsumeTickState`) and produces a report: per-tick
  `delta`, cumulative `total`, rates, peaks, percentiles.

### Population (B, C)

Readers are derived from the writers' stream pools, so they read streams that
get written:

- The `user` and `clients` pools are prefixes of one user-id vector. User
  index `u` is the same user in both.
- `--clients` readers are assigned round-robin to users; user `u` gets client
  ids `1..k_u`. Each reader is one `(user, client)`.
- With `team` in `--targets`, user `u` is in team `u mod S_team`.
- With `epoch`, user `u` reads epoch streams `u·E … u·E+E−1` (mod
  `S_epoch`); all its clients share them.

A reader's streams: its user stream, its client stream, its team stream and
its `E` epoch streams, each only if the kind is in `--targets`. Startup
prints the share of each pool's streams that have a reader ("coverage").

```
users:    u0        u1        u2   ...     (shared by user/clients pools)
readers:  (u0,c1)   (u1,c1)   (u2,c1) ...  (u0,c2) (u1,c2) ...   round-robin
teams:    t0        t1        t0  ...      u mod S_team
epochs:   e0..e2    e3..e5    e6..e8 ...   E = 3
```

### Start positions

Read before any writer starts; a push in between would otherwise sit below a
cursor and count as missing.

- B: `GetLastNotificationId` of each stream ("start at latest").
- C: `InitAcks` per stream, i.e. the RFC's "when nothing is acknowledged":
  the ack is set to the stream's last notification and returned.
- Empty stream: nil UUID.

### Acks (C)

Models the RFC's delivery tags with `multiple = true`. A client counts
deliveries over all its streams. At the N-th it calls `AckNotification` once
per stream it received something on since the last ack, with that stream's
highest id. `--ack-every 1` is the live path. Failed acks stay pending until
the next ack point. A remainder below N at the end is not flushed.

### Expected vs. delivered, drain

For each successful push, a writer adds the number of readers of every
targeted stream to *expected*. Readers add every new id to *delivered*.
After the writer phase, readers get `--drain-timeout` seconds to catch up.
The drain ends early as `stalled` when nothing was delivered for
`2 s + 2 × --poll-interval`: what is still missing then is lost, not in
flight. The summary reports `missing` and `duplicates` as net counts (one
duplicate cancels one missing). Duplicates mainly come from pushes that
committed but returned an error, which are not counted as expected.

### Safety window

With `--safety-window W > 0`, a fetch starts at the ms timestamp of the
cursor minus `W`, never below the start position. Ids already delivered
inside the window are remembered per stream and skipped. A full page
continues after its last id, so a window with more rows than `--fetch-limit`
is paged through instead of re-read forever.

```
 start        bound         resumeAfter           cursor        newest
   |            |                |                   |             |
 --+--x-x-x-----+--o-x-o-x-x-----+-o-x-x-o-x-x-------+-------------+-->
   |            |<------------- window W ----------->|
   |            |  seen = ids delivered in here      |
   |<- never fetched again                           |<- not yet delivered

 x  delivered, in seen        o  late commit: below cursor, delivered
                                 on the re-read of the window
```

Cost: idle passes re-read the window (pages of only known ids count as empty
fetches) and memory for the remembered ids. A notification committed more
than `W` after a higher one was delivered is still lost.

## Database schema

Schema: `libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql`.

Each stream kind has three tables:

| Kind | Notifications (stream key + `notification_id`) | Last id per stream | Acks per client |
|---|---|---|---|
| user | `user_notifications (user_id)` | `last_user_notifications` | `user_notification_acks (user_id, client_id)` |
| client | `client_notifications (user_id, client_id)` | `last_client_notifications` | `client_notification_acks (user_id, client_id)` |
| team | `team_notifications (team_id)` | `last_team_notifications` | `team_notification_acks (user_id, client_id, team_id)` |
| epoch | `epoch_notifications (group_id, epoch)` | `last_epoch_notifications` | `epoch_notification_acks (user_id, client_id, group_id, epoch)` |
| local connection | `local_connection_notifications (user_id)` | `last_local_connection_notifications` | `local_connection_acks` (unused) |
| remote connection | `remote_connection_notifications (user_domain, user_id)` | `last_remote_connection_notifications` | `remote_connection_acks` (unused) |

Plus `epoch_history` (unused by the tool). Notification rows carry `payload
jsonb` and `origin`.

```mermaid
flowchart TB
  P["PushViaFanIn<br/>one UUIDv7 per push"] -->|per target, one transaction| N[("X_notifications<br/>INSERT")]
  P --> L[("last_X_notifications<br/>UPSERT greatest")]
  L -->|InitAcks| A[("X_notification_acks")]
  N -->|"FetchNotifications: id > cursor<br/>ORDER BY id LIMIT L"| C[reader]
  L -->|GetLastNotificationId| C
  C -->|"AckNotification: UPSERT greatest"| A
```

A push generates one UUIDv7 *before* its transaction and writes, per target,
the notification row and a `greatest(...)` upsert of the stream's `last_*`
row, all in one transaction. The store dedupes and sorts targets first
(`normalizeTargets`): duplicates would violate primary keys, and a fixed
order prevents lock-order deadlocks between concurrent pushes.

## Reading the numbers

- Percentiles are histogram bucket upper bounds, printed `p99<=1.2ms`. Four
  buckets per power of two: the value is at most 25% below the bound.
  Grafana interpolates inside the same buckets, so its values are slightly
  lower. `_sum` is estimated from bucket midpoints; sum/count is an
  approximate mean.
- Status lines show the last tick; summary lines the whole run, including
  warmup and drain.
- Summary duration and push/s, cons/s averages cover the writer phase only;
  the drain time is printed separately.
- `max` rates count only while writers run and after warmup (`-` before).
- cons/s counts deliveries, push/s pushes. They differ by the fan-out, which
  the summary prints.
- Fetch and ack latency is the client's view of one store call: connection
  acquisition, query, jsonb decoding, thread wake-up. Not database time.
  Gate wait is separate. A client's effective round time is the sum over its
  streams of gate wait + latency.
- Delivery lag is now minus the ms timestamp in the UUIDv7. It includes the
  tool's queueing and on average half the poll interval; under saturation it
  measures the backlog, not the store.
- `missing` depends on reader lag: a reader far behind the head has not yet
  passed late commits and does not lose them. Compare `missing` only at
  similar backlog. The summary prints the backlog at writer stop and warns
  when it exceeds one second of deliveries.
- Throughput varies a lot between runs (WAL flush waits; tool and database
  share the host). Use at least 3 runs and the median.

## Results so far

Late-commit loss with plain `> lastId` (`consume --clients 2000 --targets
user:2000,epoch:200 --duration 10 --drain-timeout 20`):

| Writers | missing |
|---|---|
| 1 | 0 |
| 16 | 7 of 1.57M |

Cause: a notification commits after a reader's cursor already passed its
smaller, pre-transaction UUIDv7. This is a property of the RFC's cursor
model, not a tool bug.

Safety window (16 writers, `--clients 2000 --targets user:2000,epoch:200
--duration 10`, two runs each):

| `--safety-window` | missing | cons/s avg | empty fetches |
|---|---|---|---|
| 0 | 4, 5 of ~1.5–1.9M | baseline | 88% |
| 200 | 0 | −3% | 6–13% |
| 1000 | 0 | −14% | 6–13% |

## Limitations

- `connections` targets are only supported by `produce`. Readers have no
  connection streams.
- B: with `user` and `clients` of different `STREAMS`, readers beyond the
  shorter pool poll streams nobody writes, and coverage is misleading. C
  rejects this.
- With `clients` in `--targets`, `--clients` must not exceed
  `max(STREAMS(user), STREAMS(clients)) × --clients-per-user`.
- Not simulated: `epoch_history` reads and epoch advances, the RFC's
  missed-notification check, reconnects, server flow control (serve ~100,
  wait for acks), expiry/partitioning (planned experiment D), websockets,
  native push, background jobs, legacy global ordering.
- Pushes never mix kinds, although the store accepts mixed `[Target]`.
- Lag assumes writers and readers share a clock (same process).
- Tool and database usually share the host; CPU contention affects results.
- Without `--duration`, the first Ctrl-C stops the writers and starts the
  drain; a second one exits immediately without a summary (GHC resets the
  SIGINT handler). With `--duration`, Ctrl-C during shutdown/drain ends the
  drain early (`drain: interrupted`) and still prints the summary.
- The tool generates its own keys per run; the same `--seed` reproduces keys
  only for the same tool version.

## Differences to the RFC

### Schema and SQL

| RFC | Implementation | Why |
|---|---|---|
| camelCase columns | snake_case | Repo convention. |
| `groupId text`, `epoch integer` (`epoch_history.epoch text`) | `group_id bytea`, `epoch bigint` everywhere | MLS group ids are binary; epochs are `Word64`. |
| `clientId` as `integer` or `UUID` depending on table | `client_id bigint` everywhere | Client ids are `Word64`. |
| `last_local_connection_notifications` PK `(userId, notificationId)` | PK `(user_id)` | One last id per stream. |
| No `last_remote_connection_notifications` | Added | Remote connection streams need a last id too. |
| `team_notification_acks` PK `(userId, clientId)` | PK `(user_id, client_id, team_id)` | The table has a `teamId` column; the RFC PK allows only one team per client. |
| `epoch_history` with `{ }`, `timestamtz` | Valid DDL | Typos. |
| Init-ack SQL: `ON CONFLICT DO UPDATE` without target | Explicit conflict targets (ack PKs) | Required by PostgreSQL. |
| Team init-ack selects from `team_notifications` with invalid `GROUP BY` | Reads `last_team_notifications` | Holds exactly the wanted id. |
| Epoch init-ack writes `conversation_notification_acks`, `EXCLUDED.notificationId` | `epoch_notification_acks`, `excluded.last_notification_id` | Table/column did not exist. |
| Ack writes unspecified | `greatest(old, new)` upsert | A late or repeated ack never moves backwards. |

### Semantics

- **Notification id before commit.** One UUIDv7 per push, generated before
  the transaction. Concurrent pushes commit out of id order, so a reader
  using `> lastId` can skip a notification permanently (see
  [Results](#results-so-far)). The RFC assumes ids are a safe cursor.
  Requirement for an implementation: a cursor must never move past ids that
  in-flight transactions can still commit. `--safety-window` is one tested
  mitigation. A plain sequence does not help: `nextval` is also assigned
  inside the transaction, which can commit out of order.
- **Target normalisation.** The store dedupes and sorts targets per push.
  The RFC says nothing about it; without it, duplicate targets violate PKs
  and concurrent pushes deadlock.
- **Isolation.** Configurable, default Read Committed. The `greatest`
  upserts do not need more.
- **Init acks per stream.** `InitAcks user client stream` handles one stream;
  the RFC initialises all streams of a client in one go. The tool calls it
  for each of a reader's streams. The RFC's epoch init-ack finds the
  client's epochs via `epoch_history`; `InitAcks` takes the stream from the
  caller and never reads `epoch_history`. An implementation must supply that
  lookup.
- **Multi-ack.** One `multiple = true` ack becomes one upsert per affected
  stream, since ack tables store only the last id per stream.
- **Fetch.** `FetchNotifications` is per stream, ordered by id, with a limit
  (RFC: ~100 before requiring acks).

### RFC gaps found while mapping existing code

- `notifyContacts` mixes user, connection and team recipients: needs up to
  three pushes, or mixed-kind pushes.
- MLS `propagateMessage` excludes the sender's client; `TargetEpoch` cannot
  express that.
- `removeIfLargeFanout` counts users; there is no equivalent for team or
  connection targets.
