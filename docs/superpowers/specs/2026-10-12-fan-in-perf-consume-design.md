# fan-in-perf: Experiment B (`consume`) — Design

Ticket WPB-26288, experiment B: maximal rate of adding and consuming
notifications by ~2,000 clients. Extends
`2026-10-08-fan-in-perf-design.md` (experiment A). Where that spec and the code
disagree, the code wins (see "Spec A drift").

## Goal

Measure whether PostgreSQL carries the fan-in read path while writers keep
producing. Report consumed/s (current, max), delivery lag, backlog, and whether
any notification was lost. All DB access through `Wire.FanInNotificationsStore`.

## Non-goals (follow-ups)

- **Acks (experiment C).** B readers keep cursors in memory and write nothing.
  C adds ack writes and ack-based start positions. RFC SQL bugs to fix then:
  team init-ack selects `notificationId` with `GROUP BY teamId` (invalid; read
  `last_team_notifications`), epoch init targets nonexistent
  `conversation_notification_acks` and `EXCLUDED.notificationId`,
  `ON CONFLICT DO UPDATE` lacks a target.
- **`connections` kind.** A reader would subscribe to one stream per connected
  user (RFC: acks keyed `(userId, connectedUserId)`). Needs a connection graph
  and the local/remote table split. Per-query cost equals a `user` stream read,
  so it is covered. Not covered: many streams per poll (e.g. 200
  connections); approximate with a large `--epochs-per-client`. Later: add
  `StreamConnection UserId`.
- **`epoch_history` reads.** Real clients look up their epoch streams on
  connect, including superseded epochs of the last 28 days. Needs seeding and
  an epoch-advance model (no store write op exists). B takes a fixed epoch set
  from `--epochs-per-client`. Steady-state read cost is unaffected.
- **Connect storm experiment.** 2,000 reconnects: history read, init-acks,
  missed-check.
- **Writer rate cap.** Writers run unthrottled. "Max sustainable" is where lag
  and backlog stay bounded. Add a cap only if that proves too hard to read.
- **Row-count loss verification.** B verifies by counts kept in the tool.
- Experiment D (TTL GC).

## CLI

```
fan-in-perf --db CONNSTR [global flags] consume [flags]
```

`consume` accepts all `produce` flags (`--writers`, `--targets`,
`--clients-per-user`, `--payload-bytes`, `--duration`, `--warmup`, `--seed`)
plus:

| Flag | Default | Meaning |
|---|---|---|
| `--clients N` | 2000 | simulated reader clients |
| `--epochs-per-client E` | 3 | epoch streams per client |
| `--poll-interval MS` | 100 | sleep after an empty/short fetch |
| `--fetch-limit L` | 100 | notifications per fetch (RFC: ~100) |
| `--reader-pool-size P` | 32 | reader connection pool (writers keep `--pool-size`) |
| `--drain-timeout SECS` | 10 | wait for backlog to drain at the end |

`--targets` for `consume` rejects `connections`. Other grammar unchanged.

## Store changes (`wire-subsystems`)

```haskell
data Stream
  = StreamUser !UserId
  | StreamClient !UserId !ClientId
  | StreamTeam !TeamId
  | StreamEpoch !GroupId !Epoch

FetchNotifications :: Stream -> UUID -> Int -> FanInNotificationsStore m [(UUID, Value)]
-- select notification_id, payload from <table>
--   where <key> = $1 and notification_id > $2
--   order by notification_id limit $3
```

```haskell
GetLastNotificationId :: Stream -> FanInNotificationsStore m (Maybe UUID)
-- select notification_id from last_<kind>_notifications where <key> = $1
```

- Each op is a single statement, no transaction (no isolation parameter).
- Existing primary keys serve both statements; no new indexes.
- Postgres interpreter: four statements per op via `hasql-th`, same style as
  pushes.
- Initial cursor of a reader stream = `GetLastNotificationId` ("start at
  latest", RFC "when nothing is acknowledged"); `Nothing` (empty stream) = nil
  UUID. Why: a fetch needs a lower bound; without one readers would deliver
  rows left from earlier runs, inflating consumed/s and backlog and breaking
  the `expected == delivered` check.
- All cursors are loaded in a startup phase **before writers start** (not
  timed, not counted). Otherwise a push committed between the cursor read and
  the first fetch would be counted as expected but could be below the cursor
  (false `missing`). Cost: ~N × (3 + E) reads at startup, exercising the RFC's
  start-position read once.

## Reader population

Readers must overlap writer pools, else they poll empty streams.

- Readers use the same `mkEntries` pools and seed as writers.
- Reader `i` (0-based) subscribes, for each kind present in `--targets` with
  pool size `S`, to stream `i mod S` (user, team); for `clients`, to
  `(uid, ClientId (1 + i div S))` of pool entry `i mod S` (rejected at startup
  if `N > S × --clients-per-user`; a push's `TargetUserClients` carries client
  ids `1..C`, so every such reader is addressed); for `epoch`, to
  `E` consecutive streams starting at `(i × E) mod S`.
- Subscriber count per pool stream is computed statically (`subscribers kind idx`).
- Startup prints coverage: share of pool streams with at least one subscriber,
  warns below 100% (e.g. `user:100000` with 2,000 readers = 2%: consumed/s
  stays far below targets/s).

## Reader loop (per simulated client)

```
init:  state := [(Stream, GetLastNotificationId stream)]   -- before writers start
loop:
  for each stream: fetch via reader pool → page
      record fetch latency, delivered count, lag per notification
      cursor := last notification id of page (if non-empty)
  if any stream returned a full page: loop immediately
  else sleep --poll-interval
```

- 2,000 green threads (`async`); per-client state is its stream list and cursors.
- One shared reader pool. Pool wait shows up in fetch latency. Both pools report
  `fanin_perf_pool_*{role="writer|reader"}` (gauges, counters, histograms).
- Fetch errors are counted per kind and rate-limited logged, like push errors;
  the reader continues with the unchanged cursor.
- Streams are served in any order; order within a stream is preserved.

## Loss / duplicate check (RFC finding)

`pushViaFanInImpl` generates the UUIDv7 before the transaction and retries
reuse it. Writer A takes id t1, writer B takes t2 > t1 and commits first, the
reader's cursor moves to t2, A commits t1 afterwards: a `> cursor` reader never
delivers it. B measures this.

- Writers record `expected += Σ_{target ∈ push} subscribers(target)` per
  successful push (static table lookup, no DB, no extra contention).
- Readers record `delivered`.
- On stop: stop writers, wait up to `--drain-timeout` for `delivered ==
  expected`, then stop readers. Summary prints `missing = max 0 (expected -
  delivered)` and `duplicates = max 0 (delivered - expected)`. Non-zero
  `missing` after drain is a finding about the RFC's cursor model.
- A push that errors after commit is not counted; accepted (rare, and it
  produces `duplicates`/noise rather than silent loss).
- Optional safety window `--safety-window MS` (default 0 = plain `> cursor`):
  readers fetch from the ms timestamp of the highest delivered id minus MS
  (never below the start position), skip ids already delivered within the
  window (per-stream set, pruned below the bound) and continue a scan after a
  full page so a window larger than `--fetch-limit` cannot stall the reader.
  Late commits beyond MS are still lost.

## Stats

- New reader snapshot, same style as `Snapshot` (one `IORef` per reader, ticker
  sums): per-kind `delivered`, `fetches`, `emptyFetches`, `fetchErrors`;
  histograms for fetch latency and delivery lag, reusing `bucketIndex`,
  `quantileSeconds`, `diffSnapshot` style.
- Writer snapshot gains a total `expected` counter (per-kind not needed).
- `tick` produces a consume report: consume rate current/max (max ignores
  warmup), lag p50/p99, backlog = expected − delivered.
- Lag per notification = reader's wall-clock now − ms timestamp of the UUIDv7.
  Same host as writers, so clock skew is nil. Negative values clamp to 0.
  (Quantiles are cumulative like A; documented in the status line.)

## Metrics (`experiment="consume"` for all, registered once)

| Metric | Type |
|---|---|
| `fanin_perf_consumed_total{kind}` | counter |
| `fanin_perf_fetches_total{kind}`, `fanin_perf_empty_fetches_total{kind}` | counter |
| `fanin_perf_fetch_errors_total{kind}` | counter |
| `fanin_perf_consume_rate_current`, `fanin_perf_consume_rate_max` | gauge |
| `fanin_perf_backlog` | gauge |
| `fanin_perf_clients` | gauge |
| `fanin_perf_fetch_duration_seconds` | histogram |
| `fanin_perf_delivery_lag_seconds` | histogram |

Existing push metrics remain (labelled `experiment="consume"`). The histogram
sample-group function is parametrised by metric name. Grafana dashboard
`fan-in-perf.json` gains panels: consumed/s vs pushes/s, delivery lag
p50/p99, backlog, fetch latency, empty-fetch ratio.

## Terminal

Status line extends A's:

```
t=42s push/s cur=… max=… | cons/s cur=… max=… | lag p50=… p99=… | backlog=… | err/s …
```

Summary adds consumed total, avg/max consume rate, lag p50/p99, expected,
delivered, missing, duplicates, drain result (complete / timed out after N s,
interrupted on Ctrl-C). Duration and averages cover the writer phase only.

## Modules

- `FanInPerf.Consume` (new): readers, drain, lifecycle (`runConsume`).
- `FanInPerf.Population` (new): reader→stream mapping, subscriber counts,
  coverage (pure).
- `FanInPerf.Stats`, `Metrics`, `Terminal`, `Options`, `Run`, `Store`: extended.
  `Run`/`Store` create the reader pool and a reader `Env`.
- `Produce` only exports `storePush`, `waitForStop`, `printQueuedErrors` (open `REVIEW:` comments
  stay out of scope).

## Spec A drift (fix in the A spec alongside B)

- Stats are `IORef Snapshot` + `atomicModifyIORef'`, not `MutablePrimArray`.
- `--domain` flag removed (fixed local domain); `--seed` added;
  `repeatable-read` isolation accepted; `migrate` command exists.
- Quantiles are cumulative incl. warmup.

## Testing

Unit (`fan-in-perf-tests`, hspec + QuickCheck):

- Population: every reader stream exists in its pool; subscriber counts sum to
  `N` per kind (`E × N` for epoch); coverage computation; `clients` capacity
  rejection.
- Cursor advance: page → new cursor; empty page keeps cursor; `Nothing` start
  position maps to the nil UUID.
- Expected-delivery computation per push.
- UUIDv7 timestamp extraction and lag clamp.
- Stats math for the reader snapshot (diff, max after warmup, backlog).
- `--targets` rejects `connections` for `consume`; new flags bounded.

`wire-subsystems` unit tests stay green. Store reads have no unit coverage.
Manual acceptance (user's DB): `fan-in-perf --db … reset`, then
`consume --targets user:2000,team:1,epoch:200 --duration 30` — consume rate
tracks push rate, lag bounded, `missing=0` or a reported finding; `/metrics`
serves `fanin_perf_consumed_total`; Grafana shows the new panels.

## Security

No new secrets. `/metrics` exposes counters/histograms only. No payload or id
contents in logs. Reads go only through the store effect; the admin effect is
untouched.
