# fan-in-perf

Benchmarks the notification fan-in PostgreSQL store (WPB-26288) through
`Wire.FanInNotificationsStore`. Design: `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md`.

```sh
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" migrate
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" reset
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" \
  produce --writers 32 --targets user:100000x20,team:10,epoch:1000 --duration 60
```

Experiment B (readers): `consume` runs the writers of `produce` plus simulated
reader clients (`--clients`, default 2000; `--epochs-per-client`,
`--poll-interval`, `--fetch-limit`, `--reader-pool-size`, `--drain-timeout`,
`--safety-window`).

```sh
fan-in-perf --db "$DB" consume --targets user:2000,team:1,epoch:200 --duration 30
```

Readers subscribe to the same stream pools as the writers (client `i` reads
stream `i mod STREAMS` of each kind, `--epochs-per-client` consecutive epoch
streams); startup prints how many pool streams have a reader. `connections`
targets are not supported. Readers keep their cursors in memory and start at
the latest notification of each stream (`GetLastNotificationId`), taken before
the writers start. After the writers stop, readers get `--drain-timeout`
seconds to catch up; the summary reports `missing`/`duplicates`. These are net
counts (one duplicate cancels one missing); real duplicates mainly come from
pushes that committed but returned an error (not counted as expected). A non-zero
`missing` means a notification committed after a reader's cursor had already
passed its (smaller, pre-transaction generated) UUIDv7 - a finding about the
RFC's `> lastId` model, not a tool bug. Measured with `consume --clients 2000
--targets user:2000,epoch:200 --duration 10 --drain-timeout 20`: 1 writer
`missing=0`, 16 writers `missing=7` of 1.57M.

`--safety-window MS` (default 0: plain `> lastId`, 0..60000) tests whether
re-reading a time window removes this loss: each fetch starts at the ms
timestamp of the highest delivered id minus MS (never below the start
position); ids already delivered within the window are remembered per stream
and not counted again. A full page continues the scan after its last id, so
a window holding more rows than `--fetch-limit` is paged through, not
re-fetched forever. Trade-offs: more fetched rows (each idle pass re-reads the
window; pages of only known ids count as empty fetches) and memory for the
remembered ids. A notification committed more than MS after a higher one was
delivered is still lost. Measured (16 writers, `--clients 2000 --targets user:2000,epoch:200 --duration 10`, two runs each): `--safety-window 0` → missing 4 and 5 of ~1.5–1.9M; `200` and `1000` → missing 0 (cons/s avg −3% and −14%, empty fetches 88% → 6–13%).

Delivery lag is measured from the
millisecond timestamp inside the UUIDv7 (writers and readers share a clock).
Fetch (and ack) latency is the store call, including acquiring a connection
from the reader pool. At most `--reader-pool-size` store calls run at once
(a gate in front of the pool); the wait for a gate slot is reported
separately as gate wait (summary line, `fanin_perf_gate_wait_seconds`). A
client's effective latency is gate wait + latency. The two pools report
separately as `fanin_perf_pool_*{role="writer|reader"}`.
Summary duration and push/s and cons/s averages cover the writer phase
(until the writers stopped); the drain time is printed separately. With
`--duration`, Ctrl-C during writer shutdown/drain ends the drain early
(`drain: interrupted`) and still prints the summary. Without it, the first
Ctrl-C stops the writers and a second one exits immediately (GHC resets the
SIGINT handler), so no summary.

Experiment C (acknowledging readers): `consume-ack` runs `consume` and every
simulated client acknowledges what it received. A client acks after every
N-th notification (`--ack-every N`, default 100, counted over all its
streams): one upsert per stream it received something on, up to the highest
id there (RFC: delivery tags with `multiple = true`; N = 1 is the RFC's live
path). Start positions come from resetting the client's acks to the latest
notification (RFC "when nothing is acknowledged"); run `reset` first.

```sh
fan-in-perf --db "$DB" reset
fan-in-perf --db "$DB" consume-ack --targets user:1000,clients:1000,team:10,epoch:200 --clients-per-user 2 --clients 2000 --duration 30 --ack-every 1
```

Readers are clients of users (round-robin, client ids 1..k per user); every
user is in one team (with `team` in `--targets`) and its clients share the
user's epoch streams. A reader fetches and acks as its (user, client).
`consume-ack` therefore needs equal STREAMS for `user` and `clients`.

Global flags go before the sub-command. `--targets` entries are `KIND:STREAMS[xK]`
(`KIND` in `user|clients|team|epoch|connections`): `STREAMS` distinct stream keys,
`K` targets of that kind per push. Each push uses exactly one kind.

Metrics: `http://localhost:9400/metrics` (`--metrics-port`), scraped by the
dockerephemeral OTel collector and shown in Grafana dashboard "fan-in-perf".
`consume` and `consume-ack` add `fanin_perf_gate_wait_seconds` (reader gate
wait, see above) to the fetch, lag and (C only) ack metrics.

Notes:

- Targets within a push are sorted to avoid lock-order deadlocks; the store
  itself does not sort (RFC finding). Unsorted overlapping pushes would
  deadlock (40P01) and be retried silently, inflating latency.
- p50/p99 are cumulative over the whole run, including warmup.
- `fanin_perf_targets_total{kind="clients"}` counts users (K), not rows written
  (K x `--clients-per-user`).
