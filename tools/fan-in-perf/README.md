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
`--poll-interval`, `--fetch-limit`, `--reader-pool-size`, `--drain-timeout`).

```sh
fan-in-perf --db "$DB" consume --targets user:2000,team:1,epoch:200 --duration 30
```

Readers subscribe to the same stream pools as the writers (client `i` reads
stream `i mod STREAMS` of each kind, `--epochs-per-client` consecutive epoch
streams); startup prints how many pool streams have a reader. `connections`
targets are not supported. Readers keep their cursors in memory and start at
the latest notification of each stream (`GetLastNotificationId`), taken before
the writers start. After the writers stop, readers get `--drain-timeout`
seconds to catch up; the summary reports `missing`/`duplicates`. A non-zero
`missing` means a notification committed after a reader's cursor had already
passed its (smaller, pre-transaction generated) UUIDv7 - a finding about the
RFC's `> lastId` model, not a tool bug. Delivery lag is measured from the
millisecond timestamp inside the UUIDv7 (writers and readers share a clock).
Fetch latency includes the wait for a connection from the reader pool.
Ctrl-C during writer shutdown/drain skips the final summary; use `--duration`
to get a summary.

Global flags go before the sub-command. `--targets` entries are `KIND:STREAMS[xK]`
(`KIND` in `user|clients|team|epoch|connections`): `STREAMS` distinct stream keys,
`K` targets of that kind per push. Each push uses exactly one kind.

Metrics: `http://localhost:9400/metrics` (`--metrics-port`), scraped by the
dockerephemeral OTel collector and shown in Grafana dashboard "fan-in-perf".

Notes:

- Targets within a push are sorted to avoid lock-order deadlocks; the store
  itself does not sort (RFC finding). Unsorted overlapping pushes would
  deadlock (40P01) and be retried silently, inflating latency.
- p50/p99 are cumulative over the whole run, including warmup.
- `fanin_perf_targets_total{kind="clients"}` counts users (K), not rows written
  (K x `--clients-per-user`).
