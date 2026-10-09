# fan-in-perf

Benchmarks the notification fan-in PostgreSQL store (WPB-26288) through
`Wire.FanInNotificationsStore`. Design: `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md`.

```sh
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" migrate
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" reset
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" \
  produce --writers 32 --targets user:100000x20,team:10,epoch:1000 --duration 60
```

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
