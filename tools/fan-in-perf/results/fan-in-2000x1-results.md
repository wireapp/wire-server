# Notification fan-in: PostgreSQL benchmark, `user:2000x1`, 2,000 clients (WPB-26288)

## Setup

- Date 2026-10-22, commit `f4be99793`, driver `tools/fan-in-perf/run-2000x1.py`
- Host: 22 CPUs, 62 GiB RAM; tool and database share it. PostgreSQL 17.11 (Debian 17.11-1.pgdg13+2); max_connections=1000, max_wal_size=1GB, shared_buffers=128MB, synchronous_commit=on
- pg_partman 5.5.0: six `*_notifications` tables range-partitioned by UUIDv7, 1 min partitions, 5 min retention, no default partition, maintenance by the background worker every 60 s. `plain`: unpartitioned, no TTL.
- Load: `--targets user:2000x1`: 2,000 users with one client each; every push writes 1 notification to 1 user stream (plus its `last_user_notifications` upsert) in one transaction, read by 1 client, so cons/s = push/s. B-D: `--clients 2000`, poll interval 100 ms, fetch limit 100, safety window 200 ms; C, D: ack after every notification.
- Writers A=64, B=32, C/D=16; reader pool B=64, C/D=64: highest rate in sweeps beforehand at which readers keep up. Seed 42. One run each.
- Rates are averages over the writer phase incl. 5 s warmup; p99 are histogram bucket upper bounds (at most 25 % above). `backlog at stop`: expected minus delivered when the writers stopped. `missing`: never delivered after the drain. `errors`: failed pushes + fetches + acks.
- Only the `user` stream kind is exercised (no client/team/epoch streams, fan-out 1); not comparable to runs with a mixed target set.

Per mode: `drop-db`, `migrate` (`--partitioned` for partman), before each run `reset`. Commands:

```
fan-in-perf --db "$DB" produce --writers 64 --targets user:2000x1 --warmup 5 --duration 60 --seed 42
fan-in-perf --db "$DB" consume --writers 32 --targets user:2000x1 --warmup 5 --duration 120 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 120 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 660 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```
(partman: same commands)

## A: maximal rate of adding notifications

| Mode | push/s (avg) | push p99 | errors |
|---|---|---|---|
| partman | 27,769 | 8.4ms | 0 |
| plain | 24,226 | 7.3ms | 0 |

## B: adding and consuming by 2,000 clients

| Mode | push/s (avg) | cons/s (avg) | lag p99 | backlog at stop | missing | errors |
|---|---|---|---|---|---|---|
| partman | 10,988 | 10,975 | 335.5ms | 1,476 | 0 | 0 |
| plain | 10,885 | 10,882 | 335.5ms | 388 | 0 | 0 |

## C: B, every client acknowledges every notification

| Mode | push/s (avg) | cons/s (avg) | ack/s (avg) | ack p99 | lag p99 | backlog at stop | missing | errors |
|---|---|---|---|---|---|---|---|---|
| partman | 7,130 | 7,109 | 7,091 | 12.6ms | 671.1ms | 2,491 | 0 | 0 |
| plain | 6,440 | 6,439 | 6,438 | 12.6ms | 671.1ms | 222 | 0 | 0 |

## D: C for 11 minutes while TTL garbage collection runs

| Mode | push/s (avg) | cons/s (avg) | ack/s (avg) | ack p99 | lag p99 | backlog at stop | missing | errors |
|---|---|---|---|---|---|---|---|---|
| partman | 5,793 | 5,793 | 5,793 | 14.7ms | 805.3ms | 285 | 0 | 0 |
| plain | 6,381 | 6,380 | 6,380 | 12.6ms | 671.1ms | 263 | 0 | 0 |

D partman: 11 partition drops (each takes an `ACCESS EXCLUSIVE` lock on the parent); 1,910,226 rows (about 330 s of pushes) were expired by TTL, i.e. the later drops held data.

Impact of the drops: per-second status lines within 5 s of a drop vs. all other seconds of D partman (medians):

| Window | seconds | push/s (median) | ack/s (median) | ack p99 (median) | ack p99 max | lag p99 (median) | backlog (median) |
|---|---|---|---|---|---|---|---|
| within 5 s of a drop | 108 | 4,047 | 3,986 | 7.3ms | 234.9ms | 218.1ms | 283 |
| other | 528 | 4,184 | 4,262 | 7.3ms | 1.34s | 335.5ms | 340 |

## Answers

- a. (partman) 27,769 notifications/s (avg) added, push p99 8.4ms, 0 errors.
- a. (plain) 24,226 notifications/s (avg) added, push p99 7.3ms, 0 errors.
- b. (partman) 10,988 added/s (avg) and 10,975 consumed/s (avg) by 2,000 clients, lag p99 335.5ms, 0 missing.
- b. (plain) 10,885 added/s (avg) and 10,882 consumed/s (avg) by 2,000 clients, lag p99 335.5ms, 0 missing.
- c. (partman) 7,130 added/s (avg), 7,109 consumed/s (avg) and 7,091 acknowledged/s (avg), ack p99 12.6ms, 0 missing.
- c. (plain) 6,440 added/s (avg), 6,439 consumed/s (avg) and 6,438 acknowledged/s (avg), ack p99 12.6ms, 0 missing.
- d. During TTL GC (11 partition drops in 11 min): within 5 s of a drop vs. otherwise, ack/s 0.94 x, ack p99 1.00 x, lag p99 0.65 x (medians); 0 errors, 0 missing, 0 deadlocks.

## Notes

- No deadlocks, no duplicates, all drains complete.

<details><summary>Tool summaries</summary>

`A-partman`

```
summary: duration=60.0s
  pushes=1,666,318 targets=1,666,318 errors=0 (0.00%)
  push/s avg=27,769 max=35,483 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=2.1ms p99<=8.4ms
```

`B-partman`

```
summary: duration=120.0s
  pushes=1,318,554 targets=1,318,554 errors=0 (0.00%)
  push/s avg=10,988 max=16,125 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=2.6ms p99<=10.5ms
consume: delivered=1,318,554 expected=1,318,554 missing=0 duplicates=0
  cons/s avg=10,975 max=16,295 (avg incl. warmup, max after warmup, both writer phase)
  fan-out: 1.0 deliveries per push (cons/s and push/s differ by this factor)
  backlog at writer stop: 1,476 (0.1s of delivery)
  lag (cumulative) p50<=117.4ms p99<=335.5ms
  fetch (cumulative) p50<=0.8ms p99<=3.1ms empty=43.96% errors=0
  gate wait (cumulative) p50<=50.3ms p99<=234.9ms
  drain: complete after 0.2s
```

`C-partman`

```
summary: duration=120.0s
  pushes=855,624 targets=855,624 errors=0 (0.00%)
  push/s avg=7,130 max=10,821 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=1.6ms p99<=8.4ms
consume-ack: delivered=855,624 expected=855,624 missing=0 duplicates=0
  cons/s avg=7,109 max=11,225 (avg incl. warmup, max after warmup, both writer phase)
  fan-out: 1.0 deliveries per push (cons/s and push/s differ by this factor)
  backlog at writer stop: 2,491 (0.4s of delivery)
  lag (cumulative) p50<=134.2ms p99<=671.1ms
  fetch (cumulative) p50<=0.7ms p99<=2.6ms empty=58.92% errors=0
  gate wait (cumulative) p50<=58.7ms p99<=167.8ms
  acks=855,624 errors=0 ack/s avg=7,091 max=12,114
  ack latency (cumulative) p50<=0.9ms p99<=12.6ms
  drain: complete after 0.5s
```

`D-partman`

```
summary: duration=660.0s
  pushes=3,823,710 targets=3,823,710 errors=0 (0.00%)
  push/s avg=5,793 max=12,444 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=2.1ms p99<=12.6ms
consume-ack: delivered=3,823,710 expected=3,823,710 missing=0 duplicates=0
  cons/s avg=5,793 max=12,350 (avg incl. warmup, max after warmup, both writer phase)
  fan-out: 1.0 deliveries per push (cons/s and push/s differ by this factor)
  backlog at writer stop: 285 (0.0s of delivery)
  lag (cumulative) p50<=117.4ms p99<=805.3ms
  fetch (cumulative) p50<=0.7ms p99<=2.6ms empty=66.01% errors=0
  gate wait (cumulative) p50<=41.9ms p99<=201.3ms
  acks=3,823,710 errors=0 ack/s avg=5,793 max=12,635
  ack latency (cumulative) p50<=1.3ms p99<=14.7ms
  drain: complete after 0.2s
```

`A-plain`

```
summary: duration=60.0s
  pushes=1,453,674 targets=1,453,674 errors=0 (0.00%)
  push/s avg=24,226 max=34,733 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=2.6ms p99<=7.3ms
```

`B-plain`

```
summary: duration=120.0s
  pushes=1,306,195 targets=1,306,195 errors=0 (0.00%)
  push/s avg=10,885 max=16,839 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=2.6ms p99<=12.6ms
consume: delivered=1,306,195 expected=1,306,195 missing=0 duplicates=0
  cons/s avg=10,882 max=17,144 (avg incl. warmup, max after warmup, both writer phase)
  fan-out: 1.0 deliveries per push (cons/s and push/s differ by this factor)
  backlog at writer stop: 388 (0.0s of delivery)
  lag (cumulative) p50<=100.7ms p99<=335.5ms
  fetch (cumulative) p50<=0.4ms p99<=3.7ms empty=48.67% errors=0
  gate wait (cumulative) p50<=16.8ms p99<=268.4ms
  drain: complete after 0.1s
```

`C-plain`

```
summary: duration=120.0s
  pushes=772,891 targets=772,891 errors=0 (0.00%)
  push/s avg=6,440 max=13,700 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=1.8ms p99<=8.4ms
consume-ack: delivered=772,891 expected=772,891 missing=0 duplicates=0
  cons/s avg=6,439 max=12,873 (avg incl. warmup, max after warmup, both writer phase)
  fan-out: 1.0 deliveries per push (cons/s and push/s differ by this factor)
  backlog at writer stop: 222 (0.0s of delivery)
  lag (cumulative) p50<=100.7ms p99<=671.1ms
  fetch (cumulative) p50<=0.3ms p99<=1.3ms empty=70.37% errors=0
  gate wait (cumulative) p50<=1.8ms p99<=134.2ms
  acks=772,891 errors=0 ack/s avg=6,438 max=14,810
  ack latency (cumulative) p50<=1.0ms p99<=12.6ms
  drain: complete after 0.1s
```

`D-plain`

```
summary: duration=660.0s
  pushes=4,211,410 targets=4,211,410 errors=0 (0.00%)
  push/s avg=6,381 max=12,870 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=1.8ms p99<=10.5ms
consume-ack: delivered=4,211,410 expected=4,211,410 missing=0 duplicates=0
  cons/s avg=6,380 max=13,290 (avg incl. warmup, max after warmup, both writer phase)
  fan-out: 1.0 deliveries per push (cons/s and push/s differ by this factor)
  backlog at writer stop: 263 (0.0s of delivery)
  lag (cumulative) p50<=100.7ms p99<=671.1ms
  fetch (cumulative) p50<=0.3ms p99<=1.3ms empty=69.97% errors=0
  gate wait (cumulative) p50<=2.1ms p99<=167.8ms
  acks=4,211,410 errors=0 ack/s avg=6,380 max=14,403
  ack latency (cumulative) p50<=1.0ms p99<=12.6ms
  drain: complete after 0.1s
```

</details>
