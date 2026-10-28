# fan-in-perf - load generator

## Overview

Load generator for the notification fan-in approach as proposed in the RFC
[RFC: Notification
fan-in](https://wearezeta.atlassian.net/wiki/spaces/PAD/pages/2774597689/2026-04-14+RFC+Notification+fan-in).
It facilitates the experiments of
[WPB-26288](https://wearezeta.atlassian.net/browse/WPB-26288) by also providing
metrics. The experiments themselves aren't hard-coded, so this `fan-in-perf`
tool can also be used to run a variety of other experiments (to be invented).

`./run-2000x1.py` is an experiment driver that provides the tool invocation
order and report rendering for the known experiments.

To have more control over the infrastructure setup, not interfere with existing
requirements (e.g. integration tests) and provide observability,
`./docker-compose.yaml` describes the separated test environment. It also
contains a Grafana LGTM stack with metrics fetchers.

## Setup and run

### Firewall rules

The docker-compose environment needs to reach out to the host to scrape metrics
from the tool. On NixOS this can be allowed with this Firewall rule:

```nix
networking.firewall = {
  trustedInterfaces = [ "docker0" ];
  extraCommands = ''
    # fan-in-perf: otel-collector (demo_wire) -> host tool
    iptables -I INPUT 3 -s 172.20.9.0/24 -d 172.20.9.1 -p tcp --dport 9400 -j ACCEPT
  '';
};
```

Depending on your OS and setup, this rule may differ. The `fan-in-perf` tool
can be run without this access as its `/metrics` endpoint is passive; the
metrics will then of course not be available in Grafana LGTM.

### docker-compose

The `docker-compose` env can be managed with the usual commands. E.g. to start it:

```sh
docker-compose up
```

N.B. this will emit warnings about tools metrics not being accessible while the
tool is not running.

### Grafana

As part of the `docker-compose` environment, the Grafana LGTM instance is
reachable at `http://localhost:3000`. Credentials are `admin` / `admin`.

There are two relevant dashboards:

- _fan-in-perf_: A custom board with a variety of metrics from the tool.
- _postgresql-exporter_: A generic board taken from public sources to observe Postgresql.

### Cabal

To not introduce bottlenecks on the tool side, compile `fan-in-perf` with
optimization turned on.

`cabal.project.local`:

```cabal
-- optimize only the fan-in perf tool
package fan-in-perf
  optimization: 2

```

As the `fan-in-perf` cabal project is (mostly) self-sufficient, it's good
enough to only optimize this Cabal component.

### fan-in-perf

The `fan-in-perf` tool is a binary built by Cabal. You can invoke it via Cabal
or directly by starting its binary. Additionally to load generation, it also
provides commands to manage the database.

An example run could be:

```sh
# Reset/initialize the database
$ fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/fan_in_perf" reset-db
recreated fan-in notification tables (plain)

# Run the (toy) experiment: 2 users push notifications to the other user's user stream for 10 Seconds
$ fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/fan_in_perf" produce --duration 10 --targets user:2x1
produce: writers=16 targets=user:2x1 seed=6361057343649634839
summary: duration=10.0s
  pushes=46,990 targets=46,990 errors=0 (0.00%)
  push/s avg=4,696 max=4,811 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=2.1ms p99<=21.0ms
  database: deadlocks=0 rows=47,006
```

To execute `fan_in_perf` via Cabal:

```sh
cabal run fan-in-perf -- --db "$DB" consume --writers 32 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30
consume: writers=32 clients=2000 targets=user:2000x1 seed=42
coverage user: 100.00% of streams have a reader
consume: loading start positions
consume: running
consume: writers stopped, draining
summary: duration=12.0s
  pushes=266,834 targets=266,834 errors=0 (0.00%)
  push/s avg=22,233 max=22,582 (avg incl. warmup, max after warmup)
  latency (cumulative, incl. warmup) p50<=1.3ms p99<=4.2ms
consume: delivered=266,834 expected=266,834 missing=0 duplicates=0
  cons/s avg=22,026 max=23,434 (avg incl. warmup, max after warmup, both writer phase)
  fan-out: 1.0 deliveries per push (cons/s and push/s differ by this factor)
  backlog at writer stop: 2,482 (0.1s of delivery)
  lag (cumulative) p50<=100.7ms p99<=268.4ms
  fetch (cumulative) p50<=0.4ms p99<=2.1ms empty=14.98% errors=0
  gate wait (cumulative) p50<=100.7ms p99<=201.3ms
  drain: complete after 0.2s
  database: deadlocks=0 rows=266,834
```

`fan-in-perf --help` and `fan-in-perf <command> --help` are quite useful to
explore (or remember) tool arguments.

## fan-in-perf commands tutorial

This section describes the tool invocation for each experiment. This
(hopefully!) serves as tutorial of its usage. The commands refer to the
`--quick` mode of `./run-2000x1.py`. Use larger durations to get more realistic
results.

### A: maximal rate of adding notifications

### partman

```
fan-in-perf --db "$DB" reset-db --partitioned
fan-in-perf --db "$DB" produce --writers 64 --targets user:2000x1 --warmup 5 --duration 6 --seed 42
```

`reset-db --partitioned` resets the database and setups up [pg_partman](https://github.com/pgpartman/pg_partman).
The configured retention is 5 Minutes. The partman's background-worker runs
every Minute.

The `produce` command only produces new pushes. It does not consume them. Its arguments have these meanings:

- `--writers 64`: 64 concurrent writer threads
- `--targets user:2000x1`: 2000 distinct users, every push targets one of them
- `--warmup 5`: 5 Seconds where several metrics aren't tracked (to let the CPU upscale, caches settle, ...)
- `--duration 6`: The whole experiment ends after 6 Seconds
- `--seed 42`: 42 is the seed of the random generator. I.e. with this seed the same test data is generated across runs.

### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" produce --writers 64 --targets user:2000x1 --warmup 5 --duration 6 --seed 42
```

As above, but `reset-db` without `--partitioned` only resets the database
_without_ creating a pg_partman configuration.

### B: adding and consuming by 2,000 clients

### partman

```
fan-in-perf --db "$DB" reset-db --partitioned
fan-in-perf --db "$DB" consume --writers 32 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30
```

`consume` produces and consumes at the same time to measure the consumption
rates. It has a smaller number of `--writers` configured to give space for
consumer threads.

- `--clients 2000`: simulates 2000 reader clients
- `--reader-pool-size 64`: sets the Postgresql connection pool size for the simulated readers
- `--safety-window`: in Milliseconds defines how much fetching looks back after the cursor to deal with late/delayed commits
- `--drain-timeout`: How many Seconds to drain after the writers stopped writing

The safety-window illustrated:

```
id time ──────────────────────────────────────────────►

writer 1:  id X generated ·········· slow commit ·····► COMMIT (late)
writer 2:       id Y generated ─► COMMIT
                                      │
reader:                               fetch #1 sees Y, cursor := Y
                                                              │
                                                              fetch #2

Plain (W = 0): fetch #2 asks "id > Y"
                      X  Y
   ───────────────────●──●─────────────►
                      ▲  └ cursor
                      X < Y, committed after fetch #1: never returned → missing

Safety window (W = 200 ms): fetch #2 asks "id > Y − W"
                      X  Y
   ───────────────────●──●─────────────►
                      ┌─────────────────►
                      Y−W   re-read zone: X found
                            (Y re-read too, dropped as duplicate)
```

### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" consume --writers 32 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30
```

## C: Like B, but every client acknowledges every notification

### partman

```
fan-in-perf --db "$DB" reset-db --partitioned
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

`consume-ack` produces and consumes (like B), but acknowledges every Nth
notification. Here `--ack-every 1` leads to acknowledging every notification.

### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

## D: Like C, but for 66 s while TTL garbage collection runs

We just need to make sure the garbage collection happens by increasing the
duration with `--duration 66` (experiment runtime: 66 Seconds).

### partman

```
fan-in-perf --db "$DB" reset-db --partitioned
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 66 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 66 --seed 42 --clients 2000 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

## E: Like B, but split phases: produce only, then 2,000 clients consume only

### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" produce --writers 128 --targets user:2000x1 --warmup 5 --duration 30 --seed 42
fan-in-perf --db "$DB" vacuum
fan-in-perf --db "$DB" consume-only --reader-pool-size 32 --targets user:2000x1 --seed 42 --clients 2000 --warmup 2 --timeout 1200
```

The `vacuum` command runs the Postgresql
[`VACUUM`](https://www.postgresql.org/docs/current/sql-vacuum.html) statement
to ensure we've got a reproducible clean state before the consumption starts.
`consume-only` is like `consume`, but without feeding producers.
(Retrospectively, `consume` should probably have been named e.g.
`produce-consume`.) `--timeout 1200` allows the readers / consumers 1200 to
finish.

### partman

Not run as its 5 Minutes retention would drop partitions during the consume phase.

## F: Like E with acknowledges: produce only, then 2,000 clients consume and acknowledge only

## plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" produce --writers 128 --targets user:2000x1 --warmup 5 --duration 30 --seed 42
fan-in-perf --db "$DB" vacuum
fan-in-perf --db "$DB" consume-only-ack --reader-pool-size 32 --ack-every 1 --targets user:2000x1 --seed 42 --clients 2000 --warmup 2 --timeout 1200
```

# partman

Not run as its 5 min retention would drop partitions during the consume phase.
