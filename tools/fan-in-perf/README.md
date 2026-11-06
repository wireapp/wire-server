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
order and report rendering for the known experiments. `--payload-bytes 1024`
runs them with a more realistic payload size (a typical message event: ~400
bytes of envelope plus the encrypted message). `./run-team-1.py` runs the same
experiments with `--targets team:1`: 2000 users with 2 clients each read one
team stream (4000 readers). `./run-team-1-1-client.py` does the same with 1
client per user (2000 readers). `./run-2000x1-2-clients-1kb.py` runs `user:2000x1` with
2 clients per user (4000 readers) and 1 KB payload. All share
`./drivers-lib.py`.

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
cabal run fan-in-perf -- --db "$DB" consume --writers 32 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --reader-pool-size 64 --safety-window 200 --drain-timeout 30
consume: writers=32 readers=2000 targets=user:2000x1 seed=42
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

#### partman

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

#### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" produce --writers 64 --targets user:2000x1 --warmup 5 --duration 6 --seed 42
```

As above, but `reset-db` without `--partitioned` only resets the database
_without_ creating a pg_partman configuration.

### B: adding and consuming by 2,000 clients

#### partman

```
fan-in-perf --db "$DB" reset-db --partitioned
fan-in-perf --db "$DB" consume --writers 32 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --reader-pool-size 64 --safety-window 200 --drain-timeout 30
```

`consume` produces and consumes at the same time to measure the consumption
rates. It has a smaller number of `--writers` configured to give space for
consumer threads.

- readers: one per client of every online user (default: the whole pool). `user:2000x1` has 2000 users, all online, with one client each, hence 2000 readers (see [Defining the simulation](#defining-the-simulation))
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

#### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" consume --writers 32 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --reader-pool-size 64 --safety-window 200 --drain-timeout 30
```

### C: Like B, but every client acknowledges every notification

#### partman

```
fan-in-perf --db "$DB" reset-db --partitioned
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

`consume-ack` produces and consumes (like B), but acknowledges every Nth
notification. Here `--ack-every 1` leads to acknowledging every notification.

#### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 12 --seed 42 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

### D: Like C, but for 66 s while TTL garbage collection runs

We just need to make sure the garbage collection happens by increasing the
duration with `--duration 66` (experiment runtime: 66 Seconds).

#### partman

```
fan-in-perf --db "$DB" reset-db --partitioned
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 66 --seed 42 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

#### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" consume-ack --writers 16 --targets user:2000x1 --warmup 5 --duration 66 --seed 42 --reader-pool-size 64 --safety-window 200 --drain-timeout 30 --ack-every 1
```

### E: Like B, but split phases: produce only, then 2,000 clients consume only

#### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" produce --writers 128 --targets user:2000x1 --warmup 5 --duration 30 --seed 42
fan-in-perf --db "$DB" vacuum
fan-in-perf --db "$DB" consume-only --reader-pool-size 32 --targets user:2000x1 --seed 42 --warmup 2 --timeout 1200
```

The `vacuum` command runs the Postgresql
[`VACUUM`](https://www.postgresql.org/docs/current/sql-vacuum.html) statement
to ensure we've got a reproducible clean state before the consumption starts.
`consume-only` is like `consume`, but without feeding producers.
(Retrospectively, `consume` should probably have been named e.g.
`produce-consume`.) `--timeout 1200` allows the readers / consumers 1200 to
finish.

#### partman

Not run as its 5 Minutes retention would drop partitions during the consume phase.

### F: Like E with acknowledges: produce only, then 2,000 clients consume and acknowledge only

#### plain

```
fan-in-perf --db "$DB" reset-db
fan-in-perf --db "$DB" produce --writers 128 --targets user:2000x1 --warmup 5 --duration 30 --seed 42
fan-in-perf --db "$DB" vacuum
fan-in-perf --db "$DB" consume-only-ack --reader-pool-size 32 --ack-every 1 --targets user:2000x1 --seed 42 --warmup 2 --timeout 1200
```

#### partman

Not run as its 5 min retention would drop partitions during the consume phase.

## Defining the simulation

While the many parameters were mostly optimized for the author's machine or
patience, some arguments required special attentions as they define the meaning
of the simulation. Extracted from `--help`:

```
--targets SPEC           Target mix: comma-separated entries KIND:STREAMS[xK].
                           KIND: user|clients|team|epoch|connections (each at most once).
                           STREAMS: distinct stream keys pre-generated for that kind.
                           K: targets of that kind per push (default 1, at most STREAMS).
                           Each push picks one entry uniformly, then K distinct keys of it;
                           a push never mixes kinds. For 'clients', K counts users
                           (rows written = K x --clients-per-user).
                           Example: user:1000x20,team:10
                             user: pool of 1000 keys, 20 per push
                             team: pool of 10 keys, 1 per push
--clients-per-user C     Client ids (devices) per user (a 'clients' push writes
                         K x C rows) (default: 1)
--online-users U         Users that are online, i.e. have readers (readers = U
                         x --clients-per-user, must stay within the reader
                         limit). Default: all users of the 'user'/'clients'
                         pool, capped to the reader limit; required if
                         --targets has no such entry
--payload-bytes B        Approximate JSON payload size (default: 512)
```

### Readers

`--targets` says what the writers can address. Of the users in its `user` /
`clients` pool, `--online-users` are online, and each of them has
`--clients-per-user` (C) clients; every client is one reader thread.

```
addressable users (pool of `user`/`clients` in --targets)
  `- first U = --online-users (default: all)          U <= pool size
       `- C clients each (--clients-per-user)
            `- readers = U x C                         <= 100000
```

The default takes as many pool users as the limit allows (100000 / C).

Without a `user` / `clients` pool (e.g. `team:10`) there is no pool to take
users from: `--online-users` is required and the users are synthetic.

| Command line                                                 | Online users | Readers           |
| ------------------------------------------------------------ | ------------ | ----------------- |
| `--targets user:2000x1` (experiments A-F)                    | 2000         | 2000 (1 per user) |
| `--targets user:1000x1 --clients-per-user 2`                 | 1000         | 2000 (2 per user) |
| `--targets user:1000000x20 --online-users 2000`              | 2000         | 2000              |
| `--targets user:1000000x20 --clients-per-user 2`             | 50000        | 100000 (limit)    |
| `--targets team:10 --online-users 2000 --clients-per-user 3` | 2000         | 6000              |

Streams of users that are not online have no reader: the `coverage` line of a
run shows the share of streams with a reader, and the expected deliveries only
count the online users.

A reader polls the streams of its (user, client):

```
reader = (user U, client c)
 |- user stream of U            if `user` in --targets
 |- stream of (U, c)            if `clients` in --targets
 |- stream of U's team          if `team` in --targets (user u is in team u mod #teams)
 `- E epoch streams of U        if `epoch` in --targets (--epochs-per-user, shared by U's clients)
```

Epoch membership is per client in the RFC (`epoch_history` is keyed by
`(groupId, epoch, userId, clientId)`) and in MLS (`mls_group_member_client`).
The tool simplifies it to per user: all clients of a user are in the same
epochs, the common case. A client that is not (yet) in a group its siblings are
in is not modelled. Acks are still per (user, client).

A delivery is one notification handed to one reader, so a row in a stream with
R readers counts R times: a team stream is read by all clients of the team's
users.

## Experimental Gundeck API

Gundeck exposes endpoints under `/i/fan-in-perf/` when its YAML configuration contains
`postgresql` libpq connection settings. Omit that field to disable the API.
Use the local development service stack and its mock AWS configuration; this
adapter never calls SNS.

Use a dedicated database and database user for this benchmark, separate from
Brig and Galley. The PostgreSQL server may be shared, but the database must not
be. Provision `fan_in_perf` and its user before running schema setup. These
settings do not inherit Brig or Galley connection configuration.

Helm overrides to merge with the existing deployment values (including its
Gundeck RabbitMQ credentials):

```yaml
gundeck:
  config:
    postgresql:
      host: postgresql
      port: "5432"
      dbname: fan_in_perf
      user: benchmark
    postgresqlPool:
      size: 32
      acquisitionTimeout: 5
      idlenessTimeout: 600
  secrets:
    pgPassword: "YOUR_BENCHMARK_DATABASE_PASSWORD"
```

The chart mounts the password from the Gundeck Secret and sets
`postgresqlPassword: /etc/wire/gundeck/secrets/pgPassword`; it does not put the
password in the ConfigMap. For local runs, use the same `postgresql` and
`postgresqlPool` fields at the top level of `gundeck.yaml`, and optionally set
`postgresqlPassword` to a local password file, following Brig's convention.

Initialize the dedicated database with the existing `reset-db --partitioned`
command before starting Gundeck. Service startup does not reset or migrate data.
The default pool has 32 connections, a five-second acquisition timeout and a
600-second idleness timeout; `postgresqlPool` overrides these settings.
For this spike Gundeck depends on the `fan-in-perf` library to reuse its SQL;
this is an experimental dependency, not a proposed production package layout.

Each endpoint accepts its own JSON body. For example, publish with
`POST /i/fan-in-perf/publish` and `{"payload":{"text":"test"}}`.

Requests use a JSON body without authentication in the isolated benchmark
environment. This is an experimental internal API.
It operates exclusively on the synthetic group `fan-in-perf-experimental`, epoch
1; no production group IDs can be supplied. Each publish stores one shared row.

| Endpoint (POST) | JSON body fields | Response |
| --- | --- | --- |
| `/i/fan-in-perf/publish` | `payload`: JSON object | `null` after persistence and sending a wakeup |
| `/i/fan-in-perf/fetch` | `cursor`: UUID, `limit`: integer 1–1000 | Array of `[notification UUID, payload]` pairs |
| `/i/fan-in-perf/initialize` | `user`: synthetic user UUID, `client`: client ID | Latest UUID or `null`; initializes the client's acknowledgement |
| `/i/fan-in-perf/acknowledge` | `user`, `client`, `notification`: UUID | `null` after monotonic acknowledgement update |

Bodies are limited to 64 KiB. Invalid requests return 400, unsupported methods 405, oversized bodies 413, and storage or wakeup failures 503 with
no database details. Existing routes continue through Gundeck's usual handler.

`initialize` starts at the latest event: invoke it before publishing, not when
resuming a disconnected client. The API currently has no resume operation.
The existing benchmark readers' cursor/look-back handling still needs to be
preserved when building the WebSocket reader.

Publishing stores the event first, then sends a persistent JSON wakeup identifying
its group and epoch to the durable `fan-in-perf-wakeups` queue. The queue is
created at startup when the experimental PostgreSQL configuration is present,
using Gundeck's configured RabbitMQ connection and vhost. Event payloads stay in
PostgreSQL. Fetching, initialization and acknowledgements do not send wakeups.

A failed storage write sends no wakeup. A wakeup failure returns 503 without
retrying the database write; the event may already be persisted. The two writes
are not atomic and retrying a failed publish can create duplicate events. This
prototype uses the existing RabbitMQ publishing path without publisher confirms:
a successful HTTP response does not prove durable broker acceptance. Do not use
these responses as a broker-delivery counter.

### Wakeup worker and Cannon receiver

The worker starts alongside Gundeck when experimental PostgreSQL is configured.
Before publishing, register each synthetic participant with
`POST /i/fan-in-perf/register`, using `{"user":"<user-uuid>","client":"<client-id>"}`.
Registration is idempotent and separate from acknowledgement initialization.
Participants persist in `fan_in_perf_participants`; the benchmark's explicit
`reset-db` command recreates this table (also required when upgrading an older
benchmark database). Do not reset a database whose results you want to retain.

Connect those clients through Cannon's ordinary WebSocket endpoint with their
client IDs so that existing presence registration runs. For every wakeup, the
worker looks up current Redis presences for registered participants, filters by
client ID and sends one signal per distinct Cannon address. There is no static
Cannon list. Offline recipients do not block queue progress; they must catch up
when connecting. Database or presence lookup failures requeue the wakeup.
This fixture represents one fixed group, not production MLS authorization.

The worker uses a dedicated RabbitMQ connection with the existing reconnect
helper and re-declares the durable queue when a channel opens. It prefetches one
message, forwards the signal to every target's `POST /i/fan-in-perf/wakeup`
endpoint, then acknowledges the RabbitMQ delivery only if all targets succeed.
HTTP calls have a two-second timeout and do not follow redirects. Any failed
target causes a one-second delay and requeue; already-successful targets may
therefore receive duplicate signals. Malformed or oversized queue messages are
rejected without requeue and logged. This serial worker is a starting point,
not a tuned throughput configuration.

Cannon validates the fixed stream signal and returns 204 after advancing an
in-memory generation counter. The counter provides bounded, coalescing wakeup
state shared by future readers: one client cannot consume another client's
signal. Its HTTP success means signal acceptance, not event delivery. On restart,
clients must catch up from PostgreSQL rather than rely on this in-memory state.
The receiver is an internal experimental route on this branch.

Cannon fetching/WebSocket delivery, a WebSocket load generator, controlled
arrival rates and reconnect handling are still pending. The receiver currently
records wakeups but does not deliver event payloads. Do not interpret these HTTP
responses as end-to-end delivery measurements.
