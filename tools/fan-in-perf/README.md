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
$ fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:55432/fan_in_perf" reset-db
recreated fan-in notification tables (plain)

# Run the (toy) experiment: 2 users push notifications to the other user's user stream for 10 Seconds
$ fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:55432/fan_in_perf" produce --duration 10 --targets user:2x1
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
`fanInPerfPostgresql` libpq connection settings. Omit that field to disable the API.
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
    fanInPerfPostgresql:
      host: postgresql
      port: "5432"
      dbname: fan_in_perf
      user: benchmark
    fanInPerfPostgresqlPool:
      size: 32
      acquisitionTimeout: "5s"
      idlenessTimeout: "600s"
  secrets:
    fanInPerfPgPassword: "YOUR_BENCHMARK_DATABASE_PASSWORD"
```

The chart mounts the password from the Gundeck Secret and sets
`fanInPerfPostgresqlPassword: /etc/wire/gundeck/secrets/fanInPerfPgPassword`; it does not put the
password in the ConfigMap. For local runs, use the same `fanInPerfPostgresql` and
`fanInPerfPostgresqlPool` fields at the top level of `gundeck.yaml`, and optionally set
`fanInPerfPostgresqlPassword` to a local password file, following Brig's convention.

Initialize the dedicated database with the existing `reset-db --partitioned`
command before starting Gundeck. Service startup does not reset or migrate data.
The default pool has 32 connections, a five-second acquisition timeout and a
600-second idleness timeout; `fanInPerfPostgresqlPool` overrides these settings.
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
Both experimental endpoints are explicit named Servant routes in Cannon’s
combined API. No middleware intercepts their paths. The wakeup route streams
JSON input and enforces its 1 KiB limit before decoding.

The WebSocket adapter described below consumes this wakeup hub. The wakeup
receiver's HTTP response alone is still not an end-to-end delivery measurement.

### Experimental Cannon event stream

Cannon now fetches the shared epoch directly through the same PostgreSQL store
code as Gundeck. Enable it in local Cannon YAML with `fanInPerfPostgresql`,
`fanInPerfPostgresqlPool` and optionally `fanInPerfPostgresqlPassword`, using the same dedicated
benchmark database as Gundeck. These are optional; without `fanInPerfPostgresql`, both
experimental Cannon endpoints reject requests. The chart does not yet expose these
Cannon settings; this milestone is intended for local service runs.

```yaml
fanInPerfPostgresql:
  host: localhost
  port: "55432"
  user: wire-server
  dbname: fan_in_perf
fanInPerfPostgresqlPassword: /path/to/local/postgres-password
fanInPerfPostgresqlPool:
  size: 32
  acquisitionTimeout: "5s"
  idlenessTimeout: "600s"
```

Register each synthetic `(user, client)` via Gundeck's registration endpoint,
then open `ws://<cannon-host>:<port>/i/fan-in-perf/events` with headers
`Z-User: <user-uuid>`, `Z-Client: <client-id>` and a fresh
`Z-Connection: <connection-id>`. This internal-only benchmark endpoint trusts
these identities, as do the other internal fixture APIs; do not expose it through
public ingress. Unknown participants are rejected before upgrading the socket.
It uses ordinary Cannon presence registration, so there is no extra connection
to open and no static Cannon list to configure.

The server sends JSON (binary WebSocket messages):

```json
{"type":"events","tag":1,"events":[{"id":"<notification-uuid>","payload":{}}]}
```

Reply with `{"type":"ack","tag":1}`. The tag is scoped to this connection.
Cannon persists the last event's position for that device, then sends
`{"type":"acked","tag":1}`. It sends `{"type":"ready"}` whenever catch-up
finds no more events and waits for another wakeup. The generation is sampled
before fetching to avoid losing wakeups that arrive during catch-up.

There is at most one page (100 events) in flight per connection. A missing ack
closes the session after 30 seconds; malformed or wrong-tag acks also close it.
Incoming frames/messages are limited to 1 KiB. The experimental protocol uses
this conservative stop-and-wait policy for correctness, not peak throughput.

A registered device without a saved ack reads all retained fixture events.
Reconnects resume after the saved ack; unacknowledged pages are replayed.
Do not call `initialize` on reconnect: that separate benchmark operation jumps
the cursor to the newest event. Initial connection catch-up also recovers events
published while the device was offline, without requiring a fresh wakeup.

This is still a fixed-epoch prototype: membership changes, expired-cursor gap
reporting, native pushes, controlled arrival rates and a production delivery-tag
protocol are not implemented. Use a single publisher for this milestone: UUID
cursor ordering with concurrently committed writes remains a known limitation
of the existing store; this path does not implement the polling benchmark's
look-back window. The new delivery unit tests include a real local WebSocket
transport test with two clients and independent reconnect progress, backed by an
in-memory store. They do not launch the full Gundeck/RabbitMQ/PostgreSQL stack.


The benchmark Compose stack publishes PostgreSQL on `127.0.0.1:55432`; the
regular development stack keeps `5432`. Exporters use the benchmark container's
internal port `5432`. Both Gundeck and Cannon must use `55432` for local runs.
The generic `postgresql` configuration namespace is reserved for a future
ordinary service database connection; it does not enable the benchmark. Rename
existing experimental configuration keys to `fanInPerfPostgresql`,
`fanInPerfPostgresqlPool` and `fanInPerfPostgresqlPassword` (and the Gundeck Helm
secret key to `fanInPerfPgPassword`). No database tables or data are renamed.

To opt in with the local service runner, add the Cannon YAML block above to
both `services/cannon/cannon.integration.yaml` and
`services/gundeck/gundeck.integration.yaml`, using an absolute password-file
path. Keep these overrides local: ordinary service runs do not need the
benchmark database. Initialize the dedicated benchmark schema explicitly before
starting the services; never run `reset-db` against the regular database.

### End-to-end smoke command

When running the binary directly, make the Nix development libraries available
to the runtime loader. From the repository root, run once in your terminal:

```bash
export LD_LIBRARY_PATH="$PWD/.env/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
```

This also applies to the `load` command below. `LIBRARY_PATH` from direnv is used
at link time; it does not replace `LD_LIBRARY_PATH` at runtime. Without this,
the executable may fail to locate `librusty_jwt_tools_ffi.so` before it starts.

With the regular development dependencies, benchmark PostgreSQL, Gundeck and
Cannon running and configured as above, build and run:

```bash
set -o pipefail
cabal build exe:fan-in-perf 2>&1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
"$(cabal list-bin exe:fan-in-perf)" smoke \
  --gundeck-url http://127.0.0.1:8086 \
  --cannon-url ws://127.0.0.1:8083 \
  --timeout-seconds 30
```

These are the defaults; `fan-in-perf smoke` is sufficient when using those
addresses. Only internal `http://` and `ws://` base URLs are supported. Run this
against the dedicated experimental setup, with no other publisher active.

The command registers two fresh synthetic devices and initializes their cursors
past existing events. It checks that both receive and acknowledge the same live
event, disconnects one device, publishes another event, and verifies offline
catch-up on reconnect. Finally, both reconnect to verify acknowledged events are
not replayed. Each step prints `RUN`/`OK`; success prints `PASS`, and failure or
timeout exits nonzero with `FAIL`.

This exercises the real Gundeck publish, RabbitMQ worker, Redis presence,
Cannon WebSocket and PostgreSQL acknowledgement path. It does not test native
pushes or load capacity. It neither starts services nor resets the database and
does not require `--db`. Each successful run leaves two participant records and
two events in the fixture. The timeout applies per step; connection setup and
teardown are additionally bounded by an overall timeout of twelve times that
value.

The command's unit tests use in-process HTTP/WebSocket fixtures, including
missing-wakeup and missing-ack-persistence failures. Passing those tests alone
does not establish that the full service stack is configured correctly.

### Service load generator

After the end-to-end smoke test passes, use `load` to exercise the same HTTP,
RabbitMQ wakeup and Cannon WebSocket path with a configurable number of online
devices. Build `exe:fan-in-perf` as above, then run:

```bash
"$(cabal list-bin exe:fan-in-perf)" load \
  --devices 100 \
  --rate 10 \
  --duration 60 \
  --payload-bytes 256 \
  --drain-seconds 30 \
  --summary-json /tmp/fan-in-load.json
```

This represents one shared group with 100 eligible devices and ten application
events per second: 1,000 expected device deliveries per second. Each device uses
a fresh synthetic user and client. Registration and initial catch-up finish
before publishing begins. Connection setup is limited by `--connect-concurrency`
(default 20); a slot is released when a device first reaches `ready`, while that
WebSocket stays open. This separates startup pressure from steady-state load.
Use a larger value explicitly when testing connection bursts. Every received page is acknowledged, and the command
waits for all expected acknowledgement confirmations before succeeding.

Options (all counts and durations must be positive):

| Option | Default | Meaning |
| --- | --- | --- |
| `--devices` | 10 | Number of online WebSocket clients, up to 100,000 |
| `--connect-concurrency` | 20 | Maximum sessions concurrently connecting and awaiting initial readiness |
| `--rate` | 1 | Target publishes per second, up to 100,000 |
| `--duration` | 30 | Publishing duration in seconds, up to 86,400 |
| `--payload-bytes` | 256 | ASCII padding size, excluding the JSON envelope; up to 60,000 |
| `--drain-seconds` | 30 | Deadline for outstanding acknowledgements, up to 3,600 |
| `--setup-seconds` | 300 | Separate deadlines for registration and connection readiness, up to 86,400 |
| `--timeout-seconds` | 30 | HTTP request and acknowledgement-confirmation timeout, up to 300 |
| `--gundeck-url` | `http://127.0.0.1:8086` | Internal Gundeck base URL |
| `--cannon-url` | `ws://127.0.0.1:8083` | Internal Cannon base URL |
| `--summary-json` | unset | Optional final JSON output file |

The publisher is sequential because this prototype does not yet handle the
concurrent-commit ordering hazard. It does not queue an unbounded backlog or
send catch-up bursts if it falls behind. Compare the achieved publish rate with
the target: a passing run below the target does not prove capacity at that rate.
The publishing window excludes setup and drain; an in-flight HTTP request can
extend it by the request timeout.

Once per second, output shows successful publishes, confirmed device deliveries,
and their difference (`outstanding`). This is delivery backlog, **not RabbitMQ
queue depth**; inspect RabbitMQ separately to locate wakeup-processing backlog.
The final JSON includes achieved publish rate, missing deliveries, publish errors,
and delivery latency mean, maximum and approximate p50/p95/p99. Latency runs from
just before the HTTP publish request to receipt of a WebSocket page, using the
load generator's monotonic clock. Samples are committed to the statistics after
acknowledgement confirmation. Percentiles are power-of-two millisecond bucket
upper bounds, not exact percentiles. Measurement includes generator scheduling
and HTTP latency; monitor generator CPU and file-descriptor limits at high load.

Any device/protocol failure, publish failure or drain timeout fails the run with
a nonzero exit status. Failed publishes are not retried: a failed HTTP response
can correspond to an already-persisted event. Consequently, counts on a failed
run can include deliveries from a publish not counted as successful; do not use
that run as a clean throughput result. The JSON `success` field records this.

Run only one publisher against the dedicated experimental database. This first
version models one fixed epoch, all clients online, and one selected Cannon;
it does not simulate reconnect churn, multiple groups, multiple Cannon addresses
or native pushes. It does not change worker concurrency or reset the database.
Participant records and events remain after each run; accumulated participants
can increase worker lookup costs. Use a fresh benchmark fixture for comparable
runs (the documented `reset-db` command deletes previous benchmark data).
Stop the service runner before resetting and restart it after schema setup
finishes. Otherwise, Gundeck can process queued wakeups while the participant
table is temporarily absent, resulting in missing-table errors and requeues.

If startup fails before publishing, the command reports how many devices reached
readiness and the failure category. Other connections are cancelled, so Cannon
may subsequently log `ConnectionClosed`; those messages alone do not identify
the original failure. Check the first error and `ulimit -Sn` in both the generator
and service-launch shells. Both processes need room for the requested sockets
plus database, HTTP and other descriptors. Raising a shell limit affects only
processes subsequently started from that shell; restart affected services if
needed. Lowering connection concurrency does not reduce the final socket count.
