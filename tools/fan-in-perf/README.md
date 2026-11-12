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

For a distributed test on the backend-integration-tests cluster without Terraform, see the
[Kubernetes setup](kubernetes/README.md).

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

Once per second, output shows successful publishes, received deliveries, sent
acknowledgements, confirmed deliveries, and outstanding work at each stage.
`outstanding` remains expected deliveries minus confirmed deliveries. This is delivery backlog, **not RabbitMQ
queue depth**; inspect RabbitMQ separately to locate wakeup-processing backlog.
The final JSON includes achieved publish rate, missing deliveries, publish errors,
and delivery latency mean, maximum and approximate p50/p95/p99. Latency runs from
just before the HTTP publish request to receipt of a WebSocket page, using the
load generator's monotonic clock. Receipt samples are recorded immediately, even when acknowledgement confirmation
is still pending. Percentiles are power-of-two millisecond bucket
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

#### Diagnosing a slow drain

The final JSON separates these stages (delivery counters count events per device):

- `received_deliveries`: this run's events received over WebSockets.
- `ack_sent_deliveries`: received events covered by successfully sent ack frames.
- `acknowledged_deliveries`: events whose ack confirmation was received.
- `not_received_deliveries`: expected events not yet received.
- `ack_not_sent_deliveries`: received events whose ack send has not completed.
- `ack_sent_unconfirmed_deliveries`: events with an ack sent but no confirmation.
- `received_unconfirmed_deliveries`: the sum of the previous two ack stages.

`missing_deliveries` retains its previous meaning: expected minus confirmed, not
proof of message loss. `ack_requests_sent` and `ack_confirmations_received` count
**pages**, since one ack can cover up to 100 events. `ack_latency_*_ms` measures
from just before sending an ack to receipt of its matching confirmation, including
send time, server persistence and scheduling. Its histogram counts confirmed
pages, not events; p50/p95/p99 are power-of-two millisecond upper bounds. Pending
acks are absent from that histogram, so inspect the pending ages as well.

`lagging_devices` lists every device with unconfirmed expected events or a pending
ack at the final snapshot, including on timeout or another failure. Devices use
run-local indices, not real user identifiers. Each entry contains
`last_sequence_received`, `last_sequence_confirmed`, `ack_pending`,
`ack_send_completed`, and `pending_ack_age_ms` (null when no ack is pending).
Pending age starts just before the send attempt. Device state uses bounded memory
per configured device and is retained when sessions are cancelled at shutdown.
These are client observations: a missing confirmation does not establish whether
the server has already persisted the acknowledgement.

For the next diagnostic run, keep the workload unchanged and use
`--drain-seconds 120`. The separate `--timeout-seconds` still bounds each wait for
an acknowledgement confirmation; increase it too if you want to observe a single
pending ack for longer than the default 30 seconds. Compare the counters with
PostgreSQL pool waits, locks and queries, RabbitMQ queue depth, and service CPU.

#### Server-side acknowledgement timings

Cannon exposes `fanin_perf_server_ack_seconds{stage="..."}` histograms at
`http://127.0.0.1:8083/i/metrics` when the experimental PostgreSQL configuration
is enabled. These are separate from the load generator's client-side JSON.

| Stage | Boundaries |
| --- | --- |
| `pool_acquire` | Start of the acknowledgement storage call to entry into its first acquired PostgreSQL session; includes effect dispatch and new-connection setup if needed |
| `database_write` | Execution of the acknowledgement upsert on the acquired connection, including the database round trip and autocommit |
| `persistence` | Entire acknowledgement storage operation, including acquisition, upsert and any retries |
| `receive_queue` | Ack frame read by Cannon's receiver thread to validation/dequeue by its delivery loop |
| `confirmation_send` | Sending the confirmation frame, including socket-write scheduling and existing send retries |
| `receive_to_confirmation` | Ack frame read by Cannon to completion of the confirmation send |

All measurements use monotonic time, in seconds. Labels are fixed stage names;
there are no per-user or per-device metric labels. Only completed stages produce
samples. Failed or cancelled writes/sends do not count as successful completions.
`pool_acquire` measures the first acquisition of each ack operation; a retried
session does not produce another acquisition sample. Successful retried writes
are measured normally, and `persistence` includes the complete retry duration.
The upsert statement and retry policy are unchanged.

The existing `wire_hasql_pool_connection_acquisition_seconds` measures opening
new connections, **not waiting for a pooled connection**. Use the new
`pool_acquire` stage for that investigation. Stages overlap: acquisition and write
are inside persistence, and the other stages are inside the receive-to-confirmation
measurement. Do not add all six durations or subtract their percentiles.
Server measurements do not include time before the receiver reads the frame or
time after the send returns, so compare them with client ack-confirmation latency.

Restart the services with the rebuilt binaries. The benchmark OTel collector now
scrapes Cannon on `host.docker.internal:8083/i/metrics` every second. Restart it
to load that configuration:

```bash
docker compose -f tools/fan-in-perf/docker-compose.yaml restart otel-collector
curl -s http://127.0.0.1:8083/i/metrics | rg 'fanin_perf_server_ack_seconds'
```

Samples appear after acknowledgements are processed. If the collector is not
running, start the benchmark Compose stack first. Ensure the benchmark Docker
network can reach the host's port 8083; a working localhost curl alone does not
prove container connectivity. Verify `up{job="fan-in-cannon"}` is 1 in Grafana.

Mean duration by stage in milliseconds (one-minute window):

```promql
1000 * sum by (stage) (rate(fanin_perf_server_ack_seconds_sum{job="fan-in-cannon"}[1m]))
  / sum by (stage) (rate(fanin_perf_server_ack_seconds_count{job="fan-in-cannon"}[1m]))
```

Approximate p95 by stage in milliseconds:

```promql
1000 * histogram_quantile(0.95,
  sum by (stage, le) (rate(fanin_perf_server_ack_seconds_bucket{job="fan-in-cannon"}[1m]))
)
```

Repeat the same workload. A high `pool_acquire` suggests pressure on pool capacity;
a high `database_write` directs attention to database queries, locks, I/O or the
network round trip. `receive_queue` and `confirmation_send` help distinguish
Cannon scheduling and socket-write delays. The instrumentation adds some overhead;
keep it enabled consistently when comparing runs.

The receive handoff now has additional stages in the same histogram family:

| Stage | Boundaries |
| --- | --- |
| `ack_parse` | WebSocket read returns → ack decoding completes; decoding and the numeric tag are explicitly evaluated in the receiver thread |
| `ack_enqueue` | Parsing completes → the STM queue write returns |
| `enqueue_to_dequeue` | Parsing completes, immediately before enqueue → queue read returns in the delivery thread, timestamped **inside** the timeout wrapper |
| `ack_timeout_exit` | Queue read returns → `System.Timeout.timeout` returns to the delivery loop |
| `ack_validate` | Timeout wrapper returns → the delivery loop validates the expected tag |
| `event_page_send` | The preceding event-page send call starts → returns, including encoding and existing send retries |
| `event_send_after_ack_read` | Ack read → preceding event-page send returns, or zero if the send had already returned |

`enqueue_to_dequeue` includes enqueueing, not just residence in the queue. A
precise STM commit timestamp would require unsafe instrumentation or additional
synchronisation; neither is used here. Compare it with `ack_enqueue` rather than
adding both. An enqueue completion timestamp can be later than dequeue when the
producer is delayed after commit. All timings are elapsed time and can include
thread scheduling and GC pauses.

The broad `receive_queue` measurement is retained. Its subdivisions are
`ack_parse`, `enqueue_to_dequeue`, `ack_timeout_exit` and `ack_validate`.
`event_send_after_ack_read` overlaps those intervals and reveals whether the send
call was still returning after the ack arrived; it is not an additional cost to
sum. Send and validation samples are emitted for pages with a matching ack;
parse/enqueue samples can also exist for invalid acknowledgements. The existing
Grafana query automatically includes the new labels.

Restart Cannon after rebuilding, keeping the collector running. With the current
`socat` workaround, stop `socat` before restarting `make cr`, then restart the
forwarder once Cannon is listening. Repeat the same workload to compare stages.

### Direct delivery versus wakeup + HTTP fetch

Both `smoke` and `load` accept `--fetch-percent` (0–100, default 0):

- `0`: all devices use `/i/fan-in-perf/events`, receiving payload pages and
  acknowledging them over WebSocket (the previous behavior).
- `100`: all devices use `/i/fan-in-perf/wakeups`. Cannon sends availability
  signals, including the durable cursor on connection. Devices fetch pages of up
  to 100 events from Gundeck's `/i/fan-in-perf/fetch` and acknowledge the last ID
  through `/i/fan-in-perf/acknowledge`. Cannon does not fetch payloads for this mode.
- `50`: half the devices use each path, receiving the same shared group events.
  For load, `floor(devices * fetch-percent / 100)` devices fetch; the rest receive
  directly. Smoke has two devices: percentages below 50 select zero fetch devices,
  50–99 select one, and 100 selects both.

Each fetch device runs one sequential fetch/ack loop until an empty page. It then
sends `{"type":"caught-up","tag":N}` on its wakeup socket. This releases the next
availability signal; it does **not** persist any acknowledgement. Cannon coalesces
wakeups during catch-up, keeping one signal in flight per device. A generation
snapshot prevents signals arriving during catch-up from being lost. Reconnecting
starts at the durable device cursor, so an unacknowledged HTTP page can be replayed.
No full production protocol, native SNS push, or client authentication is added;
these remain opt-in, internal synthetic benchmark endpoints.

The fetch client reports an acknowledgement confirmation only after a successful
HTTP response. Its ready state requires an empty fetch. Failure of either fetch or
ack aborts the run; ambiguous acknowledgements are not automatically retried.

Load summaries retain their aggregate fields and add `fetch_percent` and `by_mode`
(`direct` and `fetch`). Each mode reports its actual device count, expected/received/
acknowledged deliveries, missing deliveries, publish-to-receive latency, publish-to-confirmed-ack latency,
and acknowledgement round-trip latency. Fetch mode also reports successful HTTP fetch requests, fetched
events, events per fetch, mean request time, and mean time from receiving a wakeup
to completing its first fetch. Empty fetches count as requests; fetched events may
include other runs' events, while delivery counts and latency include only this
run. Missing deliveries are computed per mode so one path cannot hide the other.
Server acknowledgement-stage histograms still measure the direct Cannon path;
use client fetch timings and Gundeck pool metrics for HTTP fetch/ack.

Start locally, using the existing service and database setup. Rebuild Cannon and
fan-in-perf, then restart Cannon before testing the new route:

```bash
set -o pipefail
cabal build exe:cannon exe:fan-in-perf 2>&1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'

"$(cabal list-bin exe:fan-in-perf)" smoke --fetch-percent 0
"$(cabal list-bin exe:fan-in-perf)" smoke --fetch-percent 100
"$(cabal list-bin exe:fan-in-perf)" smoke --fetch-percent 50
```

After all three smoke cases pass, use this local matrix (six runs):

| Devices | Fetch percentage | Publish rate | Duration | Drain limit |
| --- | --- | --- | --- | --- |
| 100 | 0, 50, 100 | 10/s | 60 s | 60 s |
| 1,000 | 0, 50, 100 | 10/s | 60 s | 60 s |

Example for the mixed case:

```bash
"$(cabal list-bin exe:fan-in-perf)" load \
  --devices 100 --fetch-percent 50 --rate 10 --duration 60 \
  --drain-seconds 60 --payload-bytes 256 \
  --summary-json /tmp/fan-in-100-mixed.json
```

Keep resources and pool settings fixed. Use equivalent initial database conditions
for comparisons; reset the fixture only with services/workers stopped, using the
existing reset procedure. Retained-data runs must be labelled as such. Local tests
validate the paths and metrics; later cluster tests can extend group sizes to
1,000, 5,000 and 10,000 at the same three percentages. The Kubernetes renderer also
accepts `--fetch-percent` for smoke/load Jobs; rebuilt Cannon and generator images
are required before using it on a cluster.
