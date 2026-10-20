# fan-in-perf: Experiment C (`consume-ack`) — Design

Ticket WPB-26288, experiment C: maximal rate of adding and consuming
notifications by ~2,000 clients that acknowledge all notifications. Extends
`2026-10-12-fan-in-perf-consume-design.md` (experiment B) and shares its
writers, readers, drain, stats, metrics and terminal code. Where the B spec and
this one disagree (population), this one wins.

## Goal

Measure whether PostgreSQL carries the fan-in read path **plus ack writes**
while writers keep producing. Report everything B reports plus ack writes/s
(current, max), ack latency and ack errors. All DB access through
`Wire.FanInNotificationsStore`.

## Non-goals (follow-ups)

- **Missed-notification check** (RFC "Finding streams which have missed
  notifications") and **re-reading acks on reconnect**: part of the connect
  storm experiment.
- **Server flow control** (RFC: serve ~100 notifications, then wait for acks).
  Readers keep fetching regardless of acks; a large `--ack-every` simulates a
  client that acks rarely, not a server blocked on acks.
- **`connections` acks** (`local_connection_acks`, `remote_connection_acks`):
  `connections` stays rejected, as in B.
- **Flushing** the remainder below N at page end or drain: unacked
  notifications would just be redelivered on reconnect.
- Experiment D (TTL GC).

## CLI

```
fan-in-perf --db CONNSTR [global flags] consume-ack [consume flags] [--ack-every N]
```

| Flag | Default | Meaning |
|---|---|---|
| `--ack-every N` | 100 | a client acks after every N-th notification it received (all streams), `1..10_000` |

- All `consume` flags via the same parser. `ConsumeOptions` gains
  `mode :: ReaderMode`:

  ```haskell
  data ReaderMode = NoAcks | Acks {ackEvery :: Int}
  ```

  `consume` sets `NoAcks`, `consume-ack` sets `Acks`. One `Consume` command
  constructor; `Run`/`defaultPoolSize` unchanged. The experiment label
  (`consume` / `consume-ack`) derives from the mode.
- `consume-ack` rejects `connections` (like `consume`) and `user` and
  `clients` entries with different STREAMS (see Population).

## Population (all experiments)

B's population has no coherent client identity: reader `i` reads user stream
`userPool[i mod S_u]` and client stream of `clientsPool[i mod S_c]`, two
independently generated users, and clients of one user can land in different
teams. Acks must be written with the identity the notification was fetched
with, so C needs one identity per reader. The coherent model applies to A, B
and C (pure startup work, no DB cost).

### Shared user ids (`FanInPerf.Targets`)

One distinct uid vector of length `max(S_u, S_c)`; the user pool takes the
first `S_u`, the clients pool the first `S_c`. A and B stay unconstrained. No
wrap-around (`mod`): it would duplicate `(uid, cids)` keys in one pool and a
push could insert the same client row twice (PK violation). Push SQL, tables
and key randomness are unchanged; `user_*` and `client_*` rows sit in
different tables, so shared uids add no contention. The same `--seed`
generates different keys than earlier runs (RNG draw order changes).

### Types (`FanInPerf.Population`)

```haskell
data SimTeam = SimTeam
  { tid :: TeamId,
    members :: NonEmpty SimUser
  }

data SimUser = SimUser
  { uid :: UserId,
    epochs :: [(GroupId, Epoch)],  -- [] : no 'epoch' in --targets
    clients :: NonEmpty ClientId   -- 1..k, each one reader thread
  }

data Population = Population
  { teams :: [SimTeam],            -- 'team' in --targets: every user in exactly one team
    personalUsers :: [SimUser],    -- otherwise: all users here
    kinds :: Set TargetKind,       -- kinds present in --targets
    subscribers :: Map Key Int     -- readers per stream (unchanged)
  }

-- | What one reader thread needs.
data Reader = Reader
  { team :: Maybe TeamId,
    user :: SimUser,
    cid :: ClientId
  }

readers :: Population -> [Reader]
readers p =
  [Reader (Just t.tid) u c | t <- p.teams, u <- toList t.members, c <- toList u.clients]
    <> [Reader Nothing u c | u <- p.personalUsers, c <- toList u.clients]

-- | The only place reader streams come from.
readerStreams :: Set TargetKind -> Reader -> [Stream]
readerStreams kinds r =
  [StreamUser r.user.uid | KindUser `Set.member` kinds]
    <> [StreamClient r.user.uid r.cid | KindClients `Set.member` kinds]
    <> [StreamTeam t | Just t <- [r.team]]
    <> [uncurry StreamEpoch e | e <- r.user.epochs]
```

A reader fetches and acks as `(r.user.uid, r.cid)`; `readerStreams` derives
the user and client streams from the same pair, so fetch and ack identities
cannot diverge.

### Construction (`mkPopulation`)

- User set: `S = max(S_u, S_c)` shared uids if `user` or `clients` is in
  `--targets`; otherwise `S = ⌈N / --clients-per-user⌉` synthetic, seeded uids.
- `N` clients round-robin over the users: user `u` gets clients `1..k_u`,
  `k_u = ⌈(N − u) / S⌉` (counts differ by at most 1). Users without clients
  are not in the population.
- Epochs per user: `E` consecutive epoch streams starting at
  `(u × E) mod S_e`; all clients of a user share them.
- Teams: user `u` joins team `u mod S_t`; teams without members are dropped
  (the coverage line reports unread team streams).
- Checks (startup error): with `clients` present `max k_u ≤ --clients-per-user`
  (pushes address client ids `1..C`); `E ≤ S_e` (as in B); `consume-ack` with
  both `user` and `clients` needs `S_u == S_c`, otherwise some identities lie
  outside one pool.
- With `N > S`, B's subscriber counts per stream change (several clients per
  user, coherent team/epochs). Coverage output is kept.

## Acks

### RFC semantics

Delivery tags are per websocket (= per client) across all streams; an ack
with `multiple = true` acks all messages up to the tag, whatever stream they
came from. The RFC batch example acks after up to 50 events of any stream;
live sync acks each event (`multiple = false`). Server side one multi-ack
becomes one upsert per affected stream with that stream's highest acked id
(tables store only `last_notification_id`). `--ack-every N` models this: one
counter per client across streams; N = 1 is the live path.

### Per-reader state (`FanInPerf.Acks`, new, pure)

```haskell
data Pending = Pending
  { stream :: Stream,
    maxId :: UUID,   -- highest delivered, not yet acked
    count :: Int     -- delivered since this stream's last ack
  }

-- | Streams with unacked deliveries; the single source of truth.
newtype AckState = AckState (Map Key Pending)

unacked :: AckState -> Int
unacked (AckState m) = sum (map (.count) (Map.elems m))

-- | One delivered notification. Once 'unacked' reaches N, returns all pending
-- entries and clears them; otherwise nothing.
deliver :: Int -> Stream -> UUID -> AckState -> ([Pending], AckState)
```

- The reader calls `deliver` for every newly delivered id of a page, in page
  order. With `--safety-window`, late ids below the cursor also count towards
  N; `maxId` stays the stream's maximum.
- Trigger is `unacked >= N` (a sum over at most `3 + E` entries), so no
  separate counter can drift from the map.
- Ack point: one `AckNotification` per returned stream (1 to `3 + E`),
  sequential, each through the reader pool and the `QSem` gate (cannon fetches
  and acks on one pool). The client's next fetch waits for them: ack latency
  slows the client as it would in reality.
- Ack failure: counted per kind, rate-limited logged; the entry is merged
  back into the state with its `maxId` but count 0 (failed acks stay pending
  with their id but start counting from zero), so the next regular ack point,
  after N further deliveries, retries (idempotent via `GREATEST`) instead of
  every following delivery. The in-memory cursor advances regardless.

### Store (`Wire.FanInNotificationsStore`)

```haskell
-- | RFC "when nothing is acknowledged": sets the client's ack of the stream
-- to the stream's latest notification. 'Nothing': empty stream, no ack row.
InitAcks :: UserId -> ClientId -> Stream -> FanInNotificationsStore m (Maybe UUID)
-- | Raises the client's ack of the stream to the id (never lowers it).
AckNotification :: UserId -> ClientId -> Stream -> UUID -> FanInNotificationsStore m ()
```

One statement each, no transaction, four statements per op via `hasql-th`
(user shown; client, team, epoch analogous):

```sql
-- InitAcks
insert into user_notification_acks (user_id, client_id, last_notification_id)
  select $1, $2, notification_id from last_user_notifications where user_id = $1
  on conflict (user_id, client_id) do update
    set last_notification_id = excluded.last_notification_id
  returning last_notification_id

-- AckNotification
insert into user_notification_acks (user_id, client_id, last_notification_id)
  values ($1, $2, $3)
  on conflict (user_id, client_id) do update
    set last_notification_id = greatest(user_notification_acks.last_notification_id, excluded.last_notification_id)
```

- Ack table keys per migration: user/client `(user_id, client_id)`, team
  `(user_id, client_id, team_id)`, epoch `(user_id, client_id, group_id,
  epoch)`. Existing primary keys serve all statements; no new indexes.
- RFC SQL fixed: explicit conflict targets; team init reads
  `last_team_notifications` (RFC: invalid `GROUP BY` on `team_notifications`);
  epoch init writes `epoch_notification_acks` with `excluded.last_notification_id`
  (RFC: nonexistent `conversation_notification_acks`, `EXCLUDED.notificationId`).

### Startup

- `consume-ack`: `InitAcks` for every reader stream, before writers start, not
  timed or counted (same reason as B's start positions). Result = start
  position; `Nothing` = nil UUID. Prints
  `consume-ack: acks reset to latest (K streams, M with notifications)`
  (M: start position not nil, i.e. InitAcks wrote a row). Leftover acks of earlier
  runs are overwritten (client reset) for non-empty streams; empty streams
  keep stale rows, so run `reset` before a measurement.
- `consume`: unchanged (`GetLastNotificationId`), `deliver` never called.

## Code sharing (`FanInPerf.Consume`)

- One reader loop; the mode is data (`ReaderMode`), not injected functions.
- Start loader branches on the mode (`GetLastNotificationId` / `InitAcks`).
- `readerPass` / `readerRound` return a record instead of triples:

  ```haskell
  data PassResult = PassResult
    { subs :: [Subscription],
      more :: Bool,                   -- some page was full
      delivered :: [(Stream, UUID)],  -- newly delivered ids, page order
      errors :: [Text]
    }
  ```

- `readerLoop` keeps an `AckState` in `Acks` mode, feeds `delivered` to
  `deliver` and runs due acks via `ackPending` (gated `runStore`); failed
  entries are merged back into the `AckState`.
- Writers, `drain`, `stopAndDrain`, ticker, error reporter: shared unchanged.
- Loss check unchanged: delivery-based (`expected` vs `delivered`).

## Stats (`FanInPerf.ConsumeStats`)

- `ReaderSnapshot` gains `acks :: Map TargetKind Int` (upserts),
  `ackErrors :: Map TargetKind Int`, `ackLatency :: IntMap Int` (histogram,
  ns, `bucketIndex`); sum/diff extended.
- `recordAck stats kind latencyNs`, `recordAckError stats kind`. Latency is
  measured around the gated upsert (pool wait included, like fetches).
- `ConsumeReport` gains `ackRate`, `maxAckRate` (ignores warmup),
  `ackErrorRate`, `ackP50`, `ackP99`; zero / `Nothing` in B. One record, no
  mode branching in `consumeTick`.

## Metrics (`experiment="consume-ack"`)

| Metric | Type |
|---|---|
| `fanin_perf_acks_total{kind}` | counter |
| `fanin_perf_ack_errors_total{kind}` | counter |
| `fanin_perf_ack_rate_current`, `fanin_perf_ack_rate_max` | gauge |
| `fanin_perf_ack_duration_seconds` | histogram |

- Registered only in `Acks` mode; B's `/metrics` output unchanged. B's
  consume and push metrics are reused with the new label. Reader pool metrics
  (`role="reader"`) include ack traffic.
- Dashboard `fan-in-perf.json`: ack/s vs consumed/s, ack latency p50/p99, ack
  errors/s.

## Terminal

```
t=42s push/s cur=… max=… | cons/s cur=… max=… | ack/s cur=… max=… p99=… | lag p50=… p99=… | backlog=… | err/s …
```

- `ack/s` segment only in `consume-ack`; `err/s` includes ack errors.
- Summary adds ack writes total, avg/max ack/s, ack latency p50/p99, ack
  errors. Expected / delivered / missing / duplicates unchanged.

## Modules

- `FanInPerf.Targets`: shared uid prefix for user and clients pools.
- `FanInPerf.Population`: types above, `readers`, `readerStreams`, rewritten
  `mkPopulation`.
- `FanInPerf.Acks` (new, pure): `Pending`, `AckState`, `unacked`, `deliver`.
- `FanInPerf.Consume`: mode-aware start loader, `PassResult`, ack points in
  `readerLoop`, `ackPending`.
- `FanInPerf.ConsumeStats`, `Metrics`, `Terminal`, `Options`: extended.
- `Wire.FanInNotificationsStore` (+ `.Postgres`): `InitAcks`,
  `AckNotification`.
- README, Grafana dashboard. B spec: drift note (population superseded).

## Testing

Unit (`fan-in-perf-tests`, hspec + QuickCheck):

- `deliver`: N = 1 acks every notification; `unacked` spans streams and
  equals deliveries since the last ack point; an ack point returns every
  pending stream with its maximum id and leaves the state empty; nothing due
  below N; a failed entry merged back keeps the higher id and its count
  (next ack point due earlier).
- Population: `(uid, cid)` unique; uids unique; `Σ k_u = N`; client ids per
  user are `1..k_u`; user stream uid = reader uid; client stream = reader
  identity; clients of one user share team and epochs; subscriber sums are
  `N` per kind (`E × N` for epoch); capacity and `S_u ≠ S_c` rejections; user
  and clients pools share their uid prefix and stay distinct.
- Options: `consume-ack` parses `--ack-every` with its bounds; `consume`
  stays `NoAcks`; `connections` rejected.
- Stats / terminal: ack fields in sum/diff/tick, max ack rate ignores warmup,
  status line and summary with and without acks.

`wire-subsystems` type-checks and its unit tests stay green. Store SQL has no
unit coverage (as in B).

### Manual acceptance (user's DB, `$DB` = connection string)

1. `fan-in-perf --db "$DB" reset`
2. `fan-in-perf --db "$DB" consume-ack --targets user:1000,clients:1000,team:10,epoch:200 --clients-per-user 2 --clients 2000 --duration 30 --ack-every 1`, then the same with `--ack-every 100`.
3. Tool: ack/s > 0, ack errors 0, `missing=0` (or a reported finding);
   `/metrics` serves `fanin_perf_acks_total`; Grafana panels show data.
4. Database (read-only `psql "$DB"` after the `--ack-every 1` run; after
   `reset` an ack row appears at a reader's first ack point, so with
   `--ack-every 1` every reader whose stream received a push has one), all
   must hold:

```sql
-- at most one ack row per reader: <= 2000 user and client acks
select count(*) from user_notification_acks;
select count(*) from client_notification_acks;
-- client ids are 1..k (k = 2 here)
select count(*) from client_notification_acks where client_id not between 1 and 2;  -- 0
-- identity coherence: every client ack has the matching user ack
select count(*) from client_notification_acks c
  left join user_notification_acks u using (user_id, client_id)
  where u.user_id is null;                                   -- 0
-- one team per user (all its clients)
select count(*) from (select user_id from team_notification_acks
  group by 1 having count(distinct team_id) > 1) x;          -- 0
-- clients of a user ack only the user's epochs
select count(*) from (select user_id from epoch_notification_acks
  group by user_id having count(distinct (group_id, epoch)) > 3) x;  -- 0 (E = 3)
-- no ack beyond the stream's latest notification
select count(*) from user_notification_acks a
  join last_user_notifications l using (user_id)
  where a.last_notification_id > l.notification_id;          -- 0
```

## Security

- No new secrets; the connection string stays a CLI argument and is never
  printed or written to docs.
- All SQL via parameterised `hasql-th` statements, no string building.
- `--ack-every` bounded like the other flags.
- Errors are logged as kind and error message, never payloads or ids.
- Acceptance queries are read-only.
