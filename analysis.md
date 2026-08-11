# backend-notification-pusher stuck offline after RabbitMQ outage

## Symptom

After a short RabbitMQ outage, `backend-notification-pusher` stayed offline even
after RabbitMQ came back and was being used successfully by other components in
the same pod. No further connection attempts were logged for this worker.

## Root cause

`libs/extended/src/Network/AMQP/Extended.hs`, `openChan` (lines 236-242):

```haskell
openChan conn = do
  Log.info l $ Log.msg (Log.val "Opening channel with RabbitMQ")
  chan <- liftIO $ Q.openChannel conn
  liftBaseWith $ \runInIO ->
    Q.addChannelExceptionHandler chan (void . runInIO . chanExceptionHandler conn)
  Log.info l $ Log.msg (Log.val "RabbitMQ channel opened")
  hooks.onNewChannel chan
```

`hooks.onNewChannel chan` is called with no exception guard. This is
inconsistent with the other two hooks in `RabbitMqHooks`, which are both
wrapped in `` `catch` logException ``:

- `onConnectionClose` — guarded
- `onChannelException` — guarded
- `onNewChannel` — **not guarded**

Any exception escaping `onNewChannel` propagates out of `openChan`, out of
`connectWithRetries`'s `bracket`, and out of `openConnectionWithRetries`
entirely. All three call sites invoke `openConnectionWithRetries` via a
fire-and-forget, unlinked async:

```haskell
void $ async $ liftIO $ openConnectionWithRetries ...
```

(`services/background-worker/src/Wire/BackendNotificationPusher.hs:371-372`,
same pattern in `DeadUserNotificationWatcher.hs` and `Jobs/Consumer.hs`).

Since the async is never `link`ed or `wait`ed on, GHC silently discards any
exception thrown inside it. The thread just dies. No log line, no crash, no
pod restart — the container stays up (only this one thread died), so
Kubernetes never notices anything is wrong.

This directly contradicts the intent stated in the comment right above the
call site (`BackendNotificationPusher.hs:266-268`):

> If this throws an exception on the Chan / in the forever loop, the
> exception will bubble all the way up and kill the pod. Kubernetes should
> restart the pod automatically.

That's the intended design; the unlinked `async` breaks it.

### What actually threw

For `BackendNotificationPusher`, `onNewChannel` calls `startPusher`, which
calls `getRemoteDomains` (admin HTTP API on port 15672). That call has its own
retry policy capped at 60s cumulative delay
(`BackendNotificationPusher.hs:320`, `limitRetriesByCumulativeDelay
60_000_000`), unlike the AMQP connection retry itself which retries forever.
If the admin API is still unreachable after that cap, `getRemoteDomains`
gives up and throws; `startPusher`'s own cleanup handler
(`BackendNotificationPusher.hs:263`, `throwM e`) rethrows it further —
straight through the unguarded `onNewChannel` hook. (Possibly compounded by
an unguarded `Q.cancelConsumer chan` call in that same cleanup path throwing
on an already-dead channel — unconfirmed which exception fired first; not
relevant to the fix.)

### Confirmed from the log

Sorted the log chronologically by timestamp (not by array order, which is
newest-first and easy to misread):

- The `backend-notification-pusher`-tagged connection's last-ever log line is
  at `11:52:28.065` — "Failed to connect to RabbitMQ" (retry 18,
  `willRetry:true`) and "Exception occurred while refreshing domains" (retry
  12, `willRetry:true`), same timestamp. Nothing tagged logs again, ever.
- A second, separate AMQP connection in the same pod — untagged, one of the
  publisher-side connections created via `mkRabbitMqChannelMVar` in
  `Env.hs:168` / `Env.hs:171` (`"background-worker-jobs-publisher"` /
  `"background-worker-backend-notifications"`, both using the raw unnamed
  logger, not the named clone `BackendNotificationPusher` uses) — keeps
  retrying and succeeds at `11:52:44.562` ("RabbitMQ channel opened").

This matches the observed symptom exactly: "RabbitMQ was back and used by
other entities" = the publisher connections recovered fine; the consumer
connection driving actual notification delivery died silently and never came
back.

### Correction of an earlier (wrong) hypothesis

Initially assumed `Q.closeConnection` (called by `bracket`'s cleanup on
exception) does *not* trigger `addConnectionClosedHandler`, so no reconnect
would even be attempted. Checked the `amqp-0.24.0` package source directly
(`Network/AMQP/Internal.hs:452`, and the `forkFinally'` finalizer at lines
291-314): the closed-handler fires on **both** an explicit `closeConnection`
call and an abnormal exception-close. A reconnect attempt was in fact made
(visible in the log as the tagged retry-18 sequence) — it also died, just not
for that reason.

## Confirming evidence from an earlier log window (11:51:35 - 11:53:53)

A second log export, covering the actual onset of the outage, shows all four
RabbitMQ connections in the pod dropping together at `11:51:35-36`:
`background-job-consumer`, `dead-user-notification-watcher`, two unnamed
publisher connections, and `backend-notification-pusher`. This wasn't an
isolated failure — it's one shared network outage hitting every connection at
once.

Two contrasting outcomes in this window:

- `background-job-consumer` and `dead-user-notification-watcher` each stall
  for **~2m13s** (last attempt logged ~`11:51:36-37`, next at `11:53:50`) —
  far longer than the 5s-capped backoff between logged attempts elsewhere.
  Consistent with a blocked `connect()` syscall (packets silently dropped
  rather than actively refused; close to Linux's ~127s default TCP connect
  timeout), not application-level backoff. Both resolve within 1-3s of
  unblocking and succeed.
- `backend-notification-pusher`'s tagged sequence dies at **exactly
  `11:52:08`** — "Failed to connect to RabbitMQ" (retry 18) and "Exception
  occurred while refreshing domains" (retry 12) both fire at the same
  timestamp, then nothing, ever, in this or the later window.

Read `DeadUserNotificationWatcher.hs:121-127` for contrast: its `onNewChannel`
just calls `startConsumer chan` then `forever $ threadDelay maxBound` —
nothing in that path can throw. That's exactly why it self-heals through the
same outage while `BackendNotificationPusher`'s `onNewChannel` (which calls
`startPusher` → `getRemoteDomains`, an HTTP call with its own throwing 60s-cap
retry) does not. Same library, same unguarded hook, but only the pusher's
hook does something exception-prone — strong contrastive confirmation of the
root cause.

## Metric confirmation

`wire_background_worker_running_workers{worker="backend-notification-pusher"}`
time series (15-min resolution, `2026-07-19` through `2026-08-05`) has
exactly one transition in the whole 18-day export:

```
2026-07-23 11:45:00 -> 1
2026-07-23 12:00:00 -> 0
```

That 15-minute bucket straddles the `11:52:08` death confirmed above. The
gauge then reads `0` for every single sample through the end of the export
(`2026-08-05 15:00`) — it never recovered on its own, matching the reported
symptom exactly. (An earlier report of the drop happening "around 13:40" was
just the Berlin-local display of this same UTC `11:45-12:00` bucket — no
second incident.)

This also confirms the death goes through `startPusher`'s `cleanup` handler
(`BackendNotificationPusher.hs:263`, `throwM e`) rather than through the
guarded `onChannelException`/`onConnectionClose` hooks: if either of those
had fired, they'd have called `markAsNotWorking` explicitly, but the gauge
transition happens without a matching "RabbitMQ channel closed" /
"RabbitMQ connection is closed" log line from `Extended.hs` — the failure
truly bypasses that machinery entirely, exactly as the unguarded-`onNewChannel`
theory predicts.

## Reproduction

No need to reconstruct exact production network timing. The defect is
directly and deterministically reproducible in isolation:

```haskell
openConnectionWithRetries logger amqpEndpoint (Just "test") RabbitMqHooks
  { onNewChannel = \_ -> throwIO (userError "boom")
  , onConnectionClose = pure ()
  , onChannelException = \_ -> pure ()
  }
```

Run against a local RabbitMQ (`docker run rabbitmq`), invoke this inside an
`async` without `link`/`wait`ing on it. Observed result: one successful
connect, one "channel opened" log line, then total silence forever — no
crash, no further retries.

(The production trigger is a network partition long enough for
`getRemoteDomains`'s 60s cap — or some other exception on the
`onNewChannel` path — to fire while a reconnect is in flight. Not needed to
prove or fix the underlying defect.)

## Fix

Three options, not mutually exclusive. Recommended combination: A + B.
C is optional/complementary, addresses only the specific trigger, not the
general defect.

### Option A (primary) — guard `onNewChannel` in `Extended.hs`

Same file, same pattern already used for the other two hooks:

```haskell
hooks.onNewChannel chan `catch` \e -> do
  logException l "onNewChannel hook threw an exception" e
  openChan conn
```

Single change in the shared library; fixes all three current callers
(`BackendNotificationPusher`, `DeadUserNotificationWatcher`,
`Jobs/Consumer`) at once, since they all go through this one function.
Brings `onNewChannel` in line with the documented hook contract ("any
exceptions thrown by this would be logged and ignored" — currently only
true for the other two hooks).

Risk to consider before implementing: `openChan conn` retries immediately,
with no backoff of its own. If the same hook keeps throwing fast (unlike our
case, where the trigger is `getRemoteDomains`'s 60s-capped retry, which
naturally paces each retry cycle), this could busy-loop hammering
`Q.openChannel`/the hook body. Worth either reusing the existing
`connectWithRetries` backoff policy here, or capping recursion depth /
adding a short delay before the retry, so a fast, persistently-throwing hook
can't spin.

### Option B (secondary, defense in depth) — link the async

At each call site (`BackendNotificationPusher.hs:371-372`,
`DeadUserNotificationWatcher.hs:119`, `Jobs/Consumer.hs:74`):

```haskell
a <- async $ liftIO $ openConnectionWithRetries ...
liftIO $ link a
```

Restores the contract documented in the comment at
`BackendNotificationPusher.hs:266-268` ("exception will bubble all the way
up and kill the pod; Kubernetes should restart it automatically") for
whatever still escapes Option A — e.g. a genuinely new/unanticipated
exception type, or a bug in Option A's own retry logic.

Trade-off to weigh: this makes a pod-wide crash-and-restart the fallback
for *any* uncaught exception from *any* of the three workers' hooks, not
just the one that failed. A transient hook hiccup in, say,
`Jobs/Consumer` would now restart the whole `background-worker` pod,
taking `BackendNotificationPusher` and `DeadUserNotificationWatcher` down
with it too, briefly. Given Option A should catch the common case, this
should only fire rarely — but worth deciding if that blast radius is
acceptable, or if each worker's async should be linked/supervised
independently instead of accepting a whole-pod restart either way.

### Option C (optional, addresses only the specific trigger)

`getRemoteDomains`'s retry policy (`BackendNotificationPusher.hs:320`,
`limitRetriesByCumulativeDelay 60_000_000`) is what actually threw in this
incident. Could remove the cap and retry indefinitely, matching the AMQP
connection layer's own policy — the admin API and the AMQP broker are the
same service, so if one is unreachable long enough to matter, so is the
other, and the AMQP layer already retries forever.

Note this doesn't fix the general defect — any other exception on the
`onNewChannel` path (or a future change to this or another hook) would
still silently kill the whole reconnect loop without Option A. Treat as a
complementary hardening, not a substitute.
