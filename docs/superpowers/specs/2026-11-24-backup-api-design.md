# Backup API: implementation design

Source spec: `2026-11-16 RFC Backup API(1).md` (repo root, Confluence export).
This document records how the RFC gets built and where implementation
deviates from or adds to the RFC. Everything the RFC states and this document
doesn't contradict holds as written (routes, payloads, tables, SQL, semantics).

## Intent

Some customers require client backups to be stored server-side instead of
locally. Clients upload encrypted full + incremental backup files per epoch;
wire-server stores them in S3, tracks them in Postgres, and deletes them
reliably (incl. all S3 versions, GDPR) on request, on client removal and on
user/team deletion.

## Goals / non-goals / anti-goals

- Goals: everything in the RFC's Proposal section, for team users only.
- Non-goals: anything beyond the Secure Automatic Backups RFC; dedicated
  backup bucket; backups for users without a team.
- Anti-goals (failure even if it "works"):
  - S3 objects that are never deleted (leaks, GDPR breach).
  - A removed client or another user reading/creating backups.
  - Uploads buffered in memory.
  - An upload racing the orphan cleanup into a DB row pointing to a deleted object.

## Decisions (made with the requester)

| Topic | Decision |
| --- | --- |
| API version | Routes `From 'V20` (V19 is finalized; V20 is the dev version). |
| File ids | UUID **v4** (`Data.UUID.V4`), not v7. Nothing orders by id; `created_at` orders incrementals. |
| S3 ownership | Cargohold is the only service talking to S3. Background-worker only triggers cleanup via a cargohold internal endpoint. |
| Code structure | Approach 1: Polysemy effects + subsystem in `wire-subsystems`; cargohold stays shallow. |
| Users without team | No backend-hosted backups: 403. |
| Delivery | One branch, tracer-bullet style: vertical slices, each a reviewable end-to-end improvement with its own tests (see Commit sequence). Specs/plans may be split per slice. |
| `enforcedSchedule` | Optional in the feature config. |
| Server-side deletion | Brig orphans the files itself in Postgres (`OrphanClient`, same DB, one tx per client); cargohold's cleanup cron deletes the S3 objects. No RabbitMQ job, no internal delete endpoints. Cron every 5 min. |
| Cargohold Postgres | Mandatory config for every deployment (same DB brig/galley use). |

## Approaches considered

1. **Polysemy in wire-subsystems (chosen).** `BackupStore` (Postgres),
   `BackupObjectStore` (S3), `BackupSubsystem` (rules). Cargohold interprets
   the S3 effect with its existing S3 code. Unit-testable rules; follows the
   AGENTS.md direction. Cost: cargohold gains polysemy / wire-subsystems /
   hasql deps.
2. **Plain cargohold** (hasql + logic in `AppT`, cleanup timer thread with
   advisory lock). Smallest diff, but against repo direction, logic only
   integration-testable, timer in every replica. Rejected.
3. **Hybrid** (only `BackupStore` in wire-subsystems). Dependency cost
   without testability gain. Rejected.

## RFC deviations and additions

- `/v20/backups/...` instead of `/v19/...`.
- File ids are UUID v4.
- "Deleted from S3 immediately" = inline best-effort S3 deletion right after
  the orphaning transaction commits; the cron-triggered cleanup is the safety
  net (see Cleanup).
- Concurrency uses a per-client advisory lock instead of `SELECT ... FOR UPDATE`
  (see Concurrency). The RFC's `FOR UPDATE` cannot cover creating a new epoch
  or counting files, as there is no row to lock yet.
- Server-side deletion: no internal cargohold delete endpoints. Brig orphans
  the files directly in Postgres; S3 objects are deleted by the next cleanup
  run (≤ 5 min), not "right away" (see brig).
- Team deletion needs no galley change: galley's team finalization deletes
  every member through brig, which orphans the member's backups.
- Downloads are always S3-presigned, never CloudFront (CloudFront may be scoped
  to the assets origin).
- `enforcedSchedule` is optional (RFC: default `weekly`).
- Additional error labels, see Errors.

## Architecture

### wire-api

- `Wire.API.Backup` (new): types from the RFC's Types table (`BackupEpoch`
  as `Word32` newtype rejecting 0, `BackupFileId` newtype over UUID,
  `BackupFile`, `BackupFiles`, `EpochSummary`, `ClientBackups`, list
  response). JSON schemas via `schema-profunctor`.
- `Wire.API.Routes.Public.Cargohold`: new `BackupAPI` (all routes
  `From 'V20`, `ZLocalUser :> ZClient`). Uploads use a new raw body
  combinator: `StreamBody' NoFraming OctetStream` conduit plus a required
  `Content-Length` header (`Header' '[Required, Strict] "Content-Length" Word64`).
  Downloads reuse the `AssetRedirect`-style 302 `MultiVerb`.
- `Wire.API.Routes.Internal.Cargohold`:
  - `POST /i/backups/cleanup` (200 `{ "deleted": n }`, processes at most one batch)
- `Wire.API.Team.Feature`: `AutomaticBackupsConfig` (barbies style, template:
  TOTP commit `90afdb712`; enum template: `EnforceFileDownloadLocationConfigB`).
  `FeatureSymbol = "automaticBackups"`. Config `enforcedSchedule`
  (`daily|weekly|monthly`, **optional**: omitted / absent = no enforced
  schedule; default absent; deviates from the RFC default `weekly`), `enforced` (default `false`),
  `serverOnly` (default `false`). Default status `disabled`, lock status
  `unlocked` (RFC); helm default `disabled` + `locked` (RFC Rollout).

### wire-subsystems

- Migration `postgres-migrations/<ts>-backups.sql`: the three tables from the
  RFC, `client_id` stored as `text` like other PG tables storing `ClientId`
  (builder: confirm existing representation and follow it). Index on
  `backup_orphans (orphaned_at)`, index on `backup_incrementals (full_id)`.
  `backup_orphans` gets a generated column (RFC addition) making the orphan
  kind explicit without redundancy:
  `kind text GENERATED ALWAYS AS (CASE WHEN file_created_at IS NULL THEN 'upload' ELSE 'deletion' END) STORED`.
- `Wire.BackupStore` effect + `Wire.BackupStore.Postgres` interpreter
  (hasql-th, `runTransaction`). All transactional logic from the RFC lives
  here as whole-transaction operations, e.g. `reserveUpload`,
  `commitFull`, `commitIncremental`, `orphanClient`, `orphanUser`,
  `listClients`, `listEpochFiles`, `lookupFile`, `selectCleanupBatch`,
  `markDeletionTried`, `deleteOrphans`. Each mutating operation is one
  transaction.
- `Wire.BackupObjectStore` effect (S3 abstraction):
  - `putObject :: BackupFileId -> Word64 -> ConduitT () ByteString (ResourceT IO) () -> m ()`
  - `presignGet :: BackupFileId -> m URI` (host selection for multi-ingress
    from `ZHostOpt` on the download routes, like the asset routes)
  - `deleteObjects :: [BackupFileId] -> m [BackupFileId]` returns the ids
    whose objects (all versions) are gone; implements the RFC's versioned /
    unversioned / `NotImplemented` logic.
- `Wire.BackupSubsystem` effect + interpreter: business rules (checks,
  epoch rules, limits, orchestrating reservation → upload → commit, inline
  deletion). Uses `BackupStore`, `BackupObjectStore`, `BrigAPIAccess`
  (`LookupClients`), `GalleyAPIAccess` (`GetTeamId`, `GetFeatureConfigForTeam`),
  `Now`, `Random`/UUID generation, `Input BackupConfig`, `Error BackupError`.
- In-memory interpreters for `BackupStore` and `BackupObjectStore` for unit
  tests (existing pattern: `wire-subsystems` mock interpreters).

### cargohold

- Deps: `polysemy`, `wire-subsystems`, `hasql-pool` (+ `make regen-local-nix-derivations`).
- Options: `postgresql`, `postgresqlPassword`, `postgresqlPool` (same shape as
  brig), `galley :: Endpoint`, and `backups`:

  | Option | Default (builder's call, adjustable) |
  | --- | --- |
  | `maxFullBytes` | 1 GiB |
  | `maxIncrementalBytes` | 100 MiB |
  | `maxFilesPerEpoch` | 1000 (includes the full) |
  | `uploadTimeout` | 45 min |
  | `orphanGracePeriod` | 60 min; startup fails unless `> uploadTimeout` |
  | `cleanupBatchSize` | 100 (a versioned batch must finish within background-worker's 30 s HTTP timeout) |

- `Env`: hasql pool; galley endpoint.
- `CargoHold.Backup` (new): `BackupObjectStore` interpreter on top of
  `CargoHold.S3`/`CargoHold.AWS`. Keys `backups/<uuid>` in the existing bucket.
  `putObject` reuses the `S3.uploadV3` pipeline (`isolate` / `countC` /
  `streamUpload`, `incompleteBody` on mismatch), run with `runResourceT` in
  `Embed IO`. New S3 ops: `GetBucketVersioning`, `ListObjectVersions`,
  `DeleteObjects` (≤ 1000 keys/versions per call; listings
  in parallel, bounded).
- `runBackupEffects :: Sem BackupEffects a -> Handler a` interprets the
  whole stack in `Embed IO` and maps `BackupError` to cargohold's Wai
  `Error` (see Errors). Handlers are one-liners calling the subsystem.
- No migrations run by cargohold (brig's `migrate-postgres` / galley run
  them). Cargohold may start before the tables exist; backup requests then
  fail with 500 until migrations ran. No startup check.
- Helm: cargohold configmap gets `postgresql*`, `galley`, `backups`; secret
  mount for the pg password like galley. Integration config:
  `cargohold.integration.yaml`, plus cargohold dbname override in
  `integration/test/Testlib/ModService.hs`.
- nginz: `/backups` location for cargohold (versioned paths), with
  `disable_request_buffering: true` and `max_body_size: "0"` (cargohold
  enforces limits), in `charts/nginz/values.yaml`,
  `services/nginz/integration-test/conf/nginz/nginx.conf` and the
  `deploy/dockerephemeral/federation-v*` nginz confs.

### background-worker

- Own Options (not galley's Opts, which bg-worker also parses):
  `cargohold :: Endpoint` and `backupsCleanup { schedule :: CronSchedule }`
  (helm default `"*/5 * * * *"`, builder's call).
- Arbiter cron job `backups-cleanup` (template: `meetings-cleanup` in
  `Workers.hs`, `SkipOverlap`) calling `POST /i/backups/cleanup` repeatedly
  while it returns `deleted > 0` and a per-run cap isn't hit (cap: builder's call).

### brig

Brig never calls cargohold for backup deletion. `backup_orphans` is a durable
deletion queue in the database brig and cargohold share (helm
`dbname: wire-server`): brig orphans the files itself via the `BackupStore`
Postgres interpreter (brig's interpreter stack already has the hasql pool);
cargohold's cleanup cron deletes the S3 objects. S3 stays cargohold-only.

- Client removal: `OrphanClient uid cid` in `ClientSubsystem` `execDelete`.
  Covers public client deletion and eviction in `addClient`.
- User deletion: at the start of `deleteAccount` (`Brig/API/User.hs`), before
  any state changes, `OrphanClient` once per client having backups
  (`ListBackupClients`), one transaction each. A failure leaves nothing
  half-deleted and the caller can retry. Covers self-delete, SCIM pending-user
  delete, `DELETE /i/users/:uid` and therefore team deletion (galley deletes
  every member through it), plus the `DeleteUser` internal event path.
  Bots (`deleteBot`) aren't covered; they have no team, so they never have
  backups.
- Why not a RabbitMQ job: background jobs are dropped after 3 attempts (no
  dead-letter queue), so a deletion could be lost; the orphans table already
  is durable and retried by the cron.
- Already issued download URLs stay valid until `downloadLinkTTL`; a removed
  client can't obtain new ones (client-exists check).

## Request flow

### Checks (every public route, in this order)

1. Write routes and `DELETE`: path `<ClientId>` == `Z-Client`, else 403
   `backup-forbidden`.
2. Calling client (`Z-Client`) still exists (`LookupClients [uid]`), else
   403 `backup-forbidden`.
3. All routes except `DELETE`: user has a team (`GetTeamId`), and team
   feature `automaticBackups` is `enabled` (`GetFeatureConfigForTeam`), else
   403 `backup-disabled`.

Reads (list, epoch files, downloads) may target any client of the caller
(restore on a new device). They never cross users: every query is keyed by
the caller's user id.

### Upload (full and incremental)

1. Checks. `Content-Length` > limit → 413 `backup-too-large`;
   `Content-Length` must be ≥ 1 (400 `invalid-length`, reuse existing label).
2. Pre-check (cheap, non-authoritative): epoch accepted (`max`/`max+1`/`1`),
   full id current (incremental), file count < `maxFilesPerEpoch`. Avoids
   wasted uploads.
3. `reserveUpload`: insert orphan row (`file_created_at NULL`).
4. Stream to S3 under `timeout uploadTimeout`; on timeout / incomplete body
   the request fails and the reservation stays for cleanup.
5. If now > request start + `uploadTimeout`: fail (keeps the invariant
   "commit happens before the grace period ends").
6. Commit transaction (authoritative re-check of all rules under the lock);
   claiming the reservation must delete exactly 1 row, else roll back and
   fail 500 (cleanup already took it). Full commit of `max + 1` orphans
   epochs `<= epoch - 2`. Replacing a full orphans the old full and its
   incrementals.
7. After commit: inline best-effort deletion of the orphans this commit
   created (see Cleanup).

### Delete client backups

One transaction orphans everything of `(user, client)`, then inline
best-effort deletion (`deleteOrphansNow`; S3 errors are logged, rows stay
for the cron; Postgres errors propagate, as Postgres is assumed reliable).

### Download

Look up file under `(user, client, epoch, full)` (and incremental id), 404
`backup-not-found` otherwise, then 302 to `presignGet` (TTL
`downloadLinkTTL`, existing option).

## Concurrency

Every mutating transaction (commit full / incremental, orphan client)
starts with `SELECT pg_advisory_xact_lock(<key>)` where the key is derived
from `(user_id, client_id)`; exactly one lock per transaction. Per-user
deletion loops over the user's clients, one transaction per client (no
multi-lock ordering, no deadlocks; idempotent, so retries resume). Isolation `ReadCommitted`. This
serializes all writers of one client, covering new-epoch creation, stale
epoch removal and the file count, which `FOR UPDATE` cannot. Reads take no
lock.

## Cleanup

- Eligible orphans: `kind = 'deletion' OR orphaned_at < now() - orphanGracePeriod`.
- `POST /i/backups/cleanup`: select up to `cleanupBatchSize` eligible
  orphans, `ORDER BY deletion_last_tried NULLS FIRST, orphaned_at`, with
  `FOR UPDATE SKIP LOCKED` in a short transaction that also sets
  `deletion_last_tried = now()`. Then `deleteObjects`; delete orphan rows
  for the ids reported gone. Missing objects count as gone.
- Versioning: `GetBucketVersioning` per run (per cleanup call / inline
  deletion): empty status → `DeleteObjects` with keys only; otherwise or on
  error → `ListObjectVersions` per file (prefix `backups/<id>`), then
  `DeleteObjects` over all versions + delete markers. `NotImplemented` from
  `ListObjectVersions` → plain exact keys `backups/<id>` (keys are exact, no
  listing needed; RFC deviation), remaining files of that call skip listing.
- Inline deletion after a commit uses the same code path on the just-created
  orphan ids. Failures only log; rows stay for the cron.
- Double deletion (inline and cron at the same time) is harmless: S3 deletes
  and row deletes are idempotent.

## Errors

| Situation | Status | Label |
| --- | --- | --- |
| Path client ≠ `Z-Client` on write/delete; calling client removed | 403 | `backup-forbidden` |
| No team or feature disabled | 403 | `backup-disabled` |
| Epoch not `max`/`max+1`/`1` | 409 | `backup-epoch-invalid` |
| Full id not current | 409 | `backup-full-missing` |
| `maxFilesPerEpoch` reached | 409 | `backup-epoch-full` |
| Body larger than limit | 413 | `backup-too-large` |
| `Content-Length` missing / malformed | 400 | (servant default) |
| `Content-Length` 0 | 400 | `invalid-length` |
| Body shorter than `Content-Length` | 400 | `incomplete-body` |
| File not found | 404 | `backup-not-found` |
| Epoch 0 in path | 400 | (path parser) |

Messages generic; no internal ids or S3 details in error bodies.

## Commit sequence: tracer-bullet vertical slices (one branch)

Work and commit in vertical slices: each commit is a meaningful, end-to-end
improvement cutting through all layers it needs (wire-api → store →
subsystem → cargohold/brig/background-worker → config → tests), small
enough for a human to review in one sitting. No horizontal "all types" or
"all store functions" commits. Each slice brings its own unit tests and
integration tests, adds only the migration/columns/options/S3 ops it uses,
type-checks without warnings, has green unit tests of changed packages, is
formatted, and keeps Cabal/Nix deps aligned.

Intermediate states are safe to merge because the feature flag defaults to
`disabled` + `locked` in helm; e.g. orphans created before the cleanup
slice just wait for it.

1. **Feature flag `automaticBackups`.** wire-api config, galley get/put,
   stern, helm, docs, changelog, integration test.
2. **Tracer bullet: full backup round trip.** Migration with
   `backup_epochs` + `backup_orphans`; `BackupStore` (reserve, commit full,
   list epoch files, lookup), `BackupObjectStore` (put, presign),
   `BackupSubsystem` with checks (ownership, client exists, team + flag),
   epoch acceptance, full replacement and stale-epoch removal (both only
   create orphans); cargohold deps/options/pool/galley endpoint/helm/
   integration config/nginz; routes `POST .../full`,
   `GET .../epochs/<e>`, `GET .../full/<id>`; size limit, upload timeout.
3. **Incrementals.** `backup_incrementals` table, `POST .../incremental`,
   `GET .../incremental/<id>`, `maxFilesPerEpoch`, replacement/stale
   removal now also orphan incrementals.
4. **Listings.** `GET /backups`, `GET /backups/clients/<cid>`.
5. **Client deletes own backups.** `DELETE /backups/clients/<cid>` plus
   inline S3 deletion of orphans (unversioned path: `DeleteObjects` by key);
   inline deletion also applied after replacements/stale-epoch removal.
6. **Cleanup.** `POST /i/backups/cleanup` (expired reservations, failed
   deletions) + background-worker options and cron job.
7. **Versioned buckets.** `GetBucketVersioning`, `ListObjectVersions`,
   version-aware `DeleteObjects`, `NotImplemented` fallback (unit-tested).
8. **Server-side deletion.** `ListBackupClients`; brig orphans backups in
   `execDelete` (client removal) and at the start of `deleteAccount` (user
   deletion, transitively team deletion).

## Testing

- Unit (`wire-subsystems`, in-memory interpreters): epoch acceptance table
  from the RFC, stale epoch removal, full replacement orphaning old full +
  incrementals, incremental against stale full, `maxFilesPerEpoch`, all
  403 cases, size limit, reservation-claim failure, versioned /
  unversioned / `NotImplemented` deletion paths.
- Integration (`integration/`): happy paths for all routes, 302 + download
  via presigned URL (`noRedirect`), security cases (other user, other
  client write, removed client, team-less user, flag disabled, oversize,
  short body), client removal / user deletion / team deletion eventually remove
  backups, cleanup endpoint deletes eligible orphans and leaves fresh
  reservations. Local garage is unversioned (`GetBucketVersioning` stub
  returns "not enabled"; `ListObjectVersions` returns 501), so versioned
  paths are covered by unit tests only.
- Not run by the agent: integration tests (per AGENTS.md).

## Left to the builder

- Module names and exact effect operation signatures.
- Cron schedule, per-run cap, option defaults above.
- Representation of `ClientId` in Postgres (follow existing tables).
