# Automatic backups

Clients can upload encrypted backup files to cargohold. Cargohold stores them
in S3 (the asset bucket) and nowhere else: no database. Gated by the team
feature `automaticBackups`.

## Requirements

- Optional secret `backupHmacKey` (see `config-options.md`). Without it backups
  are not configured: all public backup routes (upload and read) return
  403 `backup-feature-disabled`. The internal deletes succeed as no-ops, so
  client and user deletion in brig keep working.
- The S3 store must give strong list-after-write consistency (see
  [Store requirements](#store-requirements)).

## API (public, via nginz `/backups`)

Available from API version V19 (prefix `/v19/backups`).

- `GET /backups` list the restorable backups of all clients of the user (per client
  its epochs, per epoch a summary: newest `createdAt`, `fileCount`, total `size`), e.g.
  for a new client that restores. The summary stays small however many files exist;
  the files of the chosen epoch come from `GET /{epoch}` below.

All other paths are under `/backups/clients/{cid}/epochs`:

- `GET /` (client) the same summary for one client.
- `POST /{epoch}/full` upload the full file of an epoch (raw body, `Content-Length`
  required). Replaces the previous full of the epoch.
- `POST /{epoch}/full/{fullId}/incremental` upload an incremental file under the full file
  `fullId` (the id from the full upload response), which must be the epoch's newest full.
- `GET /{epoch}` list the current files of an epoch: `{"full": ..., "incrementals": [...]}`.
- `GET /{epoch}/full/{fullId}` and `GET /{epoch}/full/{fullId}/incremental/{id}` download:
  302 redirect to a presigned S3 URL.

- `DELETE /backups/clients/{cid}` delete all backups of the calling client (`cid` must be
  `Z-Client`, else 403). The team feature is not checked. Succeeds if nothing exists. Uploads
  running at that time are rejected (403) and removed, see
  [Deletion / upload ordering invariant](#deletion--upload-ordering-invariant). The client should
  stop scheduling uploads before the call.

Backups are also deleted on client and user deletion (brig, internal). After storing a full file
the server deletes all files of all but the two newest epochs of the client.

`fullId` and `id` come from the upload responses and the list response.

Limits (`cargohold.settings.backupLimits`): a full file up to `maxFullBytes`,
incremental files up to `maxIncrementalBytes`, at most `maxFilesPerEpoch` files per
epoch.

### Newest full wins

A stored object is complete (S3 shows it only when its upload finished), so there is
no lock or readiness flag. The current full of an epoch is the one with the greatest
id (UUIDv7, i.e. creation order). An incremental is stored under the full it extends
and is current only while that full is the current one. The client learns a full's id
from the response of the finished upload, so an incremental can only extend a finished
full, and an incremental of a replaced full is never listed, whatever the timing of the
uploads (ids are assigned at upload start, so ordering incrementals by id alone would
not be safe).

After a full is stored the server deletes everything else in the epoch, so a new full
replaces the old one and its incrementals. Every upload deletes what the current full
superseded, which also removes an incremental that was stored just after a newer full
(it raced the new full). Concurrent fulls prune each other, a late older one deletes
itself.

A failed or aborted upload (any error, client disconnect) removes the file it created.
Incremental uploads need the current full (`409 backup-full-missing` otherwise).

### Why list + per-file redirect

Presigned URLs are bearer links with a short TTL. Listing returns id,
size and createdAt (no URLs); each download asks for its own redirect, so URLs are minted when needed
and expire quickly. Nothing is pre-signed in bulk.

## Storage layout

One S3 object per uploaded file, in the asset bucket:

```
backup/<hmac(uid)>/<hmac(cid)>/<epoch>/<fullId>/full
backup/<hmac(uid)>/<hmac(cid)>/<epoch>/<fullId>/incremental/<id>
```

- `hmac(x)`: lowercase hex HMAC-SHA256 of `x` with `backupHmacKey` (64 chars).
  Plain user or client ids never appear in S3: keys show up in presigned URLs
  and logs.
- `backup/<hmac(uid)>/<hmac(cid)>/.tombstones/<id>`: empty object written by the client's own
  backup deletion, see [Deletion / upload ordering invariant](#deletion--upload-ordering-invariant).
  Not an epoch, listings ignore it.
- `epoch`: decimal, `fullId`, `id`: UUIDv7 (creation order). An incremental key
  names the full file it extends.
- `backup/` is outside the `v3/<retention>/` prefixes, so asset lifecycle
  rules do not expire backups.

Presigned URLs reveal `hmac(uid)` and `hmac(cid)`: they link downloads of the
same device, nothing more.

### Why path-only metadata

All metadata is encoded in the key. `ListObjectsV2` filters by key prefix and
returns key, size and `LastModified`, but never user metadata or tags.

| API field | Source |
|---|---|
| epochs | `CommonPrefixes` of the device prefix (delimiter `/`) |
| `id`, `fullId`, kind | key segments |
| `size` | listing `Size` |
| `createdAt` | UUIDv7 timestamp |

`x-amz-meta-*` is not used: it is immutable, needs a HEAD per object, and is
returned as response headers to the holder of a presigned URL. Object tags are
not used: one extra call per object and uneven support in S3-compatible stores.
Keys that do not parse (non-decimal epoch, unknown kind, invalid UUID) are
ignored on list and not addressable.

HMAC is one-way: every operation starts from a known user and client id.
Changing or losing `backupHmacKey` makes all existing backups unreachable and
undeletable via the API. Never rotate it.

### Store requirements

The store must provide strong list-after-write consistency: a lagging list
would leave orphans on client or user deletion. It must also support
`ListObjectsV2` with delimiter `/`, `ListObjectVersions`, `DeleteObjects` and
presigned GET.

| Store | List after write |
|---|---|
| AWS S3 | strong |
| Cloudflare R2 | strong |
| Ceph RGW | strong per docs (multisite lag unclear) |
| Garage | strong (quorum); gap while the cluster layout changes |
| MinIO | strong per vendor |
| Wasabi | strong per docs |
| SeaweedFS, Backblaze B2 | unknown |

### Versioned buckets

Bucket versioning may be on or off. Deletes list all versions under the prefix
(`ListObjectVersions`, including delete markers) and `DeleteObjects` each
`VersionId` explicitly, in batches of at most 1000. On a versioned bucket a
plain delete only adds a delete marker and the encrypted data would stay
restorable, which violates "backups of a removed device MUST be deleted
immediately". Unversioned buckets report version id `null`; same code path. If
the store answers `NotImplemented` for `ListObjectVersions`, cargohold falls
back to `ListObjectsV2` plus plain delete.

## Deletion / upload ordering invariant

Client removal never touches cargohold directly: it only enqueues the client
deletion and deletes the client row (`execDelete` in
`Wire.ClientSubsystem.Interpreter`), so an unavailable cargohold or S3 cannot
block it (clients may be removed for security reasons). The queue consumer
(`Brig.InternalEvent.Process.onEvent`, `DeleteClient`) deletes the client row
again (idempotent), then the client's backups, as its last step. A failing
deletion makes the queue redeliver the event until it succeeds.

Uploads and removals are ordered so that no backup outlives its client. Brig deletes the
client row before the backups. An upload (`uploadBackupImpl`,
`Wire.BackupSubsystem.Interpreter`) first PUTs the object and only then re-checks that the
client exists, deleting the object if not. So for any upload overlapping a removal, either
the backup deletion lists the object, or the re-check sees the client gone and the upload
deletes its object.

Deleting the backups on request of the client (`DELETE /backups/clients/{cid}`) has no client row
to order against, so it uses tombstones. It first stores a tombstone (`.tombstones/<id>`, a UUIDv7
like file ids), then deletes all epochs, then the older tombstones (the newest one stays). After
its PUT and the client check, an upload lists the tombstones: one newer than the file's id means
the deletion started after the upload did, so the upload deletes its object and fails with 403.
Either the deletion lists the object or the upload sees the tombstone. Uploads started after the
deletion have newer ids and are unaffected. This relies on UUIDv7 time order across cargohold
instances, like "newest full wins". Client and user deletion remove tombstones with the prefix.

User deletion works the same way with a backend-internal queue event
(`DeleteBackups`, not a Wire event, never fanned out). `deleteAccount`
(`Brig.API.User`) enqueues it first, before the user is marked deleted, so a failing
enqueue leaves the deletion retriable. The consumer (`onEvent`, `DeleteBackups`) deletes
the user's client rows (idempotent), then all backups of the user (by prefix) as its last
step. A failing deletion makes the queue redeliver the event until it succeeds.

## Known limitations

- Upload returns HTTP 200 (not 201).
- Missing `Content-Length` yields 400.
- With `aws.multiIngress` configured, backup download URLs are signed with the
  default S3 env (`s3Endpoint`) and may be unreachable to clients. Follow-up:
  thread `Z-Host` through.
- Backups of a removed client or user are deleted asynchronously by the brig delete queue, which
  redelivers until cargohold succeeds. Until then they are unreachable via the API (reads
  need a live client). There is no dead-letter handling beyond the queue's own configuration.
- Bot accounts are deleted without backup cleanup (`deleteBot` in
  `Brig.Provider.API` only calls `ClientStore.delete`). Bots are assumed unable
  to upload (no team feature); not verified in code.

## Timeouts

Uploads can be 1 GiB. nginz allows 1024m on `/backups` with no request
buffering. The chart has no per-path timeouts: global `proxy_read_timeout`
(1h) and `client_body_timeout` (60s) apply.
