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
- `POST /{epoch}/incremental` upload an incremental file (needs a full file).
- `GET /{epoch}` list the current files of an epoch.
- `GET /{epoch}/files/{kind}/{id}` download: 302 redirect to a presigned S3 URL.

There are no public deletes. Backups are deleted on client and user deletion (brig,
internal). After storing a full file the server deletes all files of all but the two newest
epochs of the client.

`kind` is `full` or `incremental` and comes from the list response.

Limits (`cargohold.settings.backupLimits`): a full file up to `maxFullBytes`,
incremental files up to `maxIncrementalBytes`, at most `maxFilesPerEpoch` files per
epoch.

### Newest full wins

A stored object is complete (S3 shows it only when its upload finished), so there is
no lock or readiness flag. The current files of an epoch are the full with the
greatest id (UUIDv7, i.e. creation order) and the incrementals with a greater id.
After a full is stored the server deletes everything else in the epoch, so a new full
replaces the old one and its incrementals. Concurrent fulls prune each other, a late
older one deletes itself. Clients must not upload incrementals while a new full of the
same epoch is uploaded: they are deleted when the full is stored.

A failed or aborted upload (any error, client disconnect) removes the file it created.
Incremental uploads need a full (`409 backup-full-missing` otherwise).

### Why list + per-file redirect

Presigned URLs are bearer links with a short TTL. Listing returns id, kind,
size and createdAt (no URLs); each download asks for its own redirect, so URLs are minted when needed
and expire quickly. Nothing is pre-signed in bulk.

## Storage layout

One S3 object per uploaded file, in the asset bucket:

```
backup/<hmac(uid)>/<hmac(cid)>/<epoch>/<kind>/<id>
```

- `hmac(x)`: lowercase hex HMAC-SHA256 of `x` with `backupHmacKey` (64 chars).
  Plain user or client ids never appear in S3: keys show up in presigned URLs
  and logs.
- `epoch`: decimal, `kind`: `full` or `incremental`, `id`: UUIDv7 (key order
  is creation order).
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
| `id`, `kind` | key segments |
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

A client's backups are deleted in cargohold /after/ the client row is gone
(`execDelete` in `Wire.ClientSubsystem.Interpreter`). Together with the order
in `uploadBackupImpl` (`Wire.BackupSubsystem.Interpreter`: PUT the object,
then re-check the client) no backup outlives its client: either the prefix
listing of the deletion sees the uploaded object, or the uploader sees the
client is gone and deletes that object again. This relies on strong
list-after-write consistency. Failure of the cargohold call propagates, i.e.
the client removal fails.

## Known limitations

- Upload returns HTTP 200 (not 201).
- Missing `Content-Length` yields 400.
- With `aws.multiIngress` configured, backup download URLs are signed with the
  default S3 env (`s3Endpoint`) and may be unreachable to clients. Follow-up:
  thread `Z-Host` through.
- Client removal deletes the client row first, then the backups. If the
  cargohold call fails after the row deletion, a retry hits `ClientNotFound`
  and that client's backups can be orphaned. This is inherent to the ordering.
- Client removal (`DELETE /clients/:cid`, and eviction of an old client when
  `POST /clients` adds one) and account deletion now synchronously call
  cargohold. If cargohold or S3 is unavailable these fail with 5xx.
  After a failed removal the client row may already be gone, so a retry sees
  `ClientNotFound` and that client's backups may be orphaned. Follow-up:
  best-effort deletion plus a sweep.
- Bot accounts are deleted without backup cleanup (`deleteBot` in
  `Brig.Provider.API` only calls `ClientStore.delete`). Bots are assumed unable
  to upload (no team feature); not verified in code.

## Timeouts

Uploads can be 1 GiB. nginz allows 1024m on `/backups` with no request
buffering. The chart has no per-path timeouts: global `proxy_read_timeout`
(1h) and `client_body_timeout` (60s) apply.
