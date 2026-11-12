# Automatic backups

Clients can upload encrypted backup files to cargohold. Cargohold stores the
bytes in S3 and the metadata in Postgres. Gated by the team feature
`automaticBackups`.

## Requirements

Cargohold needs a Postgres database (see `config-options.md`):
`cargohold.config.postgresql`, `postgresqlPool` and
`cargohold.secrets.pgPassword`. Migration `20261111120000-backups.sql` is
applied by brig.

## API (public, via nginz `/backups`)

All paths are under `/backups/clients/{cid}/epochs`:

- `GET /` list epochs with files.
- `POST /{epoch}` upload a file (raw body, `Content-Length` required).
- `GET /{epoch}` list files of an epoch.
- `GET /{epoch}/files/{id}` download: 302 redirect to a presigned S3 URL.
- `DELETE /{epoch}` and `DELETE /` (client) delete backups.

Limits (`cargohold.settings.backupLimits`): first file of an epoch up to
`maxFullBytes`, later files up to `maxIncrementalBytes`, at most
`maxFilesPerEpoch` files per epoch.

### Why list + per-file redirect

Presigned URLs are bearer links with a short TTL. Listing returns file ids
only; each download asks for its own redirect, so URLs are minted when needed
and expire quickly. Nothing is pre-signed in bulk.

### S3 keys, no metadata

Blob keys are derived from random ids; no user-visible metadata is stored on
the S3 object. All metadata (owner, client, epoch, kind, size) lives in
Postgres, so S3 holds only opaque bytes.

## Deletion / upload ordering invariant

A client's backups are deleted in cargohold /after/ the client row is gone
(`execDelete` in `Wire.ClientSubsystem.Interpreter`). Together with the order
in `uploadBackupImpl` (`Wire.BackupSubsystem.Interpreter`: insert backup row,
then re-check the client) no backup outlives its client: either removal sees
the uploader's row, or the uploader sees the client is gone and rolls back.
Failure of the cargohold call propagates, i.e. the client removal fails.

## Known limitations

- Upload returns HTTP 200 (not 201).
- Missing `Content-Length` yields 400.
- With `aws.multiIngress` configured, backup download URLs are signed with the
  default S3 env (`s3Endpoint`) and may be unreachable to clients. Follow-up:
  thread `Z-Host` through.
- Client removal deletes the client row first, then the backups. If the
  cargohold call fails after the row deletion, a retry hits `ClientNotFound`
  and that client's backups can be orphaned. This is inherent to the ordering.

## Timeouts

Uploads can be 1 GiB. nginz allows 1024m on `/backups` with no request
buffering. The chart has no per-path timeouts: global `proxy_read_timeout`
(1h) and `client_body_timeout` (60s) apply.
