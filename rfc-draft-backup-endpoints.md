## Backup endpoints

Public endpoints only. Grouped by path. `<type>` marks a value of that type (see Types), the examples show filled-in values.

Auth for all routes: `Authorization: Bearer <AccessToken>`. nginz sets `Z-User` (`<UserId>`) and `Z-Client` (`<ClientId>`) from the token. Clients don't send them.

Backup routes exist from API version V19, i.e. `/v19/backups/...` (nginz routes `/backups` with or without version prefix). Feature routes have no version bound (any `/v{n}/teams/...`).

### Types

| Type               | JSON / URL form                                                                                                 | Example                                |
| ------------------ | --------------------------------------------------------------------------------------------------------------- | -------------------------------------- |
| `<ClientId>`       | as in the rest of the Wire API (64-bit hex numeral)                                                             | `4b2f7a9c1d3e5f60`                     |
| `<UserId>`         | as in the rest of the Wire API (UUID)                                                                           | `9c1e7a52-3d84-4b6f-8e0a-71f2c5d9b3a4` |
| `<BackupEpoch>`    | integer 1…4294967295 (0 is rejected with a 400 from the path parser)                                            | `2`                                    |
| `<BackupKind>`     | `full` or `incremental`                                                                                         | `full`                                 |
| `<BackupFileId>`   | UUID (v7, assigned by the server; its time part is `createdAt`)                                                 | `0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07` |
| `<TeamId>`         | as in the rest of the Wire API (UUID)                                                                           | `3f0c6d1a-9b7e-4c25-a1d8-6e2b5f4a7c90` |
| `<UTCTimeMillis>`  | ISO 8601 UTC with milliseconds                                                                                  | `2026-11-12T10:15:30.123Z`             |
| `<Word64>`         | non-negative integer                                                                                            | `1048576`                              |
| `<BackupFile>`     | `{"id": <BackupFileId>, "kind": <BackupKind>, "size": <Word64>, "createdAt": <UTCTimeMillis>, "ready": <Bool>}` | see below                              |
| `<BackupSchedule>` | `daily`, `weekly` or `monthly`                                                                                  | `weekly`                               |
| `<FeatureStatus>`  | `enabled` or `disabled`                                                                                         | `enabled`                              |
| `<LockStatus>`     | `locked` or `unlocked`                                                                                          | `unlocked`                             |

`<BackupFile>` example:

```json
{
  "id": "0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07",
  "kind": "full",
  "size": 1048576,
  "createdAt": "2026-11-12T10:15:30.123Z",
  "ready": true
}
```

### Rules

Rules shared by all routes under `/backups/clients/<ClientId>`:

- Read routes (GET): `Z-Client` and `<ClientId>` must both be clients of the caller, else 404 `backup-not-found`. A client may read the backups of any client of the same user.
- Write routes (POST, DELETE): `<ClientId>` must equal `Z-Client` (else 403 `backup-forbidden`) and be a client of the caller (else 404 `backup-not-found`).
- If the server has no HMAC key configured (backups not configured), every route under `/backups` answers 403 `backup-feature-disabled`, DELETE included: object keys are derived from the HMAC key, so the server can't locate stored backups to delete them. With a key configured, DELETE works for the own client independent of the team flag `automaticBackups`.
- A file is _ready_ once its upload is committed. Only fulls can be not ready (upload in progress or interrupted).

---

### `/backups/clients/<ClientId>`

#### `DELETE /backups/clients/<ClientId>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 204, empty

Delete all backups of the client (all epochs, files and locks, all object versions). Own client only; idempotent.

Example: `DELETE /v19/backups/clients/4b2f7a9c1d3e5f60` → `204`

---

### `/backups/clients/<ClientId>/epochs`

#### `GET /backups/clients/<ClientId>/epochs`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200 `{"epochs": [<BackupEpoch>]}`, ascending

List the epochs that have anything stored for the client. An epoch that only has a lock (full upload started, nothing stored) is listed too.

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60/epochs` → `200`

```json
{ "epochs": [1, 2] }
```

---

### `/backups/clients/<ClientId>/epochs/<BackupEpoch>`

#### `GET /backups/clients/<ClientId>/epochs/<BackupEpoch>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200 `{"files": [<BackupFile>]}`. Order: `full` first, then `incremental`, each by `id`.

List the files of an epoch (metadata only, no URLs). A full whose upload is in progress or was interrupted is listed with `"ready": false`: `createdAt` is the start of its upload and `size` is 0 if nothing was stored yet. Incrementals are always `"ready": true`. The server can't tell in-progress from stale (no lock timeout): a client with no upload running deletes the epoch (`DELETE` below) and uploads a new full. 404 `backup-not-found` if the epoch has neither a file nor a lock.

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2` → `200`

```json
{
  "files": [
    {
      "id": "0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07",
      "kind": "full",
      "size": 1048576,
      "createdAt": "2026-11-12T10:15:30.123Z",
      "ready": true
    },
    {
      "id": "0195f2d4-1b02-7e55-9f10-2c3d4e5f6a7b",
      "kind": "incremental",
      "size": 2048,
      "createdAt": "2026-11-12T10:35:30.456Z",
      "ready": true
    }
  ]
}
```

#### `DELETE /backups/clients/<ClientId>/epochs/<BackupEpoch>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 204, empty

Delete an epoch (files and lock, all object versions). Own client only; idempotent. Recovery from an interrupted full upload.

Example: `DELETE /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2` → `204`

---

### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/full`

#### `POST /backups/clients/<ClientId>/epochs/<BackupEpoch>/full`

- **Headers:** `Z-User`, `Z-Client`, `Content-Length: <Word64>` (required), `Content-Type: application/octet-stream`
- **Request body:** raw backup bytes (`Content-Length` ≤ `maxFullBytes`)
- **Response:** 200 `<BackupFile>` with `"kind": "full"`, `"size"` = `Content-Length`, `"ready": true`

Upload the full file of an epoch. Exactly one full per epoch.

Checked before any write, in this order: own client, team flag `automaticBackups` enabled (user has a team), epoch rule, ready full exists (409 `backup-full-exists`), `Content-Length` over the cap (413), any lock in the epoch (409 `backup-full-locked`: another full upload in progress, or one was interrupted).

Epoch rule (epochs ≥ 1): with no epoch stored any epoch is allowed; with one stored epoch `N` only `N` or `N+1`; with two consecutive epochs `N`, `N+1` only `N+1`; otherwise none (409 `backup-epoch-not-allowed`). Delete old epochs to proceed.

Concurrency: the server puts a lock marker, lists the epoch again and aborts (removing its lock) if a foreign ready full (`backup-full-exists`) or lock (`backup-full-locked`) appeared. A tight race can fail both uploads with `backup-full-locked`; the client retries. The lock is removed after the file is stored: that is the commit that makes the full ready.

A failed upload (fewer bytes than `Content-Length` arrive, client disconnect, storage error, client removed meanwhile) removes file and lock and surfaces the error; a short body or disconnect gives 400 `incomplete-body`. After a hard server crash, or if the final lock removal fails (500), the full stays `ready: false` until the epoch is deleted.

Size subtlety: the server stores exactly the first `Content-Length` bytes and ignores any excess, because the HTTP layer hands the handler no more than that. Too many bytes can't be detected and give a successful upload of a truncated file; too few give 400 `incomplete-body`. The response `size` is the number of bytes stored, so clients should compare it with the number of bytes they sent and, if it differs, delete the epoch and retry.

Example: `POST /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/full` with `Content-Length: 1048576` and 1048576 body bytes → `200`

```json
{
  "id": "0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07",
  "kind": "full",
  "size": 1048576,
  "createdAt": "2026-11-12T10:15:30.123Z",
  "ready": true
}
```

---

### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/incremental`

#### `POST /backups/clients/<ClientId>/epochs/<BackupEpoch>/incremental`

- **Headers:** `Z-User`, `Z-Client`, `Content-Length: <Word64>` (required), `Content-Type: application/octet-stream`
- **Request body:** raw backup bytes (`Content-Length` ≤ `maxIncrementalBytes`)
- **Response:** 200 `<BackupFile>` with `"kind": "incremental"`, `"ready": true`

Upload an incremental file. Needs a ready full in the epoch: otherwise 409 `backup-full-locked` if the epoch has a lock (unfinished full), else 409 `backup-full-missing`. Then epoch rule (as for full), size cap (413), and file count cap `maxFilesPerEpoch` counting ready files including the full (409 `backup-too-many-files`). No lock; concurrent incrementals can slightly exceed the file cap. A failed upload leaves no file (400 `incomplete-body` for a short body or disconnect).

Size subtlety: the server stores exactly the first `Content-Length` bytes and ignores any excess, because the HTTP layer hands the handler no more than that. Too many bytes can't be detected and give a successful upload of a truncated file; too few give 400 `incomplete-body`. The response `size` is the number of bytes stored, so clients should compare it with the number of bytes they sent and, if it differs, delete the epoch and retry (or upload the incremental again, which is a new file).

Example: `POST /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/incremental` with `Content-Length: 2048` and 2048 body bytes → `200`

```json
{
  "id": "0195f2d4-1b02-7e55-9f10-2c3d4e5f6a7b",
  "kind": "incremental",
  "size": 2048,
  "createdAt": "2026-11-12T10:35:30.456Z",
  "ready": true
}
```

---

### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/files/<BackupKind>/<BackupFileId>`

#### `GET /backups/clients/<ClientId>/epochs/<BackupEpoch>/files/<BackupKind>/<BackupFileId>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 302, empty body, `Location: <Url>` (presigned S3 URL, bearer link valid for `downloadLinkTTL` seconds)

Download one file via redirect. 404 `backup-not-found` if no such file exists (a lock-only full has no file, so it is 404, also for a wrong `<BackupKind>`). 409 `backup-full-locked` for a full that exists but is not ready (costs one extra lookup of its lock).

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/files/full/0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07` → `302`, `Location: https://s3.example.com/backup/…?X-Amz-Signature=…`

---

### `/teams/<TeamId>/features/automaticBackups`

Generic team feature API. `<ConfigBody>` is `{"schedule": <BackupSchedule>, "enforced": <Bool>, "serverOnly": <Bool>}`. Defaults: `status` `disabled`, `lockStatus` `unlocked`, `schedule` `weekly`, `enforced` and `serverOnly` `false`. The flag is checked on uploads only.

#### `GET /teams/<TeamId>/features/automaticBackups`

- **Headers:** `Z-User`
- **Request body:** none
- **Response:** 200 `{"status": <FeatureStatus>, "lockStatus": <LockStatus>, "config": <ConfigBody>, "ttl": "unlimited"}`

Read the team feature config. Caller must be a team member.

Example: `GET /v19/teams/3f0c6d1a-9b7e-4c25-a1d8-6e2b5f4a7c90/features/automaticBackups` → `200`

```json
{
  "status": "enabled",
  "lockStatus": "unlocked",
  "config": { "schedule": "daily", "enforced": false, "serverOnly": false },
  "ttl": "unlimited"
}
```

#### `PUT /teams/<TeamId>/features/automaticBackups`

- **Headers:** `Z-User`, `Content-Type: application/json`
- **Request body:** `{"status": <FeatureStatus>, "config": <ConfigBody>, "ttl": <FeatureTTL>}`. `ttl` is optional (`"unlimited"`, `"0"` or positive integer).
- **Response:** 200, same body as `GET`

Set the team feature config. Needs the permission to change team features. 409 `feature-locked` if `lockStatus` is `locked`. `ttl` is ignored; the response always has `"ttl": "unlimited"`.

Example: `PUT /v19/teams/3f0c6d1a-9b7e-4c25-a1d8-6e2b5f4a7c90/features/automaticBackups`

```json
{
  "status": "enabled",
  "config": { "schedule": "daily", "enforced": false, "serverOnly": false }
}
```

→ `200`

```json
{
  "status": "enabled",
  "lockStatus": "unlocked",
  "config": { "schedule": "daily", "enforced": false, "serverOnly": false },
  "ttl": "unlimited"
}
```

---

### Errors

| Status | Label                                | Raised by                                                                                                                                                                                                                                 |
| ------ | ------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 400    | `incomplete-body`                    | `POST …/full`, `POST …/incremental` (fewer bytes than `Content-Length` arrive, or disconnect)                                                                                                                                            |
| 403    | `backup-feature-disabled`            | `POST …/full`, `POST …/incremental` (team flag not enabled, or user has no team); every `/backups/…` route (backups not configured on the server, i.e. no HMAC key)                                                          |
| 403    | `backup-forbidden`                   | `POST …/full`, `POST …/incremental`, `DELETE …/epochs/<BackupEpoch>`, `DELETE /backups/clients/<ClientId>` (`<ClientId>` is not `Z-Client`)                                                                                               |
| 404    | `backup-not-found`                   | `GET …/epochs`, `GET …/epochs/<BackupEpoch>`, `GET …/files/…` (`Z-Client` or `<ClientId>` is not a client of the caller; for the last two also no such epoch or file); `POST`s and `DELETE`s (`<ClientId>` is not a client of the caller) |
| 409    | `backup-epoch-not-allowed`           | `POST …/full`, `POST …/incremental`                                                                                                                                                                                                       |
| 409    | `backup-full-exists`                 | `POST …/full` (a ready full exists, or one appeared during the upload)                                                                                                                                                                    |
| 409    | `backup-full-locked`                 | `POST …/full` (a lock exists, or appeared during the upload); `POST …/incremental` (only an unfinished full); `GET …/files/…` (full not ready)                                                                                            |
| 409    | `backup-full-missing`                | `POST …/incremental`                                                                                                                                                                                                                      |
| 409    | `backup-too-many-files`              | `POST …/incremental`                                                                                                                                                                                                                      |
| 413    | `backup-too-large`                   | `POST …/full`, `POST …/incremental` (declared `Content-Length` over the cap; checked on the declared value, before any byte is read)                                                                                                                                                              |
| 403    | `operation-denied`, `no-team-member` | `GET`, `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                    |
| 404    | `no-team`                            | `GET`, `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                    |
| 409    | `feature-locked`                     | `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                           |
