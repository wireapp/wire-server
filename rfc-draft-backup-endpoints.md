## API endpoints

Public endpoints only. Grouped by path. `<type>` marks a value of that type (see Types), the examples show filled-in values.

Auth for all routes: `Authorization: Bearer <AccessToken>`. nginz sets `Z-User` (`<UserId>`) and `Z-Client` (`<ClientId>`) headers from the token. Clients don't send them.

Backup routes exist from API version `V19`, i.e. `/v19/backups/...`. Feature routes have no version bound (any `/v{n}/teams/...`).

### Types

| Type               | JSON / URL form                                                                                   | Example                                |
| ------------------ | ------------------------------------------------------------------------------------------------- | -------------------------------------- |
| `<ClientId>`       | as in the rest of the Wire API (64-bit hex numeral)                                               | `4b2f7a9c1d3e5f60`                     |
| `<UserId>`         | as in the rest of the Wire API (UUID)                                                             | `9c1e7a52-3d84-4b6f-8e0a-71f2c5d9b3a4` |
| `<BackupEpoch>`    | integer 1…4294967295 (0 is rejected with a 400 from the path parser)                              | `2`                                    |
| `<BackupKind>`     | `full` or `incremental`                                                                           | `full`                                 |
| `<BackupFileId>`   | UUID (v7, assigned by the server; its time part is `createdAt`)                                   | `0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07` |
| `<TeamId>`         | as in the rest of the Wire API (UUID)                                                             | `3f0c6d1a-9b7e-4c25-a1d8-6e2b5f4a7c90` |
| `<UTCTimeMillis>`  | ISO 8601 UTC with milliseconds                                                                    | `2026-11-12T10:15:30.123Z`             |
| `<ByteCount>`      | number of bytes, non-negative integer                                                             | `1048576`                              |
| `<BackupFile>`     | `{"id": <BackupFileId>, "kind": <BackupKind>, "size": <ByteCount>, "createdAt": <UTCTimeMillis>}` | see below                              |
| `<EpochSummary>`   | `{"epoch": <BackupEpoch>, "createdAt": <UTCTimeMillis>, "fileCount": <Word32>, "size": <ByteCount>}` | see `GET /backups`                  |
| `<ClientBackups>`  | `{"client": <ClientId>, "epochs": [<EpochSummary>]}`                                              | see `GET /backups`                     |
| `<BackupSchedule>` | `daily`, `weekly` or `monthly`                                                                    | `weekly`                               |
| `<FeatureStatus>`  | `enabled` or `disabled`                                                                           | `enabled`                              |
| `<LockStatus>`     | `locked` or `unlocked`                                                                            | `unlocked`                             |

---

### `/backups/…`

Rules shared by all routes under `/backups`:

- If the server has no HMAC key configured (backups not configured), every route under `/backups` answers 403 `backup-feature-disabled`.
- There is no public delete endpoint. Backups of a client are deleted when the client is deleted, all backups of a user when the user is deleted (internal, by brig).
- The server keeps at most the two newest epochs of a client: after a successful upload it deletes all older ones.

#### `/backups`

##### `GET /backups`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200 `{"clients": [<ClientBackups>]}`

List the restorable backups of all clients of the caller, e.g. for a new client that restores a backup. Per client its epochs (ascending), per epoch a summary of the current files (see the rules below): `createdAt` is the creation time of the newest current file, `fileCount` the number of current files including the full, `size` their total bytes. Epochs without a full and clients without any epoch are omitted. Clients are ordered by their newest `createdAt`, newest first. `Z-Client` must be a client of the caller (else 404 `backup-not-found`). No team flag check. The summary is small whatever the number of files. To restore, the client picks an entry, lists its files with `GET /backups/clients/<ClientId>/epochs/<BackupEpoch>` and fetches each via the download route.

Example: `GET /v19/backups` → `200`

```json
{
  "clients": [
    {
      "client": "4b2f7a9c1d3e5f60",
      "epochs": [
        {
          "epoch": 2,
          "createdAt": "2026-11-12T10:35:30.456Z",
          "fileCount": 2,
          "size": 1050624
        }
      ]
    }
  ]
}
```

---

#### `/backups/clients/<ClientId>/…`

Rules shared by all routes under `/backups/clients/<ClientId>`:

- Read routes (GET): `Z-Client` and `<ClientId>` must both be clients of the caller, else 404 `backup-not-found`. A client may read the backups of any client of the same user.
- Write routes (POST): `<ClientId>` must equal `Z-Client` (else 403 `backup-forbidden`) and be a client of the caller (else 404 `backup-not-found`).
- A stored file is always complete: the object only becomes visible once its upload finished. A failed upload leaves no file.
- _Newest full wins_: a full upload replaces the epoch's previous full and the incrementals of it. Files are ordered by `<BackupFileId>` (UUIDv7, i.e. server time); the current files of an epoch are the full with the greatest id and the incrementals with a greater id. A client must not upload incrementals while it uploads a new full of the same epoch: they are deleted when the full is stored.

##### `/backups/clients/<ClientId>`

###### `GET /backups/clients/<ClientId>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200 `<ClientBackups>`

The restorable epochs of one client, as one entry of `GET /backups`. `"epochs": []` if the client has no restorable backup.

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60` → `200`

```json
{
  "client": "4b2f7a9c1d3e5f60",
  "epochs": [
    {
      "epoch": 2,
      "createdAt": "2026-11-12T10:35:30.456Z",
      "fileCount": 2,
      "size": 1050624
    }
  ]
}
```

---

##### `/backups/clients/<ClientId>/epochs/<BackupEpoch>`

###### `GET /backups/clients/<ClientId>/epochs/<BackupEpoch>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200 `{"files": [<BackupFile>]}`. Order: `full` first, then `incremental`, each by `id`.

List the current files of an epoch (metadata only, no URLs). 404 `backup-not-found` if the epoch has no full.

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2` → `200`

```json
{
  "files": [
    {
      "id": "0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07",
      "kind": "full",
      "size": 1048576,
      "createdAt": "2026-11-12T10:15:30.123Z"
    },
    {
      "id": "0195f2d4-1b02-7e55-9f10-2c3d4e5f6a7b",
      "kind": "incremental",
      "size": 2048,
      "createdAt": "2026-11-12T10:35:30.456Z"
    }
  ]
}
```

---

##### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/full`

###### `POST /backups/clients/<ClientId>/epochs/<BackupEpoch>/full`

- **Headers:** `Z-User`, `Z-Client`, `Content-Length: <ByteCount>` (required), `Content-Type: application/octet-stream`
- **Request body:** raw backup bytes (`Content-Length` ≤ `maxFullBytes`)
- **Response:** 200 `<BackupFile>` with `"kind": "full"`, `"size"` = number of bytes stored

Upload the full file of an epoch. It replaces the previous full of the epoch and the incrementals of it (newest full wins, see `/backups/clients/<ClientId>/…`).

Checked before any write, in this order: own client, team flag `automaticBackups` enabled (user has a team), epoch rule, `Content-Length` over the cap (413).

Epoch rule (epochs ≥ 1): with no epoch stored any epoch is allowed; otherwise only the newest stored epoch `N` or `N+1` (409 `backup-epoch-not-allowed`). After a successful upload all but the two newest epochs are deleted by the server.

After storing, the server lists the epoch and deletes every full and incremental the new full superseded (older full, incrementals with a smaller id). Concurrent full uploads delete each other, a late older one deletes itself; the newest wins.

A failed upload (fewer bytes than `Content-Length` arrive, client disconnect, storage error, client removed meanwhile) removes the file and surfaces the error; a short body or disconnect gives 400 `incomplete-body`.

Size subtlety: the server stores exactly the first `Content-Length` bytes and ignores any excess, because the HTTP layer hands the handler no more than that. Too many bytes can't be detected and give a successful upload of a truncated file; too few give 400 `incomplete-body`. The response `size` is the number of bytes stored, so clients should compare it with the number of bytes they sent and, if it differs, upload a new full.

Example: `POST /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/full` with `Content-Length: 1048576` and 1048576 body bytes → `200`

```json
{
  "id": "0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07",
  "kind": "full",
  "size": 1048576,
  "createdAt": "2026-11-12T10:15:30.123Z"
}
```

---

##### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/incremental`

###### `POST /backups/clients/<ClientId>/epochs/<BackupEpoch>/incremental`

- **Headers:** `Z-User`, `Z-Client`, `Content-Length: <ByteCount>` (required), `Content-Type: application/octet-stream`
- **Request body:** raw backup bytes (`Content-Length` ≤ `maxIncrementalBytes`)
- **Response:** 200 `<BackupFile>` with `"kind": "incremental"`

Upload an incremental file. Needs a full in the epoch (else 409 `backup-full-missing`). Then epoch rule (as for full), size cap (413), and file count cap `maxFilesPerEpoch` counting the current files including the full (409 `backup-too-many-files`). Concurrent incrementals can slightly exceed the file cap. A failed upload leaves no file (400 `incomplete-body` for a short body or disconnect). Do not upload while a new full of the epoch is uploaded: the incremental is deleted when the full is stored.

Size subtlety: the server stores exactly the first `Content-Length` bytes and ignores any excess, because the HTTP layer hands the handler no more than that. Too many bytes can't be detected and give a successful upload of a truncated file; too few give 400 `incomplete-body`. The response `size` is the number of bytes stored, so clients should compare it with the number of bytes they sent and, if it differs, upload the incremental again (a new file) or a new full.

Example: `POST /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/incremental` with `Content-Length: 2048` and 2048 body bytes → `200`

```json
{
  "id": "0195f2d4-1b02-7e55-9f10-2c3d4e5f6a7b",
  "kind": "incremental",
  "size": 2048,
  "createdAt": "2026-11-12T10:35:30.456Z"
}
```

---

##### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/files/<BackupKind>/<BackupFileId>`

###### `GET /backups/clients/<ClientId>/epochs/<BackupEpoch>/files/<BackupKind>/<BackupFileId>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 302, empty body, `Location: <Url>` (presigned S3 URL, bearer link valid for `downloadLinkTTL` seconds)

Download one file via redirect. 404 `backup-not-found` if no such file exists(also for a wrong `<BackupKind>`).

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

| Status | Label                                | Raised by                                                                                                                                                                                                                                                                              |
| ------ | ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 400    | `incomplete-body`                    | `POST …/full`, `POST …/incremental` (fewer bytes than `Content-Length` arrive, or disconnect)                                                                                                                                                                                          |
| 403    | `backup-feature-disabled`            | `POST …/full`, `POST …/incremental` (team flag not enabled, or user has no team); every `/backups` route (backups not configured on the server, i.e. no HMAC key)                                                                                                                      |
| 403    | `backup-forbidden`                   | `POST …/full`, `POST …/incremental` (`<ClientId>` is not `Z-Client`)                                                                                                                                                                                                                   |
| 404    | `backup-not-found`                   | `GET /backups` (`Z-Client` is not a client of the caller); `GET /backups/clients/<ClientId>`, `GET …/epochs/<BackupEpoch>`, `GET …/files/…` (`Z-Client` or `<ClientId>` is not a client of the caller; for the last two also no such epoch or file); `POST`s (`<ClientId>` is not a client of the caller) |
| 409    | `backup-epoch-not-allowed`           | `POST …/full`, `POST …/incremental`                                                                                                                                                                                                                                                    |
| 409    | `backup-full-missing`                | `POST …/incremental`                                                                                                                                                                                                                                                                   |
| 409    | `backup-too-many-files`              | `POST …/incremental`                                                                                                                                                                                                                                                                   |
| 413    | `backup-too-large`                   | `POST …/full`, `POST …/incremental` (declared `Content-Length` over the cap; checked on the declared value, before any byte is read)                                                                                                                                                   |
| 403    | `operation-denied`, `no-team-member` | `GET`, `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                                                                 |
| 404    | `no-team`                            | `GET`, `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                                                                 |
| 409    | `feature-locked`                     | `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                                                                        |
