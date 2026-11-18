## API endpoints

Only public endpoints are described here. They are grouped by path. `<type>`
marks a value of that type (see Types), the examples show filled-in values.

All routes are authenticated via an access token in the header: `Authorization:
Bearer <AccessToken>`. nginz extracts and sets the internal `Z-User`
(`<UserId>`) and `Z-Client` (`<ClientId>`) headers from the token. Clients
don't (and cannot) provide those.

Backup routes exist from the development API version, which is `V19` at the
time of writing (this may change due to deferred finishing/merging of this
feature). I.e. all described the endpoints need a `/v19` (or later) prefix in
their path: `/v19/backups/...`. Feature routes are not version bound and can be
access with any version prefix (e.g. `/v{n}/teams/...`).

### Types

This table describes the `<type>` placeholders used in described HTTP routes
(paths) and payloads.

| Type               | JSON / URL form                                                                                          | Example                                |
| ------------------ | -------------------------------------------------------------------------------------------------------- | -------------------------------------- |
| `<ClientId>`       | as in the rest of the Wire API (64-bit hex numeral)                                                      | `4b2f7a9c1d3e5f60`                     |
| `<UserId>`         | as in the rest of the Wire API (UUID)                                                                    | `9c1e7a52-3d84-4b6f-8e0a-71f2c5d9b3a4` |
| `<BackupEpoch>`    | integer 1…4294967295 (0 is rejected with a 400 from the path parser)                                     | `2`                                    |
| `<BackupFileId>`   | UUID (v7, assigned by the server; its time part is `createdAt`). The id of a full file is its `<FullId>` | `0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07` |
| `<TeamId>`         | as in the rest of the Wire API (UUID)                                                                    | `3f0c6d1a-9b7e-4c25-a1d8-6e2b5f4a7c90` |
| `<UTCTimeMillis>`  | ISO 8601 UTC with milliseconds                                                                           | `2026-11-12T10:15:30.123Z`             |
| `<ByteCount>`      | number of bytes, non-negative integer                                                                    | `1048576`                              |
| `<BackupFile>`     | `{"id": <BackupFileId>, "size": <ByteCount>, "createdAt": <UTCTimeMillis>}`                              | see below                              |
| `<BackupFiles>`    | `{"full": <BackupFile>, "incrementals": [<BackupFile>]}`                                                 | see `GET …/epochs/<BackupEpoch>`       |
| `<EpochSummary>`   | `{"epoch": <BackupEpoch>, "createdAt": <UTCTimeMillis>, "fileCount": <Word32>, "size": <ByteCount>}`     | see `GET /backups`                     |
| `<ClientBackups>`  | `{"client": <ClientId>, "epochs": [<EpochSummary>]}`                                                     | see `GET /backups`                     |
| `<BackupSchedule>` | `daily`, `weekly` or `monthly`                                                                           | `weekly`                               |
| `<FeatureStatus>`  | `enabled` or `disabled`                                                                                  | `enabled`                              |
| `<LockStatus>`     | `locked` or `unlocked`                                                                                   | `unlocked`                             |

---

### `/backups/…`

Rules shared by all routes under `/backups`:

- If the server has no HMAC key configured (backups not configured), every route under `/backups` answers 403 `backup-feature-disabled`.
- `Z-Client` must be a current client of the caller (checked against brig, the token alone doesn't prove it), else 404 `backup-not-found`.
  - This guards against the case where a client has a valid authorization token, but gets deleted while the token is valid.
- Only uploads check the team flag `automaticBackups`; reads work whatever its state.
- There is no public delete endpoint. Backups of a client are deleted when the client is deleted, all backups of a user when the user is deleted (internal, by brig).
- The server keeps at most the two newest epochs of a client: after a full file is stored (`POST …/full`) it deletes all files of every epoch except the two newest, i.e. the oldest one when a third epoch starts. Storing an incremental file deletes nothing.

#### `/backups`

##### `GET /backups`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200 `{"clients": [<ClientBackups>]}`

List the restorable backups of all clients of the caller; e.g. to provide an
overview for a new client that restores a backup.

Shows per client its epochs (ascending), per epoch a summary of the current
files. The epoch's contents are:

- `createdAt`: the creation time of the newest current file
- `fileCount`: the number of current files including the full
- `size`: their total bytes.

Epochs without a full and clients without any epoch are omitted. (Epochs
without a full can only exist due to technical corner cases, they have no
meaning to clients and users.)

Clients are ordered by their newest `createdAt`, newest first.

The summary is small whatever the number of files: Listing all files as well
could easily lead to huge payloads. Thus, we provide only a summary of the
epochs here.

To restore, the client picks an entry, lists its files with `GET
/backups/clients/<ClientId>/epochs/<BackupEpoch>` and fetches each via the
download route.

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

- Read routes (GET):
  - `Z-Client` and `<ClientId>` must both be clients of the caller, else 404 `backup-not-found`.
  - A client may read the backups of any client of the same user.
- Write routes (POST):
  - `Z-Client` and `<ClientId>` must both be clients of the caller, else 404 `backup-not-found`. (Same rule as for GET.)
  - `<ClientId>` must equal `Z-Client` (else 403 `backup-forbidden`). I.e. clients can only write for their own, never for others!
- A stored file is always complete: the object only becomes visible once its upload finished. A failed upload leaves no file.

##### Newest full wins

A full upload replaces the epoch's previous full and the incrementals of it.
The current full of an epoch is the one with the greatest `<BackupFileId>`
(UUIDv7, i.e. server time). An incremental is stored under the full it extends
(`<FullId>` in its path) and is current only while that full is the current
one. The client learns a full's id from the response of the finished upload (or
from `GET …/epochs/<BackupEpoch>`), so an incremental can only extend a full
that finished. An incremental of a replaced full is never listed, whatever the
timing of the uploads, and the server deletes it on a later upload of the
epoch.

This clear relation between fulls and incrementals prevents a couple of difficult issues:

- Incrementals can never be accounted to a wrong full. (This would render the backup useless!)
- Race-conditions on server-side regarding
  - lock creation
  - clock synchronization
  - crashes

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
- **Response:** 200 `<BackupFiles>`: the current full and the incrementals under it, by `id`.

List the current files of an epoch (metadata only, no URLs). `full.id` is the
`<FullId>` in the paths of the upload and download routes of all of these
files. 404 `backup-not-found` if the epoch has no full.

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2` → `200`

```json
{
  "full": {
    "id": "0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07",
    "size": 1048576,
    "createdAt": "2026-11-12T10:15:30.123Z"
  },
  "incrementals": [
    {
      "id": "0195f2d4-1b02-7e55-9f10-2c3d4e5f6a7b",
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
- **Response:** 200 `<BackupFile>`, `"size"` = number of bytes stored. Its `id` is the `<FullId>` of the incrementals that extend this full

Upload the full file of an epoch. It replaces the previous full of the epoch
and the incrementals of it.

Checked before any write, in this order: own client, team flag
`automaticBackups` enabled (user has a team), epoch rule, `Content-Length` over
the cap (413).

Epoch rule (epochs ≥ 1): with no epoch stored any epoch is allowed; otherwise
only the newest stored epoch `N` or `N+1` (409 `backup-epoch-not-allowed`).
After a full file is stored, the server deletes all files of all but the two
newest epochs; storing an incremental file deletes nothing (this happens via
another endpoint anyways).

After storing, the server lists the epoch and deletes everything the new full
superseded: older fulls (according to the mentioned rule) and all incrementals
stored under them. Concurrent full uploads delete each other, a late older one
deletes itself and responds with error; the newest wins.

A failed upload (fewer bytes than `Content-Length` arrive, client disconnect,
storage error, client removed meanwhile) removes the file and surfaces the
error; a short body or disconnect gives 400 `incomplete-body`.

Size subtlety: the server stores exactly the first `Content-Length` bytes and
ignores any excess, because the HTTP layer hands the handler no more than that.
Too many bytes can't be detected and give a successful upload of a truncated
file; too few give 400 `incomplete-body`. The response `size` is the number of
bytes stored, so clients should compare it with the number of bytes they sent
and, if it differs, upload a new full.

Example: `POST /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/full` with
`Content-Length: 1048576` and 1048576 body bytes → `200`

```json
{
  "id": "0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07",
  "size": 1048576,
  "createdAt": "2026-11-12T10:15:30.123Z"
}
```

---

##### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/full/<FullId>/incremental`

###### `POST /backups/clients/<ClientId>/epochs/<BackupEpoch>/full/<FullId>/incremental`

- **Headers:** `Z-User`, `Z-Client`, `Content-Length: <ByteCount>` (required), `Content-Type: application/octet-stream`
- **Request body:** raw backup bytes (`Content-Length` ≤ `maxIncrementalBytes`)
- **Response:** 200 `<BackupFile>`

Upload an incremental file under the full `<FullId>`, which must be the epoch's current full (else 409 `backup-full-missing`: no such full, or it was replaced; upload a new full). `<FullId>` is the `id` of a finished full upload. Then epoch rule (as for full), size cap (413), and file count cap `maxFilesPerEpoch` counting the current files including the full (409 `backup-too-many-files`). Concurrent incrementals can slightly exceed the file cap. A failed upload leaves no file (400 `incomplete-body` for a short body or disconnect).

Size subtlety: the server stores exactly the first `Content-Length` bytes and ignores any excess, because the HTTP layer hands the handler no more than that. Too many bytes can't be detected and give a successful upload of a truncated file; too few give 400 `incomplete-body`. The response `size` is the number of bytes stored, so clients should compare it with the number of bytes they sent and, if it differs, upload the incremental again (a new file) or a new full.

Example: `POST /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/full/0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07/incremental` with `Content-Length: 2048` and 2048 body bytes → `200`

```json
{
  "id": "0195f2d4-1b02-7e55-9f10-2c3d4e5f6a7b",
  "size": 2048,
  "createdAt": "2026-11-12T10:35:30.456Z"
}
```

---

##### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/full/<FullId>`

###### `GET /backups/clients/<ClientId>/epochs/<BackupEpoch>/full/<FullId>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 302, empty body, `Location: <Url>` (presigned S3 URL, bearer link valid for `downloadLinkTTL` seconds)

Download the full file via redirect. 404 `backup-not-found` if no such file exists (e.g. it was replaced by a newer full).

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/full/0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07` → `302`, `Location: https://s3.example.com/backup/…?X-Amz-Signature=…`

---

##### `/backups/clients/<ClientId>/epochs/<BackupEpoch>/full/<FullId>/incremental/<BackupFileId>`

###### `GET /backups/clients/<ClientId>/epochs/<BackupEpoch>/full/<FullId>/incremental/<BackupFileId>`

- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 302, empty body, `Location: <Url>` (as above)

Download an incremental file via redirect. 404 `backup-not-found` if no such file exists under that full.

Example: `GET /v19/backups/clients/4b2f7a9c1d3e5f60/epochs/2/full/0195f2c0-7a1e-7c3b-8d2a-5e6f4a1b9c07/incremental/0195f2d4-1b02-7e55-9f10-2c3d4e5f6a7b` → `302`, `Location: https://s3.example.com/backup/…?X-Amz-Signature=…`

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

| Status | Label                                | Raised by                                                                                                                                                                                                                                                                                                                                    |
| ------ | ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 400    | `incomplete-body`                    | `POST …/full`, `POST …/incremental` (fewer bytes than `Content-Length` arrive, or disconnect)                                                                                                                                                                                                                                                |
| 403    | `backup-feature-disabled`            | `POST …/full`, `POST …/incremental` (team flag not enabled, or user has no team); every `/backups` route (backups not configured on the server, i.e. no HMAC key)                                                                                                                                                                            |
| 403    | `backup-forbidden`                   | `POST …/full`, `POST …/incremental` (`<ClientId>` is not `Z-Client`)                                                                                                                                                                                                                                                                         |
| 404    | `backup-not-found`                   | `GET /backups` (`Z-Client` is not a client of the caller); `GET /backups/clients/<ClientId>`, `GET …/epochs/<BackupEpoch>`, `GET …/full/<FullId>[/incremental/<BackupFileId>]` (`Z-Client` or `<ClientId>` is not a client of the caller; for the last two also no such epoch or file); `POST`s (`<ClientId>` is not a client of the caller) |
| 409    | `backup-epoch-not-allowed`           | `POST …/full`, `POST …/incremental`                                                                                                                                                                                                                                                                                                          |
| 409    | `backup-full-missing`                | `POST …/full/<FullId>/incremental` (no full `<FullId>`, or not the current one)                                                                                                                                                                                                                                                              |
| 409    | `backup-too-many-files`              | `POST …/incremental`                                                                                                                                                                                                                                                                                                                         |
| 413    | `backup-too-large`                   | `POST …/full`, `POST …/incremental` (declared `Content-Length` over the cap; checked on the declared value, before any byte is read)                                                                                                                                                                                                         |
| 403    | `operation-denied`, `no-team-member` | `GET`, `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                                                                                                                       |
| 404    | `no-team`                            | `GET`, `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                                                                                                                       |
| 409    | `feature-locked`                     | `PUT /teams/<TeamId>/features/automaticBackups`                                                                                                                                                                                                                                                                                              |
