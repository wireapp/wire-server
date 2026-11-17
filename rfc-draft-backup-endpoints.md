## Backup endpoints

Auth for all routes: `Authorization: Bearer <access token>`. nginz sets `Z-User` and `Z-Client` from the token. Clients don't send them.

Backup routes (1–6) exist from API version V19, e.g. `/v19/backups/...`. Feature routes (7, 8) have no version bound (any `/v{n}/teams/...`).

### 1. `GET /backups/clients/{cid}/epochs`
- **Path params:** `cid` (ClientId)
- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200
  ```json
  { "epochs": [1, 2] }
  ```
- **Meaning:** list epochs stored for the device

### 2a. `POST /backups/clients/{cid}/epochs/{epoch}/full`
- **Path params:** `cid`; `epoch` (Word32, ≥ 1)
- **Headers:** `Z-User`, `Z-Client`, `Content-Length` (required), `Content-Type: application/octet-stream`
- **Request body:** raw backup bytes (≤ `maxFullBytes`)
- **Response:** 200
  ```json
  { "id": "<uuid>", "kind": "full", "size": 1048576, "createdAt": "2026-11-12T10:15:30.123Z", "ready": true }
  ```
- **Meaning:** upload the full file of an epoch. Exactly one full per epoch: 409 `backup-full-exists` if a ready full exists, 409 `backup-full-locked` if a full upload is in progress or was interrupted. Server checks epoch rule, cap and the feature flag. `cid` must equal `Z-Client`. A failed or aborted upload leaves no file and no lock.

### 2b. `POST /backups/clients/{cid}/epochs/{epoch}/incremental`
- **Path params:** `cid`; `epoch`
- **Headers:** `Z-User`, `Z-Client`, `Content-Length` (required), `Content-Type: application/octet-stream`
- **Request body:** raw backup bytes (≤ `maxIncrementalBytes`)
- **Response:** 200, same body as 2a with `"kind": "incremental"`
- **Meaning:** upload an incremental file. Needs a ready full in the epoch (409 `backup-full-missing`, or 409 `backup-full-locked` if only an unfinished full exists). Epoch rule, file count cap and feature flag apply. `cid` must equal `Z-Client`.

### 3. `GET /backups/clients/{cid}/epochs/{epoch}`
- **Path params:** `cid`, `epoch`
- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 200. Order: `full` first, then `incremental`, each by `id`.
  ```json
  { "files": [ { "id": "<uuid>", "kind": "full|incremental", "size": 1048576, "createdAt": "…", "ready": true } ] }
  ```
- **Meaning:** list files of an epoch (metadata only, no URLs). A full whose upload is in progress or was interrupted is listed with `"ready": false`; `createdAt` is the start of its upload and `size` is 0 if nothing was stored yet. The server can't tell in-progress from stale (no lock timeout): a client with no upload running deletes the epoch (#5) and uploads a new full. 404 if the epoch has neither files nor an upload.

### 4. `GET /backups/clients/{cid}/epochs/{epoch}/files/{kind}/{id}`
- **Path params:** `cid`, `epoch`, `kind` (`full` or `incremental`), `id` (UUID)
- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 302, empty body, `Location: <presigned S3 URL>`
- **Meaning:** download one file via redirect. 409 `backup-full-locked` for a full that is not ready.

### 5. `DELETE /backups/clients/{cid}/epochs/{epoch}`
- **Path params:** `cid`, `epoch`
- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 204, empty
- **Meaning:** delete an epoch (files and lock). Own client only; idempotent. 204 without effect if backups are not configured on the server.

### 6. `DELETE /backups/clients/{cid}`
- **Path params:** `cid`
- **Headers:** `Z-User`, `Z-Client`
- **Request body:** none
- **Response:** 204, empty
- **Meaning:** delete all backups of the device. Own client only; idempotent.

### 7. `GET /teams/{tid}/features/automaticBackups`
- **Path params:** `tid` (TeamId)
- **Headers:** `Z-User`
- **Request body:** none
- **Response:** 200 (`ttl` optional)
  ```json
  {
    "status": "enabled|disabled",
    "lockStatus": "locked|unlocked",
    "config": { "schedule": "daily|weekly|monthly", "enforced": false, "serverOnly": false },
    "ttl": "unlimited"
  }
  ```
- **Meaning:** read the team feature config

### 8. `PUT /teams/{tid}/features/automaticBackups`
- **Path params:** `tid`
- **Headers:** `Z-User`, `Content-Type: application/json`
- **Request body:** (`ttl` optional)
  ```json
  {
    "status": "enabled|disabled",
    "config": { "schedule": "daily|weekly|monthly", "enforced": false, "serverOnly": false },
    "ttl": "unlimited"
  }
  ```
- **Response:** 200, same body as #7
- **Meaning:** set the team feature config

### Feature defaults
`status` is `disabled`, `lockStatus` is `unlocked`, `schedule` is `weekly`, `enforced` and `serverOnly` are `false`.

### Errors

| Status | Label | Raised by |
|---|---|---|
| 403 | `backup-feature-disabled` | 2a, 2b (team flag not enabled, or user has no team); 1, 2a, 2b, 3, 4 (backups not configured on the server, i.e. no HMAC key) |
| 403 | `backup-forbidden` | 2a, 2b, 5, 6 (`cid` is not `Z-Client`) |
| 404 | `backup-not-found` | 1, 3, 4 (`Z-Client` or `cid` is not a client of the caller; 3, 4: no such epoch or file); 2a, 2b, 5, 6 (`cid` is not a client of the caller) |
| 409 | `backup-epoch-not-allowed` | 2a, 2b |
| 409 | `backup-full-exists` | 2a (a ready full exists) |
| 409 | `backup-full-locked` | 2a (lock present), 2b (only an unfinished full), 4 (full not ready) |
| 409 | `backup-full-missing` | 2b |
| 409 | `backup-too-many-files` | 2b |
| 413 | `backup-too-large` | 2a, 2b (declared `Content-Length` over the cap) |
| 400 | `incomplete-body` | 2a, 2b (body length differs from `Content-Length`) |
| 403, 404 | `operation-denied`, `no-team-member`, `no-team` | 7, 8 (existing feature-API errors; labels not verified) |
