# TOTP (RFC 6238) support in wire-server: Implementation Plan

This document is dual-use: you can read it yourself (if you like lots of detail), or you can use it as a skill for solving all the tickets that will come out of it.

Scope: server-side TOTP second factor — enrolment, management, verification, recovery codes — exposed as versioned, documented, publicly reachable REST endpoints on brig, with unit and integration tests.

---

## 0. Library choice

`Crypto.OTP` from `crypton` is the right call. Relevant surface:

```haskell
type OTP     = Word32
type OTPTime = Word64
data OTPDigits = OTP4 | OTP5 | OTP6 | OTP7 | OTP8 | OTP9
data ClockSkew = NoSkew | OneStep | TwoSteps | ThreeSteps | FourSteps
data TOTPParams h

mkTOTPParams  :: HashAlgorithm h => h -> OTPTime -> Word16 -> OTPDigits -> ClockSkew -> Either String (TOTPParams h)
defaultTOTPParams :: TOTPParams SHA1          -- t0=0, period=30, OTP6, TwoSteps
totp       :: (HashAlgorithm h, ByteArrayAccess k) => TOTPParams h -> k -> OTPTime -> OTP
totpVerify :: (HashAlgorithm h, ByteArrayAccess k) => TOTPParams h -> k -> OTPTime -> OTP -> Bool
```

`crypton` is already a dependency of brig, spar and cargohold, so no new package enters the dependency set. Base32 encoding of the shared secret (what authenticator apps consume) comes from `memory`'s `Data.ByteArray.Encoding.convertToBase Base32` — also already in the tree.

`ClockSkew` is a *radius*, not a window: `totpVerify` checks steps `t-r … t+r` where `r = fromEnum skew` (`Crypto/OTP.hs`, `totpVerify`/`range`). So `TwoSteps` means five candidate steps, i.e. one code stays valid for 150 s at a 30 s period. Our config exposes the **total** window instead; see §4.

`totpVerify` is written to be constant-time: it evaluates every candidate step and folds the comparisons with a branch-free `eqMask`, never stopping at the first hit. Anything we hand-roll must preserve that.

Four things `crypton` does **not** give us, which we must build:

1. **Replay protection.** `totpVerify` accepts any code inside the window, so the same 6 digits stay valid for the whole window (90 s at our default, §4). We must record the highest time-step already consumed per credential and reject anything `<=` it (§3).
2. **Which step matched.** `totpVerify` returns a `Bool`, and both replay protection and the learned offset (§4) need the matched step. We therefore call `totp` ourselves for each candidate step and fold the comparisons the way `totpVerify` does — all candidates evaluated, no early exit.
3. **Expiry of unfinished enrolments.** The secret handed out at enrolment must not linger forever if the user never activates it (§3).
4. **Recovery codes.** Entirely our own (§3, §4).

`crypton` has no QR functionality; QR is an unrelated concern. Generating one server-side would mean a new dependency (`qrcode-core` + `qrcode-juicypixels`, or `haskell-qrencode` binding libqrencode) plus an image encoder in brig. We do **not** do that. The server returns the `otpauth://` URI and the Base32 secret; clients render the QR.

The API documentation must state the `otpauth://` URI format explicitly so client teams do not have to reverse-engineer it. The enrolment response carries both the URI and the raw Base32 secret (§2), so a client need not parse the former to obtain the latter.

---

## 1. Module layout

A small, independent Polysemy effect that lives inside the `AuthenticationSubsystem` module namespace, with its persistence layer flat alongside the other stores.

```
libs/wire-api/src/Wire/API/User/Auth/TOTP.hs                       -- wire types + schemas
libs/wire-api/src/Wire/API/Routes/Public/Brig/TOTP.hs              -- servant API + OpenAPI docs
libs/wire-subsystems/src/Wire/AuthenticationSubsystem/TOTP.hs             -- effect (GADT + makeSem)
libs/wire-subsystems/src/Wire/AuthenticationSubsystem/TOTP/Config.hs      -- TOTPSubsystemConfig (params, limits, enc. key)
libs/wire-subsystems/src/Wire/AuthenticationSubsystem/TOTP/Crypto.hs      -- secret encryption at rest
libs/wire-subsystems/src/Wire/AuthenticationSubsystem/TOTP/Error.hs       -- TOTPError
libs/wire-subsystems/src/Wire/AuthenticationSubsystem/TOTP/Interpreter.hs -- the real logic
libs/wire-subsystems/src/Wire/TOTPStore.hs                         -- store effect
libs/wire-subsystems/src/Wire/TOTPStore/Postgres.hs                -- store interpreter
```

Rationale:

- The module tree states the relationship (`TOTP` ⊂ authentication) without inflating the already-large `AuthenticationSubsystem` GADT, which is additionally awkward because it sits in brig's `RecursiveEffects` group (`services/brig/src/Brig/CanonicalInterpreter.hs:184`) and is let-tied to `UserSubsystem`/`ClientSubsystem`.
- Stores in this repo are uniformly flat (`UserGroupStore`, `PasswordStore`, `SessionStore`, …), so `Wire.TOTPStore` matches convention and avoids a four-level nesting.

**Dependency direction (keeps the graph acyclic):**

```
AuthenticationSubsystem  ──depends on──▶  AuthenticationSubsystem.TOTP  ──▶  TOTPStore, HashPassword, RateLimit, Random, Now
```

The TOTP effect must **not** depend on `AuthenticationSubsystem`. Password re-authentication before a sensitive TOTP operation therefore happens one level up (§6), never inside the TOTP effect. Concretely, `TOTP` gets interpreted in `NonRecursiveEffects2` / `BrigLowerLevelEffects`, below `AuthenticationSubsystem`.

---

## 2. Wire types (`Wire.API.User.Auth.TOTP`)

```haskell
data TOTPCredential                      -- phantom tag, added to Data.Id (cf. UserGroupId precedent)
type TOTPCredentialId = Id TOTPCredential

newtype TOTPSecret   = TOTPSecret ByteString -- JSON: unpadded Base32 (RFC 4648)
newtype TOTPCode     = TOTPCode  Text        -- JSON: digit string; parse checks digits-only and
                                             -- length 6..9 (wire-api cannot see the configured
                                             -- digit count; the exact-length check is the verifier's)
newtype RecoveryCode = RecoveryCode Text     -- JSON: "xxxx-xxxx-xxxx-xxxx", Crockford Base32,
                                             -- 16 symbols = 80 bits (plan-otp-13)

-- Wherever a second factor is *submitted* (ReauthBody, Login), either kind of
-- code is allowed. Parse into this sum once, at the JSON boundary, instead of
-- shape-sniffing a Text later (plan-otp-14): the two shapes are disjoint by
-- construction (digits-only vs. Crockford-with-dashes), and the parser is
-- where that invariant lives and is tested.
data SecondFactorCode = SFTotp TOTPCode | SFRecovery RecoveryCode

data TOTPHashAlgorithm = TOTPSHA1 | TOTPSHA256 | TOTPSHA512   -- site-configurable, §8

-- public metadata; the secret is never returned again after enrolment
data TOTPCredentialInfo = TOTPCredentialInfo
  { id_        :: TOTPCredentialId
  , name       :: Text
  , createdAt  :: UTCTimeMillis
  , activatedAt:: Maybe UTCTimeMillis
  }

data NewTOTPCredential = NewTOTPCredential
  { name       :: Text
  , clientTime :: UTCTimeMillis   -- the client's idea of "now"; see §4 clock skew
  , reauth     :: ReauthBody      -- flattened into the same JSON object: a request
                                  -- has exactly one body, so "ReauthBody + NewTOTPCredential"
                                  -- must be one type (§5)
  }

-- one-time response to enrolment
data TOTPEnrolment = TOTPEnrolment
  { credentialId :: TOTPCredentialId
  , secret       :: TOTPSecret
  , uri          :: Text   -- otpauth://totp/Wire:alice%40example.com?secret=..&issuer=Wire&algorithm=SHA1&digits=6&period=30
  , algorithm    :: TOTPHashAlgorithm
  , digits       :: Word8
  , period       :: Word16
  , serverTime   :: UTCTimeMillis   -- lets the client self-diagnose clock drift
  , expiresAt    :: UTCTimeMillis   -- unfinished enrolments expire, see §3
  }

data ActivateTOTPCredential = ActivateTOTPCredential
  { code       :: TOTPCode
  , clientTime :: UTCTimeMillis
  }

newtype TOTPCredentialName = TOTPCredentialName { name :: Text }
newtype TOTPRecoveryCodes  = TOTPRecoveryCodes { codes :: [RecoveryCode] }
```

Plus the re-authentication payload shared by every privileged endpoint (§6):

```haskell
data ReauthBody = ReauthBody
  { password :: Maybe PlainTextPassword6
  , totpCode :: Maybe SecondFactorCode  -- JSON field: "totp_code"; a TOTP or a recovery
                                        -- code (a bare TOTPCode here would reject every
                                        -- recovery code at parse time, §4 step 4)
  }
```

All of these via `Data.Schema` (`deriving (FromJSON, ToJSON, S.ToSchema) via Schema X`), with `description`/`example` modifiers on every field so the OpenAPI output is self-explanatory. `Arbitrary` instances for roundtrip tests.

Do **not** derive `Show` for `TOTPSecret`/`TOTPCode` naively — use a redacting `Show` (cf. `Data.Secret.SecretText` in `libs/types-common`) so secrets can't land in logs. Same for other serializations if available, like ToByteString, ...

---

## 3. Storage

New Postgres tables (new persistence goes to Postgres; migrations live in `libs/wire-subsystems/postgres-migrations/`). The user table is `wire_user`.

```sql
-- <timestamp>-create-totp-tables.sql
CREATE TABLE totp_credential (
  user_id        uuid        NOT NULL REFERENCES wire_user(id) ON DELETE CASCADE,
  id             uuid        NOT NULL,
  name           text        NOT NULL,
  secret         bytea       NOT NULL,     -- encrypted, see below
  key_id         text        NOT NULL,     -- id of the encryption key used
  algorithm      text        NOT NULL,
  digits         smallint    NOT NULL,
  period_seconds smallint    NOT NULL,
  step_offset    smallint    NOT NULL DEFAULT 0,  -- learned clock skew, see §4
  created_at     timestamptz NOT NULL,
  activated_at   timestamptz,              -- NULL = pending enrolment
  PRIMARY KEY (user_id, id)
);

-- supports the pending-enrolment sweep
CREATE INDEX totp_credential_pending_idx
  ON totp_credential (created_at) WHERE activated_at IS NULL;

-- replay protection; short-lived by construction, see below
CREATE TABLE totp_step_claim (
  user_id       uuid        NOT NULL,
  credential_id uuid        NOT NULL,
  step          bigint      NOT NULL,
  expires_at    timestamptz NOT NULL,
  PRIMARY KEY (user_id, credential_id),
  FOREIGN KEY (user_id, credential_id)
    REFERENCES totp_credential (user_id, id) ON DELETE CASCADE
);

CREATE INDEX totp_step_claim_expiry_idx ON totp_step_claim (expires_at);

CREATE TABLE totp_recovery_code (
  user_id    uuid        NOT NULL REFERENCES wire_user(id) ON DELETE CASCADE,
  id         uuid        NOT NULL,
  code_hash  text        NOT NULL,         -- argon2id via Wire.HashPassword
  created_at timestamptz NOT NULL,
  used_at    timestamptz,
  PRIMARY KEY (user_id, id)
);
```

### No durable last-use record

Two different things got conflated under "last used", and only one of them has anything to do with replay protection:

- **`last_used_at`** — a plain audit timestamp. Nothing depends on it. Dropped entirely: no column, no field in `TOTPCredentialInfo`. A second-factor usage history is a plausible future feature ("you have an authenticator you have not used in a year"), and it can be added then, as a product decision.
- **`last_used_step`** — *is* the replay protection. It cannot be dropped: without remembering which step was consumed, the same six digits work twice for as long as the window lasts (§0).

The uncomfortable part: a step number **is** a timestamp. `step × period` is the Unix time of the user's last successful second-factor authentication, to 30-second granularity. Keeping a permanent `last_used_step` while dropping `last_used_at` for privacy reasons would have bought approximately nothing — same fact, worse encoding.

**So bound its lifetime instead.** The claim is only load-bearing for as long as a code it could reject is still acceptable, and that is a short, computable interval:

> A stored claim for step `s` can only reject something while some acceptable code has a step `≤ s`. Codes acceptable at local step `t` are `[t-r, t+r]`, and the claim was written when `t ∈ [s-r, s+r]`. The claim therefore stops being able to reject anything `2r` steps after it was written — and `r` is 1 at the default `skew = 3`. (The learned `step_offset` cancels out: both sides are in the credential's own step numbering.)

So each claim carries `expires_at = now + (skew + 1) × period` — the derived bound `2r × period = (skew - 1) × period`, plus two steps of slack for clock drift between replicas. Two minutes at the defaults. Past that the row is ignored (the compare-and-set treats an expired claim as absent) and deleted by the sweep (§3, `plan-otp-16`).

The resulting property, which is the one worth stating in the privacy review: **the database never holds a record of when a user last authenticated that is older than a couple of minutes.** A dump, a backup, or a read-only SQL injection yields "this person used their second factor within the last two minutes, or nothing at all" instead of a per-credential login-time history going back years.

Two things this deliberately does *not* claim. It is not erasure: Postgres `DELETE` leaves dead tuples until autovacuum, and the WAL and any backup taken in those two minutes still contain the value. And the claim is kept in its own table rather than as a column on `totp_credential` precisely so that deletion is a row disappearing rather than a column being nulled — the credential row then holds enrolment metadata only, structurally, and cannot accumulate behavioural data by someone later adding "just one more" column next to it.

If brig already has a Redis available (confirm — do not add one for this), the same claim as a key with a native TTL is strictly better on the erasure axis and removes the sweep entirely. The store interface below is identical either way, so that is a swap of one interpreter, not a redesign.

The `algorithm`, `digits` and `period` columns record what the credential was *enrolled* with. Config (§8) decides what new enrolments get; existing credentials keep their own parameters forever, because the authenticator app has them baked in and there is no way to update it. Changing the site config therefore never invalidates an enrolled credential.

### Pending-enrolment expiry

A pending row holds a secret that was shown to the client exactly once; left alone it would sit there indefinitely. Two layers, both cheap:

1. **Authoritative, in the subsystem.** Every read of a pending credential compares `created_at` against `Now.get - ttl` and treats an older row as absent (`TOTPEnrolmentExpired`). Expiry is therefore correct even if the sweeper is down or lagging; nothing depends on the job for *security*, only for disk. The check sits in the subsystem, not in the SQL, per the dumb-store rule below — which also means it is unit-testable against a settable clock.
2. **Reclamation, job-side.** A periodic `DELETE FROM totp_credential WHERE activated_at IS NULL AND created_at < $1`, in bounded batches, with cutoff and batch bound computed by the caller, scheduled via the existing job machinery (`Wire.JobSubsystem` / brig's `Wire.BackgroundJobsRunner`).

TTL configurable, default 15 minutes (long enough to install an authenticator app and scan a QR code, short enough not to matter).

### Secret at rest

AES-256-GCM with a key from brig's config (`crypton`'s `Crypto.Cipher.AES.AES256` + `Crypto.Cipher.Types` AEAD), stored as `nonce ‖ ciphertext ‖ tag` in `secret`, with the key identifier in `key_id` so later key rotation needs no schema change. The user id is bound in as additional authenticated data, so a ciphertext cannot be transplanted between rows.

Precedent for secrets-in-config exists (zauth private keys, TURN secret). The key is optional in config; brig fails at startup if TOTP is enabled without one. This is the difference between "DB read = full 2FA bypass" and "DB read + config read". Roughly 60 lines in `AuthenticationSubsystem/TOTP/Crypto.hs`.

Recovery codes are a different matter: they are password-equivalent and are stored **hashed** (argon2id via `Wire.HashPassword`), never encrypted-reversible.

### Store effect

**The store is a dumb wrapper around the database.** It stores, finds, retrieves, updates and deletes rows. It contains no policy: no TTL arithmetic, no "is this activated", no "may this user have another credential", no deciding what counts as a replay. All of that is the subsystem's job (§4). The store takes values it is given and hands back rows as they are.

Consequences, stated explicitly because each one removes something a store effect is normally tempted to do:

- No `GetActivatedCredentials` — `GetCredentials` returns every row and the subsystem filters. A user has at most `maxCredentialsPerUser` (default 5) of them, so there is nothing to optimise.
- No TTL parameter anywhere. Pending-expiry is a subsystem decision made against `Now.get`; the sweep takes a cutoff that the subsystem computed.
- No `Bool` returns that mean "the row existed". The subsystem reads first and decides what a missing row means (`TOTPNotFound`, or silence, depending on the operation).
- Writes are field setters, named after the field, not after the use case.

```haskell
data TOTPStore m a where
  InsertCredential         :: UserId -> StoredTOTPCredential -> TOTPStore m ()
  GetCredential            :: UserId -> TOTPCredentialId -> TOTPStore m (Maybe StoredTOTPCredential)
  GetCredentials           :: UserId -> TOTPStore m [StoredTOTPCredential]
  GetCredentialsForUsers   :: [UserId] -> TOTPStore m [StoredTOTPCredential]   -- batch read (plan-otp-22)
  SetCredentialActivated   :: UserId -> TOTPCredentialId -> UTCTime -> Int16 -> TOTPStore m ()  -- Int16 = step_offset
  SetCredentialName        :: UserId -> TOTPCredentialId -> Text -> TOTPStore m ()
  -- now, expiresAt: both computed by the subsystem
  ClaimStepIfNewer         :: UserId -> TOTPCredentialId -> Word64 -> UTCTime -> UTCTime -> TOTPStore m Bool
  DeleteCredential         :: UserId -> TOTPCredentialId -> TOTPStore m ()
  DeleteAllCredentials     :: UserId -> TOTPStore m ()
  -- cutoff + batch bound, both computed by the caller (plan-otp-16 deletes in
  -- bounded batches; the store knows neither TTLs nor safe batch sizes)
  DeletePendingCreatedBefore :: UTCTime -> Int -> TOTPStore m Int
  DeleteExpiredStepClaims    :: UTCTime -> Int -> TOTPStore m Int
  InsertRecoveryCodes      :: UserId -> [(RecoveryCodeId, Password)] -> TOTPStore m ()
  GetRecoveryCodes         :: UserId -> TOTPStore m [StoredRecoveryCode]   -- including used ones
  MarkRecoveryCodeUsed     :: UserId -> RecoveryCodeId -> UTCTime -> TOTPStore m Bool
  DeleteRecoveryCodes      :: UserId -> TOTPStore m ()
```

**The two deliberate exceptions**, `ClaimStepIfNewer` and `MarkRecoveryCodeUsed`, both return `Bool` and both look like logic. They are not: they are atomicity, which is the one thing the subsystem physically cannot do for itself. A read-then-write in the subsystem loses the race between two concurrent requests presenting the same code, and that race *is* the replay attack. Each is one statement with the condition in the `WHERE` clause, returning the affected row count:

```sql
-- ClaimStepIfNewer: $3 = step, $4 = expires_at, $5 = now
INSERT INTO totp_step_claim (user_id, credential_id, step, expires_at)
VALUES ($1, $2, $3, $4)
ON CONFLICT (user_id, credential_id) DO UPDATE
   SET step = EXCLUDED.step, expires_at = EXCLUDED.expires_at
 WHERE totp_step_claim.step < EXCLUDED.step
    OR totp_step_claim.expires_at <= $5;   -- an expired claim is not a claim

-- MarkRecoveryCodeUsed
UPDATE totp_recovery_code SET used_at = $3
  WHERE user_id = $1 AND id = $2 AND used_at IS NULL;
```

Both `now` and `expires_at` are passed in by the subsystem rather than taken from `now()` in SQL — the store stays dumb, and the replay tests stay runnable against a settable clock.

The *policy* — that a non-increasing step is a replay and maps to `TOTPCodeReused`, and how long a claim is worth keeping — stays in the subsystem. The store only offers "write this value iff it is strictly greater or the previous one has expired", a database primitive. Do not implement either as read-then-write, and make the in-memory mock interpreter reproduce the compare-and-set *and the expiry* faithfully, or the replay tests pass against the mock and the bug ships.

---

## 4. Subsystem effect

```haskell
data TOTP m a where
  EnrolCredential        :: UserId -> Text -> UTCTime -> TOTP m TOTPEnrolment   -- UTCTime = client's "now"
  ActivateCredential     :: UserId -> TOTPCredentialId -> TOTPCode -> UTCTime -> TOTP m [RecoveryCode]
  ListCredentials        :: UserId -> TOTP m [TOTPCredentialInfo]
  RenameCredential       :: UserId -> TOTPCredentialId -> Text -> TOTP m ()
  DeleteCredential       :: UserId -> TOTPCredentialId -> TOTP m ()
  DeleteAllCredentials   :: UserId -> TOTP m ()
  HasActivatedCredential :: UserId -> TOTP m Bool
  GetEnrolmentStatuses   :: [UserId] -> TOTP m (Map UserId TOTPEnrolmentStatus)  -- plan-otp-22
  -- tries every activated credential, then recovery codes (plan-otp-14);
  -- consumes the matched step or code. Returns what matched rather than Bool:
  -- callers react differently to a burned recovery code (notify, plan-otp-17)
  -- and to a replay (plan-otp-09), and a Bool cannot carry either fact.
  VerifySecondFactor     :: UserId -> SecondFactorCode -> TOTP m SecondFactorResult

data SecondFactorResult = MatchedTOTP | MatchedRecoveryCode | Replayed | NoMatch

makeSem ''TOTP
```

Interpreter dependencies: `TOTPStore`, `HashPassword` (recovery codes), `RateLimit`, `Random`, `Wire.Sem.Now`, `Input TOTPSubsystemConfig`, `Error TOTPError`, `TinyLog`.

Verification algorithm in `VerifySecondFactor`:

1. `now <- Now.get`; `GetCredentials` and keep the activated ones (the filter is here, not in the store).
2. For each activated credential: decrypt the secret, compute the corrected current step `t = now / period + step_offset`, and evaluate `totp params secret` at every step in `[t-r … t+r]`, `r` the window radius from config. Compare all of them, fold the results, no early exit — this is what `crypton`'s own `totpVerify` does and the reason we cannot just call it is that we need to know *which* step matched.
3. On a match at step `s`: `ClaimStepIfNewer uid credId s now (now + claimTTL)`, with `claimTTL = (skew + 1) × period` (§3). `False` ⇒ the step was already consumed ⇒ `Replayed` (callers map it to `TOTPCodeReused`).
4. If no TOTP matched and the input is `SFRecovery`, hash-match against the unused recovery codes (`GetRecoveryCodes`, filter `used_at IS NULL` here) and `MarkRecoveryCodeUsed`; a match is `MatchedRecoveryCode`, which callers treat as a notification trigger (plan-otp-17).
5. On `NoMatch`, charge the per-user rate-limit bucket (see below).
6. To the end user every failure reads the same (`totp-invalid-code`), with `TOTPCodeReused` the one deliberate exception (plan-otp-09); code comparisons use constant-time primitives.

**Ordering rule for the whole stack:** a factor is never *consumed* while an earlier factor has already failed. Password first, TOTP second (§6); if the password is wrong, no step is claimed and no recovery code is burned. Otherwise a client that submits password and code together would burn a 30-second code on a typo in the password field and leave the user waiting for the next one.

### Clock skew handling

Three layers, in increasing order of how much they actually help.

**(a) Verification window.** Symmetric around the current step, in both directions: a code is accepted for steps `t-r … t+r`, so it tolerates an authenticator that is up to `r` steps behind *and* one that is up to `r` steps ahead.

The config knob `skew` is the **total window size in steps**, not the radius — so `skew = 3` means one step back, the current one, one step forward, and a given code is usable for 90 s at a 30 s period. Because the window is symmetric around a single current step, it is necessarily odd-sized; `crypton`'s `ClockSkew` is the radius `r`, with `NoSkew … FourSteps`, so the allowed values are exactly:

| `skew` (config) | radius | `ClockSkew` | code lifetime at `period = 30` |
|---|---|---|---|
| 1 | 0 | `NoSkew` | 30 s |
| 3 | 1 | `OneStep` | 90 s |
| 5 | 2 | `TwoSteps` | 150 s |
| 7 | 3 | `ThreeSteps` | 210 s |
| 9 | 4 | `FourSteps` | 270 s |

An even `skew` is a config error and brig refuses to start (§8) rather than silently rounding — a site admin who writes `4` must find out that they cannot have it, not get 3 or 5 without being told. Parse `skew` once, at config time, into the radius (or directly into `crypton`'s `ClockSkew`) and carry that in `TOTPSubsystemConfig`: the odd/even check and the `(skew - 1) / 2` division then exist in exactly one place (`plan-otp-06`) instead of being re-derived at every verification site.

Default **3** (±1 step), which is RFC 6238 §5.2's recommendation ("at most one time step"). `crypton`'s `defaultTOTPParams` uses `TwoSteps`, i.e. a 150 s lifetime, which is more than we need given that (c) handles persistent drift properly; we do not use `defaultTOTPParams`.

On its own, (a) does nothing for a device that is persistently minutes off, and widening it until it does is exactly the wrong fix: it lengthens every code's life for every user on the backend.

**(b) Enrolment-time clock check.** `POST /self/totp` and `POST /self/totp/:id/activate` both carry a `clientTime` field. The server compares it against `Now.get`; if `|Δ| > maxClientClockSkew` (config, default 30 s) it rejects with `409 totp-client-clock-skew`, whose error body carries the server time (via `RichError`, cf. `verificationCodeSubsystemErrorToHttpError`) so the client can say "your device clock is 4 minutes fast, fix it and retry" instead of the user discovering this later as a mystery "invalid code" at login. `TOTPEnrolment` also echoes `serverTime` so a client can self-diagnose before even trying.

Honest caveat, worth documenting for client teams: the clock that *actually* matters is the one on the device running the authenticator app, which is not necessarily the device calling this API. This check catches the common same-device case and gives a good diagnostic; it is not a guarantee. That is what (c) is for.

The defaults of (b) and (c) interact: (b) rejects a client clock more than 30 s off even though (c) could learn around ±60 s (`maxLearnedStepOffset = 2`). That is deliberate — a fixable clock should be fixed, not accommodated — but a site that widens `maxLearnedStepOffset` should widen `maxClientClockSkew` with it, or (b) rejects enrolments that (c) was explicitly configured to absorb.

**(c) Learned per-credential offset.** The code submitted at activation tells us exactly which time step the authenticator is on. If it matches step `n+k` rather than `n`, store `step_offset = k` on the credential and apply it on every subsequent verification. A phone that is persistently 45 s fast then keeps working indefinitely, *without* widening the window for anybody else — the window stays `skew` steps wide around the *corrected* time, so this costs nothing in security.

Activation therefore scans a **wider** range than verification does: `|k| ≤ maxLearnedStepOffset` (config, default 2, i.e. ±60 s). That is the whole point of the split — the generous range is used once, under an already-authenticated request, to pin the offset down; the tight `skew` window then applies to every subsequent login. Beyond that range, activation fails with the clock-skew error rather than silently accepting a badly-set clock.

The claimed step is recorded in the credential's own (offset-corrected) step numbering — the same number the authenticator used. Be consistent about this or replay protection silently breaks when the offset is non-zero, and no test catches it unless one is written for a skewed credential. (The claim's `expires_at` is wall-clock and the offset cancels out of its derivation, §3, so a skewed credential needs no special TTL.)

### Rate limiting

Use the existing per-user bucket — `doRateLimited (RateLimitUser uid)` against brig's single `rateLimitEnv` (`Brig/App.hs:301`, configured by `settings.passwordHashingRateLimit`). No new bucket. Reasoning:

- **Cost per TOTP attempt is negligible.** One indexed Postgres read, an AES-GCM decrypt of a 20-byte secret, and ~5 HMAC-SHA1 evaluations over 8 bytes — i.e. one DB roundtrip, sub-millisecond. The operation it guards (password verification) is Argon2id: deliberately memory-hard, ~100 ms and tens of MB. TOTP verification is two to three orders of magnitude cheaper than what the same request already paid for, so it is not a meaningful new DoS amplifier.
- **The real risk is guessing, not DoS.** 6 digits with the default `skew = 3` gives 3 valid codes in 10⁶, i.e. 3×10⁻⁶ per attempt (5×10⁻⁶ at `skew = 5`, which is the number to quote if the default changes). At the per-user rate (`burst: 5`, `inverseRate: 60s` ⇒ 60/h in `brig.integration.yaml`) an attacker who already has the password needs on the order of years for a 1% success chance. That is plenty.
- **Recovery codes are different.** Verifying one is an argon2id hash check — the same ~100 ms, memory-hard cost as a password. The recovery-code branch must therefore be reached only after the TOTP branch has failed, and must itself be rate limited.

One wrinkle to handle: there is a *single* bucket per `RateLimitKey`, shared with password hashing. A login that verifies password **and** TOTP would consume two tokens of a burst-5 bucket, so a user who fat-fingers their code three times could get a confusing 429 on an otherwise correct password. Therefore: **charge the bucket only on failure**, not on every attempt. Successful second factors cost nothing.

### Errors

`AuthenticationSubsystem/TOTP/Error.hs`, mapped to `Wire.API.Error.Brig`:

| constructor | HTTP | label |
|---|---|---|
| `TOTPNotFound` | 404 | `totp-not-found` |
| `TOTPAlreadyActivated` | 409 | `totp-already-activated` |
| `TOTPNotActivated` | 409 | `totp-not-activated` |
| `TOTPEnrolmentExpired` | 410 | `totp-enrolment-expired` |
| `TOTPInvalidCode` | 403 | `totp-invalid-code` |
| `TOTPCodeReused` | 403 | `totp-code-reused` |
| `TOTPClientClockSkew` | 409 | `totp-client-clock-skew` (body carries `server_time`) |
| `TOTPTooManyCredentials` | 409 | `totp-too-many-credentials` |
| `TOTPRequired` | 403 | `totp-required` |
| `TOTPDisabled` | 403 | `totp-disabled` |

---

## 5. REST API (brig)

`libs/wire-api/src/Wire/API/Routes/Public/Brig/TOTP.hs`, spliced into `BrigAPI`, all endpoints `:> From 'V19` (current development version — `isDevelopmentVersion V19 = True`).

| verb | path | `Named` | body → response |
|---|---|---|---|
| POST | `/self/totp` | `totp-enrol` | `NewTOTPCredential` (embeds `ReauthBody`, §2) → 201 `TOTPEnrolment` |
| POST | `/self/totp/:id/activate` | `totp-activate` | `ActivateTOTPCredential` → 200 `TOTPRecoveryCodes` |
| GET | `/self/totp` | `totp-list` | → 200 `[TOTPCredentialInfo]` |
| PUT | `/self/totp/:id` | `totp-rename` | `TOTPCredentialName` → 200 |
| DELETE | `/self/totp/:id` | `totp-delete` | `ReauthBody` → 200 |

These five are credential *management*. None of them authenticates anybody — they sit behind zauth like every other `/self` route, and the `ReauthBody` they carry re-proves an existing session. **Using** a second factor happens at the endpoints below.

### Existing endpoints that gain a second factor

No new routes, no new paths: each of these gains one optional field in a body it already has.

| verb | path | change | ticket |
|---|---|---|---|
| POST | `/login` | `Login` gains `totp_code :: Maybe SecondFactorCode` | `plan-otp-11` |
| GET | `/i/users/:uid/reauthenticate` | `ReAuthUser` gains the same field; galley and spar call this for `DeleteTeam` and `CreateScimToken` | `plan-otp-18` |
| DELETE | `/self` | `ReauthBody` | `plan-otp-18` |
| DELETE | `/clients/:id` | `ReauthBody` | `plan-otp-18` |
| PUT | `/self/password` | `ReauthBody` | `plan-otp-18` |
| PUT | `/self/email` | `ReauthBody` | `plan-otp-18` |
| — | OAuth access revocation | `ReauthBody` | `plan-otp-18` |

`POST /login` is the one that matters for the first release: without it, a user can enrol an authenticator and is never asked for it. It is the difference between shipping a settings screen and shipping a second factor.

Note that `Login` already has an `lCode` field (JSON `verification_code`) — that is the **emailed** code, a different mechanism (§6). `totp_code` is a new, separate field. Do not reuse `verification_code`; the two coexist until `plan-otp-21`.

#### Backwards compatibility, and the one place it breaks

- **Additive optional field, so no version bump.** The `/self/totp` endpoints are `From 'V19`; the login body change needs no version at all. A `Login` body *without* `totp_code` must still parse — that is the regression test the additive claim rests on (§7).
- **The field goes in the body.** Never a query parameter — a code in the URL is written to access logs at every proxy hop, and OTPs must not be logged (OWASP). Never a custom header — nginz rewrites `Z-*` headers, and a non-`Z-` header is easy to drop in a proxy chain. Two of the endpoints above have no natural body today; those get one (§6).
- **The challenge is retry-based.** First attempt without a code → `403 totp-required`; the client prompts the user and re-posts the *same* body with `totp_code` filled in. No challenge token, no server-side pending-login state, no second endpoint, nothing to expire. The client does not need to know in advance whether the account has a second factor.
- **Where it genuinely breaks:** a user who enrols on an up-to-date client and then logs in on an older one gets a `403 totp-required` that the old client cannot act on. Per-user opt-in bounds this (the user chose to enrol, on a client new enough to offer enrolment), but it is a real support case, not a theoretical one. Consequences: the error label must be stable and documented from the first release, and client releases should land before the feature is announced.
- **`totp-required` must be unreachable until the password has been verified.** Otherwise the response distinguishes "this account exists and has a second factor" from "bad credentials" *before* any secret is checked — an account-enumeration oracle, and a disclosure of who has 2FA enabled. §4's ordering rule already forces this sequence for a different reason (never consume a factor after an earlier one failed); this is the second, independent reason it is not negotiable. The test is that "wrong password, no code" and "wrong password, correct code" both return exactly what a wrong password returns today, byte for byte.

Internal (`Wire.API.Routes.Internal.Brig`), for ops/support and integration tests:

| verb | path | purpose |
|---|---|---|
| GET | `/i/users/:uid/totp` | list credentials (metadata only) |
| DELETE | `/i/users/:uid/totp` | admin reset for a user who lost both their authenticator and their recovery codes |

The internal `DELETE` needs an audit log line (`TinyLog` at `Info` with user id and request id) since it removes a second factor.

### Events

Changing a second factor is a security-relevant change to the account, and the user's *other* devices have to hear about it. The precedent is exact: `user.client-add` / `user.client-remove` (`Wire.API.UserEvent`, `ClientEvent`) are pushed to the user's own clients when a device is added or removed. Do the same.

```haskell
data TOTPEvent
  = TOTPCredentialAdded   !TOTPCredentialInfo
  | TOTPCredentialUpdated !TOTPCredentialInfo
  | TOTPCredentialRemoved !TOTPCredentialId
  deriving stock (Eq, Show)
```

with `EventType` elements `user.totp-add`, `user.totp-update`, `user.totp-remove`, pushed to self only.

Four decisions baked into that shape:

- **`Added` fires on activation, not on enrolment.** A pending credential is not a second factor; announcing it to other devices would put a half-finished entry in every settings screen, and the user might never complete it. It also means the event can never carry a secret, because by activation time the secret has already been handed out exactly once and is not in the payload.
- **The payload is `TOTPCredentialInfo`**, which by construction contains no secret and no `lastUsedAt` (D13). Do not invent a wider payload type for the event.
- **`Updated` exists for rename.** It is not a security signal, it is list freshness — without it, renaming an authenticator on one device leaves a stale name on the others, which is exactly the class of bug this section is here to prevent.
- **This is in addition to the email**, not instead of it. The email (`plan-otp-07`) is the out-of-band channel that reaches a user whose session an attacker controls; the event is the in-band channel that keeps the UI honest. They solve different problems and an attacker who can suppress one cannot generally suppress the other.

Ticket split: `Added`/`Updated` land with the endpoints that cause them (`plan-otp-07`), `Removed` with `plan-otp-12`, and the internal admin reset (`plan-otp-15`) and team-scoped revocation (`plan-otp-23`) must emit `Removed` too — an ops action that silently diverges the user's UI from reality is worse than no event at all.

A later addition is the team-scoped surface — `GET /teams/:tid/totp-status` (`plan-otp-22`), `DELETE /teams/:tid/members/:uid/totp` and `DELETE /teams/:tid/totp` (`plan-otp-23`) — under the same rules: `From 'V19`, documented in the commit that introduces them, routed in `charts/nginz`.

### Documentation is part of the endpoint, not a follow-up

Every route carries `Summary`, a multi-sentence `Description`, `ZLocalUser`, and an explicit `CanThrow` for every error it can produce, written **in the same commit that introduces the route**. The content checklist is in `plan-otp-07`. No endpoint merges with a one-line `Summary` and nothing else.

Handlers in `services/brig/src/Brig/API/Public.hs` (a `totpAPI :: ServerT TOTPAPI (Handler r)` block next to `userGroupAPI`).

### nginz

The paths are added to `charts/nginz/values.yaml`, alongside the existing `/self/...` entries. They ship as part of the chart; the API surface is not an operator knob. Exact entries and the two things to get right — **no `oauth_scope`** (second-factor management must require a real user session, never a third-party OAuth token) and route ordering/anchoring against `/self$` and any catch-all — are in `plan-otp-08`.

---

## 6. Using TOTP as a second factor

TOTP is built as its own clean, self-contained mechanism. It does **not** reuse `verificationCode` / `EnforceVerificationCodeEither`, and it does not try to refactor the existing second-factor code on the way past — that mess is real but it gets its own ticket (`plan-otp-21-second-factor-cleanup.md`).

Factual note for that ticket: the existing `verificationCode` is not only for SCIM tokens. `VerificationAction = CreateScimToken | Login | DeleteTeam` (`libs/wire-api/src/Wire/API/User.hs:1989`), and `enforceVerificationCodeEither` is called from `Brig.User.Auth.verifyLoginCode` (login), from `Brig.API.Auth.reauthenticate` (the internal `/i/users/:uid/reauthenticate` endpoint used by galley and spar), and from the SCIM token path. So login is already covered by the emailed-code mechanism, and the two mechanisms will coexist until the cleanup ticket unifies them.

### The mechanism

One function in `AuthenticationSubsystem`, used by every operation that today asks for a password:

```haskell
data SensitiveOperation
  = OpLogin
  | OpDeleteSelf
  | OpDeleteClient
  | OpChangePassword
  | OpChangeEmail
  | OpCreateScimToken
  | OpDeleteTeam
  | OpEnrolTOTP -- sensitive, but no bootstrapping problem: first use will work without OTP
  | OpDeleteTOTPCredential
  | OpRevokeOAuthAccess
  deriving (Eq, Show, Bounded, Enum)

EnsureReauthenticated :: Local UserId -> SensitiveOperation -> ReauthBody -> AuthenticationSubsystem m ()
```

Semantics, in order:

1. Verify the password if the account has one (existing `reauthenticateEither` logic, unchanged). If it fails, stop here — do not touch the second factor, so no code is consumed (§4, ordering rule).
2. If the user has an activated TOTP credential: require `totpCode` and verify it via `TOTP.VerifySecondFactor` (which also accepts a recovery code). Missing → `TOTPRequired`; `NoMatch` → `TOTPInvalidCode`; `Replayed` → `TOTPCodeReused`.
3. Otherwise: no second factor.

This is where the `AuthenticationSubsystem → TOTP` dependency lives, which is why the TOTP effect is interpreted below it (§1).

### Why this is safe to roll out everywhere at once

Enforcement is **per-user opt-in by enrolment**. A user with no activated credential sees no behaviour change at any endpoint. Adding an optional `totp_code` field to an existing request body is a backwards-compatible schema change, so no new API version is needed for the bodies themselves (the new `/self/totp` endpoints are still `From 'V19`).

### SSO accounts: exclusive, not additive

An account authenticates **either** via SSO **xor** via password (plus, optionally, TOTP). The two are mutually exclusive, and the exclusion is enforced in both directions: if SSO succeeds, a password and/or a `totp_code` in the same request are not merely ignored but rejected; conversely a password account never takes an SSO assertion.

So TOTP simply does not apply to SSO users. That resolves what would otherwise be a hole in `EnsureReauthenticated` step 1 ("verify the password *if the account has one*"): for an SSO account there is no password, no activated credential, and therefore nothing to degrade to. Three consequences that must be implemented, not merely assumed:

- `POST /self/totp` **rejects** SSO accounts outright. Without this, an SSO user enrolling their first authenticator would pass `EnsureReauthenticated` having proved nothing at all — no password to check, no prior credential to challenge. This is the concrete bug the xor rule closes; it needs its own test.
- Team-level enforcement (`plan-otp-19`, `plan-otp-24`, `plan-otp-25`) must **exempt SSO members**, or the feature demands a second factor that those users are structurally unable to enrol. Multi-factor for SSO accounts is the identity provider's job, and a team that wants it configures it there. Say this in the feature's documentation, because "we turned on TOTP enforcement and our SAML users were locked out" is otherwise a guaranteed incident.
- `plan-otp-22`'s team MFA status must report SSO members as a distinct third state, not as "no second factor". They are not non-compliant; they are out of scope.

### Password reset

Resetting a password must not become a way around the second factor. Today it already is not, but by accident rather than by design, and the accident is one refactor away from breaking.

**The load-bearing property:** `POST /password-reset/complete` returns `200` with an empty body and issues **no cookie**. The user must subsequently log in, and that login is challenged normally. So email possession alone yields a changed password, never a session. This property is what actually prevents the bypass, it is currently unwritten, and it deserves an explicit regression test: reset a TOTP user's password, then assert the following login still demands a code.

**On top of that, `CompletePasswordReset` gains an optional `totp_code :: Maybe SecondFactorCode`**, demanded when the account has an activated credential. This is not redundant with the login challenge: without it, anyone holding the user's mailbox can change the password at will — a denial of service against the legitimate user, and a persistent foothold that converts into full access the moment support removes the second factor for an apparently-stuck user. Four details:

- **The gate is per-user, not per-site.** The condition is "this user has an activated credential", exactly as in `EnsureReauthenticated` step 2 — *not* the site-level `totp.enabled`, which only says whether the feature exists at all. Gating on the site flag would demand a code from users who have never enrolled.
- **Recovery codes count**, because the field is `SecondFactorCode`. "Lost phone *and* forgot password" must stay recoverable, and recovery codes are how. The genuinely unrecoverable case — no authenticator, no recovery codes, no password — is unchanged from today and is what `plan-otp-15` admin reset exists for.
- **Ordering, again:** validate the emailed reset code first; a wrong one never consumes a TOTP step or burns a recovery code.
- **No enumeration concern here**, unlike at login (§5): by the time we ask for a second factor, the caller has already proved mailbox possession with a valid reset code. `totp-required` at this point tells an attacker nothing they could not already learn.

### Endpoints to wire up

This plan covers `POST /login`, the enrolment endpoints (`OpEnrolTOTP` — what stops a stolen session plus a stolen password from quietly adding a second authenticator, see `plan-otp-11`) and `DELETE /self/totp/:id`. The rest (`DELETE /self`, `DELETE /clients/:id`, `PUT /self/password`, email change, OAuth revocation, `/i/users/:uid/reauthenticate`) land in `plan-otp-18-reauth-remaining-operations.md`.

Two of those endpoints have no natural request body; for those, add one. Never a query param — a code in the URL is written to access logs at every proxy hop, and OTPs must not be logged (OWASP) — and not a custom header (nginz's zauth module rewrites `Z-*` headers, and a non-`Z-` header is easy to drop in a proxy chain).

### Team-level enforcement

Making a team *require* TOTP means a new galley team feature (`totp`, `TOTPConfig { enforced, enforcedFor, gracePeriodDays }` — the full shape is settled in `plan-otp-19` even though 24/25 give two of the fields meaning); see `plan-otp-19-team-feature-enforcement.md`. Note the deliberate naming split: the galley feature config is `TOTPConfig`, brig's subsystem config is `TOTPSubsystemConfig` (§1) — brig imports both.

---

## 7. Testing strategy

Per-ticket detail lives in the tickets. The cross-cutting rules:

- **Serialisation.** Every new type goes into `libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs`; `TOTPCredentialId` into the `HttpApiData` roundtrip; client-facing response types get golden JSON. A `Login` body *without* `totp_code` must still parse — that guards the additive-field claim in §6.
- **RFC 6238 Appendix B vectors** (T = 59, 1111111109, 1111111111, 1234567890, 2000000000, 20000000000 for SHA1/SHA256/SHA512) are asserted against our own parameter plumbing, not just against `crypton`.
- **A settable clock.** Unit tests use a mock `Wire.Sem.Now` so skew, replay and expiry are deterministic rather than timing-dependent. No `threadDelay`-based tests.
- **The opt-in guarantee is the single most important regression test**: a user with no activated credential must be unaffected at every endpoint touched.
- Integration tests need a client-side TOTP generator; see `plan-otp-04-integration-test-helper.md`.
- `integration/test/Test/Swagger.hs` must stay green and show the new endpoints.

---

## 8. Config, ops, docs

- `services/brig/src/Brig/Options.hs`: a `totp :: Maybe TOTPOpts` section — `enabled`, `secretEncryptionKeys` (list of `{ keyId, key }`, base64, one marked active so rotation is possible), `issuer` (defaults to the backend domain, shown in the authenticator app), `algorithm`, `period`, `digits`, `skew` (total window, odd, §4), `maxLearnedStepOffset`, `maxClientClockSkew`, `maxCredentialsPerUser`, `recoveryCodeCount`, `pendingEnrolmentTTL`. Mirror into `charts/brig/values.yaml`, `charts/brig/templates/configmap.yaml`, and `hack/helm_vars/` + the integration `brig.yaml` so CI exercises the feature.
- `charts/nginz/values.yaml`: route entries (§5).
- `changelog.d/`: entries for the new API and the new config options.
- `docs/`: the enrolment flow (with the `otpauth://` URI format and the client-renders-the-QR note), the clock-skew diagnostics, and the ops runbook for resetting a locked-out user.
- `postgres-schema.sql`: regenerate after the migration lands.

### The algorithm knob

`algorithm` is a site-admin setting in `charts/brig/values.yaml`, accepting exactly the hashes `crypton` can drive TOTP with:

| value | note |
|---|---|
| `sha1` | **default**; what RFC 6238 and essentially every authenticator app implement |
| `sha256` | RFC 6238 §1.2 variant; supported by fewer apps |
| `sha512` | ditto |

The set is closed by RFC 6238 (which defines HMAC-SHA1/SHA256/SHA512 only) and by `crypton`: `hotp`'s dynamic truncation needs a digest of at least `minimumDigestSize = 20` bytes, and `mkTOTPParams` rejects anything shorter — so MD5 and the truncated SHA-2 variants are out at the library level, not just by our choice. Validate the string at config-parse time against that list; do not pass an arbitrary name through.

Two warnings belong in the values.yaml comment, not only in `docs/`:

1. **Most authenticator apps ignore the `algorithm` parameter in the `otpauth://` URI** and assume SHA1. Google Authenticator is the notable example. Setting `sha256` or `sha512` on a backend whose users have such an app produces credentials that enrol fine and then never verify — a failure mode that looks exactly like clock skew and is far more annoying to diagnose. The same caveat applies to non-default `digits` and `period`.
2. **The setting applies to new enrolments only.** Existing credentials keep the algorithm stored in their row (§3), so changing it does not lock anyone out, and does not migrate anyone either. A site that wants to move its existing users has to make them re-enrol.

---

## 9. Decisions

| | decision |
|---|---|
| **D1** Module layout | Nested `Wire.AuthenticationSubsystem.TOTP`, flat `Wire.TOTPStore`. §1 |
| **D2** Pending enrolments | TTL, not a one-pending-slot cap. Enforced query-side *and* swept by a job. §3 |
| **D3** Secret at rest | AES-256-GCM with a key from brig config, `key_id` column for rotation, user id as AAD. Recovery codes hashed, not encrypted. §3 |
| **D4** Rate limiting | Reuse the existing per-user bucket; charge only on failure. Recovery-code checks are argon2id-expensive and must sit behind the TOTP branch. §4 |
| **D5** Second-factor wiring | Clean, self-contained `EnsureReauthenticated`; do **not** overload `verificationCode`. Login and enrolment + TOTP-disable ship first; the rest follows. §6 |
| **D6** Multiple authenticators | Yes, with a configurable per-user cap. |
| **D7** Recovery | Recovery code creation and recovery-code login ship with the initial release. Regeneration, status and notifications come later. Internal admin reset exists as the last resort. §5 |
| **D8** QR codes | Client-side. `crypton` has nothing to do with QR, and we add no QR dependency to brig. The server returns the `otpauth://` URI and the Base32 secret. §0 |
| **D9** Clock skew | Three layers: a symmetric verification window configured as a **total** odd step count (`skew`, default 3), an enrolment-time `clientTime` check with a diagnostic error, and a learned per-credential `step_offset` pinned at activation over a wider `maxLearnedStepOffset` range. §4 |
| **D10** nginz | Routes **are** shipped in `charts/nginz/values.yaml`, without `oauth_scope`. Site admins do not define the API surface. §5 |
| **D11** Documentation | OpenAPI documentation lands in the same commit as the route it documents, not afterwards. §5 |
| **D12** Store effects are dumb | `TOTPStore` does storage only — no TTL arithmetic, no activated/pending filtering, no replay policy. The two compare-and-set writes are atomicity primitives, not logic, and are named and justified as such. §3 |
| **D13** No durable last-use record | No `last_used_at` anywhere. The replay claim (a last-use timestamp in disguise) lives in its own `totp_step_claim` table and expires `(skew + 1) × period` after writing — provably past the point it can reject anything — so the DB never holds a login-time history. §3 |
| **D14** Hash algorithm | Site-configurable (`sha1` default, `sha256`, `sha512` — the set RFC 6238 defines and `crypton` accepts), stored per credential, applies to new enrolments only, with the app-compatibility caveat documented at the knob. §8 |
| **D15** SSO xor password | An account authenticates via SSO **xor** password(+TOTP); the exclusion is enforced both ways. TOTP therefore does not apply to SSO accounts: enrolment rejects them, team enforcement exempts them, MFA status reports them as a third state. §6 |
| **D16** Password reset | Reset grants no session, so the next login is challenged normally — that is the property that prevents the bypass, and it gets a regression test. On top, `CompletePasswordReset` gains an optional `totp_code`, gated per-user, accepting recovery codes. §6 |
| **D17** Events | `user.totp-add` / `-update` / `-remove` to self, modelled on `user.client-add`/`-remove`, carrying `TOTPCredentialInfo`. Fired on activation (not enrolment), and by admin reset and team revocation too. In addition to the notification email, not instead of it. §5 |
