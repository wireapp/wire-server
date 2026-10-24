# TOTP 00 — mock API

```
assignedTo: Matthias Fischmann
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §2 (types), §5 (routes), §0 (`otpauth://`). A copy of that spec lives in this repo at `otp-tickets/plan-otp.md`; every `§n` and `Dn` reference below points into it, so you do not need Confluence access to implement this ticket.

## Why this one exists

Client teams need the request/response contract and a reachable endpoint to code against **before** the subsystem, the store, the crypto and the migrations exist. This ticket delivers exactly that and nothing else: the real types, the real route tree, the real OpenAPI document, routing through nginz, and handlers that answer with `Arbitrary`-generated values instead of touching a database.

"Reachable" is the operative word. A mock that only answers on brig's internal port is a mock for *us*; client developers work against staging through nginz like everyone else. So the chart routes ship here, not in `TOTP 08`.

It is deliberately the cheapest possible thing that produces a working `curl` *from a client developer's laptop*. No storage, no crypto, no rate limiting, no `crypton`, no migration, **and no feature flag**. The only artefact that survives into the real implementation unchanged is the types-and-routes half; the handlers get thrown away in `TOTP 07`.

Relationship to the rest of the plan: this is `TOTP 01` (types) plus the servant half of `TOTP 07` (routes + docs) plus the nginz half of `TOTP 08`, with a stub handler module in between. Doing it as one ticket is the point — the deliverable is "a client developer can integrate", and that needs all four or none.

---

## Handover notes — read before writing code

This ticket is written to be implementable without the surrounding conversation. What follows is the context an implementer would otherwise have to rediscover.

### What to read first

| | |
|---|---|
| `otp-tickets/plan-otp.md` | the full spec. §2 is the type list, §4 the error table and the subsystem design, §5 the REST surface, §8 config. Read §0, §2, §4, §5. Skip §3 (storage) and §6 (second-factor wiring) — nothing here touches them. |
| `otp-tickets/plan-otp-01-api-types.md` | the sibling ticket whose work this one absorbs. Its "Requirements" section is the authority on secret handling and parser strictness. |
| `otp-tickets/plan-otp-07-enrolment-endpoints.md` | its "Documentation is part of this ticket" section is the checklist the route `Description`s must satisfy. |

The sibling tickets are drafts and will likely be re-cut. Where they disagree with *this* file, this file wins.

### Repo mechanics that are not obvious

- **Build with `make c package=brig` (or `package=wire-api`), not bare `cabal build`** — bare cabal fails dependency resolution in this tree. Add `test=1` to run that package's test suite. Non-interactive shells do not pick up `.envrc`, so prefix with `direnv exec .` if you are not in a direnv-activated shell.
- **Do not hand-edit module lists in `.cabal` files.** `wire-api.cabal:69`, `brig.cabal:77`, `types-common.cabal:15` and `integration.cabal:90` carry `-- cabal-fmt: expand <dir>` directives, and `make c` runs `treefmt` → `cabal-fmt --inplace` first, which regenerates those lists from the filesystem. You *do* hand-edit `build-depends`.
- **Integration test discovery has two hard rules** (`integration/Setup.hs`, `collectTestsInModule`): a test is picked up only if its name starts with `test` **and** it has a top-level type signature; and the module must use traditional `import qualified X as Y`, never `import X qualified as Y` — `haskell-src-exts` cannot parse `ImportQualifiedPost` and will silently skip the module. A test that does not appear in `make devtest` output is almost always one of these two.
- **Golden files are generated on first run.** `assertJSONIsGolden` (`libs/wire-api/test/golden/Test/Wire/API/Golden/Runner.hs`) writes the file if it is missing and *then* asserts it existed — so the first run creates the JSON and fails, and the second passes. Run the golden suite twice and commit what it wrote; do not hand-write the JSON.
- **`Data.ByteArray.Encoding` (from `memory`) is importable in wire-api without adding `memory` to `build-depends`** — precedent at `libs/wire-api/src/Wire/API/User/Scim.hs:53`. Use it for Base32 rather than hand-rolling, but see the padding trap below.
- `ZLocalUser` yields a `Local UserId`, not a `UserId`. `Handler r = ExceptT HttpError (AppT r)` (`services/brig/src/Brig/API/Handler.hs`); the idiom for calling a subsystem from a handler is `lift . liftSem $ …`.

### Traps

1. **Base32 padding.** `convertToBase Base32` emits `=`-padded output; our wire format is *unpadded*. Encode with a trailing `Text.dropWhileEnd (== '=')`, and on decode re-pad to a multiple of 8 before calling `convertFromBase`. Also uppercase the input before decoding, so the "accept lowercase" rule is actually implemented. Reject input that *arrives* padded — that is a deliberate strictness decision from `TOTP 01`, not an accident.
2. **Adding an `Id` tag is a five-place ritual** in `libs/types-common/src/Data/Id.hs`: the `IdTag` constructor (`:115`), a case in `idTagName`, a `KnownIdTag` instance, the `type TOTPCredentialId = Id 'TOTPCredential` alias, *and* the module export list. Miss one and the error message points somewhere else entirely. `UserGroupId` is the template — grep for it and copy all five.
3. **`TOTPClientClockSkew` wants a `RichError` body** carrying `server_time` (§4). Do not build that here; ship it as a plain `StaticError` like its neighbours and let `TOTP 10` upgrade it. It is a rabbit hole and the mock never throws it.
4. The redacting `Show` on `TOTPSecret`/`TOTPCode`/`RecoveryCode` means test failure output is unhelpful by design. That is the correct trade; do not "temporarily" derive `Show` to debug and then forget.

---

## Scope

```
libs/types-common/src/Data/Id.hs                      -- TOTPCredential phantom tag       (anchor :115)
libs/wire-api/src/Wire/API/Error/Brig.hs              -- BrigError constructors + MapError (anchor :23)
libs/wire-api/src/Wire/API/User/Auth/TOTP.hs          -- types + schemas + Arbitrary        (new)
libs/wire-api/src/Wire/API/Routes/Public/Brig/TOTP.hs -- servant API + OpenAPI docs         (new)
libs/wire-api/src/Wire/API/Routes/Public/Brig.hs      -- splice TOTPAPI into BrigAPI        (anchor :89)
services/brig/src/Brig/API/TOTP/Mock.hs               -- the mock handlers                  (new, deleted by TOTP 07)
services/brig/src/Brig/API/Public.hs                  -- splice totpAPI into servantSitemap (anchor :418, :461)
services/brig/brig.cabal                              -- QuickCheck into the library stanza
charts/nginz/values.yaml                              -- public routes                      (anchor :230)
services/nginz/integration-test/conf/nginz/nginx.conf -- same routes, for the nginz test    (anchor :258)
integration/test/Test/TOTP.hs                         -- integration tests                  (new)
libs/wire-api/test/unit/…, libs/wire-api/test/golden/… -- roundtrip + golden
changelog.d/1-api-changes/<ticket-id>                 -- one line
```

`UserGroupAPI` (`Public/Brig.hs:329`) and `userGroupAPI` (`Public.hs:461`) are the structural template for both splices — a recent, versioned, `ZLocalUser` route group with its handler block. Copy its shape.

Explicitly **not** in scope: `services/brig/src/Brig/Options.hs`. See §5.

### 1. Data types

The full §2 set, because clients need the whole shape and a half-shape is worse than none:

| type | notes for the mock |
|---|---|
| `TOTPCredentialId = Id TOTPCredential` | new phantom tag in `Data.Id`, cf. `UserGroupId` |
| `TOTPSecret` | JSON: unpadded Base32 (RFC 4648). `Arbitrary` must emit **valid** Base32 — see below |
| `TOTPCode` | JSON: digit string, parse checks digits-only, length 6–9 |
| `RecoveryCode` | JSON: `xxxx-xxxx-xxxx-xxxx`, Crockford Base32 (`0123456789ABCDEFGHJKMNPQRSTVWXYZ` — no I, L, O, U) |
| `SecondFactorCode = SFTotp \| SFRecovery` | parsed at the JSON boundary; the two shapes are disjoint |
| `TOTPHashAlgorithm = TOTPSHA1 \| TOTPSHA256 \| TOTPSHA512` | JSON is lowercase (`"sha1"`); the `otpauth://` URI spells it uppercase. Two renderings, one type. |
| `TOTPCredentialInfo` | `id_`, `name`, `createdAt`, `activatedAt` — **no** `lastUsedAt` (D13; leave a comment saying so) |
| `NewTOTPCredential` | `name`, `clientTime`, flattened `ReauthBody` |
| `TOTPEnrolment` | `credentialId`, `secret`, `uri`, `algorithm`, `digits`, `period`, `serverTime`, `expiresAt` |
| `ActivateTOTPCredential` | `code`, `clientTime` |
| `TOTPCredentialName`, `TOTPRecoveryCodes` | |
| `ReauthBody` | `password`, `totp_code :: Maybe SecondFactorCode` |

"Flattened" means one JSON object, not a nested `reauth` key: a request has exactly one body. With `Data.Schema` this is an `ObjectSchema SwaggerDoc ReauthBody` that both `ReauthBody`'s own `ToSchema` and `NewTOTPCredential`'s compose in.

All serialisation via `Data.Schema`, with `description` and `example` on **every** field. The redacting-`Show`/unexported-constructor rules for `TOTPSecret`, `TOTPCode` and `RecoveryCode` from `TOTP 01` apply here already — it is much cheaper to get that right now than to retrofit it once twenty call sites exist. Provide exactly one obviously-named escape hatch per secret type (`unsafeRevealTOTPSecret`).

### 1b. Errors

New `BrigError` constructors with their `MapError` instances (`libs/wire-api/src/Wire/API/Error/Brig.hs`), from §4. The mock throws **none** of them; they exist so the routes carry real `CanThrow`s and the OpenAPI document lists them, because that list is the contract clients write their error handling against.

| constructor | HTTP | label |
|---|---|---|
| `TOTPNotFound` | 404 | `totp-not-found` |
| `TOTPAlreadyActivated` | 409 | `totp-already-activated` |
| `TOTPNotActivated` | 409 | `totp-not-activated` |
| `TOTPEnrolmentExpired` | 410 | `totp-enrolment-expired` |
| `TOTPInvalidCode` | 403 | `totp-invalid-code` |
| `TOTPCodeReused` | 403 | `totp-code-reused` |
| `TOTPClientClockSkew` | 409 | `totp-client-clock-skew` |
| `TOTPTooManyCredentials` | 409 | `totp-too-many-credentials` |
| `TOTPRequired` | 403 | `totp-required` |
| `TOTPDisabled` | 403 | `totp-disabled` |

Style to match, from the same file:

```haskell
type instance MapError 'TOTPNotFound = 'StaticError 404 "totp-not-found" "TOTP credential not found"
```

Per-route `CanThrow` sets — these are a judgement call about what the *real* handler will be able to produce, and they are worth arguing with if you disagree, because they are published:

| route | `CanThrow` |
|---|---|
| `totp-enrol` | `BadCredentials`, `TOTPRequired`, `TOTPInvalidCode`, `TOTPCodeReused`, `TOTPClientClockSkew`, `TOTPTooManyCredentials`, `TOTPDisabled`, `RateLimitExceeded` |
| `totp-activate` | `TOTPNotFound`, `TOTPAlreadyActivated`, `TOTPEnrolmentExpired`, `TOTPInvalidCode`, `TOTPClientClockSkew`, `TOTPDisabled`, `RateLimitExceeded` |
| `totp-list` | `TOTPDisabled` |
| `totp-rename` | `TOTPNotFound`, `TOTPDisabled` |
| `totp-delete` | `BadCredentials`, `TOTPNotFound`, `TOTPRequired`, `TOTPInvalidCode`, `TOTPCodeReused`, `TOTPDisabled`, `RateLimitExceeded` |

`BadCredentials` and `RateLimitExceeded` already exist in `BrigError`; do not add them again.

### 2. Routes

`libs/wire-api/src/Wire/API/Routes/Public/Brig/TOTP.hs`, spliced into `BrigAPI`, all `:> From 'V19`, all `ZLocalUser`:

| verb | path | `Named` | body → response |
|---|---|---|---|
| POST | `/self/totp` | `totp-enrol` | `NewTOTPCredential` → 201 `TOTPEnrolment` |
| POST | `/self/totp/:id/activate` | `totp-activate` | `ActivateTOTPCredential` → 200 `TOTPRecoveryCodes` |
| GET | `/self/totp` | `totp-list` | → 200 `[TOTPCredentialInfo]` |
| PUT | `/self/totp/:id` | `totp-rename` | `TOTPCredentialName` → 200 |
| DELETE | `/self/totp/:id` | `totp-delete` | `ReauthBody` → 200 |

`DELETE` is in scope here although `TOTP 07` defers it to `TOTP 12` — the reason for deferring it there is that deleting a second factor must *require* a second factor, and a mock enforces nothing anyway. Clients need the route to exist.

The enrolment response carries `Cache-Control: no-store`, because it is the one response in the set that carries a secret and because the header is part of the route *type*. Pattern to copy: `create-access-token` in `Wire.API.Routes.Public.Brig` (`:1215`), i.e. `WithHeaders '[Header "Cache-Control" CacheControl] (TOTPEnrolment, CacheControl) (Respond 201 … TOTPEnrolment)`; the generic `AsHeaders '[h] a (a, h)` instance in `Wire.API.Routes.MultiVerb:318` covers the plumbing, and `CacheControl` comes from `Network.Wai.Utilities.Headers`. Getting this in now means `TOTP 07` changes a handler, not a signature.

Internal routes (`/i/users/:uid/totp`) are **out of scope**: nothing consumes them until there is real state.

Documentation per D11, in the same commit — the `TOTP 07` checklist applies verbatim (two-step enrolment, the `otpauth://` format field by field, client-renders-the-QR, secret-returned-once, the TTL, what `clientTime` is for). Add one line to each route's `Description` saying the endpoint is currently a mock that returns random data and persists nothing. That line matters more now that the routes are publicly reachable, and it is the only text that gets deleted in `TOTP 07`.

### 3. Arbitrary instances

In the wire-api library next to the types (not in the test suite) so brig can use them. The idiom is `deriving (Arbitrary) via (GenericUniform X)` from `Wire.Arbitrary`; wire-api's library stanza already depends on QuickCheck. Three instances must be hand-written rather than derived, because a client that parses the response will choke otherwise:

- **`TOTPSecret`** — generate 20 random bytes and keep the Base32-encodable invariant, not an arbitrary `ByteString` that fails its own parser on roundtrip. (20 bytes is 160 bits, which is exactly 32 Base32 characters and therefore needs no padding — a convenient coincidence, not something to rely on if you change the length.)
- **`TOTPCode`** — exactly 6 digits, leading zeros allowed.
- **`RecoveryCode`** — the real `xxxx-xxxx-xxxx-xxxx` Crockford shape.

Everything else can be `GenericUniform`. These three are needed by the Aeson roundtrip tests regardless, so this is not mock-only work. Whatever strictness the parsers get, the generators must satisfy it — the roundtrip test is what catches a mismatch, so write both halves together.

### 4. Mock handlers

`Brig/API/TOTP/Mock.hs`, one `totpAPI :: ServerT TOTPAPI (Handler r)` block, spliced next to `userGroupAPI` (`Public.hs:461`). The body of every handler is the same shape:

```haskell
-- the whole mock in one line, modulo fixups
arbitraryIO :: (Arbitrary a, MonadIO m) => m a
arbitraryIO = liftIO (QC.generate arbitrary)
```

Per route:

- `totp-enrol` → `arbitraryIO @TOTPEnrolment`, then **fix up three fields** (below). 201, `Cache-Control: no-store`.
- `totp-activate` → `arbitraryIO @TOTPRecoveryCodes`, resized to exactly 10 codes.
- `totp-list` → `QC.generate (resize 3 arbitrary) :: IO [TOTPCredentialInfo]` — a list of 0–3, so clients exercise both the empty and the populated rendering.
- `totp-rename`, `totp-delete` → `pure ()`, 200.

Request bodies are parsed (that is half the value of the mock — clients find out immediately if they send the wrong shape) and then ignored. `ReauthBody` is not checked; no password, no code, no 403.

**The three fixups**, because a purely random `TOTPEnrolment` is actively misleading:

1. `uri` must be a real `otpauth://totp/Wire:<label>?secret=<secret>&issuer=Wire&algorithm=SHA1&digits=6&period=30`, built from the *same* `secret` field in the same response, with `<label>` percent-encoded. A client that scans the QR it rendered must get an authenticator entry that is at least self-consistent. Random `Text` here would silently train clients against a URI format we then change. Use the caller's handle for `<label>` if `UserSubsystem.getSelfProfile` gives one and the user id otherwise — a QR that shows the developer their own handle is worth the one effect dependency. Hardcode `issuer=Wire`; making it the backend domain is `TOTP 06`'s config work.
2. `serverTime = now`, `expiresAt = now + 15min`. Random timestamps make every client's expiry logic look broken.
3. `algorithm = TOTPSHA1`, `digits = 6`, `period = 30` — the defaults, not a random draw, for the same reason.

Everything else stays random on purpose: random `credentialId`s and `name`s make it obvious to the client developer that this is not persisted state.

Note for `brig.cabal`: `QuickCheck` is currently a dependency of the `brig-integration` executable only, not of the library. Add it to the library stanza's `build-depends`. It is already in the transitive closure via wire-api, so this costs nothing in build time, and it is the one dependency line that disappears again with the mock module.

### 5. Gating: the API version, and nothing else

**One gate: `From 'V19`.** `isDevelopmentVersion V19 = True` (`Wire.API.Routes.Version:333`), and `charts/wire-server/values.yaml:1280` ships `setDisabledAPIVersions: [development]`, so on a default deployment the whole of V19 — these endpoints included — answers `404 unsupported-version` (`Wire.API.Routes.Version.Wai:47`). A site that wants the mock drops `development` from that list. That is a decision an operator makes once, for the backend, with a well-understood blast radius, and it is already the mechanism we use for every unfinished API.

**No `totp.enabled` config knob.** An earlier draft of this ticket added a minimal `totp` section to `Brig.Options` purely so the mock could be switched off. That is a second gate with the same meaning as the first, and it costs a config option in `Brig.Options`, `charts/brig/values.yaml`, the configmap template, `hack/helm_vars/`, and `brig.integration.yaml` — all of which `TOTP 06` would then have to rework when the knob acquires its real siblings. Introduce the option in `TOTP 06`, where it has something to configure.

A consequence worth stating plainly: there is no way to disable the mock on a backend that has development versions enabled, short of redeploying with them disabled. That is acceptable precisely because the endpoints do nothing — they read no state, write no state, and leak nothing a random number generator does not already know.

**Routed through nginz.** The paths go into `charts/nginz/values.yaml` alongside the other `/self/...` entries (`:230` ff.):

```yaml
    - path: /self/totp$
      envs:
      - all
    - path: /self/totp/([^/]*)$
      envs:
      - all
    - path: /self/totp/([^/]*)/activate$
      envs:
      - all
```

Two things to get right:

- **No `oauth_scope` / `oauth_scopes` key.** Second-factor management must require a real user session, not a third-party OAuth token — an OAuth app being able to enrol or remove a user's second factor would defeat the point. The neighbouring `/self$` entry has `oauth_scopes: ["read:self"]`; do not copy it. This matters today even though the mock enforces nothing, because the entry we ship now is the entry that will still be there when the handlers become real.
- The chart's `/self$` entry is anchored, so it does not shadow these; the surrounding `/self/<x>` entries are unanchored prefixes but none of them is a prefix of `/self/totp`. Confirm rather than assume — nginz matches on path, not method, and the template (`charts/nginz/templates/conf/_nginx.conf.tpl:243`) emits one `location ~* ^(/v[0-9]+)?<path>` per entry, in values-file order.

The integration-test nginz is a **separate, hand-maintained copy** (`services/nginz/integration-test/conf/nginz/nginx.conf`) and it does *not* mirror the chart: it has a single unanchored `location ~* ^(/v[0-9]+)?/self` block at `:258` carrying `oauth_scope self`. So `/self/totp` would already reach brig there, with the wrong OAuth semantics and without proving anything about the chart. Add explicit `/self/totp` locations **above** that block (nginx regex locations are first-match-wins in file order) so the integration test exercises the configuration we actually intend.

This is the half of `TOTP 08` that moves here. What stays in `TOTP 08` is the `docs/` page, which describes a feature that does not exist yet.

## Acceptance criteria

- `make c package=wire-api` and `make c package=brig` clean.
- `integration/test/Test/Swagger.hs` green, with the five new endpoints in the document. (It also runs the `vacuum` OpenAPI linter, so a malformed schema fails there rather than in review.)
- All new types in `libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs`; `TOTPCredentialId` in the `HttpApiData` roundtrip.
- Golden JSON for `TOTPEnrolment`, `TOTPCredentialInfo`, `TOTPRecoveryCodes`, `ReauthBody` (manual golden modules under `libs/wire-api/test/golden/Test/Wire/API/Golden/Manual/`, registered in `Manual.hs`; `App.hs` there is a recent template).
- Negative parse tests: padded Base32, malformed Base32, non-digit `TOTPCode`, wrong-length `TOTPCode`, a recovery code in the wrong shape, and a `SecondFactorCode` that parses as exactly one of its two branches and never both. The mock's main job is telling clients their request is wrong, so the parsers are the part worth testing.
- A test asserting `show` of a `TOTPSecret`, a `TOTPCode` and a `RecoveryCode` leaks nothing.
- One integration test per route, asserting **only** that it answers with the documented status and a body that parses as the documented type — a property test over ~100 generated responses is the honest version of this, since that is exactly what the handler does.
- A test that the `uri` field of an enrolment response parses as an `otpauth://` URI and that its `secret` parameter equals the response's `secret` field. This is the one piece of mock behaviour worth pinning down, because it is the one clients will build on.
- The enrolment response carries `Cache-Control: no-store`.
- **Version guard:** the same request at `V18` → `404`. This is now the only gate, so it is the test that must not be skipped.
- **nginz:** at least one route exercised **through nginz** rather than against brig directly, covering both an authenticated success and an unauthenticated `401`. Use `rawBaseRequest <user> Nginz Versioned <path>`; `integration/test/Test/Auth.hs:47` is the precedent. A typo in the chart is otherwise invisible to CI, and route-exposure changes are exactly the kind that get merged broken.
- `changelog.d/1-api-changes/<ticket-id>` entry, saying plainly that the endpoints are a mock and that they live on a development API version.

## Notes

- **No state.** `GET /self/totp` does not return what you just enrolled, and activating twice succeeds twice. Resist adding an `IORef (Map UserId [...])` "since it is five lines" — it is five lines plus a semantics discussion about what the mock promises, and the moment it has state, clients start depending on behaviour that the real implementation will contradict (expiry, replay, activation rules). If stateful mocking turns out to be genuinely needed, that is a separate ticket with its own acceptance criteria, not a drive-by.
- **No deterministic error triggers** either (no "code `000000` → 403"). Same reasoning: the first magic value invites a second. Clients test their error paths against the OpenAPI document and the error table; the mock's job is the happy path.
- The mock handler module is **deleted** by `TOTP 07`, not grown into the real one. Keeping it in its own file (`Brig/API/TOTP/Mock.hs`) rather than inline in `Public.hs` makes that deletion a `git rm`.
- Tell the client teams, in the same message as "the endpoints are up", that the backend must have development versions enabled and that the paths are under `/v19`. Otherwise the first report back will be a 404 that looks like our bug.
- Blocks nothing and is blocked by nothing. `TOTP 01`'s types are produced here, so `TOTP 01` either becomes this ticket or shrinks to "review and harden what the mock shipped". `TOTP 08` loses its nginz half and keeps the docs page.
