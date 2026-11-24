# Backup API Slice 1: automaticBackups Feature Flag Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add team feature `automaticBackups` (config `enforcedSchedule` optional, `enforced`, `serverOnly`) with public get/put, internal galley routes, stern, helm, docs, changelog, unit + integration tests. Backend doesn't act on the flag yet (later slices read it).

**Architecture:** Barbies-style config in `Wire.API.Team.Feature`, appended after `TotpConfig` in `Features`. Everything else is generic feature machinery (DB store, TTL handling, all-features endpoints). Wiring copies TOTP commit `90afdb712`. Optional enum field copies `EnforceFileDownloadLocationConfig` (`NestedMaybe`); enum copies `PreventAdminlessGroupsPromotionStrategy`.

**Tech Stack:** Haskell, schema-profunctor, barbies, servant, tasty/HUnit/QuickCheck, integration testlib, helm.

**Spec:** docs/superpowers/specs/2026-11-24-backup-api-design.md

## Global Constraints

- Feature symbol: `"automaticBackups"` (camelCase, exactly the RFC route segment)
- Config JSON keys: `enforcedSchedule`, `enforced`, `serverOnly`
- `enforcedSchedule` values: `daily` | `weekly` | `monthly` (lowercase only); absent = no enforced schedule; key omitted from JSON when absent (never `null`); default absent
- `enforced` default `false`; `serverOnly` default `false`; both required in request `config` (like TOTP)
- Code default: status `disabled`, lockStatus `unlocked`
- Helm + `galley.integration.yaml` default: status `disabled`, lockStatus `locked`
- Feature routes unversioned (no `From 'V20`), like TOTP
- `ttl` in PUT parsed and ignored; responses always `"ttl": "unlimited"` (generic `Feature`/`LockableFeature` schema, no new code)
- Don't touch `services/brig/docs/swagger-v*.json` (finalized snapshots) or `publication/` (release time)
- One commit for the whole slice (end of Task 3)

## Review Focus

1. **`null` instead of omitted `enforcedSchedule`.** Roundtrip tests pass either way. Test: Task 1 `absent schedule is omitted, not null`; Task 3 `defAllFeatures` entry without the key (exact match).
2. **Clearing a schedule doesn't stick.** DB layer (`Covered Maybe`) must store absent as `null` so it overrides a server default that sets a schedule; PUT without schedule after PUT with schedule must drop it. Test: Task 1 `DB: stored absent schedule overrides server default`; Task 3 update order (with schedule, then without) + final "unlock → last update" check.
3. **Wrong-case / unknown schedule accepted.** Test: Task 1 `unknown or wrong-case schedule rejected`; Task 3 invalid updates `"hourly"`, `"Daily"`.
4. **TTL echoed or persisted.** `setFlag` strips `ttl`, so generic tests never send one. Test: Task 3 `testAutomaticBackupsTtlIgnored`.
5. **Default drift (code unlocked vs helm locked).** No integration test sees the code default (yaml overrides it). Test: Task 1 `default is disabled and unlocked`; Task 3 `defAllFeatures` pins yaml/helm default.

---

### Task 1: wire-api type, defaults, unit tests (no commit)

**Files:**
- Modify: `libs/wire-api/src/Wire/API/Team/Feature.hs`
  - export list after `TotpConfig,` (~line 127)
  - `FeatureSingleton` GADT after `FeatureSingletonTotpConfig` (~line 300)
  - new block after the TOTP block (after `IsFeatureConfig TotpConfig`, before `-- BackgroundEffects Feature` ~line 2495)
  - `type Features` append after `TotpConfig` (~line 2626)
- Modify: `libs/wire-api/src/Wire/API/Team/FeatureFlags.hs` after `FeatureDefaults TotpConfig` (~line 448)
- Create: `libs/wire-api/test/unit/Test/Wire/API/Team/Feature.hs`
- Modify: `libs/wire-api/test/unit/Test/Wire/API/Run.hs` (import + `Team.Feature.tests`)
- Modify: `libs/wire-api/wire-api.cabal` test-suite `wire-api-tests` other-modules (~line 743, alphabetical: `Test.Wire.API.Team.Export`, `Test.Wire.API.Team.Feature`, `Test.Wire.API.Team.Member`)
- Modify: `libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs` (~line 258)

**Interfaces:**
- Consumes: `Wear`, `Covered`, `Bare`, `FieldF`/`fieldF`, `NestedMaybe`/`nestedMaybeField` (`Wire.API.Team.Feature.Profunctor`), `BarbieFeature`, `ParseDbFeature (parseDbConfig, serialiseDbConfig)`, `DbConfig`, `defUnlockedFeature`, `GenericUniform`, `RenderableTypeName`.
- Produces (exported from `Wire.API.Team.Feature`):
  - `data BackupSchedule = BackupScheduleDaily | BackupScheduleWeekly | BackupScheduleMonthly` (`Eq, Show, Generic`, `ToJSON/FromJSON/S.ToSchema via Schema`, `Arbitrary via GenericUniform`)
  - `data AutomaticBackupsConfigB t f = AutomaticBackupsConfig { enforcedSchedule :: Wear t f (Maybe BackupSchedule), enforced :: Wear t f Bool, serverOnly :: Wear t f Bool }`
  - `type AutomaticBackupsConfig = AutomaticBackupsConfigB Bare Identity`
  - `FeatureSingletonAutomaticBackupsConfig :: FeatureSingleton AutomaticBackupsConfig`
  - `type FeatureSymbol AutomaticBackupsConfig = "automaticBackups"`
  - `newtype instance FeatureDefaults AutomaticBackupsConfig = AutomaticBackupsDefaults (LockableFeature AutomaticBackupsConfig)` (`Wire.API.Team.FeatureFlags`)

- [ ] **Step 1: Write failing unit tests**

`libs/wire-api/test/unit/Test/Wire/API/Team/Feature.hs` (license header as in `Team/Member.hs`):

```haskell
module Test.Wire.API.Team.Feature (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (parseEither)
import Data.Default
import Data.Schema (schemaParseJSON, schemaToJSON)
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Team.Feature

tests :: TestTree
tests =
  testGroup
    "Wire.API.Team.Feature.AutomaticBackups"
    [ testCase "default is disabled and unlocked" $
        def @(LockableFeature AutomaticBackupsConfig)
          @?= LockableFeature FeatureStatusDisabled LockStatusUnlocked (AutomaticBackupsConfig Nothing False False),
      testCase "absent schedule is omitted, not null" $
        schemaToJSON (def @AutomaticBackupsConfig)
          @?= object ["enforced" .= False, "serverOnly" .= False],
      testCase "schedule is encoded lowercase" $
        schemaToJSON (AutomaticBackupsConfig (Just BackupScheduleMonthly) True True)
          @?= object ["enforcedSchedule" .= ("monthly" :: Text), "enforced" .= True, "serverOnly" .= True],
      testCase "unknown or wrong-case schedule rejected" $
        for_ ["hourly", "Daily", ""] $ \s ->
          assertBool (show s) . isLeft $
            parseEither (schemaParseJSON @AutomaticBackupsConfig) (cfgJson (String s)),
      testCase "null schedule parses as absent" $
        parseEither schemaParseJSON (cfgJson Null) @?= Right (AutomaticBackupsConfig Nothing False False),
      testCase "DB: stored absent schedule overrides server default" $ do
        let serverDefault = AutomaticBackupsConfig (Just BackupScheduleWeekly) False False
            stored = serialiseDbConfig (AutomaticBackupsConfig Nothing True False)
        (($ serverDefault) <$> parseEither parseDbConfig stored)
          @?= Right (AutomaticBackupsConfig Nothing True False)
    ]
  where
    cfgJson s = object ["enforcedSchedule" .= s, "enforced" .= False, "serverOnly" .= False]
```

Bare configs have no `ToJSON`/`FromJSON` instances (only `ToSchema` via `BarbieFeature`), hence `schemaToJSON`/`schemaParseJSON`. Don't add JSON instances to the config.

Register in `Run.hs` (`import Test.Wire.API.Team.Feature qualified as Team.Feature`, add `Team.Feature.tests` next to `Team.Member.tests`) and in `wire-api.cabal`.

`Aeson.hs`, after `testRoundTrip @(Team.Feature.Feature Team.Feature.LegalholdConfig)`:

```haskell
      testRoundTrip @(Team.Feature.LockableFeature Team.Feature.AutomaticBackupsConfig),
      testRoundTrip @(Team.Feature.Feature Team.Feature.AutomaticBackupsConfig),
```

- [ ] **Step 2: Run, expect failure**

`cabal test wire-api-tests --test-options='-p AutomaticBackups' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error `Not in scope: type constructor or class 'AutomaticBackupsConfig'`.

- [ ] **Step 3: Implement**

`Feature.hs`: exports `BackupSchedule (..)`, `AutomaticBackupsConfigB (..)`, `AutomaticBackupsConfig`. Singleton constructor. Enum, modelled on `PreventAdminlessGroupsPromotionStrategy`:

```haskell
instance ToSchema BackupSchedule where
  schema =
    enum @Text $
      mconcat
        [ element "daily" BackupScheduleDaily,
          element "weekly" BackupScheduleWeekly,
          element "monthly" BackupScheduleMonthly
        ]
```

Config block = TOTP block with these differences:
- `deriving instance` `Eq`/`Show` for `AutomaticBackupsConfig`, plus `FunctorB`/`ApplicativeB`/`TraversableB (AutomaticBackupsConfigB Covered)`
- `Arbitrary` via `GenericUniform AutomaticBackupsConfig`; `ParseDbFeature` and `ToSchema` via `BarbieFeature AutomaticBackupsConfigB`
- `instance Default AutomaticBackupsConfig where def = AutomaticBackupsConfig Nothing False False`
- schema:

```haskell
instance (Typeable f, FieldF f, NestedMaybe f) => ToSchema (AutomaticBackupsConfigB Covered f) where
  schema =
    object $
      AutomaticBackupsConfig
        <$> enforcedSchedule .= nestedMaybeField "enforcedSchedule" (unnamed schema)
        <*> enforced .= fieldF "enforced" schema
        <*> serverOnly .= fieldF "serverOnly" schema
```

- `instance Default (LockableFeature AutomaticBackupsConfig) where def = defUnlockedFeature {status = FeatureStatusDisabled}` (**not** TOTP's `defLockedFeature`)
- `ToObjectSchema`: `objectSchema = field "config" schema`
- `IsFeatureConfig`: symbol `"automaticBackups"`, `featureSingleton = FeatureSingletonAutomaticBackupsConfig`
- Haddock: flag is read by clients; backend uses only `status` (from slice 2); `serverOnly` not checked by server.

Append `AutomaticBackupsConfig` to `type Features` after `TotpConfig`.

`FeatureFlags.hs`: `FeatureDefaults AutomaticBackupsConfig` = copy of the `TotpConfig` instance (`AutomaticBackupsDefaults`, `OptionalField AutomaticBackupsConfig`).

If `enforced`/`serverOnly` record selectors clash with something in scope (none in `Feature.hs` today), fix at the use site, not by renaming JSON keys.

- [ ] **Step 4: Run, expect pass**

`make c package=wire-api test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all `wire-api-tests` and golden tests pass (incl. `AllTeamFeatures` roundtrip, `FeatureFlags accepts its canonical JSON representation`), no warnings.

No commit. Downstream packages (galley, wire-subsystems) don't build yet because the positional `allFeaturesAPI` is now one handler short. Task 2 fixes that.

---

### Task 2: galley, wire-subsystems, stern, config, docs, changelog (no commit)

**Files (all: append right after the matching `TotpConfig` / `mfa-totp` entry):**
- Modify: `libs/wire-subsystems/src/Wire/FeaturesConfigSubsystem/Types.hs` (~line 173): `instance GetFeatureConfig AutomaticBackupsConfig`
- Modify: `libs/wire-api/src/Wire/API/Routes/Public/Galley/Feature.hs` (~line 107): `:<|> FeatureAPIGetPut AutomaticBackupsConfig`
- Modify: `libs/wire-api/src/Wire/API/Routes/Internal/Galley.hs` (~line 124): `:<|> IFeatureStatusLockStatusPut AutomaticBackupsConfig`
- Modify: `services/galley/src/Galley/API/Internal.hs`: one more `<@> featureAPI1Full` at end of `allFeaturesAPI` (lines 266–301, positional); `<@> mkNamedAPI @'("ilock", AutomaticBackupsConfig) (updateLockStatus @AutomaticBackupsConfig)` after line 330
- Modify: `services/galley/src/Galley/API/Public/Feature.hs` (~line 90): `<@> featureAPIGetPut @AutomaticBackupsConfig`
- Modify: `services/galley/src/Galley/API/Teams/Features.hs` (~line 558): `instance SetFeatureConfig AutomaticBackupsConfig`
- Modify: `tools/stern/src/Stern/API/Routes.hs` (after lines 380 and 538) and `tools/stern/src/Stern/API.hs` (after lines 207 and 248)
- Modify: `charts/wire-server/values.yaml` (after `mfa-totp` block, ~line 287)
- Modify: `charts/wire-server/templates/galley/configmap.yaml` (after `mfa-totp` block, ~line 254)
- Modify: `services/galley/galley.integration.yaml` (after `mfa-totp` block, ~line 175)
- Modify: `docs/src/developer/reference/config-options.md` (new `### Automatic backups` after `### TOTP`, before `### Federation Domain`, ~line 626)
- Create: `changelog.d/2-features/WPB-25648-automatic-backups-feature-flag`

**Interfaces:**
- Consumes: everything Task 1 produces; `FeatureAPIGetPut`, `IFeatureStatusLockStatusPut`, `featureAPIGetPut`, `featureAPI1Full`, `updateLockStatus`, `MkFeatureGetRoute`/`MkFeaturePutRoute`/`MkFeatureLockUnlockRoute`, `mkFeatureGetRoute`/`mkFeaturePutRoute`/`mkFeatureLockUnlockRoute`.
- Produces: `GET|PUT /teams/:tid/features/automaticBackups`; `GET|PUT|PATCH /i/teams/:tid/features/automaticBackups`; `PUT /i/teams/:tid/features/automaticBackups/(un)locked`; stern routes `get-route-automatic-backups-config`, `put-route-automatic-backups-config`, `lock-unlock-route-automatic-backups-config`.

- [ ] **Step 1: Failing check**

`make c package=galley | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: type error in `allFeaturesAPI` (`IAllFeaturesAPI Features` has one more element than the handler chain) and/or missing `GetFeatureConfig AutomaticBackupsConfig`.

- [ ] **Step 2: Implement wiring** (see Files). Stern uses the full-config put (`MkFeaturePutRoute`/`mkFeaturePutRoute`), like channels/preventAdminlessGroups, not TOTP's status-only put, so support can set the schedule:

```haskell
-- Routes.hs (after put-route-totp-config)
    :<|> Named "get-route-automatic-backups-config" (MkFeatureGetRoute AutomaticBackupsConfig)
    :<|> Named "put-route-automatic-backups-config" (MkFeaturePutRoute AutomaticBackupsConfig)
-- Routes.hs (after lock-unlock-route-totp-config)
    :<|> Named "lock-unlock-route-automatic-backups-config" (MkFeatureLockUnlockRoute AutomaticBackupsConfig)
```

`API.hs`: the matching `Named @"…" (mkFeatureGetRoute @AutomaticBackupsConfig)` / `mkFeaturePutRoute` / `mkFeatureLockUnlockRoute` at the same positions.

- [ ] **Step 3: Config + docs + changelog**

`values.yaml` and `galley.integration.yaml` (indent as neighbours):

```yaml
automaticBackups:
  defaults:
    status: disabled
    lockStatus: locked
    config:
      enforced: false
      serverOnly: false
```

`configmap.yaml` (camelCase key, dot access is fine):

```yaml
        {{- if .settings.featureFlags.automaticBackups }}
        automaticBackups:
          {{- toYaml .settings.featureFlags.automaticBackups | nindent 10 }}
        {{- end }}
```

`hack/helm_vars/wire-server/values.yaml.gotmpl` doesn't override `featureFlags.mfa-totp`, so CI uses the chart default. Leave it untouched for `automaticBackups` as well.

Docs `### Automatic backups`: one paragraph (team feature telling clients whether and how to back up to the server; `enforcedSchedule` optional `daily|weekly|monthly`, omitted = none; `enforced`; `serverOnly` not checked by the server; code default disabled/unlocked, helm default disabled/locked), the yaml example above, and the internal lock-status route line as in TOTP.

Changelog `changelog.d/2-features/WPB-25648-automatic-backups-feature-flag`:
`` Add the `automaticBackups` team feature flag (config: optional `enforcedSchedule` (`daily`, `weekly`, `monthly`), `enforced`, `serverOnly`). Disabled and locked by default (helm). ``

- [ ] **Step 4: Run, expect pass**

```
make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
make c package=galley | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
make c package=stern test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
helm template charts/wire-server | grep -A6 'automaticBackups:'
```

Expected: builds with no warnings; `wire-subsystems-tests` and `stern-tests` green; rendered galley config contains the `automaticBackups` defaults block. (If `helm template` needs extra values, skip it and note that in the hand-off.)

No commit.

---

### Task 3: integration test, quality gates, single commit

**Files:**
- Create: `integration/test/Test/FeatureFlags/AutomaticBackups.hs`
- Modify: `integration/integration.cabal` (~line 142, between `Test.FeatureFlags.AssetAuditLog` and `Test.FeatureFlags.BackgroundEffects`)
- Modify: `integration/test/Test/FeatureFlags/Util.hs`: `defAllFeatures` entry after `"mfa-totp"` (~line 265); `hasExplicitLockStatus "automaticBackups" = True` after line 294
- Modify: `integration/test/Test/Migration/TeamFeatures.hs` (~line 106): append `"automaticBackups"` to `unlockableFeatures`

**Interfaces:**
- Consumes: `mkFeatureTests`, `addUpdate`, `addInvalidUpdate`, `runFeatureTests`, `checkPatch`, `APIAccess`, `createTeam`, `Internal.setTeamFeatureLockStatus`, `Public.setTeamFeatureConfig`, `Public.getTeamFeature`.
- Produces: `testAutomaticBackups :: (HasCallStack) => APIAccess -> App ()`, `testPatchAutomaticBackups :: (HasCallStack) => App ()`, `testAutomaticBackupsTtlIgnored :: (HasCallStack) => App ()`.

- [ ] **Step 1: Write tests**

`Util.hs` `defAllFeatures`:

```haskell
      "automaticBackups"
        .= object
          [ "lockStatus" .= "locked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config" .= object ["enforced" .= False, "serverOnly" .= False]
          ],
```

`AutomaticBackups.hs` (license header as `Totp.hs`; imports `qualified API.Galley as Public`, `qualified API.GalleyInternal as Internal`, `SetupHelpers`, `Test.FeatureFlags.Util`, `Testlib.Prelude`):

```haskell
testAutomaticBackups :: (HasCallStack) => APIAccess -> App ()
testAutomaticBackups access =
  mkFeatureTests "automaticBackups"
    -- order matters: the 2nd update clears the schedule set by the 1st;
    -- runFeatureTests' final check expects the last update to survive lock/unlock
    & addUpdate (cfg [ "enforcedSchedule" .= "daily", "enforced" .= True, "serverOnly" .= True ])
    & addUpdate (cfg [ "enforced" .= False, "serverOnly" .= True ])
    & addInvalidUpdate (cfg [ "enforcedSchedule" .= "hourly", "enforced" .= False, "serverOnly" .= False ])
    & addInvalidUpdate (cfg [ "enforcedSchedule" .= "Daily", "enforced" .= False, "serverOnly" .= False ])
    & addInvalidUpdate (cfg [ "enforced" .= ("yes" :: String), "serverOnly" .= False ])
    & runFeatureTests OwnDomain access
  where
    cfg fields = object ["status" .= "enabled", "config" .= object fields]

testPatchAutomaticBackups :: (HasCallStack) => App ()
testPatchAutomaticBackups = do
  checkPatch OwnDomain "automaticBackups" $ object ["lockStatus" .= "unlocked"]
  checkPatch OwnDomain "automaticBackups" $ object ["lockStatus" .= "unlocked", "status" .= "enabled"]
  checkPatch OwnDomain "automaticBackups" $
    object
      [ "lockStatus" .= "unlocked",
        "config" .= object ["enforcedSchedule" .= "weekly", "enforced" .= True, "serverOnly" .= False]
      ]

testAutomaticBackupsTtlIgnored :: (HasCallStack) => App ()
testAutomaticBackupsTtlIgnored = do
  (owner, tid, _) <- createTeam OwnDomain 0
  Internal.setTeamFeatureLockStatus owner tid "automaticBackups" "unlocked"
  let body =
        object
          [ "status" .= "enabled",
            "config" .= object ["enforcedSchedule" .= "monthly", "enforced" .= False, "serverOnly" .= False],
            "ttl" .= (60 :: Int)
          ]
  bindResponse (Public.setTeamFeatureConfig owner tid "automaticBackups" body) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "ttl" `shouldMatch` "unlimited"
  bindResponse (Public.getTeamFeature owner tid "automaticBackups") $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "ttl" `shouldMatch` "unlimited"
    resp.json %. "config.enforcedSchedule" `shouldMatch` "monthly"
```

The generic tests in `Test/FeatureFlags.hs` (`testAllFeatures`, `testFeatureConfigConsistency`, `testNonMemberAccess` via the `Feature` test cases derived from `defAllFeatures`) pick up the feature automatically.

- [ ] **Step 2: Compile only (never run)**

`make c package=integration | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: builds, no warnings. **The human runs** the integration tests, e.g. `TEST_INCLUDE=AutomaticBackups,testAllFeatures,testTeamFeaturesMigration,testFeatureConfigConsistency make devtest`.

- [ ] **Step 3: Quality gates**

```
make format
make c test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
git status --short
```

Expected: whole project type-checks, no warnings, unit tests green. `make regen-local-nix-derivations` is not needed: no dependency changes (the new test module only adds an `other-modules` line). `git status` shows only the files listed in Tasks 1–3, with no `swagger-v*.json` or `publication/` changes.

- [ ] **Step 4: Commit (only commit of the slice)**

```
git add libs/wire-api/src/Wire/API/Team/Feature.hs \
  libs/wire-api/src/Wire/API/Team/FeatureFlags.hs \
  libs/wire-api/src/Wire/API/Routes/Public/Galley/Feature.hs \
  libs/wire-api/src/Wire/API/Routes/Internal/Galley.hs \
  libs/wire-api/test/unit/Test/Wire/API/Team/Feature.hs \
  libs/wire-api/test/unit/Test/Wire/API/Run.hs \
  libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs \
  libs/wire-api/wire-api.cabal \
  libs/wire-subsystems/src/Wire/FeaturesConfigSubsystem/Types.hs \
  services/galley/src/Galley/API/Internal.hs \
  services/galley/src/Galley/API/Public/Feature.hs \
  services/galley/src/Galley/API/Teams/Features.hs \
  services/galley/galley.integration.yaml \
  tools/stern/src/Stern/API.hs tools/stern/src/Stern/API/Routes.hs \
  charts/wire-server/values.yaml charts/wire-server/templates/galley/configmap.yaml \
  docs/src/developer/reference/config-options.md \
  changelog.d/2-features/WPB-25648-automatic-backups-feature-flag \
  integration/integration.cabal \
  integration/test/Test/FeatureFlags/AutomaticBackups.hs \
  integration/test/Test/FeatureFlags/Util.hs \
  integration/test/Test/Migration/TeamFeatures.hs
git commit -m "Add automaticBackups team feature flag"
```

(Append the attribution trailer required by the session.)

## Unresolved questions

- Stern: full-config put (planned) or TOTP-style status-only put?
- `enforced`/`serverOnly` required in PUT `config` (planned, like TOTP) or optional with default `false`?
- Changelog file name: ticket id known? Otherwise `automatic-backups-feature-flag`.

## Unresolved questions

- None. (`enforced`/`serverOnly` stay mandatory in PUT, decided 2026-11-24.)
