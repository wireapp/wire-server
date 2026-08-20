-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.FeatureFlags.MlsMigration where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import qualified Data.Aeson as A
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testMlsMigration :: (HasCallStack) => APIAccess -> App ()
testMlsMigration access = do
  -- first we have to enable mls
  (owner, tid, _) <- createTeam OwnDomain 0
  void $ Public.setTeamFeatureConfig owner tid "mls" mlsEnable >>= getJSON 200
  mkFeatureTests "mlsMigration"
    & addUpdate mlsMigrationConfig1
    & addUpdate mlsMigrationConfig2
    & setOwner owner
      >>= runFeatureTests OwnDomain access

testMlsMigrationDefaults :: (HasCallStack) => App ()
testMlsMigrationDefaults = do
  withModifiedBackend
    def
      { galleyCfg = setField "settings.featureFlags.mlsMigration.defaults.lockStatus" "unlocked"
      }
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 0
      void
        $ Internal.patchTeamFeature owner tid "mls" (object ["status" .= "enabled"])
        >>= getJSON 200
      feat <- Internal.getTeamFeature owner tid "mlsMigration" >>= getJSON 200
      feat %. "config" `shouldMatch` mlsMigrationDefaultConfig

testMlsMigrationAllowManualMigration :: (HasCallStack) => App ()
testMlsMigrationAllowManualMigration = do
  (owner, tid, _) <- createTeam OwnDomain 0
  void $ Public.setTeamFeatureConfig owner tid "mls" mlsEnable >>= getJSON 200

  getResp0 <- Public.getTeamFeature owner tid "mlsMigration" >>= getJSON 200
  (getResp0 %. "config" %. "allowManualMigration") `shouldMatch` False

  patchResp0 <-
    Internal.patchTeamFeature owner tid "mlsMigration" (object ["lockStatus" .= "unlocked"])
      >>= getJSON 200
  (patchResp0 %. "config" %. "allowManualMigration") `shouldMatch` False

  setResp1 <-
    Public.setTeamFeatureConfig owner tid "mlsMigration" mlsMigrationConfig1
      >>= getJSON 200
  getResp1 <- Public.getTeamFeature owner tid "mlsMigration" >>= getJSON 200
  (getResp1 %. "config" %. "allowManualMigration") `shouldMatch` False
  (getResp1 %. "config") `shouldMatch` (setResp1 %. "config")

  setResp2 <-
    Public.setTeamFeatureConfig owner tid "mlsMigration" mlsMigrationConfig2
      >>= getJSON 200
  getResp2 <- Public.getTeamFeature owner tid "mlsMigration" >>= getJSON 200
  (getResp2 %. "config" %. "allowManualMigration") `shouldMatch` True
  (getResp2 %. "config") `shouldMatch` (setResp2 %. "config")

  let patchWithoutField =
        object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "startTime" .= "2030-01-01T00:00:00Z"
                ]
          ]
  setResp3 <-
    Public.setTeamFeatureConfig owner tid "mlsMigration" patchWithoutField
      >>= getJSON 200
  getResp3 <- Public.getTeamFeature owner tid "mlsMigration" >>= getJSON 200
  (getResp3 %. "config" %. "allowManualMigration") `shouldMatch` False
  (getResp3 %. "config") `shouldMatch` (setResp3 %. "config")

  let patchWithField =
        object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "startTime" .= "2030-01-01T00:00:00Z",
                  "allowManualMigration" .= True
                ]
          ]
  setResp4 <-
    Public.setTeamFeatureConfig owner tid "mlsMigration" patchWithField
      >>= getJSON 200
  getResp4 <- Public.getTeamFeature owner tid "mlsMigration" >>= getJSON 200
  (getResp4 %. "config" %. "allowManualMigration") `shouldMatch` True
  (getResp4 %. "config") `shouldMatch` (setResp4 %. "config")

-- | PUT replaces the whole config; it does not merge omitted fields with the
-- previously stored value. This pins down that assumption so that future
-- schema changes to individual fields (e.g. allowManualMigration) don't
-- accidentally start relying on merge behaviour that doesn't exist.
testMlsMigrationPutDoesNotMergeOmittedFields :: (HasCallStack) => App ()
testMlsMigrationPutDoesNotMergeOmittedFields = do
  (owner, tid, _) <- createTeam OwnDomain 0
  void $ Public.setTeamFeatureConfig owner tid "mls" mlsEnable >>= getJSON 200
  void
    $ Internal.patchTeamFeature owner tid "mlsMigration" (object ["lockStatus" .= "unlocked"])
    >>= getJSON 200

  beforePatch <-
    Public.setTeamFeatureConfig owner tid "mlsMigration" mlsMigrationConfig2
      >>= getJSON 200
  (beforePatch %. "config" %. "finaliseRegardlessAfter") `shouldMatch` "2031-10-17T00:00:00Z"

  let partialConfig =
        object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "startTime" .= "2030-01-01T00:00:00Z"
                ]
          ]
  void
    $ Public.setTeamFeatureConfig owner tid "mlsMigration" partialConfig
    >>= getJSON 200
  afterPatch <- Public.getTeamFeature owner tid "mlsMigration" >>= getJSON 200

  -- omitted fields are dropped, not carried over from mlsMigrationConfig2
  assertFieldMissing afterPatch "config.finaliseRegardlessAfter"

  -- allowManualMigration is always rendered (never omitted); reset to its
  -- default of False, not merged from mlsMigrationConfig2's True
  (afterPatch %. "config" %. "allowManualMigration") `shouldMatch` False

mlsEnableConfig :: Value
mlsEnableConfig =
  object
    [ "protocolToggleUsers" .= ([] :: [String]),
      "defaultProtocol" .= "mls",
      "supportedProtocols" .= ["mls"],
      "allowedCipherSuites" .= ([1] :: [Int]),
      "defaultCipherSuite" .= A.Number 1
    ]

mlsEnable :: Value
mlsEnable =
  object
    [ "status" .= "enabled",
      "config" .= mlsEnableConfig
    ]

mlsMigrationDefaultConfig :: Value
mlsMigrationDefaultConfig =
  object
    [ "startTime" .= "2029-05-16T10:11:12.123Z",
      "finaliseRegardlessAfter" .= "2029-10-17T00:00:00Z",
      "allowManualMigration" .= False
    ]

mlsMigrationConfig1 :: Value
mlsMigrationConfig1 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "startTime" .= "2029-05-16T10:11:12.123Z",
            "finaliseRegardlessAfter" .= "2030-10-17T00:00:00Z",
            "allowManualMigration" .= False
          ]
    ]

mlsMigrationConfig2 :: Value
mlsMigrationConfig2 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "startTime" .= "2030-05-16T10:11:12.123Z",
            "finaliseRegardlessAfter" .= "2031-10-17T00:00:00Z",
            "allowManualMigration" .= True
          ]
    ]
