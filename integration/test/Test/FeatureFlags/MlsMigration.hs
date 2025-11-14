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
      "finaliseRegardlessAfter" .= "2029-10-17T00:00:00Z"
    ]

mlsMigrationConfig1 :: Value
mlsMigrationConfig1 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "startTime" .= "2029-05-16T10:11:12.123Z",
            "finaliseRegardlessAfter" .= "2030-10-17T00:00:00Z"
          ]
    ]

mlsMigrationConfig2 :: Value
mlsMigrationConfig2 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "startTime" .= "2030-05-16T10:11:12.123Z",
            "finaliseRegardlessAfter" .= "2031-10-17T00:00:00Z"
          ]
    ]
