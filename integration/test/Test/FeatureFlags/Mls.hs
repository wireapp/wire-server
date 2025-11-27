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

module Test.FeatureFlags.Mls where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testMls :: (HasCallStack) => APIAccess -> App ()
testMls access =
  do
    user <- randomUser OwnDomain def
    uid <- asString $ user %. "id"
    mkFeatureTests "mls"
      & addUpdate (mls1 uid)
      & addUpdate mls2
      & addInvalidUpdate mlsInvalidConfig
      & runFeatureTests OwnDomain access

testMlsPatch :: (HasCallStack) => App ()
testMlsPatch = do
  mlsMigrationDefaultConfig <- defAllFeatures %. "mlsMigration.config"
  withModifiedBackend
    def
      { galleyCfg =
          setField
            "settings.featureFlags.mlsMigration.defaults"
            ( object
                [ "lockStatus" .= "locked",
                  "status" .= "disabled",
                  "config" .= mlsMigrationDefaultConfig
                ]
            )
      }
    $ \domain -> do
      checkPatch domain "mls" $ object ["lockStatus" .= "locked"]
      checkPatch domain "mls" $ object ["status" .= "enabled"]
      checkPatch domain "mls"
        $ object ["lockStatus" .= "locked", "status" .= "enabled"]
      checkPatch domain "mls"
        $ object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "protocolToggleUsers" .= ([] :: [String]),
                  "defaultProtocol" .= "mls",
                  "supportedProtocols" .= ["proteus", "mls"],
                  "allowedCipherSuites" .= ([1] :: [Int]),
                  "defaultCipherSuite" .= toJSON (1 :: Int),
                  "groupInfoDiagnostics" .= True
                ]
          ]
      checkPatch domain "mls"
        $ object
          [ "config"
              .= object
                [ "protocolToggleUsers" .= ([] :: [String]),
                  "defaultProtocol" .= "mls",
                  "supportedProtocols" .= ["proteus", "mls"],
                  "allowedCipherSuites" .= ([1] :: [Int]),
                  "defaultCipherSuite" .= toJSON (1 :: Int),
                  "groupInfoDiagnostics" .= True
                ]
          ]

mls1 :: String -> Value
mls1 uid =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "protocolToggleUsers" .= [uid],
            "defaultProtocol" .= "mls",
            "supportedProtocols" .= ["proteus", "mls"],
            "allowedCipherSuites" .= ([1] :: [Int]),
            "defaultCipherSuite" .= toJSON (1 :: Int),
            "groupInfoDiagnostics" .= True
          ]
    ]

mls2 :: Value
mls2 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "protocolToggleUsers" .= ([] :: [String]),
            "defaultProtocol" .= "mls",
            "supportedProtocols" .= ["mls"],
            "allowedCipherSuites" .= ([1] :: [Int]),
            "defaultCipherSuite" .= toJSON (1 :: Int),
            "groupInfoDiagnostics" .= True
          ]
    ]

mlsInvalidConfig :: Value
mlsInvalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "protocolToggleUsers" .= ([] :: [String]),
            "defaultProtocol" .= "mls",
            "supportedProtocols" .= ["proteus"],
            "allowedCipherSuites" .= ([1] :: [Int]),
            "defaultCipherSuite" .= toJSON (1 :: Int),
            "groupInfoDiagnostics" .= True
          ]
    ]
