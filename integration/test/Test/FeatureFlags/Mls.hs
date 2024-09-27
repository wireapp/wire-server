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
      let defCfg =
            object
              [ "lockStatus" .= "unlocked",
                "status" .= "disabled",
                "ttl" .= "unlimited",
                "config"
                  .= object
                    [ "protocolToggleUsers" .= ([] :: [String]),
                      "defaultProtocol" .= "proteus",
                      "supportedProtocols" .= ["proteus", "mls"],
                      "allowedCipherSuites" .= ([1] :: [Int]),
                      "defaultCipherSuite" .= toJSON (1 :: Int)
                    ]
              ]
      checkPatch domain "mls" True defCfg $ object ["lockStatus" .= "locked"]
      checkPatch domain "mls" True defCfg $ object ["status" .= "enabled"]
      checkPatch domain "mls" True defCfg
        $ object ["lockStatus" .= "locked", "status" .= "enabled"]
      checkPatch domain "mls" True defCfg
        $ object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "protocolToggleUsers" .= ([] :: [String]),
                  "defaultProtocol" .= "mls",
                  "supportedProtocols" .= ["proteus", "mls"],
                  "allowedCipherSuites" .= ([1] :: [Int]),
                  "defaultCipherSuite" .= toJSON (1 :: Int)
                ]
          ]
      checkPatch domain "mls" True defCfg
        $ object
          [ "config"
              .= object
                [ "protocolToggleUsers" .= ([] :: [String]),
                  "defaultProtocol" .= "mls",
                  "supportedProtocols" .= ["proteus", "mls"],
                  "allowedCipherSuites" .= ([1] :: [Int]),
                  "defaultCipherSuite" .= toJSON (1 :: Int)
                ]
          ]

mlsDefaultConfig :: Value
mlsDefaultConfig =
  object
    [ "protocolToggleUsers" .= ([] :: [String]),
      "defaultProtocol" .= "proteus",
      "supportedProtocols" .= ["proteus", "mls"],
      "allowedCipherSuites" .= ([1] :: [Int]),
      "defaultCipherSuite" .= toJSON (1 :: Int)
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
            "defaultCipherSuite" .= toJSON (1 :: Int)
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
            "defaultCipherSuite" .= toJSON (1 :: Int)
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
            "defaultCipherSuite" .= toJSON (1 :: Int)
          ]
    ]
