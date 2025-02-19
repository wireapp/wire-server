module Test.FeatureFlags.Mls where

import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testMls :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testMls table access =
  do
    user <- randomUser OwnDomain def
    uid <- asString $ user %. "id"
    mkFeatureTests "mls"
      & addUpdate (mls1 uid)
      & addUpdate mls2
      & addInvalidUpdate mlsInvalidConfig
      & setTable table
      & runFeatureTests OwnDomain access

testMlsPatch :: (HasCallStack) => FeatureTable -> App ()
testMlsPatch table = do
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
      checkPatchWithTable table domain "mls" $ object ["lockStatus" .= "locked"]
      checkPatchWithTable table domain "mls" $ object ["status" .= "enabled"]
      checkPatchWithTable table domain "mls"
        $ object ["lockStatus" .= "locked", "status" .= "enabled"]
      checkPatchWithTable table domain "mls"
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
      checkPatchWithTable table domain "mls"
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

testMlsReadOnly :: (HasCallStack) => APIAccess -> App ()
testMlsReadOnly access =
  runFeatureTestsReadOnly OwnDomain access
    $ mkFeatureTests "mls"
    & addUpdate mls2

testPatchMlsReadOnly :: (HasCallStack) => App ()
testPatchMlsReadOnly = checkPatchReadOnly OwnDomain "mls" mls2

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
