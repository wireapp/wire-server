module Test.FeatureFlags.Channels where

import Test.FeatureFlags.Util
import Testlib.Prelude

testChannels :: (HasCallStack) => APIAccess -> App ()
testChannels access =
  mkFeatureTests "channels"
    & addUpdate validConfig
    & addInvalidUpdate invalidConfig
    & runFeatureTests OwnDomain access

validConfig :: Value
validConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "allowed_to_create_channels" .= "everyone",
            "allowed_to_open_channels" .= "everyone"
          ]
    ]

invalidConfig :: Value
invalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "allowed_to_create_channels" .= "everyone",
            "allowed_to_open_channels" .= "INVALID"
          ]
    ]

testPatchChannels :: (HasCallStack) => App ()
testPatchChannels = do
  checkPatch OwnDomain "channels"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "channels"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "channels"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "channels"
    $ object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "allowed_to_create_channels" .= "admins",
              "allowed_to_open_channels" .= "everyone"
            ]
      ]
