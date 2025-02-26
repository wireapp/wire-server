module Test.FeatureFlags.Channels where

import SetupHelpers
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
