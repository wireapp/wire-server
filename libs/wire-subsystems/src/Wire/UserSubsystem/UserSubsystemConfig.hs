module Wire.UserSubsystem.UserSubsystemConfig where

import Imports
import Util.Timeout
import Wire.API.User
import Wire.Arbitrary

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultLocale :: Locale,
    searchSameTeamOnly :: Bool,
    maxTeamSize :: Word32,
    activationCodeTimeout :: Timeout
  }
  deriving (Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserSubsystemConfig)
