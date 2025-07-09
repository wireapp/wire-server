module Wire.UserSubsystem.UserSubsystemConfig where

import Data.Default
import Data.Domain
import Data.HashSet
import Data.LanguageCodes
import Imports
import Util.Timeout
import Wire.API.User
import Wire.Arbitrary

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultLocale :: Locale,
    searchSameTeamOnly :: Bool,
    maxTeamSize :: Word32,
    activationCodeTimeout :: Timeout,
    blockedDomains :: HashSet Domain
  }
  deriving (Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserSubsystemConfig)

-- | Some defaults to simplify test setups - Not meant for usage in production
instance Default UserSubsystemConfig where
  def =
    UserSubsystemConfig
      { emailVisibilityConfig = EmailVisibleIfOnSameTeam (),
        defaultLocale =
          Locale
            { lLanguage = Language EN,
              lCountry = Nothing
            },
        searchSameTeamOnly = False,
        maxTeamSize = 100,
        activationCodeTimeout = Timeout . fromIntegral @Word $ 24 * 3600,
        blockedDomains = empty
      }
