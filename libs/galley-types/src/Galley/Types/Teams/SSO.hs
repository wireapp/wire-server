{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Galley.Types.Teams.SSO where

import Imports
import Data.Aeson
import Data.Json.Util

import qualified Data.Text as T

data SSOStatus = SSODisabled | SSOEnabled
   deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON SSOStatus where
    toJSON SSOEnabled  = "enabled"
    toJSON SSODisabled = "disabled"

instance FromJSON SSOStatus where
    parseJSON = withText "SSOStatus" $ \case
      "enabled"  -> pure SSOEnabled
      "disabled" -> pure SSODisabled
      x -> fail $ "unexpected status type: " <> T.unpack x

data SSOTeamConfig = SSOTeamConfig
    { ssoTeamConfigStatus :: !SSOStatus
    }
  deriving stock (Eq, Show, Generic)

instance ToJSON SSOTeamConfig where
    toJSON s = object
        $ "status" .= ssoTeamConfigStatus s
        # []

instance FromJSON SSOTeamConfig where
    parseJSON = withObject "SSOTeamConfig" $ \o ->
        SSOTeamConfig <$> o .: "status"
