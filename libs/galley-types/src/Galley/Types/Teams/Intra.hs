{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Galley.Types.Teams.Intra where

import Imports
import Data.Aeson
import Data.Aeson.TH
import Data.Json.Util
import Data.Time (UTCTime)
import Galley.Types.Teams (Team)

import qualified Data.Currency as Currency

data TeamStatus
    = Active
    | PendingDelete
    | Deleted
    | Suspended
    | PendingActive
    deriving (Eq, Show, Generic)

instance ToJSON TeamStatus where
    toJSON Active        = String "active"
    toJSON PendingDelete = String "pending_delete"
    toJSON Deleted       = String "deleted"
    toJSON Suspended     = String "suspended"
    toJSON PendingActive = String "pending_active"

instance FromJSON TeamStatus where
    parseJSON (String "active")         = pure Active
    parseJSON (String "pending_delete") = pure PendingDelete
    parseJSON (String "deleted")        = pure Deleted
    parseJSON (String "suspended")      = pure Suspended
    parseJSON (String "pending_active") = pure PendingActive
    parseJSON other                     = fail $ "Unknown TeamStatus: " <> show other

data TeamData = TeamData
    { tdTeam       :: !Team
    , tdStatus     :: !TeamStatus
    , tdStatusTime :: !(Maybe UTCTime) -- This needs to be a Maybe due to backwards compatibility
    }
    deriving (Eq, Show, Generic)

instance ToJSON TeamData where
    toJSON (TeamData t s st) = object
        $ "team"        .= t
        # "status"      .= s
        # "status_time" .= (toUTCTimeMillis <$> st)
        # []

instance FromJSON TeamData where
    parseJSON = withObject "team-data" $ \o -> do
        TeamData <$> o .:  "team"
                 <*> o .:  "status"
                 <*> o .:? "status_time"

data TeamStatusUpdate = TeamStatusUpdate
    { tuStatus   :: !TeamStatus
    , tuCurrency :: !(Maybe Currency.Alpha)
    -- TODO: Remove Currency selection once billing supports currency changes after team creation
    }
    deriving (Eq, Show, Generic)

instance FromJSON TeamStatusUpdate where
    parseJSON = withObject "team-status-update" $ \o ->
        TeamStatusUpdate <$> o .:  "status"
                         <*> o .:? "currency"

instance ToJSON TeamStatusUpdate where
    toJSON s = object [ "status"   .= tuStatus s
                      , "currency" .= tuCurrency s
                      ]

newtype TeamName = TeamName
    { tnName :: Text }
    deriving (Eq, Show, Generic)

deriveJSON toJSONFieldName ''TeamName
