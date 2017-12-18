{-# LANGUAGE OverloadedStrings #-}

module Galley.Types.Teams.Intra where

import Data.Aeson
import Data.Json.Util
import Data.Monoid
import Data.Text (Text)
import Data.Time (UTCTime)
import Galley.Types.Teams (Team)

data TeamStatus
    = Active
    | PendingDelete
    | Deleted
    | Suspended
    | PendingActive
    deriving (Eq, Show)

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

instance ToJSON TeamData where
    toJSON (TeamData t s st) = object
        $ "team"        .= t
        # "status"      .= s
        # "status_time" .= (UTCTimeMillis <$> st)
        # []

instance FromJSON TeamData where
    parseJSON = withObject "team-data" $ \o -> do
        TeamData <$> o .:  "team"
                 <*> o .:  "status"
                 <*> o .:? "status_time"

newtype TeamStatusUpdate = TeamStatusUpdate
    { tuStatus :: TeamStatus }

instance FromJSON TeamStatusUpdate where
    parseJSON = withObject "team-status-update" $ \o ->
        TeamStatusUpdate <$> o .: "status"

instance ToJSON TeamStatusUpdate where
    toJSON s = object ["status" .= tuStatus s]

newtype TeamName = TeamName
    { tnName :: Text }

instance FromJSON TeamName where
    parseJSON = withObject "team-name" $ \o ->
        TeamName <$> o .: "name"

instance ToJSON TeamName where
    toJSON s = object ["name" .= tnName s]
