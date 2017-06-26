{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | API data types used only for intra-environment communication.
-- TODO: Move to Brig.Types.User.Intra / Internal
module Brig.Types.Intra where

import Brig.Types.Connection
import Brig.Types.User
import Control.Monad (mzero)
import Data.Aeson
import Data.Id (TeamId, UserId)
import Data.Json.Util
import Data.Set (Set)

import qualified Data.HashMap.Strict as M
import qualified Data.Text           as Text

-------------------------------------------------------------------------------
-- AccountStatus

data AccountStatus
    = Active
    | Suspended
    | Deleted
    deriving Eq

instance FromJSON AccountStatus where
    parseJSON = withText "account-status" $ \s -> case Text.toLower s of
        "active"    -> pure Active
        "suspended" -> pure Suspended
        "deleted"   -> pure Deleted
        _           -> fail $ "Invalid account status: " ++ Text.unpack s

instance ToJSON AccountStatus where
    toJSON Active    = String "active"
    toJSON Suspended = String "suspended"
    toJSON Deleted   = String "deleted"

newtype AccountStatusUpdate = AccountStatusUpdate
    { suStatus :: AccountStatus }

instance FromJSON AccountStatusUpdate where
    parseJSON = withObject "account-status-update" $ \o ->
        AccountStatusUpdate <$> o .: "status"

instance ToJSON AccountStatusUpdate where
    toJSON s = object ["status" .= suStatus s]

-------------------------------------------------------------------------------
-- ConnectionStatus

data ConnectionStatus = ConnectionStatus
    { csFrom       :: !UserId
    , csTo         :: !UserId
    , csStatus     :: !Relation
    } deriving (Eq, Show)

instance FromJSON ConnectionStatus where
    parseJSON = withObject "connection-status" $ \o ->
        ConnectionStatus <$> o .: "from"
                         <*> o .: "to"
                         <*> o .: "status"

instance ToJSON ConnectionStatus where
    toJSON cs = object
        [ "from"   .= csFrom cs
        , "to"     .= csTo cs
        , "status" .= csStatus cs
        ]

-------------------------------------------------------------------------------
-- UserAccount

-- | A UserAccount is targeted to be used by our \"backoffice\" and represents
-- all the data related to a user in our system, regardless of whether they
-- are active or not, their status, etc.
data UserAccount = UserAccount
    { accountUser       :: !User
    , accountStatus     :: !AccountStatus
    }

instance FromJSON UserAccount where
    parseJSON j@(Object o) = do
        u <- parseJSON j
        s <- o .:  "status"
        return $ UserAccount u s
    parseJSON _ = mzero

instance ToJSON UserAccount where
    toJSON (UserAccount u s) =
        let Object o = toJSON u
        in Object $ M.insert "status" (toJSON s) o

-------------------------------------------------------------------------------
-- UserIds

newtype UserIds = UserIds
    { userIds :: [UserId]
    } deriving (Eq, Show, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- AutoConnect

-- | List of users to establish a 2-way accepted connection for a given user
data AutoConnect = AutoConnect
    { acUsrs :: !(Set UserId)
    } deriving (Eq, Show)

instance FromJSON AutoConnect where
    parseJSON = withObject "AutoConnect" $ \o ->
        AutoConnect <$> o .: "users"

instance ToJSON AutoConnect where
    toJSON ac = object
        [ "users" .= acUsrs ac
        ]

-------------------------------------------------------------------------------
-- UserTeam

-- | A UserTeam is targeted to be used by galley to check the team
-- that the user is bound to
data UserTeam = UserTeam
    { utUser :: UserId
    , utTeam :: Maybe TeamId
    }

instance FromJSON UserTeam where
    parseJSON = withObject "UserTeam" $ \o -> do
        UserTeam <$> o .:  "id"
                 <*> o .:? "team"

instance ToJSON UserTeam where
    toJSON ut = object
        $ "id"   .= utUser ut
        # "team" .= utTeam ut
        # []
