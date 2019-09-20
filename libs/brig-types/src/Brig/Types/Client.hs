{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}

-- TODO: Move to Brig.Types.User.Client
module Brig.Types.Client
    ( module Brig.Types.Client
    , module C
    , module P
    ) where

import Imports
import Brig.Types.User.Auth (CookieLabel)
import Brig.Types.Common as C
import Brig.Types.Client.Prekey as P
import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.Misc (Location, PlainTextPassword (..))


-- * Data Types:

-- [Note: LegalHold]
--
-- Short feature description:
-- LegalHold is an enterprise feature, enabled on a per-team basis, and within a
-- team on a per-user basis
-- * A LegalHoldClient is a client outside that user's control (but under the
--   control of that team's business)
-- * Users need to click "accept" before a LegalHoldClient is added to their
--   account.
-- * Any user interacting with a user which has a LegalHoldClient will upon
--   first interaction receive a warning, have the option of cancelling the
--   interation, and on an ongoing basis see a visual indication in all
--   conversations where such a device is active.

data ClientType
    = TemporaryClientType
    | PermanentClientType
    | LegalHoldClientType -- see Note [LegalHold]
    deriving (Eq, Ord, Show)

data ClientClass
    = PhoneClient
    | TabletClient
    | DesktopClient
    | LegalHoldClient -- see Note [LegalHold]
    deriving (Eq, Ord, Show)

data NewClient = NewClient
    { newClientPrekeys  :: [Prekey]
    , newClientLastKey  :: !LastPrekey
    , newClientType     :: !ClientType
    , newClientLabel    :: !(Maybe Text)
    , newClientClass    :: !(Maybe ClientClass)
    , newClientCookie   :: !(Maybe CookieLabel)
    , newClientPassword :: !(Maybe PlainTextPassword)
    , newClientModel    :: !(Maybe Text)
    }

newClient :: ClientType -> LastPrekey -> NewClient
newClient t k = NewClient
    { newClientPrekeys  = []
    , newClientLastKey  = k
    , newClientType     = t
    , newClientLabel    = Nothing
    , newClientClass    = if t == LegalHoldClientType then Just LegalHoldClient else Nothing
    , newClientCookie   = Nothing
    , newClientPassword = Nothing
    , newClientModel    = Nothing
    }

data Client = Client
    { clientId       :: !ClientId
    , clientType     :: !ClientType
    , clientTime     :: !UTCTimeMillis
    , clientClass    :: !(Maybe ClientClass)
    , clientLabel    :: !(Maybe Text)
    , clientCookie   :: !(Maybe CookieLabel)
    , clientLocation :: !(Maybe Location)
    , clientModel    :: !(Maybe Text)
    } deriving (Eq, Show, Generic)

data PubClient = PubClient
    { pubClientId     :: !ClientId
    , pubClientClass  :: !(Maybe ClientClass)
    } deriving (Eq, Show, Generic)

newtype RmClient = RmClient
    { rmPassword :: Maybe PlainTextPassword
    } deriving (Generic)

data UpdateClient = UpdateClient
    { updateClientPrekeys :: ![Prekey]
    , updateClientLastKey :: !(Maybe LastPrekey)
    , updateClientLabel   :: !(Maybe Text)
    } deriving (Generic)

-- * JSON instances:

instance ToJSON Client where
    toJSON c = object
        $ "id"       .= clientId c
        # "type"     .= clientType c
        # "label"    .= clientLabel c
        # "class"    .= clientClass c
        # "time"     .= clientTime c
        # "cookie"   .= clientCookie c
        # "location" .= clientLocation c
        # "model"    .= clientModel c
        # []

instance FromJSON Client where
    parseJSON = withObject "Client" $ \o ->
        Client <$> o .:  "id"
               <*> o .:  "type"
               <*> o .:  "time"
               <*> o .:? "class"
               <*> o .:? "label"
               <*> o .:? "cookie"
               <*> o .:? "location"
               <*> o .:? "model"

instance ToJSON PubClient where
    toJSON c = object
        $ "id"     .= pubClientId c
        # "class"  .= pubClientClass c
        # []

instance FromJSON PubClient where
    parseJSON = withObject "PubClient" $ \o ->
        PubClient <$> o .:  "id"
                  <*> o .:? "class"

instance ToJSON ClientType where
    toJSON TemporaryClientType = String "temporary"
    toJSON PermanentClientType = String "permanent"
    toJSON LegalHoldClientType = String "legalhold"

instance FromJSON ClientType where
    parseJSON = withText "ClientType" $ \txt -> case txt of
        "temporary" -> return TemporaryClientType
        "permanent" -> return PermanentClientType
        "legalhold" -> return LegalHoldClientType
        _           -> fail "Must be one of {'temporary', 'permanent', 'legalhold'}."

instance ToJSON ClientClass where
    toJSON PhoneClient   = String "phone"
    toJSON TabletClient  = String "tablet"
    toJSON DesktopClient = String "desktop"
    toJSON LegalHoldClient = String "legalhold"

instance FromJSON ClientClass where
    parseJSON = withText "ClientClass" $ \txt -> case txt of
        "phone"     -> return PhoneClient
        "tablet"    -> return TabletClient
        "desktop"   -> return DesktopClient
        "legalhold" -> return LegalHoldClient
        _           -> fail "Must be one of {'phone', 'tablet', 'desktop', 'legalhold'}."

instance ToJSON NewClient where
    toJSON c = object
        $ "type"     .= newClientType c
        # "prekeys"  .= newClientPrekeys c
        # "lastkey"  .= newClientLastKey c
        # "label"    .= newClientLabel c
        # "class"    .= newClientClass c
        # "cookie"   .= newClientCookie c
        # "password" .= newClientPassword c
        # "model"    .= newClientModel c
        # []

instance FromJSON NewClient where
    parseJSON = withObject "NewClient" $ \o ->
        NewClient <$> o .:  "prekeys"
                  <*> o .:  "lastkey"
                  <*> o .:  "type"
                  <*> o .:? "label"
                  <*> o .:? "class"
                  <*> o .:? "cookie"
                  <*> o .:? "password"
                  <*> o .:? "model"

instance ToJSON RmClient where
    toJSON (RmClient pw) = object [ "password" .= pw ]

instance FromJSON RmClient where
    parseJSON = withObject "RmClient" $ \o ->
        RmClient <$> o .:? "password"

instance ToJSON UpdateClient where
    toJSON c = object
        $ "prekeys" .= updateClientPrekeys c
        # "lastkey" .= updateClientLastKey c
        # "label"   .= updateClientLabel c
        # []

instance FromJSON UpdateClient where
    parseJSON = withObject "RefreshClient" $ \o ->
        UpdateClient <$> o .:?  "prekeys" .!= []
                     <*> o .:? "lastkey"
                     <*> o .:? "label"
