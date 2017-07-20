{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}

-- TODO: Move to Brig.Types.User.Client
module Brig.Types.Client
    ( module Brig.Types.Client
    , module C
    , module P
    ) where

import Brig.Types.User.Auth (CookieLabel)
import Brig.Types.Common as C
import Brig.Types.Client.Prekey as P
import Data.Aeson
import Data.Id
import Data.IP
import Data.Json.Util
import Data.Misc (Location, IpAddr (..), PlainTextPassword (..))
import Data.Text (Text)
import Data.Time (UTCTime)

-- * Data Types:

data ClientType
    = TemporaryClient
    | PermanentClient
    deriving (Eq, Ord, Show)

data ClientClass
    = PhoneClient
    | TabletClient
    | DesktopClient
    deriving (Eq, Ord, Show)

data NewClient a = NewClient
    { newClientPrekeys  :: [Prekey]
    , newClientLastKey  :: !LastPrekey
    , newClientSigKeys  :: !a
    , newClientType     :: !ClientType
    , newClientLabel    :: !(Maybe Text)
    , newClientClass    :: !(Maybe ClientClass)
    , newClientCookie   :: !(Maybe CookieLabel)
    , newClientPassword :: !(Maybe PlainTextPassword)
    , newClientModel    :: !(Maybe Text)
    }

newClient :: ClientType -> LastPrekey -> a -> NewClient a
newClient t k a = NewClient
    { newClientPrekeys  = []
    , newClientLastKey  = k
    , newClientSigKeys  = a
    , newClientType     = t
    , newClientLabel    = Nothing
    , newClientClass    = Nothing
    , newClientCookie   = Nothing
    , newClientPassword = Nothing
    , newClientModel    = Nothing
    }

data Client = Client
    { clientId       :: !ClientId
    , clientType     :: !ClientType
    , clientTime     :: !UTCTime
    , clientClass    :: !(Maybe ClientClass)
    , clientLabel    :: !(Maybe Text)
    , clientCookie   :: !(Maybe CookieLabel)
    , clientAddress  :: !(Maybe IP)
    , clientLocation :: !(Maybe Location)
    , clientModel    :: !(Maybe Text)
    } deriving (Eq, Show)

data PubClient = PubClient
    { pubClientId     :: !ClientId
    , pubClientClass  :: !(Maybe ClientClass)
    } deriving (Eq, Show)

newtype RmClient = RmClient
    { rmPassword :: Maybe PlainTextPassword
    }

data UpdateClient a = UpdateClient
    { updateClientPrekeys :: ![Prekey]
    , updateClientLastKey :: !(Maybe LastPrekey)
    , updateClientSigKeys :: !(Maybe a)
    , updateClientLabel   :: !(Maybe Text)
    }

-- * JSON instances:

instance ToJSON Client where
    toJSON c = object
        $ "id"       .= clientId c
        # "type"     .= clientType c
        # "label"    .= clientLabel c
        # "class"    .= clientClass c
        # "time"     .= UTCTimeMillis (clientTime c)
        # "cookie"   .= clientCookie c
        # "address"  .= (IpAddr <$> clientAddress c)
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
               <*> (fmap ipAddr <$> o .:? "address")
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
    toJSON TemporaryClient = String "temporary"
    toJSON PermanentClient = String "permanent"

instance FromJSON ClientType where
    parseJSON = withText "ClientType" $ \txt -> case txt of
        "temporary" -> return TemporaryClient
        "permanent" -> return PermanentClient
        _           -> fail "Must be one of {'temporary', 'permanent'}."

instance ToJSON ClientClass where
    toJSON PhoneClient   = String "phone"
    toJSON TabletClient  = String "tablet"
    toJSON DesktopClient = String "desktop"

instance FromJSON ClientClass where
    parseJSON = withText "ClientClass" $ \txt -> case txt of
        "phone"   -> return PhoneClient
        "tablet"  -> return TabletClient
        "desktop" -> return DesktopClient
        _         -> fail "Must be one of {'phone', 'tablet', 'desktop'}."

instance ToJSON a => ToJSON (NewClient a) where
    toJSON c = object
        $ "type"     .= newClientType c
        # "prekeys"  .= newClientPrekeys c
        # "lastkey"  .= newClientLastKey c
        # "sigkeys"  .= newClientSigKeys c
        # "label"    .= newClientLabel c
        # "class"    .= newClientClass c
        # "cookie"   .= newClientCookie c
        # "password" .= newClientPassword c
        # "model"    .= newClientModel c
        # []

instance FromJSON a => FromJSON (NewClient a) where
    parseJSON = withObject "NewClient" $ \o ->
        NewClient <$> o .:  "prekeys"
                  <*> o .:  "lastkey"
                  <*> o .:  "sigkeys"
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

instance ToJSON a => ToJSON (UpdateClient a) where
    toJSON c = object
        $ "prekeys" .= updateClientPrekeys c
        # "lastkey" .= updateClientLastKey c
        # "sigkeys" .= updateClientSigKeys c
        # "label"   .= updateClientLabel c
        # []

instance FromJSON a => FromJSON (UpdateClient a) where
    parseJSON = withObject "RefreshClient" $ \o ->
        UpdateClient <$> o .:?  "prekeys" .!= []
                     <*> o .:? "lastkey"
                     <*> o .:? "sigkeys"
                     <*> o .:? "label"
