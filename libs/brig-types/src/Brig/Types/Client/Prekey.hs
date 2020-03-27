{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Client.Prekey
  ( PrekeyId (..),
    Prekey (..),
    LastPrekey,
    lastPrekey,
    unpackLastPrekey,
    lastPrekeyId,
    clientIdFromPrekey,
    PrekeyBundle (..),
    ClientPrekey (..),
  )
where

import Data.Aeson
import Data.Hashable (hash)
import Data.Id
import Imports

newtype PrekeyId = PrekeyId {keyId :: Word16}
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic)

data Prekey
  = Prekey
      { prekeyId :: !PrekeyId,
        prekeyKey :: !Text
      }
  deriving (Eq, Show, Generic)

data PrekeyBundle
  = PrekeyBundle
      { prekeyUser :: !OpaqueUserId,
        prekeyClients :: ![ClientPrekey]
      }
  deriving (Eq, Show, Generic)

data ClientPrekey
  = ClientPrekey
      { prekeyClient :: !ClientId,
        prekeyData :: !Prekey
      }
  deriving (Eq, Show, Generic)

newtype LastPrekey
  = LastPrekey
      {unpackLastPrekey :: Prekey}
  deriving (Eq, Show, Generic)

lastPrekey :: Text -> LastPrekey
lastPrekey = LastPrekey . Prekey lastPrekeyId

lastPrekeyId :: PrekeyId
lastPrekeyId = PrekeyId maxBound

clientIdFromPrekey :: Prekey -> ClientId
clientIdFromPrekey prekey =
  newClientId . fromIntegral . hash . prekeyKey $ prekey

-- JSON

instance ToJSON LastPrekey where
  toJSON = toJSON . unpackLastPrekey

instance FromJSON LastPrekey where
  parseJSON =
    parseJSON
      >=> ( \k ->
              if prekeyId k == lastPrekeyId
                then return $ LastPrekey k
                else fail "Invalid last prekey ID."
          )

instance ToJSON Prekey where
  toJSON k =
    object
      [ "id" .= prekeyId k,
        "key" .= prekeyKey k
      ]

instance FromJSON Prekey where
  parseJSON = withObject "Prekey" $ \o ->
    Prekey <$> o .: "id" <*> o .: "key"

instance ToJSON PrekeyBundle where
  toJSON k =
    object
      [ "user" .= prekeyUser k,
        "clients" .= prekeyClients k
      ]

instance FromJSON PrekeyBundle where
  parseJSON = withObject "PrekeyBundle" $ \o ->
    PrekeyBundle <$> o .: "user" <*> o .: "clients"

instance ToJSON ClientPrekey where
  toJSON k =
    object
      [ "client" .= prekeyClient k,
        "prekey" .= prekeyData k
      ]

instance FromJSON ClientPrekey where
  parseJSON = withObject "ClientPrekey" $ \o ->
    ClientPrekey <$> o .: "client" <*> o .: "prekey"
