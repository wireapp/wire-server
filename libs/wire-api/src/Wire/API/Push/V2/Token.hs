{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Push.V2.Token
  ( -- * PushToken
    PushTokenList (..),
    PushToken,
    pushToken,
    tokenTransport,
    tokenApp,
    tokenClient,
    token,

    -- * PushToken fields
    Transport (..),
    Token (..),
    AppName (..),

    -- * Swagger
    modelPushToken,
    modelPushTokenList,
    typeTransport,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- PushToken

newtype PushTokenList = PushTokenList
  { pushTokens :: [PushToken]
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

modelPushTokenList :: Doc.Model
modelPushTokenList = Doc.defineModel "PushTokenList" $ do
  Doc.description "List of Native Push Tokens"
  Doc.property "tokens" (Doc.array (Doc.ref modelPushToken)) $
    Doc.description "Push tokens"

instance ToJSON PushTokenList where
  toJSON (PushTokenList t) = object ["tokens" .= t]

instance FromJSON PushTokenList where
  parseJSON = withObject "PushTokenList" $ \p ->
    PushTokenList <$> p .: "tokens"

data PushToken = PushToken
  { _tokenTransport :: Transport,
    _tokenApp :: AppName,
    _token :: Token,
    _tokenClient :: ClientId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PushToken)

pushToken :: Transport -> AppName -> Token -> ClientId -> PushToken
pushToken tp an tk cl = PushToken tp an tk cl

modelPushToken :: Doc.Model
modelPushToken = Doc.defineModel "PushToken" $ do
  Doc.description "Native Push Token"
  Doc.property "transport" typeTransport $
    Doc.description "Transport"
  Doc.property "app" Doc.string' $
    Doc.description "Application"
  Doc.property "token" Doc.bytes' $
    Doc.description "Access Token"
  Doc.property "client" Doc.bytes' $ do
    Doc.description "Client ID"
    Doc.optional

instance ToJSON PushToken where
  toJSON p =
    object $
      "transport" .= _tokenTransport p
        # "app" .= _tokenApp p
        # "token" .= _token p
        # "client" .= _tokenClient p
        # []

instance FromJSON PushToken where
  parseJSON = withObject "PushToken" $ \p ->
    PushToken
      <$> p .: "transport"
      <*> p .: "app"
      <*> p .: "token"
      <*> p .: "client"

--------------------------------------------------------------------------------
-- Transport

data Transport
  = GCM
  | APNS
  | APNSSandbox
  | APNSVoIP
  | APNSVoIPSandbox
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform Transport)

typeTransport :: Doc.DataType
typeTransport =
  Doc.string $
    Doc.enum
      [ "GCM",
        "APNS",
        "APNS_SANDBOX",
        "APNS_VOIP",
        "APNS_VOIP_SANDBOX"
      ]

instance ToJSON Transport where
  toJSON GCM = "GCM"
  toJSON APNS = "APNS"
  toJSON APNSSandbox = "APNS_SANDBOX"
  toJSON APNSVoIP = "APNS_VOIP"
  toJSON APNSVoIPSandbox = "APNS_VOIP_SANDBOX"

instance FromJSON Transport where
  parseJSON = withText "transport" $ \case
    "GCM" -> return GCM
    "APNS" -> return APNS
    "APNS_SANDBOX" -> return APNSSandbox
    "APNS_VOIP" -> return APNSVoIP
    "APNS_VOIP_SANDBOX" -> return APNSVoIPSandbox
    x -> fail $ "Invalid push transport: " ++ show x

instance FromByteString Transport where
  parser =
    takeByteString >>= \case
      "GCM" -> return GCM
      "APNS" -> return APNS
      "APNS_SANDBOX" -> return APNSSandbox
      "APNS_VOIP" -> return APNSVoIP
      "APNS_VOIP_SANDBOX" -> return APNSVoIPSandbox
      x -> fail $ "Invalid push transport: " <> show x

newtype Token = Token
  { tokenText :: Text
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, FromByteString, ToByteString, Arbitrary)

newtype AppName = AppName
  { appNameText :: Text
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, IsString, Arbitrary)

makeLenses ''PushToken
