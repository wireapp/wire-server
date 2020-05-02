{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
  ( Transport (..),
    Token (..),
    AppName (..),
    PushToken,
    pushToken,
    tokenTransport,
    tokenApp,
    tokenClient,
    token,
    PushTokenList (..),
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Imports

-----------------------------------------------------------------------------
-- Transport

data Transport
  = GCM
  | APNS
  | APNSSandbox
  | APNSVoIP
  | APNSVoIPSandbox
  deriving (Eq, Ord, Show, Bounded, Enum)

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
  parser = takeByteString >>= \case
    "GCM" -> return GCM
    "APNS" -> return APNS
    "APNS_SANDBOX" -> return APNSSandbox
    "APNS_VOIP" -> return APNSVoIP
    "APNS_VOIP_SANDBOX" -> return APNSVoIPSandbox
    x -> fail $ "Invalid push transport: " <> show x

-----------------------------------------------------------------------------
-- PushToken

newtype Token = Token
  { tokenText :: Text
  }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromByteString, ToByteString)

newtype AppName = AppName
  { appNameText :: Text
  }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString)

data PushToken = PushToken
  { _tokenTransport :: !Transport,
    _tokenApp :: !AppName,
    _token :: !Token,
    _tokenClient :: !ClientId
  }
  deriving (Eq, Ord, Show)

makeLenses ''PushToken

pushToken :: Transport -> AppName -> Token -> ClientId -> PushToken
pushToken tp an tk cl = PushToken tp an tk cl

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
    PushToken <$> p .: "transport"
      <*> p .: "app"
      <*> p .: "token"
      <*> p .: "client"

newtype PushTokenList = PushTokenList
  { pushTokens :: [PushToken]
  }
  deriving (Eq, Show)

instance ToJSON PushTokenList where
  toJSON (PushTokenList t) = object ["tokens" .= t]

instance FromJSON PushTokenList where
  parseJSON = withObject "PushTokenList" $ \p ->
    PushTokenList <$> p .: "tokens"
