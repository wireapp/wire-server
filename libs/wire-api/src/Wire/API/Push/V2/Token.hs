{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

    -- * API types
    AddTokenError (..),
    AddTokenSuccess (..),
    AddTokenResponses,
    DeleteTokenResponses,
  )
where

import Control.Lens (makeLenses, (?~), (^.))
import Cassandra qualified as C
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.OpenApi (ToParamSchema)
import Data.OpenApi qualified as S
import Data.Text qualified as Text
import Data.SOP
import Data.Schema
import Generics.SOP qualified as GSOP
import Imports
import Servant
import Wire.API.Error
import Wire.API.Error.Gundeck qualified as E
import Wire.API.PostgresMarshall
import Wire.API.Routes.MultiVerb
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- PushToken

newtype PushTokenList = PushTokenList
  { pushTokens :: [PushToken]
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema PushTokenList)

instance ToSchema PushTokenList where
  schema =
    objectWithDocModifier (description ?~ "List of Native Push Tokens") $
      PushTokenList
        <$> pushTokens
          .= fieldWithDocModifier "tokens" (description ?~ "Push tokens") (array schema)

data PushToken = PushToken
  { _tokenTransport :: Transport,
    _tokenApp :: AppName,
    _token :: Token,
    _tokenClient :: ClientId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PushToken)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema PushToken)

pushToken :: Transport -> AppName -> Token -> ClientId -> PushToken
pushToken = PushToken

instance ToSchema PushToken where
  schema =
    objectWithDocModifier desc $
      PushToken
        <$> _tokenTransport
          .= fieldWithDocModifier "transport" transDesc schema
        <*> _tokenApp
          .= fieldWithDocModifier "app" appDesc schema
        <*> _token
          .= fieldWithDocModifier "token" tokenDesc schema
        <*> _tokenClient
          .= fieldWithDocModifier "client" clientIdDesc schema
    where
      desc = description ?~ "Native Push Token"
      transDesc = description ?~ "Transport"
      appDesc = description ?~ "Application"
      tokenDesc = description ?~ "Access Token"
      clientIdDesc = description ?~ "Client ID"

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
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema Transport)

instance ToSchema Transport where
  schema =
    enum @Text $
      mconcat
        [ element "GCM" GCM,
          element "APNS" APNS,
          element "APNS_SANDBOX" APNSSandbox,
          element "APNS_VOIP" APNSVoIP,
          element "APNS_VOIP_SANDBOX" APNSVoIPSandbox
        ]

instance FromByteString Transport where
  parser =
    takeByteString >>= \case
      "GCM" -> pure GCM
      "APNS" -> pure APNS
      "APNS_SANDBOX" -> pure APNSSandbox
      "APNS_VOIP" -> pure APNSVoIP
      "APNS_VOIP_SANDBOX" -> pure APNSVoIPSandbox
      x -> fail $ "Invalid push transport: " <> show x

newtype Token = Token
  { tokenText :: Text
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromByteString, ToByteString, Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema Token)

instance ToParamSchema Token where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance ToSchema Token where
  schema = Token <$> tokenText .= schema

newtype AppName = AppName
  { appNameText :: Text
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema AppName)

instance ToSchema AppName where
  schema = AppName <$> appNameText .= schema

--------------------------------------------------------------------------------
-- Cassandra + Postgres marshall instances

instance C.Cql Transport where
  ctype = C.Tagged C.IntColumn

  toCql GCM = C.CqlInt 0
  toCql APNS = C.CqlInt 1
  toCql APNSSandbox = C.CqlInt 2
  toCql APNSVoIP = C.CqlInt 3
  toCql APNSVoIPSandbox = C.CqlInt 4

  fromCql (C.CqlInt i) = case i of
    0 -> pure GCM
    1 -> pure APNS
    2 -> pure APNSSandbox
    3 -> pure APNSVoIP
    4 -> pure APNSVoIPSandbox
    n -> Left $ "unexpected transport: " ++ show n
  fromCql _ = Left "transport: int expected"

instance C.Cql Token where
  ctype = C.Tagged C.TextColumn
  toCql = C.CqlText . tokenText
  fromCql (C.CqlText txt) = Right (Token txt)
  fromCql _ = Left "Token: Text expected"

instance C.Cql AppName where
  ctype = C.Tagged C.TextColumn
  toCql = C.CqlText . appNameText
  fromCql (C.CqlText txt) = Right (AppName txt)
  fromCql _ = Left "App: Text expected"

instance PostgresMarshall Int32 Transport where
  postgresMarshall = \case
    GCM -> 0
    APNS -> 1
    APNSSandbox -> 2
    APNSVoIP -> 3
    APNSVoIPSandbox -> 4

instance PostgresUnmarshall Int32 Transport where
  postgresUnmarshall = \case
    0 -> Right GCM
    1 -> Right APNS
    2 -> Right APNSSandbox
    3 -> Right APNSVoIP
    4 -> Right APNSVoIPSandbox
    n -> Left ("unexpected transport: " <> Text.pack (show n))

instance PostgresMarshall Text Token where
  postgresMarshall = tokenText

instance PostgresUnmarshall Text Token where
  postgresUnmarshall = Right . Token

instance PostgresMarshall Text AppName where
  postgresMarshall = appNameText

instance PostgresUnmarshall Text AppName where
  postgresUnmarshall = Right . AppName

makeLenses ''PushToken

--------------------------------------------------------------------------------
-- Add token types

type AddTokenErrorResponses =
  '[ ErrorResponse 'E.AddTokenErrorNoBudget,
     ErrorResponse 'E.AddTokenErrorNotFound,
     ErrorResponse 'E.AddTokenErrorInvalid,
     ErrorResponse 'E.AddTokenErrorTooLong,
     ErrorResponse 'E.AddTokenErrorMetadataTooLong,
     ErrorResponse 'E.AddTokenErrorApnsVoipNotSupported
   ]

type AddTokenSuccessResponses =
  WithHeaders
    '[ Header "Location" Token
     ]
    AddTokenSuccess
    (Respond 201 "Push token registered" PushToken)

type AddTokenResponses = AddTokenErrorResponses .++ '[AddTokenSuccessResponses]

data AddTokenError
  = AddTokenErrorNoBudget
  | AddTokenErrorNotFound
  | AddTokenErrorInvalid
  | AddTokenErrorTooLong
  | AddTokenErrorMetadataTooLong
  | AddTokenErrorApnsVoipNotSupported
  deriving (Show, Generic)
  deriving (AsUnion AddTokenErrorResponses) via GenericAsUnion AddTokenErrorResponses AddTokenError

instance GSOP.Generic AddTokenError

data AddTokenSuccess = AddTokenSuccess PushToken

instance AsHeaders '[Token] PushToken AddTokenSuccess where
  fromHeaders (I _ :* Nil, t) = AddTokenSuccess t
  toHeaders (AddTokenSuccess t) = (I (t ^. token) :* Nil, t)

instance (res ~ AddTokenResponses) => AsUnion res (Either AddTokenError AddTokenSuccess) where
  toUnion = eitherToUnion (toUnion @AddTokenErrorResponses) (Z . I)
  fromUnion = eitherFromUnion (fromUnion @AddTokenErrorResponses) (unI . unZ)

--------------------------------------------------------------------------------
-- Delete token types

type DeleteTokenResponses =
  '[ ErrorResponse 'E.TokenNotFound,
     RespondEmpty 204 "Push token unregistered"
   ]
