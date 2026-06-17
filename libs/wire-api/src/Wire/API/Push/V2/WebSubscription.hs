{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Push.V2.WebSubscription
  ( -- * WebPushSubscription
    WebPushSubscriptionList (..),
    WebPushSubscription,
    webPushSubscription,
    wpsEndpoint,
    wpsKeys,
    wpsExpirationTime,
    wpsClient,

    -- * WebPushSubscription fields
    EndpointUrl (..),
    mkEndpointUrl,
    P256dhKey (..),
    mkP256dhKey,
    AuthSecret (..),
    mkAuthSecret,
    WebPushKeys (..),
    wpkP256dh,
    wpkAuth,

    -- * API types
    AddWebPushError (..),
    AddWebPushSuccess (..),
    AddWebPushResponses,
    DeleteWebPushRequest (..),
    DeleteWebPushResponses,
  )
where

import Control.Lens (makeLenses, (?~), (^.))
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.Id (ClientId)
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.SOP
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Generics.SOP qualified as GSOP
import Imports
import Servant (FromHttpApiData (..), Header, ToHttpApiData (..), type (.++))
import Test.QuickCheck (Arbitrary (arbitrary))
import URI.ByteString (schemeBSL, strictURIParserOptions, uriSchemeL)
import URI.ByteString qualified as URI
import Wire.API.Error
import Wire.API.Error.Gundeck qualified as E
import Wire.API.Routes.MultiVerb
import Wire.Arbitrary (GenericUniform (..))

--------------------------------------------------------------------------------
-- EndpointUrl

-- | The HTTPS endpoint of a browser push service to which encrypted web push
-- notifications are delivered (RFC 8030). The URL must use the @https:@ scheme;
-- non-HTTPS endpoints are rejected to avoid leaking the user identity over an
-- insecure transport (RFC 8030 §8.2).
newtype EndpointUrl = EndpointUrl
  { endpointUrlText :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema EndpointUrl)

mkEndpointUrl :: Text -> Either String EndpointUrl
mkEndpointUrl raw = do
  uri <- first show (URI.parseURI strictURIParserOptions (encodeUtf8 raw))
  if uri ^. uriSchemeL . schemeBSL == "https"
    then Right (EndpointUrl raw)
    else Left ("Non-HTTPS endpoint URL: " <> show raw)

instance ToSchema EndpointUrl where
  schema =
    endpointUrlText
      .= parsedText "EndpointUrl" mkEndpointUrl

instance ToParamSchema EndpointUrl where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance ToHttpApiData EndpointUrl where
  toUrlPiece = endpointUrlText

instance FromHttpApiData EndpointUrl where
  parseUrlPiece = first Text.pack . mkEndpointUrl

instance Arbitrary EndpointUrl where
  arbitrary = pure $ EndpointUrl "https://example.com/webpush/subscription"

--------------------------------------------------------------------------------
-- P256dhKey

-- | The client's ECDH P-256 public key, in raw uncompressed form (65 bytes:
-- @0x04@ followed by the 32-byte X and 32-byte Y coordinates), as defined by
-- RFC 8291 §3.1. It is transported base64url-encoded and stored decoded.
newtype P256dhKey = P256dhKey
  { p256dhKeyBytes :: BS.ByteString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema P256dhKey)

mkP256dhKey :: BS.ByteString -> Either String P256dhKey
mkP256dhKey bs
  | BS.length bs == 65 = Right (P256dhKey bs)
  | otherwise =
      Left $
        "Invalid p256dh key: expected 65 bytes, got " <> show (BS.length bs)

instance ToSchema P256dhKey where
  schema =
    (decodeUtf8 . B64U.encodeUnpadded . p256dhKeyBytes)
      .= parsedText "P256dhKey" parseP256dh
    where
      parseP256dh :: Text -> Either String P256dhKey
      parseP256dh t = do
        bs <- first show (B64U.decodeUnpadded (encodeUtf8 t))
        mkP256dhKey bs

instance Arbitrary P256dhKey where
  arbitrary = P256dhKey . BS.pack <$> replicateM 65 (arbitrary @Word8)

--------------------------------------------------------------------------------
-- AuthSecret

-- | The authentication secret shared between the application server and the
-- push service (RFC 8291 §2, 16 random bytes), transported base64url-encoded
-- and stored decoded.
newtype AuthSecret = AuthSecret
  { authSecretBytes :: BS.ByteString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema AuthSecret)

mkAuthSecret :: BS.ByteString -> Either String AuthSecret
mkAuthSecret bs
  | BS.length bs == 16 = Right (AuthSecret bs)
  | otherwise =
      Left $
        "Invalid auth secret: expected 16 bytes, got " <> show (BS.length bs)

instance ToSchema AuthSecret where
  schema =
    (decodeUtf8 . B64U.encodeUnpadded . authSecretBytes)
      .= parsedText "AuthSecret" parseAuthSecret
    where
      parseAuthSecret :: Text -> Either String AuthSecret
      parseAuthSecret t = do
        bs <- first show (B64U.decodeUnpadded (encodeUtf8 t))
        mkAuthSecret bs

instance Arbitrary AuthSecret where
  arbitrary = AuthSecret . BS.pack <$> replicateM 16 (arbitrary @Word8)

--------------------------------------------------------------------------------
-- WebPushKeys

data WebPushKeys = WebPushKeys
  { _wpkP256dh :: !P256dhKey,
    _wpkAuth :: !AuthSecret
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform WebPushKeys)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema WebPushKeys)

instance ToSchema WebPushKeys where
  schema =
    object $
      WebPushKeys
        <$> _wpkP256dh
          .= field "p256dh" schema
        <*> _wpkAuth
          .= field "auth" schema

--------------------------------------------------------------------------------
-- WebPushSubscription

data WebPushSubscription = WebPushSubscription
  { _wpsEndpoint :: !EndpointUrl,
    _wpsKeys :: !WebPushKeys,
    -- | Optional expiry, in milliseconds since the Unix epoch. 'Nothing' means
    -- the subscription does not expire (W3C Push API §3.4).
    _wpsExpirationTime :: !(Maybe Word64),
    _wpsClient :: !ClientId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform WebPushSubscription)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema WebPushSubscription)

instance ToSchema WebPushSubscription where
  schema =
    object $
      WebPushSubscription
        <$> _wpsEndpoint
          .= field "endpoint" schema
        <*> _wpsKeys
          .= field "keys" schema
        <*> _wpsExpirationTime
          .= maybe_ (optField "expiration_time" schema)
        <*> _wpsClient
          .= field "client" schema

webPushSubscription ::
  EndpointUrl ->
  WebPushKeys ->
  Maybe Word64 ->
  ClientId ->
  WebPushSubscription
webPushSubscription = WebPushSubscription

newtype WebPushSubscriptionList = WebPushSubscriptionList
  { wpsList :: [WebPushSubscription]
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema WebPushSubscriptionList)

instance ToSchema WebPushSubscriptionList where
  schema =
    objectWithDocModifier (description ?~ "List of Web Push Subscriptions") $
      WebPushSubscriptionList
        <$> wpsList
          .= fieldWithDocModifier "subscriptions" (description ?~ "Web push subscriptions") (array schema)

makeLenses ''WebPushKeys
makeLenses ''WebPushSubscription

--------------------------------------------------------------------------------
-- Add web push subscription types

type AddWebPushErrorResponses =
  '[ ErrorResponse 'E.WebPushErrorInvalid,
     ErrorResponse 'E.WebPushErrorTooMany
   ]

type AddWebPushSuccessResponses =
  WithHeaders
    '[ Header "Location" EndpointUrl
     ]
    AddWebPushSuccess
    (Respond 201 "Web push subscription registered" WebPushSubscription)

type AddWebPushResponses = AddWebPushErrorResponses .++ '[AddWebPushSuccessResponses]

data AddWebPushError
  = AddWebPushErrorInvalid
  | AddWebPushErrorTooMany
  deriving (Show, Generic)
  deriving (AsUnion AddWebPushErrorResponses) via GenericAsUnion AddWebPushErrorResponses AddWebPushError

instance GSOP.Generic AddWebPushError

data AddWebPushSuccess = AddWebPushSuccess WebPushSubscription

instance AsHeaders '[EndpointUrl] WebPushSubscription AddWebPushSuccess where
  fromHeaders (I _ :* Nil, sub) = AddWebPushSuccess sub
  toHeaders (AddWebPushSuccess sub) = (I (sub ^. wpsEndpoint) :* Nil, sub)

instance (res ~ AddWebPushResponses) => AsUnion res (Either AddWebPushError AddWebPushSuccess) where
  toUnion = eitherToUnion (toUnion @AddWebPushErrorResponses) (Z . I)
  fromUnion = eitherFromUnion (fromUnion @AddWebPushErrorResponses) (unI . unZ)

--------------------------------------------------------------------------------
-- Delete web push subscription types

newtype DeleteWebPushRequest = DeleteWebPushRequest
  { deleteWebPushRequestEndpoint :: EndpointUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema DeleteWebPushRequest)

instance ToSchema DeleteWebPushRequest where
  schema =
    object $
      DeleteWebPushRequest
        <$> deleteWebPushRequestEndpoint
          .= field "endpoint" schema

type DeleteWebPushResponses =
  '[ ErrorResponse 'E.WebPushSubscriptionNotFound,
     RespondEmpty 204 "Web push subscription unregistered"
   ]
