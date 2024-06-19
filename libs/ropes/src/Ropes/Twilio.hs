{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Ropes.Twilio
  ( -- * Types
    SID (..),
    AccessToken (..),
    Credentials,
    Message (..),
    MessageId,
    LookupDetail (..),
    CarrierInfo (..),
    PhoneType (..),
    LookupResult (..),
    ErrorResponse (..),
    ParseError (..),

    -- * Functions
    sendMessage,
    sendMessages,
    lookupPhone,
    tryTwilio,
  )
where

import Control.Error (ExceptT (..))
import Control.Exception
import Data.Aeson
import Data.ByteString.Char8 qualified as C
import Data.ISO3166_CountryCodes (CountryCode)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as N
import Data.Text.Encoding (encodeUtf8)
import Imports hiding (head, length)
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

-- * Types

newtype MessageId = MessageId ByteString

newtype SID = SID ByteString

newtype AccessToken = AccessToken ByteString

data Credentials = Credentials
  { sid :: SID,
    token :: AccessToken
  }

instance FromJSON Credentials where
  parseJSON = withObject "credentials" $ \o ->
    Credentials
      <$> (SID . encodeUtf8 <$> o .: "sid")
      <*> (AccessToken . encodeUtf8 <$> o .: "token")

data Message = Message
  { msgFrom :: !Text,
    msgTo :: !Text,
    msgText :: !Text
  }
  deriving (Eq, Show)

data ErrorResponse = ErrorResponse
  { errStatus :: !Int,
    errMessage :: !Text,
    errCode :: !(Maybe Int),
    errMoreInfo :: !(Maybe Text)
  }
  deriving (Eq, Show, Typeable)

instance Exception ErrorResponse

instance FromJSON ErrorResponse where
  parseJSON = withObject "error-response" $ \o ->
    ErrorResponse
      <$> o .: "status"
      <*> o .: "message"
      <*> o .:? "code"
      <*> o .:? "more_info"

newtype ParseError = ParseError String
  deriving (Eq, Show, Typeable)

instance Exception ParseError

data MessageResponse = MessageResponse
  { msgId :: !MessageId
  }

instance FromJSON MessageResponse where
  parseJSON = withObject "MessageResponse" $ \o ->
    MessageResponse . MessageId . encodeUtf8 <$> o .: "sid"

data LookupDetail
  = LookupNoDetail
  | LookupCarrier
  deriving (Eq, Show)

data LookupResult = LookupResult
  { lookupE164 :: !Text,
    lookupCarrier :: !(Maybe CarrierInfo)
  }

data CarrierInfo = CarrierInfo
  { carrierName :: !(Maybe Text),
    carrierType :: !(Maybe PhoneType)
  }

data PhoneType
  = Landline
  | Mobile
  | VoIp
  deriving (Eq, Show)

instance FromJSON LookupResult where
  parseJSON = withObject "LookupResult" $ \o ->
    LookupResult
      <$> o .: "phone_number"
      <*> o .:? "carrier"

instance FromJSON CarrierInfo where
  parseJSON = withObject "CarrierInfo" $ \o ->
    CarrierInfo
      <$> o .:? "name"
      <*> o .:? "type"

instance FromJSON PhoneType where
  parseJSON = withText "PhoneType" $ \case
    "mobile" -> pure Mobile
    "landline" -> pure Landline
    "voip" -> pure VoIp
    x -> fail $ "Unexpected phone type: " ++ show x

-- * Functions

tryTwilio :: (MonadIO m) => IO a -> ExceptT ErrorResponse m a
tryTwilio = ExceptT . liftIO . try

sendMessage :: Credentials -> Manager -> Message -> IO MessageId
sendMessage cr mgr msg = N.head <$> sendMessages cr mgr (msg :| [])

sendMessages :: Credentials -> Manager -> NonEmpty Message -> IO (NonEmpty MessageId)
sendMessages cr mgr msgs = forM msgs $ \m -> do
  let req = urlEncodedBody (form m) . applyBasicAuth tSid tToken $ apiReq
  rsp <- httpLbs req mgr
  if responseStatus rsp == status201
    then case eitherDecode (responseBody rsp) of
      Right r -> pure $ msgId r
      Left e -> throwIO $ ParseError e
    else case eitherDecode (responseBody rsp) of
      Right e -> throwIO (e :: ErrorResponse)
      Left e -> throwIO $ ParseError e
  where
    apiReq =
      defaultRequest
        { method = "POST",
          host = "api.twilio.com",
          secure = True,
          port = 443,
          path = "/2010-04-01/Accounts/" <> tSid <> "/Messages.json"
        }
    (SID tSid, AccessToken tToken) = (sid cr, token cr)
    form m =
      [ ("From", encodeUtf8 . msgFrom $ m),
        ("To", encodeUtf8 . msgTo $ m),
        ("Body", encodeUtf8 . msgText $ m)
      ]

lookupPhone ::
  Credentials ->
  Manager ->
  Text ->
  LookupDetail ->
  Maybe CountryCode ->
  IO LookupResult
lookupPhone cr mgr phone detail country = do
  let req = applyBasicAuth tSid tToken apiReq
  rsp <- httpLbs req mgr
  if responseStatus rsp == status200
    then case eitherDecode (responseBody rsp) of
      Right r -> pure r
      Left e -> throwIO $ ParseError e
    else case eitherDecode (responseBody rsp) of
      Right e -> throwIO (e :: ErrorResponse)
      Left e -> throwIO $ ParseError e
  where
    (SID tSid, AccessToken tToken) = (sid cr, token cr)
    apiReq =
      defaultRequest
        { method = "GET",
          host = "lookups.twilio.com",
          secure = True,
          port = 443,
          path = "/v1/PhoneNumbers/" <> encodeUtf8 phone,
          queryString = renderSimpleQuery False queryItems
        }
    queryItems =
      catMaybes
        [ countryCode <$> country,
          lookupType detail
        ]
    countryCode c = ("CountryCode", C.pack (show c))
    lookupType LookupNoDetail = Nothing
    lookupType LookupCarrier = Just ("Type", "carrier")
