{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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

module Ropes.Nexmo
  ( -- * Types
    ApiKey (..),
    ApiSecret (..),
    Credentials,
    ParseError (..),
    Charset (..),

    -- * SMS
    MessageErrorResponse (..),
    MessageErrorStatus (..),
    Message (..),
    MessageId,
    MessageResponse,

    -- * Call
    Call (..),
    CallId,
    CallErrorResponse (..),
    CallErrorStatus (..),

    -- * Functions
    sendCall,
    sendMessage,
    sendMessages,
    sendFeedback,
    msgIds,
  )
where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Imports hiding (head, length)
import Network.HTTP.Client hiding (Response)
import Network.HTTP.Types

-- * Types

newtype ApiKey = ApiKey Text deriving (FromJSON)

newtype ApiSecret = ApiSecret Text deriving (FromJSON)

data Charset = GSM7 | GSM8 | UCS2 deriving (Eq, Show)

data Credentials = Credentials
  { key :: ApiKey,
    secret :: ApiSecret
  }

instance FromJSON Credentials where
  parseJSON = withObject "credentials" $ \o ->
    Credentials <$> o .: "key"
      <*> o .: "secret"

-- * SMS related

newtype MessageId = MessageId {messageIdText :: Text} deriving (Eq, Show)

data Message = Message
  { msgFrom :: !Text,
    msgTo :: !Text,
    msgText :: !Text,
    msgType :: !Charset
  }
  deriving (Eq, Show)

newtype MessageResponse = MessageResponse {msgIds :: NonEmpty MessageId}
  deriving (Eq, Show)

data MessageErrorStatus
  = MessageThrottled
  | MessageInternal
  | MessageUnroutable
  | MessageNumBarred
  | MessagePartnerAccountBarred
  | MessagePartnerQuotaExceeded
  | MessageTooLong
  | MessageCommunicationFailed
  | MessageInvalidSenderAddress
  | MessageFacilityNotAllowed
  | MessageInvalidMessageClass
  | MessageOther
  deriving (Eq, Show)

instance FromJSON MessageErrorStatus where
  parseJSON "1" = return MessageThrottled
  parseJSON "5" = return MessageInternal
  parseJSON "6" = return MessageUnroutable
  parseJSON "7" = return MessageNumBarred
  parseJSON "8" = return MessagePartnerAccountBarred
  parseJSON "9" = return MessagePartnerQuotaExceeded
  parseJSON "12" = return MessageTooLong
  parseJSON "13" = return MessageCommunicationFailed
  parseJSON "15" = return MessageInvalidSenderAddress
  parseJSON "19" = return MessageFacilityNotAllowed
  parseJSON "20" = return MessageInvalidMessageClass
  parseJSON _ = return MessageOther

data MessageErrorResponse = MessageErrorResponse
  { erStatus :: !MessageErrorStatus,
    erErrorText :: !(Maybe Text)
  }
  deriving (Eq, Show, Typeable)

instance Exception MessageErrorResponse

instance FromJSON MessageErrorResponse where
  parseJSON = withObject "message-error-response" $ \o ->
    MessageErrorResponse <$> o .: "status"
      <*> o .:? "error-text"

newtype ParseError = ParseError String
  deriving (Eq, Show, Typeable)

instance Exception ParseError

instance FromJSON MessageId where
  parseJSON = withText "MessageId" $ return . MessageId

instance ToJSON MessageId where
  toJSON = String . messageIdText

instance FromJSON Charset where
  parseJSON "text" = return GSM7
  parseJSON "binary" = return GSM8
  parseJSON "unicode" = return UCS2
  parseJSON x = fail $ "Unsupported charset " <> (show x)

instance ToJSON Charset where
  toJSON GSM7 = "text"
  toJSON GSM8 = "binary"
  toJSON UCS2 = "unicode"

-- * Internal message parsers

parseMessageFeedback :: Value -> Parser (Either MessageErrorResponse MessageId)
parseMessageFeedback j@(Object o) = do
  st <- o .: "status"
  case (st :: Text) of
    "0" -> Right <$> parseMessageId j
    _ -> Left <$> parseJSON j
parseMessageFeedback _ = fail "Ropes.Nexmo: message should be an object"

parseMessageId :: Value -> Parser MessageId
parseMessageId = withObject "message-response" (.: "message-id")

parseMessageResponse :: Value -> Parser (Either MessageErrorResponse MessageResponse)
parseMessageResponse = withObject "nexmo-response" $ \o -> do
  xs <- o .: "messages"
  ys <- sequence <$> mapM parseMessageFeedback xs
  case ys of
    Left e -> return $ Left e
    Right (f : fs) -> return $ Right $ MessageResponse (f :| fs)
    Right _ -> fail "Must have at least one message-id"

-- * Call related

newtype CallId = CallId {callIdText :: Text} deriving (Eq, Show)

data Call = Call
  { callFrom :: !(Maybe Text),
    callTo :: !Text,
    callText :: !Text,
    callLang :: !(Maybe Text),
    callRepeat :: !(Maybe Int)
  }

data CallErrorStatus
  = CallThrottled
  | CallInternal
  | CallDestinationNotPermitted
  | CallDestinationBarred
  | CallPartnerQuotaExceeded
  | CallInvalidDestinationAddress
  | CallUnroutable
  | CallOther
  deriving (Eq, Show)

instance FromJSON CallErrorStatus where
  parseJSON "1" = return CallThrottled
  parseJSON "5" = return CallInternal
  parseJSON "6" = return CallDestinationNotPermitted
  parseJSON "7" = return CallDestinationBarred
  parseJSON "9" = return CallPartnerQuotaExceeded
  parseJSON "15" = return CallInvalidDestinationAddress
  parseJSON "17" = return CallUnroutable
  parseJSON _ = return CallOther

data CallErrorResponse = CallErrorResponse
  { caStatus :: !CallErrorStatus,
    caErrorText :: !(Maybe Text)
  }
  deriving (Eq, Show, Typeable)

instance Exception CallErrorResponse

instance FromJSON CallErrorResponse where
  parseJSON = withObject "call-error-response" $ \o ->
    CallErrorResponse <$> o .: "status"
      <*> o .:? "error-text"

-- * Internal call parsers

parseCallId :: Value -> Parser CallId
parseCallId = withObject "call-response" $ \o ->
  CallId <$> o .: "call_id"

parseCallResponse :: Value -> Parser (Either CallErrorResponse CallId)
parseCallResponse j@(Object o) = do
  st <- o .: "status"
  case (st :: Text) of
    "0" -> Right <$> parseCallId j
    _ -> Left <$> parseJSON j
parseCallResponse _ = fail "Ropes.Nexmo: response should be an object"

-- * Feedback related

data Feedback = Feedback
  { feedbackId :: !(Either CallId MessageId),
    feedbackTime :: !UTCTime,
    feedbackDelivered :: !Bool
  }
  deriving (Eq, Show)

data FeedbackErrorResponse = FeedbackErrorResponse Text
  deriving (Eq, Show)

instance Exception FeedbackErrorResponse

-- * Functions

sendCall :: Credentials -> Manager -> Call -> IO CallId
sendCall cr mgr call = httpLbs req mgr >>= parseResult
  where
    parseResult res = case parseEither parseCallResponse =<< eitherDecode (responseBody res) of
      Left e -> throwIO $ ParseError e
      Right r -> either throwIO return r
    req =
      defaultRequest
        { method = "POST",
          host = "api.nexmo.com",
          secure = True,
          port = 443,
          path = "/tts/json",
          requestBody = RequestBodyLBS $ encode body,
          requestHeaders = [(hContentType, "application/json")]
        }
    (ApiKey apiKey, ApiSecret apiSecret) = (key cr, secret cr)
    body =
      object
        [ "api_key" .= apiKey,
          "api_secret" .= apiSecret,
          "from" .= callFrom call,
          "to" .= callTo call,
          "text" .= callText call,
          "repeat" .= callRepeat call,
          "lg" .= callLang call
        ]

sendFeedback :: Credentials -> Manager -> Feedback -> IO ()
sendFeedback cr mgr fb = httpLbs req mgr >>= parseResponse
  where
    req =
      defaultRequest
        { method = "POST",
          host = "api.nexmo.com",
          secure = True,
          port = 443,
          path =
            either
              (const "/conversions/voice")
              (const "/conversions/sms")
              (feedbackId fb),
          requestBody = RequestBodyLBS $ encode body,
          requestHeaders = [(hContentType, "application/json")]
        }
    (ApiKey apiKey, ApiSecret apiSecret) = (key cr, secret cr)
    body =
      object
        [ "api_key" .= apiKey,
          "api_secret" .= apiSecret,
          "message-id" .= either callIdText messageIdText (feedbackId fb),
          "delivered" .= feedbackDelivered fb,
          "timestamp" .= nexmoTimeFormat (feedbackTime fb)
        ]
    -- Format as specified https://docs.nexmo.com/api-ref/conversion-api/request
    -- Note that the claim that "If you do not set this parameter, the Cloud
    -- Communications Platform uses the time it recieves this request." is false
    -- You must _always_ specify a timestamp
    nexmoTimeFormat = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    parseResponse res =
      unless (responseStatus res == status200) $
        throwIO $
          FeedbackErrorResponse (decodeUtf8 . toStrict . responseBody $ res)

sendMessage :: Credentials -> Manager -> Message -> IO MessageResponse
sendMessage cr mgr msg = N.head <$> sendMessages cr mgr (msg :| [])

sendMessages :: Credentials -> Manager -> NonEmpty Message -> IO (NonEmpty MessageResponse)
sendMessages cr mgr msgs = forM msgs $ \m -> httpLbs (req m) mgr >>= parseResult
  where
    parseResult res = case parseEither parseMessageResponse =<< eitherDecode (responseBody res) of
      Left e -> throwIO $ ParseError e
      Right r -> either throwIO return r
    req m =
      defaultRequest
        { method = "POST",
          host = "rest.nexmo.com",
          secure = True,
          port = 443,
          path = "/sms/json",
          requestBody = RequestBodyLBS $ encode (body m),
          requestHeaders = [(hContentType, "application/json")]
        }
    (ApiKey apiKey, ApiSecret apiSecret) = (key cr, secret cr)
    body m =
      object
        [ "api_key" .= apiKey,
          "api_secret" .= apiSecret,
          "from" .= msgFrom m,
          "to" .= msgTo m,
          "text" .= msgText m,
          "type" .= msgType m
        ]
