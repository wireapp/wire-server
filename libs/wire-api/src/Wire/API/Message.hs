{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.Message
  ( -- * Message
    NewOtrMessage (..),

    -- * Recipients
    OtrRecipients (..),
    UserClientMap (..),

    -- * Filter
    OtrFilterMissing (..),
    ClientMismatch (..),
    UserClients (..),

    -- * Swagger
    modelNewOtrMessage,
    modelOtrRecipients,
    modelClientMismatch,
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util
import qualified Data.Swagger.Build.Api as Doc
import Data.Time
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.User.Client (UserClientMap (..), UserClients (..), modelOtrClientMap, modelUserClients)

--------------------------------------------------------------------------------
-- Message

data NewOtrMessage = NewOtrMessage
  { newOtrSender :: ClientId,
    newOtrRecipients :: OtrRecipients,
    newOtrNativePush :: Bool,
    newOtrTransient :: Bool,
    newOtrData :: Maybe Text,
    newOtrReportMissing :: Maybe [OpaqueUserId]
    -- FUTUREWORK: if (and only if) clients can promise this uid list will always exactly
    -- be the list of uids we could also extract from the messages' recipients field, we
    -- should do the latter, for two reasons: (1) no need for an artificial limit on the
    -- body field length, because it'd be just a boolean; (2) less network consumption.
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewOtrMessage)

modelNewOtrMessage :: Doc.Model
modelNewOtrMessage = Doc.defineModel "NewOtrMessage" $ do
  Doc.description "OTR message per recipient"
  Doc.property "sender" Doc.bytes' $
    Doc.description "The sender's client ID"
  Doc.property "recipients" (Doc.ref modelOtrRecipients) $
    Doc.description "Per-recipient data (i.e. ciphertext)."
  Doc.property "native_push" Doc.bool' $ do
    Doc.description "Whether to issue a native push to offline clients."
    Doc.optional
  Doc.property "transient" Doc.bool' $ do
    Doc.description "Whether to put this message into the notification queue."
    Doc.optional
  Doc.property "data" Doc.bytes' $ do
    Doc.description
      "Extra (symmetric) data (i.e. ciphertext) that is replicated \
      \for each recipient."
    Doc.optional
  Doc.property "report_missing" (Doc.unique $ Doc.array Doc.bytes') $ do
    Doc.description "List of user IDs"
    Doc.optional

instance ToJSON NewOtrMessage where
  toJSON otr =
    object $
      "sender" .= newOtrSender otr
        # "recipients" .= newOtrRecipients otr
        # "native_push" .= newOtrNativePush otr
        # "transient" .= newOtrTransient otr
        # "data" .= newOtrData otr
        # "report_missing" .= newOtrReportMissing otr
        # []

instance FromJSON NewOtrMessage where
  parseJSON = withObject "new-otr-message" $ \o ->
    NewOtrMessage
      <$> o .: "sender"
      <*> o .: "recipients"
      <*> o .:? "native_push" .!= True
      <*> o .:? "transient" .!= False
      <*> o .:? "data"
      <*> o .:? "report_missing"

--------------------------------------------------------------------------------
-- Recipients

newtype OtrRecipients = OtrRecipients
  { otrRecipientsMap :: UserClientMap Text
  }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, Semigroup, Monoid, Arbitrary)

modelOtrRecipients :: Doc.Model
modelOtrRecipients = Doc.defineModel "OtrRecipients" $ do
  Doc.description "Recipients of OTR content."
  -- FUTUREWORK: is this right?
  Doc.property "" (Doc.ref modelOtrClientMap) $
    Doc.description "Mapping of user IDs to 'OtrClientMap's."

--------------------------------------------------------------------------------
-- Filter

-- | A setting for choosing what to do when a message has not been encrypted
-- for all recipients.
data OtrFilterMissing
  = -- | Pretend everything is okay
    OtrIgnoreAllMissing
  | -- | Complain (default)
    OtrReportAllMissing
  | -- | Complain only about missing
    --      recipients who are /not/ on this list
    OtrIgnoreMissing (Set OpaqueUserId)
  | -- | Complain only about missing
    --      recipients who /are/ on this list
    OtrReportMissing (Set OpaqueUserId)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OtrFilterMissing)

data ClientMismatch = ClientMismatch
  { cmismatchTime :: UTCTime,
    -- | Clients that the message /should/ have been encrypted for, but wasn't.
    missingClients :: UserClients,
    -- | Clients that the message /should not/ have been encrypted for, but was.
    redundantClients :: UserClients,
    deletedClients :: UserClients
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientMismatch)

modelClientMismatch :: Doc.Model
modelClientMismatch = Doc.defineModel "ClientMismatch" $ do
  Doc.description "Map of missing, redundant or deleted clients."
  Doc.property "time" Doc.dateTime' $
    Doc.description "Server timestamp (date and time)"
  Doc.property "missing" (Doc.ref modelUserClients) $
    Doc.description "Map of missing clients per user."
  Doc.property "redundant" (Doc.ref modelUserClients) $
    Doc.description "Map of redundant clients per user."
  Doc.property "deleted" (Doc.ref modelUserClients) $
    Doc.description "Map of deleted clients per user."

instance ToJSON ClientMismatch where
  toJSON m =
    object
      [ "time" .= toUTCTimeMillis (cmismatchTime m),
        "missing" .= missingClients m,
        "redundant" .= redundantClients m,
        "deleted" .= deletedClients m
      ]

instance FromJSON ClientMismatch where
  parseJSON = withObject "ClientMismatch" $ \o ->
    ClientMismatch
      <$> o .: "time"
      <*> o .: "missing"
      <*> o .: "redundant"
      <*> o .: "deleted"
