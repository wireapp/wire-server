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
    protoToNewOtrMessage,

    -- * Priority
    Priority (..),

    -- * Recipients
    OtrRecipients (..),
    protoFromOtrRecipients,
    UserClientMap (..),

    -- * Filter
    OtrFilterMissing (..),
    ClientMismatch (..),
    UserClients (..),
    ReportMissing (..),
    IgnoreMissing (..),

    -- * Swagger
    modelNewOtrMessage,
    modelOtrRecipients,
    modelClientMismatch,
    typePriority,
  )
where

import Control.Lens (view, (?~))
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import Data.CommaSeparatedList (CommaSeparatedList (fromCommaSeparatedList))
import Data.Id
import Data.Json.Util
import qualified Data.Map.Strict as Map
import qualified Data.ProtocolBuffers as Protobuf
import Data.Schema
import Data.Serialize (runGetLazy)
import qualified Data.Set as Set
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Imports
import Servant (FromHttpApiData (..))
import Wire.API.Arbitrary (Arbitrary (..), GenericUniform (..))
import qualified Wire.API.Message.Proto as Proto
import Wire.API.ServantProto (FromProto (..))
import Wire.API.User.Client (UserClientMap (..), UserClients (..), modelOtrClientMap, modelUserClients)

--------------------------------------------------------------------------------
-- Message

data NewOtrMessage = NewOtrMessage
  { newOtrSender :: ClientId,
    newOtrRecipients :: OtrRecipients,
    newOtrNativePush :: Bool,
    newOtrTransient :: Bool,
    newOtrNativePriority :: Maybe Priority,
    newOtrData :: Maybe Text,
    newOtrReportMissing :: Maybe [UserId]
    -- FUTUREWORK: if (and only if) clients can promise this uid list will always exactly
    -- be the list of uids we could also extract from the messages' recipients field, we
    -- should do the latter, for two reasons: (1) no need for an artificial limit on the
    -- body field length, because it'd be just a boolean; (2) less network consumption.
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewOtrMessage)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema NewOtrMessage)

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
  Doc.property "native_priority" typePriority $ do
    Doc.description "The native push priority (default 'high')."
    Doc.optional
  Doc.property "data" Doc.bytes' $ do
    Doc.description
      "Extra (symmetric) data (i.e. ciphertext) that is replicated \
      \for each recipient."
    Doc.optional
  Doc.property "report_missing" (Doc.unique $ Doc.array Doc.bytes') $ do
    Doc.description "List of user IDs"
    Doc.optional

instance ToSchema NewOtrMessage where
  schema =
    object "new-otr-message" $
      NewOtrMessage
        <$> newOtrSender .= field "sender" schema
        <*> newOtrRecipients .= field "recipients" schema
        <*> newOtrNativePush .= (field "native_push" schema <|> pure True)
        <*> newOtrTransient .= (field "transient" schema <|> pure False)
        <*> newOtrNativePriority .= opt (field "native_priority" schema)
        <*> newOtrData .= opt (field "data" schema)
        <*> newOtrReportMissing .= opt (field "report_missing" (array schema))

instance FromProto NewOtrMessage where
  fromProto bs = protoToNewOtrMessage <$> runGetLazy Protobuf.decodeMessage bs

protoToNewOtrMessage :: Proto.NewOtrMessage -> NewOtrMessage
protoToNewOtrMessage msg =
  NewOtrMessage
    { newOtrSender = Proto.toClientId (view Proto.newOtrMessageSender msg),
      newOtrRecipients = protoToOtrRecipients (view Proto.newOtrMessageRecipients msg),
      newOtrNativePush = view Proto.newOtrMessageNativePush msg,
      newOtrTransient = view Proto.newOtrMessageTransient msg,
      newOtrData = toBase64Text <$> view Proto.newOtrMessageData msg,
      newOtrNativePriority = protoToPriority <$> view Proto.newOtrMessageNativePriority msg,
      newOtrReportMissing = protoToReportMissing $ view Proto.newOtrMessageReportMissing msg
    }

protoToReportMissing :: [Proto.UserId] -> Maybe [UserId]
protoToReportMissing [] = Nothing
protoToReportMissing us = Just $ view Proto.userId <$> us

--------------------------------------------------------------------------------
-- Priority

-- | Native push notification priority flag.  'LowPriority' is never used, but might be in the
-- future.
--
-- @neongreen writes: [...] nobody seems to ever set `native_priority` in the client code. Exhibits
-- A1 and A2:
--
-- * <https://github.com/search?q=org%3Awireapp+native_priority&type=Code>
-- * <https://sourcegraph.com/search?q=native_priority+repo:^github\.com/wireapp/+#1>
--
-- see also: 'Wire.API.Message.Proto.Priority'.
data Priority = LowPriority | HighPriority
  deriving stock (Eq, Show, Ord, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform Priority)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema Priority

typePriority :: Doc.DataType
typePriority =
  Doc.string $
    Doc.enum
      [ "low",
        "high"
      ]

instance ToSchema Priority where
  schema =
    enum @Text "Priority" $
      mconcat
        [ element "low" LowPriority,
          element "high" HighPriority
        ]

protoToPriority :: Proto.Priority -> Priority
protoToPriority Proto.LowPriority = LowPriority
protoToPriority Proto.HighPriority = HighPriority

--------------------------------------------------------------------------------
-- Recipients

-- FUTUREWORK: Add ToSchema when 'NewOtrMessage' has ToSchema
newtype OtrRecipients = OtrRecipients
  { otrRecipientsMap :: UserClientMap Text
  }
  deriving stock (Eq, Show)
  deriving newtype (ToSchema, A.ToJSON, A.FromJSON, Semigroup, Monoid, Arbitrary)

-- FUTUREWORK: Remove when 'NewOtrMessage' has ToSchema
modelOtrRecipients :: Doc.Model
modelOtrRecipients = Doc.defineModel "OtrRecipients" $ do
  Doc.description "Recipients of OTR content."
  Doc.property "" (Doc.ref modelOtrClientMap) $
    Doc.description "Mapping of user IDs to 'OtrClientMap's."

protoToOtrRecipients :: [Proto.UserEntry] -> OtrRecipients
protoToOtrRecipients =
  OtrRecipients . UserClientMap
    . foldl' userEntries mempty
  where
    userEntries :: Map UserId (Map ClientId Text) -> Proto.UserEntry -> Map UserId (Map ClientId Text)
    userEntries acc x =
      let u = view Proto.userEntryId x
          c = view Proto.userEntryClients x
          m = foldl' clientEntries mempty c
       in Map.insert (view Proto.userId u) m acc
    clientEntries acc x =
      let c = Proto.toClientId $ view Proto.clientEntryId x
          t = toBase64Text $ view Proto.clientEntryMessage x
       in Map.insert c t acc

protoFromOtrRecipients :: OtrRecipients -> [Proto.UserEntry]
protoFromOtrRecipients rcps =
  let m = userClientMap (otrRecipientsMap rcps)
   in map mkProtoRecipient (Map.toList m)
  where
    mkProtoRecipient (usr, clts) =
      let xs = map mkClientEntry (Map.toList clts)
       in Proto.userEntry (Proto.fromUserId usr) xs
    mkClientEntry (clt, t) = Proto.clientEntry (Proto.fromClientId clt) (fromBase64Text t)

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
    OtrIgnoreMissing (Set UserId)
  | -- | Complain only about missing
    --      recipients who /are/ on this list
    OtrReportMissing (Set UserId)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OtrFilterMissing)

data ClientMismatch = ClientMismatch
  { cmismatchTime :: UTCTimeMillis,
    -- | Clients that the message /should/ have been encrypted for, but wasn't.
    missingClients :: UserClients,
    -- | Clients that the message /should not/ have been encrypted for, but was.
    redundantClients :: UserClients,
    deletedClients :: UserClients
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema ClientMismatch

instance Arbitrary ClientMismatch where
  arbitrary =
    ClientMismatch
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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

instance ToSchema ClientMismatch where
  schema =
    object "ClientMismatch" $
      ClientMismatch
        <$> cmismatchTime .= field "time" schema
        <*> missingClients .= field "missing" schema
        <*> redundantClients .= field "redundant" schema
        <*> deletedClients .= field "deleted" schema

-- QueryParams

data IgnoreMissing
  = IgnoreMissingAll
  | IgnoreMissingList (Set UserId)
  deriving (Show, Eq)

instance S.ToParamSchema IgnoreMissing where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

instance FromHttpApiData IgnoreMissing where
  parseQueryParam = \case
    "true" -> Right IgnoreMissingAll
    "false" -> Right $ IgnoreMissingList mempty
    list -> IgnoreMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list

data ReportMissing
  = ReportMissingAll
  | ReportMissingList (Set UserId)

instance S.ToParamSchema ReportMissing where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

instance FromHttpApiData ReportMissing where
  parseQueryParam = \case
    "true" -> Right ReportMissingAll
    "false" -> Right $ ReportMissingList mempty
    list -> ReportMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list

--------------------------------------------------------------------------------
-- Utilities

fromBase64Text :: Text -> ByteString
fromBase64Text = B64.decodeLenient . encodeUtf8

toBase64Text :: ByteString -> Text
toBase64Text = decodeUtf8 . B64.encode
