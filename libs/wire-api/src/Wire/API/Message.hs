{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

-- | This module interfaces with two protobuf libraries: protobuf and
-- proto-lens.
--
-- The protobuf library was used to manually map types from
-- github.com/wireapp/generic-message-proto/proto/otr.proto. These types are in
-- 'Wire.API.Message.Proto'.
--
-- The proto-lens library was introduced afterwards to automatically map types
-- from the above proto definition. The types are in 'Proto.Otr' of
-- wire-message-proto-lens package.
module Wire.API.Message
  ( -- * Message
    MessageMetadata (..),
    defMessageMetadata,
    NewOtrMessage (..),
    QualifiedNewOtrMessage (..),
    qualifiedNewOtrMetadata,
    protoToNewOtrMessage,

    -- * Protobuf messages
    mkQualifiedOtrPayload,

    -- * Priority
    Priority (..),

    -- * Recipients
    OtrRecipients (..),
    QualifiedOtrRecipients (..),
    protoFromOtrRecipients,
    UserClientMap (..),

    -- * Mismatch
    OtrFilterMissing (..),
    ClientMismatch (..),
    ClientMismatchStrategy (..),
    MessageSendingStatus (..),
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

import Control.Lens (view, (.~), (?~))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.CommaSeparatedList (CommaSeparatedList (fromCommaSeparatedList))
import Data.Domain (Domain, domainText, mkDomain)
import Data.Id
import Data.Json.Util
import qualified Data.Map.Strict as Map
import qualified Data.ProtoLens as ProtoLens
import qualified Data.ProtoLens.Field as ProtoLens
import qualified Data.ProtocolBuffers as Protobuf
import Data.Qualified (Qualified (..))
import Data.Schema
import Data.Serialize (runGet)
import qualified Data.Set as Set
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text.Read as Reader
import qualified Data.UUID as UUID
import Imports
import qualified Proto.Otr
import qualified Proto.Otr_Fields as Proto.Otr
import Servant (FromHttpApiData (..))
import Wire.API.Arbitrary (Arbitrary (..), GenericUniform (..))
import qualified Wire.API.Message.Proto as Proto
import Wire.API.ServantProto (FromProto (..), ToProto (..))
import Wire.API.User.Client (QualifiedUserClientMap (QualifiedUserClientMap), QualifiedUserClients, UserClientMap (..), UserClients (..), modelOtrClientMap, modelUserClients)

--------------------------------------------------------------------------------
-- Message

data MessageMetadata = MessageMetadata
  { mmNativePush :: Bool,
    mmTransient :: Bool,
    mmNativePriority :: Maybe Priority,
    mmData :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Arbitrary) via (GenericUniform MessageMetadata)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema MessageMetadata)

messageMetadataObjectSchema :: ObjectSchema SwaggerDoc MessageMetadata
messageMetadataObjectSchema =
  MessageMetadata
    <$> mmNativePush .= fmap (fromMaybe True) (optField "native_push" schema)
    <*> mmTransient .= fmap (fromMaybe False) (optField "transient" schema)
    <*> mmNativePriority .= maybe_ (optField "native_priority" schema)
    <*> mmData .= maybe_ (optField "data" schema)

instance ToSchema MessageMetadata where
  schema = object "MessageMetadata" messageMetadataObjectSchema

defMessageMetadata :: MessageMetadata
defMessageMetadata =
  MessageMetadata
    { mmNativePush = True,
      mmTransient = False,
      mmNativePriority = Nothing,
      mmData = Nothing
    }

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

newOtrMessageMetadata :: NewOtrMessage -> MessageMetadata
newOtrMessageMetadata msg =
  MessageMetadata
    (newOtrNativePush msg)
    (newOtrTransient msg)
    (newOtrNativePriority msg)
    (newOtrData msg)

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
      mk
        <$> newOtrSender .= field "sender" schema
        <*> newOtrRecipients .= field "recipients" schema
        <*> newOtrMessageMetadata .= messageMetadataObjectSchema
        <*> newOtrReportMissing .= maybe_ (optField "report_missing" (array schema))
    where
      mk :: ClientId -> OtrRecipients -> MessageMetadata -> Maybe [UserId] -> NewOtrMessage
      mk cid rcpts mm =
        NewOtrMessage
          cid
          rcpts
          (mmNativePush mm)
          (mmTransient mm)
          (mmNativePriority mm)
          (mmData mm)

instance FromProto NewOtrMessage where
  fromProto bs = protoToNewOtrMessage <$> runGet Protobuf.decodeMessage bs

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

data QualifiedNewOtrMessage = QualifiedNewOtrMessage
  { qualifiedNewOtrSender :: ClientId,
    qualifiedNewOtrRecipients :: QualifiedOtrRecipients,
    qualifiedNewOtrNativePush :: Bool,
    qualifiedNewOtrTransient :: Bool,
    qualifiedNewOtrNativePriority :: Maybe Priority,
    qualifiedNewOtrData :: ByteString,
    qualifiedNewOtrClientMismatchStrategy :: ClientMismatchStrategy
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform QualifiedNewOtrMessage)

qualifiedNewOtrMetadata :: QualifiedNewOtrMessage -> MessageMetadata
qualifiedNewOtrMetadata msg =
  MessageMetadata
    { mmNativePush = qualifiedNewOtrNativePush msg,
      mmTransient = qualifiedNewOtrTransient msg,
      mmNativePriority = qualifiedNewOtrNativePriority msg,
      mmData = Just . toBase64Text $ qualifiedNewOtrData msg
    }

instance S.ToSchema QualifiedNewOtrMessage where
  declareNamedSchema _ =
    pure $
      S.NamedSchema (Just "QualifiedNewOtrMessage") $
        mempty
          & S.description
            ?~ "This object can only be parsed from Protobuf.\n\
               \The specification for the protobuf types is here: \n\
               \https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

instance FromProto QualifiedNewOtrMessage where
  fromProto bs = protolensToQualifiedNewOtrMessage =<< ProtoLens.decodeMessage bs

instance ToProto QualifiedNewOtrMessage where
  toProto = ProtoLens.encodeMessage . qualifiedNewOtrMessageToProto

protolensToQualifiedNewOtrMessage :: Proto.Otr.QualifiedNewOtrMessage -> Either String QualifiedNewOtrMessage
protolensToQualifiedNewOtrMessage protoMsg = do
  recipients <- protolensOtrRecipientsToOtrRecipients $ view Proto.Otr.recipients protoMsg
  strat <- protolensToClientMismatchStrategy $ view Proto.Otr.maybe'clientMismatchStrategy protoMsg
  pure $
    QualifiedNewOtrMessage
      { qualifiedNewOtrSender = protolensToClientId $ view Proto.Otr.sender protoMsg,
        qualifiedNewOtrRecipients = recipients,
        qualifiedNewOtrNativePush = view Proto.Otr.nativePush protoMsg,
        qualifiedNewOtrTransient = view Proto.Otr.transient protoMsg,
        qualifiedNewOtrNativePriority = protolensToPriority <$> view Proto.Otr.maybe'nativePriority protoMsg,
        qualifiedNewOtrData = view Proto.Otr.blob protoMsg,
        qualifiedNewOtrClientMismatchStrategy = strat
      }

protolensToClientId :: Proto.Otr.ClientId -> ClientId
protolensToClientId = newClientId . view Proto.Otr.client

qualifiedNewOtrMessageToProto :: QualifiedNewOtrMessage -> Proto.Otr.QualifiedNewOtrMessage
qualifiedNewOtrMessageToProto msg =
  ProtoLens.defMessage
    & Proto.Otr.sender .~ clientIdToProtolens (qualifiedNewOtrSender msg)
    & Proto.Otr.recipients .~ qualifiedOtrRecipientsToProtolens (qualifiedNewOtrRecipients msg)
    & Proto.Otr.blob .~ qualifiedNewOtrData msg
    & Proto.Otr.nativePush .~ qualifiedNewOtrNativePush msg
    & Proto.Otr.maybe'nativePriority .~ fmap priorityToProtolens (qualifiedNewOtrNativePriority msg)
    & Proto.Otr.transient .~ qualifiedNewOtrTransient msg
    & Proto.Otr.maybe'clientMismatchStrategy ?~ clientMismatchStrategyToProtolens (qualifiedNewOtrClientMismatchStrategy msg)

mkQualifiedOtrPayload :: ClientId -> [(Qualified UserId, ClientId, ByteString)] -> ByteString -> ClientMismatchStrategy -> Proto.Otr.QualifiedNewOtrMessage
mkQualifiedOtrPayload sender entries dat strat =
  qualifiedNewOtrMessageToProto
    QualifiedNewOtrMessage
      { qualifiedNewOtrSender = sender,
        qualifiedNewOtrRecipients = mkRecipients entries,
        qualifiedNewOtrNativePush = True,
        qualifiedNewOtrNativePriority = Nothing,
        qualifiedNewOtrTransient = False,
        qualifiedNewOtrClientMismatchStrategy = strat,
        qualifiedNewOtrData = dat
      }
  where
    mkRecipients =
      QualifiedOtrRecipients . QualifiedUserClientMap
        . foldr
          ( \(Qualified u d, c, t) ->
              Map.insertWith
                (Map.unionWith (<>))
                d
                (Map.singleton u (Map.singleton c t))
          )
          mempty

clientIdToProtolens :: ClientId -> Proto.Otr.ClientId
clientIdToProtolens cid =
  ProtoLens.defMessage
    & Proto.Otr.client .~ (either error fst . Reader.hexadecimal $ client cid)

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

protolensToPriority :: Proto.Otr.Priority -> Priority
protolensToPriority = \case
  Proto.Otr.LOW_PRIORITY -> LowPriority
  Proto.Otr.HIGH_PRIORITY -> HighPriority

priorityToProtolens :: Priority -> Proto.Otr.Priority
priorityToProtolens LowPriority = Proto.Otr.LOW_PRIORITY
priorityToProtolens HighPriority = Proto.Otr.HIGH_PRIORITY

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
    mkClientEntry (clt, t) = Proto.clientEntry (Proto.fromClientId clt) (fromBase64TextLenient t)

newtype QualifiedOtrRecipients = QualifiedOtrRecipients
  { qualifiedOtrRecipientsMap :: QualifiedUserClientMap ByteString
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (Semigroup, Monoid) via (QualifiedUserClientMap (First ByteString))

protolensOtrRecipientsToOtrRecipients :: [Proto.Otr.QualifiedUserEntry] -> Either String QualifiedOtrRecipients
protolensOtrRecipientsToOtrRecipients entries =
  QualifiedOtrRecipients . QualifiedUserClientMap <$> protolensToQualifiedUCMap entries
  where
    protolensToQualifiedUCMap :: [Proto.Otr.QualifiedUserEntry] -> Either String (Map Domain (Map UserId (Map ClientId ByteString)))
    protolensToQualifiedUCMap qualifiedEntries = parseMap (mkDomain . view Proto.Otr.domain) (protolensToUCMap . view Proto.Otr.entries) qualifiedEntries

    protolensToUCMap :: [Proto.Otr.UserEntry] -> Either String (Map UserId (Map ClientId ByteString))
    protolensToUCMap es = parseMap parseUserId parseClientMap es

    parseUserId :: Proto.Otr.UserEntry -> Either String UserId
    parseUserId =
      maybe (Left "Invalid UUID") (pure . Id)
        . UUID.fromByteString
        . LBS.fromStrict
        . view Proto.Otr.uuid
        . view Proto.Otr.user

    parseClientMap :: Proto.Otr.UserEntry -> Either String (Map ClientId ByteString)
    parseClientMap entry = parseMap parseClientId parseText $ view Proto.Otr.clients entry

    parseClientId :: Proto.Otr.ClientEntry -> Either String ClientId
    parseClientId = pure . protolensToClientId . view Proto.Otr.client

    parseText :: Proto.Otr.ClientEntry -> Either String ByteString
    parseText = pure . view Proto.Otr.text

qualifiedOtrRecipientsToProtolens :: QualifiedOtrRecipients -> [Proto.Otr.QualifiedUserEntry]
qualifiedOtrRecipientsToProtolens (QualifiedOtrRecipients (QualifiedUserClientMap recipients)) =
  map (uncurry quEntry) . Map.assocs $ recipients
  where
    quEntry :: Domain -> Map UserId (Map ClientId ByteString) -> Proto.Otr.QualifiedUserEntry
    quEntry domain m =
      ProtoLens.defMessage
        & Proto.Otr.domain .~ domainText domain
        & Proto.Otr.entries .~ map (uncurry uEntry) (Map.assocs m)

    uEntry :: UserId -> Map ClientId ByteString -> Proto.Otr.UserEntry
    uEntry uid m =
      ProtoLens.defMessage
        & Proto.Otr.user . Proto.Otr.uuid .~ LBS.toStrict (UUID.toByteString (toUUID uid))
        & Proto.Otr.clients .~ map (uncurry cEntry) (Map.assocs m)

    cEntry :: ClientId -> ByteString -> Proto.Otr.ClientEntry
    cEntry cid msg =
      ProtoLens.defMessage
        & Proto.Otr.client .~ clientIdToProtolens cid
        & Proto.Otr.text .~ msg

parseMap :: (Applicative f, Ord k) => (a -> f k) -> (a -> f v) -> [a] -> f (Map k v)
parseMap keyParser valueParser xs = Map.fromList <$> traverse (\x -> (,) <$> keyParser x <*> valueParser x) xs

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

data ClientMismatchStrategy
  = MismatchReportAll
  | MismatchIgnoreAll
  | MismatchReportOnly (Set (Qualified UserId))
  | MismatchIgnoreOnly (Set (Qualified UserId))
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientMismatchStrategy)

protolensToClientMismatchStrategy :: Maybe Proto.Otr.QualifiedNewOtrMessage'ClientMismatchStrategy -> Either String ClientMismatchStrategy
protolensToClientMismatchStrategy = \case
  Nothing -> Left "ClientMismatchStrategy not specified!"
  Just (Proto.Otr.QualifiedNewOtrMessage'IgnoreAll _) -> Right MismatchIgnoreAll
  Just (Proto.Otr.QualifiedNewOtrMessage'ReportAll _) -> Right MismatchReportAll
  Just (Proto.Otr.QualifiedNewOtrMessage'IgnoreOnly ignoreOnly) -> MismatchIgnoreOnly <$> protolensToSetQualifiedUserIds ignoreOnly
  Just (Proto.Otr.QualifiedNewOtrMessage'ReportOnly reportOnly) -> MismatchReportOnly <$> protolensToSetQualifiedUserIds reportOnly

clientMismatchStrategyToProtolens ::
  ClientMismatchStrategy ->
  Proto.Otr.QualifiedNewOtrMessage'ClientMismatchStrategy
clientMismatchStrategyToProtolens = \case
  MismatchIgnoreAll ->
    Proto.Otr.QualifiedNewOtrMessage'IgnoreAll ProtoLens.defMessage
  MismatchReportAll ->
    Proto.Otr.QualifiedNewOtrMessage'ReportAll ProtoLens.defMessage
  MismatchIgnoreOnly users ->
    Proto.Otr.QualifiedNewOtrMessage'IgnoreOnly
      ( ProtoLens.defMessage
          & Proto.Otr.userIds .~ map qualifiedUserIdToProtolens (toList users)
      )
  MismatchReportOnly users ->
    Proto.Otr.QualifiedNewOtrMessage'ReportOnly
      ( ProtoLens.defMessage
          & Proto.Otr.userIds .~ map qualifiedUserIdToProtolens (toList users)
      )

protolensToSetQualifiedUserIds :: ProtoLens.HasField s "userIds" [Proto.Otr.QualifiedUserId] => s -> Either String (Set (Qualified UserId))
protolensToSetQualifiedUserIds = fmap Set.fromList . mapM protolensToQualifiedUserId . view Proto.Otr.userIds

protolensToQualifiedUserId :: Proto.Otr.QualifiedUserId -> Either String (Qualified UserId)
protolensToQualifiedUserId protoQuid =
  Qualified
    <$> parseIdFromText (view Proto.Otr.id protoQuid)
    <*> mkDomain (view Proto.Otr.domain protoQuid)

qualifiedUserIdToProtolens :: Qualified UserId -> Proto.Otr.QualifiedUserId
qualifiedUserIdToProtolens (Qualified uid domain) =
  ProtoLens.defMessage
    & Proto.Otr.id .~ idToText uid
    & Proto.Otr.domain .~ domainText domain

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

data MessageSendingStatus = MessageSendingStatus
  { mssTime :: UTCTimeMillis,
    mssMissingClients :: QualifiedUserClients,
    mssRedundantClients :: QualifiedUserClients,
    mssDeletedClients :: QualifiedUserClients,
    mssFailedToSend :: QualifiedUserClients
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema MessageSendingStatus

instance ToSchema MessageSendingStatus where
  schema =
    object "MessageSendingStatus" $
      MessageSendingStatus
        <$> mssTime
          .= fieldWithDocModifier
            "time"
            (description ?~ "Time of sending message.")
            schema
        <*> mssMissingClients
          .= fieldWithDocModifier
            "missing"
            (description ?~ "Clients that the message /should/ have been encrypted for, but wasn't.")
            schema
        <*> mssRedundantClients
          .= fieldWithDocModifier
            "redundant"
            (description ?~ "Clients that the message /should not/ have been encrypted for, but was.")
            schema
        <*> mssDeletedClients
          .= fieldWithDocModifier
            "deleted"
            (description ?~ "Clients that were deleted.")
            schema
        <*> mssFailedToSend
          .= fieldWithDocModifier
            "failed_to_send"
            ( description
                ?~ "When message sending fails for some clients but succeeds for others,\
                   \this field will contain the list of clients for which the message sending \
                   \failed. This list should be empty when message sending is not even tried, \
                   \like when some clients are missing."
            )
            schema

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
