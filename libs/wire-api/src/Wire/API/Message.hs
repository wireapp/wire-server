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
    ClientMismatch (..),
    ClientMismatchStrategy (..),
    MessageSendingStatus (..),
    UserClients (..),
    ReportMissing (..),
    IgnoreMissing (..),
  )
where

import Control.Lens (view, (.~), (?~))
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.CommaSeparatedList (CommaSeparatedList (fromCommaSeparatedList))
import Data.Domain (Domain, domainText, mkDomain)
import Data.Id
import Data.Json.Util
import Data.Map.Strict qualified as Map
import Data.OpenApi qualified as S
import Data.ProtoLens qualified as ProtoLens
import Data.ProtoLens.Field qualified as ProtoLens
import Data.ProtocolBuffers qualified as Protobuf
import Data.Qualified (Qualified (..))
import Data.Schema
import Data.Serialize (runGet)
import Data.Set qualified as Set
import Data.Text.Read qualified as Reader
import Data.UUID qualified as UUID
import Imports
import Proto.Otr qualified
import Proto.Otr_Fields qualified as Proto.Otr
import Servant (FromHttpApiData (..))
import Wire.API.Message.Proto qualified as Proto
import Wire.API.ServantProto (FromProto (..), ToProto (..))
import Wire.API.User.Client
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

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
protolensToClientId = ClientId . view Proto.Otr.client

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
      QualifiedOtrRecipients
        . QualifiedUserClientMap
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
    & Proto.Otr.client .~ (either error fst . Reader.hexadecimal $ clientToText cid)

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

protoToOtrRecipients :: [Proto.UserEntry] -> OtrRecipients
protoToOtrRecipients =
  OtrRecipients
    . UserClientMap
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

protolensToSetQualifiedUserIds :: (ProtoLens.HasField s "userIds" [Proto.Otr.QualifiedUserId]) => s -> Either String (Set (Qualified UserId))
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
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

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
    mssFailedToSend :: QualifiedUserClients,
    mssFailedToConfirmClients :: QualifiedUserClients
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema MessageSendingStatus

instance ToSchema MessageSendingStatus where
  schema =
    objectWithDocModifier
      "MessageSendingStatus"
      (description ?~ combinedDesc)
      $ MessageSendingStatus
        <$> mssTime .= field "time" schema
        <*> mssMissingClients .= field "missing" schema
        <*> mssRedundantClients .= field "redundant" schema
        <*> mssDeletedClients .= field "deleted" schema
        <*> mssFailedToSend .= field "failed_to_send" schema
        <*> mssFailedToConfirmClients .= field "failed_to_confirm_clients" schema
    where
      combinedDesc =
        "The Proteus message sending status. It has these fields:\n\
        \- `time`: "
          <> timeDesc
          <> "\n\
             \- `missing`: "
          <> missingDesc
          <> "\n\
             \- `redundant`: "
          <> redundantDesc
          <> "\n\
             \- `deleted`: "
          <> deletedDesc
          <> "\n\
             \- `failed_to_send`: "
          <> failedToSendDesc
      timeDesc = "Time of sending message."
      missingDesc = "Clients that the message /should/ have been encrypted for, but wasn't."
      redundantDesc = "Clients that the message /should not/ have been encrypted for, but was."
      deletedDesc = "Clients that were deleted."
      failedToSendDesc =
        "When message sending fails for some clients but succeeds for others, \
        \e.g., because a remote backend is unreachable, \
        \this field will contain the list of clients for which the message sending \
        \failed. This list should be empty when message sending is not even tried, \
        \like when some clients are missing."

-- QueryParams

data IgnoreMissing
  = IgnoreMissingAll
  | IgnoreMissingList (Set UserId)
  deriving (Show, Eq)

instance S.ToParamSchema IgnoreMissing where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

instance FromHttpApiData IgnoreMissing where
  parseQueryParam = \case
    "true" -> Right IgnoreMissingAll
    "false" -> Right $ IgnoreMissingList mempty
    list -> IgnoreMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list

data ReportMissing
  = ReportMissingAll
  | ReportMissingList (Set UserId)

instance S.ToParamSchema ReportMissing where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

instance FromHttpApiData ReportMissing where
  parseQueryParam = \case
    "true" -> Right ReportMissingAll
    "false" -> Right $ ReportMissingList mempty
    list -> ReportMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list
