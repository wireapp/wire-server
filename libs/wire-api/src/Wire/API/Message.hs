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

    -- * Priority
    Priority (..),

    -- * Recipients
    OtrRecipients (..),
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

import Control.Lens ((?~))
import qualified Data.Aeson as A
import Data.CommaSeparatedList (CommaSeparatedList (fromCommaSeparatedList))
import Data.Id
import Data.Json.Util
import Data.Schema
import qualified Data.Set as Set
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Servant (FromHttpApiData (..))
import Wire.API.Arbitrary (Arbitrary (..), GenericUniform (..))
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
