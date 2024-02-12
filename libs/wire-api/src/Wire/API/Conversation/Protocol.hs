{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Wire.API.Conversation.Protocol
  ( ProtocolTag (..),
    protocolTag,
    protocolTagSchema,
    protocolValidAction,
    Epoch (..),
    Protocol (..),
    _ProtocolMLS,
    _ProtocolMixed,
    _ProtocolProteus,
    conversationMLSData,
    protocolSchema,
    ConversationMLSData (..),
    ProtocolUpdate (..),
  )
where

import Control.Arrow
import Control.Lens (Traversal', makePrisms, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Time.Clock
import Imports
import Wire.API.Conversation.Action.Tag
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.SubConversation
import Wire.Arbitrary

data ProtocolTag = ProtocolProteusTag | ProtocolMLSTag | ProtocolMixedTag
  deriving stock (Eq, Show, Enum, Ord, Bounded, Generic)
  deriving (Arbitrary) via GenericUniform ProtocolTag

instance S.ToSchema ProtocolTag

data ConversationMLSData = ConversationMLSData
  { -- | The MLS group ID associated to the conversation.
    cnvmlsGroupId :: GroupId,
    -- | The current epoch number of the corresponding MLS group.
    cnvmlsEpoch :: Epoch,
    -- | The time stamp of the epoch.
    cnvmlsEpochTimestamp :: Maybe UTCTime,
    -- | The cipher suite to be used in the MLS group.
    cnvmlsCipherSuite :: CipherSuiteTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ConversationMLSData
  deriving (ToJSON, FromJSON) via Schema ConversationMLSData

mlsDataSchema :: ObjectSchema SwaggerDoc ConversationMLSData
mlsDataSchema =
  ConversationMLSData
    <$> cnvmlsGroupId
      .= fieldWithDocModifier
        "group_id"
        (description ?~ "A base64-encoded MLS group ID")
        schema
    <*> cnvmlsEpoch
      .= fieldWithDocModifier
        "epoch"
        (description ?~ "The epoch number of the corresponding MLS group")
        schema
    <*> cnvmlsEpochTimestamp
      .= fieldWithDocModifier
        "epoch_timestamp"
        (description ?~ "The timestamp of the epoch number")
        schemaEpochTimestamp
    <*> cnvmlsCipherSuite
      .= fieldWithDocModifier
        "cipher_suite"
        (description ?~ "The cipher suite of the corresponding MLS group")
        schema

instance ToSchema ConversationMLSData where
  schema = object "ConversationMLSData" mlsDataSchema

-- | Conversation protocol and protocol-specific data.
data Protocol
  = ProtocolProteus
  | ProtocolMLS ConversationMLSData
  | ProtocolMixed ConversationMLSData
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Protocol

$(makePrisms ''Protocol)

conversationMLSData :: Traversal' Protocol ConversationMLSData
conversationMLSData _ ProtocolProteus = pure ProtocolProteus
conversationMLSData f (ProtocolMLS mls) = ProtocolMLS <$> f mls
conversationMLSData f (ProtocolMixed mls) = ProtocolMixed <$> f mls

protocolTag :: Protocol -> ProtocolTag
protocolTag ProtocolProteus = ProtocolProteusTag
protocolTag (ProtocolMLS _) = ProtocolMLSTag
protocolTag (ProtocolMixed _) = ProtocolMixedTag

-- | Certain actions need to be performed at the level of the underlying
-- protocol (MLS, mostly) before being applied to conversations. This function
-- returns whether a given action tag is directly applicable to a conversation
-- with the given protocol.
protocolValidAction :: Protocol -> ConversationActionTag -> Bool
protocolValidAction ProtocolProteus _ = True
protocolValidAction (ProtocolMixed _) _ = True
protocolValidAction (ProtocolMLS _) ConversationJoinTag = False
protocolValidAction (ProtocolMLS _) ConversationLeaveTag = True
protocolValidAction (ProtocolMLS _) ConversationRemoveMembersTag = False
protocolValidAction (ProtocolMLS _) ConversationDeleteTag = True
protocolValidAction (ProtocolMLS _) _ = True

instance ToSchema ProtocolTag where
  schema =
    enum @Text "Protocol" $
      mconcat
        [ element "proteus" ProtocolProteusTag,
          element "mls" ProtocolMLSTag,
          element "mixed" ProtocolMixedTag
        ]

deriving via (Schema ProtocolTag) instance FromJSON ProtocolTag

deriving via (Schema ProtocolTag) instance ToJSON ProtocolTag

protocolTagSchema :: ObjectSchema SwaggerDoc ProtocolTag
protocolTagSchema = fmap (fromMaybe ProtocolProteusTag) (optField "protocol" schema)

protocolSchema :: ObjectSchema SwaggerDoc Protocol
protocolSchema =
  snd
    <$> (protocolTag &&& id)
      .= bind
        (fst .= protocolTagSchema)
        (snd .= dispatch protocolDataSchema)

instance ToSchema Protocol where
  schema = object "Protocol" protocolSchema

deriving via (Schema Protocol) instance FromJSON Protocol

deriving via (Schema Protocol) instance ToJSON Protocol

deriving via (Schema Protocol) instance S.ToSchema Protocol

protocolDataSchema :: ProtocolTag -> ObjectSchema SwaggerDoc Protocol
protocolDataSchema ProtocolProteusTag = tag _ProtocolProteus (pure ())
protocolDataSchema ProtocolMLSTag = tag _ProtocolMLS mlsDataSchema
protocolDataSchema ProtocolMixedTag = tag _ProtocolMixed mlsDataSchema

newtype ProtocolUpdate = ProtocolUpdate {unProtocolUpdate :: ProtocolTag}
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via GenericUniform ProtocolUpdate

instance ToSchema ProtocolUpdate where
  schema = object "ProtocolUpdate" (ProtocolUpdate <$> unProtocolUpdate .= protocolTagSchema)

deriving via (Schema ProtocolUpdate) instance FromJSON ProtocolUpdate

deriving via (Schema ProtocolUpdate) instance ToJSON ProtocolUpdate

deriving via (Schema ProtocolUpdate) instance S.ToSchema ProtocolUpdate
