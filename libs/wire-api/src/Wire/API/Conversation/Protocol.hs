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
    Epoch (..),
    Protocol (..),
    _ProtocolMLS,
    _ProtocolMixed,
    _ProtocolProteus,
    protocolSchema,
    ConversationMLSData (..),
    ActiveMLSConversationData (..),
    optionalActiveMLSConversationDataSchema,
    cnvmlsEpoch,
    ProtocolUpdate (..),
  )
where

import Control.Applicative
import Control.Arrow
import Control.Lens (makePrisms, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Schema
import Data.Time.Clock
import Imports
import Test.QuickCheck
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.Arbitrary

data ProtocolTag = ProtocolProteusTag | ProtocolMLSTag | ProtocolMixedTag
  deriving stock (Eq, Show, Enum, Ord, Bounded, Generic)
  deriving (Arbitrary) via GenericUniform ProtocolTag

instance S.ToSchema ProtocolTag

data ConversationMLSData = ConversationMLSData
  { -- | The MLS group ID associated to the conversation.
    cnvmlsGroupId :: GroupId,
    -- | Information available once the conversation is active (epoch > 0).
    cnvmlsActiveData :: Maybe ActiveMLSConversationData
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via Schema ConversationMLSData

arbitraryActiveData :: Gen (Maybe ActiveMLSConversationData)
arbitraryActiveData = do
  epoch <- arbitrary
  if epoch == Epoch 0
    then pure Nothing
    else
      fmap Just $
        ActiveMLSConversationData epoch <$> arbitrary <*> arbitrary

instance Arbitrary ConversationMLSData where
  arbitrary = ConversationMLSData <$> arbitrary <*> arbitraryActiveData

cnvmlsEpoch :: ConversationMLSData -> Epoch
cnvmlsEpoch = maybe (Epoch 0) (.epoch) . cnvmlsActiveData

mlsDataSchema :: Maybe Version -> ObjectSchema SwaggerDoc ConversationMLSData
mlsDataSchema v =
  ConversationMLSData
    <$> cnvmlsGroupId
      .= fieldWithDocModifier
        "group_id"
        (description ?~ "A base64-encoded MLS group ID")
        schema
    <*> cnvmlsActiveData .= optionalActiveMLSConversationDataSchema v

optionalActiveMLSConversationDataSchema ::
  Maybe Version ->
  ObjectSchema SwaggerDoc (Maybe ActiveMLSConversationData)
optionalActiveMLSConversationDataSchema (Just v)
  | v < V6 =
      -- legacy serialisation
      mk
        <$> maybe (Epoch 0) (.epoch)
          .= fieldWithDocModifier
            "epoch"
            (description ?~ "The epoch number of the corresponding MLS group")
            schema
        <*> fmap (.epochTimestamp)
          .= field "epoch_timestamp" (named "EpochTimestamp" . nullable . unnamed $ utcTimeSchema)
        <*> maybe MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 (.ciphersuite)
          .= fieldWithDocModifier
            "cipher_suite"
            (description ?~ "The cipher suite of the corresponding MLS group")
            schema
  where
    mk :: Epoch -> Maybe UTCTime -> CipherSuiteTag -> Maybe ActiveMLSConversationData
    mk (Epoch 0) _ _ = Nothing
    mk epoch ts cs = ActiveMLSConversationData epoch <$> ts <*> pure cs
optionalActiveMLSConversationDataSchema _ =
  mk
    <$> maybe (Epoch 0) (.epoch)
      .= fieldWithDocModifier
        "epoch"
        (description ?~ "The epoch number of the corresponding MLS group")
        schema
    <*> fmap (.epochTimestamp)
      .= maybe_
        ( optFieldWithDocModifier
            "epoch_timestamp"
            (description ?~ "The timestamp of the epoch number")
            utcTimeSchema
        )
    <*> fmap (.ciphersuite)
      .= maybe_
        ( optFieldWithDocModifier
            "cipher_suite"
            (description ?~ "The cipher suite of the corresponding MLS group")
            schema
        )
  where
    mk :: Epoch -> Maybe UTCTime -> Maybe CipherSuiteTag -> Maybe ActiveMLSConversationData
    mk (Epoch 0) _ _ = Nothing
    mk epoch ts cs = ActiveMLSConversationData epoch <$> ts <*> cs

instance ToSchema ConversationMLSData where
  schema = object "ConversationMLSData" (mlsDataSchema Nothing)

instance ToSchema (Versioned 'V5 ConversationMLSData) where
  schema = Versioned <$> object "ConversationMLSDataV5" (unVersioned .= mlsDataSchema (Just V5))

-- TODO: Fix API compatibility
data ActiveMLSConversationData = ActiveMLSConversationData
  { -- | The current epoch number of the corresponding MLS group.
    epoch :: Epoch,
    -- | The time stamp of the epoch.
    epochTimestamp :: UTCTime,
    -- | The cipher suite to be used in the MLS group.
    ciphersuite :: CipherSuiteTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform ActiveMLSConversationData
  deriving (ToJSON, FromJSON) via Schema ActiveMLSConversationData

instance ToSchema ActiveMLSConversationData where
  schema = object "ActiveMLSConversationData" activeMLSConversationDataSchema

activeMLSConversationDataSchema :: ObjectSchema SwaggerDoc ActiveMLSConversationData
activeMLSConversationDataSchema =
  ActiveMLSConversationData
    <$> (.epoch)
      .= fieldWithDocModifier
        "epoch"
        (description ?~ "The epoch number of the corresponding MLS group")
        schema
    <*> (.epochTimestamp)
      .= fieldWithDocModifier
        "epoch_timestamp"
        (description ?~ "The timestamp of the epoch number")
        utcTimeSchema
    <*> (.ciphersuite)
      .= fieldWithDocModifier
        "cipher_suite"
        (description ?~ "The cipher suite of the corresponding MLS group")
        schema

-- | Conversation protocol and protocol-specific data.
data Protocol
  = ProtocolProteus
  | ProtocolMLS ConversationMLSData
  | ProtocolMixed ConversationMLSData
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Protocol

$(makePrisms ''Protocol)

protocolTag :: Protocol -> ProtocolTag
protocolTag ProtocolProteus = ProtocolProteusTag
protocolTag (ProtocolMLS _) = ProtocolMLSTag
protocolTag (ProtocolMixed _) = ProtocolMixedTag

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

protocolSchema :: Maybe Version -> ObjectSchema SwaggerDoc Protocol
protocolSchema v =
  snd
    <$> (protocolTag &&& id)
      .= bind
        (fst .= protocolTagSchema)
        (snd .= dispatch (protocolDataSchema v))

instance ToSchema Protocol where
  schema = object "Protocol" (protocolSchema Nothing)

instance ToSchema (Versioned 'V5 Protocol) where
  schema = object "Protocol" (Versioned <$> unVersioned .= protocolSchema (Just V5))

deriving via (Schema Protocol) instance FromJSON Protocol

deriving via (Schema Protocol) instance ToJSON Protocol

deriving via (Schema Protocol) instance S.ToSchema Protocol

protocolDataSchema :: Maybe Version -> ProtocolTag -> ObjectSchema SwaggerDoc Protocol
protocolDataSchema _ ProtocolProteusTag = tag _ProtocolProteus (pure ())
protocolDataSchema v ProtocolMLSTag = tag _ProtocolMLS (mlsDataSchema v)
protocolDataSchema v ProtocolMixedTag = tag _ProtocolMixed (mlsDataSchema v)

newtype ProtocolUpdate = ProtocolUpdate {unProtocolUpdate :: ProtocolTag}
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via GenericUniform ProtocolUpdate

instance ToSchema ProtocolUpdate where
  schema = object "ProtocolUpdate" (ProtocolUpdate <$> unProtocolUpdate .= protocolTagSchema)

deriving via (Schema ProtocolUpdate) instance FromJSON ProtocolUpdate

deriving via (Schema ProtocolUpdate) instance ToJSON ProtocolUpdate

deriving via (Schema ProtocolUpdate) instance S.ToSchema ProtocolUpdate
