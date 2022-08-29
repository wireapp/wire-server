{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
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

module Wire.API.Team.Conversation
  ( -- * TeamConversation
    TeamConversation,
    newTeamConversation,
    conversationId,

    -- * TeamConversationList
    TeamConversationList,
    newTeamConversationList,
    teamConversations,

    -- * Swagger
    modelTeamConversation,
    modelTeamConversationList,
  )
where

import Control.Lens (At (at), makeLenses, over, (?~))
import Data.Aeson hiding (fieldLabelModifier)
import Data.Id (ConvId)
import Data.Proxy
import Data.Swagger
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- TeamConversation

newtype TeamConversation = TeamConversation
  { _conversationId :: ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamConversation)

instance ToSchema TeamConversation where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef (Proxy @ConvId)
    pure $
      NamedSchema (Just "TeamConversation") $
        mempty
          & description ?~ "team conversation data"
          & over
            properties
            (at "conversation" ?~ idSchema)

newTeamConversation :: ConvId -> TeamConversation
newTeamConversation = TeamConversation

modelTeamConversation :: Doc.Model
modelTeamConversation = Doc.defineModel "TeamConversation" $ do
  Doc.description "team conversation data"
  Doc.property "conversation" Doc.bytes' $
    Doc.description "conversation ID"

instance ToJSON TeamConversation where
  toJSON t =
    object
      [ "conversation" .= _conversationId t,
        -- FUTUREWORK: get rid of the "managed" field in the next version of the API
        "managed" .= False
      ]

instance FromJSON TeamConversation where
  parseJSON = withObject "team conversation" $ \o ->
    TeamConversation <$> o .: "conversation"

--------------------------------------------------------------------------------
-- TeamConversationList

newtype TeamConversationList = TeamConversationList
  { _teamConversations :: [TeamConversation]
  }
  deriving (Generic)
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

instance ToSchema TeamConversationList where
  declareNamedSchema _ = do
    convs <- declareSchema (Proxy @[TeamConversation])
    pure $
      NamedSchema (Just "TeamConversationList") $
        mempty
          & description ?~ "team conversation list"
          & properties . at "conversations" ?~ Inline convs

newTeamConversationList :: [TeamConversation] -> TeamConversationList
newTeamConversationList = TeamConversationList

modelTeamConversationList :: Doc.Model
modelTeamConversationList = Doc.defineModel "TeamConversationListList" $ do
  Doc.description "list of team conversations"
  Doc.property "conversations" (Doc.unique $ Doc.array (Doc.ref modelTeamConversation)) $
    Doc.description "the array of team conversations"

instance ToJSON TeamConversationList where
  toJSON t = object ["conversations" .= _teamConversations t]

instance FromJSON TeamConversationList where
  parseJSON = withObject "team conversation list" $ \o -> do
    TeamConversationList <$> o .: "conversations"

makeLenses ''TeamConversation
makeLenses ''TeamConversationList
