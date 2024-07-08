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
  )
where

import Control.Lens (makeLenses, (?~))
import Data.Aeson qualified as A
import Data.Id (ConvId)
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- TeamConversation

newtype TeamConversation = TeamConversation
  { _conversationId :: ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamConversation)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TeamConversation)

managedDesc :: Text
managedDesc =
  "This field MUST NOT be used by clients. "
    <> "It is here only for backwards compatibility of the interface."

instance ToSchema TeamConversation where
  schema =
    objectWithDocModifier
      "TeamConversation"
      (description ?~ "Team conversation data")
      $ TeamConversation
        <$> _conversationId .= field "conversation" schema
        <* const ()
          .= fieldWithDocModifier
            "managed"
            (description ?~ managedDesc)
            (c (False :: Bool))
    where
      c :: (A.ToJSON a) => a -> ValueSchema SwaggerDoc ()
      c val = mkSchema mempty (const (pure ())) (const (pure (A.toJSON val)))

newTeamConversation :: ConvId -> TeamConversation
newTeamConversation = TeamConversation

--------------------------------------------------------------------------------
-- TeamConversationList

newtype TeamConversationList = TeamConversationList
  { _teamConversations :: [TeamConversation]
  }
  deriving (Generic)
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TeamConversationList)

instance ToSchema TeamConversationList where
  schema =
    objectWithDocModifier
      "TeamConversationList"
      (description ?~ "Team conversation list")
      $ TeamConversationList
        <$> _teamConversations .= field "conversations" (array schema)

newTeamConversationList :: [TeamConversation] -> TeamConversationList
newTeamConversationList = TeamConversationList

makeLenses ''TeamConversation
makeLenses ''TeamConversationList
