{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Team.Conversation
  ( -- * TeamConversation
    TeamConversation,
    newTeamConversation,
    conversationId,
    managedConversation,

    -- * TeamConversationList
    TeamConversationList,
    newTeamConversationList,
    teamConversations,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id (ConvId)
import Imports

--------------------------------------------------------------------------------
-- TeamConversation

data TeamConversation = TeamConversation
  { _conversationId :: ConvId,
    _managedConversation :: Bool
  }

newTeamConversation :: ConvId -> Bool -> TeamConversation
newTeamConversation = TeamConversation

instance ToJSON TeamConversation where
  toJSON t =
    object
      [ "conversation" .= _conversationId t,
        "managed" .= _managedConversation t
      ]

instance FromJSON TeamConversation where
  parseJSON = withObject "team conversation" $ \o ->
    TeamConversation <$> o .: "conversation" <*> o .: "managed"

--------------------------------------------------------------------------------
-- TeamConversationList

newtype TeamConversationList = TeamConversationList
  { _teamConversations :: [TeamConversation]
  }

newTeamConversationList :: [TeamConversation] -> TeamConversationList
newTeamConversationList = TeamConversationList

instance ToJSON TeamConversationList where
  toJSON t = object ["conversations" .= _teamConversations t]

instance FromJSON TeamConversationList where
  parseJSON = withObject "team conversation list" $ \o -> do
    TeamConversationList <$> o .: "conversations"

makeLenses ''TeamConversation
makeLenses ''TeamConversationList
