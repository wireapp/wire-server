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

import Control.Lens (makeLenses, (?~))
import qualified Data.Aeson as A
import Data.Id (ConvId)
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Routes.Version
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- TeamConversation

-- FUTUREWORK: The version tag type argument can be dropped as soon as versions
-- below V2 are not supported any more.
-- Derivation of various JSON instances is verbose with this version tag type variable, but this can be simplified once the old version can be dropped.
newtype TeamConversation (v :: *) = TeamConversation
  { _conversationId :: ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform (TeamConversation v))

instance ToSchema (TeamConversation (Until 'V2)) where
  schema =
    objectWithDocModifier
      "TeamConversation"
      (description ?~ "Team conversation data")
      $ TeamConversation
        <$> _conversationId .= field "conversation" schema
        <* const ()
          .= fieldWithDocModifier
            "managed"
            (description ?~ "(Not parsed any more) Whether this is a managed team conversation")
            (c (False :: Bool))
    where
      c :: A.ToJSON a => a -> ValueSchema SwaggerDoc ()
      c val = mkSchema mempty (const (pure ())) (const (pure (A.toJSON val)))

instance ToSchema (TeamConversation (From 'V2)) where
  schema =
    objectWithDocModifier
      "TeamConversation"
      (description ?~ "Team conversation data")
      $ TeamConversation
        <$> _conversationId .= field "conversation" schema

deriving via
  Schema (TeamConversation (Until 'V2))
  instance
    A.FromJSON (TeamConversation (Until 'V2))

deriving via
  Schema (TeamConversation (Until 'V2))
  instance
    A.ToJSON (TeamConversation (Until 'V2))

deriving via
  Schema (TeamConversation (From 'V2))
  instance
    A.FromJSON (TeamConversation (From 'V2))

deriving via
  Schema (TeamConversation (From 'V2))
  instance
    A.ToJSON (TeamConversation (From 'V2))

deriving via
  Schema (TeamConversation (Until 'V2))
  instance
    S.ToSchema (TeamConversation (Until 'V2))

deriving via
  Schema (TeamConversation (From 'V2))
  instance
    S.ToSchema (TeamConversation (From 'V2))

newTeamConversation :: ConvId -> TeamConversation v
newTeamConversation = TeamConversation

modelTeamConversation :: Doc.Model
modelTeamConversation = Doc.defineModel "TeamConversation" $ do
  Doc.description "team conversation data"
  Doc.property "conversation" Doc.bytes' $
    Doc.description "conversation ID"
  Doc.property "managed" Doc.bytes' $
    Doc.description "Whether the conversation is managed (deprecated)"

--------------------------------------------------------------------------------
-- TeamConversationList

-- FUTUREWORK: The version tag type argument can be dropped as soon as versions
-- below V2 are not supported any more.
newtype TeamConversationList (v :: *) = TeamConversationList
  { _teamConversations :: [TeamConversation v]
  }
  deriving (Generic)
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

instance ToSchema (TeamConversation v) => ToSchema (TeamConversationList v) where
  schema =
    objectWithDocModifier
      "TeamConversationList"
      (description ?~ "Team conversation list")
      $ TeamConversationList
        <$> _teamConversations .= field "conversations" (array schema)

deriving via
  Schema (TeamConversationList (Until 'V2))
  instance
    A.FromJSON (TeamConversationList (Until 'V2))

deriving via
  Schema (TeamConversationList (Until 'V2))
  instance
    A.ToJSON (TeamConversationList (Until 'V2))

deriving via
  Schema (TeamConversationList (From 'V2))
  instance
    A.FromJSON (TeamConversationList (From 'V2))

deriving via
  Schema (TeamConversationList (From 'V2))
  instance
    A.ToJSON (TeamConversationList (From 'V2))

deriving via
  Schema (TeamConversationList (Until 'V2))
  instance
    S.ToSchema (TeamConversationList (Until 'V2))

deriving via
  Schema (TeamConversationList (From 'V2))
  instance
    S.ToSchema (TeamConversationList (From 'V2))

newTeamConversationList :: [TeamConversation v] -> TeamConversationList v
newTeamConversationList = TeamConversationList

modelTeamConversationList :: Doc.Model
modelTeamConversationList = Doc.defineModel "TeamConversationListList" $ do
  Doc.description "list of team conversations"
  Doc.property "conversations" (Doc.unique $ Doc.array (Doc.ref modelTeamConversation)) $
    Doc.description "the array of team conversations"

makeLenses ''TeamConversation
makeLenses ''TeamConversationList
