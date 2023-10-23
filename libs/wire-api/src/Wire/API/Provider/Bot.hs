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

module Wire.API.Provider.Bot
  ( -- * Bot Views
    BotConvView,
    botConvView,
    botConvId,
    botConvName,
    botConvMembers,
    BotUserView (..),
  )
where

import Control.Lens (makeLenses)
import Data.Aeson qualified as A
import Data.Handle (Handle)
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.Conversation.Member (OtherMember (..))
import Wire.API.User.Profile (ColourId, Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- BotConvView

-- | A conversation as seen by a bot.
data BotConvView = BotConvView
  { _botConvId :: ConvId,
    _botConvName :: Maybe Text,
    _botConvMembers :: [OtherMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BotConvView)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema BotConvView

instance ToSchema BotConvView where
  schema =
    object "BotConvView" $
      BotConvView
        <$> _botConvId .= field "id" schema
        <*> _botConvName .= maybe_ (optField "name" schema)
        <*> _botConvMembers .= field "members" (array schema)

botConvView :: ConvId -> Maybe Text -> [OtherMember] -> BotConvView
botConvView = BotConvView

--------------------------------------------------------------------------------
-- BotUserView

data BotUserView = BotUserView
  { botUserViewId :: UserId,
    botUserViewName :: Name,
    botUserViewColour :: ColourId,
    botUserViewHandle :: Maybe Handle,
    botUserViewTeam :: Maybe TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform BotUserView)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema BotUserView

instance ToSchema BotUserView where
  schema =
    object "BotUserView" $
      BotUserView
        <$> botUserViewId .= field "id" schema
        <*> botUserViewName .= field "name" schema
        <*> botUserViewColour .= field "accent_id" schema
        <*> botUserViewHandle .= optField "handle" (maybeWithDefault A.Null schema)
        <*> botUserViewTeam .= optField "team" (maybeWithDefault A.Null schema)

makeLenses ''BotConvView
