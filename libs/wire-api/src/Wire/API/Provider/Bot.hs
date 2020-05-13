{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Aeson
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util ((#))
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation.Member (OtherMember (..))
import Wire.API.User.Profile (ColourId, Name)

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

botConvView :: ConvId -> Maybe Text -> [OtherMember] -> BotConvView
botConvView = BotConvView

instance ToJSON BotConvView where
  toJSON c =
    object $
      "id" .= _botConvId c
        # "name" .= _botConvName c
        # "members" .= _botConvMembers c
        # []

instance FromJSON BotConvView where
  parseJSON = withObject "BotConvView" $ \o ->
    BotConvView
      <$> o .: "id"
      <*> o .:? "name"
      <*> o .: "members"

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

instance ToJSON BotUserView where
  toJSON u =
    object
      [ "id" .= botUserViewId u,
        "name" .= botUserViewName u,
        "accent_id" .= botUserViewColour u,
        "handle" .= botUserViewHandle u,
        "team" .= botUserViewTeam u
      ]

instance FromJSON BotUserView where
  parseJSON = withObject "BotUserView" $ \o ->
    BotUserView
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "accent_id"
      <*> o .:? "handle"
      <*> o .:? "team"

makeLenses ''BotConvView
