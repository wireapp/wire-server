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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Brig.Types.Search where

import Data.Aeson
import Data.Id (TeamId, UserId)
import Imports

data SearchResult a
  = SearchResult
      { searchFound :: Int,
        searchReturned :: Int,
        searchTook :: Int,
        searchResults :: [a]
      }
  deriving (Show)

-- | This is a subset of 'User' and json instances should reflect that.
data Contact
  = Contact
      { contactUserId :: UserId,
        contactName :: Text,
        contactColorId :: Maybe Int,
        contactHandle :: Maybe Text,
        contactTeam :: Maybe TeamId
      }
  deriving (Show)

data TeamSearchInfo
  = -- | When searching user is not part of a team.
    NoTeam
  | -- | When searching user is part of a team and 'Brig.Options.setSearchSameTeamOnly' is True
    TeamOnly TeamId
  | -- | When searching user is part of a team and 'Brig.Options.setSearchSameTeamOnly' is False
    TeamAndNonMembers TeamId

instance ToJSON a => ToJSON (SearchResult a) where
  toJSON r =
    object
      [ "found" .= searchFound r,
        "returned" .= searchReturned r,
        "took" .= searchTook r,
        "documents" .= searchResults r
      ]

instance FromJSON a => FromJSON (SearchResult a) where
  parseJSON = withObject "SearchResult" $ \o ->
    SearchResult <$> o .: "found"
      <*> o .: "returned"
      <*> o .: "took"
      <*> o .: "documents"

instance ToJSON Contact where
  toJSON c =
    object
      [ "id" .= contactUserId c,
        "name" .= contactName c,
        "accent_id" .= contactColorId c,
        "handle" .= contactHandle c,
        "team" .= contactTeam c
      ]

instance FromJSON Contact where
  parseJSON =
    withObject "Contact" $ \o ->
      Contact
        <$> o .: "id"
        <*> o .: "name"
        <*> o .:? "accent_id"
        <*> o .:? "handle"
        <*> o .:? "team"
