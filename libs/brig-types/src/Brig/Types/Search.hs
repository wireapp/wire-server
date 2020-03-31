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
