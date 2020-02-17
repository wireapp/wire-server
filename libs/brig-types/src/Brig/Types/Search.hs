{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Brig.Types.Search where

import Data.Aeson
import Data.Id (TeamId, UserId)
import Data.Json.Util
import Imports

data SearchResult a
  = SearchResult
      { searchFound :: Int,
        searchReturned :: Int,
        searchTook :: Int,
        searchResults :: [a]
      }
  deriving (Show)

data Contact
  = Contact
      { contactUserId :: UserId,
        contactName :: Text,
        contactColorId :: Maybe Int,
        contactHandle :: Maybe Text,
        contactTeamId :: Maybe TeamId
      }
  deriving (Show)

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
    object $
      "id" .= contactUserId c
        # "name" .= contactName c
        # "accent_id" .= contactColorId c
        # "handle" .= contactHandle c
        # "team_id" .= contactTeamId c
        # []

instance FromJSON Contact where
  parseJSON =
    withObject "Contact" $ \o ->
      Contact
        <$> o .: "id"
        <*> o .: "name"
        <*> o .:? "accent_id"
        <*> o .:? "handle"
        <*> o .:? "team_id"
