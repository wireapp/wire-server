{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Search
  ( SearchResult (..),
    Contact (..),
    TeamContact (..),

    -- * Swagger
    modelSearchResult,
    modelSearchContact,
    modelTeamContact,
  )
where

import Data.Aeson
import Data.Id (TeamId, UserId)
import Data.Json.Util (UTCTimeMillis)
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.User (ManagedBy)
import Wire.API.User.Identity (Email (..))

--------------------------------------------------------------------------------
-- SearchResult

data SearchResult a = SearchResult
  { searchFound :: Int,
    searchReturned :: Int,
    searchTook :: Int,
    searchResults :: [a]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform (SearchResult a))

modelSearchResult :: Doc.Model -> Doc.Model
modelSearchResult modelContact = Doc.defineModel "SearchResult" $ do
  Doc.description "Search Result"
  Doc.property "found" Doc.int32' $
    Doc.description "Total number of hits"
  Doc.property "returned" Doc.int32' $
    Doc.description "Number of hits returned"
  Doc.property "took" Doc.int32' $
    Doc.description "Search time in ms"
  Doc.property "documents" (Doc.array (Doc.ref modelContact)) $
    Doc.description "List of contacts found"

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
    SearchResult
      <$> o .: "found"
      <*> o .: "returned"
      <*> o .: "took"
      <*> o .: "documents"

--------------------------------------------------------------------------------
-- Contact

-- | Returned by 'searchIndex' under @/contacts/search@.
-- This is a subset of 'User' and json instances should reflect that.
data Contact = Contact
  { contactUserId :: UserId,
    contactName :: Text,
    contactColorId :: Maybe Int,
    contactHandle :: Maybe Text,
    contactTeam :: Maybe TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Contact)

modelSearchContact :: Doc.Model
modelSearchContact = Doc.defineModel "Contact" $ do
  Doc.description "Contact discovered through search"
  Doc.property "id" Doc.string' $
    Doc.description "User ID"
  Doc.property "name" Doc.string' $
    Doc.description "Name"
  Doc.property "handle" Doc.string' $
    Doc.description "Handle"
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent color"
    Doc.optional
  Doc.property "team" Doc.string' $ do
    Doc.description "Team ID"
    Doc.optional

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

--------------------------------------------------------------------------------
-- TeamContact

-- | Returned by 'browseTeam' under @/teams/:tid/search@.
data TeamContact = TeamContact
  { teamContactUserId :: UserId,
    teamContactName :: Text,
    teamContactColorId :: Maybe Int,
    teamContactHandle :: Maybe Text,
    teamContactTeam :: Maybe TeamId,
    teamContactEmail :: Maybe Email,
    teamContactCreatedAt :: Maybe UTCTimeMillis,
    teamContactManagedBy :: Maybe ManagedBy,
    teamContactSAMLIdp :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamContact)

modelTeamContact :: Doc.Model
modelTeamContact = Doc.defineModel "TeamContact" $ do
  Doc.description "Contact discovered through search"
  Doc.property "id" Doc.string' $
    Doc.description "User ID"
  Doc.property "name" Doc.string' $
    Doc.description "Name"
  Doc.property "handle" Doc.string' $
    Doc.description "Handle"
  Doc.property "accent_id" Doc.int32' $ do
    Doc.description "Accent color"
    Doc.optional
  Doc.property "team" Doc.string' $ do
    Doc.description "Team ID"
    Doc.optional
  Doc.property "email" Doc.string' $ do
    Doc.description "Email address"
    Doc.optional

instance ToJSON TeamContact where
  toJSON c =
    object
      [ "id" .= teamContactUserId c,
        "name" .= teamContactName c,
        "accent_id" .= teamContactColorId c,
        "handle" .= teamContactHandle c,
        "team" .= teamContactTeam c,
        "email" .= teamContactEmail c,
        "created_at" .= teamContactCreatedAt c,
        "managed_by" .= teamContactManagedBy c,
        "saml_idp" .= teamContactSAMLIdp c
      ]

instance FromJSON TeamContact where
  parseJSON =
    withObject "Contact" $ \o ->
      TeamContact
        <$> o .: "id"
        <*> o .: "name"
        <*> o .:? "accent_id"
        <*> o .:? "handle"
        <*> o .:? "team"
        <*> o .:? "email"
        <*> o .:? "created_at"
        <*> o .:? "managed_by"
        <*> o .:? "saml_idp"
