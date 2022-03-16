{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Search
  ( SearchResult (..),
    Contact (..),
    TeamContact (..),
    RoleFilter (..),
    Sso (..),
    TeamUserSearchSortOrder (..),
    TeamUserSearchSortBy (..),
    FederatedUserSearchPolicy (..),

    -- * Swagger
    modelSearchResult,
    modelSearchContact,
    modelTeamContact,
  )
where

import Control.Lens (makePrisms, (?~))
import Data.Aeson hiding (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString (sepBy)
import Data.Attoparsec.ByteString.Char8 (char, string)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..))
import Data.Id (TeamId, UserId)
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Team.Role (Role)
import Wire.API.User (ManagedBy)
import Wire.API.User.Identity (Email (..))

--------------------------------------------------------------------------------
-- SearchResult

data SearchResult a = SearchResult
  { searchFound :: Int,
    searchReturned :: Int,
    searchTook :: Int,
    searchResults :: [a],
    searchPolicy :: FederatedUserSearchPolicy
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (Arbitrary) via (GenericUniform (SearchResult a))

instance Foldable SearchResult where
  foldMap f r = foldMap f (searchResults r)

instance Traversable SearchResult where
  traverse f r = do
    newResults <- traverse f (searchResults r)
    pure $ r {searchResults = newResults}

instance ToSchema a => ToSchema (SearchResult a) where
  schema =
    object "SearchResult" $
      SearchResult
        <$> searchFound .= fieldWithDocModifier "found" (S.description ?~ "Total number of hits") schema
        <*> searchReturned .= fieldWithDocModifier "returned" (S.description ?~ "Total number of hits returned") schema
        <*> searchTook .= fieldWithDocModifier "took" (S.description ?~ "Search time in ms") schema
        <*> searchResults .= fieldWithDocModifier "documents" (S.description ?~ "List of contacts found") (array schema)
        <*> searchPolicy .= fieldWithDocModifier "search_policy" (S.description ?~ "Search policy that was applied when searching for users") schema

deriving via (Schema (SearchResult Contact)) instance ToJSON (SearchResult Contact)

deriving via (Schema (SearchResult Contact)) instance FromJSON (SearchResult Contact)

deriving via (Schema (SearchResult Contact)) instance S.ToSchema (SearchResult Contact)

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

instance ToJSON (SearchResult TeamContact) where
  toJSON r =
    Aeson.object
      [ "found" Aeson..= searchFound r,
        "returned" Aeson..= searchReturned r,
        "took" Aeson..= searchTook r,
        "documents" Aeson..= searchResults r,
        "search_policy" Aeson..= searchPolicy r
      ]

instance FromJSON (SearchResult TeamContact) where
  parseJSON = withObject "SearchResult" $ \o ->
    SearchResult
      <$> o .: "found"
      <*> o .: "returned"
      <*> o .: "took"
      <*> o .: "documents"
      <*> o .: "search_policy"

--------------------------------------------------------------------------------
-- Contact

-- | Returned by 'searchIndex' under @/contacts/search@.
-- This is a subset of 'User' and json instances should reflect that.
data Contact = Contact
  { contactQualifiedId :: Qualified UserId,
    contactName :: Text,
    contactColorId :: Maybe Int,
    contactHandle :: Maybe Text,
    contactTeam :: Maybe TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Contact)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema Contact

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

instance ToSchema Contact where
  schema =
    objectWithDocModifier "Contact" (description ?~ "Contact discovered through search") $
      Contact
        <$> contactQualifiedId .= field "qualified_id" schema
        <* (qUnqualified . contactQualifiedId) .= optField "id" schema
        <*> contactName .= field "name" schema
        <*> contactColorId .= optField "accent_id" (maybeWithDefault Aeson.Null schema)
        <*> contactHandle .= optField "handle" (maybeWithDefault Aeson.Null schema)
        <*> contactTeam .= optField "team" (maybeWithDefault Aeson.Null schema)

--------------------------------------------------------------------------------
-- TeamContact

data Sso = Sso
  { ssoIssuer :: Text,
    ssoNameId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Sso)

instance ToJSON Sso where
  toJSON c =
    Aeson.object
      [ "issuer" Aeson..= ssoIssuer c,
        "nameid" Aeson..= ssoNameId c
      ]

instance FromJSON Sso where
  parseJSON = withObject "Sso" $ \o ->
    Sso
      <$> o .: "issuer"
      <*> o .: "nameid"

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
    teamContactSAMLIdp :: Maybe Text,
    teamContactRole :: Maybe Role,
    teamContactScimExternalId :: Maybe Text,
    teamContactSso :: Maybe Sso
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
  Doc.property "scim_external_id" Doc.string' $ do
    Doc.description "SCIM external ID"
    Doc.optional

instance ToJSON TeamContact where
  toJSON c =
    Aeson.object
      [ "id" Aeson..= teamContactUserId c,
        "name" Aeson..= teamContactName c,
        "accent_id" Aeson..= teamContactColorId c,
        "handle" Aeson..= teamContactHandle c,
        "team" Aeson..= teamContactTeam c,
        "email" Aeson..= teamContactEmail c,
        "created_at" Aeson..= teamContactCreatedAt c,
        "managed_by" Aeson..= teamContactManagedBy c,
        "saml_idp" Aeson..= teamContactSAMLIdp c,
        "role" Aeson..= teamContactRole c,
        "scim_external_id" Aeson..= teamContactScimExternalId c,
        "sso" Aeson..= teamContactSso c
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
        <*> o .:? "role"
        <*> o .:? "scim_external_id"
        <*> o .:? "sso"

data TeamUserSearchSortBy
  = SortByName
  | SortByHandle
  | SortByEmail
  | SortBySAMLIdp
  | SortByManagedBy
  | SortByRole
  | SortByCreatedAt
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform TeamUserSearchSortBy)

instance ToByteString TeamUserSearchSortBy where
  builder SortByName = "name"
  builder SortByHandle = "handle"
  builder SortByEmail = "email"
  builder SortBySAMLIdp = "saml_idp"
  builder SortByManagedBy = "managed_by"
  builder SortByRole = "role"
  builder SortByCreatedAt = "created_at"

instance FromByteString TeamUserSearchSortBy where
  parser =
    SortByName <$ string "name"
      <|> SortByHandle <$ string "handle"
      <|> SortByEmail <$ string "email"
      <|> SortBySAMLIdp <$ string "saml_idp"
      <|> SortByManagedBy <$ string "managed_by"
      <|> SortByRole <$ string "role"
      <|> SortByCreatedAt <$ string "created_at"

data TeamUserSearchSortOrder
  = SortOrderAsc
  | SortOrderDesc
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform TeamUserSearchSortOrder)

instance ToByteString TeamUserSearchSortOrder where
  builder SortOrderAsc = "asc"
  builder SortOrderDesc = "desc"

instance FromByteString TeamUserSearchSortOrder where
  parser =
    SortOrderAsc <$ string "asc"
      <|> SortOrderDesc <$ string "desc"

newtype RoleFilter = RoleFilter [Role]
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform RoleFilter)

instance ToByteString RoleFilter where
  builder (RoleFilter roles) = mconcat $ intersperse "," (fmap builder roles)

instance FromByteString RoleFilter where
  parser = RoleFilter <$> parser `sepBy` char ','

data FederatedUserSearchPolicy
  = NoSearch
  | ExactHandleSearch
  | FullSearch
  deriving (Show, Eq, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform FederatedUserSearchPolicy)
  deriving (ToJSON, FromJSON) via (Schema FederatedUserSearchPolicy)

instance ToSchema FederatedUserSearchPolicy where
  schema =
    enum @Text "FederatedUserSearchPolicy" $
      element "no_search" NoSearch
        <> element "exact_handle_search" ExactHandleSearch
        <> element "full_search" FullSearch

makePrisms ''FederatedUserSearchPolicy
