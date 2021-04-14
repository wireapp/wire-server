{-# LANGUAGE ApplicativeDo #-}
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
    RoleFilter (..),
    TeamUserSearchSortOrder (..),
    TeamUserSearchSortBy (..),

    -- * Swagger
    modelSearchResult,
    modelSearchContact,
    modelTeamContact,
  )
where

import Control.Lens (over, (.~), (?~))
import Data.Aeson
import Data.Attoparsec.ByteString (sepBy)
import Data.Attoparsec.ByteString.Char8 (char, string)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHasMap
import Data.Id (TeamId, UserId)
import Data.Json.Util (UTCTimeMillis)
import Data.Proxy (Proxy (..))
import Data.Qualified
import Data.Swagger hiding (Contact)
import qualified Data.Swagger.Build.Api as Doc
import Deriving.Swagger
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
    searchResults :: [a]
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (Arbitrary) via (GenericUniform (SearchResult a))

instance Foldable SearchResult where
  foldMap f r = foldMap f (searchResults r)

instance Traversable SearchResult where
  traverse f r = do
    newResults <- traverse f (searchResults r)
    pure $ r {searchResults = newResults}

instance ToSchema (SearchResult Contact) where
  declareNamedSchema _ = do
    intSchema <- declareSchema (Proxy @Int)
    contacts <- declareSchema (Proxy @[Contact])
    pure $
      NamedSchema (Just "SearchResult") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ InsOrdHasMap.fromList
              [ ("found", Inline (intSchema & description ?~ "Total number of hits")),
                ("returned", Inline (intSchema & description ?~ "Total number of hits returned")),
                ("took", Inline (intSchema & description ?~ "Search time in ms")),
                ("documents", Inline (contacts & description ?~ "List of contacts found"))
              ]
          & required .~ ["found", "returned", "took", "documents"]

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

type ContactLabelMappings = '["color_id" ':-> "accent_id"]

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

instance ToSchema Contact where
  declareNamedSchema _ = do
    genericSchema <-
      genericDeclareNamedSchema
        ( swaggerOptions
            @'[FieldLabelModifier (StripPrefix "contact", CamelToSnake, LabelMappings ContactLabelMappings)]
        )
        (Proxy @Contact)
    idSchema <- declareSchemaRef (Proxy @UserId)
    pure $
      genericSchema
        & over (schema . properties) (InsOrdHasMap.insert "id" idSchema)

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
      [ "id" .= qUnqualified (contactQualifiedId c), -- For backwards compatibility
        "qualified_id" .= contactQualifiedId c,
        "name" .= contactName c,
        "accent_id" .= contactColorId c,
        "handle" .= contactHandle c,
        "team" .= contactTeam c
      ]

instance FromJSON Contact where
  parseJSON =
    withObject "Contact" $ \o ->
      Contact
        <$> o .: "qualified_id"
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
    teamContactSAMLIdp :: Maybe Text,
    teamContactRole :: Maybe Role
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
        "saml_idp" .= teamContactSAMLIdp c,
        "role" .= teamContactRole c
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
