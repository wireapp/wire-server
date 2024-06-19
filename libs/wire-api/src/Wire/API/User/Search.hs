{-# LANGUAGE ApplicativeDo #-}
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

module Wire.API.User.Search
  ( SearchResult (..),
    Contact (..),
    TeamContact (..),
    RoleFilter (..),
    Sso (..),
    TeamUserSearchSortOrder (..),
    TeamUserSearchSortBy (..),
    FederatedUserSearchPolicy (..),
    PagingState (..),
  )
where

import Cassandra qualified as C
import Control.Error
import Control.Lens (makePrisms, (?~))
import Data.Aeson hiding (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString (sepBy)
import Data.Attoparsec.ByteString.Char8 (char, string)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..))
import Data.Either.Combinators (mapLeft)
import Data.Id (TeamId, UserId)
import Data.Json.Util (UTCTimeMillis)
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Qualified
import Data.Schema
import Data.Text qualified as T
import Data.Text.Ascii (AsciiBase64Url, toText, validateBase64Url)
import Imports
import Servant.API (FromHttpApiData, ToHttpApiData (..))
import Web.Internal.HttpApiData (parseQueryParam)
import Wire.API.Team.Role (Role)
import Wire.API.User (ManagedBy)
import Wire.API.User.Identity (Email (..))
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

-------------------------------------------------------------------------------
-- PagingState

newtype PagingState = PagingState {unPagingState :: AsciiBase64Url}
  deriving newtype (Eq, Show, Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema PagingState

instance ToSchema PagingState where
  schema = (toText . unPagingState) .= parsedText "PagingState" (fmap PagingState . validateBase64Url)

instance ToParamSchema PagingState where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance FromHttpApiData PagingState where
  parseQueryParam s = mapLeft T.pack $ PagingState <$> validateBase64Url s

instance ToHttpApiData PagingState where
  toQueryParam = toText . unPagingState

instance ToByteString PagingState where
  builder = builder . unPagingState

instance FromByteString PagingState where
  parser = fmap PagingState parser

--------------------------------------------------------------------------------
-- SearchResult

data SearchResult a = SearchResult
  { searchFound :: Int,
    searchReturned :: Int,
    searchTook :: Int,
    searchResults :: [a],
    searchPolicy :: FederatedUserSearchPolicy,
    searchPagingState :: Maybe PagingState,
    searchHasMore :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving (Arbitrary) via (GenericUniform (SearchResult a))

instance Foldable SearchResult where
  foldMap f r = foldMap f (searchResults r)

instance Traversable SearchResult where
  traverse f r = do
    newResults <- traverse f (searchResults r)
    pure $ r {searchResults = newResults}

instance (ToSchema a) => ToSchema (SearchResult a) where
  schema =
    object "SearchResult" $
      SearchResult
        <$> searchFound .= fieldWithDocModifier "found" (S.description ?~ "Total number of hits") schema
        <*> searchReturned .= fieldWithDocModifier "returned" (S.description ?~ "Total number of hits returned") schema
        <*> searchTook .= fieldWithDocModifier "took" (S.description ?~ "Search time in ms") schema
        <*> searchResults .= fieldWithDocModifier "documents" (S.description ?~ "List of contacts found") (array schema)
        <*> searchPolicy .= fieldWithDocModifier "search_policy" (S.description ?~ "Search policy that was applied when searching for users") schema
        <*> searchPagingState .= maybe_ (optFieldWithDocModifier "paging_state" (S.description ?~ "Paging state that should be supplied to retrieve the next page of results") schema)
        <*> searchHasMore .= maybe_ (optFieldWithDocModifier "has_more" (S.description ?~ "Indicates whether there are more results to be fetched") schema)

deriving via (Schema (SearchResult Contact)) instance ToJSON (SearchResult Contact)

deriving via (Schema (SearchResult TeamContact)) instance ToJSON (SearchResult TeamContact)

deriving via (Schema (SearchResult Contact)) instance FromJSON (SearchResult Contact)

deriving via (Schema (SearchResult TeamContact)) instance FromJSON (SearchResult TeamContact)

deriving via (Schema (SearchResult Contact)) instance S.ToSchema (SearchResult Contact)

deriving via (Schema (SearchResult TeamContact)) instance S.ToSchema (SearchResult TeamContact)

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

-- | Related to `UserSSOId`, but more straight-forward because it does not take SCIM externalId into account.
data Sso = Sso
  { ssoIssuer :: Text,
    ssoNameId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Sso)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Sso)

instance ToSchema Sso where
  schema =
    object "Sso" $
      Sso
        <$> ssoIssuer .= field "issuer" schema
        <*> ssoNameId .= field "nameid" schema

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
    teamContactSso :: Maybe Sso,
    teamContactEmailUnvalidated :: Maybe Email
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamContact)
  deriving (ToJSON, FromJSON) via (Schema TeamContact)

instance ToSchema TeamContact where
  schema =
    object "TeamContact" $
      TeamContact
        <$> teamContactUserId .= field "id" schema
        <*> teamContactName .= field "name" schema
        <*> teamContactColorId .= optField "accent_id" (maybeWithDefault Aeson.Null schema)
        <*> teamContactHandle .= optField "handle" (maybeWithDefault Aeson.Null schema)
        <*> teamContactTeam .= optField "team" (maybeWithDefault Aeson.Null schema)
        <*> teamContactEmail .= optField "email" (maybeWithDefault Aeson.Null schema)
        <*> teamContactCreatedAt .= optField "created_at" (maybeWithDefault Aeson.Null schema)
        <*> teamContactManagedBy .= optField "managed_by" (maybeWithDefault Aeson.Null schema)
        <*> teamContactSAMLIdp .= optField "saml_idp" (maybeWithDefault Aeson.Null schema)
        <*> teamContactRole .= optField "role" (maybeWithDefault Aeson.Null schema)
        <*> teamContactScimExternalId .= optField "scim_external_id" (maybeWithDefault Aeson.Null schema)
        <*> teamContactSso .= optField "sso" (maybeWithDefault Aeson.Null schema)
        <*> teamContactEmailUnvalidated .= optField "email_unvalidated" (maybeWithDefault Aeson.Null schema)

data TeamUserSearchSortBy
  = SortByName
  | SortByHandle
  | SortByEmail
  | SortBySAMLIdp
  | SortByManagedBy
  | SortByRole
  | SortByCreatedAt
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform TeamUserSearchSortBy)

instance S.ToParamSchema TeamUserSearchSortBy where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.enum_ ?~ fmap teamUserSearchSortByName [minBound .. maxBound]

instance ToByteString TeamUserSearchSortBy where
  builder = teamUserSearchSortByName

instance FromByteString TeamUserSearchSortBy where
  parser =
    asum $
      [minBound .. maxBound] <&> \ctor ->
        ctor <$ string (teamUserSearchSortByName ctor)

instance FromHttpApiData TeamUserSearchSortBy where
  parseQueryParam name = note ("Unknown search sort: " <> name) $
    getAlt $
      flip foldMap [minBound .. maxBound] $ \s ->
        guard (teamUserSearchSortByName s == name) $> s

teamUserSearchSortByName :: (IsString a) => TeamUserSearchSortBy -> a
teamUserSearchSortByName SortByName = "name"
teamUserSearchSortByName SortByHandle = "handle"
teamUserSearchSortByName SortByEmail = "email"
teamUserSearchSortByName SortBySAMLIdp = "saml_idp"
teamUserSearchSortByName SortByManagedBy = "managed_by"
teamUserSearchSortByName SortByRole = "role"
teamUserSearchSortByName SortByCreatedAt = "created_at"

data TeamUserSearchSortOrder
  = SortOrderAsc
  | SortOrderDesc
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform TeamUserSearchSortOrder)

instance S.ToParamSchema TeamUserSearchSortOrder where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.enum_ ?~ fmap teamUserSearchSortOrderName [minBound .. maxBound]

instance ToByteString TeamUserSearchSortOrder where
  builder = teamUserSearchSortOrderName

instance FromByteString TeamUserSearchSortOrder where
  parser =
    asum $
      [minBound .. maxBound] <&> \ctor ->
        ctor <$ string (teamUserSearchSortOrderName ctor)

instance FromHttpApiData TeamUserSearchSortOrder where
  parseQueryParam name = note ("Unknown search order: " <> name) $
    getAlt $
      flip foldMap [minBound .. maxBound] $ \s ->
        guard (teamUserSearchSortOrderName s == name) $> s

teamUserSearchSortOrderName :: (IsString a) => TeamUserSearchSortOrder -> a
teamUserSearchSortOrderName SortOrderAsc = "asc"
teamUserSearchSortOrderName SortOrderDesc = "desc"

newtype RoleFilter = RoleFilter [Role]
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform RoleFilter)

instance S.ToParamSchema RoleFilter where
  toParamSchema _ = S.toParamSchema (Proxy @[Role])

instance ToByteString RoleFilter where
  builder (RoleFilter roles) = mconcat $ intersperse "," (fmap builder roles)

instance FromByteString RoleFilter where
  parser = RoleFilter <$> parser `sepBy` char ','

instance FromHttpApiData RoleFilter where
  parseQueryParam = fmap RoleFilter . traverse parseQueryParam . T.split (== ',')

data FederatedUserSearchPolicy
  = NoSearch
  | ExactHandleSearch
  | FullSearch
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform FederatedUserSearchPolicy)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema FederatedUserSearchPolicy)

instance ToSchema FederatedUserSearchPolicy where
  schema =
    enum @Text "FederatedUserSearchPolicy" $
      element "no_search" NoSearch
        <> element "exact_handle_search" ExactHandleSearch
        <> element "full_search" FullSearch

instance C.Cql FederatedUserSearchPolicy where
  ctype = C.Tagged C.IntColumn

  toCql NoSearch = C.CqlInt 0
  toCql ExactHandleSearch = C.CqlInt 1
  toCql FullSearch = C.CqlInt 2

  fromCql (C.CqlInt 0) = pure NoSearch
  fromCql (C.CqlInt 1) = pure ExactHandleSearch
  fromCql (C.CqlInt 2) = pure FullSearch
  fromCql n = Left $ "Unexpected SearchVisibilityInbound: " ++ show n

makePrisms ''FederatedUserSearchPolicy
