-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.UserGroup.Pagination where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LB
import Data.Default
import Data.Json.Util (UTCTimeMillis)
import Data.OpenApi qualified as S
import Data.OpenApi.ParamSchema qualified as O
import Data.Proxy
import Data.Range as Range
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import GHC.Records (HasField (getField))
import Imports
import Servant.API
import Test.QuickCheck.Gen as Arbitrary
import Wire.API.UserGroup
import Wire.Arbitrary as Arbitrary

-- | Servant combinator for a pagination query.  Actually, it's not merely pagination, but
-- also sorting and filtering.  Please generalize when needed elsewhere.
--
-- Prior art: https://github.com/chordify/haskell-servant-pagination/
-- TODO: document how has_more works.
type PaginationQuery =
  QueryParam' '[Optional, Strict, Description "Search string"] "q" Text
    :> QueryParam' '[Optional, Strict] "sort_by" SortBy
    :> QueryParam' '[Optional, Strict] "sort_order" SortOrder
    :> QueryParam' '[Optional, Strict] "page_size" PageSize
    :> QueryParam'
         '[ Optional,
            Strict,
            Description
              "Pagination state from last response (opaque to clients). \
              \If you set pagination_state, you cannot set any of the other \
              \query params for sorting, filtering, and pagination."
          ]
         "pagination_state"
         PaginationState
    :> Get '[JSON] PaginationResult

------------------------------

data SortBy = SortByName | SortByCreatedAt
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortBy

instance GHC.Records.HasField "toText" SortBy Text where
  getField SortByName = "name"
  getField SortByCreatedAt = "created_at"

instance Default SortBy where
  def = SortByCreatedAt

instance ToSchema SortBy where
  schema =
    enum @Text "SortBy" $
      mconcat
        [ element "name" SortByName,
          element "created_at" SortByCreatedAt
        ]

instance Arbitrary SortBy where
  arbitrary = Arbitrary.elements [minBound ..]

instance FromHttpApiData SortBy where
  parseUrlPiece "name" = pure SortByName
  parseUrlPiece "created_at" = pure SortByCreatedAt
  parseUrlPiece bad = Left $ "SortBy: could not parse " <> bad

instance O.ToParamSchema SortBy -- TODO: what does swagger look like here?

------------------------------

data SortOrder = Asc | Desc
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema SortOrder

instance GHC.Records.HasField "op" SortOrder Text where
  getField Asc = ">"
  getField Desc = "<"

instance GHC.Records.HasField "toText" SortOrder Text where
  getField Asc = "asc"
  getField Desc = "desc"

instance Arbitrary SortOrder where
  arbitrary = Arbitrary.elements [minBound ..]

defaultSortOrder :: SortBy -> SortOrder
defaultSortOrder SortByName = Asc
defaultSortOrder SortByCreatedAt = Desc

instance ToSchema SortOrder where
  schema =
    enum @Text "SortOrder" $
      mconcat
        [ element "asc" Asc,
          element "desc" Desc
        ]

instance FromHttpApiData SortOrder where
  parseUrlPiece "asc" = pure Asc
  parseUrlPiece "desc" = pure Desc
  parseUrlPiece bad = Left $ "SortOrder: could not parse " <> bad

instance O.ToParamSchema SortOrder -- TODO: what does swagger look like here?

------------------------------

newtype PageSize = PageSize {fromPageSize :: Range 1 500 Int}
  deriving (Eq, Show, Ord, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PageSize

pageSizeToInt :: PageSize -> Int
pageSizeToInt = fromRange . fromPageSize

pageSizeFromInt :: Int -> Either Text PageSize
pageSizeFromInt = fmap PageSize . first T.pack . (Range.checkedEither @Int @1 @500 :: Int -> Either String (Range 1 500 Int))

-- | Doesn't crash on bad input, but shrinks it into the allowed range.
pageSizeFromIntUnsafe :: Int -> PageSize
pageSizeFromIntUnsafe = PageSize . (unsafeRange @Int @1 @500) . (+ 1) . (`mod` 500) . (+ (-1))

instance Arbitrary PageSize where
  arbitrary = pageSizeFromIntUnsafe <$> arbitrary

instance ToSchema PageSize where
  schema = PageSize <$> fromPageSize .= schema

instance FromHttpApiData PageSize where
  parseUrlPiece = parseUrlPiece @Int >=> pageSizeFromInt

instance O.ToParamSchema PageSize

instance Default PageSize where
  def = PageSize (unsafeRange 15)

-- TODO: serialize paginationstate more efficiently.  protobuf?

------------------------------

-- | Offset-based pagination.
--
-- FUTUREWORK: For cursor-based pagination, there is postgres machinery.  It requires
-- transaction handling, but this may not imply table locks, so it may be fine.  (Jumping to
-- the last page may not be that relevant in practice, refining the search is more common.)
data PaginationState = PaginationState
  { -- | `searchString` always applies to name, no matter what the sort order.  `Nothing`
    -- means do not filter.
    searchString :: Maybe Text,
    -- TODO: check that this aligns with lastSeen
    sortBy :: SortBy,
    sortOrder :: SortOrder,
    pageSize :: PageSize,
    -- | Next page starts after this (either name or created_at, depending on sortBy).  Id is
    -- tie breaker.
    lastSeen :: Maybe LastSeen
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PaginationState

data LastSeen = LastSeen
  { name :: Maybe UserGroupName,
    createdAt :: Maybe UTCTimeMillis,
    tieBreaker :: UserGroupId
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema LastSeen

instance ToSchema LastSeen where
  schema =
    object "LastSeen" $
      LastSeen
        <$> (.name) .= maybe_ (optField "name" schema)
        <*> (.createdAt) .= maybe_ (optField "created_at" schema)
        <*> (.tieBreaker) .= field "tie_breaker" schema

instance Arbitrary LastSeen where
  arbitrary = LastSeen <$> arbitrary <*> arbitrary <*> arbitrary

instance Default PaginationState where
  def =
    PaginationState
      { searchString = Nothing,
        sortBy = def,
        sortOrder = defaultSortOrder def,
        pageSize = def,
        lastSeen = Nothing
      }

-- TODO: use caml case for everything
instance ToSchema PaginationState where
  schema =
    object "PagintationStatePayload" $
      PaginationState
        <$> (.searchString) .= maybe_ (optField "search_string" schema)
        <*> (.sortBy) .= field "sort_by" schema
        <*> (.sortOrder) .= field "sort_order" schema
        <*> (.pageSize) .= field "page_size" schema
        <*> (.lastSeen) .= maybe_ (optField "last_seen" schema)

instance Arbitrary PaginationState where
  arbitrary =
    PaginationState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance FromHttpApiData PaginationState where
  parseUrlPiece = parseUrlPieceViaSchema

instance ToHttpApiData PaginationState where
  toUrlPiece = toUrlPieceViaSchema

instance O.ToParamSchema PaginationState where
  toParamSchema _ =
    -- PaginationState is supposed to be opaque for clients, no need to swagger docs.
    O.toParamSchema (Proxy @Text)

------------------------------

data PaginationResult = PaginationResult
  { page :: [UserGroup],
    state :: PaginationState
  }
  deriving (Eq, Show, Generic)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema PaginationResult

instance ToSchema PaginationResult where
  schema =
    objectWithDocModifier "PagintationResult" docs $
      PaginationResult
        <$> page .= field "page" (array schema)
        <*> (toUrlPieceViaSchema . state) .= field "state" (withParser schema p)
    where
      p :: Text -> A.Parser PaginationState
      p t =
        case parseUrlPieceViaSchema t of
          Left err -> fail $ "PaginationResult: could not parse state: " <> T.unpack err
          Right ps -> pure ps
      docs :: NamedSwaggerDoc -> NamedSwaggerDoc
      docs =
        description
          ?~ "This is the last page iff it contains fewer rows than requested. There \
             \may return 0 rows on a page."

instance Arbitrary PaginationResult where
  arbitrary = PaginationResult <$> arbitrary <*> arbitrary

------------------------------

parseUrlPieceViaSchema :: (A.FromJSON a) => Text -> Either Text a
parseUrlPieceViaSchema t =
  case Base64.decode (T.encodeUtf8 t) of
    Left err -> Left $ "Base64 decode error: " <> T.pack err
    Right bs -> first T.pack $ A.eitherDecode (LB.fromStrict bs)

toUrlPieceViaSchema :: (A.ToJSON a) => a -> Text
toUrlPieceViaSchema = T.decodeUtf8 . Base64.encode . LB.toStrict . A.encode
