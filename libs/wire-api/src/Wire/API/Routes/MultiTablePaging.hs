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

module Wire.API.Routes.MultiTablePaging
  ( GetMultiTablePageRequest (..),
    MultiTablePage (..),
    LocalOrRemoteTable (..),
    MultiTablePagingState (..),
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Kind
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Range
import Data.Schema
import Data.Text qualified as Text
import GHC.TypeLits
import Imports
import Wire.API.Routes.MultiTablePaging.State

-- | A request for a page of results from the database. Type arguments:
--
--  * @name@   Name of the resources being paginated through
--  * @tables@ A (usually finite) type that represent the table currently being
--             used (must be an instance of 'PagingTable')
--  * @max@    Maximum page size
--  * @def@    Default page size
--
-- See 'ConversationPagingState' for an example.
data GetMultiTablePageRequest (name :: Symbol) (tables :: Type) (max :: Nat) (def :: Nat) = GetMultiTablePageRequest
  { gmtprSize :: Range 1 max Int32,
    gmtprState :: Maybe (MultiTablePagingState name tables)
  }
  deriving stock (Show, Eq)

-- We can't use deriving via due to this error:
-- .../wire-server/libs/wire-api/src/Wire/API/Routes/MultiTablePaging.hs:24:12: error:
--     • Couldn't match type ‘Data.Singletons.Prelude.Ord.Case_6989586621679792881
--                              1 max (CmpNat 1 max)’
--                      with ‘'True’
--         arising from the 'deriving' clause of a data type declaration
--     • When deriving the instance for (ToJSON
--                                         (GetMultiTablePageRequest name tables max def))
--    |
-- 24 |   deriving ToJSON via Schema (GetMultiTablePageRequest name tables max def)
--    |            ^^^^^^

type RequestSchemaConstraint name tables max def = (KnownNat max, KnownNat def, Within Int32 1 max, 1 <= def, def <= max, PagingTable tables, KnownSymbol name)

deriving via
  Schema (GetMultiTablePageRequest name tables max def)
  instance
    (RequestSchemaConstraint name tables max def) => ToJSON (GetMultiTablePageRequest name tables max def)

deriving via
  Schema (GetMultiTablePageRequest name tables max def)
  instance
    (RequestSchemaConstraint name tables max def) => FromJSON (GetMultiTablePageRequest name tables max def)

deriving via
  Schema (GetMultiTablePageRequest name tables max def)
  instance
    ( Typeable tables,
      RequestSchemaConstraint name tables max def
    ) =>
    S.ToSchema (GetMultiTablePageRequest name tables max def)

instance (RequestSchemaConstraint name tables max def) => ToSchema (GetMultiTablePageRequest name tables max def) where
  schema =
    let addPagingStateDoc =
          description
            ?~ "optional, when not specified, the first page will be returned.\
               \Every returned page contains a paging_state, this should be supplied to retrieve the next page."
        addSizeDoc = description ?~ ("optional, must be <= " <> textFromNat @max <> ", defaults to " <> textFromNat @def <> ".")
     in objectWithDocModifier
          ("GetPaginated_" <> textFromSymbol @name)
          (description ?~ "A request to list some or all of a user's " <> textFromSymbol @name <> ", including remote ones")
          $ GetMultiTablePageRequest
            <$> gmtprSize .= (fromMaybe (toRange (Proxy @def)) <$> optFieldWithDocModifier "size" addSizeDoc schema)
            <*> gmtprState .= maybe_ (optFieldWithDocModifier "paging_state" addPagingStateDoc schema)

textFromNat :: forall n. (KnownNat n) => Text
textFromNat = Text.pack . show . natVal $ Proxy @n

textFromSymbol :: forall s. (KnownSymbol s) => Text
textFromSymbol = Text.pack . symbolVal $ Proxy @s

-- | The result of a multi-table paginated query. Contains the list of results,
-- a flag indicating whether there are more, and the state to pass to the next
-- query.
data MultiTablePage (name :: Symbol) (resultsKey :: Symbol) (tables :: Type) a = MultiTablePage
  { mtpResults :: [a],
    mtpHasMore :: Bool,
    mtpPagingState :: MultiTablePagingState name tables
  }
  deriving stock (Eq, Show)

type PageSchemaConstraints name resultsKey tables a = (KnownSymbol resultsKey, KnownSymbol name, ToSchema a, PagingTable tables)

deriving via
  (Schema (MultiTablePage name resultsKey tables a))
  instance
    (PageSchemaConstraints name resultsKey tables a) =>
    ToJSON (MultiTablePage name resultsKey tables a)

deriving via
  (Schema (MultiTablePage name resultsKey tables a))
  instance
    (PageSchemaConstraints name resultsKey tables a) =>
    FromJSON (MultiTablePage name resultsKey tables a)

deriving via
  (Schema (MultiTablePage name resultsKey tables a))
  instance
    (Typeable tables, Typeable a, PageSchemaConstraints name resultsKey tables a) =>
    S.ToSchema (MultiTablePage name resultsKey tables a)

instance
  (KnownSymbol resultsKey, KnownSymbol name, ToSchema a, PagingTable tables) =>
  ToSchema (MultiTablePage name resultsKey tables a)
  where
  schema =
    object (textFromSymbol @name <> "_Page") $
      MultiTablePage
        <$> mtpResults .= field (textFromSymbol @resultsKey) (array schema)
        <*> mtpHasMore .= field "has_more" schema
        <*> mtpPagingState .= field "paging_state" schema

-- | A type to be used as the @tables@ argument of 'GetMultiTablePageRequest'
-- when the resources being paginated through are split into local and remote.
data LocalOrRemoteTable
  = PagingLocals
  | PagingRemotes
  deriving stock (Show, Eq)

instance PagingTable LocalOrRemoteTable where
  encodePagingTable PagingLocals = 0
  encodePagingTable PagingRemotes = 1

  decodePagingTable 0 = pure PagingLocals
  decodePagingTable 1 = pure PagingRemotes
  decodePagingTable x = fail $ "Expected 0 or 1 while parsing LocalOrRemoteTable, got: " <> show x
