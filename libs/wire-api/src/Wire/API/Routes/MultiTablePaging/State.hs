-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.MultiTablePaging.State
  ( MultiTablePagingState (..),
    PagingTable (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as BS
import Data.Json.Util (fromBase64Text, toBase64Text)
import Data.Proxy
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as Text
import GHC.TypeLits
import Imports

-- | The state of a multi-table paginated query. It is made of a reference to
-- the table currently being paginated, as well as an opaque token returned by
-- Cassandra.
data MultiTablePagingState (name :: Symbol) tables = MultiTablePagingState
  { mtpsTable :: tables,
    mtpsState :: Maybe ByteString
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema (MultiTablePagingState name tables)

encodePagingState :: PagingTable tables => MultiTablePagingState name tables -> ByteString
encodePagingState (MultiTablePagingState table state) =
  let encodedTable = encodePagingTable table
      encodedState = fromMaybe "" state
   in BS.cons encodedTable encodedState

parseConversationPagingState :: PagingTable tables => ByteString -> Either String (MultiTablePagingState name tables)
parseConversationPagingState = AB.parseOnly conversationPagingStateParser

conversationPagingStateParser :: PagingTable tables => AB.Parser (MultiTablePagingState name tables)
conversationPagingStateParser = do
  table <- AB.anyWord8 >>= decodePagingTable
  state <- (AB.endOfInput $> Nothing) <|> (Just <$> AB.takeByteString <* AB.endOfInput)
  pure $ MultiTablePagingState table state

-- | A class for values that can be encoded with a single byte. Used to add a
-- byte of extra information to the paging state in order to recover the table
-- information from a paging token.
class PagingTable t where
  -- Using 'Word8' because 256 tables ought to be enough.
  encodePagingTable :: t -> Word8
  decodePagingTable :: MonadFail m => Word8 -> m t

instance (PagingTable tables, KnownSymbol name) => ToSchema (MultiTablePagingState name tables) where
  schema =
    (toBase64Text . encodePagingState)
      .= parsedText
        (Text.pack (symbolVal (Proxy @name)) <> "_PagingState")
        (parseConversationPagingState <=< fromBase64Text)
