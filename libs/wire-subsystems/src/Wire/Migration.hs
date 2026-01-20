-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Migration where

import Cassandra
import Cassandra.Settings
import Data.Aeson
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import GHC.Generics (Generically (..))
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger qualified as Log
import Wire.Util (embedClient)

data MigrationOptions = MigrationOptions
  { pageSize :: Int32,
    parallelism :: Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via Generically MigrationOptions

logRetrievedPage :: (Member TinyLog r) => Int32 -> (a -> b) -> ConduitM (Int32, [a]) [b] (Sem r) ()
logRetrievedPage pageSize toRow =
  C.mapM
    ( \(i, rows) -> do
        let estimatedRowsSoFar = (i - 1) * pageSize + fromIntegral (length rows)
        info $ Log.msg (Log.val "retrieved page") . Log.field "estimatedRowsSoFar" estimatedRowsSoFar
        pure $ map toRow rows
    )

withCount :: (Monad m) => ConduitM () [a] m () -> ConduitM () (Int32, [a]) m ()
withCount = zipSources (C.sourceList [1 ..])

paginateSem ::
  forall a b q r.
  ( Tuple a,
    Tuple b,
    RunQ q,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (Embed IO) r
  ) =>
  q R a b ->
  QueryParams a ->
  RetrySettings ->
  ConduitT () [b] (Sem r) ()
paginateSem q p r = do
  go =<< lift getFirstPage
  where
    go page = do
      lift $ info $ Log.msg (Log.val "got a page")
      unless (null (result page)) $
        yield (result page)
      when (hasMore page) $
        go =<< lift (getNextPage page)

    getFirstPage :: Sem r (Page b)
    getFirstPage = do
      client <- input
      embedClient client $ retry r (paginate q p)

    getNextPage :: Page b -> Sem r (Page b)
    getNextPage page = do
      client <- input
      embedClient client $ retry r (nextPage page)
