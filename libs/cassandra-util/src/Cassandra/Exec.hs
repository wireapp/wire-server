{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- | This module exports components from Cassandra's Database.CQL.IO, adding a few functions we find useful, that are built on top of it.
module Cassandra.Exec
  ( params,
    paramsP,
    x5,
    x1,
    syncCassandra,
    paginateC,
    module C,
  )
where

import Cassandra.CQL (Consistency, R)
import Control.Monad.Catch
import Data.Conduit
-- We only use these locally.
import Database.CQL.IO (RetrySettings, RunQ, defRetrySettings, eagerRetrySettings)
-- Things we just import and re-export.
import Database.CQL.IO as C (BatchM, Client, ClientState, MonadClient, Page (..), PrepQuery, Row, addPrepQuery, addQuery, adjustConsistency, adjustResponseTimeout, adjustSendTimeout, batch, emptyPage, init, liftClient, localState, paginate, prepared, query, query1, queryString, retry, runClient, schema, setConsistency, setSerialConsistency, setType, shutdown, trans, write)
import Database.CQL.Protocol (Error, QueryParams (QueryParams), Tuple)
import Imports hiding (init)

params :: Tuple a => Consistency -> a -> QueryParams a
params c p = QueryParams c False p Nothing Nothing Nothing Nothing
{-# INLINE params #-}

paramsP :: Consistency -> a -> Int32 -> QueryParams a
paramsP c p n = QueryParams c False p (Just n) Nothing Nothing Nothing
{-# INLINE paramsP #-}

-- | 'x5' must only be used for idempotent queries, or for cases
-- when a duplicate write has no severe consequences in
-- the context of the application's data model.
-- For more info see e.g.
-- https://docs.datastax.com/en/developer/java-driver//3.6/manual/idempotence/
--
-- The eager retry policy permits 5 retries with exponential
-- backoff (base-2) with an initial delay of 100ms, i.e. the
-- retries will be performed with 100ms, 200ms, 400ms, 800ms
-- and 1.6s delay, respectively, for a maximum delay of ~3s.
x5 :: RetrySettings
x5 = eagerRetrySettings
{-# INLINE x5 #-}

-- | Single, immediate retry, always safe.
-- The 'defRetryHandlers' used are safe also with non-idempotent queries.
x1 :: RetrySettings
x1 = defRetrySettings
{-# INLINE x1 #-}

data CassandraError
  = Cassandra !Error
  | Comm !IOException
  | InvalidData !Text
  | Other !SomeException
  deriving (Show)

syncCassandra :: (Functor m, MonadIO m, MonadCatch m) => m a -> m (Either CassandraError a)
syncCassandra m =
  catches
    (Right <$> m)
    [ Handler $ \(e :: Error) -> return . Left . Cassandra $ e,
      Handler $ \(e :: IOException) -> return . Left . Comm $ e,
      Handler $ \(e :: SomeException) -> return . Left . Other $ e
    ]

-- | Stream results of a query.
--
-- You can execute this conduit by doing @transPipe (runClient ...)@.
paginateC ::
  (Tuple a, Tuple b, RunQ q, MonadClient m) =>
  q R a b ->
  QueryParams a ->
  RetrySettings ->
  ConduitM () [b] m ()
paginateC q p r = go =<< lift (retry r (paginate q p))
  where
    go page = do
      unless (null (result page)) $
        yield (result page)
      when (hasMore page) $
        go =<< lift (retry r (liftClient (nextPage page)))
